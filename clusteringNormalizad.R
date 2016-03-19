# clustering con normalizacion de los datos.
# la idea es realizar un clustering con normalización de los datos para comprobar si de esta manera
# podemos aumentar la complejidad del modelo y ganar algo mas de detalle.


# Funciones ---------------------------------------------------------------

# funcion para calcular rapidamente accuracy de una confusion table
getAcc <- function (tabla) (tabla[4] + tabla[1]) / sum(tabla)

# para cada una de las columnas del data frame queremos comprobar si es un level.
# si es un level, entonces tendremos que sustituirlo por un numero
# Esta funcion asigna las variables que son factores a enteros para poder realizar con ellos un
#  proceso de clustering
#  data_inicial:        data frame con los datos orginales.
#  data_final  :        data frame con los datos originales pero en las columnas no tiene factor.
#  return      :        un data frame con valores numericos en lugar de factores.
changeDataFrame <- function (data_inicial, data_final){
  n <- ncol(data_inicial)
  for (i in 1:n) {
    if (is.factor(data_inicial[,i])){
      data_final[,i] <- sapply (data_inicial[,i], function (x){which(x == levels(data_inicial[,i]))})
    }else{
      data_final[,i] <- data_inicial[,i]
    }# end if 
  }# end for
  return (data_final)
}# end fucntion



# loading the data --------------------------------------------------------

# loading the data --------------------------------------------------------

train     <- read.csv ("data/train.csv") #, stringsAsFactors=FALSE)
train.new <- read.csv ("data/train.csv" , stringsAsFactors=FALSE)
test   <- read.csv ("data/test.csv" ) #, stringsAsFactors=FALSE)
Ntrain <- nrow(train)
summary(train)

# ahora podemos utilizar clustering para ver si esto nos da mas informacion
df <- changeDataFrame(train, train.new)

# NORMALIZING THE DATA. Para realizar un clustering debemos primero realizar una normalizacion de los
# datos.
require (caret)
# primero preprocesamos
preproc = preProcess(df)
# luego obtenemos el data frame normalizado
df.norm = predict(preproc, df)
summary(df.norm)
# podemos eliminar las variables que eran auxiliares.
remove(train.new)

# podemos definir las variables para el clustering
# no utilizamos ni el ID, ni la fecha ni happy. El resto de las variables que no son las preguntas
# son utilizadas. 

variables   <- c(3,4,5,6,7) # variables a incluir

# como son pocas variables podriamos utilizar un cluster hierarquico para determinar un numero de cluster
# que pueda ser razonable.


# HIERARCHICAL CLUSTERINGg --------------------------------------------------

distances = dist(df[, variables], method = "euclidean")
# Ahora creamos el cluster
clusterTrain = hclust(distances, method="ward")
# dibujamos el dendodrama
plot(clusterTrain)

# del dendrodrama podriamos obtener un numero de clustering adecuado. Algunos numeros adecuados parecen
# 2, 3, 4, 5, 7 y posiblemente 14.
# como queremos hacer grupos de noticias en un blog, parece razonable hacer 5 clusters.
clusterGroups = cutree(clusterTrain, k = 5)

cluster1 <- subset(train, clusterGroups == 1)
cluster2 <- subset(train, clusterGroups == 2)
cluster3 <- subset(train, clusterGroups == 3)
cluster4 <- subset(train, clusterGroups == 4)
cluster5 <- subset(train, clusterGroups == 5)


# Tambien podemos utilizar un modelo con random forest.

# Ahora utilizamos un randomforest
no_incluimos <- c(1,2,3,4,5,6,7)
# Funcion para calcular el AUC de un modelo dados los datos y las columnas que no incluimos.
getAUC <- function (cluster1, no.incluir) {
  require(randomForest)
  require(ROCR)
  set.seed(1000)
  radomb <- randomForest(Happy ~ . , data=cluster1[,-no.incluir], method="class")
  predict.random.1 <- predict(radomb)
  print(getAcc(table(cluster1$Happy, predict.random.1 > 0.5)))
  # Test set AUC 
  ROCRpred = prediction(predict.random.1, cluster1$Happy);
  as.numeric(performance(ROCRpred, "auc")@y.values)  
}

getPrediction <- function (cluster1, no.incluir) {
  require(randomForest)
  require(ROCR)
  set.seed(1000)
  radomb <- randomForest(Happy ~ . , data=cluster1[,-no.incluir], method="class")
  predict.random.1 <- predict(radomb)
  return(predict.random.1)
}



getAUC(cluster1, no_incluimos)
getAUC(cluster2, no_incluimos)
getAUC(cluster3, no_incluimos)
getAUC(cluster4, no_incluimos)
getAUC(cluster5, no_incluimos)
# Pero para calcular el AUC tendriamos que considerar todos los modelos y ponerlos juntos.
p1 <- getPrediction(cluster1, no_incluimos)
p2 <- getPrediction(cluster2, no_incluimos)
p3 <- getPrediction(cluster3, no_incluimos)
p4 <- getPrediction(cluster4, no_incluimos)
p5 <- getPrediction(cluster5, no_incluimos)

train$prop <- 0
train$prop[clusterGroups == 1] <- p1
train$prop[clusterGroups == 2] <- p2
train$prop[clusterGroups == 3] <- p3
train$prop[clusterGroups == 4] <- p4
train$prop[clusterGroups == 5] <- p5

# Ahora tendriamos que ver el AUC
# Test set AUC 
ROCRpred = prediction(train$prop, train$Happy);
as.numeric(performance(ROCRpred, "auc")@y.values)  
# y podríamos comparar con el AUC que podríamos conseguir sin un clustering de los clientes
# de anteriores resultado podemos comprobar que el modelo con todas las variables y un random Forest
# nos da una estimacion del AUC del 0.72.
# getAUC(train, c(1,2))


# ahora sería el momento de realizar el clustering
# ahora realizamos los clusterings
set.seed(888)
KMC = kmeans(df[,variables], centers = 7) # no utilizamos ni el ID, ni la fecha ni happy

# Ahora vamos a incluir una variable que sea cluster y la variable income y todas las preguntas para 
# realizar el modelo

train.cluster <- train
train.cluster$prop <- NULL
train.cluster$cluster <- KMC$cluster

#no.incluir <- c(1,2,3,5,6,7)
no.incluir <- c(1,2)
require(randomForest)
require(ROCR)
set.seed(1000)
radomb <- randomForest(Happy ~ . , data = train.cluster[,-no.incluir], method="class")
predict.rf.cluster <- predict(radomb)

ROCRpred = prediction(predict.rf.cluster, train$Happy);
as.numeric(performance(ROCRpred, "auc")@y.values)

#save(radomb, file = "models/ramdom_forest_with_cluster_2.RData")
# podemos ver cuales son las caracteristicas que parecen contribuir mas.
str(radomb)
importance(radomb)
features <- which(importance(radomb) > 5)
# pero lo importante ahora es que los indices no corresponden a las variables
# a la variable features tenemos que añadirle 3 al primero 
features <- features + c(3,rep(7,length(features)-1))
features <- c(8,features)                              # añadimos la columna Happy
# Tenemos que añadir la variable happy para hacer regresion
# probamos con las variables que parecen destacar
set.seed(1000)
radomb.features <- randomForest(Happy ~ . , data = train.cluster[,c(8,features)], method="class")
predict.rf.cluster <- predict(radomb.features)

ROCRpred = prediction(predict.rf.cluster, train$Happy);
as.numeric(performance(ROCRpred, "auc")@y.values)

radomb.features <- randomForest(Happy ~ Q98197 + votes + Income, data = train, method="class")
predict.rf.cluster <- predict(radomb.features)
ROCRpred = prediction(predict.rf.cluster, train$Happy);
as.numeric(performance(ROCRpred, "auc")@y.values)