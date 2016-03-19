# kaggel competition.
# 1. hacer algun tipo de clustering
# 2. como es una encuesta, en lugar de realizar una imputación de los diferentes valores, podríamos considerar
# la ausencia de información como un dato mas. Es decir que si deja de contestar a una pregunta, lo podríamos 
# incluir en el analisis en lugar de imputarlo o imaginarnos cual podría ser la respuesta. 


# usefull data and functios -----------------------------------------------

blanck <- ""       # empty data for some questions.

# Funcion para calcular el porcentaje de elementos que cumplen ser iguales a un determinado valor de 
# columna:  columna que estamos analizando
# valor:    valor al que tienen que se iguales
# N:        Numero total de datos.
inside   <- function (columna, valor,N) {as.data.frame(table(columna == valor)/N)[2,2]}


# Para hacer algunos calculos de como acerca de la distribucion de valores en cada caso.
auxiliar <- function (col, col2, i){as.data.frame(tapply(col, col2 == i,sum)/length(col))[2,1]}

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

# funcion para sustituir todos los valores de la funcion dada por otro determinado valor
# Todos los valores del data frame df son substituidos por subs
# df:       DataFrame con los datos a cambiar
# valor:    Valor que queremos substituir
# subs :    Valor por el que sustituimos.
substituirInDataFrame <- function (df, valor, subs) {
  as.data.frame(lapply(df, function(x){replace(x, x == valor, subs)}))
}

# loading the data --------------------------------------------------------

train     <- read.csv ("data/train.csv") #, stringsAsFactors=FALSE)
train.new <- read.csv ("data/train.csv" , stringsAsFactors=FALSE)
test   <- read.csv ("data/test.csv" ) #, stringsAsFactors=FALSE)
Ntrain <- nrow(train)
summary(train)

# ahora podemos utilizar clustering para ver si esto nos da mas informacion
df <- changeDataFrame(train, train.new)

# ahora realizamos los clusterings
set.seed(888)
KMC = kmeans(df[,-c(1,2,8)], centers = 2) # no utilizamos ni el ID, ni la fecha ni happy
str(KMC)
summary(KMC$centers)
cluster1 <- subset(train,KMC$cluster == 1)
cluster2 <- subset(train,KMC$cluster == 2)
table(cluster1$Happy)
table(cluster2$Happy)

# Luego podemos hacer clustering y añadirlos para ver si sacamos mas informacion de esta manera.
# ahora realizamos los clusterings
set.seed(888)
KMC = kmeans(df[,-c(1,2,8)], centers = 100) # no utilizamos ni el ID, ni la fecha ni happy
# ahora tenemos que utilizar estos datos para los modelos. Por ejemplo, en lugar de las preguntas podemos
# crear un data frame que utilice la informacion de los clustering.
train.clustered         <- data.frame(Happy = train$Happy)
train.clustered$cluster <- KMC$cluster
train.clustered$votes   <- train$votes
# tambien podemos incluir las preguntas que son mas respondidas.
# podriamos intentar ver cuales son las preguntas a las que la gente mas responde la gente para incluirlas
# dentro de nuestro modelo.
# podemos sacar el indice en el que todas las preguntas son las mas respondidas
preguntas   <- train [,9:109]
respondidas <- apply(preguntas, 2, function (c){sum (c != "")})
summary(respondidas)

indice_preguntas <- which (respondidas > 10)
n_col            <- ncol(train.clustered) + 1
final            <- n_col + length(indice_preguntas)
nuevas_col       <- n_col:final
train.clustered[,nuevas_col] <- train[,indice_preguntas]
#ahora intentariamos ver si podemos ajustar consiguiendo mejores resultados.
train.clustered$Happy.1 = NULL
require (rpart)
require (rpart.plot)
model.tree <- rpart(Happy ~ . , data = train.clustered, method="class")
prp(model.tree)

# Veamos cual es la accuracy de lo que tenemos con un corte del 0.5
train.predict <- predict(model.tree)[,2]
getAcc(table(train.clustered$Happy, train.predict > 0.3))

# Luego podemos hacer clustering y añadirlos para ver si sacamos mas informacion de esta manera.
# ahora realizamos los clusterings
set.seed(888)
KMC = kmeans(df[,c(3,4,5,6,7)], centers = 50) # no utilizamos ni el ID, ni la fecha ni happy
# ahora tenemos que utilizar estos datos para los modelos. Por ejemplo, en lugar de las preguntas podemos
# crear un data frame que utilice la informacion de los clustering.
train.clustered         <- data.frame(Happy = train$Happy)
train.clustered$cluster <- KMC$cluster
train.clustered$votes   <- train$votes
# tambien podemos incluir las preguntas que son mas respondidas.
# podriamos intentar ver cuales son las preguntas a las que la gente mas responde la gente para incluirlas
# dentro de nuestro modelo.
# podemos sacar el indice en el que todas las preguntas son las mas respondidas
cluster1 <- subset(df, KMC$cluster == 1)
cluster2 <- subset(df, KMC$cluster == 2)
cluster3 <- subset(df, KMC$cluster == 3)
cluster4 <- subset(df, KMC$cluster == 4)
require (rpart)
require (rpart.plot)
model.tree <- rpart(Happy ~ . , data = cluster2[,-c(1,2,3,4,5,6,7)], method="class")
prp(model.tree)

# Veamos cual es la accuracy de lo que tenemos con un corte del 0.5
train.predict <- predict(model.tree)[,2]
getAcc(table(cluster2$Happy, train.predict > 0.5))

table(cluster2$Happy, train.predict > 0.5)

# Tambien podemos utilizar un modelo con random forest.
require(randomForest)
# Ahora utilizamos un randomforest
set.seed(1000)
radomb <- randomForest(Happy ~ . , data=cluster4[,-c(1,2,3,4,5,6,7)], method="class")
predict.random.1 <- predict(radomb)
getAcc(table(cluster4$Happy, predict.random.1 > 0.5))

# Tambien podriamos ver que pasa si ponemos un numero asignando cluster 
df$cluster <- KMC$cluster

# Ahora utilizamos un randomforest
set.seed(1000)
radomb <- randomForest(Happy ~ . , data=df[,-c(1,2)], method="class")
predict.random.1 <- predict(radomb)
getAcc(table(df$Happy, predict.random.1 > 0.5))
