# aplicacion del modelo a los datos de test.
# 25-04-2014 creamos el modelo a partir de los datos normalizados. 

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



# a. Cargar los datos para generar los clusters y el modelo
train     <- read.csv ("data/train.csv") #, stringsAsFactors=FALSE)
load("data/df_normalizado.RData")
test      <- read.csv ("data/test.csv")
test.new  <- read.csv ("data/test.csv", stringsAsFactors=FALSE) # Para realizar la normalizacion


# b. Generar los cluster.
# podemos definir las variables para el clustering
# no utilizamos ni el ID, ni la fecha ni happy. El resto de las variables que no son las preguntas
# son utilizadas. 

variables   <- c(3,4,5,6,7)                    # variables a incluir
set.seed(888)                                  # generamos la semilla aleatoria
KMC = kmeans(df.norm[,variables], centers = 7) # no utilizamos ni el ID, ni la fecha ni happy

train$cluster <- KMC$cluster                   # add cluster variable
df.norm$cluster    <- KMC$cluster
# c. Entrenamos el modelo

no.incluir <- c(1,2)                      # no incluimos ni el ID ni la fecha.
require(randomForest)
require(ROCR)
set.seed(1000)
# utilizamos el conjunto normalizado de datos.
modelo <- randomForest(Happy ~ . , 
                       data     = df.norm[,-no.incluir], 
                       do.trace = 100,
                       method="class")
predict.rf.cluster <- predict(modelo)
# Obtenemos una estimacion del AUC en el conjunto train.
ROCRpred = prediction(predict.rf.cluster, df.norm$Happy);
as.numeric(performance(ROCRpred, "auc")@y.values)

# guardamos el modelo
save (modelo, file ="models/randomforest_normalizado_7_cluster.RData")

# 1. Tenemos que normalizar los datos de test
test.n <- changeDataFrame(test, test.new)       # para obtener los datos
remove(test.new)                                # para ahorrar memoria
require (caret)
# primero preprocesamos
preproc = preProcess(test.n)
# luego obtenemos el data frame normalizado
test.norm = predict(preproc, test.n)
summary (test.norm)                              # deben aparecer todos con media cero
save    (test.norm, file ="data/testnormalizado.RData")
# 2. Generar los cluster a partir de los cluster generados

require(flexclust)

KMC.kcca     = as.kcca(KMC, df.norm[,variables])
testClusters = predict(KMC.kcca, newdata = test.norm)

# 3. Añadir los datos de cluster a los datos de test
test.norm$cluster <- testClusters

# 4. Aplicar el modelo
prediccion.test <- predict(modelo, newdata = test.norm)

# 5. Salvar al modelo.