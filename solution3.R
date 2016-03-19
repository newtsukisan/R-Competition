# Podemos intentar una seleccion de parametros del modelo a traves de 
# una cross validation.

# a. Cargar los datos para generar los clusters y el modelo
test      <- read.csv("data/test.csv")
train     <- read.csv ("data/train.csv")
# a.1 cargamos los datos normalizados de otras sesiones.
load    ("data/df_normalizado.RData")
load    ("data/testnormalizado.RData")


# b. Generar los cluster.
# podemos definir las variables para el clustering
# no utilizamos ni el ID, ni la fecha ni happy. El resto de las variables que no son las preguntas
# son utilizadas. 

variables   <- c(3,4,5,6,7)                    # variables a incluir
set.seed(888)                                  # generamos la semilla aleatoria
KMC = kmeans(df.norm[,variables], centers = 7) # no utilizamos ni el ID, ni la fecha ni happy

train$cluster <- KMC$cluster                   # add cluster variable

# intentamos utilizar paralelizacion
library(doMC)
registerDoMC(cores = 4)
## All subsequent models are then run in parallel

# Ahora tenemos que ver mediante cross validation cuales pueden ser los mejores parametros para el modelo
require(caret)
require(randomForest)
no.incluir <- c(1,2)                      # no incluimos ni el ID ni la fecha.
# Metodo para realizar la cross validation
fitControl = trainControl( method = "cv", number = 10 )
# ahora mediante caret seleccionamos los mejores parametros para el modelo.
rfmodel <- train(Happy ~ . , 
                 data = train[,-no.incluir],
                 method = "rf",
                 trControl = fitControl)

# ahora tendriamos que usar predict para 
save (rfmodel, file ="models/rfmodel_cross_validated.RData")


predict.rf.cluster <- predict(rfmodel, newdata = train)

# Obtenemos una estimacion del AUC en el conjunto train.
ROCRpred = prediction(predict.rf.cluster, df.norm$Happy);
as.numeric(performance(ROCRpred, "auc")@y.values)

# c. Guardamos todos los datos.

# d. Preparamos los datos de test para el cluster. Para poder predecir con estos datos
require(flexclust)
# para el cluster tenemos que utilizar las columnas normalizadas
KMC.kcca     = as.kcca(KMC, df.norm[,variables])
testClusters = predict(KMC.kcca, newdata = test.norm[,variables])

# 3. Añadir los datos de cluster a los datos de test
test$cluster <- testClusters

# 4. Aplicar el modelo
prediccion.test <- predict(rfmodel, newdata = test)

# 5. Salvar al modelo.
test.salida <- data.frame(UserID = test$UserID, Probability1 = prediccion.test)
write.csv(test.salida[,c(1,2)], file ="solutions/sol4.csv", row.names=FALSE)


