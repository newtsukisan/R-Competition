# Como aplicar uno de los modelos a los datos de test.
# Si incluimos una feature que sea cluster, podríamos ver si al aumentar el numero de klusterings 
# aumentamos los valores del AUC, es decir añadimos complejidad al sistema.

# Una forma de evaluar el modelo es a traves de una cross validation. Viendo si el parametro de complejidad
# es alto o es bajo. 

# cargamos los datos que ya tenemos normalizados.
train     <- read.csv ("data/train.csv") #, stringsAsFactors=FALSE)
load("data/df_normalizado.RData")

# Ahora podemos realizar el clustering de los datos.
# ahora realizamos los clusterings
set.seed(888)
KMC = kmeans(df.norm[,-c(1,2,8)], centers = 1000) # no utilizamos ni el ID, ni la fecha ni happy

train$cluster <- KMC$cluster
# dividimos en train y test

require (rpart)
require (rpart.plot)
#Podríamos realizar un validacion cruzada
# Install cross-validation packages
library(caret)
library(e1071)
set.seed (111)
# Define cross-validation experiment
# Aqui lo que hacemos es definir como metodo para la validación cross validation y 10
# grupos diferentes para realizar la cross validation
fitControl = trainControl( method = "cv", number = 10 )
cartGrid   = expand.grid( .cp = (1:50)*0.01) # parametros de complejidad utilizados

# Perform the cross validation
train(Happy ~ ., 
      data = train,             # datos para entrenar
      method = "rpart",         # metodo a utilizar para la clasificacion
      trControl = fitControl,   # control de la validacion
      tuneGrid = cartGrid )     # variacion del parametro de complejidad del modelo

# Ahora construimos el modelo

cart.cross  <- rpart(Happy ~., data = train,control=rpart.control(cp = 0.02))
predit.cart <- predict(cart.cross )
prp(cart.cross)

ROCRpred = prediction(predit.cart , train$Happy);
as.numeric(performance(ROCRpred, "auc")@y.values)

# PRUEBAS CON RANDOM FOREST

require(randomForest)
require(ROCR)
set.seed(1000)
radomb <- randomForest(Happy ~ . , 
                       data = train[,-c(2)],
                       do.trace = 10,
                       ntree    = 100,
                       method="class")
predict.rf.cluster <- predict(radomb)


ROCRpred = prediction(predict.rf.cluster, train$Happy);
as.numeric(performance(ROCRpred, "auc")@y.values)

save (radomb, file = "models/random_forest_100_todos_menos_fechas.RData")
