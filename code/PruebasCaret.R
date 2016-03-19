# PRUEBAS PARA UTILIZAR CARET EN LA DETERMINACIÓN DE LOS MODELOS PREDICTIVOS
# Cargamos las liberias que nos van a hacer falta

require(caret)
require(mlbench)
data(Sonar)

set.seed(107)

inTrain <- createDataPartition(y = Sonar$Class,
                              ## the outcome data are needed
                              p = .75,
                              ## The percentage of data in the
                              ## training set
                              list = FALSE)
                              ## The format of the results
## The output is a set of integers for the rows of Sonar
## that belong in the training set.
str(inTrain)

# Ahora ya podemos tener los dos grupos, de test y de training.
training <- Sonar[ inTrain,]
testing  <- Sonar[-inTrain,]
nrow(training)

# Ahora podemos entrenar un modelo. caret ofrece una sintaxis comun para entrenar los modelo
# Para controlar el training method
ctrl <- trainControl(method = "repeatedcv",
                       repeats = 3,
                       classProbs = TRUE,
                       summaryFunction = twoClassSummary)


plsFit <- train(Class ~ .,
                 data = training,
                 method = "pls",
                 ## Center and scale the predictors for the training
                 ## set and all future samples.
                 preProc = c("center", "scale"))


## To illustrate, a custom grid is used
require(rpart)
require(randomForest)
cartGrid = expand.grid( .cp = (1:50)*0.01) # parametros de complejidad utilizados
set.seed(123)
rdaFit <- train(Class ~ .,
                  data = training,
                  method = "rf",
                  trControl = ctrl)

# Podemos entrenar el modelo y mediante cross validation realizar la mejor estimacion de los parametros
# Luego, basicamente para utilizar este sistema, definimos un control y dejamos que mediante cross validation
# podamos obtener el mejor modelo. Podríamos intentarlo tambien con el sistema boruta para la 
# seleccion de las features.


