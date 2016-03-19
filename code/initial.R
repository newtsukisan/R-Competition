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

# funcion para sustituir todos los valores de la funcion dada por otro determinado valor
# Todos los valores del data frame df son substituidos por subs
# df:       DataFrame con los datos a cambiar
# valor:    Valor que queremos substituir
# subs :    Valor por el que sustituimos.
substituirInDataFrame <- function (df, valor, subs) {
  as.data.frame(lapply(df, function(x){replace(x, x == valor, subs)}))
}

# loading the data --------------------------------------------------------

train  <- read.csv ("data/train.csv")
test   <- read.csv ("data/test.csv" )
Ntrain <- nrow(train)
summary(train)
# Podemos comprobar que todos los valores que presentan NA se encuentran en el año de nacimieto.
sum(is.na(train))

# Preanalisis -------------------------------------------------------------

# como detectamos los valores que no estan presentes??
# cuantos de los que contestan a todas la preguntas son felices.
# Cual es el porcentaje de personas que contestan a todas la preguntas
as.data.frame(tapply(train$Happy, train$votes == 101,sum)/Ntrain)[2,1]
respondones <- as.data.frame(table(train$votes == 101)/nrow(train))[2,2]

hist(train$votes)
lista <- sapply(20:101, function (i) {auxiliar(train$Happy, train$votes, i)})

# cambiamos los datos del año.
train$YOB[is.na(train$YOB)] <- 0

# ponemos todas la preguntas a vacío --------------------------------------
# para reemplazar los factores tenemos que utilizar otras formas 



# primer ajuste -----------------------------------------------------------
library(caTools)
set.seed(123)
split = sample.split(train$Happy, SplitRatio = 0.7)

trainSparse = subset(train, split==TRUE)
testSparse  = subset(train, split==FALSE)


require (rpart)
require (rpart.plot)
model.tree <- rpart(Happy ~ ., data=trainSparse[,-c(1,2)], method="class")
prp(model.tree)


# Veamos cual es la accuracy de lo que tenemos con un corte del 0.5
train.predict <- predict(model.tree)[,2]
getAcc(table(trainSparse$Happy, train.predict > 0.5))
getAcc(table(trainSparse$Happy, train.predict > 0.55))

require(ROCR)
ROCRpred = prediction(train.predict, trainSparse$Happy);
as.numeric(performance(ROCRpred, "auc")@y.values)

# veamos cual es la prediccion con el modelo de linea base.
table(trainSparse$Happy)
1823/nrow(trainSparse)

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
      data = trainSparse,       # datos para entrenar
      method = "rpart",         # metodo a utilizar para la clasificacion
      trControl = fitControl,   # control de la validacion
      tuneGrid = cartGrid )     # variacion del parametro de complejidad del modelo

predict.test <- predict(model.tree, newdata = testSparse)[,2]
getAcc(table(testSparse$Happy, predict.test > 0.5))
ROCRpred = prediction(predict.test, testSparse$Happy);
as.numeric(performance(ROCRpred, "auc")@y.values)


# Tambien podemos utilizar un modelo con random forest.
require(randomForest)
# Ahora utilizamos un randomforest
set.seed(1000)
radomb <- randomForest(Happy ~ . , data=trainSparse[,-2], method="class")
predict.random.1    <- predict(radomb)
predict.random.test <- predict(radomb, newdata = testSparse)
getAcc(table(trainSparse$Happy, predict.random.1 > 0.5))

ROCRpred = prediction(predict.random.1, trainSparse$Happy);
as.numeric(performance(ROCRpred, "auc")@y.values)

# Podriamos ver el AUC para el test y ver posible resultado tendríamos
ROCRpred = prediction(predict.random.test, testSparse$Happy);
as.numeric(performance(ROCRpred, "auc")@y.values)

# podemos ver tambien que sucede si solo utilizamos la variables votes
votes.rpart   <- rpart(Happy ~ votes + Income, data=trainSparse[,-2], method="class")
predict.votes <- predict(votes.rpart)[,2]
getAcc(table(trainSparse$Happy, predict.votes > 0.5))
prp(votes.rpart)
# vemos que podemos encontrar con solo los votos
ROCRpred = prediction(predict.votes, trainSparse$Happy);
as.numeric(performance(ROCRpred, "auc")@y.values)

# podriamos intentar ver cuales son las preguntas a las que la gente mas responde la gente para incluirlas
# dentro de nuestro modelo.
# podemos sacar el indice en el que todas las preguntas son las mas respondidas
preguntas   <- train [,9:109]
respondidas <- apply(preguntas, 2, function (c){sum (c != "")})
summary(respondidas)

indice_preguntas <- which (respondidas > 3304)
# podemos ver tambien que sucede si solo utilizamos la variables votes
votes.rpart   <- rpart(Happy ~ ., data=trainSparse[,indice_preguntas], method="class")
predict.votes <- predict(votes.rpart)[,2]
getAcc(table(trainSparse$Happy, predict.votes > 0.5))
prp(votes.rpart)
