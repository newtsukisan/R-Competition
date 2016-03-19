# PROGRAMA PARA OBTENER LAS "LEARNING CURVES" DE UN MODELO.
# De esta forma podremos saber si tenemos overfitting o underfitting con nuestro modelo.
# En el caso de tener overfitting, tendremos que aumentar la regularizacion y/o disminuir
# la complejidad del modelo. Tambien necesitariamos mas datos.
# Si tenemos un caso de underfitting, tendremos que disminuir la regularizacion y/o aumemtar la
# complejidad del sistema

#function for getting error rate in classification process
misclass <- function(tt){
  #parameters
  #tt         table(predict,correcto)
  #output value of error rate
  sum(tt[row(tt) != col(tt)]) / sum(tt)
}
#Funcion para obtener los errores de clasficiacion
getErrorRate <- function(model, data.test, trueValues){
  misclass(table(predict(model,data.test),trueValues))
}



# 2. Crear un bucle donde.
# 2.a Tomemos un numero de elementos
# 2.b Realicemos el ajuste del modelo con ese numero de elementos en train.
# 2.c Calculemos el error para train
# 2.d Calculemos el error para validation
# 2.e Devolvamos dos listas de vectores.
# Pasos
# 1. Tener dos grupos de datos. Validacion y training.
load ("data/samsungData.rda")


# preparing data ----------------------------------------------------------
#as there are some repeated names, we change all names.
colnames(samsungData) <- c(sapply(1:563,function(i) paste0("var",as.character(i))))
colnames(samsungData)[562] <- "subject"
colnames(samsungData)[563] <- "activity"
samsungData$activity <- as.factor(samsungData$activity)

# primer ajuste -----------------------------------------------------------
library(caTools)
set.seed(123)
split = sample.split(samsungData$activity, SplitRatio = 0.7)  # Utilizamos el 30 % en validacion

train1     = subset(samsungData, split==TRUE)
validation = subset(samsungData, split==FALSE)



library(doMC)
registerDoMC(cores=2)
require(foreach)


require (rpart)
require (rpart.plot)
# Obtenemos los diferentes tipos de errores.
N.data <- nrow(train1)                      # Numero total de datos en el training set
n.ptos <- 40                                # Numero de puntos que tenemos
# Para utilizar internamente
incremento <- floor(N.data/n.ptos)
indice     <- seq(incremento,n.ptos*incremento,incremento)
incluir    <- foreach (i = indice) %dopar% {
  sample(1:N.data, i);
}
# Generamos los modelos
modelos <- foreach (i = 1:n.ptos) %dopar% {
  # Querriamos obtener una lista de modelos.
  # Aqui tenemos que incluir la parte del modelo
  rpart(activity ~ var559+var560, data=train1[incluir[[i]],], method="class")
}

save (modelos, file = "models/lista-modelos-SamsungData.RData")
load ("models/lista-modelos-SamsungData.RData")

# Ahora necesitariamos una lista con los errores. 
# Aproximacion para el error en el conjunto de training
# Necesitariamos una forma de evaluar los errores en multiclass
require(ROCR)
validation.error <- foreach (i = 1:n.ptos, .combine=c) %dopar% {
  train.predict <- predict(modelos[[i]], newdata = validation, type = "class");
  tt <- table(train.predict,validation$activity)
  sum(tt[row(tt) != col(tt)]) / sum(tt)
}
# Vector con los valores de error en el grupo de validacion
train.error <- foreach (i = 1:n.ptos, .combine=c) %dopar% {
  train.predict <- predict(modelos[[i]], newdata = train1[incluir[[i]],], type = "class")
  tt <- table(train.predict,train1[incluir[[i]],]$activity)
  sum(tt[row(tt) != col(tt)]) / sum(tt)
}

# Ahora tendriamos que dibujar los datos para visualizarlos.

data <- data.frame(m = 1:n.ptos, val.error = validation.error, train.error = train.error)

require(ggplot2)
p <- ggplot(data, aes())
p +
  geom_smooth(aes(x = m, y = val.error, stat="loess", colour = "Validation error"))   +
  geom_smooth(aes(x = m, y = train.error, stat="loess", colour = "Training error")) #+
  #geom_line(aes(m, train.error, colour = "Training error") ) + 
  #geom_line(aes(m, val.error, colour = "Validation error"))
# Parece que tendriamos un caso de overfitting por la separacion entre los dos errores.

require(randomForest)
model.forest.1 <- randomForest(activity ~ ., 
                               data = samsungData,
                               do.trace=100,
                               ntree=500,
                               model = FALSE)

importance(model.forest.1)
samsungData$var559
