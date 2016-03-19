# PROGRAMA PARA OBTENER LAS "LEARNING CURVES" DE UN MODELO.
# De esta forma podremos saber si tenemos overfitting o underfitting con nuestro modelo.
# En el caso de tener overfitting, tendremos que aumentar la regularizacion y/o disminuir
# la complejidad del modelo. Tambien necesitariamos mas datos.
# Si tenemos un caso de underfitting, tendremos que disminuir la regularizacion y/o aumemtar la
# complejidad del sistema

# 2. Crear un bucle donde.
# 2.a Tomemos un numero de elementos
# 2.b Realicemos el ajuste del modelo con ese numero de elementos en train.
# 2.c Calculemos el error para train
# 2.d Calculemos el error para validation
# 2.e Devolvamos dos listas de vectores.
# Pasos
# 1. Tener dos grupos de datos. Validacion y training.
train  <- read.csv ("data/train.csv")
Ntrain <- nrow(train)
# primer ajuste -----------------------------------------------------------
library(caTools)
set.seed(123)
split = sample.split(train$Happy, SplitRatio = 0.7)  # Utilizamos el 30 % en validacion

train1     = subset(train, split==TRUE)
validation = subset(train, split==FALSE)



library(doMC)
registerDoMC(cores=2)
require(foreach)

N.data <- 1000
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
  rpart(Happy ~ ., data=train1[incluir[[i]],-c(1,2)], method="class")
}

save (modelos, file = "models/lista-modelos.RData")

# Ahora necesitariamos una lista con los errores. 
# Aproximacion para el error en el conjunto de training
require(ROCR)
train.error <- foreach (i = 1:n.ptos, .combine=c) %dopar% {
  train.predict <- predict(modelos[[i]])[,2];
  ROCRpred = prediction(train.predict, train1[incluir[[i]],]$Happy);
  1-as.numeric(performance(ROCRpred, "auc")@y.values)
}
# Vector con los valores de error en el grupo de validacion
validation.error <- foreach (i = 1:n.ptos, .combine=c) %dopar% {
  train.predict <- predict(modelos[[i]], newdata = validation)[,2]
  ROCRpred = prediction(train.predict, validation$Happy);
  1-as.numeric(performance(ROCRpred, "auc")@y.values)
}

# Ahora tendriamos que dibujar los datos para visualizarlos.

data <- data.frame(m = 1:n.ptos, val.error = validation.error, train.error = train.error)

require(ggplot2)
p <- ggplot(data, aes())
p +
  geom_smooth(aes(x = m, y = val.error, stat="loess", colour = "Validation error"))   +
  geom_smooth(aes(x = m, y = train.error, stat="loess", colour = "Training error"))+
  geom_line(aes(m, train.error, colour = "Training error") ) + 
  geom_line(aes(m, val.error, colour = "Validation error"))

