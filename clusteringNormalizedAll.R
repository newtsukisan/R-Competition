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

save (df.norm,  file ="data/df_normalizado.RData")
# podemos definir las variables para el clustering
# no utilizamos ni el ID, ni la fecha ni happy. El resto de las variables que no son las preguntas
# son utilizadas. 

variables   <- c(3,4,5,6,7) # variables a incluir

# ahora sería el momento de realizar el clustering
# ahora realizamos los clusterings
set.seed(888)
KMC = kmeans(df[,-c(1,2,8)],   centers = 100) # no utilizamos ni el ID, ni la fecha ni happy
cluster1 <- subset(train,KMC$cluster ==1)
cluster2 <- subset(train,KMC$cluster ==2)

table(subset(train,KMC$cluster ==4)$Happy)
