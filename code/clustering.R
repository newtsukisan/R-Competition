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

train     <- read.csv ("data/train.csv") #, stringsAsFactors=FALSE)
train.new <- read.csv ("data/train.csv" , stringsAsFactors=FALSE)
test   <- read.csv ("data/test.csv" ) #, stringsAsFactors=FALSE)
Ntrain <- nrow(train)
summary(train)
# Podemos comprobar que todos los valores que presentan NA se encuentran en el año de nacimieto.
sum(is.na(train))

# intentamos cambiar los que tenemos como "" a empty en uno diferente
train [train == "" ] <- "empty"

# Podríamos comprobar como se comporta si hacemos un clustering.
# Ahora podemos realizar el clustering a traves de k means
set.seed(888)
KMC = kmeans(train[,-c(2,8)], centers = 2)
str(KMC)
train[,-c(2,8)]
KMC = kmeans(train[,c(3,4,5)],centers = 2)


is.factor(train$UserID)
KMC = kmeans(train$EducationLevel, centers = 1)
length(levels (train$EducationLevel))

# para los niveles
l <- levels (train$EducationLevel)
which("" == l)
# queremos sustituir cada elemento por un numero
t <-  sapply (train[,4], function (x){which(x == levels(train[,4]))})
# para cada una de las columnas del data frame queremos comprobar si es un level.
# si es un level, entonces tendremos que sustituirlo por un numero
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
df <- changeDataFrame(train, train.new)
