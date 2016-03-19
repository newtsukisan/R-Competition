# Podemos buscar las features que son representativas.

require(Boruta)
# Realizando feature selection 
set.seed(777);

# loading the data --------------------------------------------------------

train  <- read.csv ("data/train.csv")
test   <- read.csv ("data/test.csv" )

#Run Boruta on this data
Boruta(Happy ~.,data=train[,-c(1,2)],doTrace=2)->Boruta.iris.extended
