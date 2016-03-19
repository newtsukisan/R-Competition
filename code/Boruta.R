require(Boruta)
# Realizando feature selection 
set.seed(777);
#Add some nonsense attributes to iris dataset by shuffling original attributes
iris.extended<-data.frame(iris,apply(iris[,-5],2,sample));
names(iris.extended)[6:9]<-paste("Nonsense",1:4,sep="");
#Run Boruta on this data
Boruta(Species~.,data=iris.extended,doTrace=2)->Boruta.iris.extended
#Nonsense attributes should be rejected
print(Boruta.iris.extended);
#Boruta using rFerns􏰀 importance
Boruta(Species~.,data=iris.extended,getImp=getImpFerns)->Boruta.ferns.irisE
print(Boruta.ferns.irisE);
Boruta.ferns.irisE$finalDecision == "Confirmed"
