
library(data.table)
load("C:/Users/Clement/Desktop/Partage_virtual_box/blablacar/dataset/base_from_25-05-2018_to_01-09-2018.Rdata")

#table(b$departure_city_name)
##Transformation des prix en numérique
b$price<-sapply(b$price,function(x){
  #x<-btest$price[1]
  as.numeric(sub(substr(x,(nchar(x)-1),nchar(x)),"",x))
  })

b$price_with_commision<-sapply(b$price_with_commision,function(x){
  #x<-btest$price_with_commision[1]
  as.numeric(sub(",",".",sub(substr(x,(nchar(x)-1),nchar(x)),"",x)))
})

#je vais me servir de l'id de la requête pour définir les variables
#query_departure_city_name and query_arrival_city_name pour avoir les différences entre la ville demandée

# btest=b[1:10,]
b$query_departure_city_name<-sapply(strsplit(b$requete_id," "),function(x){x[3]})
b$query_arrival_city_name<-sapply(strsplit(b$requete_id," "),function(x){x[4]})
b$departure_day<-sapply(strsplit(b$departure_dates," "),function(x){x[1]})
b$departure_time<-sapply(strsplit(b$departure_dates," "),function(x){x[2]})
fwrite(b,file="C:/Users/Clement/Desktop/Partage_virtual_box/blablacar/dataset/base_from_25-05-2018_to_01-09-2018_CG.csv",quote=TRUE,row.names=FALSE,sep=",",)

###Essayer defaire une table associant depcom aux libellés de nom de ville
###idées de stat autour des cartes .