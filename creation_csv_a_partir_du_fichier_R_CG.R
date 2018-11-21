rm(list=ls())
gc()
library(data.table)
# load("C:/Users/Clement/Desktop/Partage_virtual_box/blablacar/dataset/base_from_25-05-2018_to_01-09-2018_augmente_CG.Rdata")
load("C:/Users/Clement/Desktop/blablacar/dataset/base_from_25-05-2018_to_01-09-2018_augmente_CG.Rdata")
#load("W:/Blablacar/git/base_from_25-05-2018_to_01-09-2018_augmente_CG.Rdata")
#load(W:/Blablacar/base/base_from_25-05-2018_to_01-09-2018.Rdata")
#table(b$departure_city_name)
##Transformation des prix en numérique

#length(unique(paste(base$requete_id,base$permanent_id)))
#Ok on dégage beaucoup de doublon comme ça regarder les doubles si les lignes osnt bien identiques
#Les dégager avant d'enregistrer le csv
b<-data.frame(sapply(base,as.character),stringsAsFactors = FALSE)

base<-NULL
#btest<-b[1:10,]
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
b$price_per_dist=b$price/as.numeric(b$distance)
b$price_per_duration=b$price/as.numeric(b$duration) #dist km
b$vitesse<-as.numeric(b$distance)/as.numeric(b$duration)#secondes
b$id_base<-paste(b$requete_id,b$permanent_id)
b$departure_day<-as.Date(sapply(strsplit(b$departure_day,"/"),function(x){
  return(paste0(x[1],"/",x[2],"/",substr(x[3],3,4)))
}),"%d/%m/%y")
b$day<-wday(b$departure_day,label=TRUE)
b$month<-month(b$departure_day,label=TRUE)
b<-b[!duplicated(b$id_base),]
#doublons
  # duplicated(c(1,1,1,2,2))
  # doublons<-b[duplicated(b$id_base),]
  # #test
  # doublons$id_base[1]
  # b[b$id_base=="2018-05-25 03:21:17 Bourg-en-Bresse Gap 25/05/2018 100 1091271743-bourg-en-bresse-lyon",]
#Exactement les mêmes lignes..
#Enlever les doublons
fwrite(b,file="C:/Users/Clement/Desktop/blablacar/dataset/base_from_25-05-2018_to_01-09-2018_augmente_et_nettoye.csv",quote=TRUE,row.names=FALSE,sep=",")
save.image(file ="C:/Users/Clement/Desktop/blablacar/dataset/base_from_25-05-2018_to_01-09-2018_augmente_et_nettoye.Rdata" )
###Essayer defaire une table associant depcom aux libellés de nom de ville
###idées de stat autour des cartes .


#