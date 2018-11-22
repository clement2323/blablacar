rm(list=ls())
gc()

library(lubridate)
library(data.table)
library(ggplot2)


library(classInt)
library(leaflet)
library(htmlwidgets)
library("sp")
library("rgdal")
library("RColorBrewer")
library("ggplot2")
library(leaflet.extras)
library(haven)
library(rgeos)
library(spdep)
library(maptools)


library("FactoMineR")
library("factoextra")

#TO DO 
#Extirper les variables surpplémentaires des requêtes
#à savoir, prix recommandé "recomanded price", pager_total qui va nous donner le nombre de voyage qu'on aurait pu avoir
#avec une requête bien que la limite autorisée soit de 100
#l'ordre aléatoire dans le requêtage du scrapping est volontaire car on ne veut pas enquêter un voyage à chaque fois à la même heure


##Objectif, faire un  modèle de régression où le prix est expliqué par les variables de bablacar
##Et un autre où les déterminants géographiques et les caractéristiques des chauffeurs jouent

#Sur une carte faire apparaitre les flux les plus demandés dans notre base(suffit de faire des traits gros selon le nombre d'occurences)

##Modèle de durée sur temps continu mais observation discrète à étudier pour omprendre le taux de remplissage
##Tauix de remplissage qui nous aidera aussi pour le prix
#survival analysis in discret time
#Machine learning livre de thomas,elements of machine learning springer hastier

#Demander un cluster aux informaticiens de l'ENSAE, les appeler pour avooir tot en réseau + la puissance de calcul  

##Base IGN pur faire le lien entre les latitudes longitude et les depcom, ou y a lelr à coup de ctrl c ctrl v +lowcase
##But avec les depcom on va pouvoir avoir ele tyoe de territoire de la ville de départ et d'arrivée ce qui va influer sur le prix (utilisation des aires urbaines)
##Présence ou non de péage sur la route, ça doit forcmnt joué
##En clair soit on essaie de récupérer ces informations liées au coupke origine trajet sur des sources externes,
# Soit on s'amuse à mettre des dummies au pour chaque couple qui explqiuera l'effet fixe.
#si trop de dummies on peut faire des couples départements départements
#Question, quid des valeurs manquantes dans les modèles de régression??
###Existe t'il des indicateurs d'accessibilité des communes ?

##comprendre le prix recommandé, ce dernier est attaché à une requête 
##Mais peut-être que ce prix recommandé évolue au cours du temps ? selon quels facteurs ?
##Faire un modèle utilisant ce prix (ce sera le modèle utilisant les variables de blablacar)

##A combien de temps une  offre apparait-elle en moyenne avant  de sa date de départ ??

# base<-fread(input="C:/Users/Clement/Desktop/blablacar/dataset/base_from_25-05-2018_to_01-09-2018_augmente_et_nettoye.csv",sep=","
#       ,stringsAsFactors = FALSE
#       ,colClasses = list(charater=c("requete_id","departure_dates","price_color","permanent_id","departure_city_name","arrival_city_name","model","make","id","comfort",
#                                     "freeway","booking_mode","booking_type","trip_details_id","query_departure_city_name","query_arrival_city_name","departure_day","departure_time","id_base")
#                          ,numeric=c("price","price_with_commision","seats_left","seats","duration","distance","departure_latitude","departure_longitude","arrival_latitude","arrival_longitude"))
#       ,encoding = "UTF-8")
#load("C:/Users/Clement/Desktop/blablacar/dataset/base_from_25-05-2018_to_01-09-2018_augmente_et_nettoye.Rdata")
load("C:/Users/Clement/Desktop/blablacar/dataset/base_niveau_voyage.Rdata")

#Latin-1

#####setNames pour récupérer directement les villes avec leur latitudes chopper les latitudes et longitudes des requêtes etc..

#table ville coordonnée
# 
# d1<-data.frame(ville=b$departure_city_name,latitude=b$departure_latitude,longitude=b$departure_longitude,stringsAsFactors = FALSE)
# d2<-data.frame(ville=b$arrival_city_name,latitude=b$arrival_latitude,longitude=b$arrival_longitude,stringsAsFactors = FALSE)
# d3<-rbind(d1,d2)
# d3$latitude<-as.numeric(d3$latitude)
# d3$longitude<-as.numeric(d3$longitude)
# ville_vers_latitude<-setNames(d3$latitude,d3$ville)
# ville_vers_longitude<-setNames(d3$longitude,d3$ville)
##Appproximatif vu que R prend la première coord qui vient
# coords_ville<-data.frame(latitude=ville_vers_latitude[unique(d3$ville)], longitude= ville_vers_longitude[unique(d3$ville)] ,row.names= unique(d3$ville))
# saveRDS(object = coords_ville,file = "C:/Users/Clement/Desktop/blablacar/dataset/coords_ville.Rds")

##Test sur une journée
coords_ville<-readRDS(file = "C:/Users/Clement/Desktop/blablacar/dataset/coords_ville.Rds" )
coords_ville$city_name<-row.names(coords_ville)
###Faire des petites bases de test sur un jour
##Je sauve la base des coordonnées après
##Filtre sur un paris Rennes
##"On fera passer les couples en argument par la suite
b_niveau_voyage<-b[!duplicated(b$permanent_id),]
rm(b) 
gc()

#test<-b_niveau_voyage[b_niveau_voyage$query_departure_city_name=="Paris" & b_niveau_voyage$query_arrival_city_name=="Rennes",]

carte_res_requete<-function(query_departure_city,query_arrival_city,m=NULL){
#query_departure_city<-"Bourges"; query_arrival_city<-"Gap"
#query_departure_city<-"Toulouse"; query_arrival_city<-"Montauban"  
test<-b_niveau_voyage[b_niveau_voyage$query_departure_city_name==query_departure_city & b_niveau_voyage$query_arrival_city_name==query_arrival_city,]

#unique(paste(b_niveau_voyage$query_departure_city_name,b_niveau_voyage$query_arrival_city_name))
depart<-cbind(coords_ville[test$query_departure_city_name,],coords_ville[test$departure_city_name,])
names(depart)<-c("latitude_query","longitude_query","city_name_query","latitude","longitude","city_name")
arrivee<-cbind(coords_ville[test$query_arrival_city_name,],coords_ville[test$arrival_city_name,]) ##3,4 vrai départ et arrivée; 1, départ et arrivée demandée
names(arrivee)<-c("latitude_query","longitude_query","city_name_query","latitude","longitude","city_name")

if(is.null(m)){
m <- leaflet() %>%
  # addTiles()
  addProviderTiles(providers$OpenStreetMap.Mapnik) 
}

for(i in seq_along(depart$city_name)){
  m<-addPolylines(map = m,c(depart$longitude[i],arrivee$longitude[i]), c(depart$latitude[i],arrivee$latitude[i]),group="liens" ,
                  color="purple",
                  weight=1)
}
m<- addCircles(map=m, lng =arrivee$longitude , lat =arrivee$latitude , radius = 5, layerId = NULL,
               group = "villes d'arrivée", stroke = TRUE, color ="red",
               weight = 3,
               opacity = 0.8, fill = TRUE, fillColor = "red"
               , fillOpacity = 0.7,
               dashArray = NULL, label = arrivee$city_name, popupOptions = NULL
               
)
m<- addCircles(map=m, lng =depart$longitude , lat =depart$latitude , radius = 5, layerId = NULL,
               group = "villes de départ", stroke = TRUE, color ="blue",
               weight = 3,
               opacity = 0.8, fill = TRUE, fillColor = "red"
               , fillOpacity = 0.7,
               dashArray = NULL, label = depart$city_name, popupOptions = NULL
               
)

m<- addCircles(map=m, lng =c(depart$longitude_query[1],arrivee$longitude_query[1]) , lat =c(depart$latitude_query[1],arrivee$latitude_query[1]) , radius = 10, layerId = NULL,
               group = "villes demandées", stroke = TRUE, color ="black",
               weight = 7,
               opacity = 1, fill = TRUE, fillColor = "red"
               , fillOpacity = 1,
               dashArray = NULL, label = c(depart$city_name_query[1],arrivee$city_name_query[1]), popupOptions = NULL
               
) 
# m<-addPolylines(map = m,c(depart$longitude_query[1],arrivee$longitude_query[1]), c(depart$latitude_query[1],arrivee$latitude_query[1]),group="liens" ,
#                       color="black",
#                       weight=3)

                
m<-addLayersControl(m,
                    overlayGroups = c("villes d'arrivée","villes de départ","villes demandées","liens"),
                    options = layersControlOptions(collapsed = FALSE)
)
m
}

# b$distance ##distance en km
# b$duration ##duration en secondes

m<-carte_res_requete("Bourges","Gap")
carte_res_requete("Toulouse","Montauban",m=m)

ggplot(b_niveau_voyage, aes(x=vitesse, y=price)) + geom_point()
lapply(split(b_niveau_voyage,b_niveau_voyage$comfort),function(x){summary(x$price)})

# $basique
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 2.00    7.00   13.00   17.62   25.00   88.00 
# 
# $confortable
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 1.0     8.0    15.0    18.9    26.0   198.0 
# 
# $luxe
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 2.00    9.00   17.00   20.81   29.00  147.00 
# 
# $normal
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 1.0     7.0    15.0    18.7    26.0   756.0 

lapply(split(b_niveau_voyage,b_niveau_voyage$comfort),function(x){summary(x$price_per_dist)})
# $basique
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.02183 0.06013 0.06338 0.06410 0.06719 0.16250 
# 
# $confortable
# Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
# 0.002762 0.060484 0.063457 0.064300 0.067093 0.258621 
# 
# $luxe
# Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
# 0.008621 0.060811 0.063604 0.064512 0.067340 0.193548 
# 
# $normal
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.00385 0.06044 0.06344 0.06429 0.06711 0.61765 


###nb voyage fonction de l'heure
b_niveau_voyage$departure_time_num<-sapply(strsplit(b_niveau_voyage$departure_time,":"),function(x){
  #x<-strsplit(b$departure_time,":")[[1]]
  as.numeric(paste0(x[1],".",x[2]))
})

b_niveau_voyage$heure_quali<-sapply(b_niveau_voyage$departure_time_num,function(x){
r="non_def"
    if(19<=x ){r="tard"}
  if(13<=x && x<19){r="aprem"}
  if(8<=x && x<13){r="matin"}
  if(5<=x && x<8){r="trop tôt"}
  if(0<=x && x<5){r="nuit"}
return(r)
  })
  sapply(split(b_niveau_voyage,b_niveau_voyage$heure_quali),function(x){summary(
    #x<-split(b_niveau_voyage,b_niveau_voyage$heure_quali)[[1]]
    x$price_per_dist)})

# aprem     matin     nuit      tard  trop tôt
# Min.      1.00000   1.00000   2.0000   1.00000   1.00000
# 1st Qu.   7.00000   9.00000   8.0000   6.00000   7.00000
# Median   14.00000  17.00000  18.0000  11.00000  18.00000
# Mean     16.76689  21.33364  23.4038  15.10194  22.50734
# 3rd Qu.  23.00000  30.00000  35.0000  19.00000  34.00000
# Max.    236.00000 209.00000 209.0000 756.00000 184.00000
  
  # aprem       matin        nuit        tard    trop tôt
  # Min.    0.00275989 0.003577818 0.009891197 0.007541478 0.002762431
  # 1st Qu. 0.06015038 0.060869565 0.061475410 0.060000000 0.060810811
  # Median  0.06331471 0.063492063 0.064000000 0.063464837 0.063694268
  # Mean    0.06414469 0.064435397 0.065538621 0.064512101 0.064489550
  # 3rd Qu. 0.06711409 0.066869301 0.069030040 0.068181818 0.067307692
  # Max.    0.38596491 0.617647059 0.333333333 0.401459854 0.258620690
# unique(b_niveau_voyage$make)
sapply(split(b_niveau_voyage,b_niveau_voyage$booking_mode),function(x){summary(x$price_per_dist)})
# auto    manual
# Min.      1.00000   1.00000
# 1st Qu.   7.00000   8.00000
# Median   14.00000  15.00000
# Mean     17.85089  19.57154
# 3rd Qu.  25.00000  27.00000
# Max.    756.00000 204.00000
# 
# auto      manual
# Min.    0.00275989 0.003849856
# 1st Qu. 0.06000000 0.060836502
# Median  0.06325301 0.063655031
# Mean    0.06382309 0.064833447
# 3rd Qu. 0.06637168 0.068027211
# Max.    0.40145985 0.617647059

#unique(b_niveau_voyage$make)..
###Petit test d'Analyse factorielle
#names(b_niveau_voyage)
sum(table(b_niveau_voyage$comfort))
sum(table(b_niveau_voyage$make)) #1132466

# save(b_niveau_voyage,file ="C:/Users/Clement/Desktop/blablacar/dataset/base_niveau_voyage.Rdata" )
# unique(b_niveau_voyage$day)=="ven\\."
memory.limit(10000)
###Réfléchir à comment tirer de l'information##2chantillon de 10000 dans la base des voyages
base<-b_niveau_voyage[round(runif(20000)*(dim(b_niveau_voyage)[1]-1)+1),c("day","price_per_dist","booking_mode","comfort","heure_quali","vitesse")]#"make"
##Limite aux lundi pour diminuer la taille
# base<-b_niveau_voyage[b_niveau_voyage$day=="ven\\.",c("day","month","price_per_dist","booking_mode","comfort","make","heure_quali","vitesse")] 
res.famd<-FAMD (base, ncp =3, sup.var = NULL, ind = NULL, graph = TRUE)


#Si c'est toujours la galère, échantilloner la base et faire des ACP plusieurs fois, voir randomforest
fviz_screeplot(res.famd)
# Graphique des variables
fviz_famd_var (res.famd, repel = TRUE)
# Contribution à la première dimension
fviz_contrib (res.famd, "var", axes = 1)
# Contribution à la deuxième dimension
fviz_contrib (res.famd, "var", axes = 2)
###base sur moins de jours pour tester
gc()