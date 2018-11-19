rm(list=ls())

## On construit la base à partir des réponses individuels à chaque requête, stockés dans les fichiers "res_trips"
## Ici: on ne garde que certaines variables par rapport aux requêtes initiales: peut être modifié

setwd("W:/")

library(jsonlite)
# library(lubridate)
library(ggplot2)
# library(tidyr)
# library(dplyr)

folders<-dir("blablacar/base/ensemble des données extraites")[grep("^res_trips_",dir("blablacar/base/ensemble des données extraites"))]

## Ici: on ne garde que certaines variables par rapport aux requêtes initiales: peut être modifié
traitement_data<-function(res,f){
  #load("W:/Blablacar/base/ensemble des données extraites/res_trips_2018-05-25/1.Rdata")
  #res<-res[[10]] str(res$trips,1)
  od<-strsplit(strsplit(gsub("https://www.blablacar.fr/search?fn=","",res$links[[2]],fixed=T),'&db')[[1]],'&tn=')[[1]]
  dreq<-strsplit(strsplit(gsub("https://www.blablacar.fr/search?fn=","",res$links[[2]],fixed=T),'&db=')[[1]][2],'&de=')[[1]][1]
  
  
  if(res$pager$total>0){
    req_id<-paste(file.info(f)$mtime,paste(od,collapse=" "),dreq,res$pager$limit,sep=" ")
    
    data1<-data.frame(origin=od[1],
                      destination=od[2],
                      total=res$pager$total,
                      distance=ifelse(!is.null(res$distance),res$distance,NA),
                      duration=ifelse(!is.null(res$duration),res$duration,NA),
                      requete_id=req_id)
    
    data2<-data.frame(requete_id=req_id,
                      departure_dates=res$trips$departure_date,
                      price=res$trips$price$string_value ,
                      price_color=res$trips$price$price_color,
                      price_with_commision=res$trips$price_with_commission$string_value,
                      seats_left=res$trips$seats_left,
                      seats=res$trips$seats,
                      duration=res$trips$duration$value,
                      distance=res$trips$distance$value,
                      permanent_id=res$trips$permanent_id,
                      departure_city_name=res$trips$departure_place$city_name,
                      departure_latitude=res$trips$departure_place$latitude,
                      departure_longitude=res$trips$departure_place$longitude,
                      arrival_city_name=res$trips$arrival_place$city_name,
                      arrival_latitude=res$trips$arrival_place$latitude,
                      arrival_longitude=res$trips$arrival_place$longitude,
                      
                      model=ifelse(is.null(res$trips$car$model),NA, res$trips$car$model),#PAs toujours les infos là dedans
                      make=ifelse(is.null(res$trips$car$make),NA, res$trips$car$make),
                      id=ifelse(is.null(res$trips$car$id),NA, res$trips$car$id),
                      comfort=ifelse(is.null(res$trips$car$comfort),NA, res$trips$car$comfort),
                      comfort_nb_star=ifelse(is.null(res$trips$car$comfort_nb_star),NA, res$trips$car$comfort_nb_star),
                      freeway=ifelse(is.null(res$trips$car$freeway),NA, res$trips$car$freeway),
                      booking_mode=res$trips$booking_mode,
                      booking_type=res$trips$booking_type,
                      
                      # arrival et departure cityname et lat/long
                      
                      trip_details_id=res$trips$trip_details_id)
    return(list(data1,data2))
  }
  
  
}



base<-NULL

list_fold<-lapply(folders,function(fold){
  #fold=folders[1]
  setwd("W:/blablacar/base/ensemble des données extraites")
  setwd(fold)
  print(" ######################## ")
  print(fold)
  print(" ######################## ")
  #for(f in dir()){
  list_f<-lapply(dir(),function(f){
    #f<-dir()[1]
    load(f)
    print(f)
    if(is.null(names(res))){
      tmp<-lapply((1:length(res)),function(i){
        #i=2
        #print(i)
        return(traitement_data(res[[i]],f)[[2]])
      })
      # head(tmp[[1]])
      # head(tmp[[13]])
      r<-do.call(rbind,tmp)
    }else{
      r<-traitement_data(res,f)[[2]]
      #base<-rbind(base,r[[2]])
    }
    return(r)
  })
  return(do.call(rbind,list_f))
    
  #head(str(l))
})

base<-do.call(rbind,list_fold)
list_fold<-NULL
save.image(file ="W:/Blablacar/git/base_from_25-05-2018_to_01-09-2018_augmente_CG.Rdata")