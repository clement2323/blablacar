rm(list=ls())
gc()

library(jsonlite)
library(lubridate)
library(ggplot2)
library(tidyr)
library(dplyr)

## On construit la base à partir des réponses individuels à chaque requête, stockés dans les fichiers "res_trips"
## Ici: on ne garde que certaines variables par rapport aux requêtes initiales: peut être modifié
##getwd()
#setwd("C:Users/Clement/Desktop/blablacar")


##source("C:Users/Clement/Desktop/blablacar/002_build_daily_base.R")
##construction d 'une base propre à faire une fois que j'aurais récupéré les requêtes de Milena
#fichiers=list.files()
# load("C:/Users/Clement/Desktop/blablacar/dataset/exemple_fichier_individuel_1.Rdata")
# load("exemple_fichier_individuel_2.Rdata")
load("C:/Users/Clement/Desktop/blablacar/dataset/base_from_25-05-2018_to_01-09-2018.Rdata")
getwd()
head(b)
dim(b)

nombre_de_voyage_sans_doublon=length(unique(b$permanent_id))

#vecteur donnant le nombre de fois que chaque trahjet a été rrequeêté pour la base 
vecteur<-sapply(split(b$requete_id,b$permanent_id),length)

##petite densité histogramme//
head(vecteur)
summary(vecteur)
hist(vecteur,ylim = 50)

sapply(b,function(x){length(unique(x)) })
length(unique(paste(b$requete_id,b$permanent_id)))