##R Final Project

##Going to run some fun R commands
##and stats 
##on field data from Summer 2021 sampling
##of Gunnison's Prairie Dog in Arizona

##set working directory

setwd("C:/Users/annac/Desktop/Grad School/Classes/Fall2021_Classes/Intro to R/Final Project")

getwd()

##Load in sampling data

pdog<-read.csv("GUPD_Summer_2021.csv") ##lot of blank rows...

pdog<-pdog[-c(134:827), ] ##got rid of empty rows; don't always need

pdog<-pdog[-c(6:7), ] ##got rid of samples with NA's

##subset out pdogs from KAIB

kaib<-pdog[c(1:25), ]

##order kaib pdogs by decreasing weight

library(taRifx)

kaib.weight<-sort(kaib, decresing=TRUE, ~weight.grams)

kaib.weight<-kaib[ , order("weight.grams", decreasing=TRUE)]

##Group and summarize

library(tidyverse)

flea.sum<-pdog %>% group_by(Site.ID) %>% summarise(mean.fleas=mean(Number.Fleas))

##treated sites and urban sites have fewer fleas

##Custom function for converting weight.grams to weight.kg

g2kg<-function(x) {
  weight.kg<-x/1000
  return(weight.kg)
}

pdog$weight.kg<-g2kg(pdog$weight.grams)

##if_else statements
##using flea.sum data
##average number of fleas per site

HOLX<-0.625

PVMP<-4.71

TRTR<-20.3

SOPC<-0.0

if(SOPC<TRTR) {
  print("treated site")
} else if (SOPC==TRTR) {
  print("untreated site")
}

##[1] "treated site"

##lapply to convert lat and long columns to degrees/minutes/seconds

library(celestial)

##test out function

deg2dms(35.14489)

lapply(pdog$Lat, FUN=deg2dms)

lapply(pdog$Long, FUN=deg2dms) ##didn't like that my "long" columns are ~113
