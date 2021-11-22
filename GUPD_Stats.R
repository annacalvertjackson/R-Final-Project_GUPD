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

pdog<-pdog[-c(134:827), ] ##got rid of empty rows

##subset out pdogs from KAIB

kaib<-filter(pdog, pdog$Site.ID=="KAIB")

