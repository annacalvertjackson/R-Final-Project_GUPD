##R Final Project

##Going to run some fun R commands
##and stats 
##on field data from Summer 2021 sampling
##of Gunnison's Prairie Dog in Arizona

##set working directory

setwd("C:/Users/annac/Desktop/Grad School/Classes/Fall2021_Classes/Intro to R/Final Project")

getwd()

##Load in sampling data

library(readxl)

pdog<-read_excel("GUPD_Summer_2021.xlsx", sheet=1)

