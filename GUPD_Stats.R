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

pdog<-pdog[-c(6, 7, 114, 118), ] ##got rid of samples with NA's

##subset out pdogs from KAIB

kaib<-pdog[c(1:23), ]

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

##gather "nobuto", "blood vial", and "tissue" into one column

col_vals<-c("Nobuto", "Blood.Vial", "Tissue")
  
pdog_gather<-gather(data=pdog, key=sample.types, value=value, all_of(col_vals))

##try out melt() with number values
##weight, length, and tail length

library(reshape2)

pdog_melt<-melt(data=pdog, id.vars="Site.ID", "Collection.Date", "Sample.Number",
                "PDOG.ID", "Lat", "Long", "Recapture", "Previously.Tagged", 
                "PIT.Tag.Number", "Sex", "Shaved", "Number.Fleas", "Nobuto",
                "Blood.Vial", "Tissue", "Age", "Trap.Location", "weight.kg", 
                measure.vars="weight.grams", "length.cm", "tail.length.cm",
                variable.name=c("measurements"))

##for loops! Get a quick glance at flea counts..

for(i in 1:length(pdog$PDOG.ID)) {
  print(pdog$Number.Fleas[i])
}

##kinda lame, but figured out how to incorporate data frame values into for loops
##should the need ever arise

##group pdogs by site and find mean weight!

library(plyr)

pdog_weights<-ddply(.data=pdog, .variables=c("Site.ID"), 
                       .fun=summarise, mean.weight=mean(weight.grams))
head(pdog_weights)

##use ddply to count the number of samples per site

pdog_counts<-ddply(.data=pdog, .variables=c("Site.ID"), .fun=nrow)

head(pdog_counts)
