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

##simple histogram

hist(pdog$Number.Fleas) ##quick visualization of flea # range

##simple point plot

plot(pdog$weight.grams, type="p") ##weights all over the place

##humble beginnings...

##ggplot finally!
##let's see if we can make a pretty histogram
##avg. number of fleas per site

fleas<-ggplot(data=flea.sum, aes(x=Site.ID, y=mean.fleas, fill=Site.ID)) +
                geom_histogram(stat="identity") +
                xlab("Site") + ylab("Avg. Number Fleas") +
                labs(fill="Site ID") +
                theme_classic()

fleas
##a trend line wouldn't be too appropriate here
##dealing with a categorical x variable...

##how about weight vs. fleas

weight<-ggplot(data=pdog, aes(x=weight.grams, y=Number.Fleas)) +
                geom_point(color="darkseagreen2", size=3) +
                geom_smooth(method="lm", se=FALSE, color="firebrick1") +
                xlab("Weight (g)") + ylab("Flea Count") +
                scale_x_continuous(name="Weight (g)", breaks=seq(from=0, to=1500, by=200)) +
                theme_classic()

weight ##slightly positive regression

##Christmas colors!

##Anova

m1<-aov(formula=Number.Fleas~weight.grams, data=pdog)

summary(m1)

##Soo weight doesn't explain a whole lot variance in flea count..
##p=0.15

m2<-lm(formula=Number.Fleas~weight.grams, data=pdog)

summary(m2)

##R-squared pretty low (low correlation)
##p=0.15

m3<-lm(formula=sqrt(Number.Fleas)~weight.grams, data=pdog)

summary(m3)
##transforming y variable made things even less correlated/significant

##Model Comparison

AIC(m1, m2, m3)

##But surprisingly, m3 (transformed y variable) has the lowest (best)
##AIC score

shapiro.test(pdog$Number.Fleas) ##Not normal!

sqrt.fleas<-sqrt(pdog$Number.Fleas)

shapiro.test(sqrt.fleas) ##square root makes data slightly more normal

##Export plots



##Map of 2021 Trapping Sites

##Libraries I need:
library(ggplot2)
library(ggmap)
library(maps)
library(mapdata)

##Base states data

states<- map_data("state")

arizona<- subset(states, region %in% c("arizona"))

map_arizona<-ggplot(data = arizona) + 
  geom_polygon(aes(x = long, y = lat, group=group), fill = "palegreen", color = "black") +
  coord_fixed(1.3)

map_arizona


##Read in data points for 2021 trapping sites

contemporary<-read.csv("2021_sites.csv")

head(contemporary)

##map the contemporary points

ggplot() +
  geom_point(data=contemporary, aes(x=Longitude, y=Latitude), color="red") ##Color has to be specified outside of aes()

##combine contemporary points onto the Arizona map

map<- ggplot(data = arizona) + 
  geom_polygon(aes(x = long, y = lat, group=group), fill = "palegreen", color = "black") +
  coord_fixed(1.3) +
  geom_point(data=contemporary, aes(x=Longitude, y=Latitude), color="red") +
  theme(legend.position = "none") + ##this gets rid of legend
  theme_classic()

map

map.labels<-geom_text(data=contemporary, aes(label=site, group=site))

map + map.labels

##Will try to add in Navajo nation sites next summer!