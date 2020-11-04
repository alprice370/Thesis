##This shows how data was loaded and wrangled

###Load Data###
rm(list=ls(all=TRUE)) 
setwd("~/Desktop/MasterR")
diets.original <- read.csv("Working_United.csv")
setwd("~/Desktop/MasterR")
isotope.original <- read.csv("isotopedata.csv")

###Packages##  
#May not need all of these right now
library(SIBER)
library(dplyr)
library(ggplot2) 
library(tidyverse)
library(Rmisc)
library(gridExtra)

###Wrangle the Data####
colnames(diets.original)
colnames(diets.original)=c( "fishid","svspp","cruise6","station","pdsex","pdid","pdlen","pdwgt",   
                            "sizecat","pdgutw","pdgutv","fhdat","stratum","beglat","beglon","declat",  
                            "declon","month","day","year","purcode","season","geoarea","pynam",
                            "gencat","gensci","analcat","analsci","collcat","collsci","pynum","pyamtw",  
                            "pyamtv","pdscinam" ,"pdcomnam","location")

colnames(isotope.original)
colnames(isotope.original)=c("year", "month", "fishid", "c13l","n15l","c13m","n15m","c13s",    
                             "n15s","location","len","weight","sex","age","consumer")
isotopes_for_merge <- isotope.original[1:407, 1:14]

master_df <- merge(diets.original, isotopes_for_merge, by=c("fishid"), all.x = T)
colnames(master_df)

master_df <- select(master_df, -(contains(".y")))
master_df <- rename(master_df, c("year.x" = "year", "location.x"="location", "month.x"="month"))
