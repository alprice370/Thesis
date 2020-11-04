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

##### INTRO  QUESTIONS - DATA COLLECTED BY ME @ UMES ######

##WRANGLING
basics <-(master_df[master_df$location!="",])
basics <- basics[!duplicated(basics$fishid), ]
basics$location_type<- ifelse(basics$location=="NB_ONE", "N",
                              ifelse(basics$location=="NB_TWO","N","A"))

##Q:How many fish caught at each site at each year
t1 <- with(basics, table (year, location))
write.table(t1, file = "year_location.txt", sep = ",", quote = FALSE, row.names = F)
###A: 107 and 300 in 2016 & 2018, respectively

##Q:How many fish caught at each location type?
with(basics, table (location_type, year))
###A: 197 @ Artificial, 210 @ Natural

#Q: What was the original sex proportion?
plot1 <- ggplot(data=basics)+geom_bar(mapping = aes(x=sex, y=..prop..,group=1))
plot1 <- plot1+ labs(x = "Sex")+ labs(y = "Prop")+ labs(title = "Proportion of BSB Sex")
plot1
table(basics$sex)  ## 233 Female, 167 Male, & 7 Trans

##T-test for aged fish at location type
with(basics, t.test(len~location_type))  ##p=<<0.05 - theyre different
with(basics, t.test(age~location_type)) ##P=.1163, No signif diff in ages b/w sites
with(basics, t.test(true_age~sex))  ###p-value = 0.000228

##K-S test
ks.test(basics$len[isotope$location_type=="N"],basics$len[isotope$location_type=="A"])

##### INTRO  QUESTIONS -UMES & NOAA COMPARED######

###WRANGLING###
basics.n <- subset(master_df,select = -c(2:3,6,12:13,37:42))
basics.n$source <- ifelse(basics.n$geoarea=="", "ANDRE", "NOAA")
basics.n <- basics.n[!duplicated(basics.n$fishid), ]

##Reassign sex
basics.n$pdsex[basics.n$pdsex=="U"] <- "4"  #For mine, "U" meant trans
basics.n$pdsex[basics.n$pdsex=="F"] <- "2"
basics.n$pdsex[basics.n$pdsex=="M"] <- "1"
basics.n$pdsex[basics.n$pdsex=="4"] <- "4"  ### Becasue five NOAA fish were sexed as "4"
basics.n$pdsex[basics.n$pdsex==""] <- "0" ##Because it's still unknown

basics.n$sex.x <- ifelse(basics.n$pdsex == 0, 'Unknown',
                         ifelse(basics.n$pdsex == 1, 'Male',
                                ifelse(basics.n$pdsex == 2, 'Female',
                                       ifelse(basics.n$pdsex == 4, 'Trans', NA))))  
transform(basics.n,sex.x=factor(basics.n$sex.x,levels=c("Unknown", "Female","Trans", "Male")))

##Then, either change or leave this
table(basics.n$pdsex, basics.n$source)
###Make a year deletion###
table(basics.n$year,basics.n$source)
basics.n <- basics.n[basics.n$year!="2017",]  
###NOAA data ends in 2016; I only had 10 unreliable samples in 2017
basics.n <- select(basics.n,-(sex))
basics.n <- rename(basics.n,c("sex.x" = "sex"))
###Define size category
basics.n$sizecat[basics.n$len<=25] <- "S"
basics.n$sizecat[basics.n$len>25] <- "M"
basics.n$sizecat[basics.n$len>50] <- "L"

##Location Type
basics.n$location_type<- ifelse(basics.n$location=="NB_ONE", "N",
                                ifelse(basics.n$location=="NB_TWO","N","A"))
##True age
basics.n$true_age<- ((basics.n$age)+((basics.n$mon-4)/12))
basics.n$true_age <- round(basics.n$true_age, digits = 4)

##I want to look at local MAB fish - None are caught in Winter anyway
basics.n <- filter(basics.n, geoarea=="MAB"| geoarea=="", year>2000)  ##last 10 years
basics.n <- filter(basics.n,season!="WINTER")
basics.n <-basics.n%>%mutate(Lati=trunc(beglat/100) + ((beglat-(trunc(beglat/100)*100))/100)*1.66 ) %>%
  mutate(Long=trunc(beglon/100)+ ((beglon-(trunc(beglon/100)*100))/100)*1.66) %>%
  mutate(Long=Long*-1)
table(basics.n$season)
table(basics.n$geoarea)
