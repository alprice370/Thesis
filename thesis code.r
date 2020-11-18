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

##### INTRO  QUESTIONS FOR DATA COLLECTED BY ME @ UMES ######

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

## Define Location Type
basics.n$location_type<- ifelse(basics.n$location=="NB_ONE", "N",
                                ifelse(basics.n$location=="NB_TWO","N","A"))
##True age - Likely won't need this.  essentailly retroages fish.
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

###Figure2 -proportion of fish by source, and sex  Also number caught by source.
plot1a <- qplot(x=pdlen,y=..count../sum(..count..), data =basics.n[basics.n$source=="ANDRE",],
                geom = "histogram", binwidth = 0.5, fill=sex)+
  labs(x="Total Length (cm)", y="Proportion", col="Sex")+ggtitle("Length Frequencies of Black Sea Bass", subtitle = "A (n=407)")+
  theme(plot.title = element_text(hjust = 0.5))+xlim(0,60)
plot1a <- plot1a+scale_fill_manual(values=c("tomato1","dodgerblue4","aquamarine4" ,"grey48"), 
                                   name="Sex",labels=c("Female", "Male", "Trans", "Unknown"))

plot1b <- qplot(x=pdlen,y=..count../sum(..count..), data =basics.n[basics.n$source=="NOAA",],
                geom = "histogram", binwidth = 0.5, fill=sex)+
  labs(x="Total Length (cm)", y="Proportion", col="Sex")+ggtitle(NULL, subtitle = "B (n=1304)")+
  theme(plot.title = element_text(hjust = 0.5))+ylim(0,.100)
plot1b <- plot1b+scale_fill_manual(values=c("tomato1","dodgerblue4","aquamarine4" ,"grey48"), 
                                   name="Sex",labels=c("Female", "Male", "Trans", "Unknown"))

fig2 <- grid.arrange(plot1a, plot1b)

#####STATISTICS####
###TEST: Is age different at habiatat type?##
##locatoin and age
with(basics.n, t.test((true_age[location_type=="N"]), (true_age[location_type=="A"]))) ##p=0.172 - they don't pick location with age 
with(basics.n, t.test((age[location_type=="N"]), (age[location_type=="A"]))) ##p=0.1163 - they don't pick locaiotn with age 

with(basics.n, wilcox.test((true_age[location_type=="N"]), (true_age[location_type=="A"]))) ##p=0.1647 - they don't pick locaiotn with age 
##locaiton and sizecat
with(basics.n, t.test(len~location_type)) ##p-value = 2.853e-06
#sex and age
with(basics.n, t.test((true_age[sex=="Male"]), (true_age[sex=="Female"])))  ##p=0.0015 - they (can) change sex as they age
#length and sex
with(basics.n, t.test((len[sex=="Male"]), (len[sex=="Female"])))  ##1.355e-10 - they get bigger as they age
##Against NOAA
with(basics.n, t.test((pdlen[source=="NOAA"]), (pdlen[source=="ANDRE"])))
table(basics.n$source)
 

#############
#### Size at age/ Age-length data #####
###############

##Age-length keys are later combined b/c stats didn't show
##signif differneces b/w sexes and sizes @ sites

#REGRESSION
agelength <- basics
agelength.lm <- lm(len~age, data=agelength) #regression of the data
coefs <- coef(agelength.lm) #gives you the intercept and slope
#for every unit increase in age, the model predicts a unit increase in total length

summary(agelength.lm) #summary of the regression
pt(coefs[[1]]/coefs[[2]], df=279, lower.tail=FALSE) #this was on an old regression hw, not sure if needed
#based on the t-test (and regression summary), the regression slope is statistically different from zero (p-value <0.001)

confint(agelength.lm)[2,] #confidence interval

summary(agelength.lm)$r.square #how much variation in abundance the linear regression model explains
#it explains 52% of the variation. Should consider other explanations for large variation in total length.

#REGRESSION PLOT

age_plot_1 <- ggplot(data=agelength, aes(age, len)) + 
  geom_point(aes(color=sex)) +
  stat_smooth(method = "lm", col = "black") + labs(x="Fish Age (years)",
                                                   y="Total Fish Length (cm)",
                                                   colour="Sex")+
  scale_color_manual(values=c("tomato1","dodgerblue4","grey48"),labels=c("Female", "Male","Trans"))+
  ggtitle("Age-Length Regression by Sex")+theme(plot.title = element_text(hjust = 0.5))

age_plot_1
# fig.name <- paste("Age-length.png")
# dev.copy(png, fig.name, 500, 500);dev.off()

#QQPLOT
qqnorm(rstandard(agelength.lm))
qqline(rstandard(agelength.lm))

#RESIDUALS VS FITTED
plot(agelength.lm,1)
abline(h=c(-2,0,2),lty=3,col="grey60")

#COOKs DISTANCE
plot(agelength.lm,4)  ## all fish
h=qf(0.5,2,50)

##to remove the outliers
rmoutlier <-agelength[-c(244, 521), ]
rmoutlier.lm <- lm(len~age, data=rmoutlier)
##Remove 244, 521 b/c they're outliers
plot(rmoutlier.lm,4)


#### Age-Length Key Construction  - Code from Derek Ogle ####
##install.packages("FSAdata")
library(magrittr)
library(FSA)                                # for headtail(), alkPlot()
library(FSAdata)                            # for SpotVA2 data
library(dplyr)                              # for filter(), mutate()
library(nnet)                               # for multinom()


lenbrks <- seq(10,40,5)
agelength <- mutate(agelength,lcat=lencat((len), breaks = lenbrks)) # nearest 5 cm
headtail(agelength)

( raw <- xtabs(~lcat+age,data=agelength) )
( ALK.obs <- prop.table(raw,margin=1) )

mlr <- multinom(age~lcat,data=agelength,maxit=500)

minx <- min(agelength$len)
maxx <- max(agelength$len)

lens <- seq(10, 40,5) # plotting intervals
ALK.sm <- predict(mlr,data.frame(lcat=lens),type="probs")
row.names(ALK.sm) <- lens
round(ALK.sm,3)

alkPlot(ALK.obs,xlab="Total Length (cm)") 
alkPlot(ALK.sm,xlab="Total Length (cm)") 

alkPlot(ALK.sm,xlab="Total Length (cm)",pal="gray",showLegend=TRUE)
alkPlot(ALK.sm,xlab="Total Length (cm)",pal="gray",showLegend=TRUE,type="area")

alkPlot(ALK.sm,xlab="Total Length (cm)",pal="gray",type="lines")
age_plot_2 <- alkPlot(ALK.sm, xlab="Total Length (cm)", ylab = "Age (years)",type="bubble", main="Black Sea Bass Length at Age", cex.main=2, cex.xlab=3, cex.ylab=3)
dev.copy(png,'Age_plot2.png', width=2000, height=1600, res=200) ; dev.off()

### Age-length Key application

agelength.mod <- alkIndivAge(ALK.obs,age~len,data=agelength)
headtail(agelength.mod)

agelength.comb <- rbind(agelength,agelength.mod)
str(agelength.comb)

agefreq <- xtabs(~age,data=agelength.comb)
prop.table(agefreq)

hist(~age,data=agelength.comb,breaks=0:6,xlab="Age (yrs)")

( sp.sum <- Summarize(len~age,data=agelength.comb,digits=2) )
plot(len~age,data=agelength.comb,ylab="Total Length (mm)",xlab="Age (yrs)",pch=16,col=rgb(0,0,0,0.1))
lines(mean~age,data=sp.sum,col="blue",lwd=2)
