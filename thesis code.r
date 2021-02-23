#Created by André Price
#This script covers my MS thesis and creates plots if needed
rm(list=ls(all=TRUE)) #Clear Console
HD=setwd("~/Desktop/Chosen_Folder"); HD
##choose the needed input spreadsheets
diets.original <- read.csv("Working_United.csv")
isotope.original <- read.csv("isotopedata.csv")

##Needed packages.  Uncomment if needed.
##install.packages(c("FSA","SIBER", "FSAdata", "ggrepel", "nlstools", "plotrix", "Rmisc", "siar", "tidyverse"))
library(SIBER)
library(dplyr)
library(ggplot2) 
library(tidyverse)
library(Rmisc)
library(gridExtra)

####Order and wrangle the dataset#######
Fold=TRUE ##dummy varialbe that allows us to collapse long sections of code
if (Fold) {
  
  ##Wrangle and merge the data
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
}##Fold Data Inputs - Does not include Figure 1 mapping data
######Wrangle the Data
if (Fold) {
  ##Add some extra columns, formatting and subsetting the data for basic data interrogation
  # 
  # ##This adds a "natural" or "artificial" designation  
  # basics <-(master_df[master_df$location!="",])
  # basics <- basics[!duplicated(basics$fishid), ]
  # basics$location_type<- ifelse(basics$location=="NB_ONE", "N",
  #                               ifelse(basics$location=="NB_TWO","N","A"))
  
  ##### INTRO  QUESTIONS -Comparing UMES and NOAA data#####
  ###Wrangling###
  basics.n <- subset(master_df,select = -c(2:3,6,12:13,37:42))
  basics.n$source <- ifelse(basics.n$geoarea=="", "ANDRE", "NOAA")
  basics.n <- basics.n[!duplicated(basics.n$fishid), ]
  
  ##Reassign and standardize sex designations
  basics.n$pdsex[basics.n$pdsex=="U"] <- "4"  #For mine, "U" meant trans
  basics.n$pdsex[basics.n$pdsex=="F"] <- "2"
  basics.n$pdsex[basics.n$pdsex=="M"] <- "1"
  basics.n$pdsex[basics.n$pdsex=="4"] <- "4"  ### Becasue five NOAA fish were sexed as "4"
  basics.n$pdsex[basics.n$pdsex==""] <- "0" ##Because it's still unknown
  ##Another set of columns used for plotting
  basics.n$sex.x <- ifelse(basics.n$pdsex == 0, 'Unknown',
                           ifelse(basics.n$pdsex == 1, 'Male',
                                  ifelse(basics.n$pdsex == 2, 'Female',
                                         ifelse(basics.n$pdsex == 4, 'Trans', NA))))
  transform(basics.n,sex.x=factor(basics.n$sex.x,levels=c("Unknown", "Female","Trans", "Male")))
  
  ###2017 Fish were destroyed, delete them from the dataset
  table(basics.n$year,basics.n$source)
  basics.n <- basics.n[basics.n$year!="2017",]  
  ##See how many of each sex for source
  table(basics.n$pdsex, basics.n$source)
  ###Primitive column rename
  basics.n <- select(basics.n,-(sex))
  basics.n <- rename(basics.n,c("sex.x" = "sex"))
  ##Designate size class - size based on Byron and Link (2010)
  basics.n$sizecat[basics.n$len<=25] <- "S"
  basics.n$sizecat[basics.n$len>25] <- "M"
  basics.n$sizecat[basics.n$len>50] <- "L"
  
  ##Designates site as natual or aritificial
  basics.n$location_type<- ifelse(basics.n$location=="NB_ONE", "N",
                                  ifelse(basics.n$location=="NB_TWO","N","A"))
  ##True age - will be used in basic age stats
  basics.n$true_age<- ((basics.n$age)+((basics.n$mon-4)/12))
  basics.n$true_age <- round(basics.n$true_age, digits = 4)
  
  ##I want to look at a certain subset of trawl caught fish from Mid Atlantic Bight
  basics.n <- filter(basics.n, geoarea=="MAB"| geoarea=="", year>2000)  ##from year 2k -2k16
  basics.n <- filter(basics.n,season!="WINTER")  ##Not from winter trawl
  basics.n <-basics.n%>%mutate(Lati=trunc(beglat/100) + ((beglat-(trunc(beglat/100)*100))/100)*1.66 ) %>%
    mutate(Long=trunc(beglon/100)+ ((beglon-(trunc(beglon/100)*100))/100)*1.66) %>%
    mutate(Long=Long*-1)
  table(basics.n$season)  ##umes vs noaa at season
  table(basics.n$geoarea)  ##shows umes vs. noaa
  
} ##Fold data wrangling

#######Results##
#######Fish Sizes/ Fish by source/ Figure2####
if (Fold){
  ##mean sizes of males and females by source
  summarySE(basics.n, measurevar="pdlen", groupvars=c("source", "sex"), na.rm=TRUE)
  with(basics.n[basics.n$source=="ANDRE",], t.test(pdlen[sex=="Male"], pdlen[sex=="Female"])) ##for UMES
  with(basics.n[basics.n$source=="NOAA",], t.test(pdlen[sex=="Male"], pdlen[sex=="Female"]))  ##for NOAA
  with(basics.n, table (year, source))  ##number of fish by source by year
  table(basics.n$sex, basics.n$source) ##Number of fish by sex
  
  ##Figure2 Plot
  plot3a <- qplot(x=pdlen,y=..count../sum(..count..), data =basics.n[basics.n$source=="ANDRE",],
                  geom = "histogram", binwidth = 0.5, fill=sex)+
    labs(x="Total Length (cm)", y="Proportion", col="Sex")+ggtitle("Length Frequencies of Black Sea Bass", subtitle = "A (n=407)")+
    theme(plot.title = element_text(hjust = 0.5))+xlim(0,60)
  plot3a <- plot3a+scale_fill_manual(values=c("tomato1","dodgerblue4","aquamarine4" ,"grey48"), 
                                     name="Sex",labels=c("Female", "Male", "Trans", "Unknown"))
  
  plot3b <- qplot(x=pdlen,y=..count../sum(..count..), data =basics.n[basics.n$source=="NOAA",],
                  geom = "histogram", binwidth = 0.5, fill=sex)+
    labs(x="Total Length (cm)", y="Proportion", col="Sex")+ggtitle(NULL, subtitle = "B (n=1304)")+
    theme(plot.title = element_text(hjust = 0.5))+ylim(0,.100)
  plot3b <- plot3b+scale_fill_manual(values=c("tomato1","dodgerblue4","aquamarine4" ,"grey48"), 
                                     name="Sex",labels=c("Female", "Male", "Trans", "Unknown"))
  
  Figure2 <- grid.arrange(plot3a, plot3b); Figure2
  #copy figure2 to .PNG file
  #dev.copy(png,'Figure2.png', width=2000, height=1600, res=200) ; dev.off()
} ##Fold Proportions and Figure 2 Plotting
#######Statistics for age at habitat type########
if(Fold){
  ###TEST: Is age different at habiatat type?##
  ##locatoin and age
  with(basics.n, t.test((true_age[location_type=="N"]), (true_age[location_type=="A"]))) ##p=0.172 - they don't pick locaiotn with age 
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
} ##Fold T-Tests for habitat type
#######Size at age/Length#######
if (Fold){
  
  ##REGRESSION Setup and analysis##
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
}### Fold regression analysis and setup
#######Age-Length Key/ Figure 3########
if(Fold){
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
  
  ##This produces Figure 3
  alkPlot(ALK.sm,xlab="Total Length (cm)",pal="gray",type="lines")
  Figure3 <- alkPlot(ALK.sm, xlab="Total Length (cm)", ylab = "Age (years)",type="bubble", main="Black Sea Bass Length at Age", cex.main=2, cex.xlab=3, cex.ylab=3)
  dev.copy(png,'Figure3.png', width=2000, height=1600, res=200) ; dev.off()
  
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
}##Fold Construction and apllication of Age-Length Key/ Figure3
if (Fold){
  ###For Presentation
  ##Artificial
  ageart <- agelength[agelength$location_type=="A",]
  
  lenbrks <- seq(10,40,5)
  ageart <- mutate(ageart,lcat=lencat((len), breaks = lenbrks)) # nearest 5 cm
  headtail(ageart)
  
  ( raw <- xtabs(~lcat+age,data=ageart) )
  ( ALK.obs <- prop.table(raw,margin=1) )
  
  mlr <- multinom(age~lcat,data=ageart,maxit=500)
  
  minx <- min(ageart$len)
  maxx <- max(ageart$len)
  
  lens <- seq(10, 40,5) # plotting intervals
  ALK.sm <- predict(mlr,data.frame(lcat=lens),type="probs")
  row.names(ALK.sm) <- lens
  round(ALK.sm,3)
  
  alkPlot(ALK.obs,xlab="Total Length (cm)") 
  alkPlot(ALK.sm,xlab="Total Length (cm)") 
  
  alkPlot(ALK.sm,xlab="Total Length (cm)",pal="gray",showLegend=TRUE)
  alkPlot(ALK.sm,xlab="Total Length (cm)",pal="gray",showLegend=TRUE,type="area")
  
  alkPlot(ALK.sm,xlab="Total Length (cm)",pal="gray",type="lines")
  age_plot_nat <- alkPlot(ALK.sm, xlab="Total Length (cm)", ylab = "Age (years)",type="bubble", main="Black Sea Bass Length at Age - Artificial", cex.main=2, cex.xlab=3, cex.ylab=3)
  dev.copy(png,'AgeArt.png', width=2000, height=1600, res=200) ; dev.off()
  
  ##Natural
  agenat <- agelength[agelength$location_type=="N",]
  
  lenbrks <- seq(10,40,5)
  agenat <- mutate(agenat,lcat=lencat((len), breaks = lenbrks)) # nearest 5 cm
  headtail(agenat)
  
  ( raw <- xtabs(~lcat+age,data=agenat) )
  ( ALK.obs <- prop.table(raw,margin=1) )
  
  mlr <- multinom(age~lcat,data=agenat,maxit=500)
  
  minx <- min(agenat$len)
  maxx <- max(agenat$len)
  
  lens <- seq(10, 40,5) # plotting intervals
  ALK.sm <- predict(mlr,data.frame(lcat=lens),type="probs")
  row.names(ALK.sm) <- lens
  round(ALK.sm,3)
  
  alkPlot(ALK.obs,xlab="Total Length (cm)") 
  alkPlot(ALK.sm,xlab="Total Length (cm)") 
  
  alkPlot(ALK.sm,xlab="Total Length (cm)",pal="gray",showLegend=TRUE)
  alkPlot(ALK.sm,xlab="Total Length (cm)",pal="gray",showLegend=TRUE,type="area")
  
  alkPlot(ALK.sm,xlab="Total Length (cm)",pal="gray",type="lines")
  age_plot_nat <- alkPlot(ALK.sm, xlab="Total Length (cm)", ylab = "Age (years)",type="bubble", main="Black Sea Bass Length at Age - Natural", cex.main=2, cex.xlab=3, cex.ylab=3)
  dev.copy(png,'AgeNat.png', width=2000, height=1600, res=200) ; dev.off()
  
}##FoldPresentation section for agelength - extra plots if needed to be broken down by site type
#######Von Bertalanffy with ANOVA####
if(Fold){
  agelen <- agelength[!is.na(agelength$age),]
  View(agelen)
  library(nlstools)          # for nlsBoot()
  library(plotrix)
  
  xlbl <- "Age (yrs)" #creating character variables we will use for plots 
  ylbl <- "Total Length (cm)"
  clr <- col2rgbt("black",5/10) #sets the transparency (1/20) of the color palette
  
  plot(len~age,data=agelen,pch=19,col=clr,xlab=xlbl,ylab=ylbl)
  
  #Declare a function
  vb <- vbFuns("Typical",msg=TRUE) #von bert.
  vb
  
  #Find starting values --automatic generation
  sv <- vbStarts(len~age,data=agelen, type="Typical", plot=TRUE) 
  
  #Check Assumptions
  fit1 <- nls(len~vb(age,Linf,K,t0),data=agelen,start=sv)
  residPlot(fit1,loess="TRUE")
  residPlot(fit1,loess="TRUE", resid.type="standardized") #another way of looking at residuals
  
  #Summarize the model fit
  summary(fit1,correlation=TRUE) #assuming additive error structure
  ( cf <- coef(fit1) )
  confint(fit1) #gives me an error message 
  
  #Bootstrapping
  boot1 <- nlsBoot(fit1,niter=1000) #have to specify the model fit, so the model fit object already has everything save. And 1000 iterations performed. 
  str(boot1)
  headtail(boot1$coefboot)
  confint(boot1,plot=TRUE,rows=1,cols=3) #condidence intervals
  
  #Visualize the fit
  age_plot_3 <- plot(len~age,data=agelen,xlab=xlbl,ylab=ylbl,pch=19,col=clr,
                     main="VonB Curve for Age at Length")
  age_plot_3 <-curve(vb(x,cf),from=1,to=6,n=100,lwd=2,col="red",add=TRUE)
  age_plot_3
  #dev.copy(png,'Age_pot3.png', width=2000, height=1600, res=200) ; dev.off()
  
  mean.Linf <- mean(boot1$coefboot[,1])
  mean.K <- mean(boot1$coefboot[,2])
  mean.t0 <- mean(boot1$coefboot[,3])
  
  ##Among group statistical comparisons
  ##Define Location Type
  rmoutlier$location_type<- ifelse(rmoutlier$Location=="NB_ONE" |rmoutlier$Location=="NB_TWO", "N","A")
  
  sis <- filter(rmoutlier, !is.na(age), !is.na(sex))%>%mutate(lcat=lencat(len, w=1))
  mod1 <- multinom(age~lcat,data=sis, maxit=500)
  mod2 <- multinom(age~lcat*location_type,data=sis, maxit=500)
  
  anova(mod1, mod2)
  
  mod3 <- multinom(age~lcat,data=sis, maxit=500)
  mod4 <- multinom(age~lcat*sex,data=sis, maxit=500)
  
  anova(mod3, mod4)
}##end VonB fold. This section includes ANOVA statistics 
#######Diets and Prey/ fig 4/ supp 1 &2####
#Disclaimer:if you want to look deep into the weeds, look here. If not, I suggest the "next"deep weeds" fold gets run while collapsed
##it contains lots of code that was necessary at some point, but primitively constructed
if (Fold){
  diets.n <- basics.n
  with(diets.n, table(gencat, source))
  diets.n <- filter(diets.n, gencat!="") ### b/c 9 of my fish dont have stomach data
  
  table(diets.n$source)  ###How many fish in each source
  table(diets.n$source=="ANDRE", diets.n$location_type)
  prey.table <- table(diets.n$pynam, diets.n$source)
  write.table(prey.table, file = "prey_table.txt", sep = ",", quote = FALSE, row.names = T)
  
  diet_plot1a <- ggplot (diets.n[diets.n$source=="ANDRE",] ,aes(x=gencat))+geom_bar(aes(y=(..count..)/sum(..count..)))
  diet_plot1a <- diet_plot1a+labs(title="Major Taxa Consumed by BSB",subtitle="A (n=398)", y="Proportion", x="Item")+
    theme(axis.text.x=element_text(angle = 45, hjust=1))
  diet_plot1a
  diet_plot1b <- ggplot (diets.n[diets.n$source=="NOAA",] ,aes(x=gencat))+geom_bar(aes(y=(..count..)/sum(..count..)))
  diet_plot1b <- diet_plot1b+labs(title="", subtitle="B (n=1304)", y="Proportion", x="Item")+
    theme(axis.text.x=element_text(angle = 45, hjust=1))
  diet_plot1b
  diet_plot1 <- grid.arrange(diet_plot1a,diet_plot1b,nrow=2, ncol=1)+theme(axis.text.x=element_text(angle = 45, hjust=1))
  #dev.copy(png,'Diet_plot1.png', width=2000, height=1600, res=200) ; dev.off()
  
  diet_plot2a <- ggplot (diets.n[diets.n$source=="ANDRE" & diets.n$location_type=="A",] ,aes(x=gencat))+geom_bar(aes(y=(..count..)/sum(..count..)))
  diet_plot2a <- diet_plot2a+labs(title="Major Taxa Consumed by BSB by Site Type", subtitle="Artificial (n=193)", y="Proportion", x="Item")+
    theme(axis.text.x=element_text(angle = 45, hjust=1))
  diet_plot2a
  diet_plot2b <- ggplot (diets.n[diets.n$source=="ANDRE" & diets.n$location_type=="N",] ,aes(x=gencat))+geom_bar(aes(y=(..count..)/sum(..count..)))
  diet_plot2b <- diet_plot2b+labs(title="", subtitle="Natural (n=205)", y="Proportion", x="Item")+
    theme(axis.text.x=element_text(angle = 45, hjust=1))
  diet_plot2b
  diet_plot2 <- grid.arrange(diet_plot2a,diet_plot2b,nrow=2, ncol=1)+theme(axis.text.x=element_text(angle = 45, hjust=1))
  #dev.copy(png,'Diet_plot2.png', width=2000, height=1600, res=200) ; dev.off()
  
  
  table(diets.n$gencat=="MISC")##16 rows have non-organic content
  
  ##Find a way to sort out non-organic remains###
  ##Make an "organic" column
  #### Loop sequence for "other" category
  diets.n$othergencat<- ifelse(diets.n$gencat=="CHAETO"|diets.n$gencat=="CNIDARIA"|diets.n$gencat== "CTENOP"|
                                 diets.n$gencat=="PORIFE"|diets.n$gencat== "WORMS"|
                                 diets.n$gencat== "ELASMO"|diets.n$gencat== "UROCHO"|diets.n$gencat== "BRYOZO"|
                                 diets.n$gencat== "CNIDAR"|diets.n$gencat== "ECHINO"|
                                 diets.n$gencat== "AR","OTHER", paste(diets.n$gencat)) 
  
  ##Making new columns to view higher reslolution
  diets.n$othergensci<- ifelse(diets.n$othergencat=="OTHER", "OTHER", paste(diets.n$gensci)) 
  diets.n$otheranalcat<- ifelse(diets.n$othergencat=="OTHER", "OTHER", paste(diets.n$analcat))
  diets.n$otheranalsci<- ifelse(diets.n$othergencat=="OTHER", "OTHER", paste(diets.n$analsci))
  diets.n$othercollcat<- ifelse(diets.n$othergencat=="OTHER", "OTHER", paste(diets.n$collcat))
  diets.n$othercollsci<- ifelse(diets.n$othergencat=="OTHER", "OTHER", paste(diets.n$collsci))
  diets.n$otherpynam<- ifelse(diets.n$otheranalsci=="OTHER", "OTHER", paste(diets.n$pynam))
  
  ##After some categories removed
  diet_plot3 <- ggplot(data=diets.n)+geom_bar(mapping = aes(x=othergencat, y=..prop.., group=1))+ facet_wrap(~location_type)
  diet_plot3 <- diet_plot3+ labs(x = "Item")+ labs(y = "Proportion")
  diet_plot3 <-diet_plot3+ggtitle("Proportion of Prey by Location Type") +theme(plot.title = element_text(hjust = 0.5))
  diet_plot3 <-diet_plot3 +theme(axis.text.x=element_text(angle = 45, hjust=1))
  diet_plot3
  
  diet_plot_summary <- grid.arrange(diet_plot1a,diet_plot1b, diet_plot2a, diet_plot2b, diet_plot3, nrow=3, ncol=2)
  
  ####Which Arthropods??###
  arth.a <- subset(diets.n,source=="ANDRE" & othergensci=="ARTHROPODA")
  arth.n <- subset(diets.n, source=="NOAA" & othergensci=="ARTHROPODA")
  
  ##My data only 
  plot.a <- ggplot (arth.a,aes(x=otheranalcat))+geom_bar(aes(y=(..count..)/sum(..count..)))
  plot.a  <- plot.a+labs(title="Arthropds by Proportion at Site Type",subtitle="ANDRE", y="Proportion", x="Item")
  plot.a <- plot.a+theme(axis.text.x=element_text(angle = 45, hjust=1))
  plot.a ###80% Decapods
  
  ##Between site types
  ### Looks at breakup of arthropods ##  --Mostly decapods at both site types ~70-80%
  plot.b <- ggplot (arth.a[arth.a$location_type=="A",],aes(x=otheranalcat))+geom_bar(aes(y=(..count..)/sum(..count..)))
  plot.b  <- plot.b+labs(title="Arthropds by Proportion at Site Type",subtitle="Artifical", y="Proportion", x="Item")
  plot.b+theme(axis.text.x=element_text(angle = 45, hjust=1))
  plot.b
  plot.c <- ggplot (arth.a[arth.a$location_type=="N",],aes(x=otheranalcat))+geom_bar(aes(y=(..count..)/sum(..count..)))
  plot.c  <- plot.c+labs(title="",subtitle="Natural", y="Proportion", x="Item")
  plot.c+theme(axis.text.x=element_text(angle = 45, hjust=1))
  plot.c
  grid.arrange(plot.b, plot.c, ncol=1, nrow=2)+theme(axis.text.x=element_text(angle = 45, hjust=1))
  
  ##NOAA only
  plot.d <- ggplot (arth.n,aes(x=otheranalcat))+geom_bar(aes(y=(..count..)/sum(..count..)))
  plot.d  <- plot.d+labs(title="Arthropds by Proportion at Site Type",subtitle="NOAA", y="Proportion", x="Item")
  plot.d <- plot.d+theme(axis.text.x=element_text(angle = 45, hjust=1))
  plot.d
  
  ##Between datasets
  grid.arrange(plot.a, plot.d, ncol=1, nrow=2)+theme(axis.text.x=element_text(angle = 45, hjust=1))
  
  ###Propotions remian largeley true for general decapods 
  
  ####Which Decapods?### --mostly cancer & decapods (andre) plus a few shrimp(noaa)
  dec.a <- subset(arth.a, otheranalsci=="DECAPODA")
  dec.n<- subset(arth.n, otheranalsci=="DECAPODA")
  dec_plot_a <- ggplot (dec.a ,aes(x=othercollcat))+geom_bar(aes(y=(..count..)/sum(..count..)))
  dec_plot_a <- dec_plot_a+labs(title="Food proportion Level 3 - Andre", y="Proportion", x="Item")
  dec_plot_a+theme(axis.text.x=element_text(angle = 90, hjust=1))
  dec_plot_n <- ggplot (dec.n,aes(x=othercollcat))+geom_bar(aes(y=(..count..)/sum(..count..)))
  dec_plot_n <- dec_plot_n+labs(title="Food proportion Level 3 - NOAA", y="Proportion", x="Item")
  dec_plot_n+theme(axis.text.x=element_text(angle = 90, hjust=1))+theme(text = element_text(size=5))
  grid.arrange(dec_plot_a, dec_plot_n, ncol=1, nrow=2)
  
  ##The following tables show the intricate details that the plots can't
  table(dec.a$otherpynam)  ### Mostly cancer, decapods, and hermit crabs 
  table(dec.n$otherpynam)  ### Mostly cancer, decapods, and hermit crabs 
  
  ####Which molluscs??###
  moll.a <- subset(diets.n,source=="ANDRE" & othergensci=="MOLLUSCA")
  moll.n <- subset(diets.n, source=="NOAA" & othergensci=="MOLLUSCA")
  
  ##My data only 
  plot.e <- ggplot (moll.a,aes(x=otheranalcat))+geom_bar(aes(y=(..count..)/sum(..count..)))
  plot.e   <- plot.e +labs(title="Molluscs by Proportion at Site Type",subtitle="ANDRE", y="Proportion", x="Item")
  plot.e  <- plot.e +theme(axis.text.x=element_text(angle = 45, hjust=1))
  plot.e  ###Bivalves, cephalopds, gastropods
  
  ##Between site types
  ### Looks at breakup of arthropods ##  --Mostly decapods at both site types ~70-80%
  plot.f <- ggplot (moll.a[moll.a$location_type=="A",],aes(x=otheranalcat))+geom_bar(aes(y=(..count..)/sum(..count..)))
  plot.f  <- plot.f+labs(title="Molluscs by Proportion at Site Type",subtitle="Artifical", y="Proportion", x="Item")
  plot.f+theme(axis.text.x=element_text(angle = 45, hjust=1))
  plot.f
  
  plot.g <- ggplot (moll.a[moll.a$location_type=="N",],aes(x=otheranalcat))+geom_bar(aes(y=(..count..)/sum(..count..)))
  plot.g  <- plot.g+labs(title="",subtitle="Natural", y="Proportion", x="Item")
  plot.g+theme(axis.text.x=element_text(angle = 45, hjust=1))
  plot.g
  grid.arrange(plot.f, plot.g, ncol=1, nrow=2)+theme(axis.text.x=element_text(angle = 45, hjust=1))
  
  ##NOAA only
  plot.h <- ggplot (moll.n,aes(x=otheranalcat))+geom_bar(aes(y=(..count..)/sum(..count..)))
  plot.h  <- plot.h+labs(title="Arthropds by Proportion at Site Type",subtitle="NOAA", y="Proportion", x="Item")
  plot.h <- plot.h+theme(axis.text.x=element_text(angle = 45, hjust=1))
  plot.h
  
  ##Between datasets
  grid.arrange(plot.e, plot.h, ncol=1, nrow=2)+theme(axis.text.x=element_text(angle = 45, hjust=1))
  
  ####Which Bivalves?### --
  bi.a <- subset(moll.a, otheranalsci=="BIVALVIA")
  bi.n<- subset(moll.n, otheranalsci=="BIVALVIA")
  bi_plot_a <- ggplot (moll.a ,aes(x=othercollcat))+geom_bar(aes(y=(..count..)/sum(..count..)))
  bi_plot_a <- bi_plot_a+labs(title="Bivalves - Andre", y="Proportion", x="Item")
  bi_plot_a+theme(axis.text.x=element_text(angle = 90, hjust=1))
  bi_plot_n <- ggplot (moll.n,aes(x=othercollcat))+geom_bar(aes(y=(..count..)/sum(..count..)))
  bi_plot_n <- bi_plot_n+labs(title="Bivalves - NOAA", y="Proportion", x="Item")
  bi_plot_n+theme(axis.text.x=element_text(angle = 90, hjust=1))+theme(text = element_text(size=5))
  grid.arrange(bi_plot_a, bi_plot_n, ncol=1, nrow=2)
  
  View(bi.n)
  ##The following tables show the intricate details that the plots can't
  table(bi.a$otherpynam)  ### basically just bivalves
  table(bi.n$otherpynam)  ### bivalves and other things
  
  ##For Fish
  fish.a <- subset(diets.n,source=="ANDRE" & othergensci=="FISH")
  fish.n <- subset(diets.n, source=="NOAA" & othergensci=="FISH")
  
  ##My data only 
  plot.i <- ggplot (fish.a,aes(x=otheranalcat))+geom_bar(aes(y=(..count..)/sum(..count..)))
  plot.i   <- plot.i +labs(title="Fish by Proportion at Site Type",subtitle="ANDRE", y="Proportion", x="Item")
  plot.i  <- plot.i +theme(axis.text.x=element_text(angle = 45, hjust=1))
  plot.i  
  
  ##Between site types
  ### 
  plot.j <- ggplot (fish.a[fish.a$location_type=="A",],aes(x=otheranalcat))+geom_bar(aes(y=(..count..)/sum(..count..)))
  plot.j  <- plot.j+labs(title="Fish by Proportion at Site Type",subtitle="Artifical", y="Proportion", x="Item")
  plot.j+theme(axis.text.x=element_text(angle = 45, hjust=1))
  plot.j
  
  plot.k <- ggplot (fish.a[fish.a$location_type=="N",],aes(x=otheranalcat))+geom_bar(aes(y=(..count..)/sum(..count..)))
  plot.k  <- plot.k+labs(title="",subtitle="Natural", y="Proportion", x="Item")
  plot.k+theme(axis.text.x=element_text(angle = 45, hjust=1))
  plot.k
  grid.arrange(plot.j, plot.k, ncol=1, nrow=2)+theme(axis.text.x=element_text(angle = 45, hjust=1))
  
  ##NOAA only
  plot.l <- ggplot (fish.n,aes(x=otheranalcat))+geom_bar(aes(y=(..count..)/sum(..count..)))
  plot.l  <- plot.l+labs(title="Fish by Proportion at Site Type",subtitle="NOAA", y="Proportion", x="Item")
  plot.l <- plot.l+theme(axis.text.x=element_text(angle = 45, hjust=1))
  plot.l
  
  ##Between datasets
  grid.arrange(plot.i, plot.l, ncol=1, nrow=2)+theme(axis.text.x=element_text(angle = 45, hjust=1))
  
  ##The following tables show the intricate details that the plots can't
  table(fish.a$otherpynam)  ### basically just bony fish
  table(fish.n$otherpynam)  ### small bony fish
} ##end "deep in the weeds" stomach code
if(Fold){
  ####CHI-Squares for diet and location####
  diet_test <- filter(diets.n, source=="ANDRE")
  
  ###Independence
  #diet_test.n <- select(diet_test, c(gencat, pdgutw, location_type))
  with (diet_test, table (gencat, location_type))
  table1=matrix(c(109,42,17,7,22,37),ncol=3)
  colnames(table1)=c("Arthro","Fish", "Mollus")
  rownames(table1)=c("Artificial","Natural")
  table1
  chisq.test(table1)  ## p=1.07e-5 Signif diffrence at prey count by site
  
  with (diet_test, table (gencat, sizecat))
  table2=matrix(c(90,61,11,13,30,29),ncol=3)
  colnames(table2)=c("Arthro","Fish", "Mollus")
  rownames(table2)=c("Small","Med")
  table2
  chisq.test(table2)  ## p=0.29  No signif diffrence at prey consumption by count by site
  
  with (diet_test, table (gencat, sex))
  table3=matrix(c(96,52,6,7,42,16),ncol=3)
  colnames(table3)=c("Arthro","Fish", "Mollus")
  rownames(table3)=c("Female","Male")
  table3
  chisq.test(table3) ## p=0.18  No signif diffrence at prey consumption by count by site
  
  
  ##Sex distribution tests
  with (diet_test, table (sex, location_type))
  table4=matrix(c(0.61,0.38,0.58,0.42 ),ncol=2)
  colnames(table4)=c("Art","Nat")
  rownames(table4)=c("Female","Male")
  table4
  chisq.test(table4)
  
  ##For Supplementals
  diet_table<- table(diets.n$pynam,diets.n$source)
  with(diets.n[diets.n$source=="ANDRE",],table(pynam,location_type))
} ##Fold chi-squares for diet at locatio & size
##Disclaimer: PSIRI section is painfully long.  Will remain so until I find time to write a function to atutomate the process
if (Fold){
  #####PSIRI PLOTS######
  ######PSIRI Plot --- General Composition for NATURAL Sites- Figure 4 left####### 
  with (diet_test[diet_test$location_type=="N",], table (othergencat))
  ps.gen_nat <- select(diet_test[diet_test$location_type=="N",], othergencat, pyamtw)
  aggregate(ps.gen_nat[, 2], list(ps.gen_nat$othergencat), sum)
  
  
  labels <- c("Annellid","Arthropod","Fish","Mollusc")
  ##Actual number
  N <- c(22, 42,7,37)
  ##Actual Weight 
  W <- c(9.76,58.48,12.27,35.62)
  ##Next lines are percent number and percent weight - divided by 108 sotmachs total
  N <- c(20, 39,7,34)
  ##Next line is percent weight where total weight =116.13
  W <- c(8.4,50.3, 10.6,30.7)
  ## n=108 and FO=ni/n
  FO <- c(0.204,0.389,0.06,0.343)
  
  #This generates the plot using the barplot function twice in R
  barplot(height=N,
          width=FO,
          ylim=c(-100,100),
          space=0,
          yaxt="n"
  )
  
  barplot(height=-1*W,
          width=FO,
          space=0,
          yaxt="n",
          add=TRUE)
  
  #Generate Y-axis with the axis function and label with the mtext function 
  axis(side=2,
       at=seq(from=-100,to=100,by=25),
       labels=c(seq(100,0,-25),seq(25,100,25))
  )
  
  mtext(c("%PN","%PW"),
        side=2,
        line=2,
        at=c(50,-50)
  )
  
  #Generate FO scale bar -- this will not always be the same size from plot to plot unless you set the size of each barplot equal with par!!!
  center <- 0.5*sum(FO) #This is the center of the X-axis
  
  axis(side=1,
       at=c(center-25,center,center+25),
       labels=c(0,25,50)
  )
  
  mtext("%FO",
        side=1,
        line=2,
        at=center)
  
  #Label the bars
  #This positions the labels over the middle of each bar
  #This may get ugly if there are many bars and labels!!!
  text(cumsum(FO) - 0.5*FO,N+5,srt= 90,adj=0,labels)
  
  #dev.copy(png,'PSIRI_natural.png', width=2000, height=1600, res=200) ; dev.off()
  
  
  ######PSIRI Plot --- General Composition for ARTIFICIAL Sites- Figure 4 right########
  with (diet_test[diet_test$location_type=="A",], table (othergencat))
  ps.gen_art <- select(diet_test[diet_test$location_type=="A",], othergencat, pyamtw)
  aggregate(ps.gen_art[, 2], list(ps.gen_art$othergencat), sum)
  
  labels <- c("Annellid","Arthropod","Fish","Mollusc")
  ##Actual number
  N <- c(3,110,17,22)
  ##Actual Weight 
  W <- c(2.7, 223.5,26.1,59.7)
  ##Next lines are percent number and percent weight - divided by 152 sotmachs total
  N <- c(2, 72,11,14)
  ##Next line is percent weight where total weight =312
  W <- c(8,71, 8.4,19.1)
  ## n=152 and FO=ni/n
  FO <- c(0.02,0.72,0.11,0.14)
  
  #This generates the plot using the barplot function twice in R
  barplot(height=N,
          width=FO,
          ylim=c(-100,100),
          space=0,
          yaxt="n"
  )
  
  barplot(height=-1*W,
          width=FO,
          space=0,
          yaxt="n",
          add=TRUE)
  
  #Generate Y-axis with the axis function and label with the mtext function 
  axis(side=2,
       at=seq(from=-100,to=100,by=25),
       labels=c(seq(100,0,-25),seq(25,100,25))
  )
  
  mtext(c("%PN","%PW"),
        side=2,
        line=2,
        at=c(50,-50)
  )
  
  #Generate FO scale bar -- this will not always be the same size from plot to plot unless you set the size of each barplot equal with par!!!
  center <- 0.5*sum(FO) #This is the center of the X-axis
  
  axis(side=1,
       at=c(center-25,center,center+25),
       labels=c(0,25,50)
  )
  
  mtext("%FO",
        side=1,
        line=2,
        at=center)
  
  #Label the bars
  #This positions the labels over the middle of each bar
  #This may get ugly if there are many bars and labels!!!
  text(cumsum(FO) - 0.5*FO,N+5,srt= 90,adj=0,labels)
  
  #dev.copy(png,'PSIRI_Artif.png', width=2000, height=1600, res=200) ; dev.off()
  
  
  
  
  ######PSIRI Nat 2018 -- Supplementary 2 left#######
  labels <- c("Annellid","Arthropod","Fish","Mollusc")
  ##Actual number 
  N <- c(22, 42,7,37)
  ##Actual weight
  W <- c(12.46,143.65,29.98,75.6)
  ##Next lines are percent number and percent weight - divided by 108 sotmachs total
  N <- c(20.3, 38.9,6.5,34.3)
  ##Next line is percent weight where total weight =261.7
  W <- c(4.8,54.8, 11.5,28.9)
  ## n=108 and FO=ni/n
  FO <- c(0.204,0.389,0.064,0.343)
  
  #This generates the plot using the barplot function twice in R
  barplot(height=N,
          width=FO,
          ylim=c(-100,100),
          space=0,
          yaxt="n"
  )
  
  barplot(height=-1*W,
          width=FO,
          space=0,
          yaxt="n",
          add=TRUE)
  
  #Generate Y-axis with the axis function and label with the mtext function 
  axis(side=2,
       at=seq(from=-100,to=100,by=25),
       labels=c(seq(100,0,-25),seq(25,100,25))
  )
  
  mtext(c("%PN","%PW"),
        side=2,
        line=2,
        at=c(50,-50)
  )
  
  #Generate FO scale bar -- this will not always be the same size from plot to plot unless you set the size of each barplot equal with par!!!
  center <- 0.5*sum(FO) #This is the center of the X-axis
  
  axis(side=1,
       at=c(center-25,center,center+25),
       labels=c(0,25,50)
  )
  
  mtext("%FO",
        side=1,
        line=2,
        at=center)
  
  #Label the bars
  #This positions the labels over the middle of each bar
  #This may get ugly if there are many bars and labels!!!
  text(cumsum(FO) - 0.5*FO,N+5,srt= 90,adj=0,labels) 
  
  temp <- locator(1) # On the chart, click where you would like the text to appear
  text(temp,"General",srt= 90, font=2)
  
  dev.copy(png,'PSIRI_Natual_2018.png', width=2000, height=1600, res=200) ; dev.off()
  
  
  
  
  
  
  
  ######PSIRI Nat 2016 -- Supplementary 1 right#######
  
  with (diet_test[diet_test$location_type=="N",], table (othergencat))
  ps.gen_nat <- select(diet_test[diet_test$location_type=="N",], othergencat, pyamtw)
  ps.gen_nat <- select(diet_test[diet_test$year.x=="2016",], othergencat, pyamtw)
  aggregate(ps.gen_nat[, 2], list(ps.gen_nat$othergencat), sum)
  
  ##Artficial 2018
  labels <- c("Annellid","Arthropod","Fish","Mollusc")
  ##Actual number 
  N <- c(25, 125,24,59)
  ##Actual weight
  W <- c(12.46,143.65,29.98,75.6)
  ##Next lines are percent number and percent weight - divided by 233 sotmachs total
  N <- c(10.7, 53.6,10.3,25.3)
  ##Next line is percent weight where total weight =428.13
  W <- c(2.9,65.9, 8.9,22.2)
  ## n=233 and FO=ni/n
  FO <- c(0.107,.537,.103,0.253)
  
  #This generates the plot using the barplot function twice in R
  barplot(height=N,
          width=FO,
          ylim=c(-100,100),
          space=0,
          yaxt="n"
  )
  
  barplot(height=-1*W,
          width=FO,
          space=0,
          yaxt="n",
          add=TRUE)
  
  #Generate Y-axis with the axis function and label with the mtext function 
  axis(side=2,
       at=seq(from=-100,to=100,by=25),
       labels=c(seq(100,0,-25),seq(25,100,25))
  )
  
  mtext(c("%PN","%PW"),
        side=2,
        line=2,
        at=c(50,-50)
  )
  
  #Generate FO scale bar -- this will not always be the same size from plot to plot unless you set the size of each barplot equal with par!!!
  center <- 0.5*sum(FO) #This is the center of the X-axis
  
  axis(side=1,
       at=c(center-25,center,center+25),
       labels=c(0,25,50)
  )
  
  mtext("%FO",
        side=1,
        line=2,
        at=center)
  
  #Label the bars
  #This positions the labels over the middle of each bar
  #This may get ugly if there are many bars and labels!!!
  text(cumsum(FO) - 0.5*FO,N+5,srt= 90,adj=0,labels) 
  
  temp <- locator(1) # On the chart, click where you would like the text to appear
  text(temp,"General",srt= 90, font=2)
  
  #dev.copy(png,'PSIRI_Natual_2016.png', width=2000, height=1600, res=200) ; dev.off()
  
  ######PSIRI Art 2018 -- Supplementary 2 right#######
  with (diet_test[diet_test$location_type=="A",], table (othergencat))
  ps.gen_nat <- select(diet_test[diet_test$location_type=="A",], othergencat, pyamtw)
  ps.gen_nat <- select(diet_test[diet_test$year.x=="2018",], othergencat, pyamtw)
  aggregate(ps.gen_nat[, 2], list(ps.gen_nat$othergencat), sum)
  
  ##Artficial 2018
  labels <- c("Annellid","Arthropod","Fish","Mollusc")
  ##Actual number 
  N <- c(3, 110,17,22)
  ##Actual weight
  W <- c(12.5,143.7,29.9,75.6)
  ##Next lines are percent number and percent weight - divided by 152 sotmachs total
  N <- c(19.7,72.4,11.1,14.5)
  ##Next line is percent weight where total weight =261.7
  W <- c(4.7,54.9, 11.4,28.9)
  ## n=152 and FO=ni/n
  FO <- c(0.02,0.72,.11,0.14)
  
  #This generates the plot using the barplot function twice in R
  barplot(height=N,
          width=FO,
          ylim=c(-100,100),
          space=0,
          yaxt="n"
  )
  
  barplot(height=-1*W,
          width=FO,
          space=0,
          yaxt="n",
          add=TRUE)
  
  #Generate Y-axis with the axis function and label with the mtext function 
  axis(side=2,
       at=seq(from=-100,to=100,by=25),
       labels=c(seq(100,0,-25),seq(25,100,25))
  )
  
  mtext(c("%PN","%PW"),
        side=2,
        line=2,
        at=c(50,-50)
  )
  
  #Generate FO scale bar -- this will not always be the same size from plot to plot unless you set the size of each barplot equal with par!!!
  center <- 0.5*sum(FO) #This is the center of the X-axis
  
  axis(side=1,
       at=c(center-25,center,center+25),
       labels=c(0,25,50)
  )
  
  mtext("%FO",
        side=1,
        line=2,
        at=center)
  
  #Label the bars
  #This positions the labels over the middle of each bar
  #This may get ugly if there are many bars and labels!!!
  text(cumsum(FO) - 0.5*FO,N+5,srt= 90,adj=0,labels) 
  
  temp <- locator(1) # On the chart, click where you would like the text to appear
  text(temp,"General",srt= 90, font=2)
  
  dev.copy(png,'PSIRI_Artificial_2018.png', width=2000, height=1600, res=200) ; dev.off()
  
  
  ######PSIRI Art 2016 -- Supplementary 1 right#########
  
  with (diet_test[diet_test$location_type=="A",], table (othergencat))
  ps.gen_nat <- select(diet_test[diet_test$location_type=="A",], othergencat, pyamtw)
  #ps.gen_nat <- select(diet_test[diet_test$location_type=="A",], othergencat, pyamtw, year.x)
  #ps.gen_nat <- select(ps.gen_nat[ps.gen_nat$year.x=="2016",], othergencat, pyamtw)
  ps.gen_nat <- select(diet_test[diet_test$year.x=="2016",], othergencat, pyamtw)
  aggregate(ps.gen_nat[, 2], list(ps.gen_nat$othergencat), sum)
  
  ##Artficial 2016
  labels <- c("Annellid","Arthropod","Fish","Mollusc")
  ##Actual number 
  N <- c(3, 110,17,22)
  ##Actual weight
  W <- c(2.691,138.3,8.43,19.7)
  ##Next lines are percent number and percent weight - divided by 152 sotmachs total
  N <- c(2.0,72.4,11.1,14.5)
  ##Next line is percent weight where total weight =261.7
  W <- c(1.6,81.8, 5.0,11.6)
  ## n=152 and FO=ni/n
  FO <- c(0.02,0.72,.11,0.14)
  
  #This generates the plot using the barplot function twice in R
  barplot(height=N,
          width=FO,
          ylim=c(-100,100),
          space=0,
          yaxt="n"
  )
  
  barplot(height=-1*W,
          width=FO,
          space=0,
          yaxt="n",
          add=TRUE)
  
  #Generate Y-axis with the axis function and label with the mtext function 
  axis(side=2,
       at=seq(from=-100,to=100,by=25),
       labels=c(seq(100,0,-25),seq(25,100,25))
  )
  
  mtext(c("%PN","%PW"),
        side=2,
        line=2,
        at=c(50,-50)
  )
  
  #Generate FO scale bar -- this will not always be the same size from plot to plot unless you set the size of each barplot equal with par!!!
  center <- 0.5*sum(FO) #This is the center of the X-axis
  
  axis(side=1,
       at=c(center-25,center,center+25),
       labels=c(0,25,50)
  )
  
  mtext("%FO",
        side=1,
        line=2,
        at=center)
  
  #Label the bars
  #This positions the labels over the middle of each bar
  #This may get ugly if there are many bars and labels!!!
  text(cumsum(FO) - 0.5*FO,N+5,srt= 90,adj=0,labels) 
  
  temp <- locator(1) # On the chart, click where you would like the text to appear
  text(temp,"General",srt= 90, font=2)
  
  #dev.copy(png,'PSIRI_Artificial_2016.png', width=2000, height=1600, res=200) ; dev.off()
  
} ##End Fold for PSIRI construction/ fig4, supp 1 & 2
#######Stable Isotopes/ Figures 5 & 6####
if (Fold){
  ##Define Consumer level
  isotope.original$consumer_level<- ifelse(isotope.original$consumer=="BSB","2","1")
  ##Define Location Type
  isotope.original$location_type<- ifelse(isotope.original$location=="NB_ONE", "N",
                                          ifelse(isotope.original$location=="NB_TWO","N","A"))
  #subset, audit, and merge the data
  pred <- isotope.original[1:407,]
  prey <-isotope.original[408:440,]
  prey$c13m <-paste(prey$c13l)
  prey$c13s <-paste(prey$c13l)
  prey$n15m <-paste(prey$n15l)
  prey$n15s <-paste(prey$n15l)
  
  isotope <- rbind(pred, prey)
  colnames(isotope)
  #### Removes incomplete entries so means aren't affected
  isotope$isoname <- ifelse(isotope$consumer=="BSB","", paste(isotope$consumer))
  colnames(isotope)
  ##Adds sizecat
  isotope$sizecat[isotope$len<=25] <- "S"
  isotope$sizecat[isotope$len>25] <- "M"
  isotope$sizecat[isotope$len>50] <- "L"
  
  isotope$isoname[isotope$isoname == "Slug"] <- "Nudibranch"
  
  isotope$c13m <- as.numeric(isotope$c13m)
  isotope$n15m <- as.numeric(isotope$n15m)
  isotope$c13m <-  as.numeric(isotope$c13m)
  isotope$c13s <- as.numeric(isotope$c13s)
  isotope$n15s <-  as.numeric(isotope$n15s)
  
  ######Correcting livers for lipids - if it's ever needed
  colnames(isotope)
  lip_correct <- select(isotope, 1:19)
  colnames(lip_correct)
  
  lip_correct$isoratio_l <-(isotope$c13l)/(isotope$n15l)  ## Will give C:N ratio for liver when raw values added
  
  ###Data Visuals###
  bsb <- isotope[isotope$consumer_level=="2",]
  bsb$losiz <- paste(bsb$location_type,"|",bsb$sizecat)
  
  bsb$c13m <- as.numeric(bsb$c13m)
  bsb$n15m <- as.numeric(bsb$n15m)
  bsb$c13l <-  as.numeric(bsb$c13l)
  bsb$n15l <-  as.numeric(bsb$n15l)
  bsb$c13s <- as.numeric(bsb$c13s)
  bsb$n15s <-  as.numeric(bsb$n15s)
} ##Fold stable isotope wrangling
if (Fold){
  ###### Q1:Are SI values different at Location? or size### 
  elipse1 <- ggplot(bsb, aes(c13l, n15l, color=losiz))+labs(color="Location | Size")+
    labs(x=(expression("∂"^13*"C (‰)")),y=(expression("∂"^15*"N (‰)")))+
    geom_point() +stat_ellipse()+
    ggtitle("∂ Values by Site Type, Fish Size, and Tissue Type", subtitle = "Liver")+
    theme(plot.title = element_text(hjust = 0.5))+theme(axis.text=element_text(size=18),
                                                        axis.title=element_text(size=18,face="bold"))+
    theme(legend.text=element_text(size=14))+theme(legend.title=element_text(size=18))+
    theme(plot.subtitle=element_text(size=18))#+ theme(legend.position = "none") 
  
  elipse2 <- ggplot(bsb, aes(c13m, n15m, color=losiz))+labs(color="Location | Size")+
    labs(x=(expression("∂"^13*"C (‰)")),y=(expression("∂"^15*"N (‰)")))+
    geom_point() +stat_ellipse()+
    ggtitle("", subtitle = "Muscle")+
    theme(plot.title = element_text(hjust = 0.5))+theme(axis.text=element_text(size=18),
                                                        axis.title=element_text(size=18,face="bold"))+
    theme(legend.text=element_text(size=14))+theme(legend.title=element_text(size=18))+
    theme(plot.subtitle=element_text(size=18))+theme(legend.position = "none") 
  
  elipse3 <- ggplot(bsb, aes(c13s, n15s, color=losiz))+labs(color="Location | Size")+
    labs(x=(expression("∂"^13*"C (‰)")),y=(expression("∂"^15*"N (‰)")))+
    geom_point() +stat_ellipse()+
    ggtitle("", subtitle = "Mucus")+
    theme(plot.title = element_text(hjust = 0.5))+theme(axis.text=element_text(size=18),
                                                        axis.title=element_text(size=18,face="bold"))+
    theme(legend.text=element_text(size=14))+theme(legend.title=element_text(size=18))+
    theme(plot.subtitle=element_text(size=18))
  
  #install.packages("ggpubr")
  library(ggpubr)
  Figure5 <- ggarrange(elipse2, elipse1, elipse3,ncol=1, nrow=3,
                       common.legend = TRUE, legend = "right")
  dev.copy(png,'Figure5.png', width=2000, height=5000, res=200) ; dev.off()
} ## fold stable isotope figures
if (Fold){
  ##### ANOVAS for Stable Isotopes ###
  isotope.stats <- isotope[isotope$consumer_level=="2",]
  
  ###LIVER###
  #Nitrogen in liver###
  #by location and size
  lm.nllocsiz<- lm(n15l~location_type*sizecat, data = isotope.stats)
  ## Analysis of Variance
  anova(lm.nllocsiz)  ###  4.338e-07 *** for Location   and 0.03185 for SIZECAT
  ##Carbon in liver###
  #carbon in mucus by location###
  lm.cllocsiz<- lm(c13l~location_type*sizecat, data = isotope.stats)
  ## Analysis of Variance
  anova(lm.cllocsiz)  ###<2e-16 *** for location type only 
  
  
  ###MUSCLE##
  #####Nitrogen in muscle###
  lm.nmlocsiz<- lm(n15m~location_type*sizecat, data = isotope.stats)
  ## Analysis of Variance
  anova(lm.nmlocsiz) ##7.249e-09 ***
  ####Carbon in muscle###
  lm.cmlocsiz<- lm(c13m~location_type*sizecat, data = isotope.stats)
  ## Analysis of Variance
  anova(lm.cmlocsiz) ##7.249e-09 ***
  
  ###MUCUS##
  #Nitrogen in mucus###
  #by location and size
  lm.nslocsiz<- lm(n15s~location_type*sizecat, data = isotope.stats)
  ## Analysis of Variance
  anova(lm.nslocsiz)  ###0.03185 for SIZECAT
  ##Carbon in mucus###
  #carbon in mucus by location###
  lm.cslocsiz<- lm(c13s~location_type*sizecat, data = isotope.stats)
  ## Analysis of Variance
  anova(lm.cslocsiz)  ###Location 6.887e-06 ***  ##Sizecat6.209e-05 ***
  
}##Fold Stable Isotope stats
if (Fold){
  
  ##Predator vs. Prey from trawls isotope figures
  consumer1 <-ggplot(isotope, aes(c13m, n15m, color=consumer)) +
    geom_point() +
    stat_ellipse()+ggtitle("∂ Value by Consumer",subtitle = "Muscle")+labs(color="Consumer")+
    labs(x=(expression("∂"^13*"C (‰)")),y=(expression("∂"^15*"N (‰)")))+
    theme(plot.title = element_text(hjust = 0.5))+
    theme(axis.text=element_text(size=18, face="bold"))+
    theme(legend.text=element_text(size=14))+theme(legend.title=element_text(size=18))+
    theme(plot.subtitle=element_text(size=18))
  
  consumer2<- ggplot(isotope, aes(c13l, n15l, color=consumer)) +
    geom_point() +
    labs(x=(expression("∂"^13*"C (‰)")),y=(expression("∂"^15*"N (‰)")))+
    stat_ellipse()+ggtitle("", subtitle = "Liver")+
    theme(plot.title = element_text(hjust = 0.5))+
    theme(axis.text=element_text(size=18, face="bold"))+
    theme(legend.text=element_text(size=14))+theme(legend.title=element_text(size=18))+
    theme(plot.subtitle=element_text(size=18))
  
  consumer3 <- ggplot(isotope, aes(c13s, n15s, color=consumer)) +
    geom_point() +
    labs(x=(expression("∂"^13*"C (‰)")),y=(expression("∂"^15*"N (‰)")))+
    stat_ellipse()+ggtitle("",subtitle = "Mucus")+
    theme(plot.title = element_text(hjust = 0.5))+
    theme(axis.text=element_text(size=18, face="bold"))+
    theme(legend.text=element_text(size=14))+theme(legend.title=element_text(size=18))+
    theme(plot.subtitle=element_text(size=18))
  
  
  Figure6 <- ggarrange(consumer1, consumer2, consumer3,ncol=1, nrow=3,
                       common.legend = TRUE, legend = "right")
  dev.copy(png,'Consumer Comparison Figure.png', width=2000, height=5000, res=200) ; dev.off()
}##Fold Predator vs. Prey isotope signatures/ Figure6
if (Fold){
  ##Nitrogen in mucus by Consumer Level###
  lm.loconls<- lm(n15s~location_type*consumer_level, data = isotope)
  #Analysis of Variance
  anova(lm.loconls)  ##0.007493 ** locaotin  < 2.2e-16 *** consumer
  ##Carbon in mucus###
  locons.lm <- lm(c13s~consumer_level, data = isotope)
  coef(locons.lm)
  # Analysis of Variance
  anova(locons.lm)  ###0.04003 *
  ##nitrogen in mucus###
  locons.lm <- lm(n15s~consumer_level, data = isotope)
  coef(locons.lm)
  # Analysis of Variance
  anova(locons.lm)  ###2.2e-16 ***
  #Nitrogen in liver by Consumer Level###
  lm.loconll<- lm(n15l~location_type*consumer_level, data = isotope)
  # Analysis of Variance
  anova(lm.loconll)  ##0.0002644 ***  location < 2.2e-16 *** consumer
  ##Carbon in liver###
  loconl.lm <- lm(c13l~consumer_level, data = isotope)
  coef(loconl.lm)
  #Analysis of Variance
  anova(loconl.lm)  ###0.04003 *
  ##nitrogen in liver###
  loconl.lm <- lm(n15l~consumer_level, data = isotope)
  coef(loconl.lm)
  #Analysis of Variance
  anova(loconl.lm)  ###2.2e-16 ***
  #Nitrogen in muscle by Consumer Level###
  lm.loconlm<- lm(n15m~location_type*consumer_level, data = isotope)
  # nalysis of Variance
  anova(lm.loconlm)  
  ##Carbon in muscle ###
  loconm.lm <- lm(c13m~consumer_level, data = isotope)
  coef(loconm.lm)
  # Analysis of Variance
  anova(loconm.lm)  ###0.04003 *
  ##nitrogen in muscle ###
  loconm.lm <- lm(n15m~consumer_level, data = isotope)
  coef(loconm.lm)
  # Analysis of Variance
  anova(loconm.lm)  ###2.2e-16 ***
} ##Fold Statistics/ ANOVAS for stable isotope analysis and consumer 



#######Mapping for figure 1######
if(Fold){
  #this section runs separately and independently.  It utilizes another dataset.
  
  ##Install needed packages
  ##Sometimes R has trouble recognizing this, if so, type in using RStudio package installer 
  install.packages("sf", "maptools", "tmap", "tmaptools", "leaflet", "rgdal")
  install.packages(c("cowplot", "googleway","ggrepel", "ggmap",
                     "ggspatial", "rgeos", "libwgeom", "sf", "rnaturalearth", "rnaturalearthdata"))
  ##call packages
  library(sf)
  library(maptools)
  library(tmap)
  library(tmaptools)
  library(leaflet)
  library(rgdal)
  library(rnaturalearth)
  library(rnaturalearthdata)
  library(rgeos)
  library(ggplot2)
  library(ggmap)
  library(ggspatial) ##This is sed for arrow orientation
  
  setwd(HD)
  mapsites<- read.csv("site_coordinates.csv")
  mapsites$Long <- mapsites[, 2] + 0.006
  mapsites$latlong <- paste(mapsites$Site, "|", mapsites$Year)
  
  library(sf) 
  library(rnaturalearth) 
  library(rnaturalearthdata) 
  library(ggplot2)
  mapsites<- read.csv("site_coordinates.csv") 
  mapsites$Long <- mapsites[, 2] + 0.2
  mapsites$Year <- as.factor(mapsites$Year) 
  world <- ne_countries(scale = "medium", returnclass = "sf") 
  ggplot(data = world)+
    geom_sf()+
    geom_point(data= mapsites, size=5, aes(x = Long, y = Lat, color= Site, shape= Year))+
    labs(x="Long", y="Lat", colour="Site Type", shape="Year")+ 
    scale_color_manual(values=c("cyan3", "coral1"), 
                       labels=c("Nat","Art"))+ 
    scale_shape_manual(values=c(6,17))+
    coord_sf(xlim = c(-77, -71.5), ylim = c(34.5, 40.1), expand = FALSE)+ 
    annotate(geom = "text", x = -74.5, y = 37.25, label = "Atlantic Ocean", fontface = "italic", color = "grey22", size = 6)+ 
    xlab("Longitude") + 
    ylab("Latitude") + 
    ggtitle("Sampling Locations in 2016 and 2018")+
    theme(plot.title = element_text(hjust = 0.5))+
    geom_rect(aes(xmin = -76, xmax = -72, ymin = 35, ymax = 40),
              fill = "transparent", color = "deeppink4", size = 1.0)+
    annotation_north_arrow(location="bl", which_north="true", style=north_arrow_nautical)
  dev.copy(png,'Figure1.png', width=2000, height=1600, res=200) ; dev.off()
} ##Fold Mapping code




