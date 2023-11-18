# LOADING ALL THE NECESSARY LIBRARIES
options(warn = -1)
library(astsa, quietly=TRUE, warn.conflicts=FALSE)
library(ggplot2)
library(knitr)
library(printr)
library(plyr)
library(dplyr)
library(lubridate)
library(gridExtra)
library(reshape2)
library(TTR)
library(tidyr)
library(tidyverse)
library(modelr)
library(broom)
library(caret)
library(zoo)
library(forecast)



wdx<-read.csv("water_dataX_new.csv")
dim(wdx)

view(wdx)
str(wdx)

count_unique<-rapply(wdx, function(x) length(unique(x)))
count_unique


wdx$Temp[is.na(wdx$Temp)]=median(wdx$Temp, na.rm = TRUE)
wdx$DO[is.na(wdx$DO)]=median(wdx$DO, na.rm = TRUE)
wdx$PH[is.na(wdx$PH)]=median(wdx$PH, na.rm = TRUE)
wdx$NITRITE_N_NITRATE[is.na(wdx$NITRITE_N_NITRATE)]=median(wdx$NITRITE_N_NITRATE, na.rm = TRUE)



sum(wdx$CONDUCTIVITY=="NAN")
sum(wdx$CONDUCTIVITY=="NaN")
wdx$CONDUCTIVITY[wdx$CONDUCTIVITY=="NAN"]<-0
wdx$CONDUCTIVITY[wdx$CONDUCTIVITY=="NaN"]<-0
wdx$CONDUCTIVITY<-as.numeric(wdx$CONDUCTIVITY)
wdx$CONDUCTIVITY[wdx$CONDUCTIVITY==0]<-median(wdx$CONDUCTIVITY)
str(wdx$CONDUCTIVITY)

sum(wdx$BOD=="NAN")
wdx$BOD[wdx$BOD=="NAN"]<-0
wdx$BOD<-as.numeric(wdx$BOD)
wdx$BOD[wdx$BOD==0]<-median(wdx$BOD, na.rm = TRUE)
sum(is.na(wdx$BOD))
wdx$BOD[is.na(wdx$BOD)]=median(wdx$BOD, na.rm = TRUE)
str(wdx$BOD)
sum(is.na(wdx$BOD))

sum(wdx$FECAL_COLIFORM=="NAN")
wdx$FECAL_COLIFORM[wdx$FECAL_COLIFORM=="NAN"]<-0
wdx$FECAL_COLIFORM<-as.numeric(wdx$FECAL_COLIFORM)
wdx$FECAL_COLIFORM[wdx$FECAL_COLIFORM==0]<-median(wdx$FECAL_COLIFORM, na.rm = TRUE)
sum(is.na(wdx$FECAL_COLIFORM))
wdx$FECAL_COLIFORM[is.na(wdx$FECAL_COLIFORM)]=median(wdx$FECAL_COLIFORM, na.rm = TRUE)
str(wdx$FECAL_COLIFORM)
sum(is.na(wdx$FECAL_COLIFORM))

sum(wdx$TOTAL_COLIFORM_MEAN=="NAN")
wdx$TOTAL_COLIFORM_MEAN[wdx$TOTAL_COLIFORM_MEAN=="NAN"]<-0
wdx$TOTAL_COLIFORM_MEAN<-as.numeric(wdx$TOTAL_COLIFORM_MEAN)
wdx$TOTAL_COLIFORM_MEAN[wdx$TOTAL_COLIFORM_MEAN==0]<-median(wdx$TOTAL_COLIFORM_MEAN, na.rm = TRUE)
sum(is.na(wdx$TOTAL_COLIFORM_MEAN))
wdx$TOTAL_COLIFORM_MEAN[is.na(wdx$TOTAL_COLIFORM_MEAN)]=median(wdx$TOTAL_COLIFORM_MEAN, na.rm = TRUE)
str(wdx$TOTAL_COLIFORM_MEAN)
sum(is.na(wdx$TOTAL_COLIFORM_MEAN))


wdx_1 <- wdx[-c(1,2)]
colnames(wdx_1)<-c("state", "temp", "DO", "PH", "conductivity", "BOD", "nitrites_n_nitrates", "FECAL_COLIFORM", "TOTAL_COLIFORM", "year")

wdx_1<-group_by(wdx_1, state, year )
group_wdx_1<-summarize(wdx_1, median(temp), median(DO), median(PH), median(conductivity), median(BOD), median(nitrites_n_nitrates), median(FECAL_COLIFORM), median(TOTAL_COLIFORM))

list_df <- split(group_wdx_1, group_wdx_1$state) #split the dataset into a list of datasets based on the value of iris$Species
list2env(list_df, envir= .GlobalEnv)

KERALA[["year"]]<-strptime(KERALA[["year"]], format="%Y")
KERALA$year <- format(KERALA$year, "%Y")

MAHARASHTRA[["year"]]<-strptime(MAHARASHTRA[["year"]], format="%Y")
MAHARASHTRA$year <- format(MAHARASHTRA$year, "%Y")

GOA[["year"]]<-strptime(GOA[["year"]], format="%Y")
GOA$year <- format(GOA$year, "%Y")

PUNJAB[["year"]]<-strptime(PUNJAB[["year"]], format="%Y")
PUNJAB$year <- format(PUNJAB$year, "%Y")

TAMILNADU[["year"]]<-strptime(TAMILNADU[["year"]], format="%Y")
TAMILNADU$year <- format(TAMILNADU$year, "%Y")

GUJARAT[["year"]]<-strptime(GUJARAT[["year"]], format="%Y")
GUJARAT$year <- format(GUJARAT$year, "%Y")


statelist = c("KERALA", "MAHARASHTRA", "GOA", "PUNJAB", "TAMILNADU", "GUJARAT")

KERALA <- as.data.frame(KERALA)
MAHARASHTRA <- as.data.frame(MAHARASHTRA)
GOA <- as.data.frame(GOA)
PUNJAB <- as.data.frame(PUNJAB)
TAMILNADU <- as.data.frame(TAMILNADU)
GUJARAT <- as.data.frame(GUJARAT)

########################################################
########################################################
################# MY PART-> ############################

theme_set(
  theme_classic() + 
    theme(legend.position = "top"))

# STATE VS TEMPERATURE 


tempplot <- ggplot(NULL, aes(year, `median(temp)`, group = 1)) +
  geom_line(data=KERALA, col = "orange") +
  geom_line(data=MAHARASHTRA, col = "green") +
  geom_line(data=GOA, col = "blue") +
  geom_line(data=PUNJAB, col = "darkcyan") +
  geom_line(data=TAMILNADU, col = "purple") +
  geom_line(data=GUJARAT, col = "red")
tempplot


# STATE VS DISSOLVED OXYGEN
doplot <- ggplot(NULL, aes(year, `median(DO)`, group = 1)) +
  geom_line(data=KERALA, col = "orange") +
  geom_line(data=MAHARASHTRA, col = "green") +
  geom_line(data=GOA, col = "blue") +
  geom_line(data=PUNJAB, col = "darkcyan") +
  geom_line(data=TAMILNADU, col = "purple") +
  geom_line(data=GUJARAT, col = "red")
doplot



#STATE VS PH
KERALA$`median(PH)`[KERALA$`median(PH)`==max(KERALA$`median(PH)`)]<-median(KERALA$`median(PH)`)
MAHARASHTRA$`median(PH)`[MAHARASHTRA$`median(PH)`==max(MAHARASHTRA$`median(PH)`)]<-median(MAHARASHTRA$`median(PH)`)
GOA$`median(PH)`[GOA$`median(PH)`==max(GOA$`median(PH)`)]<-median(GOA$`median(PH)`)
PUNJAB$`median(PH)`[PUNJAB$`median(PH)`==max(PUNJAB$`median(PH)`)]<-median(PUNJAB$`median(PH)`)
TAMILNADU$`median(PH)`[TAMILNADU$`median(PH)`==max(TAMILNADU$`median(PH)`)]<-median(TAMILNADU$`median(PH)`)
GUJARAT$`median(PH)`[GUJARAT$`median(PH)`==max(GUJARAT$`median(PH)`)]<-median(GUJARAT$`median(PH)`)

phplot <- ggplot(NULL, aes(year, `median(PH)`, group = 1)) +
  geom_line(data=KERALA, col = "orange") +
  geom_line(data=MAHARASHTRA, col = "green") +
  #geom_line(data=GOA, col = "blue") +
  geom_line(data=PUNJAB, col = "darkcyan") +
  geom_line(data=TAMILNADU, col = "purple") +
  geom_line(data=GUJARAT, col = "red")
phplot


#STATE VS WATER CONDUCTIVITY
GOA$`median(conductivity)`[GOA$`median(conductivity)`==max(GOA$`median(conductivity)`)]<-median(GOA$`median(conductivity)`)
GUJARAT$`median(conductivity)`[GUJARAT$`median(conductivity)`==max(GUJARAT$`median(conductivity)`)]<-median(GUJARAT$`median(conductivity)`)
GUJARAT$`median(conductivity)`[GUJARAT$`median(conductivity)`==max(GUJARAT$`median(conductivity)`)]<-median(GUJARAT$`median(conductivity)`)

wcplot <- ggplot(NULL, aes(year, `median(conductivity)`, group = 1)) +
  geom_line(data=KERALA, col = "orange") +
  geom_line(data=MAHARASHTRA, col = "green") +
  geom_line(data=GOA, col = "blue") +
  geom_line(data=PUNJAB, col = "darkcyan") +
  geom_line(data=TAMILNADU, col = "purple") +
  geom_line(data=GUJARAT, col = "red")
wcplot


#STATE VS BIOLOGICAL OXYGEN DEMAND
bodplot <- ggplot(NULL, aes(year, `median(BOD)`, group = 1)) +
  geom_line(data=KERALA, col = "orange") +
  geom_line(data=MAHARASHTRA, col = "green") +
  geom_line(data=GOA, col = "blue") +
  geom_line(data=PUNJAB, col = "darkcyan") +
  geom_line(data=TAMILNADU, col = "purple") +
  geom_line(data=GUJARAT, col = "red")
bodplot


#STATE VS NITRITES N NITRATES
nnplot <- ggplot(NULL, aes(year, `median(nitrites_n_nitrates)`, group = 1)) +
  geom_line(data=KERALA, col = "orange") +
  geom_line(data=MAHARASHTRA, col = "green") +
  geom_line(data=GOA, col = "blue") +
  geom_line(data=PUNJAB, col = "darkcyan") +
  geom_line(data=TAMILNADU, col = "purple") +
  geom_line(data=GUJARAT, col = "red")
nnplot


#STATE VS FECAL COLIFORM
PUNJAB$`median(FECAL_COLIFORM)`[PUNJAB$`median(FECAL_COLIFORM)`==max(PUNJAB$`median(FECAL_COLIFORM)`)]<-median(PUNJAB$`median(FECAL_COLIFORM)`)
PUNJAB$`median(FECAL_COLIFORM)`[PUNJAB$`median(FECAL_COLIFORM)`==max(PUNJAB$`median(FECAL_COLIFORM)`)]<-median(PUNJAB$`median(FECAL_COLIFORM)`)
PUNJAB$`median(FECAL_COLIFORM)`[PUNJAB$`median(FECAL_COLIFORM)`==max(PUNJAB$`median(FECAL_COLIFORM)`)]<-median(PUNJAB$`median(FECAL_COLIFORM)`)

fcplot <- ggplot(NULL, aes(year, `median(FECAL_COLIFORM)`, group = 1)) +
  geom_line(data=KERALA, col = "orange") +
  geom_line(data=MAHARASHTRA, col = "green") +
  geom_line(data=GOA, col = "blue") +
  geom_line(data=PUNJAB, col = "darkcyan") +
  geom_line(data=TAMILNADU, col = "purple") +
  geom_line(data=GUJARAT, col = "red")
fcplot


#STATE VS TOTAL COLIFORM
PUNJAB$`median(TOTAL_COLIFORM)`[PUNJAB$`median(TOTAL_COLIFORM)`==max(PUNJAB$`median(TOTAL_COLIFORM)`)]<-median(PUNJAB$`median(TOTAL_COLIFORM)`)
PUNJAB$`median(TOTAL_COLIFORM)`[PUNJAB$`median(TOTAL_COLIFORM)`==max(PUNJAB$`median(TOTAL_COLIFORM)`)]<-median(PUNJAB$`median(TOTAL_COLIFORM)`)

tcplot <- ggplot(NULL, aes(year, `median(TOTAL_COLIFORM)`, group = 1)) +
  geom_line(data=KERALA, col = "orange") +
  geom_line(data=MAHARASHTRA, col = "green") +
  geom_line(data=GOA, col = "blue") +
  geom_line(data=PUNJAB, col = "darkcyan") +
  geom_line(data=TAMILNADU, col = "purple") +
  geom_line(data=GUJARAT, col = "red")
tcplot

########################################################
########################################################
################# <-TILL HERE ##########################
#Modelling

statelist = c("KERALA", "MAHARASHTRA", "GOA", "PUNJAB", "TAMILNADU", "GUJARAT")
templist = list(KERALA$`median(temp)`, MAHARASHTRA$`median(temp)`, GOA$`median(temp)`, PUNJAB$`median(temp)`, TAMILNADU$`median(temp)`, GUJARAT$`median(temp)`)

j=1
for (i in templist) {
      timeseries=ts(data=i, start = c(2005,1), frequency = 1)
      tempml <-tslm(timeseries ~ trend)
      plot(forecast(tempml ,level=c(95), h=4), main = paste("FORECAST TEMP", statelist[j]))
      (print(statelist[j]))
      (print(forecast(tempml ,level=c(95), h=4)))
j=j+1
}

dolist = list(KERALA$`median(DO)`, MAHARASHTRA$`median(DO)`, GOA$`median(DO)`, PUNJAB$`median(DO)`, TAMILNADU$`median(DO)`, GUJARAT$`median(DO)`)

j=1
for (i in dolist) {
  timeseries=ts(data=i, start = c(2005,1), frequency = 1)
  doml <-tslm(timeseries ~ trend)
  plot(forecast(doml ,level=c(95), h=4), main = paste("FORECAST DO", statelist[j]))
  (print(statelist[j]))
  (print(forecast(doml ,level=c(95), h=4)))
  j=j+1
}


phlist = list(KERALA$`median(PH)`, MAHARASHTRA$`median(PH)`, GOA$`median(PH)`, PUNJAB$`median(PH)`, TAMILNADU$`median(PH)`, GUJARAT$`median(PH)`)

j=1
for (i in phlist) {
  timeseries=ts(data=i, start = c(2005,1), frequency = 1)
  phml <-tslm(timeseries ~ trend)
  plot(forecast(phml ,level=c(95), h=4), main = paste("FORECAST pH", statelist[j]))
  (print(statelist[j]))
  (print(forecast(phml ,level=c(95), h=4)))
  j=j+1
}


conlist = list(KERALA$`median(conductivity)`, MAHARASHTRA$`median(conductivity)`, GOA$`median(conductivity)`, PUNJAB$`median(conductivity)`, TAMILNADU$`median(conductivity)`, GUJARAT$`median(conductivity)`)

j=1
for (i in conlist) {
  timeseries=ts(data=i, start = c(2005,1), frequency = 1)
  conml <-tslm(timeseries ~ trend)
  plot(forecast(conml ,level=c(95), h=4), main = paste("FORECAST CONDUCTIVITY", statelist[j]))
  (print(statelist[j]))
  (print(forecast(conml ,level=c(95), h=4)))
  j=j+1
}


bodlist = list(KERALA$`median(BOD)`, MAHARASHTRA$`median(BOD)`, GOA$`median(BOD)`, PUNJAB$`median(BOD)`, TAMILNADU$`median(BOD)`, GUJARAT$`median(BOD)`)

j=1
for (i in bodlist) {
  timeseries=ts(data=i, start = c(2005,1), frequency = 1)
  bodml <-tslm(timeseries ~ trend)
  plot(forecast(bodml ,level=c(95), h=4), main = paste("FORECAST BOD", statelist[j]))
  (print(statelist[j]))
  (print(forecast(bodml ,level=c(95), h=4)))
  j=j+1
}


nnnlist = list(KERALA$`median(nitrites_n_nitrates)`, MAHARASHTRA$`median(nitrites_n_nitrates)`, GOA$`median(nitrites_n_nitrates)`, PUNJAB$`median(nitrites_n_nitrates)`, TAMILNADU$`median(nitrites_n_nitrates)`, GUJARAT$`median(nitrites_n_nitrates)`)

j=1
for (i in nnnlist) {
  timeseries=ts(data=i, start = c(2005,1), frequency = 1)
  nnnml <-tslm(timeseries ~ trend)
  plot(forecast(nnnml ,level=c(95), h=4), main = paste("FORECAST NITRITES_N_NITRATES", statelist[j]))
  (print(statelist[j]))
  (print(forecast(nnnml ,level=c(95), h=4)))
  j=j+1
}


fcollist = list(KERALA$`median(FECAL_COLIFORM)`, MAHARASHTRA$`median(FECAL_COLIFORM)`, GOA$`median(FECAL_COLIFORM)`, PUNJAB$`median(FECAL_COLIFORM)`, TAMILNADU$`median(FECAL_COLIFORM)`, GUJARAT$`median(FECAL_COLIFORM)`)

j=1
for (i in fcollist) {
  timeseries=ts(data=i, start = c(2005,1), frequency = 1)
  fcolml <-tslm(timeseries ~ trend)
  plot(forecast(fcolml ,level=c(95), h=4), main = paste("FORECAST FECAL_COLIFORM", statelist[j]))
  (print(statelist[j]))
  (print(forecast(fcolml ,level=c(95), h=4)))
  j=j+1
}


tcollist = list(KERALA$`median(TOTAL_COLIFORM)`, MAHARASHTRA$`median(TOTAL_COLIFORM)`, GOA$`median(TOTAL_COLIFORM)`, PUNJAB$`median(TOTAL_COLIFORM)`, TAMILNADU$`median(TOTAL_COLIFORM)`, GUJARAT$`median(TOTAL_COLIFORM)`)

j=1
for (i in tcollist) {
  timeseries=ts(data=i, start = c(2005,1), frequency = 1)
  tcolml <-tslm(timeseries ~ trend)
  plot(forecast(tcolml ,level=c(95), h=4), main = paste("FORECAST TOTAL_COLIFORM", statelist[j]))
  (print(statelist[j]))
  (print(forecast(tcolml ,level=c(95), h=4)))
  j=j+1
}
