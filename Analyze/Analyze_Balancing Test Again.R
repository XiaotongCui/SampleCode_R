#Balancing Test Again
#Xiaotong Cui - 2023.04.30
#Xiaotong Cui - 2023.05.26

'任务梳理:
1. 总结各州的平均年龄，种族，性别
2. 总结数据样本中
2. 根据统计量做t-test Chi-sqr-test等
'
# library manager
library(pacman)

# packages to use load them now using the pacman "manager"
p_load(tidyverse, iotools, data.table, haven, knitr, psych, stats4, stargazer, magrittr, 
       qwraps2, Jmisc, fastDummies, corrplot, csv, skedastic, lmtest, broom, usmap,
       car, sandwich, vtable, readx, disprose, mmr, doBy, lubridate, dplr, reshape,
       ggplot2, ggridges, plotly, openxlsx, dplyr, readr,grepl)

# set working directory
setwd("~/Social Network and Vaccination")

kStatesAbb <- c("Alabama", "Alaska", "Arizona", "Arkansas", "California", 
                "Colorado", "Connecticut", "Delaware", "District of Columbia", 
                "Florida", "Georgia", "Hawaii", "Idaho", "Illinois", "Indiana", 
                "Iowa", "Kansas", "Kentucky", "Louisiana", "Maine", "Maryland", 
                "Massachusetts", "Michigan", "Minnesota", "Mississippi", "Missouri", 
                "Montana", "Nebraska", "Nevada", "New Hampshire", "New Jersey", 
                "New Mexico", "New York", "North Carolina", "North Dakota", "Ohio", 
                "Oklahoma", "Oregon", "Pennsylvania", "Rhode Island", "South Carolina", 
                "South Dakota", "Tennessee", "Texas", "Utah", "Vermont", "Virginia", 
                "Washington", "West Virginia", "Wisconsin", "Wyoming")

#Read the data
df.demography <- read.csv("Data/Demographics/sc-est2019-alldata6.csv")
saveRDS(df.demography, file = 'Data/Demographics/sc-est2019-alldata6.rds')
df.sample <- read_rds("Data/Final Data/wide table_N=92356_v0525.rds")
'
Doing Averages
'
TotalPop <- aggregate(x=df.demography$POPESTIMATE2019, by=list(df.demography$STATE),sum)
POPSUM <- sum(TotalPop$x)
df.sample$sum <- 1
df.sample$sum[is.na(df.sample$age)] <- 0
TotalPopsam <- aggregate(x=df.sample$sum, by=list(df.sample$admin1),sum)
#调整年龄数值
df.sample$age[df.sample$age == 0] <- 9
df.sample$age[df.sample$age == 1] <- 29
df.sample$age[df.sample$age == 2] <- 59
df.sample$age[is.na(df.sample$age)] <- 0
df.sample$race[is.na(df.sample$race)] <- 0
df.sample$gender[is.na(df.sample$gender)] <- 0
df.sample$admin1[is.na(df.sample$admin1)] <- "Alabama"

#Average the age
Wage <- aggregate(x=df.demography$POPESTIMATE2019, by=list(df.demography$STATE,df.demography$AGE),sum)
#Doing Averages
Wage$Multi = Wage$Group.2*Wage$x
AgeAvg <- aggregate(x=Wage$Multi, by=list(Wage$Group.1),sum)
AgeAvg$final <- AgeAvg$x/TotalPop$x
Totalagesam <- aggregate(x=df.sample$age, by=list(df.sample$admin1),sum)
Totalagesam$final <- Totalagesam$x/TotalPopsam$x

#Average the gender
Wsex <- aggregate(x=df.demography$POPESTIMATE2019, by=list(df.demography$STATE,df.demography$SEX),sum)
#Doing Averages
Wsex$Multi = Wsex$Group.2*Wsex$x
SexAvg <- aggregate(x= Wsex$Multi, by=list( Wsex$Group.1),sum)
SexAvg$final <- SexAvg$x/TotalPop$x
Totalsexsam <- aggregate(x=df.sample$gender, by=list(df.sample$admin1),sum)
Totalsexsam$final <- Totalsexsam$x/TotalPopsam$x

#Average the race
Wrace <- aggregate(x=df.demography$POPESTIMATE2019, by=list(df.demography$STATE,df.demography$RACE),sum)
#Doing Averages
Wrace$Multi = Wrace$Group.2*Wrace$x
RaceAvg <- aggregate(x= Wrace$Multi, by=list( Wrace$Group.1),sum)
RaceAvg$final <- RaceAvg$x/TotalPop$x
Totalracesam <- aggregate(x=df.sample$race, by=list(df.sample$admin1),sum)
Totalracesam$final <- Totalracesam$x/TotalPopsam$x

'
Now we do test among states
'
#Calculate Mean of the total population
Totalage <- aggregate(x=df.demography$POPESTIMATE2019, by=list(df.demography$AGE),sum)
#Doing Averages
Totalage$Multi = Totalage$Group.1/POPSUM*Totalage$x
finalage <- sum(Totalage$Multi)

Totalrace <- aggregate(x=df.demography$POPESTIMATE2019, by=list(df.demography$RACE),sum)
#Doing Averages
Totalrace$Multi = Totalrace$Group.1/POPSUM*Totalrace$x
finalrace <- sum(Totalrace$Multi)

Totalgender <- aggregate(x=df.demography$POPESTIMATE2019, by=list(df.demography$SEX),sum)
#Doing Averages
Totalgender$Multi = Totalgender$Group.1/POPSUM*Totalgender$x
finalgender <- sum(Totalgender$Multi)

#Now we do the t-test
#Age
#We calculate the sd of each population
sdage <- numeric(56)
for (i in 1:56){
  for (j in 1:4386){
    if(Wage$Group.1[j]==i){sdage[i] = sdage[i] + Wage$x[j]/(TotalPop$x[which(grepl(i,TotalPop$Group.1))]-1)*(Wage$Group.2[j]-AgeAvg$final[which(grepl(i,AgeAvg$Group.1))])^2}
  }
  sdage[i] =sqrt(sdage[i])
}


#Race
#We calculate the sd of each population
sdrace <- numeric(56)
for (i in 1:56){
  for (j in 1:306){
    if(Wrace$Group.1[j]==i){sdrace[i] = sdrace[i] + Wrace$x[j]/(TotalPop$x[which(grepl(i,TotalPop$Group.1))]-1)*(Wrace$Group.2[j]-RaceAvg$final[which(grepl(i,RaceAvg$Group.1))])^2}
  }
  sdrace[i] =sqrt(sdrace[i])
}

#Sex
#We calculate the sd of each population
sdsex <- numeric(56)
for (i in 1:56){
  for (j in 1:153){
    if(Wsex$Group.1[j]==i){sdsex[i] = sdsex[i] + Wsex$x[j]/(TotalPop$x[which(grepl(i,TotalPop$Group.1))]-1)*(Wsex$Group.2[j]-SexAvg$final[which(grepl(i,SexAvg$Group.1))])^2}
  }
  sdsex[i] =sqrt(sdsex[i])
}

agetvalue <- numeric(56)
for (i in 1:56){
  if(i!= 3 &i!= 7&i!= 14& i!= 43& i!= 52){
    agetvalue[i] = (AgeAvg$final[which(grepl(i,AgeAvg$Group.1))] - finalage)/sdage[i]}
}

agep <- 2*pnorm(agetvalue,mean=finalage,sd=sdage)


racetvalue <- numeric(56)
for (i in 1:56){
  if(i!= 3 &i!= 7&i!= 14& i!= 43& i!= 52){
    racetvalue[i] = (RaceAvg$final[which(grepl(i,RaceAvg$Group.1))] - finalrace)/sdrace[i]}
}

racep <- 2*pnorm(racetvalue,mean=finalage,sd=sdrace)


sextvalue <- numeric(56)
for (i in 1:56){
  if(i!= 3 &i!= 7&i!= 14& i!= 43& i!= 52){
    sextvalue[i] = (SexAvg$final[which(grepl(i,SexAvg$Group.1))] - finalgender)/sdsex[i]}
}

sexp <- 2*pnorm(sextvalue,mean=finalgender,sd=sdsex)


dfbalance <- data.frame(matrix(nrow = 56))
dfbalance$statenum <- 1:56
dfbalance$agetvalue <-  agetvalue
dfbalance$agep <-  agep
dfbalance$sextvalue <-  sextvalue
dfbalance$sexp <-  sexp
dfbalance$racetvalue <-  racetvalue
dfbalance$racep <-  racep

dfbalance <- dfbalance[-c(3),]
dfbalance <- dfbalance[-c(13),]
dfbalance <- dfbalance[-c(41),]
dfbalance <- dfbalance[-c(49),]
dfbalance <- dfbalance[-c(6),]

dfbalance$name <- kStatesAbb
dfbalance <- dfbalance[,-c(1)]
saveRDS(dfbalance, file = 'Data/Demographics/balance.rds')


#做版本2的t-test
#calculate sd for sample
sdracesam <- numeric(51)
sdagesam <- numeric(51)
sdsexsam <- numeric(51)
tagesam <- numeric(51)
tsexsam <- numeric(51)
tracesam <- numeric(51)
agepsam <- numeric(51)
sexpsam <- numeric(51)
racepsam <- numeric(51)
for (i in 1:51){
  print(i)
  for (j in 1:92356){
    if(df.sample$admin1[j]==TotalPopsam$Group.1[i]){
      sdracesam[i] = sdracesam[i] + (df.sample$race[j]-Totalracesam$final[i])^2/(TotalPopsam$x[i]-1)
      sdagesam[i] = sdagesam[i] + (df.sample$age[j]-Totalagesam$final[i])^2/(TotalPopsam$x[i]-1)
      sdsexsam[i] = sdsexsam[i] + (df.sample$gender[j]-Totalsexsam$final[i])^2/(TotalPopsam$x[i]-1)}
  }
  sdsexsam[i] =sqrt(sdsexsam[i])
  sdagesam[i] =sqrt(sdagesam[i])
  sdracesam[i] =sqrt(sdracesam[i])
  #find t-value
  tagesam[i] <- (Totalagesam$final[i] - AgeAvg$final[which(grepl(i,AgeAvg$Group.1))])/sdagesam[i]
  tsexsam[i] <- (Totalsexsam$final[i] - SexAvg$final[which(grepl(i,SexAvg$Group.1))])/sdsexsam[i]
  tracesam[i] <- (Totalracesam$final[i] - RaceAvg$final[which(grepl(i,RaceAvg$Group.1))])/sdracesam[i]
  #find p-value
  agepsam[i] <- 2*pnorm(tagesam[i],mean=AgeAvg$final[which(grepl(i,AgeAvg$Group.1))],sd=sdagesam[i])
  sexpsam[i] <- 2*pnorm(tsexsam[i],mean=SexAvg$final[which(grepl(i,SexAvg$Group.1))],sd=sdsexsam[i])
  racepsam[i] <- 2*pnorm(tracesam[i],mean=RaceAvg$final[which(grepl(i,RaceAvg$Group.1))],sd=sdracesam[i])
}

