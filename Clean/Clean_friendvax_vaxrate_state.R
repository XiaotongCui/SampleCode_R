#Clean_friendvax_vaxrate_state.R
#Yinzhong Shuai 2022.10.11

### Setup ----

#library manager
library(pacman)

# packages to use load them now using the pacman "manager"
p_load(tidyverse, iotools, data.table, haven, knitr, psych, stats4, stargazer, 
       magrittr, qwraps2, Jmisc, fastDummies, corrplot, csv, skedastic, lmtest,
       mmr)

# set working directory
setwd("~/Social Network and Vaccination")

# read data sets
df_friendnum <- readRDS("Data/Temp/socialnetwork_state.rds")
df_vaxrate <- readRDS("Data/Temp/df_vaxrate_state.rds")

### Clean datasets format and values ----

## clean the form of df_vaxrate
rownames(df_vaxrate) <- df_vaxrate$Location
#select states that have weights information
statesAbb <- c("AK", "AL", "AR", "AZ", "CA", "CO", "CT", "DC", "DE", "FL", 
               "GA", "HI", "IA", "ID", "IL", "IN", "KS", "KY", "LA", "MA", 
               "MD", "ME", "MI", "MN", "MO", "MS", "MT", "NC", "ND", "NE", 
               "NH", "NJ", "NM", "NV", "NY", "OH", "OK", "OR", "PA", "RI", 
               "SC", "SD", "TN", "TX", "UT", "VA", "VT", "WA", "WI", "WV", "WY")
df_vaxrate <- df_vaxrate[df_vaxrate$Location %in% statesAbb]
# sort by states' names abbreviation
df_vaxrate <- df_vaxrate[order(df_vaxrate$Location),]

## transform friend numbers into states' weight
df_friendweights <- data.frame('user_id'=df_friendnum$user_id)

# add the weights of USA friends
df_friendweights[,"friendw_USA"] <- df_friendnum[,"nation_USA"]/df_friendnum[,"count"]

# add the friend weights of each state
for (i in 1:51){ 
  statename <- statesAbb[i]
  k = i+3 # the states variables start from the third column
  df_friendweights[,paste0("friendw_",statename)] <- 
    df_friendnum[,k]/df_friendnum[,"nation_USA"] # divide by total friend number
}

# saveRDS(df_friendweights,file = 'Data/Temp/friendweights_state.rds')


## Create a column of week t by weighting the vaccine supply for each user
df_weights <- subset(df_friendweights, select = -c(user_id,friendw_USA))
df_vaxrate <- subset(df_vaxrate, select = -c(Location))

# matrix multiplication
df_friendvax <- as.data.frame(as.matrix(df_weights) %*% as.matrix(df_vaxrate))
df_friendvax <- cbind(df_friendvax,user_id = df_friendweights$user_id)

# adjust variables
df_friendvax <- df_friendvax %>% relocate(user_id)

# change column names
new_name <- c('user_id')
for (i in 5:33){
  name <- paste0("friendvax_week",i)
  new_name <- c(new_name,name)
}

colnames(df_friendvax) <- new_name

saveRDS(df_friendvax,file = 'Data/Temp/friendvax_vaxrate_state.rds')



