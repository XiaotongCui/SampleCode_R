# Clean_two-sided-friend.R
# This program clean out the panel with two-sided friends (follower & following)
# Yongyin Liang 2023.1.3

### Setup ----

# library manager
library(pacman)

# packages to use load them now using the pacman "manager"
p_load(tidyverse, iotools, data.table, haven, knitr, psych, stats4, stargazer, magrittr, 
       qwraps2, Jmisc, fastDummies, corrplot, csv, skedastic, lmtest, broom, usmap,
       car, sandwich, vtable, readxl, data.table, disprose)

statesAbb <- c("AK", "AL", "AR", "AZ", "CA", "CO", "CT", "DC", "DE", "FL", 
               "GA", "HI", "IA", "ID", "IL", "IN", "KS", "KY", "LA", "MA", 
               "MD", "ME", "MI", "MN", "MO", "MS", "MT", "NC", "ND", "NE", 
               "NH", "NJ", "NM", "NV", "NY", "OH", "OK", "OR", "PA", "RI", 
               "SC", "SD", "TN", "TX", "UT", "VA", "VT", "WA", "WI", "WV", "WY")

# working directory
setwd("~/Social Network and Vaccination")

# import data set
followerlist <- fread("Data/Twitter/followers_info.csv", integer64="character", fill = TRUE)
followinglist <- fread("Data/Twitter/users_socialnetwork.csv", integer64="character", fill = TRUE)


### Get the sub-network dataframe ----

# rename columns
names(followerlist)[names(followerlist) == 'followers_identryId'] <- 'friend_id'
names(followinglist)[names(followinglist) == 'user_id_following'] <- 'friend_id'

# merge and return two-sided friends
twosidef <- merge(followinglist, followerlist, by = c("user_id", "friend_id"), all.x = FALSE)

n_distinct(twosidef$user_id) #76607 users have mutual friends





### Clean the network data (Clean_socialnetwork.R) ----

df <- select(twosidef,"user_id", "friend_id", "verified.x", "nation", "admin1", 
             "admin1Abb", "admin3")
colnames(df) <- c('user_id', 'user_id_following', 'verified', 'nation', 'admin1', 
                  'state', 'admin3')



### Generate summary variables

df$count <- 1 #count the total number 
#df <- dummy_cols(df,select_columns = 'nation') #dummies for all nation
df$nation_USA <- ifelse(df$nation == "USA", 1, 0) #dummy for USA

for (i in 1:51){
  start_time <- Sys.time()
  statename <- statesAbb[i]
  df[,paste0("friend_", statename)] <- ifelse(df$state == statesAbb[i], 1, 0)
  end_time <- Sys.time()
  print(i)
  print(end_time - start_time)
}

#deplete columns
df <- subset(df, select = -c(user_id_following, nation, admin1, state, admin3))


### Collapse Dataset and Save

# collapse by user, summarize means
df_collapse <- df %>% group_by(user_id) %>% summarise_all(sum)

#save to rds (smaller file)
path <-  paste0("Data/Temp/mutualnetwork.rds")
saveRDS(df_collapse,file = path)



### Multiply with state vaccine data

# read data sets
df_friendnum <- readRDS("Data/Temp/mutualnetwork.rds")
df_vaxsupply <- readRDS("Data/Temp/df.1diff.distribution_state.rds")

### Clean datasets format and values ----

## transform friend numbers into states' weight
df_friendweights <- data.frame('user_id'=df_friendnum$user_id)

df_friendweights$friendnum <- df_friendnum$count
df_friendweights$friendnum_US <- df_friendnum$nation_USA

# add the weights of USA friends
df_friendweights[,"friendw_USA"] <- df_friendnum[,3]/df_friendnum[,2]

# add the friend weights of each state
for (i in 1:51){ 
  statename <- statesAbb[i]
  k = i+3 # the states variables start from the third column
  df_friendweights[,paste0("friendw_",statename)] <- 
    df_friendnum[,k]/df_friendnum[,3] # divide by total friend number
}

saveRDS(df_friendweights,file = 'Data/Temp/friendweights_state.rds')


## Create a column of week t by weighting the vaccine supply for each user
df_weights <- subset(df_friendweights, select = -c(user_id,friendnum,friendnum_US,friendw_USA))
df_vaxsupply <- subset(df_vaxsupply, select = -c(Location))

# matrix multiplication
df_friendvax <- as.data.frame( as.matrix(df_weights) %*% as.matrix(df_vaxsupply) )

# bind variables together
df_friendvax <- cbind(df_friendvax,user_id = df_friendweights$user_id, 
                      friendnum = df_friendweights$friendnum,
                      friendnum_US = df_friendweights$friendnum_US)

# adjust variables, take user_id to front
df_friendvax <- df_friendvax %>% relocate(user_id, friendnum, friendnum_US)

# change column names
new_name <- c('user_id','friendnum','friendnum_US')
for (i in 7:33){
  name <- paste0("friendvax_week",i)
  new_name <- c(new_name,name)
}

colnames(df_friendvax) <- new_name

saveRDS(df_friendvax, file = 'Data/Temp/mutual-friendvax.rds')



#### Merge data (Clean_merge.R)

# read data sets
df_uservax <- readRDS("Data/Temp/users_vaxtime.rds")
df_friendvax_mu <- readRDS("Data/Temp/mutual-friendvax.rds")
df_friendvax_vaxrate <- readRDS("Data/Temp/friendvax_vaxrate_state.rds")
df_friendlocation <- readRDS("Data/Temp/friendweights_state.rds")
df_demo <- readRDS("Data/Temp/users_demography.rds")

# merge using usernames (supply cumulative)
df_state_mu <- merge(df_uservax, df_friendvax_mu, all.x = TRUE)
df_state_mu <- merge(df_state_mu, df_friendlocation, all.x = TRUE)
df_state_mu <- merge(df_state_mu, df_demo, all.x = TRUE)

df_state_mu <- df_state_mu %>% relocate(user_id, gender, age, race)

# save
saveRDS(df_state_mu, file = 'Data/Final Data/mutual-friend-state-sample.rds')



