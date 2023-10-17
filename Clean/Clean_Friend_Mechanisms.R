#Name: Clean_Friend_Mechanisms.R
#Functions: robustness checks that require us to change the data from cleaning 
#           process
#Authors: Yongyin Liang (summarize) ; Keyu Chen ; Xiaotong Cui ; Yinzhong Shuai



################################################################################
###############                  Set Up                         ################
################################################################################

rm(list = ls())

# library manager
library(pacman)

# packages to use load them now using the pacman "manager"
p_load(tidyverse, iotools, data.table, haven, knitr, psych, stats4, stargazer, magrittr, 
       qwraps2, Jmisc, fastDummies, corrplot, csv, skedastic, lmtest, broom, usmap,
       car, sandwich, vtable, readx, disprose, mmr, doBy, lubridate, dplr, reshape,
       ggplot2, ggridges, plotly, openxlsx, dplyr, readr, zoo, geosphere)

# set working directory
setwd("~/Social Network and Vaccination")

kStatesAbb <- c("AK", "AL", "AR", "AZ", "CA", "CO", "CT", "DC", "DE", "FL", 
                "GA", "HI", "IA", "ID", "IL", "IN", "KS", "KY", "LA", "MA", 
                "MD", "ME", "MI", "MN", "MO", "MS", "MT", "NC", "ND", "NE", 
                "NH", "NJ", "NM", "NV", "NY", "OH", "OK", "OR", "PA", "RI", 
                "SC", "SD", "TN", "TX", "UT", "VA", "VT", "WA", "WI", "WV", "WY")

# functions

WideToLong <- function(df.wide){
  
  ## Vaccination Status 
  colname.vax.weekn <- append(c("user_id"), paste0("vax_week", 1:33))
  df.vax <- df.wide[colname.vax.weekn]
  
  df.vax <- melt(df.vax, id.vars = "user_id")
  df.vax[, 2] <- sub("^vax_week", "", df.vax[, 2])
  
  names(df.vax)[2] <- 'week'
  names(df.vax)[3] <- 'vax'
  
  ## Friend Vaccination Status
  colname.friendvax.weekn <- append(c("user_id"), paste0("friendvax_week", 7:33))
  df.friend <- df.wide[colname.friendvax.weekn]
  
  df.friend <- melt(df.friend, id.vars = "user_id")
  df.friend[, 2] <- sub("^friendvax_week", "", df.friend[, 2])
  
  names(df.friend)[2] <- 'week'
  names(df.friend)[3] <- 'friendvax'
  
  ## Merge
  colname.deplete <- append(colname.vax.weekn[-1], colname.friendvax.weekn[-1])
  df.othervar <- df.wide[ , ! names(df.wide) %in% colname.deplete]
  
  df.long <- merge(df.friend, df.vax, all.y=TRUE)
  df.long <- merge(df.long, df.othervar, all=TRUE)
  
  df.long$week <- as.numeric(df.long$week)
  # Free up Memory
  rm(df.friend, df.vax, df.wide)
  gc()
  
  return(df.long)
}

MultiplyNumVax <- function(df.friendnum, df.vaxsupply){
  
  # transform friend numbers into states' weight
  df.friendw <- data.frame('user_id' = df.friendnum$user_id)
  
  df.friendw$friendnum <- df.friendnum$count
  df.friendw$friendnum_US <- df.friendnum$nation_USA
  
  # add the weights of USA friends
  df.friendw[, "friendw_USA"] <- df.friendnum[, "nation_USA"]/df.friendnum[, "count"]
  
  # add the friend weights of each state
  for (i in 1:51){ 
    statename <- kStatesAbb[i]
    df.friendw[, paste0("friendw_", statename)] <- 
      df.friendnum[, paste0("friend_", statename)]/df.friendnum[, "count"] # divide by total friend number
  }
  
  # Create a column of week t by weighting the vaccine supply for each user
  df.weights <- subset(df.friendw, select = -c(user_id,friendnum,friendnum_US,friendw_USA))
  df.vaxsupply <- subset(df.vaxsupply, select = -c(Location))
  
  # matrix multiplication
  df.friendvax <- as.data.frame( as.matrix(df.weights) %*% as.matrix(df.vaxsupply) )
  
  # bind variables together
  df.friendvax <- cbind(df.friendvax,user_id = df.friendw$user_id, 
                        friendnum = df.friendw$friendnum,
                        friendnum_US = df.friendw$friendnum_US)
  
  # adjust variables, take user_id to front
  df.friendvax <- df.friendvax %>% relocate(user_id, friendnum, friendnum_US)
  
  # change column names
  new_name <- c('user_id','friendnum','friendnum_US')
  for (i in 7:33){
    name <- paste0("friendvax_week",i)
    new_name <- c(new_name,name)
  }
  
  colnames(df.friendvax) <- new_name
  
  return(df.friendvax)
  
}

################################################################################
###############         Mechanism: 1. Mutual Friendships        ################
################################################################################

# import data set
followerlist <- fread("Data/Twitter/followers_info.csv", integer64="character", 
                      fill = TRUE)
followinglist <- fread("Data/Twitter/users_socialnetwork.csv", integer64="character", 
                       fill = TRUE)

### Get the sub-network dataframe

# rename columns
names(followerlist)[names(followerlist) == 'followers_identryId'] <- 'friend_id'
names(followinglist)[names(followinglist) == 'user_id_following'] <- 'friend_id'
# merge and return two-sided friends
twosidef <- merge(followinglist, followerlist, by = c("user_id", "friend_id"), 
                  all.x = FALSE)
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
  statename <- kStatesAbb[i]
  df[, paste0("friend_", statename)] <- ifelse(df$state == kStatesAbb[i], 1, 0)
  end_time <- Sys.time()
  print(i)
  print(end_time - start_time)
}

#deplete columns
df <- subset(df, select = -c(user_id_following, nation, admin1, state, admin3))

# collapse by user, summarize means
df.collapse <- df %>% group_by(user_id) %>% summarise_all(sum)

saveRDS(df.collapse, file = "Data/Temp/mutualnetwork.rds")

# description
hist(df.collapse$count)


### Multiply with state vaccine data

# read data sets
df.friendnum <- readRDS("Data/Temp/mutualnetwork.rds")
df.vaxsupply <- readRDS("Data/Temp/df.1diff.distribution_state.rds")

df.friendvax.mu <- MultiplyNumVax(df.friendnum, df.vaxsupply)

#### Merge data (Clean_merge.R)

# read data sets
df.uservax <- readRDS("Data/Temp/users_vaxtime.rds")
df.friendvax.mu <- readRDS("Data/Temp/mutual-friendvax.rds")
df.friendvax_vaxrate <- readRDS("Data/Temp/friendvax_vaxrate_state.rds")
df.friendlocation <- readRDS("Data/Temp/friendweights_state.rds")
df.demo <- readRDS("Data/Temp/users_demography.rds")

# merge using usernames (supply cumulative)
df.state.mu <- merge(df.uservax, df.friendvax.mu, all.x = TRUE)
df.state.mu <- merge(df.state.mu, df.friendlocation, all.x = TRUE)
df.state.mu <- merge(df.state.mu, df.demo, all.x = TRUE)

df.state.mu <- df.state.mu %>% relocate(user_id, gender, age, race)

# wide to long
df.state.mu.long <- WideToLong(df.state.mu)

saveRDS(df.state.mu, file = 'Data/Final Data/mutual-friend-state-sample.rds')
saveRDS(df.state.mu.long, file = 'Data/Final Data/mutual-friend-state-sample_long.rds')






################################################################################
###############           Mechanism: 2. Influencers             ################
################################################################################

# read the data
df.socialnetwork <- fread("Data/Twitter/users_socialnetwork.csv", 
                          integer64 = "character", fill = TRUE)
df.fannum <- fread("Data/Twitter/followings_info.csv", 
                   integer64 = "character", fill = TRUE)

# identify celebrities (10k more fans)
df.fannum$celebrity <- ifelse(df.fannum$followers_count >= 30000, 1, 0)
summary(df.fannum$celebrity) # 3.73% following accounts are celebrities

# merge
df.all <- merge(df.socialnetwork, df.fannum, by= "user_id_following")

# celebrity following
df.cele <- subset(df.all, celebrity == 1,
                  select = c(user_id, nation, admin1Abb), sort(user_id))
saveRDS(df.cele, file = "Data/Temp/all_celebrity.rds")

# non-celebrity following
df.nocel <- subset(df.all, celebrity == 0,
                   select = c(user_id, nation, admin1Abb), sort(user_id))
saveRDS(df.nocel, file = "Data/Temp/no_celebrity.rds")



### Summarize the social network for each ----

# Celebrity
df.cele$count <- 1 #count the total number 
df.cele$nation_USA <- ifelse(df.cele$nation == "USA", 1, 0) #dummy for USA
for (i in 1:51){
  statename <- kStatesAbb[i]
  df.cele[,paste0("friend_",statename)] <- ifelse(df.cele$admin1Abb == kStatesAbb[i],1,0)
  print(i)
}
# deplete columns and collapse
df.cele <- subset(df.cele, select = -c(nation, admin1Abb))
df.collapse.cele <- df.cele %>% group_by(user_id) %>% summarise_all(sum)
# save to rds
saveRDS(df.collapse.cele, file = "Data/Temp/socialnetwork_state_cele.rds")

#Non-Celebrity
df.nocel$count <- 1 #count the total number 
df.nocel$nation_USA <- ifelse(df.nocel$nation == "USA", 1, 0) #dummy for USA
for (i in 1:51){
  statename <- kStatesAbb[i]
  df.nocel[,paste0("friend_",statename)] <- ifelse(df.nocel$admin1Abb == kStatesAbb[i],1,0)
  print(i)
}
# deplete columns and collapse
df.nocel <- subset(df.nocel, select = -c(nation, admin1Abb))
df.collapse.nocel <- df.nocel %>% group_by(user_id) %>% summarise_all(sum)
# save to rds
saveRDS(df.collapse.nocel, file = "Data/Temp/socialnetwork_state_nocel.rds")

### Multiply friend weights with vaccine supply

# read data sets
df.vaxsupply <- readRDS("Data/Temp/df.1diff.distribution_state.rds")

df.friendvax.cele <- MultiplyNumVax(df.collapse.cele, df.vaxsupply)
df.friendvax.nocel <- MultiplyNumVax(df.collapse.nocel, df.vaxsupply)

#### Merge data (Clean_merge.R)

# read data sets
df.uservax <- readRDS("Data/Temp/users_vaxtime.rds")
df.friendlocation <- readRDS("Data/Temp/friendweights_state.rds")
df.demo <- readRDS("Data/Temp/users_demography.rds")

# merge using usernames
df.state.cele <- merge(df.uservax, df.friendvax.cele, all.x = TRUE)
df.state.cele <- merge(df.state.cele, df.friendlocation, all.x = TRUE)
df.state.cele <- merge(df.state.cele, df.demo, all.x = TRUE)
df.state.cele <- df.state.cele %>% relocate(user_id, gender, age, race)

df.state.nocel <- merge(df.uservax, df.friendvax.nocel, all.x = TRUE)
df.state.nocel <- merge(df.state.nocel, df.friendlocation, all.x = TRUE)
df.state.nocel <- merge(df.state.nocel, df.demo, all.x = TRUE)
df.state.nocel <- df.state.nocel %>% relocate(user_id, gender, age, race)

# wide to long
df.state.cele.long <- WideToLong(df.state.cele)
df.state.nocel.long <- WideToLong(df.state.nocel)

saveRDS(df.state.cele, file = 'Data/Final Data/celebrity-friend-state-sample.rds')
saveRDS(df.state.cele.long, file = 'Data/Final Data/celebrity-friend-state-sample_long.rds')
saveRDS(df.state.nocel, file = 'Data/Final Data/non-celebrity-friend-state-sample.rds')
saveRDS(df.state.nocel.long, file = 'Data/Final Data/non-celebrity-friend-state-sample_long.rds')





################################################################################
###############            Mechanism: 3. Distances              ################
################################################################################

### data of longitudes and latitudes to each state
df.dist <- fread("Data/Region/states_distance.csv", 
                 integer64 = "character", fill = TRUE)
colnames(df.dist) <- c("admin1", "lat", "long")
df.dist.following <- df.dist
colnames(df.dist.following) <- c("admin1_following", "lat_following", "long_following")

### Decide a cutoff point of distance
# create a data frame of state pairs
departure <- df.dist$admin1
df.name <- as.data.frame(departure)
df.pair <- data.frame()
for (name in departure){
  df.name$destination <- name
  df.pair <- rbind(df.pair, df.name)
}
# assign distance to state pair
df.pair <- left_join(df.pair, df.dist, by = c('departure'='admin1'))
df.pair <- rename(df.pair, c("lat_dep" = "lat", "long_dep" = "long"))
df.pair <- left_join(df.pair, df.dist, by = c('destination'='admin1'))
df.pair <- rename(df.pair, c("lat_des" = "lat", "long_des" = "long"))
# assign distance
df.pair <- df.pair %>% 
  mutate(
    dist = geosphere::distHaversine(cbind(df.pair$long_dep, df.pair$lat_dep), 
                                    cbind(df.pair$long_des, df.pair$lat_des)))
describe(df.pair$dist) # median = 1588804


### assign distance to the social network data and subset the sample
# read the data
df.socialnetwork <- fread("Data/Twitter/users_socialnetwork.csv", 
                          integer64 = "character", fill = TRUE)
df.uservax <- readRDS("Data/Temp/users_vaxtime.rds")
# merge with user's state
df.socialnetwork <- rename(df.socialnetwork, admin1_following = admin1)
df.socialnetwork <- merge(df.socialnetwork, df.uservax[,c("user_id", "admin1")], all.x = TRUE)
# assign distance
df.socialnetwork <- left_join(df.socialnetwork, df.pair[,c("departure", "destination", "dist")],
                              by = c("admin1"="departure", "admin1_following"="destination"))

### subset sample and process the final table
# far following
df.far <- subset(df.socialnetwork, dist >= 1000000, select = c(user_id, nation, admin1Abb), sort(user_id))
# close following
df.close <- subset(df.socialnetwork, dist < 1000000, select = c(user_id, nation, admin1Abb), sort(user_id))

### Summarize the social network for each

# Far friend
df.far$count <- 1 #count the total number 
df.far$nation_USA <- ifelse(df.far$nation == "USA", 1, 0) #dummy for USA
for (i in 1:51){
  statename <- kStatesAbb[i]
  df.far[,paste0("friend_",statename)] <- ifelse(df.far$admin1Abb == kStatesAbb[i],1,0)
  print(i)
}
# deplete columns and collapse
df.far <- subset(df.far, select = -c(nation, admin1Abb))
df.collapse.far <- df.far %>% group_by(user_id) %>% summarise_all(sum)
# save to rds
saveRDS(df.collapse.far, file = "Data/Temp/socialnetwork_state_far.rds")

# Close friend
df.close$count <- 1 #count the total number 
df.close$nation_USA <- ifelse(df.close$nation == "USA", 1, 0) #dummy for USA
for (i in 1:51){
  statename <- kStatesAbb[i]
  df.close[,paste0("friend_",statename)] <- ifelse(df.close$admin1Abb == kStatesAbb[i],1,0)
  print(i)
}
# deplete columns and collapse
df.close <- subset(df.close, select = -c(nation, admin1Abb))
df.collapse.close <- df.close %>% group_by(user_id) %>% summarise_all(sum)
# save to rds
saveRDS(df.collapse.close, file = "Data/Temp/socialnetwork_state_close.rds")


### Multiply friend weights with vaccine supply

# read data sets
df.vaxsupply <- readRDS("Data/Temp/df.1diff.distribution_state.rds")
# multiply number by weight
df.friendvax.far <- MultiplyNumVax(df.collapse.far, df.vaxsupply)
df.friendvax.close <- MultiplyNumVax(df.collapse.close, df.vaxsupply)

#### Merge data (Clean_merge.R)

# read data sets
df.uservax <- readRDS("Data/Temp/users_vaxtime.rds")
df.demo <- readRDS("Data/Temp/users_demography.rds")
df.friendnum <- readRDS('Data/Final Data/wide table_N=92356_v0525.rds')[,c("user_id", "friendnum")]

# merge using usernames
df.state.far <- merge(df.uservax, df.friendvax.far, all.x = TRUE)
df.state.far <- merge(df.state.far, df.demo, all.x = TRUE)
df.state.far <- subset(df.state.far, select = -c(friendnum))
df.state.far <- merge(df.state.far, df.friendnum, all.x = TRUE)
df.state.far <- df.state.far %>% relocate(user_id, gender, age, race)

df.state.close <- merge(df.uservax, df.friendvax.close, all.x = TRUE)
df.state.close <- merge(df.state.close, df.demo, all.x = TRUE)
df.state.close <- subset(df.state.close, select = -c(friendnum))
df.state.close <- merge(df.state.close, df.friendnum, all.x = TRUE)
df.state.close <- df.state.close %>% relocate(user_id, gender, age, race)

# wide to long
df.state.far.long <- WideToLong(df.state.far)
df.state.close.long <- WideToLong(df.state.close)

saveRDS(df.state.far, file = 'Data/Final Data/far-friend-state-sample.rds')
saveRDS(df.state.far.long, file = 'Data/Final Data/far-friend-state-sample_long.rds')
saveRDS(df.state.close, file = 'Data/Final Data/close-friend-state-sample.rds')
saveRDS(df.state.close.long, file = 'Data/Final Data/close-friend-state-sample_long.rds')


