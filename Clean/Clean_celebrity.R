#Celebrity related code
#Xiaotong


### Setup ----

# library manager
library(pacman)

# packages to use load them now using the pacman "manager"
p_load(tidyverse, iotools, data.table, haven, knitr, psych, stats4, stargazer, magrittr, 
       qwraps2, Jmisc, fastDummies, corrplot, csv, skedastic, lmtest, broom, usmap,
       car, sandwich, vtable, readxl, data.table, disprose)

# working directory
setwd("~/Social Network and Vaccination")
#导入individual i的好友是谁的大表
data <- readRDS("Data/Temp/socialnetwork_particular.rds")
#导入friend info
datafollow <- read.csv("followings_info.csv")

#目的：筛选谁是celebrity,标准：粉丝数大于等于三万
datafollow$celebrity <- ifelse(datafollow$followers_count>=30000,1,0)

#现在把两个表格merge到一起
datafollow$user_id_following <- as.character(datafollow$user_id_following)
data$user_id_following <- as.character(data$user_id_following)
dataall <- merge(data, datafollow, by= "user_id_following")

#保存只有celebrity的
datacele <- subset(dataall,celebrity == 1, select = c(user_id_following:celebrity),sort(user_id))
datacele <- datacele[order(datacele$user_id),]
path <-  paste0("Data/Twitter/all_celebrity.rds")
saveRDS(datacele,file = path)

#保存没有celebrity的
datanoncel <- subset(dataall,celebrity == 0, select = c(user_id_following:celebrity),sort(user_id))
datacele <- datanoncel[order(datanoncel$user_id),]
path <-  paste0("Data/Twitter/no_celebrity.rds")
saveRDS(datanoncel,file = path)

#整理出每个state朋友含量的大表

#只有cele
# read the data (by chunk)
df <- readRDS("Data/Twitter/all_celebrity.rds")


### Generate summary variables ----

df$count <- 1 #count the total number 
#df <- dummy_cols(df,select_columns = 'nation') #dummies for all nation
df$nation_USA <- ifelse(df$nation == "USA",1,0) #dummy for USA


# statesAbb <- sort(unique(df$state)) #get a state list (include DC)
statesAbb <- c("AK", "AL", "AR", "AZ", "CA", "CO", "CT", "DC", "DE", "FL", 
               "GA", "HI", "IA", "ID", "IL", "IN", "KS", "KY", "LA", "MA", 
               "MD", "ME", "MI", "MN", "MO", "MS", "MT", "NC", "ND", "NE", 
               "NH", "NJ", "NM", "NV", "NY", "OH", "OK", "OR", "PA", "RI", 
               "SC", "SD", "TN", "TX", "UT", "VA", "VT", "WA", "WI", "WV", "WY")

for (i in 1:51){
  start_time <- Sys.time()
  statename <- statesAbb[i]
  df[,paste0("friend_",statename)] <- ifelse(df$state == statesAbb[i],1,0)
  end_time <- Sys.time()
  print(i)
  print(end_time - start_time)
}

#deplete columns
df = subset(df, select = -c(user_id_following,nation,admin1,state,admin3,verified.y,verified.x,screen_name,location) )


### Collapse Dataset and Save ----

# collapse by user, summarize means
df_collapse <- df %>% group_by(user_id) %>% summarise_all(sum)

#save to rds (smaller file)
path <-  paste0("Data/Temp/socialnetwork_state_cele.rds")
saveRDS(df_collapse,file = path)





#non celebrity
# read the data (by chunk)
df <- readRDS("Data/Twitter/no_celebrity.rds")


### Generate summary variables ----

df$count <- 1 #count the total number 
#df <- dummy_cols(df,select_columns = 'nation') #dummies for all nation
df$nation_USA <- ifelse(df$nation == "USA",1,0) #dummy for USA


# statesAbb <- sort(unique(df$state)) #get a state list (include DC)
statesAbb <- c("AK", "AL", "AR", "AZ", "CA", "CO", "CT", "DC", "DE", "FL", 
               "GA", "HI", "IA", "ID", "IL", "IN", "KS", "KY", "LA", "MA", 
               "MD", "ME", "MI", "MN", "MO", "MS", "MT", "NC", "ND", "NE", 
               "NH", "NJ", "NM", "NV", "NY", "OH", "OK", "OR", "PA", "RI", 
               "SC", "SD", "TN", "TX", "UT", "VA", "VT", "WA", "WI", "WV", "WY")

for (i in 1:51){
  start_time <- Sys.time()
  statename <- statesAbb[i]
  df[,paste0("friend_",statename)] <- ifelse(df$state == statesAbb[i],1,0)
  end_time <- Sys.time()
  print(i)
  print(end_time - start_time)
}

#deplete columns
df = subset(df, select = -c(user_id_following,nation,admin1,state,admin3,verified.y,verified.x,screen_name,location) )


### Collapse Dataset and Save ----

# collapse by user, summarize means
df_collapse <- df %>% group_by(user_id) %>% summarise_all(sum)

#save to rds (smaller file)
path <-  paste0("Data/Temp/socialnetwork_state_nocele.rds")
saveRDS(df_collapse,file = path)














### Basic Description ----

# density of the number of friend
ggplot(df_collapse, aes(x = count)) +
  geom_density(aes(y = ..count..))+
  labs(x="Number of Friends", y="Frequency", title="Distribution of Users' Number of Friends")
ggsave('Results/distribution of number of friend.png',width = 6, height = 4, dpi = 300)


# describe states distribution
sumstat <- df_collapse %>% summarise_all(mean) #summarize mean number of friends from a state
df_sumstat <- as.data.frame(t(subset(sumstat,select = -c(user_id,count,nation_USA)))) #tranpose
colnames(df_sumstat) <- c('AvgFriends') # change column name
# labels
statesAbb <- c("AK", "AL", "AR", "AZ", "CA", "CO", "CT", "DC", "DE", "FL", 
               "GA", "HI", "IA", "ID", "IL", "IN", "KS", "KY", "LA", "MA", 
               "MD", "ME", "MI", "MN", "MO", "MS", "MT", "NC", "ND", "NE", 
               "NH", "NJ", "NM", "NV", "NY", "OH", "OK", "OR", "PA", "RI", 
               "SC", "SD", "TN", "TX", "UT", "VA", "VT", "WA", "WI", "WV", "WY")
# add labels as a column
df_sumstat$states <- statesAbb
# sort according to friend number
df_sumstat <- df_sumstat[order(df_sumstat$AvgFriends,decreasing = TRUE),c(1,2)]

# barplot
df_sumstat %>% ggplot(aes(fct_rev(fct_reorder(states,AvgFriends)),AvgFriends)) + 
  geom_bar(stat="identity") + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  labs(x="States", y="Average Number", title="How many friends of a user is from a state? (Average)")
ggsave("Results/friendnumber_barplot.png",width = 6, height = 4, dpi = 300)







#B stage clean the vax rate separately
#celebrity
# read data sets
df_friendnum <- readRDS("Data/Temp/socialnetwork_state_nocele.rds")
df_vaxsupply <- readRDS("Data/Temp/df_supply_state.rds")

### Clean datasets format and values ----

## clean the form of df_vaxsupply
rownames(df_vaxsupply) <- df_vaxsupply$Location
#select states that have weights information
statesAbb <- c("AK", "AL", "AR", "AZ", "CA", "CO", "CT", "DC", "DE", "FL", 
               "GA", "HI", "IA", "ID", "IL", "IN", "KS", "KY", "LA", "MA", 
               "MD", "ME", "MI", "MN", "MO", "MS", "MT", "NC", "ND", "NE", 
               "NH", "NJ", "NM", "NV", "NY", "OH", "OK", "OR", "PA", "RI", 
               "SC", "SD", "TN", "TX", "UT", "VA", "VT", "WA", "WI", "WV", "WY")
df_vaxsupply <- df_vaxsupply[df_vaxsupply$Location %in% statesAbb]
# sort by states' names abbreviation
df_vaxsupply <- df_vaxsupply[order(df_vaxsupply$Location),]

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

saveRDS(df_friendweights,file = 'Data/Temp/friendweights_state_nocele.rds')


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
for (i in 5:33){
  name <- paste0("friendvax_week",i)
  new_name <- c(new_name,name)
}

colnames(df_friendvax) <- new_name


saveRDS(df_friendvax,file = 'Data/Temp/friendvax_state_nocele.rds')


#vax_state
#Clean_friendvax_vaxrate_state.R
#Yinzhong Shuai 2022.10.11

### Setup ----



# read data sets
df_friendnum <- readRDS("Data/Temp/socialnetwork_state_nocele.rds")
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

saveRDS(df_friendvax,file = 'Data/Temp/friendvax_vaxrate_state_nocele.rds')





#part c: Already Done
