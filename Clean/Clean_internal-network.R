#Clean_internal-networ.R
#Liang Yongyin - 2022.10.4

#In this program, I will identify the "inner" friends in the social network file;
# assign the inner friends' vaccination dummies, and count friendvax for each user;
# combine this new friendvax variables with others variables
# both count (pure number of how many friends got vaccinated that week)
# and weight (the number of friend over the total friend (outer+inner) )
# are provided in seperate dataframe


### Setup ----

# library manager
library(pacman)

# packages to use load them now using the pacman "manager"
p_load(tidyverse, iotools, data.table, haven, knitr, psych, stats4, stargazer, magrittr, 
       qwraps2, Jmisc, fastDummies, corrplot, csv, skedastic, lmtest, doBy, bit64,
       lubridate,vtable,reshape2)

# set working directory
setwd("~/Social Network and Vaccination")

# read the social network data
df_network <- fread("Data/Twitter/users_socialnetwork.csv",integer64 = "character", fill = TRUE)
df_uservax <- readRDS("Data/Temp/users_vaxtime.rds")

# rename variables
colnames(df_network) <- c('user_id','user_id_following','verified','nation','admin1','state','admin3')
df_uservax <- rename(df_uservax, user_id_following = user_id)











### identify inner network ----

# get the user_id list
# inner_id <- unique(df_network$user_id)
inner_id <- df_uservax$user_id_following

# identify inner users with an indicator
df_network$inner <- +(df_network$user_id_following %in% inner_id)
df_inner <- df_network[df_network$inner == 1]

# save inner socialnetwork
saveRDS(df_inner,file = "Data/Temp/inner-socialnetwork-expanded.rds")










### Bind inner friend's vaccination dummies into the data ----

# read the users vaccination time data
df_uservax <- as.data.frame(select(df_uservax,-3:-10))

# merge using user_id_following
df_merge <- merge(df_inner, df_uservax, all.x = FALSE)

# change variable names
colnames(df_merge)[10:42] <- paste("INfriend", colnames(df_merge[,c(10:42)]), sep = "")
df_merge <- df_merge[order(df_merge$user_id)]




### Collapse and Describe ----

#df_merge <- dummy_cols(df_merge,select_columns = 'nation') #dummies for all nation
df_merge$INfriend_nationUSA <- ifelse(df_merge$nation == "USA",1,0) #dummy for USA


# statesAbb <- sort(unique(df_merge$state)) #get a state list (include DC)
statesAbb <- c("AK", "AL", "AR", "AZ", "CA", "CO", "CT", "DC", "DE", "FL", 
               "GA", "HI", "IA", "ID", "IL", "IN", "KS", "KY", "LA", "MA", 
               "MD", "ME", "MI", "MN", "MO", "MS", "MT", "NC", "ND", "NE", 
               "NH", "NJ", "NM", "NV", "NY", "OH", "OK", "OR", "PA", "RI", 
               "SC", "SD", "TN", "TX", "UT", "VA", "VT", "WA", "WI", "WV", "WY")

for (i in 1:51){
  statename <- statesAbb[i]
  df_merge[,paste0("INfriend_",statename)] <- ifelse(df_merge$state == statesAbb[i],1,0)
}

#deplete columns
df_merge = subset(df_merge, select = -c(user_id_following,screen_name,verified,nation,admin1,state,admin3) )

# collapse by user, summarize means
df_merge_collapse <- df_merge %>% group_by(user_id) %>% summarise_all(sum)

# rename
df_merge_collapse <- rename(df_merge_collapse, INfriend = inner)

# save
saveRDS(df_merge_collapse,file = "Data/Temp/inner-socialnetwork-collapsed.rds")




### Form the large panel (both count and weight) ----

## form the count panel

# get the total number of friends
df_friend <- as.data.frame(select(df_network,1))
df_friend$ALLfriend <- 1
df_friend <- df_friend %>% group_by(user_id) %>% summarise_all(sum)

# get the user basic information
df_uservax <- rename(df_uservax, user_id = user_id_following)
df_userinfo <- readRDS("Data/Temp/users_vaxtime_dummy.rds")[,1:10]
df_userdemo <- readRDS("Data/Temp/users_demography.rds")

# get the inner friend's vaccine status and locations
df_INfriendvax <- df_merge_collapse

df_count <- merge(df_INfriendvax,df_friend, all.x = TRUE)
df_count <- merge(df_count,df_uservax, all.x = TRUE)
df_count <- merge(df_count,df_userinfo, all.x = TRUE)
df_count <- merge(df_count,df_userdemo, all.x = TRUE)



## form the weight panel

# pass on some basic information
df_weight <- as.data.frame(select(df_count,1:2,89:133))

# Change variables to weights

# count inner friend weights for each week's vaccination number
for (i in 1:33){
  j = i+3 # the vax variables from the 4 to the 36 column
  df_weight[,paste0("INfriendvaxw_week",i)] <- 
    df_count[,j]/df_count[,89] # divide by total friend number at col 89
}

# count inner friend weights for each state
for (i in 1:51){ 
  statename <- statesAbb[i]
  k = i+37 # the states variables from the 38 to the 88 column
  df_weight[,paste0("INfriendw_",statename)] <- 
    df_count[,k]/df_count[,89] # divide by total friend number
}

# complement some variables
df_weight$INfriendw_nation_USA <- df_count[,37]/df_count[,89]
df_weight$INfriendw <- df_count[,3]/df_count[,89]


# save both panels
saveRDS(df_count,file = "Data/Final Data/Inner Friends/inner_friends_count.rds")
saveRDS(df_weight,file = "Data/Final Data/Inner Friends/inner_friends_weight.rds")







### Transform from wide to long panel (only weight panel) ----

# 1. Vaccination Status 
df_vax <- df_weight[c(1,4:36)]

df_vax <- melt(df_vax, id.vars="user_id")
df_vax[,2] <- sub("^vax_week", "", df_vax[,2])

names(df_vax)[2] <- 'week'
names(df_vax)[3] <- 'vax'

# 2. Friend Vaccination Status
df_friend <- df_weight[c(1,48:80)]

df_friend <- melt(df_friend, id.vars="user_id")
df_friend[,2] <- sub("^INfriendvaxw_week", "", df_friend[,2])

names(df_friend)[2] <- 'week'
names(df_friend)[3] <- 'INfriendvaxw'

# 3. Merge
df_weight_long <- merge(df_friend,df_vax,all.y=TRUE)

df_weight_nochange <- df_weight[c(1:3,37:47,81:133)]
df_weight_long <- merge(df_weight_long, df_weight_nochange,all=TRUE)

saveRDS(df_weight_long, "Data/Final Data/Inner Friends/inner_friends_weight_long.rds")

# end of data cleaning
rm(list = ls())
gc()



### Discription of Inner Networks ----

df_in <- readRDS("Data/Temp/inner-socialnetwork-collapsed.rds")
summary(df_in$INfriend)


# density of the number of friend
ggplot(df_in, aes(x = INfriend)) +
  geom_density(aes(y = ..count..))+
  scale_x_log10()+
  labs(x="Number of Friends", y="Frequency", title="Users' Number of Inner Network Friends")
ggsave('Results/Description/Inner Network Friend Description/distribution of number of inner friend.png',width = 6, height = 4, dpi = 300)


# describe states distribution
sumstat <- df_in %>% summarise_all(mean) #summarize mean number of friends from a state
df_sumstat_state <- as.data.frame(t(subset(sumstat,select = c(37:87)))) #tranpose
colnames(df_sumstat_state) <- c('AvgFriends') # change column name
# labels
statesAbb <- c("AK", "AL", "AR", "AZ", "CA", "CO", "CT", "DC", "DE", "FL", 
               "GA", "HI", "IA", "ID", "IL", "IN", "KS", "KY", "LA", "MA", 
               "MD", "ME", "MI", "MN", "MO", "MS", "MT", "NC", "ND", "NE", 
               "NH", "NJ", "NM", "NV", "NY", "OH", "OK", "OR", "PA", "RI", 
               "SC", "SD", "TN", "TX", "UT", "VA", "VT", "WA", "WI", "WV", "WY")
# add labels as a column
df_sumstat_state$states <- statesAbb
# sort according to friend number
df_sumstat_state <- df_sumstat_state[order(df_sumstat_state$AvgFriends,decreasing = TRUE),c(1,2)]

# barplot
df_sumstat_state %>% ggplot(aes(fct_rev(fct_reorder(states,AvgFriends)),AvgFriends)) + 
  geom_bar(stat="identity") + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  labs(x="States", y="Average Number", title="How many inner friends of a user is from a state? (Average)")
ggsave("Results/Description/Inner Network Friend Description/friendnumber_state-barplot.png",width = 6, height = 4, dpi = 300)


# describe the vaccination timing
df_in_weight <- readRDS("Data/Final Data/Inner Friends/inner_friends_weight.rds")
sumstat_weight <- df_in_weight %>% summarise_all(mean) #summarize mean number of friends from a state

df_sumstat_vax <- as.data.frame(t(subset(sumstat_weight,select = c(48:80))))
colnames(df_sumstat_vax) <- c('VaxxedFriend') # change column name
df_sumstat_vax$week <- c(1:33)
df_sumstat_vax %>% ggplot(aes(week, VaxxedFriend)) + 
  geom_line(stat="identity") + 
  labs(x="week", y="percentage", title="How many % friends reported vaccination")
ggsave("Results/Description/Inner Network Friend Description/friendvax-percentage-timeline.png",width = 6, height = 4, dpi = 300)

