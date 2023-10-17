#Name: general_data_cleaning.R
#Functions: Clean each data source and merge everything into a table
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
       ggplot2, ggridges, plotly, openxlsx, dplyr, readr, zoo)

# set working directory
setwd("~/Social Network and Vaccination")

kStatesAbb <- c("AK", "AL", "AR", "AZ", "CA", "CO", "CT", "DC", "DE", "FL", 
                "GA", "HI", "IA", "ID", "IL", "IN", "KS", "KY", "LA", "MA", 
                "MD", "ME", "MI", "MN", "MO", "MS", "MT", "NC", "ND", "NE", 
                "NH", "NJ", "NM", "NV", "NY", "OH", "OK", "OR", "PA", "RI", 
                "SC", "SD", "TN", "TX", "UT", "VA", "VT", "WA", "WI", "WV", "WY")


################################################################################
###############               Clean Each Sources                ################
################################################################################



### 1. Demography  #############################################################

# read the data
df.demography <- read.csv("Data/Twitter/users_demography.csv",
                          colClasses=c("user_id" = "character"))

names(df.demography)

# convert gender 
df.demography$gender <- as.character(df.demography$gender)
df.demography$gender[which(df.demography$gender == "male")] <- "0"
df.demography$gender[which(df.demography$gender == "female")] <- "1"
df.demography$gender <- as.numeric(df.demography$gender)

# convert age 
df.demography$age <- as.character(df.demography$age)
df.demography$age[which(df.demography$age == "under18")] <- "0"
df.demography$age[which(df.demography$age == "19_39")] <- "1"
df.demography$age[which(df.demography$age == "above40")] <- "2"
df.demography$age <- as.numeric(df.demography$age)

# convert race 
df.demography$race <- as.character(df.demography$race)
df.demography$race[which(df.demography$race == "white")] <- "0"
df.demography$race[which(df.demography$race == "asian")] <- "1"
df.demography$race[which(df.demography$race == "middle eastern")] <- "2"
df.demography$race[which(df.demography$race == "indian")] <- "3"
df.demography$race[which(df.demography$race == "latino hispanic")] <- "4"
df.demography$race[which(df.demography$race == "black")] <- "5"
df.demography$race <- as.numeric(df.demography$race)

saveRDS(df.demography, file = 'Data/Temp/users_demography.rds')






### 2. User Vaccine Time #######################################################

df.uservax <- read.csv("Data/Twitter/users_vacctime.csv", 
                       colClasses=c("user_id"="character"))


df.uservax$firstd <- as.numeric(as.Date(df.uservax$date_first)) # 18583-18810
df.uservax$secd <- as.numeric(as.Date(df.uservax$date_second))

# now lets deal with howmany weaks are there in total
# 2020/11/17 - 2021/7/2 (2020-11-16 44151 is mon) 44152 - 44379
# 32 weeks + 4 days = 33 weeks

for (i in 1:33) {
  df.uservax$var <- 0
  df.uservax$condition <- ((df.uservax$firstd - 18582)/7) - i
  df.uservax$var[df.uservax$condition < 0] <- 1
  df.uservax <-  plyr::rename(df.uservax, c(var = paste("vax_week", i, sep = "")))
}
df.uservax <- subset(df.uservax, select = -c(condition))

# delete values
rm(i,j)

# convert vaccine type
df.uservax$type[which(df.uservax$type == "Pfizer")] <- "0"
df.uservax$type[which(df.uservax$type == "Moderna")] <- "1"
df.uservax$type[which(df.uservax$type == "AstraZeneca")] <- "2"
df.uservax$type[which(df.uservax$type == "Johnson")] <- "3"
df.uservax$type[which(df.uservax$type == "Not sure")] <- "99"

saveRDS(df.uservax, "Data/Temp/users_vaxtime.rds")

# Description ------------------------------------------------------------------





### 3. User Social Network #####################################################


# read the data
df.socialnetwork <- fread("Data/Twitter/users_socialnetwork.csv", 
                          integer64="character", fill = TRUE)
#rename variables
colnames(df.socialnetwork) <- c('user_id', 'user_id_following', 'verified', 
                                'nation', 'admin1', 'state', 'admin3')


# Generate summary variables ---------------------------------------------------

# count the total number 
df.socialnetwork$count <- 1 
# dummy for USA
df.socialnetwork$nation_USA <- ifelse(df.socialnetwork$nation == "USA", 1, 0) 
# dummies for each state
for (i in 1:51){
  state.name <- kStatesAbb[i]
  cat(i)
  cat(state.name)
  df.socialnetwork[, paste0("friend_", state.name)] <- 
    ifelse(df.socialnetwork$state == kStatesAbb[i], 1, 0)
}

# delete values
rm(i, state.name)
# deplete columns
df.socialnetwork <- subset(df.socialnetwork, 
                           select = -c(user_id_following, verified, nation, admin1, state, admin3))


# Collapse Dataset and Save ---------------------------------------------------

# collapse by user, summarize means
df.friendnum <- df.socialnetwork %>% group_by(user_id) %>% summarise_all(sum)

# delete redundant data set
rm(df.socialnetwork)

# save to rds (smaller file)
saveRDS(df.friendnum, file = "Data/Temp/socialnetwork_state.rds")


# Transform counts into weights ---------------------------------

df.friendweights <- data.frame('user_id' = df.friendnum$user_id)

df.friendweights$friendnum <- df.friendnum$count
df.friendweights$friendnum_US <- df.friendnum$nation_USA

# add the weights of USA friends
df.friendweights[ , "friendw_USA"] <- 
  df.friendnum[ , "nation_USA"] / df.friendnum[ , "count"]

# add the friend weights of each state
for (i in 1:51){ 
  state.name <- kStatesAbb[i]
  # divide by total friend number
  df.friendweights[ , paste0("friendw_", state.name)] <- 
    df.friendnum[ , paste0("friend_", state.name)] / df.friendnum[, "count"] 
}

saveRDS(df.friendweights, file = 'Data/Temp/friendweights_state.rds')


# Basic Description ------------------------------------------------------------

# density of the number of friend
ggplot(df.friendnum, aes(x = count)) +
  geom_density(aes(y = ..count..))+
  labs(x="Number of Friends", y="Frequency", title="Distribution of Users' Number of Friends")
ggsave('Results/distribution of number of friend.png', width = 6, height = 4, dpi = 300)


# describe states distribution
sumstat <- df.friendnum %>% summarise_all(mean) #summarize mean number of friends from a state
df.sumstat <- as.data.frame(t(subset(sumstat, select = -c(user_id, count, nation_USA)))) #tranpose
colnames(df.sumstat) <- c('AvgFriends') # change column name

# add labels as a column
df.sumstat$states <- kStatesAbb
# sort according to friend number
df.sumstat <- df.sumstat[order(df.sumstat$AvgFriends, decreasing = TRUE),c(1,2)]

# barplot
df.sumstat %>% ggplot(aes(fct_rev(fct_reorder(states, AvgFriends)), AvgFriends)) + 
  geom_bar(stat = "identity") + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
  labs(x = "States", y = "Average Number",
       title="How many friends of a user is from a state? (Average)")

ggsave("Results/friendnumber_barplot.png", width = 6, height = 4, dpi = 300)


# delete redundant dataframe
rm(i, k, state.name, sumstat, df.sumstat)












### 4. Friend Vaccination Measure ##############################################


# Clean the state's cumulative vaccine distribution Data -----------------------
# (vaccine distributed to "per person" in the state)  

# read the data
#df.vacdist_rate <-  fread("Data/Region/distribution_by_state.csv")
df.vaxdist <- fread("Data/Region/distribution_by_state.csv",
                    select=c("Date", "Location", "Admin_Per_100K"))

df.vaxdist$Date <- as.numeric(mdy(df.vaxdist$Date))

# change to wide table 
df.vaxdist.wide <- reshape(df.vaxdist, idvar = "Date", timevar = "Location", direction = "wide")
colnames(df.vaxdist.wide) <- sub("Admin_Per_100K.","",colnames(df.vaxdist.wide))

# order rows and column, select the 51 states
df.vaxdist.wide <- df.vaxdist.wide[order(Date),]
df.vaxdist.sub <- subset(df.vaxdist.wide, select = kStatesAbb)

# divide the measure by 10,000 to make it "per capita"
df.vaxdist.sub <- df.vaxdist.sub / 100000

# take seven-day moving average
df.vaxdist.wide <- cbind(as.data.frame(df.vaxdist.wide$Date), 
                         zoo::rollmean(df.vaxdist.sub, 7, 
                                       fill = TRUE, align = "right"))
colnames(df.vaxdist.wide)[1] <- "Date"

# select Thursday and reverse the table
df.vaxdist.wide$DoW <- wday(df.vaxdist.wide$Date) # day of Week
df.vaxdist.wide <- rev(subset(df.vaxdist.wide, df.vaxdist.wide$DoW == 5 & Date <= 18810))

# From natural week to defined week：
df.vaxdist.wide$week <- ceiling((df.vaxdist.wide$Date - 18582)/7)
rownames(df.vaxdist.wide) <- df.vaxdist.wide$week
df.cumulative.dist.wide <- as.data.frame(t(subset(df.vaxdist.wide[2:29,], select = -c(DoW, Date, week))))
df.cumulative.dist.wide$Location <- rownames(df.cumulative.dist.wide)


# transform cumulative to first difference
df.1diff.dist.wide <- as.data.frame(t(diff(t(as.matrix(subset(df.cumulative.dist.wide, select = -Location))))))
df.1diff.dist.wide$Location <- rownames(df.1diff.dist.wide)


# order the rows (states)
df.cumulative.dist.wide <- df.cumulative.dist.wide[order(df.cumulative.dist.wide$Location),]
df.1diff.dist.wide <- df.1diff.dist.wide[order(df.1diff.dist.wide$Location),]

# save data frames
saveRDS(df.cumulative.dist.wide, file = "Data/Temp/df_cumulative_distribution_state_vaxrate.rds")
saveRDS(df.1diff.dist.wide, file = "Data/Temp/df.1diff.distribution_state_vaxrate.rds")


# Discription --------------------------------------------

# # visualization (1st diff)
# df.1diff.dist <- as.data.frame(df.1diff.dist[df.1diff.dist$Location %in% kStatesAbb])
# 
# # 3d plot
# plot_ly(df.1diff.dist, x = ~Week, y = ~Location, z = ~Dist_1diff, 
#         type = 'scatter3d', mode = 'lines', color = ~Location)

# delete redundant data
rm(df.cumulative.dist, df.1diff.dist, df.vaxdist, week.df.vaxdist)








# 5. Multiply Friend Matrix with State Vax Matrix #################################


# define a function that multiplies the state vaccination measure with each user's
# social network matrix
FriendTimesVax <- function(df.friendweights, df.vaxsupply){
  
  ## clean the form of df.vaxsupply
  # rownames(df.vaxsupply) <- df.vaxsupply$Location
  # #select states that have weights information
  # df.vaxsupply <- df.vaxsupply[df.vaxsupply$Location %in% kStatesAbb]
  # sort by states' names abbreviation
  # df.vaxsupply <- df.vaxsupply[order(df.vaxsupply$Location), ]
  # 
  
  ## Create a column of week t by weighting the vaccine supply for each user
  df.weights <- subset(df.friendweights, 
                       select = -c(user_id, friendnum, friendnum_US, friendw_USA))
  df.vaxsupply <- subset(df.vaxsupply, select = -c(Location))
  
  # matrix multiplication
  df.friendvax <- as.data.frame(as.matrix(df.weights) %*% as.matrix(df.vaxsupply))
  
  # bind variables together
  df.friendvax <- cbind(df.friendvax, 
                        user_id = df.friendweights$user_id, 
                        friendnum = df.friendweights$friendnum, 
                        friendnum_US = df.friendweights$friendnum_US)
  
  # adjust variables, take user_id to front
  df.friendvax <- df.friendvax %>% relocate(user_id, friendnum, friendnum_US)
  
  # change column names
  new.name <- c('user_id', 'friendnum', 'friendnum_US')
  for (i in 7:33){
    name <- paste0("friendvax_week", i)
    new.name <- c(new.name,name)
  }
  
  colnames(df.friendvax) <- new.name
  
  return(df.friendvax)
}

# apply the function on different state vaccine measure
df.friendvax.dist <- FriendTimesVax(df.friendweights, df.1diff.dist.wide)
# df.friendvax.vaxrate <- FriendTimesVax(df.friendnum, )

saveRDS(df.friendvax.dist, file = 'Data/Temp/friendvax_state.rds_vaxrate')
# saveRDS(df.friendvax.vaxrate, file = 'Data/Temp/friendvax_state.rds')




################################################################################
###############                       Merge                     ################
################################################################################

# read data sets (if not in the environment)
# df.uservax <- readRDS("Data/Temp/users_vaxtime.rds")
# df.friendvax.dist <- readRDS("Data/Temp/friendvax_state.rds")
# df.friendvax.vaxrate <- readRDS("Data/Temp/friendvax_vaxrate_state.rds")
# df.friendweights <- readRDS("Data/Temp/friendweights_state.rds")
# df.demography <- readRDS("Data/Temp/users_demography.rds")


# merge using usernames (supply cumulative)
df.state.dist <- merge(df.uservax, df.friendvax.dist, all.x = TRUE)
df.state.dist <- merge(df.state.dist, df.friendweights, all.x = TRUE)
df.state.dist <- merge(df.state.dist, df.demography, all.x = TRUE)

df.state.dist <- df.state.dist %>% relocate(user_id, gender, age, race)

# merge using usernames (vaxrate)
# df.state.vaxrate <- merge(df.uservax, df.friendvax.vaxrate, all.x = TRUE)
# df.state.vaxrate <- merge(df.state.vaxrate, df.friendweights, all.x = TRUE)
# df.state.vaxrate <- merge(df.state.vaxrate, df.demography, all.x = TRUE)
# 
# df.state.vaxrate <- df.state.vaxrate %>% relocate(user_id, gender, age, race)


# Inspect the number of missing values in each column
colSums(is.na(df.state.dist))
# delete rows without friend information
df.state.dist <- df.state.dist[complete.cases(df.state.dist[ , paste0("friendvax_week", 7:33)]), ] # 92,356

# save wide table
saveRDS(df.state.dist, file = 'Data/Final Data/wide table_N=92356_v0525_vaxrate.rds')
# saveRDS(df.state.vaxrate, file = 'Data/Final Data/state-sample-vaxrate-123999.rds')


# Transform to long table function ---------------------------------------------
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

# wide to long
df.state.dist.long <- WideToLong(df.state.dist)
# df_state_vaxrate_long <- WideToLong(df.state.vaxrate)

# arrange by variables
df.state.dist.long <- df.state.dist.long %>% arrange(user_id, week)

saveRDS(df.state.dist.long, file = 'Data/Final Data/long table_N=92356_v0525_vaxrate.rds')
# saveRDS(df_state_vaxrate_long,file = 'Data/Final Data/long_state-sample-vaxrate-123999.rds')












