#Regression_celebrity.R
#Xiaotong 11.18
#用整理好的celebrity文件 分别做regression

#cele
#引用
#Clean_merge.R
#Liang Yongyin - 2022.9.6 | Yinzhong Shuai 2022.10.11

### Setup ----

# library manager
library(pacman)

# packages to use load them now using the pacman "manager"
p_load(tidyverse, iotools, data.table, haven, knitr, psych, stats4, stargazer, 
       magrittr, qwraps2, Jmisc, fastDummies, corrplot, csv, skedastic, lmtest,
       mmr)

# set working directory
setwd("~/Social Network and Vaccination")

# read data sets
df_uservax <- readRDS("Data/Temp/users_vaxtime.rds")
df_friendvax <- readRDS("Data/Temp/friendvax_state_nocele.rds")
df_friendvax_vaxrate <- readRDS("Data/Temp/friendvax_vaxrate_state_nocele.rds")
df_friendlocation <- readRDS("Data/Temp/friendweights_state_nocele.rds")
df_demo <- readRDS("Data/Temp/users_demography.rds")

# df_uservax <- subset(df_uservax, select = -c(...1))

# merge using usernames (supply)
df_state <- merge(df_uservax, df_friendvax, all.x = TRUE)
df_state <- merge(df_state, df_friendlocation, all.x = TRUE)
df_state <- merge(df_state, df_demo, all.x = TRUE)

df_state <- df_state %>% relocate(user_id,gender,age,race)

# merge using usernames (vaxrate)
df_state_vaxrate <- merge(df_uservax, df_friendvax_vaxrate, all.x = TRUE)
df_state_vaxrate <- merge(df_state, df_friendlocation, all.x = TRUE)
df_state_vaxrate <- merge(df_state, df_demo, all.x = TRUE)

df_state_vaxrate <- df_state_vaxrate %>% relocate(user_id,gender,age,race)

# save
saveRDS(df_state,file = 'Data/Final Data/state-sample-123999-nocele.rds')
saveRDS(df_state_vaxrate,file = 'Data/Final Data/state-sample-vaxrate-123999-nocele.rds')








#regression! Finanlly!

df_wide <- readRDS("Data/Final Data/state-sample-123999-nocele.rds")
df <- df_wide



### Pre-processing ----

# convert vax time
# nam_vax_week <- paste0("df_wide$vax_week", 1:33)
# fmla_sum_vax_week <- paste(nam_vax_week, collapse= "+")
# df_wide$vax_time <- 34 - eval(parse(text = fmla_sum_vax_week))

# Long panel transformation
#Chen Keyu 2022.9.15

## Vaccination Status 
colname_vax_weekn <- append(c("user_id"),paste0("vax_week", 1:33))
df_vax <- df_wide[colname_vax_weekn]

df_vax <- melt(df_vax, id.vars="user_id")
df_vax[,2] <- sub("^vax_week", "", df_vax[,2])

names(df_vax)[2] <- 'week'
names(df_vax)[3] <- 'vax'


## Friend Vaccination Status
colname_friendvax_weekn <- append(c("user_id"),paste0("friendvax_week", 5:33))
df_friend <- df_wide[colname_friendvax_weekn]

df_friend <- melt(df_friend, id.vars="user_id")
df_friend[,2] <- sub("^friendvax_week", "", df_friend[,2])

names(df_friend)[2] <- 'week'
names(df_friend)[3] <- 'friendvax'

## Merge
colname_deplete <- append(colname_vax_weekn[-1],colname_friendvax_weekn[-1])
df_othervar <- df_wide[ , ! names(df_wide) %in% colname_deplete]

df_reg <- merge(df_friend,df_vax,all.y=TRUE)
df_reg <- merge(df_reg, df_othervar,all=TRUE)

# Free up Memory
df_friend <- NULL
df_vax <- NULL
df_wide <- NULL
df_othervar <- NULL
rm(df_friend, df_vax, df_wide)
gc()

setDT(df_reg)[, friendvax_lag1 := lag(friendvax), user_id]
setDT(df_reg)[, friendvax_lag2 := lag(friendvax_lag1), user_id]
setDT(df_reg)[, friendvax_lag3 := lag(friendvax_lag2), user_id]
setDT(df_reg)[, friendvax_lag4 := lag(friendvax_lag3), user_id]

# baseline logistic model
b.simple <- glm(formula=vax ~ friendvax_lag1,
                family=binomial("logit"), data=df_reg)

b.FE <- glm(formula=vax ~ friendvax_lag1 + 
              factor(admin1Abb) + factor(week),
            family=binomial("logit"), data=df_reg)

b.benchmark <- glm(formula=vax ~ friendvax_lag1 + 
                     factor(gender) + factor(age) + factor(race) + 
                     factor(admin1Abb) + factor(week),
                   family=binomial("logit"), data=df_reg)

# b.interFE = glm(formula=vax ~ friendvax_lag1 + 
#            factor(gender) + factor(age) + factor(race) + 
#            factor(admin1Abb)*factor(week),
#          family=binomial("logit"), data=df_reg)

b.2lag = glm(formula=vax ~ friendvax_lag1 + friendvax_lag2 + 
               factor(gender) + factor(age) + factor(race) + 
               factor(admin1Abb) + factor(week),
             family=binomial("logit"), data=df_reg)

stargazer(b.simple, b.FE, b.benchmark, b.2lag, omit = c("Constant", "week", "admin1Abb",
                                                        "gender", "age", "wage", "race", "ALLfriend"), 
          covariate.labels = c("FriendVax Lag1", "FriendVax Lag2"),
          dep.var.labels.include = FALSE,
          no.space = TRUE,
          font.size = "small",
          column.sep.width = "3pt",
          omit.stat = c("ll","aic"),
          add.lines=list(c("State FE & No & Yes & Yes & Yes & Yes"),
                         c("week FE & No & Yes & Yes & Yes & Yes"),
                         c("Control & No & No & Yes & Yes & Yes")),
          title = "Regression Results",
          type = "latex",
          out="Results/Regression/benchmark_20230106_nocele")

#cele

#regression! Finanlly!

df_wide <- readRDS("Data/Final Data/state-sample-123999-cele.rds")
df <- df_wide



### Pre-processing ----

# convert vax time
# nam_vax_week <- paste0("df_wide$vax_week", 1:33)
# fmla_sum_vax_week <- paste(nam_vax_week, collapse= "+")
# df_wide$vax_time <- 34 - eval(parse(text = fmla_sum_vax_week))

# Long panel transformation
#Chen Keyu 2022.9.15

## Vaccination Status 
colname_vax_weekn <- append(c("user_id"),paste0("vax_week", 1:33))
df_vax <- df_wide[colname_vax_weekn]

df_vax <- melt(df_vax, id.vars="user_id")
df_vax[,2] <- sub("^vax_week", "", df_vax[,2])

names(df_vax)[2] <- 'week'
names(df_vax)[3] <- 'vax'


## Friend Vaccination Status
colname_friendvax_weekn <- append(c("user_id"),paste0("friendvax_week", 5:33))
df_friend <- df_wide[colname_friendvax_weekn]

df_friend <- melt(df_friend, id.vars="user_id")
df_friend[,2] <- sub("^friendvax_week", "", df_friend[,2])

names(df_friend)[2] <- 'week'
names(df_friend)[3] <- 'friendvax'

## Merge
colname_deplete <- append(colname_vax_weekn[-1],colname_friendvax_weekn[-1])
df_othervar <- df_wide[ , ! names(df_wide) %in% colname_deplete]

df_reg <- merge(df_friend,df_vax,all.y=TRUE)
df_reg <- merge(df_reg, df_othervar,all=TRUE)

# Free up Memory
df_friend <- NULL
df_vax <- NULL
df_wide <- NULL
df_othervar <- NULL
rm(df_friend, df_vax, df_wide)
gc()

setDT(df_reg)[, friendvax_lag1 := lag(friendvax), user_id]
setDT(df_reg)[, friendvax_lag2 := lag(friendvax_lag1), user_id]
setDT(df_reg)[, friendvax_lag3 := lag(friendvax_lag2), user_id]
setDT(df_reg)[, friendvax_lag4 := lag(friendvax_lag3), user_id]


df_reg$logvax <- log(df_reg$vax)
df_reg$logfriendvax_lag1 <- log(df_reg$friendvax_lag1)
df_reg$logfriendvax_lag2 <- log(df_reg$friendvax_lag2)
# baseline logistic model
b.simple <- glm(formula=vax ~ logfriendvax_lag1,
                family=binomial("logit"), data=df_reg)

b.FE <- glm(formula=vax ~ logfriendvax_lag1 + 
              factor(admin1Abb) + factor(week),
            family=binomial("logit"), data=df_reg)

b.benchmark <- glm(formula=vax ~ logfriendvax_lag1 + 
                     factor(gender) + factor(age) + factor(race) + 
                     factor(admin1Abb) + factor(week),
                   family=binomial("logit"), data=df_reg)

# b.interFE = glm(formula=vax ~ friendvax_lag1 + 
#            factor(gender) + factor(age) + factor(race) + 
#            factor(admin1Abb)*factor(week),
#          family=binomial("logit"), data=df_reg)

b.2lag = glm(formula=vax ~ logfriendvax_lag1 + logfriendvax_lag2 + 
               factor(gender) + factor(age) + factor(race) + 
               factor(admin1Abb) + factor(week),
             family=binomial("logit"), data=df_reg)

stargazer(b.simple, b.FE, b.benchmark, b.2lag, omit = c("Constant", "week", "admin1Abb",
                                                        "gender", "age", "wage", "race", "ALLfriend"), 
          covariate.labels = c("FriendVax Lag1", "FriendVax Lag2"),
          dep.var.labels.include = FALSE,
          no.space = TRUE,
          font.size = "small",
          column.sep.width = "3pt",
          omit.stat = c("ll","aic"),
          add.lines=list(c("State FE & No & Yes & Yes & Yes & Yes"),
                         c("week FE & No & Yes & Yes & Yes & Yes"),
                         c("Control & No & No & Yes & Yes & Yes")),
          title = "Regression Results",
          type = "latex",
          out="Results/Regression/benchmarklog_20230106_cele")
