# Regression_mutual-friend.R
# Yongyin Liang 2023.1.5

### Setup ----

# library manager
library(pacman)

# packages to use load them now using the pacman "manager"
p_load(tidyverse, iotools, data.table, haven, knitr, psych, stats4, stargazer, magrittr, 
       qwraps2, Jmisc, fastDummies, corrplot, csv, skedastic, lmtest, broom, usmap,
       car, sandwich, vtable, readxl, data.table, disprose)

# working directory
setwd("~/Social Network and Vaccination")

# read data
df_wide <- readRDS('Data/Final Data/mutual-friend-state-sample.rds')
df <- df_wide



### Pre-processing ----


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






### Regression ----

# create lagged variables
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


b.2lag = glm(formula=vax ~ friendvax_lag1 + friendvax_lag2 + 
               factor(gender) + factor(age) + factor(race) + 
               factor(admin1Abb) + factor(week),
             family=binomial("logit"), data=df_reg)

## summary and output
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
          title = "Robustness: Mutual Friends Effect",
          type = "latex",
          out="Results/Regression/mutual-friend_20230105")
