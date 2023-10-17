## Bootstraping Placebo Test
# Chen Keyu - 2023.1.5

# library manager
library(pacman)

# packages to use load them now using the pacman "manager"
p_load(tidyverse, iotools, data.table, haven, knitr, psych, stats4, stargazer, magrittr, 
       qwraps2, Jmisc, fastDummies, corrplot, csv, skedastic, lmtest, broom, usmap,
       car, sandwich, vtable, readxl, data.table, disprose,boot)

# working directory
setwd("~/Social Network and Vaccination")

set.seed(50000)

# read data
df_wide <- readRDS('Data/Final Data/state-sample-1diffsupply-123999.rds')
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
setDT(df_reg)[, friendvax_lag1 := lag(friendvax), user_id]
logit_test <- function(d,indices) {  
  d <- d[indices,]  
  fit <- glm(formula=vax ~ friendvax_lag1 + 
               factor(gender) + factor(age) + factor(race) + 
               factor(admin1Abb) + factor(week),
               data=df_reg,family=binomial("logit"))  
  return(coef(fit))  
}
boot_fit <- boot(  
  data = df_reg, 
  statistic = logit_test, 
  R = 10
) 
