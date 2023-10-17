#Robustness_SCI.R
#Shuai Yinzhong 2022.10.20

#============================== RELOCATION NOTICE ==============================%
#   The original Robustness_SCI.R has been removed to "Code/Clean/Clean_SCI.R", %
# please refer to the new file for more details. I sincerely apologize for the  %
# inconvenience brought to you. Thank you for your understanding.               %
#                                                                               %
# Yinzhong Shuai 2022.10.17                                                     %
#===============================================================================%
#Xiaotong Cui 2023.6.22 update a new version
### Setup ----

# library manager
library(pacman)

# packages to use load them now using the pacman "manager"
p_load(tidyverse, iotools, data.table, haven, knitr, psych, stats4, stargazer, 
       magrittr, qwraps2, Jmisc, fastDummies, corrplot, csv, skedastic, lmtest,
       mmr)


# working directory
setwd("~/Social Network and Vaccination")

# read data
#我错了我找不到最开始link的文件了呜呜呜
df_wide <- readRDS('Data/Final Data/wide table_N=92356_v0525.rds')
df <- df_wide

# Long table transformation
# Credit Chen Keyu, 神秘代码，勿动
## Vaccination Status 
colname_vax_weekn <- append(c("user_id"),paste0("vax_week", 1:33))
df_vax <- df_wide[colname_vax_weekn]

df_vax <- melt(df_vax, id.vars="user_id")
df_vax[,2] <- sub("^vax_week", "", df_vax[,2])

names(df_vax)[2] <- 'week'
names(df_vax)[3] <- 'vax'





## Log(SCI)-Weighted Vaxrate Status
colname_logsci_vaxrate_weekn <- append(c("user_id"),paste0("log_weightedvaxrate.", 5:33))
df_logweighted_vaxrate <- df_wide[colname_logsci_vaxrate_weekn]

df_logweighted_vaxrate <- melt(df_logweighted_vaxrate, id.vars="user_id")
df_logweighted_vaxrate[,2] <- sub("^log_weightedvaxrate.", "", df_logweighted_vaxrate[,2])

names(df_logweighted_vaxrate)[2] <- 'week'
names(df_logweighted_vaxrate)[3] <- 'logsci_vaxrate'

## Merge
colname_deplete <- append(colname_vax_weekn[-1], colname_sci_vaxrate_weekn[-1])
colname_deplete <- append(colname_deplete, colname_logsci_vaxrate_weekn[-1])
df_othervar <- df_wide[ , ! names(df_wide) %in% colname_deplete]

df_reg <- merge(df_weighted_vaxrate,df_vax,all.y=TRUE)
df_reg <- merge(df_logweighted_vaxrate, df_reg, all.y=TRUE)
df_reg <- merge(df_reg, df_othervar,all=TRUE)

## Free up memory
df_weighted_vaxrate <- NULL
df_logweighted_vaxrate <- NULL
df_vax <- NULL
df_wide <- NULL
df_othervar <- NULL
rm(df_weighted_vaxrate, df_logweighted_vaxrate, df_vax, df_wide, df_othervar)
gc()



### Regression ----
## Regression SCI ----
#偷懒，直接换了个变量
df_reg$friendvax <- df_reg$sci_vaxrate
# create lagged variables
setDT(df_reg)[, friendvax_lag1 := lag(friendvax), user_id]
setDT(df_reg)[, friendvax_lag2 := lag(friendvax_lag1), user_id]
setDT(df_reg)[, friendvax_lag3 := lag(friendvax_lag2), user_id]
setDT(df_reg)[, friendvax_lag4 := lag(friendvax_lag3), user_id]

# baseline logistic model
# 偷懒，只删了州的固定效应
b.simple <- glm(formula=vax ~ friendvax_lag1,
                family=binomial("logit"), data=df_reg)

b.FE <- glm(formula=vax ~ friendvax_lag1 + factor(week),
            family=binomial("logit"), data=df_reg)

b.benchmark <- glm(formula=vax ~ friendvax_lag1 + factor(gender) + 
                     factor(age) + factor(race) + factor(week),
                   family=binomial("logit"), data=df_reg)

b.2lag <- glm(formula=vax ~ friendvax_lag1 + friendvax_lag2 + 
               factor(gender) + factor(age) + factor(race) + factor(week),
             family=binomial("logit"), data=df_reg)

# 好吧，表的标题还是换了的
stargazer(b.simple, b.FE, b.benchmark, b.2lag, omit = c("Constant", "week",
                                                        "gender", "age", "wage", "race", "ALLfriend"), 
          covariate.labels = c("SCI-Vaxrate Lag1", "SCI-Vaxrate Lag2"),
          dep.var.labels.include = FALSE,
          no.space = TRUE,
          font.size = "small",
          column.sep.width = "3pt",
          omit.stat = c("ll","aic"),
          add.lines = list(c("State FE & No & No & No & No & No"),
                           c("week FE & No & Yes & Yes & Yes & Yes"),
                           c("Control & No & No & Yes & Yes & Yes")),
          title = "Regression Results",
          type = "latex",
          out = "Results/Regression/robustness-sci-vaxrate-20221118")

## Regression log(SCI) ----
# 继续偷懒
df_reg$friendvax <- df_reg$logsci_vaxrate
# create lagged variables
setDT(df_reg)[, friendvax_lag1 := lag(friendvax), user_id]
setDT(df_reg)[, friendvax_lag2 := lag(friendvax_lag1), user_id]
setDT(df_reg)[, friendvax_lag3 := lag(friendvax_lag2), user_id]
setDT(df_reg)[, friendvax_lag4 := lag(friendvax_lag3), user_id]

# baseline logistic model
b.simple <- glm(formula=vax ~ friendvax_lag1,
                family=binomial("logit"), data=df_reg)

b.FE <- glm(formula=vax ~ friendvax_lag1 + factor(week),
            family=binomial("logit"), data=df_reg)

b.benchmark <- glm(formula=vax ~ friendvax_lag1 + factor(gender) + 
                     factor(age) + factor(race) + factor(week),
                   family=binomial("logit"), data=df_reg)

b.2lag <- glm(formula=vax ~ friendvax_lag1 + friendvax_lag2 + 
               factor(gender) + factor(age) + factor(race) + factor(week),
             family=binomial("logit"), data=df_reg)


stargazer(b.simple, b.FE, b.benchmark, b.2lag, omit = c("Constant", "week",
                                                        "gender", "age", "wage", "race", "ALLfriend"), 
          covariate.labels = c("log(SCI)-Vaxrate Lag1", "log(SCI)-Vaxrate Lag2"),
          dep.var.labels.include = FALSE,
          no.space = TRUE,
          font.size = "small",
          column.sep.width = "3pt",
          omit.stat = c("ll","aic"),
          add.lines = list(c("State FE & No & No & No & No & No"),
                           c("week FE & No & Yes & Yes & Yes & Yes"),
                           c("Control & No & No & Yes & Yes & Yes")),
          title = "Regression Results",
          type = "latex",
          out = "Results/Regression/robustness-logsci-vaxrate-20221118")


