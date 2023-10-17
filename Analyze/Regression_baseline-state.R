#Regression_baseline-state.R
#Liang Yongyin - 2022.9.12 | Shuai Yinzhong 2022.9.13 | Chen Keyu 2022.9.15 | Cui Xiaotong 2022.12.11



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
df_wide <- readRDS("Data/Final Data/state-sample-123999.rds")
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

saveRDS(df_reg, "Data/Temp/vaccinerate_friendlong.rds")
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

# b.interFE = glm(formula=vax ~ friendvax_lag1 + 
#            factor(gender) + factor(age) + factor(race) + 
#            factor(admin1Abb)*factor(week),
#          family=binomial("logit"), data=df_reg)

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
          title = "Regression Results",
          type = "text",
          out="Results/Regression/benchmark_20221115")



### Robustness Check ----
# Liang Yongyin 2022.11.13

# 1. Functional Froms: vary the functional form of independent variables to 
# account for different effects at different levels of vaccination

# log
r.funcLOG <- glm(formula=vax ~ log(friendvax_lag1+1) + 
      factor(gender) + factor(age) + factor(race) + 
      factor(admin1Abb) + factor(week),
    family=binomial("logit"), data=df_reg)

# square root
r.funcSQR <- glm(formula=vax ~ sqrt(friendvax_lag1+1) + 
                   factor(gender) + factor(age) + factor(race) + 
                   factor(admin1Abb) + factor(week),
                 family=binomial("logit"), data=df_reg)

stargazer(b.benchmark, r.funcLOG, r.funcSQR,
          omit = c("Constant", "week", "admin1Abb", "gender", "age", "wage", "race", "ALLfriend"), 
                      covariate.labels = c("FriendVax Lag1", "log(FriendVax Lag1)", "sqrt(FriendVax Lag1)"),
                      dep.var.labels.include = FALSE,
                      no.space = TRUE,
                      font.size = "small",
                      column.sep.width = "3pt",
                      omit.stat = c("ll","aic"),
                      title = "Robustness - Functional Forms",
                      type = "text",
                      out="Results/Regression/robust_function_20221115")





# 2. Trimming Extreme Values

# people with too many and too few friends
plot(density(df$friendnum[!is.na(df$friendnum)]))
upper <- quantile(df$friendnum[!is.na(df$friendnum)], .75)

df_trimfriend <- trim_DF(df_reg, "friendnum", "less", upper) %>% 
  trim_DF("friendnum", "more", 10)

t.friend <- glm(formula=vax ~ friendvax_lag1 + 
                  factor(gender) + factor(age) + factor(race) + 
                  factor(admin1Abb) + factor(week),
                family=binomial("logit"), data=df_trimfriend)


stargazer(b.benchmark, r.funcLOG, r.funcSQR, t.friend,
          omit = c("Constant", "week", "admin1Abb", "age", "gender", "race"), 
          covariate.labels = c("FriendVax Lag1"),
          column.labels = c("baseline", "log function", "sqrt function","friendnum=(10,1702)"),
          dep.var.labels.include = FALSE,
          no.space = TRUE,
          font.size = "small",
          column.sep.width = "3pt",
          omit.stat = c("ll","aic"),
          add.lines=list(c("State FE & Yes & Yes & Yes & Yes"),
                         c("week FE & Yes & Yes & Yes & Yes"),
                         c("Control & Yes & Yes & Yes & Yes")),
          title = "Robustness: Functional Forms and Trimmed Sample",
          type = "latex",
          out="Results/Regression/robust_func_trim_20230106")





# 3. Heterogeneity

# gender
h.gender <- glm(formula=vax ~ friendvax_lag1 + friendvax_lag1:gender +
                    factor(age) + factor(race) + 
                    factor(admin1Abb) + factor(week),
                  family=binomial("logit"), data=df_reg)

# age
h.age <- glm(formula=vax ~ friendvax_lag1 + friendvax_lag1:factor(age) +
                factor(gender) + factor(race) + 
                factor(admin1Abb) + factor(week),
              family=binomial("logit"), data=df_reg)

# race
h.race <- glm(formula=vax ~ friendvax_lag1 + friendvax_lag1:factor(race) +
                      factor(gender) + factor(age) + 
                      factor(admin1Abb) + factor(week),
                    family=binomial("logit"), data=df_reg)


# vaccine type
h.vaxtype <- glm(formula=vax ~ friendvax_lag1 + friendvax_lag1:factor(type) +
               factor(gender) + factor(age) + factor(race) +
               factor(admin1Abb) + factor(week),
             family=binomial("logit"), data=df_reg)


stargazer(b.benchmark, h.gender, h.age, h.race, h.vaxtype,
          keep = c("friendvax_lag1", "friendvax_lag1:gender", 
                   "friendvax_lag1:factor(age)", "friendvax_lag1:factor(race)","friendvax_lag1:factor(type)"), 
          covariate.labels = c("FriendVax",
                               "FriendVax*Female",
                               "FriendVax*18-39","FriendVax*40+",
                               "FriendVax*Asian","FriendVax*Mid Asian","FriendVax*Indian","FriendVax*Hisp","FriendVax*Black",
                               "FriendVax*MD","FriendVax*AZ","FriendVax*JS"),
          dep.var.labels.include = FALSE,
          no.space = TRUE,
          font.size = "small",
          column.sep.width = "3pt",
          omit.stat = c("ll","aic"),
          title = "Mechanism: Heterogeneity in Gender, Age, Race, and Vaccine Brand",
          type = "latex",
          out="Results/Regression/robust_heter_20230106")




