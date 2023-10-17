#Clean_internal-networ.R
#Liang Yongyin - 2022.10.6


### Setup ----

# library manager
library(pacman)

# packages to use load them now using the pacman "manager"
p_load(tidyverse, iotools, data.table, haven, knitr, psych, stats4, stargazer, magrittr, 
       qwraps2, Jmisc, fastDummies, corrplot, csv, skedastic, lmtest, doBy, bit64,
       lubridate,vtable,reshape2)

# set working directory
setwd("~/Social Network and Vaccination")

df <- readRDS("Data/Final Data/Inner Friends/inner_friends_weight_long.rds")





### Regression ----

# create lagged variables
setDT(df)[, INfriendvaxw_lag1 := lag(INfriendvaxw), user_id]
setDT(df)[, INfriendvaxw_lag2 := lag(INfriendvaxw_lag1), user_id]
setDT(df)[, INfriendvaxw_lag3 := lag(INfriendvaxw_lag2), user_id]
setDT(df)[, INfriendvaxw_lag4 := lag(INfriendvaxw_lag3), user_id]

# baseline logistic model
m0 = glm(formula=vax ~ INfriendvaxw_lag1,
         family=binomial("logit"), data=df)

# week, admin1Abb Fixed effects
m1 = glm(formula=vax ~ INfriendvaxw_lag1 + factor(admin1Abb) + factor(week),
         family=binomial("logit"), data=df)

# add individual controls
m2 = glm(formula=vax ~ INfriendvaxw_lag1 + 
           factor(gender) + factor(age) + factor(race) + ALLfriend +
           factor(admin1Abb) + factor(week),
         family=binomial("logit"), data=df)

m3 = glm(formula=vax ~ INfriendvaxw_lag1 + INfriendvaxw_lag2 +
           factor(gender) + factor(age) + factor(race) + ALLfriend + 
           factor(admin1Abb) + factor(week),
         family=binomial("logit"), data=df)

m4 = glm(formula=vax ~ INfriendvaxw_lag1 + INfriendvaxw_lag2 + 
           INfriendvaxw_lag3 + INfriendvaxw_lag4 +
           factor(gender) + factor(age) + factor(race) + ALLfriend + 
           factor(admin1Abb) + factor(week),
         family=binomial("logit"), data=df)

## summary and output
summary(m0)
summary(m1)
summary(m2)
summary(m3)
summary(m4)


## Output
regtab <- stargazer(m0, m1, m2, m3, m4, omit = c("Constant", "week", "admin1Abb",
                                       "gender", "age", "wage", "race", "ALLfriend"), 
                    covariate.labels = c("FriendVax Lag1", "FriendVax Lag2", "FriendVax Lag3", "FriendVax Lag4"),
                    dep.var.labels.include = FALSE,
                    no.space = TRUE,
                    font.size = "small",
                    column.sep.width = "3pt",
                    omit.stat = c("ll","aic"),
                    add.lines=list(c("State FE & No & Yes & Yes & Yes & Yes"),
                                     c("Week FE & No & Yes & Yes & Yes & Yes"),
                                     c("Control & No & No & Yes & Yes & Yes")),
          title = "Regression Results - Inner Network",
          type = "latex")




















