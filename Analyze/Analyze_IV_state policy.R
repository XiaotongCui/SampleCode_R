#Analyze_IV_state policy.R
#Liang Yongyin - 2022.12.2 (comments & first difference & visualization)
#Cui Xiaotong - 2022.12.11 (transform wide to long & regression)
#Cui Xiaotong - 2023.4 (IV additional analysis)
#Cui Xiaotong - 2023.5 (Case Studies)

install.packages("did")
library(did)
library(pacman)
p_load(ivglm, ivreg, tidyverse, iotools, data.table, haven, knitr, psych, stats4, stargazer, magrittr, 
       qwraps2, Jmisc, fastDummies, corrplot, csv, skedastic, lmtest, broom, usmap,bife,plm,
       car, sandwich, vtable, readxl, data.table, disprose, data.table, dplr, reshape, readxl)

################################################################################
###############       Clean the state's policy Date             ################
################################################################################

# read the data
setwd("~/Social Network and Vaccination")

# read event date excel
policy_date <- read_excel("~/Social Network and Vaccination/Data/Region/COVID-19 US state policy database 3_30_2022.xlsx")
policy_date$admin1Abb <- state.abb[match(policy_date$STATE,state.name)]
policy_date$admin1Abb[policy_date$STATE == "District of Columbia"] <- "DC"
policy_date <- policy_date[-c(1:4),c("admin1Abb","STATE","PUBDATE")]
comment(policy_date$PUBDATE) <- 'Date general public became eligible for COVID-19 vaccination'

# convert time
policy_date$PUBDATE <- as.Date(as.numeric(policy_date$PUBDATE), origin = "1899-12-30") # excel date format
policy_date$week <- week(policy_date$PUBDATE) + 7 # week number in our data
policy_date$dayofWeek <- wday(policy_date$PUBDATE) # day of Week

# separate early states and late states
LateStates <- policy_date$admin1Abb[policy_date$week == 23]
EarlyStates <- policy_date$admin1Abb[policy_date$week <= 19]


################################################################################
###############       Form the data panel                       ################
################################################################################

### create the late friend fraction variable

# load friend weights
friendweights_state <- readRDS("~/Social Network and Vaccination/Data/Temp/friendweights_state.rds")

# select out the late states
df_latefriend <- friendweights_state[,c("user_id", paste0("friendw_",LateStates))]
# sum up the proportions
df_latefriend$latefriendw <- rowSums(df_latefriend[,paste0("friendw_",LateStates)], na.rm = TRUE)
# save the variable
df_latefriend <- df_latefriend[,c("user_id","latefriendw")]


### merge the data

df_wide <- readRDS("Data/Temp/users_vaxtime.rds")
df_demo <- readRDS("Data/Temp/users_demography.rds")
 
df_wide <- merge(df_wide, df_latefriend, all.x = TRUE)
df_wide <- merge(df_wide, df_demo, all.x = TRUE)

df_wide <- df_wide %>% relocate(user_id,gender,age,race)


### subset the data to early states users

df_wide <- df_wide[df_wide$admin1Abb %in% EarlyStates,]


### transform wide to long

# DID, T is late friend vax, P is dummy after week23
#contruct P = 0 when week <23
#Transfer the original long data from Regression_baseline-state.R
#longprep <- my_data <- readRDS("Data/Temp/vaccinerate_friendlong.rds")
longprep <- my_data <- readRDS('Data/Final Data/long table_N=92356_v0525.rds')
#The only thing need to be merge is Late friend Vax
df_long <- merge(longprep, df_latefriend, by = "user_id")

df_long <- df_long[df_long$admin1Abb %in% EarlyStates,]
### regression
#generate P=afterweek23
df_long$week1 <- as.numeric(df_long$week)
df_long$afterweek23 <- ifelse(df_long$week1 >= 23, 1, 0)
df_long$afterweeklatefri <- df_long$afterweek23*df_long$latefriendw
df_wide$count <- 1
numearlystate <- aggregate(x=df_wide$count, by=list(df_wide$admin1),sum)

#exo|endo|IV
#linear regression
m_iv <- ivreg(vax ~ gender + age + race + admin1Abb | friendvax |afterweeklatefri, data = df_long)
summary(m_iv)
#logistic regression
#two-stage estimation UNFOUND PACKAGES?
fitX.LZ <- glm(formula=friendvax~afterweeklatefri+ gender + age + race + admin1Abb, data = df_long)
fitY.LX <- glm(formula=vax~friendvax+ gender + age + race + admin1Abb, data = df_long)

install.packages("ivtools")
library(ivtools)

fitIV <- ivglm(estmethod="ts", fitX.LZ=fitX.LZ, fitY.LX=fitY.LX, data = df_long, 
               ctrl=TRUE) 
summary(fitIV)
stargazer(fitIV, omit = c("admin1Abb"), 
          title = "Regression Results",
          type = "latex",
          out="Results/Regression/20230106_IV")


#2SLS by hand
'
df_long$friendvax[is.na( df_long$friendvax)] = 0
ivfirst <- lm(friendvax ~ afterweeklatefri, data = df_long)
rownames(ivfirst$fitted.values) <- NULL
df_long$friendvaxhat <- ivfirst$fitted.values
ivsecond <- glm(formula=vax ~ friendvaxhat + 
                     factor(gender) + factor(age) + factor(race) + 
                     factor(admin1Abb),
                   family=binomial("logit"), data=df_long)'



#Now we'd like to conduct an event study
#First constrct event study varaible, we look into +- 4 week
df_long$afterweek22 <- ifelse(df_long$week1 >= 22, 1, 0)
df_long$afterweeklatefri_1 <- df_long$afterweek22*df_long$latefriendw

df_long$afterweek21 <- ifelse(df_long$week1 >= 21, 1, 0)
df_long$afterweeklatefri_2 <- df_long$afterweek21*df_long$latefriendw

df_long$afterweek20 <- ifelse(df_long$week1 >= 20, 1, 0)
df_long$afterweeklatefri_3 <- df_long$afterweek20*df_long$latefriendw

df_long$afterweek19 <- ifelse(df_long$week1 >= 19, 1, 0)
df_long$afterweeklatefri_4 <- df_long$afterweek19*df_long$latefriendw

df_long$afterweek24 <- ifelse(df_long$week1 >= 24, 1, 0)
df_long$afterweeklatefri_01 <- df_long$afterweek24*df_long$latefriendw

df_long$afterweek25 <- ifelse(df_long$week1 >= 25, 1, 0)
df_long$afterweeklatefri_02 <- df_long$afterweek25*df_long$latefriendw

df_long$afterweek26 <- ifelse(df_long$week1 >= 26, 1, 0)
df_long$afterweeklatefri_03 <- df_long$afterweek26*df_long$latefriendw

df_long$afterweek27 <- ifelse(df_long$week1 >= 27, 1, 0)
df_long$afterweeklatefri_04 <- df_long$afterweek27*df_long$latefriendw

did0 <- lm(friendvax ~ afterweeklatefri_2+ afterweeklatefri_3+ afterweeklatefri_4+ afterweeklatefri+ afterweeklatefri_01+ afterweeklatefri_02+ afterweeklatefri_03+ afterweeklatefri_04, data = df_long)
#-1 time as bench mark

# Fit a static logit model
#didbife <- bife(friendvax ~ afterweeklatefri_2+ afterweeklatefri_3+ afterweeklatefri_4+ afterweeklatefri+ afterweeklatefri_01+ afterweeklatefri_02+ afterweeklatefri_03+ afterweeklatefri_04 + factor(week) | user_id, df_long)
#summary(didbife)

#No control
didplm <- plm(friendvax ~ 1+afterweeklatefri_2+ afterweeklatefri_3+ afterweeklatefri_4+ afterweeklatefri+ afterweeklatefri_01+ afterweeklatefri_02+ afterweeklatefri_03+ afterweeklatefri_04,data=df_long, index = c("user_id","week"),model = "within")
summary(didplm)

#Add control
didplmcon <- plm(friendvax ~ 1+afterweeklatefri_2+ afterweeklatefri_3+ afterweeklatefri_4+ afterweeklatefri+ afterweeklatefri_01+ afterweeklatefri_02+ afterweeklatefri_03+ afterweeklatefri_04 + gender + age + race,data=df_long, index = c("user_id","week"),model = "within")
summary(didplmcon)

'
# 加载所需的库
library(ggplot2)
library(ggthemes)

# 提取回归系数和标准误差
coef_data <- summary(didplm)$coefficients[c(2,4,6),c("Estimate","Std. Error")]
coef_data$group <- rownames(coef_data)

# 绘制回归系数图表
ggplot(coef_data, aes(x = group, y = Estimate, fill = group)) +
  geom_col() +
  geom_errorbar(aes(ymin = Estimate - 1.96*`Std. Error`, 
                    ymax = Estimate + 1.96*`Std. Error`),
                width = 0.2, size = 1.2) +
  labs(x = "Group", y = "Coefficient", fill = "Group") +
  theme_bw() +
  theme(legend.position = "none")

#now what is the parameters
print(did0$coefficients)

df_long$week0 <- as.numeric(df_long$week)
df_long$user_id0 <- as.numeric(df_long$user_id)
#use the package
out <- att_gt(yname = "friendvax",
              gname = "latefriendw",
              idname = "user_id0",
              tname = "week0",
              xformla = ~1,
              data = df_long,
              est_method = "reg"
)
'

#Now we move on to case studies
'Take two states, cal and Arizona
We just choose people in Arizona, and checkout there fraction of Cal friends. 
Repeat what we have done above'


# select out the late states
df_CAfriend <- friendweights_state[,c("user_id", "friendw_CA")]
### merge the data\


df_widecase <- readRDS("Data/Temp/users_vaxtime.rds")
df_democase <- readRDS("Data/Temp/users_demography.rds")

df_widecase <- merge(df_widecase, df_CAfriend, all.x = TRUE)
df_widecase <- merge(df_widecase, df_democase, all.x = TRUE)

df_widecase <- df_widecase %>% relocate(user_id,gender,age,race)


### subset the data to early states users

df_widecase <- df_widecase[df_widecase$admin1Abb %in% 'AZ',]


#The only thing need to be merge is Late friend Vax
df_longcase <- merge(longprep, df_CAfriend, by = "user_id")
df_longcase <- df_longcase[df_longcase$admin1Abb %in% 'AZ',]
### regression
#generate P=afterweek23
df_longcase$week1 <- as.numeric(df_longcase$week)
df_longcase$afterweek23 <- ifelse(df_longcase$week1 >= 23, 1, 0)
df_longcase$afterweeklatefri <- df_longcase$afterweek23*df_longcase$friendw_CA.x

df_long <- df_longcase
#Now we'd like to conduct an event study
#First constrct event study varaible, we look into +- 4 week
df_long$afterweek22 <- ifelse(df_long$week1 >= 22, 1, 0)
df_long$afterweeklatefri_1 <- df_long$afterweek22*df_long$friendw_CA.x

df_long$afterweek21 <- ifelse(df_long$week1 >= 21, 1, 0)
df_long$afterweeklatefri_2 <- df_long$afterweek21*df_long$friendw_CA.x

df_long$afterweek20 <- ifelse(df_long$week1 >= 20, 1, 0)
df_long$afterweeklatefri_3 <- df_long$afterweek20*df_long$friendw_CA.x

df_long$afterweek19 <- ifelse(df_long$week1 >= 19, 1, 0)
df_long$afterweeklatefri_4 <- df_long$afterweek19*df_long$friendw_CA.x

df_long$afterweek24 <- ifelse(df_long$week1 >= 24, 1, 0)
df_long$afterweeklatefri_01 <- df_long$afterweek24*df_long$friendw_CA.x

df_long$afterweek25 <- ifelse(df_long$week1 >= 25, 1, 0)
df_long$afterweeklatefri_02 <- df_long$afterweek25*df_long$friendw_CA.x

df_long$afterweek26 <- ifelse(df_long$week1 >= 26, 1, 0)
df_long$afterweeklatefri_03 <- df_long$afterweek26*df_long$friendw_CA.x

df_long$afterweek27 <- ifelse(df_long$week1 >= 27, 1, 0)
df_long$afterweeklatefri_04 <- df_long$afterweek27*df_long$friendw_CA.x

did0 <- lm(friendvax ~ afterweeklatefri_2+ afterweeklatefri_3+ afterweeklatefri_4+ afterweeklatefri+ afterweeklatefri_01+ afterweeklatefri_02+ afterweeklatefri_03+ afterweeklatefri_04, data = df_long)
#-1 time as bench mark

#No control
didplm <- plm(friendvax ~ 1+afterweeklatefri_2+ afterweeklatefri_3+ afterweeklatefri_4+ afterweeklatefri+ afterweeklatefri_01+ afterweeklatefri_02+ afterweeklatefri_03+ afterweeklatefri_04,data=df_long, index = c("user_id","week"),model = "within")
summary(didplm)

#Add control
didplmcon <- plm(friendvax ~ 1+afterweeklatefri_2+ afterweeklatefri_3+ afterweeklatefri_4+ afterweeklatefri+ afterweeklatefri_01+ afterweeklatefri_02+ afterweeklatefri_03+ afterweeklatefri_04 + gender + age + race,data=df_long, index = c("user_id","week"),model = "within")
summary(didplmcon)

