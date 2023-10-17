#Analyze_static model regression.R
#Liang Yongyin - 2023.2.14



### Setup ----

# library manager
library(pacman)

# packages to use load them now using the pacman "manager"
p_load(tidyverse, iotools, data.table, haven, knitr, psych, stats4, stargazer, magrittr, 
       qwraps2, Jmisc, fastDummies, corrplot, csv, skedastic, lmtest, broom, usmap,
       car, sandwich, vtable, readxl, data.table, disprose, DescTools, ggplot2)

# working directory
setwd("~/Social Network and Vaccination")


### load and clean data ----
# X: hesitancy from May 26, 2021 t0 June 7, 2021)
county_hesitancy <- read_csv("Data/Region/Vaccine_Hesitancy_for_COVID-19__County_and_local_estimates.csv",
                             col_select = c("FIPS Code","County Name","State","Estimated hesitant",
                                            "Estimated hesitant or unsure","Estimated strongly hesitant","State Code"))
county_population <- read_csv("Data/Region/co-est2020.csv", 
                              col_select = c("STATE","COUNTY","STNAME","CTYNAME","CENSUS2010POP"))

# X: social connection data
friend_weight <- readRDS('Data/Temp/friendweights_state.rds')

# Y: each users' hesitancy = vaccine date - state announcement date
state_policy <- read_excel("Data/Region/COVID-19 US state policy database 3_30_2022.xlsx")
df_uservax <- read.csv("Data/Twitter/users_vacctime.csv",colClasses=c("user_id"="character"))

# Controls
df_demo <- readRDS("Data/Temp/users_demography.rds")


### Clean Data ----

## Dependent Variable (Y)
state_policy$admin1Abb <- state.abb[match(state_policy$STATE,state.name)]
state_policy$admin1Abb[state_policy$STATE == "District of Columbia"] <- "DC"
state_policy <- state_policy[-c(1:4),c("admin1Abb","STATE","PUBDATE")]
comment(state_policy$PUBDATE) <- 'Date general public became eligible for COVID-19 vaccination'
# convert time
state_policy$pubdate <- as.Date(as.numeric(state_policy$PUBDATE), origin = "1899-12-30") # excel date format

# user vaccine date
df_uservax$vaxdate <- as.Date(df_uservax$date_first)
df_uservax$vaxdate2 <- as.Date(df_uservax$date_second)

# merge policy date into individual dataframe
user_hesitant <- merge(df_uservax, state_policy, all.x = TRUE)
nationdate <- as.Date("2020-11-14")
user_hesitant$hesitancy <- as.numeric(difftime(user_hesitant$vaxdate, nationdate, units = "days"))
user_hesitant$hesitvax2 <- as.numeric(difftime(user_hesitant$vaxdate2, user_hesitant$vaxdate, units = "days"))

length(user_hesitant$hesitvax2)
length(user_hesitant$hesitvax2[is.na(user_hesitant$hesitvax2)])
length(user_hesitant$hesitvax2[user_hesitant$hesitvax2 != 28])
hist(user_hesitant$hesitvax2[user_hesitant$hesitvax2 != 28])


## Independent variable (X)

# get the county population proportion of each state in 2020
county_population$CENSUS2010POP[county_population$CENSUS2010POP == "X"] = 0 # NA value
county_population$FIPS <- paste(county_population$STATE,county_population$COUNTY, sep = "")
state_pop <- county_population[county_population$COUNTY == "000",][c("STATE","CENSUS2010POP")] # state population
state_pop <- rename(state_pop, statepop = CENSUS2010POP)
countypop_weight <- merge(county_population,state_pop, all.x = TRUE)[county_population$COUNTY != "000",]
countypop_weight$popweight <- as.numeric(countypop_weight$CENSUS2010POP)/as.numeric(countypop_weight$statepop)

# merge population proportion to the hesitancy table
county_hesitancy$FIPS <- sprintf("%05.0f", county_hesitancy$`FIPS Code`) # FIPS format 
hesitant_weight <- merge(county_hesitancy[,c("FIPS","Estimated hesitant","State Code")], 
                         countypop_weight[,c("FIPS","popweight","STATE")], 
                         all.x = TRUE)
hesitant_weight <- rename(hesitant_weight, admin1Abb = "State Code")

# weight county hesitancy to state hesitancy by population (2020)
hesitant_weight$hesitant_w <- hesitant_weight$`Estimated hesitant`*hesitant_weight$popweight
hesitant_weight$hesitant_w[is.na(hesitant_weight$hesitant_w)] = 0 # NA value
state_hesitancy <- hesitant_weight[,c("hesitant_w","admin1Abb")] %>% group_by(admin1Abb) %>% summarise_all(sum)

saveRDS(state_hesitancy,'Data/Temp/state_hesitancyCDC.rds')

# X: multiply social connection data for each user
df_friendw <- subset(friend_weight, select = -c(user_id,friendnum,friendnum_US,friendw_USA))
df_hesitancy <- state_hesitancy[,"hesitant_w"]
df_friendvax <- as.data.frame( as.matrix(df_friendw) %*% as.matrix(df_hesitancy) )
df_friendvax <- cbind(df_friendvax,user_id = friend_weight$user_id, 
                      friendnum = friend_weight$friendnum,
                      friendnum_US = friend_weight$friendnum_US)
df_friendvax <- rename(df_friendvax, friendhesit = hesitant_w)

## bind X and Y and controls
df <- merge(user_hesitant, df_friendvax, all.x = TRUE)
df <- merge(df, df_demo, all.x = TRUE)
df$friendhesit <- scale(df$friendhesit)[,1] # normalize X


### Regression ----

# drop those without US friends
df <- df[df$friendnum_US >= 1,]

# scatter plot
ggplot(df, aes(x=friendhesit, y=hesitancy)) +
  geom_point(size=0.001)

# OLS
m1 <- lm(hesitancy~friendhesit, data=df)
summary(m1)

m2 <- lm(hesitancy~friendhesit+gender+age+race+friendnum+factor(admin1Abb), data=df)
summary(m2)