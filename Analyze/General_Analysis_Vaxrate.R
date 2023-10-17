# General_Analysis_Vaxrate.R
# Function: 
# Summary Statistics & Descriptions; Balancing Test; Dynamic Baby Model;
# Main Regression; Robustness Checks
# Cui Xiaotong- 2023.6.22 (summarise); Liang Yongyin (summarise), Chen Keyu, Shuai Yinzhong


# clear all
rm(list=ls())

# library manager
library(pacman)

# packages to use load them now using the pacman "manager"
p_load(tidyverse, iotools, data.table, haven, knitr, psych, stats4, stargazer, magrittr, 
       qwraps2, Jmisc, fastDummies, corrplot, csv, skedastic, lmtest, broom, usmap,
       car, sandwich, vtable, readx, disprose, mmr, doBy, lubridate, dplr, reshape,
       ggplot2, ggridges, plotly, openxlsx, dplyr, readr, fixest,
       geojsonio, RColorBrewer, rgdal, rgeos, igraph, bife, texreg, miceadds,
       sjPlot, sjlabelled, sjmisc)

# suppress error
library(base64)
suppressWarnings
suppressMessages
options(error = expression(NULL))

# working directory
setwd("~/Social Network and Vaccination")

kStatesAbb <- c("AK", "AL", "AR", "AZ", "CA", "CO", "CT", "DC", "DE", "FL", 
                "GA", "HI", "IA", "ID", "IL", "IN", "KS", "KY", "LA", "MA", 
                "MD", "ME", "MI", "MN", "MO", "MS", "MT", "NC", "ND", "NE", 
                "NH", "NJ", "NM", "NV", "NY", "OH", "OK", "OR", "PA", "RI", 
                "SC", "SD", "TN", "TX", "UT", "VA", "VT", "WA", "WI", "WV", "WY")

# import data
df.wide <- readRDS('Data/Final Data/wide table_N=92356_v0525_vaxrate.rds')
df.long <- readRDS('Data/Final Data/long table_N=92356_v0525_vaxrate.rds')

# create first lag
setDT(df.long)[, friendvax_lag1 := lag(friendvax), user_id]


################################################################################
###############                 Delete empty rows             #################
################################################################################

# Inspect the number of missing values in each column
colSums(is.na(df.wide))

# delete users without reported location
df.omit <- df.wide[complete.cases(df.wide[ , "admin1Abb"]), ] # 86,832


################################################################################
###############       Summary Statistics & Descriptions        #################
################################################################################

# Table of Summary Statistics --------------------------------------------------

# Prepare data
df.st <- df.wide[ , c("gender", "race", "friendnum", "friendnum_US")]

# Generate tables
st(df.wide, file="Results/Description/summary_statistics")
st(df.st, out='latexpage', file="Results/Description/summary_statistics.tex")



# Sample user characteristics --------------------------------------------------

df.bar <- df.wide[ , c("user_id", "race", "age", "gender")]

# transform factor to characters
df.bar$gender[which(df.bar$gender == "0")] <- "M"
df.bar$gender[which(df.bar$gender == "1")] <- "F"

df.bar$age[which(df.bar$age == "0")] <- "Under 18"
df.bar$age[which(df.bar$age == "1")] <- "Younger Adults"
df.bar$age[which(df.bar$age == "2")] <- "Elder Adults"

df.bar$race[which(df.bar$race == "0")] <- "White"
df.bar$race[which(df.bar$race == "1")] <- "Asian"
df.bar$race[which(df.bar$race == "2")] <- "Middle Eastern"
df.bar$race[which(df.bar$race == "3")] <- "Indian"
df.bar$race[which(df.bar$race == "4")] <- "Hispanic"
df.bar$race[which(df.bar$race == "5")] <- "Black"

# prepare data for plots
df.bar <- df.bar[complete.cases(df.bar), ] #delete rows with missing values
# N = 65415

# new variable for plots
df.bar$num <- df.bar$race
df.bar$num[which(df.bar$num == "Under 18")] <- "0"
df.bar$num[which(df.bar$num == "Younger Adults")] <- "1"
df.bar$num[which(df.bar$num == "Elder Adults")] <- "2"

# barplot
v_factor_levels <- c("Under 18", "Younger Adults", "Elder Adults")
ggplot(df.bar, aes(x = gender)) +
  geom_bar(aes(fill = factor(age, levels = v_factor_levels)))+
  theme(legend.position = "top") +
  theme_bw() + 
  facet_grid( ~ race) +
  theme(legend.title = element_blank()) +
  labs(title = "Barplots of User Characteristics (N=65,415)", 
       x = "Gender", y = "Count")

# ggsave('Results/summary statistics.png', width = 8.5, height = 5, dpi = 300)



# Self-reported vaccination behaviors over time --------------------------------

## 1. Density plot of vaccination date
# change date format
df.density <- df.wide[ , c("user_id", "firstd")]
df.density <- df.density[complete.cases(df.density), ] # delete NAs rows
df.density$firstd <- as.Date(df.density$firstd)

ggplot(df.density, aes(x = firstd)) + 
  geom_density() + 
  scale_x_date(date_breaks = "1 month", date_labels =  "%b %Y") +
  labs(title = "Density Plot of Vaccination Behaviors Over Time (N=12,399)", 
       x = "Time", y = "Vaccination")

# ggsave('Results/vaccine date density.png', width = 7, height = 5, dpi = 300)


## 2. line plots of cumulative vaccination rate
df.sum.first <- df.wide  %>% group_by(date_first) %>% summarise(vax_first = n()) 
df.sum.second <- df.wide  %>% group_by(date_second) %>% summarise(vax_second = n()) 
df.sum.first$cum_first <- cumsum(df.sum.first[, 2])
df.sum.second$cum_second <- cumsum(df.sum.second[, 2])
names(df.sum.first)[1] = "date"
names(df.sum.second)[1] = "date"

# First dose cumulative timeline
ggplot(data = df.sum.first, mapping = aes(x = as.Date(date), 
                                          y = cum_first$vax_first, group = 1)) + 
  geom_line() + 
  scale_x_date(date_breaks = "1 month", date_labels =  "%b %Y") +
  theme(axis.text.x=element_text(angle=30, hjust=1)) +
  labs(x = "Time", y = "Count", 
       title = "Cumulative Line Plot of First-Dose Vaccination")

# ggsave("Results/Description/first_vax_cumulative_line.png", width = 6, height = 4, dpi = 300)

# First and Second dose cumulative timeline
ggplot() + 
  geom_line(data = df.sum.first, aes(x = as.Date(date), 
                                     y = cum_first$vax_first, color = "blue")) +
  geom_line(data = df.sum.second, aes(x = as.Date(date), 
                                      y = cum_second$vax_second, color = "red")) +
  scale_x_date(date_breaks = "1 month", date_labels =  "%b %Y") +
  theme(axis.text.x=element_text(angle=30, hjust=1)) +
  labs(x = "Time", y = "Count", color = "Legend", 
       title = "Cumulative Line Plot of Second-Dose Vaccination") +
  scale_color_manual(labels = c("first dose", "second dose"), 
                     values = c("blue", "red"))

# ggsave("Results/Description/first_second_vax_cumulative_line.png", 
#        width = 6, height = 4, dpi = 300)



# Location Description (State-Level) -------------------------------------------

# Hexmap Insertion
spdf <- geojson_read("Data/Map/us_states_hexgrid.geojson", what = "sp")
spdf@data <- spdf@data %>%
  mutate(google_name = gsub(" \\(United States\\)", "", google_name))
spdf.fortified <- tidy(spdf, region = "google_name")
centers <- cbind.data.frame(data.frame(gCentroid(spdf, byid = TRUE), 
                                       id = spdf@data$iso3166_2))

## 1. id distribution, state level
df.state.pop <- df.wide %>% group_by(admin1) %>% 
  summarize(user = n()) %>% 
  drop_na() %>% 
  rename(state = admin1)

# normalized by state population
population <- read_excel("Data/Region/NST-EST2022-POP.xlsx")
population <- population[9:59, c(1, 4)]
colnames(population) <- c('state', 'pop')
population$state <- sub('.', '', population$state)

df.state.pop <- merge(df.state.pop, population, all.x = TRUE)
df.state.pop$user_percap <- df.state.pop$user / df.state.pop$pop

# real map
png("Results/Description/Map/id_distribution1.png")
plot_usmap(data = df.state.pop, values = "user_percap", color = "blue") + 
  scale_fill_continuous(low = "white", high = "blue", name = "ID Distribution", 
                        label = scales :: comma) + 
  labs(title = "User Distribution Per Cap, by state") + 
  theme(legend.position = "right")
dev.off()


spdf.fortified <- spdf.fortified %>%
  left_join(., df.state.pop, by = c("id" = "state")) 

# hex map
png("Results/Description/Map/id_distribution2.png")
ggplot() +
  geom_polygon(data = spdf.fortified, aes(fill = user_percap, x = long, y = lat, 
                                          group = group), color="white") +
  scale_fill_continuous(low = "skyblue", high = "blue", name = "ID Distribution", 
                        label = scales::comma) + 
  geom_text(data = centers, aes(x = x, y = y, label = id)) + 
  labs(title = "ID Distribution Per Cap, by state") + 
  theme_void() +
  coord_map()
dev.off()


## 2. Percentage of "local" (same state) friend

# select the friend weight matrix
df.friendw <- df.wide %>% dplyr :: select(starts_with("friendw_"))
df.friendw$admin1Abb <- df.wide$admin1Abb
df.friendw <- select(df.friendw, -c("friendw_USA"))
df.friendw <- df.friendw[complete.cases(df.friendw),] # N = 85,988

# aggregate the mean of state friend weights
df.friendw  <- aggregate(df.friendw, by = list(admin1Abb = df.friendw$admin1Abb), 
                         FUN = mean, na.rm = TRUE, na.action = NULL)

# a new row indicating the average of local state friend weights
for (i in (1:51))
  df.friendw[i, 53] <- df.friendw[i, i+1] # the AK row - AK column
names(df.friendw)[53]<-"friendw_local"
df.friendw <- df.friendw[c("admin1Abb", "friendw_local")] # keep the row only

# append state name
df.code <- read.csv("Data/Map/state_code.csv")
df.friendw <- merge(df.friendw, df.code, by = "admin1Abb")

# plot
png("Results/Description/Map/intrastate_friend1.png")
plot_usmap(data = df.friendw, values = "friendw_local", color="blue") + 
  scale_fill_continuous(low = "white", high = "blue", name = "Portion", 
                        label = scales :: comma) + 
  labs(title = "Average Proportion Friends That Are In the Same State, by state") + 
  theme(legend.position = "right")
dev.off()

spdf.fortified <- tidy(spdf, region = "google_name")
spdf.fortified <- spdf.fortified %>%
  left_join(. , df.friendw, by = c("id" = "state")) 

png("Results/Description/Map/intrastate_friend2.png")
ggplot() +
  geom_polygon(data = spdf.fortified, 
               aes(fill = friendw_local, x = long, y = lat, group = group), 
               color = "white") +
  scale_fill_continuous(low = "skyblue", high = "blue", name = "Portion", 
                        label = scales :: comma) + 
  geom_text(data = centers, aes(x = x, y = y, label = id)) +
  labs(title = "Average Proportion Friends That Are In the Same State, by state") + 
  theme_void() +
  coord_map()
dev.off()


# 3. Social connection map




# delete used data set
rm(i, df.st, df.bar, df.density, df.sum.first, df.sum.second, v_factor_levels, 
   centers, df.code, df.friendw, df.state.pop, population, spdf.fortified, spdf)


################################################################################
###############                 Balancing Test                 #################
################################################################################

#Read the data of states' demography
df.demography <- read.csv("Data/Demographics/sc-est2019-alldata6.csv")


# Doing Averages ###############################################################

# population
df.total.pop <- aggregate(x = df.demography$POPESTIMATE2019, 
                          by = list(df.demography$STATE), sum)
pop.sum <- sum(df.total.pop$x)

# 1. age
df.age <- aggregate(x = df.demography$POPESTIMATE2019, 
                    by = list(df.demography$STATE, df.demography$AGE), sum)
df.age$Multi <- df.age$Group.2 * df.age$x
df.avg.age <- aggregate(x = df.age$Multi, by = list(df.age$Group.1), sum)
df.avg.age$final <- df.avg.age$x / df.total.pop$x

# 2. race
df.race <- aggregate(x = df.demography$POPESTIMATE2019, 
                     by = list(df.demography$STATE,df.demography$RACE), sum)
#Doing Averages
df.race$Multi <- df.race$Group.2 * df.race$x
df.avg.race <- aggregate(x = df.race$Multi, by = list( df.race$Group.1), sum)
df.avg.race$final <- df.avg.race$x / df.total.pop$x

# 3. sex
df.sex <- aggregate(x = df.demography$POPESTIMATE2019, 
                    by = list(df.demography$STATE, df.demography$SEX), sum)
df.sex$Multi <- df.sex$Group.2 * df.sex$x
df.avg.sex <- aggregate(x = df.sex$Multi, by = list( df.sex$Group.1), sum)
df.avg.sex$final <- df.avg.sex$x / df.total.pop$x


# Test among states ############################################################

# 1. age
df.total.age <- aggregate(x = df.demography$POPESTIMATE2019, 
                          by = list(df.demography$AGE), sum)
df.total.age$Multi <- df.total.age$Group.1 / pop.sum*df.total.age$x
final.age <- sum(df.total.age$Multi)

# 2. race
df.total.race <- aggregate(x = df.demography$POPESTIMATE2019, 
                           by = list(df.demography$RACE), sum)
df.total.race$Multi <- df.total.race$Group.1 / pop.sum*df.total.race$x
final.race <- sum(df.total.race$Multi)

# 3. gender
df.total.sex <- aggregate(x = df.demography$POPESTIMATE2019, 
                          by = list(df.demography$SEX), sum)
df.total.sex$Multi <- df.total.sex$Group.1 / pop.sum*df.total.sex$x
final.sex <- sum(df.total.sex$Multi)


# Calculate the standard deviation of each state ###############################

# 1. age
sd.age <- numeric(56)
for (i in 1:56){
  for (j in 1:4386){
    if (df.age$Group.1[j] == i){
      sd.age[i] <- sd.age[i] + df.age$x[j] / 
        (df.total.pop$x[which(grepl(i, df.total.pop$Group.1))] - 1) * 
        (df.age$Group.2[j] - df.avg.age$final[which(grepl(i, df.avg.age$Group.1))]) ^ 2
    }
  }
  sd.age[i] <- sqrt(sd.age[i])
}


# 2. race
sd.race <- numeric(56)
for (i in 1:56){
  for (j in 1:306){
    if(df.race$Group.1[j] == i){
      sd.race[i] <- sd.race[i] + df.race$x[j] / 
        (df.total.pop$x[which(grepl(i, df.total.pop$Group.1))] - 1) * 
        (df.race$Group.2[j] - df.avg.race$final[which(grepl(i, df.avg.race$Group.1))]) ^ 2
    }
  }
  sd.race[i] <- sqrt(sd.race[i])
}

# 3. sex
sd.sex <- numeric(56)
for (i in 1:56){
  for (j in 1:153){
    if (df.sex$Group.1[j] == i){
      sd.sex[i] <- sd.sex[i] + df.sex$x[j] / 
        (df.total.pop$x[which(grepl(i, df.total.pop$Group.1))]-1) * 
        (df.sex$Group.2[j] - df.avg.sex$final[which(grepl(i, df.avg.sex$Group.1))]) ^ 2
    }
  }
  sd.sex[i] =sqrt(sd.sex[i])
}


# Calculate the t value and p value of each state ###############################

no.state <- c(3, 7, 14, 43, 52)
# 1. age
t.age <- numeric(56)
for (i in 1:56){
  if ( !(i %in% no.state) ){
    t.age[i] <- (df.avg.age$final[which(grepl(i, df.avg.age$Group.1))] - final.age) / 
      sd.age[i]
  }
}
p.age <- 2*pnorm(t.age, mean = final.age, sd = sd.age)

# 2. race
t.race <- numeric(56)
for (i in 1:56){
  if ( !(i %in% no.state) ){
    t.race[i] <- (df.avg.race$final[which(grepl(i,df.avg.race$Group.1))] - final.race) / 
      sd.race[i]
  }
}

p.race <- 2*pnorm(t.race, mean = final.age, sd = sd.race)

# 3. sex
t.sex <- numeric(56)
for (i in 1:56){
  if ( !(i %in% no.state) ){
    t.sex[i] <- (df.avg.sex$final[which(grepl(i, df.avg.sex$Group.1))] - final.sex) / 
      sd.sex[i]
  }
}

p.sex <- 2*pnorm(t.sex, mean = final.sex, sd = sd.sex)


# Create the balanced table ###################################################

df.balance <- data.frame(matrix(nrow = 56))
df.balance$state.index <- 1:56

df.balance$t.age <- t.age
df.balance$p.age <- p.age
df.balance$star.age <- ifelse(abs(df.balance$p.age) <= 0.001, '***',
                              ifelse(abs(df.balance$p.age) <= 0.01, '**',
                                     ifelse(abs(df.balance$p.age) <= 0.05, '*', '')))

df.balance$t.race <- t.race
df.balance$p.race <- p.race
df.balance$star.race <- ifelse(abs(df.balance$p.race) <= 0.001, '***',
                               ifelse(abs(df.balance$p.race) <= 0.01, '**',
                                      ifelse(abs(df.balance$p.race) <= 0.05, '*', '')))

df.balance$t.sex <- t.sex
df.balance$p.sex <- p.sex
df.balance$star.sex <- ifelse(abs(df.balance$p.sex) <= 0.001, '***',
                              ifelse(abs(df.balance$p.sex) <= 0.01, '**',
                                     ifelse(abs(df.balance$p.sex) <= 0.05, '*', '')))

df.balance <- df.balance[-no.state,-1]
df.balance$name <- kStatesAbb

saveRDS(df.balance, file = 'Data/Demographics/balance.rds')



################################################################################
###############                 Dynamic Baby Model             #################
################################################################################

# read data
df.baby <- df.long

# generate lags
setDT(df.baby)[, friendvax_lag1 := lag(friendvax), user_id]
setDT(df.baby)[, friendvax_lag2 := lag(friendvax_lag1), user_id]
setDT(df.baby)[, friendvax_lag3 := lag(friendvax_lag2), user_id]


# coefficent dynamic table -----------------------------------------------------

DynamicCoeff <- function(df, model, start.week, end.week){
  # Conduct cross-sectional Regression at each time period.
  #
  # Args:
  #   df: dataframe to use in regression.
  #   model: regression model intended to use.
  #   start.week: the first week to apply the cross sectional regression at.
  #   end.week: the last week to apply the cross sectional regression at.
  
  # Returns:
  #   A tible of coefficients and CIs for each week
  
  baby.coeff <- tibble(
    Week = integer(),
    Variable = character(),
    Coefficient = double(),
    conf.low_95 = double(),
    conf.high_95 = double(),
    conf.high_90 = double(),
  )
  
  for (i in start.week:end.week){
    
    # cross-sectional data
    df.cross.section <- df[df$week == i, ]
    
    # logistic model
    fit <- glm(formula = model, 
               family = binomial("logit"), data = df.cross.section)
    
    results <- tidy(fit)
    
    fit_cis_95 <- confint(fit, level = 0.95) %>% 
      data.frame() %>%
      rename("conf.low_95" = "X2.5..",
             "conf.high_95" = "X97.5..")
    
    fit_cis_90 <- confint(fit, level = 0.90) %>% 
      data.frame() %>%
      rename("conf.low_90" = "X5..",
             "conf.high_90" = "X95..")
    
    results <- bind_cols(i, 
                         results, 
                         fit_cis_95, 
                         fit_cis_90) %>% 
      rename(Week = ...1,
             Variable = term,
             Coefficient = estimate,
             SE = std.error) %>%
      filter(Variable != "(Intercept)")
    
    results <- results %>% select(-SE, 
                                  -statistic,
                                  -p.value)
    
    baby.coeff <- rbind(baby.coeff, results)
    
    cat("Week", i, "\n")
  }
  
  return(baby.coeff)
}


# coefficient plot -------------------------------------------------------------

# models to use (change to conditional logit)
model1 <- vax ~ friendvax
model2 <- vax ~ friendvax_lag1

coeff.table1 <- DynamicCoeff(df.baby, model1, 7, 31)
coeff.table2 <- DynamicCoeff(df.baby, model2, 8, 31)

ggplot(coeff.table1, aes(x = Week, y = Coefficient)) +
  geom_hline(yintercept = 0, colour = gray(1/2), lty = 2) +
  geom_point(aes(x = Week, 
                 y = Coefficient)) + 
  geom_linerange(aes(x    = Week, 
                     ymin = conf.low_90,
                     ymax = conf.high_90),
                 lwd = 1) +
  geom_linerange(aes(x = Week, 
                     ymin = conf.low_95,
                     ymax = conf.high_95),
                 lwd = 1/2) + 
  ggtitle("Dynamic: logit(Vax) ~ FriendVax")

# ggsave('Results/Regression/dynamic baby coefficieint plots_v0525.png', width = 6, height = 4, dpi = 300)

ggplot(coeff.table2, aes(x = Week, y = Coefficient)) +
  geom_hline(yintercept = 0, colour = gray(1/2), lty = 2) +
  geom_point(aes(x = Week, 
                 y = Coefficient)) + 
  geom_linerange(aes(x    = Week, 
                     ymin = conf.low_90,
                     ymax = conf.high_90),
                 lwd = 1) +
  geom_linerange(aes(x = Week, 
                     ymin = conf.low_95,
                     ymax = conf.high_95),
                 lwd = 1/2) + 
  ggtitle("Dynamic: logit(Vax) ~ FriendVax_lag1")

# ggsave('Results/Regression/dynamic baby coefficieint plots_lag1.png', width = 6, height = 4, dpi = 300)

# clear data
rm(df.baby)


################################################################################
###############                   Main Regression              #################
################################################################################

# use data
df.main <- df.long

# time state fixed effect
m1 <- glm(formula = vax ~ friendvax_lag1 + 
            factor(week) + factor(admin1Abb),
          family = binomial("logit"), data = df.main)
summary(m1)

# time state fixed effect with individual characteristics controls
m2 <- glm(formula = vax ~ friendvax_lag1 + 
            factor(gender) + factor(age) + factor(race) + 
            factor(week) + factor(admin1Abb),
          family = binomial("logit"), data = df.main)
summary(m2)

# **MAIN** time state fixed effect with individual characteristics and network controls
m3 <- glm(formula = vax ~ friendvax_lag1 + 
            friendnum + friendnum_US + 
            factor(gender) + factor(age) + factor(race) + 
            factor(week) + factor(admin1Abb),
          family = binomial("logit"), data = df.main)
summary(m3)

# individual fixed effect
m4 <- bife(vax ~ friendvax_lag1 + factor(week) | user_id, 
           data = df.main, bias_corr = "ana")
summary(m4)

# multiple lags

# create more lagged variables
setDT(df.main)[, friendvax_lag2 := lag(friendvax_lag1), user_id]
setDT(df.main)[, friendvax_lag3 := lag(friendvax_lag2), user_id]
setDT(df.main)[, friendvax_lag4 := lag(friendvax_lag3), user_id]
setDT(df.main)[, friendvax_lag5 := lag(friendvax_lag4), user_id]
setDT(df.main)[, friendvax_lag6 := lag(friendvax_lag5), user_id]
setDT(df.main)[, friendvax_lag7 := lag(friendvax_lag6), user_id]

# subset sample
df.lag <- subset(df.main, df.main$week >= 13)

m5 <- glm(formula = vax ~ friendvax_lag1 +
            friendnum + friendnum_US + 
            factor(gender) + factor(age) + factor(race) + 
            factor(week) + factor(admin1Abb),
          family = binomial("logit"), data = df.lag)
summary(m5)

m6 <- glm(formula = vax ~ friendvax_lag1 + friendvax_lag2 +
            friendnum + friendnum_US + 
            factor(gender) + factor(age) + factor(race) + 
            factor(week) + factor(admin1Abb),
          family = binomial("logit"), data = df.lag)
summary(m6)

m7 <- glm(formula = vax ~ friendvax_lag1 + friendvax_lag2 + friendvax_lag3 + 
            friendnum + friendnum_US + 
            factor(gender) + factor(age) + factor(race) + 
            factor(week) + factor(admin1Abb),
          family = binomial("logit"), data = df.lag)
summary(m7)

m8 <- glm(formula = vax ~ friendvax_lag1 + friendvax_lag2 + friendvax_lag3 + friendvax_lag4 + 
            friendnum + friendnum_US + 
            factor(gender) + factor(age) + factor(race) + 
            factor(week) + factor(admin1Abb),
          family = binomial("logit"), data = df.lag)
summary(m8)

m9 <- glm(formula = vax ~ friendvax_lag1 + friendvax_lag2 + friendvax_lag3 + friendvax_lag4 + 
            friendvax_lag5 + 
            friendnum + friendnum_US + 
            factor(gender) + factor(age) + factor(race) + 
            factor(week) + factor(admin1Abb),
          family = binomial("logit"), data = df.lag)
summary(m9)

m10 <- glm(formula = vax ~ friendvax_lag1 + friendvax_lag2 + friendvax_lag3 + friendvax_lag4 + 
             friendvax_lag5 + friendvax_lag6 + 
             friendnum + friendnum_US + 
             factor(gender) + factor(age) + factor(race) + 
             factor(week) + factor(admin1Abb),
           family = binomial("logit"), data = df.lag)
summary(m10)

m11 <- glm(formula = vax ~ friendvax_lag1 + friendvax_lag2 + friendvax_lag3 + friendvax_lag4 + 
             friendvax_lag5 + friendvax_lag6 + friendvax_lag7 + 
             friendnum + friendnum_US + 
             factor(gender) + factor(age) + factor(race) + 
             factor(week) + factor(admin1Abb),
           family = binomial("logit"), data = df.lag)
summary(m11)

m12 <- glm(formula = vax ~ friendvax_lag1 + friendvax_lag7 + 
             friendnum + friendnum_US + 
             factor(gender) + factor(age) + factor(race) + 
             factor(week) + factor(admin1Abb),
           family = binomial("logit"), data = df.lag)
summary(m12)


# output table
table.main.reg <- 
  stargazer(m1, m2, m3, m5, m6, m7, m8, m9, m10, m11, m12,
            keep = c("friendvax_lag"),
            dep.var.labels.include = FALSE,
            no.space = TRUE,
            font.size = "small",
            column.sep.width = "3pt",
            omit.stat = c("f", "ll"),
            add.lines=list(c("State FE           & Yes & Yes & Yes & Yes & Yes & Yes & Yes & Yes & Yes"),
                           c("Week FE            & Yes & Yes & Yes & Yes & Yes & Yes & Yes & Yes & Yes"),
                           c("Individual Control &     & Yes & Yes & Yes & Yes & Yes & Yes & Yes & Yes"),
                           c("Network Control    &     &     & Yes & Yes & Yes & Yes & Yes & Yes & Yes")),
            title = "The effect of social network on vaccination",
            type = "text",
            out = "Results/Regression/main_20230424")


################################################################################
###############                Heterogeneities                 #################
################################################################################

df.heter1 <- df.long


df.heter <- df.long

# 1. individual characteristics --------------------------------

# gender
h20 <- glm(vax ~ friendvax_lag1 +
             friendnum + friendnum_US + 
             factor(age) + factor(race) + 
             factor(week) + factor(admin1Abb), 
           family = binomial("logit"), data = df.heter[df.heter$gender == 0])
h21 <- glm(vax ~ friendvax_lag1 +
             friendnum + friendnum_US  + 
             factor(age) + factor(race) + 
             factor(week) + factor(admin1Abb), 
           family = binomial("logit"), data = df.heter[df.heter$gender == 1])
# age
h30 <- glm(vax ~ friendvax_lag1 +
             friendnum + friendnum_US + 
             factor(gender) + factor(race) + 
             factor(week) + factor(admin1Abb), 
           family = binomial("logit"), data = df.heter[df.heter$age == 0])
h31 <- glm(vax ~ friendvax_lag1 +
             friendnum + friendnum_US  + 
             factor(gender) + factor(race) + 
             factor(week) + factor(admin1Abb), 
           family = binomial("logit"), data = df.heter[df.heter$age == 1])
h32 <- glm(vax ~ friendvax_lag1 +
             friendnum + friendnum_US  + 
             factor(gender) + factor(race) + 
             factor(week) + factor(admin1Abb), 
           family = binomial("logit"), data = df.heter[df.heter$age == 2])
# race
h40 <- glm(vax ~ friendvax_lag1 +
             friendnum + friendnum_US + 
             factor(gender) + factor(age) + 
             factor(week) + factor(admin1Abb), 
           family = binomial("logit"), data = df.heter[df.heter1$minority == 0])
h41 <- glm(vax ~ friendvax_lag1 +
             friendnum + friendnum_US + 
             factor(gender) + factor(age) + 
             factor(week) + factor(admin1Abb), 
           family = binomial("logit"), data = df.heter[df.heter$minority == 1])
df.look <-  df.heter[df.heter1$minority == 0]

# output

# coefficient plot
m3.b <- summary(m3)$coefficients["friendvax_lag1",][[1]]
m3.se <- summary(m3)$coefficients["friendvax_lag1",][[2]]
df.coefplot <- data.frame("All", "All", m3.b, m3.b - 1.96*m3.se, m3.b + 1.96*m3.se)

h20.b <- summary(h20)$coefficients["friendvax_lag1",][[1]]
h20.se <- summary(h20)$coefficients["friendvax_lag1",][[2]]
df.coefplot <- rbind(df.coefplot, c("Gender", "Male", h20.b, h20.b - 1.96*h20.se, h20.b + 1.96*h20.se))

h21.b <- summary(h21)$coefficients["friendvax_lag1",][[1]]
h21.se <- summary(h21)$coefficients["friendvax_lag1",][[2]]
df.coefplot <- rbind(df.coefplot, c("Gender", "Female", h21.b, h21.b - 1.96*h21.se, h21.b + 1.96*h21.se))

h30.b <- summary(h30)$coefficients["friendvax_lag1",][[1]]
h30.se <- summary(h30)$coefficients["friendvax_lag1",][[2]]
df.coefplot <- rbind(df.coefplot, c("Age", "Under 18", h30.b, h30.b - 1.96*h30.se, h30.b + 1.96*h30.se))

h31.b <- summary(h31)$coefficients["friendvax_lag1",][[1]]
h31.se <- summary(h31)$coefficients["friendvax_lag1",][[2]]
df.coefplot <- rbind(df.coefplot, c("Age", "19-39", h31.b, h31.b - 1.96*h31.se, h31.b + 1.96*h31.se))

h32.b <- summary(h32)$coefficients["friendvax_lag1",][[1]]
h32.se <- summary(h32)$coefficients["friendvax_lag1",][[2]]
df.coefplot <- rbind(df.coefplot, c("Age", "Above 40", h32.b, h32.b - 1.96*h32.se, h32.b + 1.96*h32.se))

h40.b <- summary(h40)$coefficients["friendvax_lag1",][[1]]
h40.se <- summary(h40)$coefficients["friendvax_lag1",][[2]]
df.coefplot <- rbind(df.coefplot, c("Race", "White", h40.b, h40.b - 1.96*h40.se, h40.b + 1.96*h40.se))

h41.b <- summary(h41)$coefficients["friendvax_lag1",][[1]]
h41.se <- summary(h41)$coefficients["friendvax_lag1",][[2]]
df.coefplot <- rbind(df.coefplot, c("Race", "Minority", h41.b, h41.b - 1.96*h41.se, h41.b + 1.96*h41.se))

# adjustment
colnames(df.coefplot) <- c("type", "model", "coef", "lower", "upper")
df.coefplot$coef <- as.numeric(df.coefplot$coef)
df.coefplot$lower <- as.numeric(df.coefplot$lower)
df.coefplot$upper <- as.numeric(df.coefplot$upper)

# coefficient plot
ggplot(df.coefplot, 
       aes(factor(model, 
                  level = c("All", "Male", "Female", "Under 18", "19-39", 
                            "Above 40", "White", "Minority")), coef, color = type, label = coef)) + 
  geom_point() + 
  geom_text(aes(label = round(coef, digit = 2)), nudge_x = 0.25) +
  geom_hline(yintercept = 22.951564, color = "darkgrey", linetype = "longdash") + 
  geom_pointrange(aes(ymin = lower, ymax = upper)) +
  scale_x_discrete(limits=rev) + 
  ggtitle("Heterogeneous Effects of Friends' Vaccination") +
  xlab("Group") + ylab("Coefficient Estimates") + 
  theme(legend.position = "none") +
  coord_flip()

# regression table
table.heter.reg <- 
  stargazer(m3, h20, h21, h30, h31, h32, h40, h41,
            column.labels = c("main", "Male", "Female", "Under 18", "19-39", 
                              "Above 40", "White", "Minority"),
            keep = c("friendvax_lag"),
            dep.var.labels.include = FALSE,
            no.space = TRUE,
            font.size = "small",
            column.sep.width = "3pt",
            omit.stat = c("f", "ll"),
            add.lines=list(c("State FE           & Yes & Yes & Yes & Yes & Yes & Yes & Yes & Yes & Yes"),
                           c("Week FE            & Yes & Yes & Yes & Yes & Yes & Yes & Yes & Yes & Yes"),
                           c("Individual Control & Yes & Yes & Yes & Yes & Yes & Yes & Yes & Yes & Yes"),
                           c("Network Control    & Yes & Yes & Yes & Yes & Yes & Yes & Yes & Yes & Yes")),
            title = "Heterogeneous effects of social network on vaccination",
            type = "text",
            out = "Results/Regression/heter_subgroup_v0528")


# 2. Cross State --------------------------------------------------------

# across states

m3.b <- summary(m3)$coefficients["friendvax_lag1",][[1]]
m3.se <- summary(m3)$coefficients["friendvax_lag1",][[2]]
df.coefplot.state <- data.frame("All", m3.b, m3.b - 1.96*m3.se, m3.b + 1.96*m3.se)

for (state in kStatesAbb) {
  cat(state)
  h <- glm(vax ~ friendvax_lag1 +
             friendnum + friendnum_US + 
             factor(gender) + factor(age) + factor(race) + 
             factor(week), 
           family = binomial("logit"), data = df.heter[df.heter$admin1Abb == state])
  
  h.b <- summary(h)$coefficients["friendvax_lag1",][[1]]
  h.se <- summary(h)$coefficients["friendvax_lag1",][[2]]
  df.coefplot.state <- rbind(df.coefplot.state, c(state, h.b, h.b - 1.96*h.se, h.b + 1.96*h.se))
}

# adjustment
colnames(df.coefplot.state) <- c("state", "coef", "lower", "upper")
df.coefplot.state$coef <- as.numeric(df.coefplot.state$coef)
df.coefplot.state$lower <- as.numeric(df.coefplot.state$lower)
df.coefplot.state$upper <- as.numeric(df.coefplot.state$upper)

# real map
df.ceof.map <- df.coefplot.state[!(df.coefplot.state$state == "All"),]
png("Results/Description/Map/heter_effects_0528.png")
plot_usmap(data = df.ceof.map, 
           values = "coef", color = "grey") + 
  scale_fill_gradientn(colours=c("blue","white", "red"), na.value = "grey98",
                       limits = c(-70, 70)) + 
  labs(title = "Spillover Effects, by state") + 
  theme(legend.position = "right")
dev.off()

# red/blue states

# 2020 Nov election
red.state <- c("AK", "AL", "AR", "FL", "IA", "ID", "IN", "KS", "KY", "LA", "MO", 
               "MS", "MT", "NC", "ND", "NE", "OH", "OK", "SC", "SD", "TN", "TX", 
               "UT", "WV", "WY")

blue.state <- c("AZ", "CA", "CO", "CT", "DC", "DE", "GA", "HI", "IL", "MA", 
                "MD", "ME", "MI", "MN", "NH", "NJ", "NM", "NV", "NY", "OR", 
                "PA", "RI", "VA", "VT", "WA", "WI")

# assign variable
df.coefplot.state$vote <- ifelse(df.coefplot.state$state %in% red.state, "red", "blue")

ggplot(df.coefplot.state, 
       aes(x = reorder(state,-coef), y = coef, color = vote)) + 
  geom_point() + 
  geom_hline(yintercept = 0, color = "darkgrey") +
  geom_vline(xintercept = "All", color = "darkgrey", linetype = "longdash") +
  geom_text(aes(x="WA", label="ALL", y=60), colour="darkred", angle=90) + 
  geom_pointrange(aes(ymin = lower, ymax = upper)) +
  scale_x_discrete(limits=rev) + 
  theme(axis.text.x=element_text(angle=60, hjust=1.5)) +
  scale_color_manual(values = c("#00BFCA", "#F8766D")) + 
  ggtitle("Heterogeneous Effects Cross States") +
  xlab("State") + ylab("Coefficient Estimates")

# create dummy
df.heter$blue <- ifelse(df.heter$admin1Abb %in% red.state, 0, 
                        ifelse(df.heter$admin1Abb %in% blue.state,1, NA))

h5 <- glm(vax ~ friendvax_lag1 * blue +
            friendnum + friendnum_US + 
            factor(gender) + factor(age) + factor(race) + 
            factor(week) + factor(admin1Abb), 
          family = binomial("logit"), data = df.heter)
summary(h5)

# for plots
h50 <- glm(vax ~ friendvax_lag1 +
             friendnum + friendnum_US + 
             factor(gender) + factor(age) + factor(race) + 
             factor(week) + factor(admin1Abb), 
           family = binomial("logit"), data = df.heter[df.heter$blue == 0])
h51 <- glm(vax ~ friendvax_lag1 +
             friendnum + friendnum_US  + 
             factor(gender) + factor(age) + factor(race) + 
             factor(week) + factor(admin1Abb), 
           family = binomial("logit"), data = df.heter[df.heter$blue == 1])

# h50.b <- summary(h50)$coefficients["friendvax_lag1",][[1]]
# h50.se <- summary(h50)$coefficients["friendvax_lag1",][[2]]
# df.coefplot <- rbind(df.coefplot, c("Politics", "Red State", h50.b, h50.b - 1.96*h50.se, h50.b + 1.96*h50.se))
# 
# h51.b <- summary(h51)$coefficients["friendvax_lag1",][[1]]
# h51.se <- summary(h51)$coefficients["friendvax_lag1",][[2]]
# df.coefplot <- rbind(df.coefplot, c("Politics", "Blue State", h51.b, h51.b - 1.96*h51.se, h51.b + 1.96*h51.se))


# 3. Cross Time --------------------------------------------------------

m3.b <- summary(m3)$coefficients["friendvax_lag1",][[1]]
m3.se <- summary(m3)$coefficients["friendvax_lag1",][[2]]
df.coefplot.week <- data.frame(0, m3.b, m3.b - 1.96*m3.se, m3.b + 1.96*m3.se)

for (t in 7:33) {
  print(t)
  h <- glm(vax ~ friendvax_lag1 +
             friendnum + friendnum_US + 
             factor(gender) + factor(age) + factor(race) + 
             factor(admin1Abb), 
           family = binomial("logit"), data = df.heter[df.heter$week == t])
  
  h.b <- summary(h)$coefficients["friendvax_lag1",][[1]]
  h.se <- summary(h)$coefficients["friendvax_lag1",][[2]]
  df.coefplot.week <- rbind(df.coefplot.week, c(t, h.b, h.b - 1.96*h.se, h.b + 1.96*h.se))
}

# adjustment
colnames(df.coefplot.week) <- c("week", "coef", "lower", "upper")
df.coefplot.week$coef <- as.numeric(df.coefplot.week$coef)
df.coefplot.week$lower <- as.numeric(df.coefplot.week$lower)
df.coefplot.week$upper <- as.numeric(df.coefplot.week$upper)

# plot
ggplot(df.coefplot.week[df.coefplot.week$week>8,], 
       aes(x = week, y = coef)) + 
  geom_point() + 
  geom_hline(yintercept = 22.951564, color = "darkgrey", linetype = "longdash") +
  geom_text(aes(x=33, label="ALL", y=34), colour="darkred", angle=0) + 
  geom_hline(yintercept = 0, color = "darkgrey") +
  geom_pointrange(aes(ymin = lower, ymax = upper)) +
  ggtitle("Heterogeneous Effects Along Time") +
  xlab("Week") + ylab("Coefficient Estimates")

# check for how the correlation is gone for Jan. 2021

df.early <- df.heter[df.heter$week %in% c(8, 9, 10, 11),]

h.early1 <- glm(vax ~ friendvax_lag1,
                family = binomial("logit"), data = df.early)

h.early2 <- glm(vax ~ friendvax_lag1 + factor(admin1Abb), 
                family = binomial("logit"), data = df.early)

h.early3 <- glm(vax ~ friendvax_lag1 + 
                  friendnum + friendnum_US + 
                  factor(gender) + factor(age) + factor(race) + 
                  factor(admin1Abb), 
                family = binomial("logit"), data = df.early)

h.early4 <- glm(vax ~ friendvax_lag1 +
                  factor(gender) + factor(age) + factor(race) + 
                  factor(admin1Abb), 
                family = binomial("logit"), data = df.early)



h.early <- glm(vax ~ friendvax_lag1 +
                 friendnum + friendnum_US + 
                 factor(gender) + factor(age) + factor(race) + 
                 factor(admin1Abb), 
               family = binomial("logit"), data = df.early)

table.main.reg <- 
  stargazer(h.early1, h.early2, h.early3, h.early4, h.early,
            keep = c("friendvax_lag"),
            dep.var.labels.include = FALSE,
            no.space = TRUE,
            font.size = "small",
            column.sep.width = "3pt",
            omit.stat = c("f", "ll"),
            add.lines=list(c("State FE           &     & Yes & Yes & Yes & Yes & Yes & Yes & Yes & Yes"),
                           c("Week FE            &     &     &     &     &     & Yes & Yes & Yes & Yes"),
                           c("Individual Control &     &     & Yes &     & Yes & Yes & Yes & Yes & Yes"),
                           c("Network Control    &     &     &     & Yes & Yes & Yes & Yes & Yes & Yes")),
            title = "The effect of social network on vaccination (Jan. 2021)",
            type = "text",
            out = "Results/Regression/supplement_earlyweeks_20230529")




# 4. Social Network Characteristics -------------------------------

# a. below half US friend; above half US friend 

# US friend weights
df.heter$friendw_US <- df.heter$friendnum_US / df.heter$friendnum
df.wide$friendw_US <- df.wide$friendnum_US / df.wide$friendnum

melt(df.wide[ , c("friendnum_US", "friendnum")]) %>%
  ggplot(aes(x = value, fill = variable)) + 
  geom_density(color = "#e9ecef", alpha = 0.8)

# dummies indicating below or above 50%
describe(df.wide$friendw_US, na.rm = TRUE)                                      # mean 0.5, median 0.52
df.heter$friendw_US <- ifelse(df.heter$friendw_US > 0.5, 1, 0)
describe(df.heter$friendw_US, na.rm = TRUE) 

# for plots
f10 <- glm(vax ~ friendvax_lag1 +
             friendnum + friendw_US + 
             factor(gender) + factor(age) + factor(race) + 
             factor(week) + factor(admin1Abb), 
           family = binomial("logit"), data = df.heter[df.heter$friendw_US == 0])
f11 <- glm(vax ~ friendvax_lag1 +
             friendnum + friendw_US + 
             factor(gender) + factor(age) + factor(race) + 
             factor(week) + factor(admin1Abb), 
           family = binomial("logit"), data = df.heter[df.heter$friendw_US == 1])

stargazer(f10,f11, keep = c("friendvax_lag"), type = "text")


# b. friends from geographically closer states, socially closer states




# c. mutual friendship

df.mutual <- readRDS('Data/Final Data/mutual-friend-state-sample_long.rds')
setDT(df.mutual)[, friendvax_lag1 := lag(friendvax), user_id]

f3 <- glm(vax ~ friendvax_lag1 +
            friendnum + friendnum_US + 
            factor(gender) + factor(age) + factor(race) + 
            factor(week) + factor(admin1Abb), 
          family = binomial("logit"), data = df.mutual)

# d. celebrity or not

df.nocel <- readRDS('Data/Final Data/non-celebrity-friend-state-sample_long.rds')
df.cele <- readRDS('Data/Final Data/celebrity-friend-state-sample_long.rds')

setDT(df.nocel)[, friendvax_lag1 := lag(friendvax), user_id]
setDT(df.cele)[, friendvax_lag1 := lag(friendvax), user_id]

f40 <- glm(vax ~ friendvax_lag1 +
             friendnum + friendnum_US + 
             factor(gender) + factor(age) + factor(race) + 
             factor(week) + factor(admin1Abb), 
           family = binomial("logit"), data = df.nocel)

f41 <- glm(vax ~ friendvax_lag1 +
             friendnum + friendnum_US + 
             factor(gender) + factor(age) + factor(race) + 
             factor(week) + factor(admin1Abb), 
           family = binomial("logit"), data = df.cele)

# output add to the previous coefplot

f10.b <- summary(f10)$coefficients["friendvax_lag1",][[1]]
f10.se <- summary(f10)$coefficients["friendvax_lag1",][[2]]
df.coefplot <- rbind(df.coefplot, c("Network1", "Low USfriend", f10.b, f10.b - 1.96*f10.se, f10.b + 1.96*f10.se))

f11.b <- summary(f11)$coefficients["friendvax_lag1",][[1]]
f11.se <- summary(f11)$coefficients["friendvax_lag1",][[2]]
df.coefplot <- rbind(df.coefplot, c("Network1", "High USfriend", f11.b, f11.b - 1.96*f11.se, f11.b + 1.96*f11.se))

f3.b <- summary(f3)$coefficients["friendvax_lag1",][[1]]
f3.se <- summary(f3)$coefficients["friendvax_lag1",][[2]]
df.coefplot <- rbind(df.coefplot, c("Network1", "Mutual Friends", f3.b, f3.b - 1.96*f3.se, f3.b + 1.96*f3.se))

f40.b <- summary(f40)$coefficients["friendvax_lag1",][[1]]
f40.se <- summary(f40)$coefficients["friendvax_lag1",][[2]]
df.coefplot <- rbind(df.coefplot, c("Network1", "Low USfriend", f40.b, f40.b - 1.96*f40.se, f40.b + 1.96*f40.se))

f41.b <- summary(f41)$coefficients["friendvax_lag1",][[1]]
f41.se <- summary(f41)$coefficients["friendvax_lag1",][[2]]
df.coefplot <- rbind(df.coefplot, c("Network1", "High USfriend", f41.b, f41.b - 1.96*f41.se, f41.b + 1.96*f41.se))

