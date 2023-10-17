#Clean_SCI.R
#Chen Keyu 2022.11.17 | Shuai Yinzhong 2022.11.18

### Setup ----

# library manager
library(pacman)

# packages to use load them now using the pacman "manager"
p_load(tidyverse, iotools, data.table, haven, knitr, psych, stats4, stargazer, magrittr, 
       qwraps2, Jmisc, fastDummies, corrplot, csv, skedastic, lmtest, broom, usmap,
       car, sandwich, vtable, readxl, data.table, disprose, stringr,covidcast)

# working directory ----
setwd("~/Social Network and Vaccination")


# Generate SCI state ----
## Population matched with FIP Codes
pop <- read_excel("Data/Region/co-est2021-pop.xlsx", skip = 4)
pop <- pop[c(1,4)]
colnames(pop) <- c("County","Population")
pop <- pop %>% separate(County, c("County", "State"), ",")
pop <- pop %>% mutate_at("County", str_replace, ".", "")
pop <-as.data.frame(apply(pop,2, str_remove_all, " "))
pop <- pop[1:3143,] %>% mutate_at("County", str_replace, "County", "")

fip <- read_excel("Data/Region/ozone-county-population.xlsx")
fip <- fip[1:3143,1:4]
colnames(fip) <- c("StateFips","CountyFips","State","County")
fip <- fip %>% mutate_at("County", str_replace, "County", "")
fip = as.data.frame(fip)

df<-cbind(fip,pop)
df<- df[c(1:4,7)]
df[1] <- lapply(df[1], as.numeric)
df[5] <- lapply(df[5], as.numeric)
df$Fips <- paste(df$StateFips,df$CountyFips,sep="")
df$Fips <- as.numeric(df$Fips)


## Population weight within state
df <- df %>%
  group_by(StateFips) %>%
  mutate(Percentage = Population/sum(Population))
saveRDS(df, "Data/Temp/SCI/county_population_proportion.rds")


## Weights
sci <- read.table("Data/SCI/county_county.tsv", sep = "\t", header = TRUE)
sci$user_state <- sci$user_loc %/% 1000
sci$fr_state <- sci$fr_loc %/% 1000


# df$Fips <- as.numeric(df$Fips) # 
for (i in 1:3143){
  sci$user_perc [sci$user_loc==df$Fips[i]] <- df$Percentage[i]
}
for (i in 1:3143){
  sci$fr_perc [sci$fr_loc==df$Fips[i]] <- df$Percentage[i]
}


## Aggregate by state
sci$weighted_sci <- sci$scaled_sci * sci$user_perc * sci$fr_perc
sci_state <- aggregate(sci$weighted_sci, by=list(user_loc=sci$user_state, fr_loc=sci$fr_state), FUN=sum)


# Correspond state FIPS with abbr
saveRDS(sci_state, "Data/Final Data/SCI_state.rds")
sci_state <- readRDS("Data/Final Data/SCI_state.rds")
sci_state$user_loc <- sprintf("%02d", as.numeric(sci_state$user_loc))
sci_state$fr_loc <- sprintf("%02d", as.numeric(sci_state$fr_loc))
sci_state$user_loc[which(sci_state$user_loc=="02")] <- "002"
sci_state$user_loc[which(sci_state$user_loc=="01")] <- "02"
sci_state$user_loc[which(sci_state$user_loc=="002")] <- "01"
sci_state$fr_loc[which(sci_state$fr_loc=="02")] <- "002"
sci_state$fr_loc[which(sci_state$fr_loc=="01")] <- "02"
sci_state$fr_loc[which(sci_state$fr_loc=="002")] <- "01"
sci_state$user_loc <- fips_to_abbr(sci_state$user_loc) 
sci_state$fr_loc <- fips_to_abbr(sci_state$fr_loc) 
colnames(sci_state)[3] <- "sci_index"
saveRDS(sci_state, "Data/Final Data/SCI_state.rds")


# Weight Vaxrate by SCI and log(SCI) ----
statesAbb <- c("AK", "AL", "AR", "AZ", "CA", "CO", "CT", "DC", "DE", "FL", 
               "GA", "HI", "IA", "ID", "IL", "IN", "KS", "KY", "LA", "MA", 
               "MD", "ME", "MI", "MN", "MO", "MS", "MT", "NC", "ND", "NE", 
               "NH", "NJ", "NM", "NV", "NY", "OH", "OK", "OR", "PA", "RI", 
               "SC", "SD", "TN", "TX", "UT", "VA", "VT", "WA", "WI", "WV", "WY")
vaxrate_n <- paste0("vaxrate.", 5:33)
sci_state <- sci_state[sci_state$user_loc %in% statesAbb & sci_state$fr_loc %in% statesAbb, ]
sci_state$sci_log <- log(sci_state$sci_index)

df_vaxrate <- readRDS("Data/Temp/df_vaxrate_state.rds")
rownames(df_vaxrate) <- df_vaxrate$Location
df_vaxrate <- df_vaxrate[df_vaxrate$Location %in% statesAbb]
df_vaxrate <- df_vaxrate[order(df_vaxrate$Location),]

df <- merge(sci_state, df_vaxrate, by.x = "fr_loc", by.y = "Location")
for (i in vaxrate_n) {
  df[, paste0("weighted", i)] <- df[, "sci_index"] * df[, i]
}
for (i in vaxrate_n) {
  df[, paste0("log_weighted", i)] <- df[, "sci_log"] * df[, i]
}
df <- df[c(2,34:91)]
sci_vaxrate <- aggregate(.~user_loc, df, FUN=sum)
saveRDS(sci_vaxrate, "Data/Temp/SCI/sci_vaxrate.rds")

## Merge weighted vaxrate with user data ----
df_uservax <- readRDS("Data/Temp/users_vaxtime.rds")
df_demo <- readRDS("Data/Temp/users_demography.rds")

df_state_sci <- merge(df_uservax, sci_vaxrate, by.x = "admin1Abb", by.y = "user_loc", all.x = TRUE)
df_state_sci <- merge(df_state_sci, df_demo, all.x = TRUE)

df_state_sci <- df_state_sci %>% relocate(user_id,gender,age,race)
saveRDS(df_state_sci,file = 'Data/Final Data/state-sample-sci-123999.rds')

