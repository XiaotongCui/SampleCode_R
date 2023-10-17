#Clean_vaxrate.R
#Yinzhong Shuai 2022.10.10

### Setup ----
setwd("~/Social Network and Vaccination")
library(pacman)
p_load(data.table,lubridate,dplr,reshape)

# read the data
state_distribution <- fread("Data/Region/distribution_by_state.csv",
                            select=c("Date","MMWR_week","Location","Administered_Dose1_Pop_Pct","Series_Complete_Pop_Pct"))


state_distribution[[1]] <- mdy(state_distribution[[1]])
state_distribution[[6]] <- wday(state_distribution[[1]])
state_distribution[[7]] <- as.numeric(state_distribution[[1]])

state_distribution$week <- ifelse(state_distribution$MMWR_week<30,
                                  state_distribution$MMWR_week+7,
                                  ifelse(state_distribution$MMWR_week>40,
                                         state_distribution$MMWR_week-46,
                                         NA))

state_distribution$vaxrate <- state_distribution$Administered_Dose1_Pop_Pct/100
state_distribution$vaxrate_2 <- state_distribution$Series_Complete_Pop_Pct/100

# 2 different measurements
week_state_distribution <- subset(state_distribution, V7<=18810 & V6==6,select=c("Location","week","vaxrate"))
week_state_distribution_2 <- subset(state_distribution, V7<=18810 & V6==6,select=c("Location","week","vaxrate_2"))

df_vaxrate <- reshape(week_state_distribution,
                      timevar = "week",
                      idvar = c("Location"),
                      direction = "wide")
df_vaxrate <- rev(df_vaxrate)

df_vaxrate_2 <- reshape(week_state_distribution_2,
                      timevar = "week",
                      idvar = c("Location"),
                      direction = "wide")
df_vaxrate_2 <- rev(df_vaxrate_2)

saveRDS(df_vaxrate, file = "Data/Temp/df_vaxrate_state.rds")
saveRDS(df_vaxrate_2, file = "Data/Temp/df_vaxrate_state_2dose.rds")
