#Analyze_inner_characteristics.R
#Chen Keyu - 2022.10.9

#install.packages("usmap")

library(pacman)
p_load(dplyr,usmap,ggplot2,tidyverse,iotools,data.table,haven,knitr,psych,stats4,stargazer,
       geojsonio,RColorBrewer,rgdal,broom,rgeos,magrittr,qwraps2,Jmisc,fastDummies,corrplot,
       csv,lmtest,vtable,purrr)

setwd("~/Social Network and Vaccination")

## Individual Level
df <- readRDS("Data/Final Data/Inner Friends/inner_friends_weight.rds")
df_vax <- df[c(1,4:36)]
df_vax$week <- rowSums(df_vax[2:34])
df_vax$week <- 33- df_vax$week
df_hesitancy <- df_vax[c(1,35)]
colnames(df_hesitancy)[2]<-"hesitancy"

df_hesitancy$week <- df_hesitancy[2]-1
df_friendvax <- df[c(1,48:80)]

df_scatter_plot <- merge(df_friendvax,df_hesitancy)
for(i in 1:nrow(df_scatter_plot)){
  for (j in 1:36){
    if(df_scatter_plot[i,36]==j){df_scatter_plot[i,37]=df_scatter_plot[i,j+1]}}}

colnames(df_scatter_plot)[37] <-"friend_vax"
df_scatter_plot2<-subset(df_scatter_plot, df_scatter_plot[37] < 0.25)

# Scatter Plot: Vaccine Hesitancy // Friend Vax
png("Results/Description/Scatter_Plot_vax-hesitancy-inner-friend.png")
ggplot(df_scatter_plot2, aes(friend_vax, hesitancy, color = friend_vax > 0.05))+
  geom_smooth(method="gam") +
  scale_x_log10()
  xlab("Share of Friend Vaccinated") + 
  ylab("Vaccine Hesitancy")+
  labs(title="Vaccine Hesitancy, by state") + 
  theme(legend.position = "bottom", legend.background = element_rect(color = "black",fill = "grey90", size = 1, linetype = "solid"), legend.direction = "horizontal")+
  scale_fill_discrete(name = "Share of Friend Vaccinated", labels = c("<0.0125", ">0.0125"))
dev.off()

## State Level
df_hesitancy <- aggregate(df_vax$week, by=list(state=df$admin1), FUN=mean,na.rm=TRUE, na.action=NULL)
colnames(df_hesitancy)[2]<-"hesitancy"
df_hesitancy$week <- ceiling(df_hesitancy[2])-1

df_friendvax <- df[c(1,48:80)]
df_friendvax <- aggregate(df_friendvax[,2:34], by=list(state=df$admin1), FUN=mean,na.rm=TRUE, na.action=NULL)

df_scatter_plot <- merge(df_friendvax,df_hesitancy)

df_scatter_plot$friend_vax <- 0

for(i in 1:nrow(df_scatter_plot)){
  if(df_scatter_plot[i,36]==20){df_scatter_plot[i,37]=df_scatter_plot[i,21]}
    else if(df_scatter_plot[i,36]==21){df_scatter_plot[i,37]=df_scatter_plot[i,22]}
      else if(df_scatter_plot[i,36]==22){df_scatter_plot[i,37]=df_scatter_plot[i,23]}
        else {df_scatter_plot[i,36]=df_scatter_plot[i,24]}}


# Scatter Plot: Vaccine Hesitancy // Friend Vax
png("Results/Description/Scatter_Plot2.png")
ggplot(df_scatter_plot, aes(friend_vax, hesitancy, color = friend_vax > 0.0125))+
  geom_point()+
  stat_ellipse(type = "norm", linetype = 2,show.legend = TRUE) +
  xlab("Share of Friend Vaccinated") + 
  ylab("Vaccine Hesitancy")+
  labs(title="Vaccine Hesitancy, by state") + 
  theme(legend.position = "bottom", legend.background = element_rect(color = "black",fill = "grey90", size = 1, linetype = "solid"), legend.direction = "horizontal")+
  scale_fill_discrete(name = "Share of Friend Vaccinated", labels = c("<0.0125", ">0.0125"))

dev.off()
