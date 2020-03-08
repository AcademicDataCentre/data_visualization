setwd("C:/R_ggplot")
getwd()
install.packages("dplyr",dependencies = TRUE)
library(dplyr)
install.packages("tidyverse",dependencies = TRUE)
library(tidyverse)
install.packages("ggplot2", dependencies = TRUE)
library(ggplot2)
install.packages("reshape2",dependencies = TRUE)
library(reshape2)
install.packages("wesanderson")
library(wesanderson)
# See all palettes
names(wes_palettes)
wes_palette("BottleRocket2")
install.packages("gifski",dependencies = TRUE)
install.packages("ggthemes",dependencies = TRUE)
install.packages("gganimate",dependencies = TRUE)
install.packages("scales",dependencies = TRUE)

library(gifski)
library(ggthemes)
library(gganimate)
library(scales)


# load the data

Confirmed <- read.csv("time_series_19-covid-Confirmed.csv")
View(Confirmed)

# alternatively load data directly from GitHub

Confirmed <- read.csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Confirmed.csv", header = TRUE)

# convert data from wide to long 

Confirmed<-Confirmed[,c(-1,-3,-4)]%>%melt(id="Country.Region")
View(Confirmed)

# Aggregate data by country and day

Confirmed1<-Confirmed%>%group_by(Country.Region,variable)%>%summarise(Cases=sum(value))
View(Confirmed1)

#Rename columns

colnames(Confirmed1)<-c("Country","Date","Cases")
View(Confirmed1)

#Covert factors to date format

Confirmed1$Date<-str_replace(Confirmed1$Date,"X","")
Confirmed1$Date<-gsub("[[:punct:]]","/",Confirmed1$Date)
Confirmed1$Date<-as.Date(Confirmed1$Date,"%m/%d/%y")

#Select the country 

Confirmed1_subset<-Confirmed1%>%filter(Country=="US"|Country=="Canada"|Country=="Italy"|Country=="Mainland China"|Country=="Iran"|Country=="South Korea")
View(Confirmed1_subset)

# Create a static plot 


lastDate<-max(Confirmed1_subset$Date)
ggplot(Confirmed1_subset, aes(x=Date, y=Cases, group=Country, color=Country)) +
  geom_line() +
  geom_point() + 
  scale_y_log10("Number of Confirmed Cases", breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x)))+
  scale_x_date("Day", date_breaks ="1 week", date_labels = "%b %d")+
  ggtitle(paste("Cumulative cases over time as of ",lastDate)) +
  theme_classic() + 
  theme(legend.position = c(0.13, 0.75)) 


# write ggplot code to a new data frame

confirmed_plot <- ggplot(Confirmed1_subset, aes(x=Date, y=Cases, group=Country, color=Country)) +
  geom_line() +
  geom_point() + 
  scale_y_log10("Number of Confirmed Cases", breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x)))+
  scale_x_date("Day", date_breaks ="1 week", date_labels = "%b %d")+
  ggtitle(paste("Cumulative cases over time as of ",lastDate)) +
  theme_classic() + 
  theme(legend.position = c(0.13, 0.75))

# plot and print and save


plot(confirmed_plot)

print(confirmed_plot)

ggsave("C:/R_ggplot/myplot.pdf", plot=confirmed_plot)

# Create an animation gif file

p <- ggplot(Confirmed1_subset, aes(x=Date, y=Cases, group=Country, color=Country)) +
  geom_line() +
  geom_point() + 
  geom_text(aes(x = max(Date)+.1, label = sprintf("%5.0f", Cases)), hjust=-0.5) +
  transition_reveal(Date) + 
  view_follow(fixed_y = TRUE)+
  coord_cartesian(clip = 'off') +
  scale_y_log10("Number of Confirmed cases", breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x)))+
  scale_x_date("Day", date_breaks ="1 week", date_labels = "%b %d")+
  enter_drift(x_mod = -1) + exit_drift(x_mod = 1) +
  theme_classic() + scale_color_manual(values=wes_palette("IsleofDogs1", 6)) +
  theme(legend.position = c(0.13, 0.88),
  plot.margin = margin(5.5, 40, 5.5, 5.5))

animate(p, fps=5,renderer = gifski_renderer("COVID-19.gif"))
