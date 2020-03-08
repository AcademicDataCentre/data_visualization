# load the data
Recovered <- read.csv("time_series_19-covid-Recovered.csv")
View(Recovered)
# convert data from wide to long 
Recovered<-Recovered[,c(-1,-3,-4)]%>%melt(id="Country.Region")
View(Recovered)
# Aggregate data by country and day
Recovered1<-Recovered%>%group_by(Country.Region,variable)%>%summarise(Cases=sum(value))
View(Recovered1)
#Rename columns
colnames(Recovered1)<-c("Country","Date","Cases")
View(Recovered1)
#Covert factors to date format
Recovered1$Date<-str_replace(Confirmed1$Date,"X","")
Recovered1$Date<-gsub("[[:punct:]]","/",Recovered1$Date)
Recovered1$Date<-as.Date(Confirmed1$Date,"%m/%d/%y")
#Select the country 
Recovered1_subset<-Recovered1%>%filter(Country=="US"|Country=="Canada"|Country=="Italy"|Country=="Mainland China"|Country=="Iran"|Country=="South Korea")
View(Recovered1_subset)

# Create a static plot 
lastDate<-max(Recovered1_subset$Date)
ggplot(Recovered1_subset, aes(x=Date, y=Cases, group=Country, color=Country)) +
  geom_line() +
  geom_point() + 
  scale_y_log10("Number of Confirmed Cases", breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x)))+
  scale_x_date("Day", date_breaks ="1 week", date_labels = "%b %d")+
  ggtitle(paste("Cumulative cases over time as of ",lastDate)) +
  theme_classic() + 
  theme(legend.position = c(0.13, 0.75))

# write ggplot code to a new data frame

recovered_plot <- ggplot(Recovered1_subset, aes(x=Date, y=Cases, group=Country, color=Country)) +
  geom_line() +
  geom_point() + 
  scale_y_log10("Number of Recovered", breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x)))+
  scale_x_date("Day", date_breaks ="1 week", date_labels = "%b %d")+
  ggtitle(paste("Cumulative recovered cases over time as of ",lastDate)) +
  theme_classic() + 
  theme(legend.position = c(0.13, 0.75))

# plot


plot(recovered_plot)


# Create an animation gif file

p<-ggplot(1_subset, aes(x=Date, y=Cases, group=Country, color=Country)) +
  geom_line() +
  geom_point() + 
  geom_text(aes(x = max(Date)+.1, label = sprintf("%5.0f", Cases)), hjust=-0.5) +
  transition_reveal(Date) + 
  view_follow(fixed_y = TRUE)+
  coord_cartesian(clip = 'off') +
  scale_y_log10("Number of recovered cases", breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x)))+
  scale_x_date("Day", date_breaks ="1 week", date_labels = "%b %d")+
  enter_drift(x_mod = -1) + exit_drift(x_mod = 1) +
  theme_classic() + scale_color_manual(values=wes_palette("IsleofDogs1", 6)) +
  theme(legend.position = c(0.13, 0.88),
        plot.margin = margin(5.5, 40, 5.5, 5.5))

animate(p, fps=5,renderer = gifski_renderer("COVID-19Recovered.gif"))


