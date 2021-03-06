
# merge three data frames by columns

merged_data <- cbind.data.frame(Confirmed1_subset, Recovered1_subset, Deaths1_subset)
View(merged_data)

# change names of variables to get unique names

names(merged_data) [1:9] = c("Country", "Date", "Cases Confirmed", "Country", "Date", "Cases Recovered", "Country", "Date", "Cases Deaths")

# remove duplicate columns

merged_data<-merged_data[,c(-4,-5,-7, -8)]
View(merged_data)

# filter by last date for each country

totalcases <- subset.data.frame(merged_data, merged_data$Date == "2020-03-08", na.rm=TRUE)
View(totalcases)

# make wide data long

totalcases<-totalcases[,c(-2)]%>%melt(id="Country")

# rename columns

colnames(totalcases)<-c("Country","Status","Cases")
View(totalcases)

#create stacked bar graph
# geom_bar is a bar graph representation. height of bars proportional to number of cases
# fill aesthetic = fill colour of object

bargraph <-ggplot(totalcases) + 
aes(x=Country, y = Cases, fill=Status) +  
geom_bar(stat="identity") + 
scale_fill_manual(values=wes_palette("Darjeeling1", 3)) + 
theme(axis.text.x=element_text(angle = -45, hjust = 0)) +
labs(title = "COVID-19 Confirmed, Recovered, Deaths x Country, 20-03-07") + 
scale_y_log10("Number of Confirmed Cases", breaks = trans_breaks("log10", function(x) 10^x),
              labels = trans_format("log10", math_format(10^.x))) 

# plot bargraph

plot(bargraph)

# save plot

ggsave("C:/R_ggplot/mybargraph.pdf", plot=bargraph)
                 
                 
                 
                 
                 
                 
                 
                 