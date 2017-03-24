###Installing required packages and loading the libraries
install.packages("lubridate")
install.packages("corrplot")
install.packages("maps")
install.packages("dplyr")
install.packages("data.table")
library(dplyr)
library(data.table)
library(data.table)
library(ggplot2)
library('lubridate')
library('corrplot')
library(maps)

###Loading the data set 
dataset<-fread("/Users/vipulvijigiri/Downloads/database.csv")

###DATA CLEANING
###The original dataset has a column Type=Earthquake which is made null
dataset$Type<-NULL
###Removed the columns which have NULL values
dataset$`Depth Error`<-NULL
dataset$`Depth Seismic Stations`<-NULL
dataset$`Azimuthal Gap`<-NULL
dataset$`Horizontal Distance`<-NULL
dataset$`Horizontal Error`<-NULL
dataset$`Root Mean Square`<-NULL

###storing the year and month values from the Date
dataset$year <- year(dataset$Date)
dataset$month <- month(dataset$Date)

###storing world map in a variable
worldmap <- borders("world", colour="green", fill="blue")

##storing earthquakes plot
quake_plot<-ggplot(dataset)

###combination of world map and earth quakes prone areas
complete_map <- quake_plot+worldmap

### printing earth quake prone areas in world map
print(complete_map + geom_point(aes(dataset$Longitude,dataset$Latitude,color=Magnitude),shape=20) +
        scale_color_gradient(low="red", high="yellow") +
        ggtitle("Earthquakes Prone areas in world map"))

####Range of Magnitude of earth quakes
ggplot(dataset,aes(Magnitude))+
  geom_density(fill="red")+ggtitle(expression(atop("Earthquakes", atop(italic("Magnitude"), "")))) 

###Range of Depth of earthquakes
ggplot(dataset,aes(Depth))+
  geom_density(fill="red") +ggtitle(expression(atop("Earthquakes", atop(italic("Depth"), "")))) 

###plotting the Range of different magnitude types
ggplot(earthquake_year) + 
  geom_density(aes(x =  Magnitude, colour = `Magnitude Type`, 
                   fill = `Magnitude Type`, alpha = 0.01))

###Plotting type of magnitude that occurs of earth quakes data set on world map
ggplot() + worldmap + 
  geom_point(data = earthquake_year, aes(x = Longitude, y = Latitude, colour = `Magnitude Type` )) +
  ggtitle("Magnitude Type")

###Plotting status of earth quakes on world map 
ggplot() + worldmap + 
  geom_point(data = earthquake_year, 
             aes(x = Longitude, y = Latitude, colour = Status , alpha = 0.3)) +
  ggtitle("Coloured By Status")

###number of earthquakes along last 20 years
dataset$Year <- format(as.Date(dataset$Date, format="%d/%m/%Y"),"%Y")
last20_Earthquakes <- ts(unname(table(dataset$Year)),
                         start=1996, 
                         end=2016,
                         frequency =1)
plot(last20_Earthquakes,color='red')

### Earthquakes in an year and month by magnitude
ggplot(dataset,aes(month,Magnitude))+
  geom_dotplot(color='red')+facet_wrap(~year)+
  ggtitle(expression(atop("Earthquakes by year and Month", atop(italic("Magnitude"), ""))))

### Earthquakes in an year by Mean Magnitude 
ggplot(dataset[,.("Mean_Magnitude"=mean(Magnitude)),by=year],aes(year,Mean_Magnitude))+
  geom_smooth(color='red')+
  ggtitle(expression(atop("Earthquakes by year", atop(italic("Magnitude"), ""))))

### Earthquakes in an year and month by depth
ggplot(dataset,aes(month,Depth))+
  geom_dotplot(color='red')+facet_wrap(~year)+
  ggtitle(expression(atop("Earthquakes by year and Month", atop(italic("Depth"), ""))))

### Earthquakes in an year by Mean depth
ggplot(dataset[,.("Mean_Depth"=mean(Depth)),by=year],aes(year,Mean_Depth))+
  geom_smooth(color='red')+
  ggtitle(expression(atop("Earthquakes by year", atop(italic("Depth"), ""))))

###getting the count of earth quakes every year
earthquake_year <- dataset[, Date := as.Date(Date, format = "%m/%d/%Y")]
earthquake_year <- earthquake_year[, Year := year(Date)]
Year_counts <- earthquake_year %>% 
  select(Year)  %>% 
  group_by(Year)  %>% 
  summarize(count = n())
### storing the above selected attributes into datatable
data.table(Year_counts)
###Plotting the data against count of the earth quake every year
ggplot(Year_counts, aes(x =  Year, y = count ))  + 
  theme(panel.background = element_rect(fill = "black"))+
  geom_point(color='yellow')  + geom_line(color='green')

###getting source of earthquake every year
year_source <- earthquake_year %>% 
  select(Year, Source)  %>% 
  group_by(Year, Source)  %>% 
  summarize(count = n())
### storing the above selected attributes into datatable
data.table(year_source )
###Plotting the data against the source of earth quake every year
ggplot(year_source, aes(x =  Year, y = count, fill = Source,colour = Source, alpha = 1.5))  + 
  theme(panel.background = element_rect(fill = "yellow"))+
  geom_point()  + geom_line()

###getting the magnitude type of earth quakes every year occurring every year 
magnitude_type <- earthquake_year %>% 
  select(Year, `Magnitude Type`) %>% 
  group_by(Year, `Magnitude Type`) %>% 
  summarize(count = n())
### storing the above selected attributes into datatable
data.table(magnitude_type)
###Plotting the data against the magnitude type every year
ggplot(magnitude_type, aes(x =  Year, y = count, fill = `Magnitude Type`, colour = `Magnitude Type`, alpha = 1.5))  + 
  theme(panel.background = element_rect(fill = "yellow"))+
  geom_point()  + geom_line()

###getting the location source of earth quakes every year occurring every year 
Location_source <- earthquake_year %>% 
  select(Year, `Location Source`) %>% 
  group_by(Year, `Location Source`) %>% 
  summarize(count = n())
### storing the above selected attributes into datatable
data.table(Location_source)
###Plotting the data against the location source
ggplot(Location_source, aes(x =  Year, y = count, fill = `Location Source`, colour =`Location Source`, alpha = 1.5))  + 
  theme(panel.background = element_rect(fill = "yellow"))+
  geom_point()  + geom_line()

