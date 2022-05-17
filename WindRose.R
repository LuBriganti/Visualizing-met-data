#---
#title: "WindRose"
#author: "Lucia Briganti"
#date: "11/7/2021"
#---

## Visualize meteorological data!

Easy codee for the visualization of wind variable and analyzing any anomalies. 

# upload libraries and dataset

library(openair)
library(readxl)

#First I Pick up the data (this file was provided by the National Weather Service and contains information about meteorological variables from an airport station).



myData <- read_excel("C:/Users/Lucia/Desktop/TESIS/Datos/data meteo/dataint_meteo_2010_2020.xlsx")
myData <- na.omit(myData)
head(myData)


#I have hourly data, so I will add a date column and round WS (wind speed) digits, this will be useful for the wind rose.



myData$date <- as.Date(with(myData, paste(year, month, day,sep="-")), "%Y-%m-%d")
myData$WS <- round(myData$WS, digits = 2)
head(myData)



#I want to visualize the historical wind pattern during winter in the city of Buenos Aires and compare it with the 2020 winter wind pattern, so for that I am plotting a wind rose for the last ten years (2010-2019) and another one for 2020.
#So I just subset up WS (wind speed) and WD (wind direction) from the dataframe.
#windRose() is a function of the openair package, very easy to use and aesthetic too.

# Subsetting the data

data_climatica <- selectByDate(myData, start = "2010/01/21", end = "2019/12/31" , month = 6:8 )
WR_climatica <- windRose(data_climatica, ws = "WS", wd = "WD" , ws.int =2 , breaks = c(0, 2, 4, 6),width = 0.6,annotate = FALSE, cols = "default", auto.text = FALSE, grid.line = 8, key.header = "Climatology" , key.footer = "winter 2010-2019")



data_2020 <- selectByDate(myData, start = "2020/01/01", end = "2020/12/30" , month = 6:8 )
WR_2020 <- windRose(data_2020, ws = "WS", wd = "WD" , ws.int =2 , breaks = c(0, 2, 4, 6),width = 0.6,annotate = FALSE, cols = "default", auto.text = FALSE, grid.line = 8, key.header = "Episode" , key.footer = "winter 2020")


plot(WR_2020)
plot(WR_climatica)


#This is a qualitative analyse very useful when you are looking for wind anomalies in the wind and wanna do it quickly, it worked for me in my thesis and when analyzing a particular event. 