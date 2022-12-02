library(readxl)
library(dplyr)
library('writexl')     

map(dates2, possibly(testFunction, NA))


# Data Prep - Combination Calculation

parks = read_excel("C:/Users/eceak/National Parks Route/data/National Park Coordinates.xlsx")

origin = parks$`Place Name`
destination = parks$`Place Name`

combinations = expand.grid(origin, destination)


combinations = combinations %>%
  rename(from = Var1,
         to = Var2)

DF <- subset(combinations, combinations$from != combinations$to)

# Driving Distance - Time Between All Possible Points
## install.packages("ggmap")
library(ggmap)

register_google(key = "Enter_Key", write = TRUE)


DF$from <- as.character(DF$from) # mapdist demands input to be character type
DF$to <- as.character(DF$to)     # mapdist demands input to be character type
# remove (from, to) #remove input to avoid confusion

DF$row.number <- 1:nrow(DF)

for (i in DF$row.number){
  orig <- DF[i,c('from')] # get origin from DF in the position line 'i', column 'from'
  dest <- DF[i,c('to')]   # get origin from DF in the position line 'i', column 'to'
  a <- mapdist(from = orig, to = dest, mode = "driving",output = "simple") # create temp. df 'a' with the output from mapdist
  a$row.number <- i # include in temp. df 'a' the index number of the row from DF
  DF$minutes[match(a$row.number, DF$row.number)] <- a$minutes # use the index number as a matching key to input/update the value of the variable 'minutes'
  DF$hours[match(a$row.number, DF$row.number)] <- a$hours # ibdem DF$km[match(a$row.number, DF$row.number)] <- a$km #ibdem
  DF$miles[match(a$row.number, DF$row.number)] <- a$miles # ibdem
}

write_xlsx(DF, 'route_data.xlsx')