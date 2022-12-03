library(readxl)
library(dplyr)
library(tidyr)
library(writexl)     
library(reshape2)


parks = read_excel("C:/Users/eceak/National Parks Route/data/route_data.xlsx")  # read data
 
parks_data = data.frame(parks$from, parks$to, parks$miles) # subset


parks_matrix = acast(parks_data, parks.from~parks.to, value.var="parks.miles") # create matrix

parks_matrix_df = as.data.frame(parks_matrix) # convert to df

write_xlsx(parks_matrix_df, 'parks_matrix.xlsx') # save data as excel
