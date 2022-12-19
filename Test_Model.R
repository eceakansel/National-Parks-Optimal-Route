# install.packages("addTiles")
# install.packages("addPolylines")
# install.packages("geosphere")
# install.packages("rgdal")
# install.packages("ompr")
# install.packages("ROI.plugin.glpk")


# LIBRARIES
library(readxl)
library(dplyr)
library(tidyr)
library(readr)     
library(reshape2)
library(rgdal)
library(geosphere)
library(addPolylines)
library(leaflet)
library(ompr)
library(ompr.roi)
library(ROI.plugin.glpk)
devtools::install_github("dirkschumacher/ompr.roi") #or cran version higher than 0.8.0.9



# read route data
parks = read_excel("C:/Users/eceak/National Parks Route/data/route_data.xlsx")  

# read parks coordinates for map - test
Coordinates_test = read_excel("C:/Users/eceak/National Parks Route/data/Coordinates_test.xlsx")  

# clean data and convert to matrix format - dataframe
parks_data = data.frame(parks$from, parks$to, parks$miles) 
parks_data %>% drop_na -> parks_data
parks_matrix = acast(parks_data, parks.from~parks.to, value.var="parks.miles")
distance_matrix = as.data.frame(parks_matrix) 

# test matrix
distance_test = distance_matrix[1:10, 1:10] 
distance_matrix[is.na(distance_matrix)] = 0
distance_test[is.na(distance_test)] = 0



# Model test - 10 parks only

# specify the dimensions of the distance matrix
n <- length(distance_test$`Acadia National Park, Maine, USA`)

# create a distance extraction function
dist_fun <- function(i, j) {
  vapply(seq_along(i), function(k) distance_test[i[k], j[k]], numeric(1L))
}


# build the model
model <- MILPModel() %>%
  # we create a variable that is 1 iff we travel from city i to j
  add_variable(x[i, j], i = 1:n, j = 1:n, 
               type = "integer", lb = 0, ub = 1) %>%
  
  # a helper variable for the MTZ formulation of the tsp
  add_variable(u[i], i = 1:n, lb = 1, ub = n) %>% 
  
  # minimize travel distance
  set_objective(sum_expr(colwise(dist_fun(i, j)) * x[i, j], i = 1:n, j = 1:n), "min") %>%
  
  # you cannot go to the same city
  set_bounds(x[i, i], ub = 0, i = 1:n) %>%
  
  # leave each city
  add_constraint(sum_expr(x[i, j], j = 1:n) == 1, i = 1:n) %>%
  
  # visit each city
  add_constraint(sum_expr(x[i, j], i = 1:n) == 1, j = 1:n) %>%
  
  # ensure no subtours (arc constraints)
  add_constraint(u[i] >= 2, i = 2:n) %>% 
  add_constraint(u[i] - u[j] + 1 <= (n - 1) * (1 - x[i, j]), i = 2:n, j = 2:n)

# print model
model 


result <- solve_model(model, with_ROI(solver = "glpk", verbose = TRUE))

# assign id for the join
distance_test$id <- 1:nrow(distance_test)
distance_test

# map visualization of the route
solution <- get_solution(result, x[i, j]) %>% 
  filter(value > 0)

paths <- select(solution, i, j) %>% 
  rename(from = i, to = j) %>% 
  mutate(trip_id = row_number()) %>% 
  inner_join(distance_test, by = c("from" = "id"))

paths_leaflet <- paths[1,]
paths_row <- paths[1,]

for (i in 1:n) {
  paths_row <- paths %>% filter(from == paths_row$to[1])
  
  paths_leaflet <- rbind(paths_leaflet, paths_row)
}


leaflet() %>% 
  addTiles() %>%
  addMarkers(data = Coordinates_test, ~Longitude, ~Latitude, popup = ~Name, label = ~Name) %>% 
  addPolylines(data = Coordinates_test, ~Longitude, ~Latitude, weight = 2)


