library(dplyr)
library(tidyr)
library(xts)
library(devtools)
library(xtsmatt)
library(lubridate)
#devtools::install_github("matthewcurrier/xtsmatt")
#download.file("http://m40z.com/datasets/nyc_bike_accidents.rda", "nyc_bike_accidents.rda")
load("C:/Users/mc/Google Drive/R/Projects/Napa Schools Interview/nyc_bike_accidents.rda")

# char to date
accs$date <- as.Date(accs$date, "%m/%d/%Y")


#select some variables
acc2 <- accs %>% select(date, 
                        borough, 
                        number_of_cyclist_injured, 
                        number_of_cyclist_killed, 
                        contributing_factor_vehicle_1, 
                        contributing_factor_vehicle_2) 

#Do some early aggregation
acc3 <- acc2 %>% group_by(date,
                          borough,
                          contributing_factor_vehicle_1, 
                          contributing_factor_vehicle_2) %>% summarise(n_injured=sum(number_of_cyclist_injured), 
                                                                       n_killed=sum(number_of_cyclist_killed)
                          ) %>% ungroup()

acc3$year <- format(acc3$date, "%Y")
#Remove any rows where both numeric cols are 0
acc4 <- acc3 %>% filter(n_injured!=0 | n_killed!=0)

acc4 <- acc4 %>% filter(date < as.Date("2016-01-01", "%Y-%m-%d"))
