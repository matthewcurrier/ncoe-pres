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

accs <- separate(accs, time, c("hour", "minute"), sep=":")
accs$hour <- as.numeric(accs$hour)

#select some variables
acc2 <- accs %>% select(date, 
                        hour,
                        borough, 
                        number_of_cyclist_injured, 
                        number_of_cyclist_killed, 
                        contributing_factor_vehicle_1, 
                        contributing_factor_vehicle_2) 

#Do some early aggregation
acc3 <- acc2 %>% group_by(date,
                          hour,
                          borough,
                          contributing_factor_vehicle_1, 
                          contributing_factor_vehicle_2) %>% summarise(n_injured=sum(number_of_cyclist_injured), 
                                                                       n_killed=sum(number_of_cyclist_killed)
                          ) %>% ungroup()

#Remove any rows where both numeric cols are 0
acc4 <- acc3 %>% filter(n_injured!=0 | n_killed!=0)

#Create base for chart
b <- ggplot(acc4, aes(hour)) + geom_histogram(binwidth = 1, color="grey", fill="red") + theme_simple
b <- b +   labs(title="At what time of day do most bike accidents occur?",
           subtitle="Accidents occur steadily throughout the day, culminating during the evening rush hour.",
           x="Hour of Day",
           y="Count of Accidents")

ggsave("output/histogram-time-of-day.png", b, dpi=600)
