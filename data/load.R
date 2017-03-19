library(dplyr)
library(tidyr)
library(xts)
library(devtools)
library(xtsmatt)
library(lubridate)
#devtools::install_github("matthewcurrier/xtsmatt")
download.file("http://m40z.com/datasets/nyc_bike_accidents.rda", "nyc_bike_accidents.rda")
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

#Remove any rows where both numeric cols are 0
acc4 <- acc3 %>% filter(n_injured!=0 | n_killed!=0)



#Take categorical data and add to rownames
#Do some data munging
acc4 <- acc4 %>% unite(rnames, borough, contributing_factor_vehicle_1, contributing_factor_vehicle_2, sep = "^^^")
acc5 <- acc4 %>% gather(metric, value, n_injured, n_killed)
acc6 <- acc5 %>% unite(rnames, rnames, metric, sep="^^^")
acc7 <- acc6 %>% spread(rnames, value)

#Create an xts object for fast manipulation of data
acc_xts <- xts(acc7, order.by = acc7$date)

###########At this point, you have an xts object upon which you can do some data manipulations


#Let's look at a table of contributing factors
top_causes <- acc3 %>% group_by(contributing_factor_vehicle_1) %>% 
  summarize(total=sum(n_injured) + sum(n_killed)) %>%
  ungroup()

#1 cause is unspecified, but number 2 is driver distraction.
# Let's explore number 2
acc3$year <- as.character(year(acc3$date))

#We'll look at just driver Inattention
inattention_brkout <- acc3 %>% 
  filter(contributing_factor_vehicle_1=="Driver Inattention/Distraction") %>%
  group_by(year) %>% 
  summarize(total=sum(n_injured) + sum(n_killed)) %>%
  ungroup()


#We'll look at just driveer Inattention by borough
inattention_brkout_bor <- acc3 %>%
  filter(contributing_factor_vehicle_1=="Driver Inattention/Distraction") %>%
  group_by(year, borough) %>% 
  summarize(total=sum(n_injured) + sum(n_killed)) %>%
  ungroup()



#It doesn't seem as though there is anything peculiar going with boroughs. For some reason, driver interaction/distractions
# is lower for 2016. We only have 2/3 of the data for 2016.
inattention_all_bor <- acc3 %>%
  filter(contributing_factor_vehicle_1=="Driver Inattention/Distraction") %>%
  group_by(date, borough) %>% 
  summarise(total=sum(n_injured) + sum(n_killed)) %>%
  ungroup()

inattention_all_bor$borough[inattention_all_bor$borough==''] <- 'NOT INDICATED'

inattention_all_bor2 <- inattention_all_bor %>% spread(borough, total)
inattention_all_bor2[is.na(inattention_all_bor2)] <- 0 

inatt_xts <- xts(inattention_all_bor2[, -1], order.by = inattention_all_bor2$date)

qoq_xts <- qoq(inatt_xts)
yoy_xts <- yoy(inatt_xts)
plot.xts(yoy_xts[, 5])

df <- data.frame(yoy_xts, date=index(yoy_xts))
