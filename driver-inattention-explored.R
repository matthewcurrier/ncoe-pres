library(dplyr)
library(tidyr)
library(xts)
library(devtools)
library(xtsmatt)
library(lubridate)
library(ggplot2)
#devtools::install_github("matthewcurrier/xtsmatt")
#download.file("http://m40z.com/datasets/nyc_bike_accidents.rda", "nyc_bike_accidents.rda")
load("C:/Users/mc/Google Drive/R/Projects/Napa Schools Interview/nyc_bike_accidents.rda")
source("theme_simple.R")
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




#We'll look at just driver Inattention
inattention_brkout <- acc4 %>% filter(contributing_factor_vehicle_1=="Driver Inattention/Distraction") %>% group_by(year) %>% 
  summarise(total=sum(n_injured) + sum(n_killed)) %>% ungroup()


#We'll look at just driveer Inattention by borough
inattention_brkout_bor <- acc3 %>% filter(contributing_factor_vehicle_1=="Driver Inattention/Distraction") %>%
  group_by(year, borough) %>% 
  summarise(total=sum(n_injured) + sum(n_killed)) %>% ungroup()


###############################
#Create Plot
###############################




#Create base for chart
b <- ggplot(inattention_brkout, aes(year, total, label=total, group=1))

caption <- "Note: The City Bike program began in 2013."

g <- b + geom_line(size=1.5, colour="red") +
  theme_simple + 
  labs(title="Distracted NYC Drivers Are A Danger for Cyclists",
       subtitle="From 2012 to 2015 there was a 214% increase in accidents where inattention and distraction were factors.",
       caption=caption,
       x="Year",
       y="Cylclist Injuries") + geom_point(data = inattention_brkout, shape=21, size=2.75, colour="red", fill="white") +
  geom_text(data=inattention_brkout, aes(label=total), size=rel(5), hjust=1.2,vjust=-.2)


ggsave("output/driver-distraction-line.png", g, dpi=600)
g





library(ggplot2)
ggplot(inattention_brkout, aes(year, total, group=1)) + geom_point() + geom_line()


#It doesn't seem as though there is anything peculiar going with boroughs. For some reason, driver interaction/distractions
# is lower for 2016. We only have 2/3 of the data for 2016.
inattention_all_bor <- acc3 %>% filter(contributing_factor_vehicle_1=="Driver Inattention/Distraction") %>%
  group_by(date, borough) %>% 
  summarise(total=sum(n_injured) + sum(n_killed)) %>% ungroup()

inattention_all_bor$borough[inattention_all_bor$borough==''] <- 'NOT INDICATED'

inattention_all_bor2 <- inattention_all_bor %>% spread(borough, total)
inattention_all_bor2[is.na(inattention_all_bor2)] <- 0 

inatt_xts <- xts(inattention_all_bor2[, -1], order.by = inattention_all_bor2$date)

qoq_xts <- qoq(inatt_xts)
yoy_xts <- yoy(inatt_xts)
plot.xts(yoy_xts[, 5])

df <- data.frame(yoy_xts, date=index(yoy_xts))