load('data/nyc_bike_crashes.rda')

################################################################################
#
#
#Breaks out Bike Accidents by Borough
#
#
################################################################################
library(dplyr)
library(ggplot2)

#Filter out 2016 data
nyc_bike_crashes$year <- format(nyc_bike_crashes$date, "%Y")
nyc_bike_crashes$month <- format(nyc_bike_crashes$date, "%m")
nyc_bike_crashes$ym <- paste0(nyc_bike_crashes$year,"-", nyc_bike_crashes$month)
nyc_bike_crashes <- nyc_bike_crashes %>% filter(year!='2016')


nyc_bike_crashes2 <- nyc_bike_crashes %>% 
  group_by(ym, borough) %>% 
  summarize(total_cyclists_killed_injured=sum(total_cyclists_killed_injured)) %>% 
  ungroup()

nyc_bike_crashes2$date <- as.Date(paste0(nyc_bike_crashes2$ym, "-", "1"), "%Y-%m-%d")


#Take data and plot it
p <- ggplot(nyc_bike_crashes2, aes(date, total_cyclists_killed_injured)) +
  facet_wrap(~ borough) +
  geom_line() +
  theme_bw() +
  xlab("Date") +
  ylab("Injuries and Fatalities") +
  ggtitle("NYC Bike Injuries and Fatalities by Borough (2012-2015)")

ggsave("output/bike-accs-by-borough.png", 
       p, width = 5 * 1.448229, 
       height=5, 
       units = "in", 
       dpi=600)

