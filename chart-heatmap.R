#Trying to create a heat map of which days are the worst for bicycle accidents

library(tidyverse)
library(tibble)
library(ggplot2)
library(lubridate)

load("data/nyc_bike_crashes.rda")

bikes <- nyc_bike_crashes

bikes <- tbl_df(bikes)


#Data munging
bikes2 <- bikes %>%
  group_by(date) %>%
  summarise(total_cyclists_killed_injured=sum(total_cyclists_killed_injured)) %>%
  ungroup()

bikes2 <- bikes2 %>% mutate(year=format(date, "%Y"),
                            month=format(date, "%m"),
                            day=format(date, "%d"))
bikes2 <- bikes2 %>% filter(year!='2016')

date2 <- paste(bikes2$year, bikes$month, "1", sep="-")
date2 <- as.Date(date2, "%Y-%m-%d")

bikes2$day_of_week <- weekdays(bikes2$date, abbr = FALSE)
bikes2$day_short <- format(bikes2$date, "%a")
bikes2$month <- format(bikes2$date, "%m")



bikes3 <- bikes2 %>%
  group_by(day_short, month) %>%
  summarise(total_cyclists_killed_injured=sum(total_cyclists_killed_injured)) %>%
  ungroup()


#Make Day of Week an ordered factor so heatmap makes sense
bikes3$day_short <- as.factor(bikes3$day_short)
bikes3$day_short <- factor(bikes3$day_short, levels = c("Sun", "Mon", "Tue",
                                                        "Wed", "Thu",
                                                        "Fri","Sat"))



#Make Heatmap

b <- ggplot(bikes3, aes(x=day_short, y=month, fill = total_cyclists_killed_injured)) +
  geom_tile(colour = "white")  + 
  scale_fill_gradient(low="yellow", high="red", name = "Total Cyclists \nKilled Injured") +
  ggtitle("Calendar Heatmap of Bicycle Injuries in NYC (2012-2015)") +
  ylab("Month") +
  xlab("Day")

b
ggsave("images/heatmap.png", b, dpi=500)


#Another version of heatmap. Basically, just doing some cleanup here.
b2 <- b + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                panel.background = element_blank(), axis.line = element_line(colour = "black"))

ggsave("images/heatmap2.png", b2, dpi=500)