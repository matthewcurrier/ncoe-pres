source("source.R")
source("theme_simple.R")
library(ggplot2)
#We'll look at just driver Inattention

#We'll look at just driver Inattention
ina_per <- acc3 %>%
  group_by(year) %>% 
  summarise(total=sum(n_injured) + sum(n_killed)) %>%
  ungroup() 

inattention_brkout <- acc4 %>%
  filter(contributing_factor_vehicle_1=="Driver Inattention/Distraction") %>%
  group_by(year) %>% 
  summarise(total=sum(n_injured) + sum(n_killed)) %>%
  ungroup()

#Combine ina_per and inattention_brkout
per <- left_join(inattention_brkout, ina_per, by = c("year"))

#Calculate Driver Inattention as a percentage of all accidents
per <- per %>% mutate(per_all_accs=total.x / total.y)
per <- per %>% mutate(per_x100=round(per_all_accs*100, 1))
per$labels <- paste(round(100*per$per_all_accs, 1), "%", sep="")


#Create base for chart
b <- ggplot(per, aes(year, per_x100, group=1))
g <- b + geom_bar(colour="red", stat = "identity", fill="red") +
  theme_simple + 
  labs(title="Looking at Inattention / Distraction as a Percentage \nIt's Still Bad News",
       subtitle="Percentage of all bike accidents within New York City \nwhere driver inattention / distraction was cited as a contributing factor.",
       x="Year",
       y="% of Cylclist Injuries")  +
  geom_text(data=per, 
            aes(label=labels), 
            size=rel(5), 
            vjust=-.22)
print(g)
ggsave("output/percent-bar-chart.png", g, dpi=600)

