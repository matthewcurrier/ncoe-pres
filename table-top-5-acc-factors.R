library(dplyr)
library(tidyr)
library(xts)
library(devtools)
library(xtsmatt)
library(lubridate)
library(Hmisc)
library(xtable)
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

#Remove any rows where both numeric cols are 0
acc4 <- acc3 %>% filter(n_injured!=0 | n_killed!=0)

###########At this point, you have an xts object upon which you can do some data manipulations


#Let's look at a table of contributing factors
top_causes <- acc4 %>% 
  group_by(contributing_factor_vehicle_1) %>% 
  summarize(total=sum(n_injured) + sum(n_killed)) %>% ungroup()


#Make a Chart of Top 5
top_causes <- top_causes %>% arrange(desc(total))
tbl <- top_causes[2:6, ]


write.csv(tbl, "output/top-5-bike-acc-factors.csv")

####Make a png version of the chart.

dvipng.dvi <- function (object, file, res=600)
{
  if (missing(file)){
    invisible(sys(
      paste("dvipng -T tight", "-D", res, shQuote(object$file)))
    )
  }
  else{
    invisible(sys(
      paste("dvipng -T tight", "-D", res, "-o", file, shQuote(object$file)))
    )
  }
}

colnames(tbl) <- c("Factor", "Count")
Rank <- tbl


dvipng.dvi(dvi.latex(latex(Rank)), file='output/top-5-factors.png')

##########End png creation
