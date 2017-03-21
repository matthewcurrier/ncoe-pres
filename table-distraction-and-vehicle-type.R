library(dplyr)
library(tidyr)
library(xts)
library(devtools)
library(xtsmatt)
library(lubridate)
library(Hmisc)
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
                        contributing_factor_vehicle_2,
                        vehicle_type_code_1,
                        vehicle_type_code_2,
                        vehicle_type_code_3) 

acc2 <- acc2 %>% filter(number_of_cyclist_injured!=0 | number_of_cyclist_killed!=0)
acc2 <- acc2 %>% filter(contributing_factor_vehicle_1=="Driver Inattention/Distraction")

tbl <- as.data.frame(table(acc2$vehicle_type_code_1))
tbl <- tbl %>% arrange(desc(Count))
tbl <- tbl[1:5, ]

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

colnames(tbl) <- c("Vehicle Type", "Count")

Rank <- tbl


dvipng.dvi(dvi.latex(latex(Rank)), file='output/top-5-vehicle-types-in-passenger-distraction.png')

##########End png creation
