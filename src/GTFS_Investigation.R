# mike nugent, jr.
# data analyst
# indygo

library(data.table)
library(tidytransit)
library(ggplot2)

# let's investigate some old gtfs and see which routes used to run on the red line corridor
# first we'll read in the most recent

current_gtfs <- read_gtfs("data//GTFS//2006_2//2006_2_GTFS.zip")


