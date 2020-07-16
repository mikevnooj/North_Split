# mike nugent, jr.
# data analyst
# indygo

library(data.table)
library(tidytransit)
library(ggplot2)
library(magrittr)

# let's investigate some old gtfs and see which routes used to run on the red line corridor
# first we'll read in the most recent

current_gtfs <- read_gtfs("data//GTFS//2006_2//2006_2_GTFS.zip")

# convert to sf
current_gtfs_sf <- gtfs_as_sf(current_gtfs) %>% set_hms_times %>% set_date_service_table() %>% set_servicepattern()


#detour to look at shapes and stuff
x <- current_gtfs_sf$shapes %>%
  left_join(current_gtfs_sf$trips) %>%
  left_join(current_gtfs_sf$routes) %>%
  filter(route_short_name == "90")

#okay shapes don't matter, cool LOL
x[!duplicated(x$shape_id),] %>%
  filter(route_short_name %like% "9") %>% 
  select(shape_id) %>% 
  ggplot()+
  geom_sf() +
  facet_wrap(~shape_id)
  
#let's build our sections, maybe use a bounding box?
current_gtfs$shapes %>% head()
current_gtfs_sf$shapes$dummy <- NULL

#okay so let's start by joining trips to shapes

current_gtfs_sf$trips %>% 
  left_join(current_gtfs_sf$shapes)

#lol cool that doesn't work
gtfs_shapes <- current_gtfs$shapes
#split the shapes by shape_id
list_of_line_tibbles <- split(gtfs_shapes,gtfs_shapes$shape_id)
#convert them to linestrings, will output a list
list_of_linestrings <- lapply(list_of_line_tibbles,function(df){
  m<-as.matrix(df[order(df$shape_pt_sequence),c("shape_pt_lon",
                                             "shape_pt_lat")])
  return(st_linestring(m))
})
shape_linestrings <- data.frame(
  shape_id = unique(gtfs_shapes$shape_id)
  ,st_sfc(list_of_linestrings,crs=4326) 
)

trips <- data.frame(current_gtfs$trips)

merge(trips, shape_linestrings)

setdiff(current_gtfs$trips$trip_id,merge(trips, shape_linestrings)$trip_id)
current_gtfs$trips %>%
  filter(trip_id %in% c("1733571","1733576","1733577","1733582","1733585","1733588","1733612","1733613"))

# current_gtfs_sf$shapes
# shapedummy
# tripdummy <- data.frame(trip_id = c(1:7),shape_id = c(1,2,3,1,2,3,1))
# shapedummy <- data.frame(shape_id = c(1:3))
# geometry <- st_sfc(
#   list(st_linestring(rbind(c(1,1),c(5,3),c(7,9)))
#      ,st_linestring(rbind(c(5,1),c(5,3),c(11,2)))
#      ,st_linestring(rbind(c(1,15),c(5,23),c(37,9),c(62,3)))),
#   crs = 4326
# )
# 
# shapedummy %<>% st_sf(geometry = geometry)
# 
# #ends up losing sf class but the join is correct
# class(left_join(tripdummy,shapedummy))
# 
# st_sf(left_join(tripdummy,shapedummy))
# 
# left_join(shapedummy,tripdummy)


#idiot


# merge trips and shapes --------------------------------------------------


#okay, let's merge trips and shapes
trips_without_shapes <- c("1733571","1733576","1733577","1733582","1733585","1733588","1733612","1733613")

current_gtfs_sf$trips %>%
  filter(!trip_id %in% c("1733571","1733576","1733577","1733582","1733585","1733588","1733612","1733613")) %>% 
  left_join(current_gtfs_sf$shapes)


current_trips_and_shapes <- right_join(
  current_gtfs_sf$shapes
  ,current_gtfs_sf$trips %>%
    filter(!trip_id %in% c("1733571","1733576","1733577","1733582","1733585","1733588","1733612","1733613"))
)

current_route_trips_and_shapes <- right_join(
  current_trips_and_shapes
  ,current_gtfs_sf$routes
)


