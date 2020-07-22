# mike nugent, jr.
# data analyst
# indygo

library(sf)
library(data.table)
library(tidytransit)
library(ggplot2)
library(magrittr)
library(dplyr)
library(leaflet)
library(leafem)

# let's investigate some old gtfs and see which routes used to run on the red line corridor
# first we'll read in the most recent
# add function for later
seq_along_by = function(x, by=1L, from = 1L) (seq_along(x) - 1L) * by + from



current_gtfs <- read_gtfs("data//GTFS//2006_2//2006_2_GTFS.zip")

# convert to sf
current_gtfs_sf <- gtfs_as_sf(current_gtfs) %>% set_hms_times %>% set_date_service_table() %>% set_servicepattern()

#remove trips with no shapes
current_gtfs_sf$trips %<>%
  filter(!trip_id %in% c("1733571","1733576","1733577","1733582","1733585","1733588","1733612","1733613"))

# match stops to points in shape ------------------------------------------
#add function

#check if this is snapping to the wrong side of the line
snap_points_to_line <- function(points,line){
  
  points_align <- st_nearest_points(points,line)%>%
    st_cast("POINT")
  
  points_new_geometry <- points_align[c(seq(2, length(points_align),by = 2))]
  
  points_align_end <- points %>%
    st_set_geometry(points_new_geometry)
  
}


# merge trips and shapes and routes ---------------------------------------
current_trips_and_shapes_sf <- current_gtfs_sf$shapes %>% 
  right_join(current_gtfs_sf$trips)

current_route_trips_and_shapes_sf <- current_trips_and_shapes_sf %>%
  right_join(current_gtfs_sf$routes)

# build stops to match to shapes ------------------------------------------
#now we need our stops with which we'll filter
#first create a separate df for ease of use
current_stops_sf <- Filter(function(x)!all(is.na(x)),current_gtfs_sf$stops) %>%
  select(-location_type) #clean one column

#pairs are Broad Ripple to 42nd, 38th to Statehouse, and New Jersey to University
endpoint_stop_names_SB_sf <- c("Broad.*SB","42.*SB","38.*SB","State.*SB","New.*SB","Uni.*SB")

shape_endpoints_SB_sf <- current_stops_sf %>%
  filter(grepl(paste(endpoint_stop_names_SB_sf,collapse = "|"),stop_name))

#see if it worked
shape_endpoints_SB_sf %>% 
  leaflet() %>% 
  addFeatures() %>% 
  addTiles()
#great!
#duplicate NB here later


# create shape splitting function -----------------------------------------
# first we need all route 90 SB shapes, direction_id 1 is south, 0 is NB
SB_90_sf <- current_route_trips_and_shapes_sf %>% filter(route_short_name == 90 & direction_id == 1)

#now get one routes points
SB_route_point_sf <- current_gtfs$shapes %>%
  st_as_sf(coords = c("shape_pt_lon","shape_pt_lat"),crs = 4326) %>%
  filter(shape_id == SB_90_sf$shape_id[1]) %>%
  arrange(shape_pt_sequence)

#then match those points to the endpoints
#use st_nn to make sure we get the route on the way down, not on the way back up
route_point_indices <- st_nn(shape_endpoints_SB_sf,SB_route_point_sf,k = 2) %>%
  lapply(min) %>%
  unlist()

n <- nrow(shape_endpoints_SB_sf)

SB_shape_list <- lapply(
  X = seq_along_by(1:(n/2),2), function(x){
    x<-3
    route_segment_start <- route_point_indices[x]
    route_segment_end <- route_point_indices[x+1]
    
    linestring <-  SB_route_point_sf[route_segment_start:route_segment_end,] %>%
      arrange(shape_pt_sequence) %>%
      st_coordinates() %>%
      st_linestring
    return(linestring) 
  }
)

SB_shape_list[[1]] %>%
  leaflet() %>% 
  addTiles() %>%
  addPolylines(color = "red", weight = 2)


SB_90_sf[1,] %>% 
  leaflet() %>% 
  addTiles() %>% 
  addPolylines(weight = 1) %>%
  addFeatures(SB_route_point_sf, radius = 1,label = ~shape_pt_sequence)

linestring <-  SB_route_point_sf[route_segment_start:route_segment_end,] %>%
  group_by(shape_id) %>%
  summarise(do_union = FALSE)%>%
  st_cast("LINESTRING")

linestring %>%
  leaflet() %>% 
  addTiles() %>%
  addPolylines(color = "red", weight = 2)


# OCT thru FEB
# 18 + 19 TM.Passcount
# find join query
# median speed and run time