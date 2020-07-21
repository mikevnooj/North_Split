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
library(stplanr)
library(zoom)
library(leafem)

# let's investigate some old gtfs and see which routes used to run on the red line corridor
# first we'll read in the most recent

current_gtfs <- read_gtfs("data//GTFS//2006_2//2006_2_GTFS.zip")

# convert to sf
current_gtfs_sf <- gtfs_as_sf(current_gtfs) %>% set_hms_times %>% set_date_service_table() %>% set_servicepattern()


# prep route_trip_shape table ---------------------------------------------
#remove trips that have no shapes
current_gtfs_sf$trips %<>% filter(!is.na(shape_id))

#join shapes, trips, and routes
current_route_trip_shape_sf <- current_gtfs_sf$shapes %>%
  right_join(current_gtfs_sf$trips)%>%
  right_join(current_gtfs_sf$routes)

#get route 90 trip shapes
route_90_trip_shape_sf <- current_route_trip_shape_sf %>%
  filter(route_short_name == 90)


# prep stops to be used as endpoints, then prep shapes to be segme --------
#there are three segments, Broad Ripple to 42nd, 38th to State, and NJ to Universit
route_90_segment_end_stops <- current_gtfs_sf$stops %>%
  filter(
    stop_name %like% paste(
      c("Broa.*B","42nd.*B$","38.*B$","State.*B$","New.*B$","Uni.*B$")
      ,collapse = "|"
    )#end paste
  )#endfilter

# convert route 90 shapes to points
route_90_shape_ids <- route_90_trip_shape_sf %>%
  st_drop_geometry() %>%
  distinct(shape_id) %>%
  pull(shape_id)

route_90_shape_pts_sf <- current_gtfs$shapes %>%
  filter(shape_id %in% route_90_shape_ids) %>%
  st_as_sf(
    coords = c("shape_pt_lon","shape_pt_lat")
    ,crs = 4326 
  )


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

#snap the points
snapped_segment_ends_sf <- snap_points_to_line(route_90_segment_end_stops,route_90_trip_shape_sf %>%
                                                 filter(shape_id == route_90_shape_ids[1]) %>%
                                                 slice(1)
                                                 )

#
SB_route_shape_pts_sf <- route_90_shape_pts_sf %>%
  filter(shape_id == route_90_shape_ids[1])

SB_snapped_segment_ends_sf <- snapped_segment_ends_sf %>%
  filter(stop_name %like% "SB$")

seq_along_by = function(x, by=1L, from = 1L) (seq_along(x) - 1L) * by + from

n <- nrow(SB_snapped_segment_ends_sf)

linestrings <- lapply(X=seq_along_by(1:(n/2),2) , FUN = function(i){

  start <- SB_snapped_segment_ends_sf %>%
    st_join(SB_route_shape_pts_sf,join = st_nn) %>%
    slice(i) %>% 
    pull(shape_pt_sequence)
  end <- SB_snapped_segment_ends_sf %>%
    st_join(SB_route_shape_pts_sf,join = st_nn) %>%
    slice(i+1) %>%      
    pull(shape_pt_sequence)
  
  linestring <- SB_route_shape_pts_sf %>%
    filter(shape_pt_sequence >= start,
           shape_pt_sequence < end) %>%
    st_sf() %>%
    st_coordinates() %>%
    st_linestring()
  return(linestring)
})

linestrings[[3]] %>% plot()
SB_snapped_segment_ends_sf %>% st_join(SB_route_shape_pts_sf,join = st_nn)
  


SB_snapped_segment_ends_sf %>%
  leaflet() %>%
  addTiles() %>%
  addFeatures(label = ~"snapped") %>%
  addFeatures(SB_route_shape_segment_end_pts_sf)


