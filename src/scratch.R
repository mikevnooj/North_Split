library(sf)
library(stringr)
library(data.table)
library(nngeo)
library(tidytransit)
library(ggplot2)
library(magrittr)
library(dplyr)
library(leaflet)
library(DBI)
library(leafem)
library(lubridate)
library(RANN)


# notes -------------------------------------------------------------------
#GTFS 0 is NB
#VMH 0 is NB



test_pass_count <- tbl(con,"PASSENGER_COUNT") %>%
  filter(VEHICLE_ID == 451,
         CALENDAR_ID == 120171002) %>%
  # group_by(VEHICLE_ID) %>%
  # summarise(n()) %>%
  collect() %>%
  data.table() %>%
  .[CALENDAR_raw #join calendar
    ,on = "CALENDAR_ID"
    ,names(CALENDAR_raw) := mget(paste0("i.",names(CALENDAR_raw)))
    ] %>%
  .[GEO_NODE_raw #join geo_node
    , on = "GEO_NODE_ID"
    ,names(GEO_NODE_raw) := mget(paste0("i.",names(GEO_NODE_raw)))
    ] %>%
  .[,`:=` (
    LATITUDE = fifelse( #fix latitude
      test = is.na(LATITUDE)
      ,Stop_lat/10000000
      ,LATITUDE/10000000
    )#end fifelse
    ,LONGITUDE = fifelse( #fix longitude
      is.na(LONGITUDE)
      ,Stop_lon/10000000
      ,LONGITUDE/10000000
    )#end fifelse
  )]




test_veh_loc <- tbl(con,"VEHICLE_LOCATION") %>%
  filter(VEHICLE_ID == 451,
         CALENDAR_ID == 120171002) %>%
  collect() %>%
  data.table() %>%
  .[,`:=` (
    LATITUDE = LATITUDE/10000000
    ,LONGITUDE = LONGITUDE/10000000
  )] %>%
  .[CALENDAR_raw
    ,on = "CALENDAR_ID"
    ,names(CALENDAR_raw) := mget(paste0("i.",names(CALENDAR_raw)))
    ]

test_veh_loc %>%
  st_as_sf(
    coords = c("LONGITUDE","LATITUDE")
    ,crs = 4326) %>%
  leaflet() %>%
  addFeatures(label = ~MESSAGE_TIMESTAMP) %>%
  addTiles()

test_pass_count %>%
leaflet() %>%
addCircles() %>%
addTiles()



match_points_to_segment <- function(points,segment_sf,k = 10000,radius_in_meters = 10){
  
  #get points
  points_coords <- points %>% st_coordinates()
  
  #get match ind
  match_ind <- nn2(
    points_coords
    ,segment %>% st_segmentize(5) %>% st_coordinates() %>% .[,1:2] #get segment coordinate matrix
    ,k = k
    ,searchtype = "radius"
    ,radius = 0.00001*(radius_in_meters/1.11)
  ) %>% 
    sapply(cbind) %>%
    as_tibble() %>%
    distinct(nn.idx) %>%
    pull() %>%
    sort()
  
  return(points[match_ind,])
}

match_points_to_segment()
