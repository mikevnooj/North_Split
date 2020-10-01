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
#we'll look at northbound
direction <- "NB"

con <- DBI::dbConnect(odbc::odbc(), Driver = "SQL Server", Server = "IPTC-TMDATAMART\\TMDATAMART", 
                      Database = "TMDATAMART", Port = 1433)

# get routes
ROUTE_raw <- tbl(con,"ROUTE") %>% 
  select(ROUTE_ID, ROUTE_ABBR) %>%
#  filter(ROUTE_ABBR %in% !!old_routes_on_rl) %>%
  collect() %>%
  setDT() %>%
  setkey(ROUTE_ID)

# get stops

GEO_NODE_raw <- tbl(con, "GEO_NODE") %>% 
  select(GEO_NODE_ID, Stop_number = GEO_NODE_ABBR, Stop_name = GEO_NODE_NAME,
         Stop_lat = LATITUDE, Stop_lon = LONGITUDE) %>%
  collect() %>%
  setDT() %>%
  setkey(GEO_NODE_ID)

# get calendar

CALENDAR_raw <- tbl(con, "CALENDAR") %>% 
  select(CALENDAR_ID, CALENDAR_DATE) %>%
  collect() %>%
  setDT() %>%
  setkey(CALENDAR_ID)

# get vehicles

VEHICLE_ID_raw <- tbl(con, "VEHICLE") %>% 
  select(VEHICLE_ID, PROPERTY_TAG) %>%
  collect() %>%
  setDT() %>%
  setkey(VEHICLE_ID)

# get route direction

ROUTE_DIRECTION_raw <- tbl(con,"ROUTE_DIRECTION") %>%
  collect() %>%
  setDT() %>%
  setkey(ROUTE_DIRECTION_ID)


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
  .[ROUTE_raw,
    on = "ROUTE_ID"
    ,names(ROUTE_raw) := mget(paste0("i.",names(ROUTE_raw)))
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
  )] %>%
  .[
    #change from seconds since midnight to actual timee
    
    ,`:=` (
      ARRIVAL_TIME = CALENDAR_DATE + seconds(ARRIVAL_TIME)
      ,DEPARTURE_TIME = CALENDAR_DATE + seconds(DEPARTURE_TIME)
      ,MESSAGE_TIME = CALENDAR_DATE + seconds(MESSAGE_TIME)
    ) 
  ] %>%
  .[
    #change direction label
    ,direction := case_when(
      ROUTE_ABBR %in% c(12,22,13,14) ~ ifelse(ROUTE_DIRECTION_ID==4,0,1),
      TRUE ~ ifelse(ROUTE_DIRECTION_ID==6,0,1)
    )
  ]


test_pass_count[,c("ARRIVAL_TIME","DEPARTURE_TIME","MESSAGE_TIME")] %>%
  is.na() %>%
  rowSums()



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






match_points_to_segment <- function(points,segment_sf,k = 10000,radius_in_meters = 10){
  
  #get points
  points_coords <- points %>% st_coordinates()
  
  #get match ind
  match_ind <- nn2(
    points_coords
    ,segment_sf %>% st_segmentize(5) %>% st_coordinates() %>% .[,1:2] #get segment coordinate matrix
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

test_pass_count_sf <- test_pass_count[
  direction == fifelse(direction == "SB",1,0) &
    !is.na(LONGITUDE) &
    !is.na(LATITUDE)
] %>% 
st_as_sf(
  coords = c("LONGITUDE","LATITUDE")
)

matched_pass_sf <- match_points_to_segment(points = test_pass_count_sf,segment_sf = shape_segments[1,],k = nrow(test_pass_count_sf),radius_in_meters = 20)

matched_pass <- data.table(matched_pass_sf)

matched_pass[
  ,.(min(BLOCK_STOP_ORDER),
     max(BLOCK_STOP_ORDER))
  ,.(
    TRIP_ID
    )
][
  ,V3 := V2-V1
][]



matched_pass[!is.na(MESSAGE_TIME)] %>%
  st_sf() %>% 
leaflet() %>%
addFeatures() %>%
addTiles()

matched_pass[!is.na(MESSAGE_TIME)]

#okay let's play around with veh loc
test_veh_loc_sf <- st_as_sf(test_veh_loc
                            ,coords = c("LONGITUDE","LATITUDE")
                            ,crs = 4326)

#make an sf
matched_veh_loc_sf <- match_points_to_segment(test_veh_loc_sf,shape_segments[1,],k=nrow(test_veh_loc_sf))

#make a DT
matched_veh_loc <- matched_veh_loc_sf %>% data.table()

#make an sp
matched_veh_loc_sp <- matched_veh_loc_sf %>%
    as_Spatial() %>% 
    spTransform(CRS(st_crs(7328)$proj4string))

#get segment sp
segment_sp <- as_Spatial(shape_segments[1,] %>% st_transform(7328))
  
#get seglen
segment_length <- gLength(segment_sp)

#get dist traveled
shape_dist_traveled <- gProject(segment_sp,matched_veh_loc_sp)

#add to DT
matched_veh_loc$dist_traveled <- shape_dist_traveled

#
DT <- 1
  

matched_veh_loc[
  #order by time, then get dist to next
  order(MESSAGE_TIMESTAMP)
  ,`:=` (
    dist_to_next = c(diff(dist_traveled),0)
    ,dist_trav_from_prev = dist_traveled - shift(dist_traveled)
    ,time_to_next = c(diff(MESSAGE_TIMESTAMP),0)
  )
][
  #find where trips break
 ,same_trip := time_to_next <= 120
][
  #mark negative values
  ,neg := dist_trav_from_prev < 0
][
  ,head(.SD,17) 
][]



%>%
st_sf() %>% 
leaflet() %>%
addFeatures(label = ~MESSAGE_TIMESTAMP,radius = 1) %>%
addTiles()


[
  !neg == T & same_trip == T
][
  
  ,cum_dist := cumsum(abs(dist_to_next))
  ,.(VEHICLE_ID,CALENDAR_DATE,rl)
][
  
]

matched_pass[]



[
  #flag negative
  ,neg := dist_to_next <= 0
  
][
  #create rleid
  ,rl := rleid(neg),by = .(VEHICLE_ID,CALENDAR_DATE)

][
  #group by both and get length of each run
  ,rl_len := .N,.(VEHICLE_ID,CALENDAR_DATE,rl)
      
]

View(DT)
[
  #filter rows that have more than 3 and are NEG
  !(neg == T & rl_len >= 4)
][
  #redo calcs
  ,`:=` (dist_to_next = c(diff(dist_traveled),0))
  ,.(VEHICLE_ID,CALENDAR_DATE,rl)
][
  ,`:=`(
    mph = c(NA_real_,diff(dist_traveled)/diff(as.integer(MESSAGE_TIMESTAMP))/1.466667)
    ,cum_dist = cumsum(abs(dist_to_next))
  )
  ,.(VEHICLE_ID,CALENDAR_DATE,rl)
][
  #add cum_dist_flag
  ,cum_dist_ok := cum_dist <= segment_length | cum_dist-segment_length < 20
][
  #filter
  cum_dist_ok == T
]

DT <- DT[
  #convert to feet, get min and max time
  ,.(miles_traveled = max(cum_dist)/5280 #convert to feet
     ,start_time = min(MESSAGE_TIMESTAMP) 
     ,end_time = max(MESSAGE_TIMESTAMP)
     ,pings = .N)
  ,.(VEHICLE_ID,CALENDAR_DATE,rl)
  
  ][
    #convert to hours
    ,time := as.numeric(difftime(end_time,start_time,units = "secs"))/60/60 #convert to hours
  ][
    #convert to speed
    ,speed := miles_traveled/time #get speed
  ][
    #remove records with very low ping #
    pings >= 10
  ]



DT %>% View()




DT[rl == 1] %>%
st_as_sf() %>% 
leaflet() %>%
addFeatures(label = ~MESSAGE_TIMESTAMP) %>%
addTiles()


# let's find some garbage VMH trips so we can compare them to avail's trip running times speeds ---------------------------------------
closest_VMH[[1]]


