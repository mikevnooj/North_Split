#duffy wants to know on DTC-66th
#which segments is the average speed over the speed limit
#can also check event

# when and where do red line vehicles exceed the speed limit?
# pull from factseg
# still going to do 2019-10-01 thru 2020-02-29

library(magrittr)
library(lubridate)
library(data.table)
library(dplyr)
library(tidytransit)
library(sf)
library(leaflet)
library(leafem)
library(RANN)

# import DW Fact_Seg_Adh --------------------------------------------------

con_DW <- DBI::dbConnect(odbc::odbc(), Driver = "SQL Server", Server = "AVAILDWHP01VW", 
                         Database = "DW_IndyGo", Port = 1433)

# get calendar dates

Calendar_dates <- tbl(con_DW, "DimDate") %>%
  filter(CalendarDate >= "2019-10-01", CalendarDate <= "2020-02-29") %>%
  collect() %>%
  data.table(key = "DateKey")


# get Red Line vehicles

Red_Line_Vehicles <- tbl(con_DW, "DimVehicle") %>%
  filter(MDCID >= 1950, MDCID < 2000) %>%
  collect() %>%
  data.table(key = "VehicleKey")


# get stop deets

DimStop <- tbl(con_DW, "DimStop") %>%
  collect() %>% 
  data.table(key = "StopKey")

#fix for join later
DimStop[,StopID := as.character(StopID)]

# get route 90 key

DimRoute_90 <- tbl(con_DW, "DimRoute") %>% 
  filter(RouteReportLabel == "90") %>%
  collect() %>%
  data.table(key = "RouteKey")
DimRoute_90


#get fact seg adh
FSA_raw <- tbl(con_DW, "FactSegmentAdherence") %>% 
  filter(DateKey %in% !!Calendar_dates$DateKey,
         VehicleKey %in% !!Red_Line_Vehicles$VehicleKey,
         RouteKey %in% !!DimRoute_90$RouteKey) %>%
  collect() %>% 
  data.table()





# read in current GTFS ----------------------------------------------------

current_gtfs <- read_gtfs("data//GTFS//2006_2//2006_2_GTFS.zip")

# convert to sf
current_gtfs_sf <- gtfs_as_sf(current_gtfs) %>% set_hms_times %>% set_date_service_table() %>% set_servicepattern()

#remove trips with no shapes
current_gtfs_sf$trips %<>%
  filter(!is.na(shape_id))

current_trips_and_shapes_sf <- current_gtfs_sf$shapes %>% 
  right_join(current_gtfs_sf$trips)

# join routes as well
current_route_trips_and_shapes_sf <- current_trips_and_shapes_sf %>%
  right_join(current_gtfs_sf$routes)

#stops df for ease of use
current_stops_sf <- Filter(function(x)!all(is.na(x)),current_gtfs_sf$stops) 


# G is northbound
# get nb and sb ends
endpoint_stop_names <- c("State.*NB","Broa.*SB")

segment_end_points_sf <- current_stops_sf %>%
  filter(grepl(paste(endpoint_stop_names,collapse = "|"),stop_name))

#get 90 direction and shape
route_shape_sf <- current_route_trips_and_shapes_sf %>%
  filter(route_short_name == 90) %>% #this gets the right set of shapes
  filter(st_length(geometry) == max(st_length(geometry))) %>% 
  slice(1)

#get route points
route_shape_point_sf <- current_gtfs$shapes %>% 
  st_as_sf(coords = c("shape_pt_lon","shape_pt_lat"),
           crs = 4326) %>% 
  filter(shape_id == route_shape_sf$shape_id)

#match points to endpoints
#this nn2 works identically
route_point_indices <- route_shape_point_sf %>%
  st_coordinates %>%
  nn2(query = segment_end_points_sf %>% st_coordinates()
      ,k = 2) %>%
  .$nn.idx %>%
  apply(1, min)

#input for next
n <- nrow(segment_end_points_sf)

#split into segment
segment_shape <- route_shape_point_sf[route_point_indices[1]:route_point_indices[2],] %>%
  group_by(shape_id) %>%
  arrange(shape_pt_sequence) %>%
  summarize(do_union = F) %>%
  st_cast("LINESTRING")

#turn current stops into dt
current_stops_geo_dt <- data.table(current_stops_sf,key = "stop_id")

segment_shape %>% 
  leaflet() %>%
  addFeatures() %>%
  addTiles()
  

FSA_joined <- DimStop[current_stops_geo_dt, on = c(StopID = "stop_id")
][
  ,.(StopKey,StopDesc,StopID,geometry)
][
  FSA_raw
  ,on = c(StopKey = "DepartStopKey")
][
  ,`:=`(DepartStopKey = StopKey
        ,DepartStop = StopDesc
        ,DepartStopID = StopID
        ,DepartStopGeometry = geometry)
]

x<-tbl(
 con_DW
 ,sql(
   "select RouteSegmentKey
   ,StartingStopKey
   ,EndingStopKey
   ,Geom
   from DimRouteSegment
   where RouteSegmentKey = 11010"
 )
) %>%
  collect()
