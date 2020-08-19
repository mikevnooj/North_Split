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
library(rgeos)
options(scipen = 999)
setwd("C:/Users/Michael.Nugent/Documents/Projects/R_Projects/nooj/North_Split")

# notes -------------------------------------------------------------------
#GTFS 0 is NB
#VMH 0 is NB

# inputs
#takes lane type, direction
lane_type <- c("single bi-directional","separated uni-directional","none")
direction <- "NB"

# functions ---------------------------------------------------------------
seq_along_by <- function(x, by=1L, from = 1L) (seq_along(x) - 1L) * by + from

snap_points_to_line <- function(points,line){
  
  points_align <- st_nearest_points(points,line)%>%
    st_cast("POINT")
  
  points_new_geometry <- points_align[c(seq(2, length(points_align),by = 2))]
  
  points_align_end <- points %>%
    st_set_geometry(points_new_geometry)
  
}

# read old GTFS -----------------------------------------------------------
#there were some strange errors when reading this in so we are going to do it manually
old_gtfs <-lapply(paste0("data//GTFS//1806//agency//",dir(path = "data//GTFS//1806//agency")),fread)
#set the names
names(old_gtfs) <- dir(path = "data//GTFS//1806//agency") %>%
  str_remove(".txt")

#copy so we can sf it
old_gtfs_sf <- old_gtfs

#sf shapes
old_gtfs_sf$shapes %<>%
  st_as_sf(coords = c("shape_pt_lon","shape_pt_lat")
           ,crs = 4326
  ) %>%
  group_by(shape_id) %>%
  summarize(do_union = FALSE) %>%
  st_cast("LINESTRING")

#get rid of trips with no shapes
old_gtfs_sf$trips %<>%
  filter(!is.na(shape_id))

# old GTFS convenience objects --------------------------------------------
old_gtfs_sf$stops %<>%
  st_as_sf(coords = c("stop_lon","stop_lat")
           ,crs = 4326) 

old_route_trips_and_shapes_sf <- old_gtfs_sf$shapes %>% 
  right_join(old_gtfs_sf$trips)%>%
  right_join(old_gtfs_sf$routes)

#create a stops df for ease of use
old_stops_sf <- Filter(function(x)!all(is.na(x)),old_gtfs_sf$stops) %>% #removes columns that are all blank
  select(-location_type)

#adjust stop times so we can join later
old_gtfs_sf$stop_times %<>% mutate(stop_id = as.character(stop_id)) 



# read in current GTFS ----------------------------------------------------
current_gtfs <- read_gtfs("data//GTFS//2006_2//2006_2_GTFS.zip")

# convert to sf
current_gtfs_sf <- gtfs_as_sf(current_gtfs) %>% set_hms_times %>% set_date_service_table() %>% set_servicepattern()

#remove trips with no shapes
current_gtfs_sf$trips %<>%
  filter(!is.na(shape_id))


# import PassCount and VMH ------------------------------------------------

pass_count_raw <- fread("data//processed//pass_count_raw.csv")


#import VMH
VMH_Raw <- fread("data//processed//VMH_Raw.csv")

VMH_direction_sf <- VMH_Raw[Inbound_Outbound == fifelse(direction == "SB",1,0)] %>%
  st_as_sf(
    coords = c("Longitude","Latitude")
    ,crs = 4326
  )

VMH_direction_sf %>% filter(Vehicle_ID == 1996,
                            Transit_Day == "2019-10-10",
                            Time > "2019-10-10 12:15:00",
                            Time < "2019-10-10 17:00:00")

#bastard row names
VMH_direction_sf$rn <- seq_along(1:nrow(VMH_direction_sf))


# create convenience objects ----------------------------------------------
# merge trips and shapes
current_trips_and_shapes_sf <- current_gtfs_sf$shapes %>% 
  right_join(current_gtfs_sf$trips)

# join routes as well
current_route_trips_and_shapes_sf <- current_trips_and_shapes_sf %>%
  right_join(current_gtfs_sf$routes)

#stops df for ease of use
current_stops_sf <- Filter(function(x)!all(is.na(x)),current_gtfs_sf$stops) 

#pairs are Broad Ripple to 42nd, 38th to Statehouse, and New Jersey to University
endpoint_stop_names <- paste0(c("Broad.*","42.*","38.*","State.*","New.*","Uni.*"),direction)

segment_end_points_sf <- current_stops_sf %>%
  filter(grepl(paste(endpoint_stop_names,collapse = "|"),stop_name)) %>%
  mutate(
    lane_type = case_when(
      stop_name %like% paste(endpoint_stop_names[1:2],collapse = "|") ~ "single bi-directional"
      ,stop_name %like% paste(endpoint_stop_names[3:4],collapse = "|") ~ "separated uni-directional"
      ,TRUE ~ "none"
    )#end case_when
  )#end mutate
    


#get 90 and direction shape
route_shape_sf <- current_route_trips_and_shapes_sf %>%
  filter(route_short_name == 90,
         direction_id == ifelse(direction == "SB",1,0)) %>% #this gets the right set of shapes
  filter(st_length(geometry) == max(st_length(geometry))) %>%
  slice(1)

#get route points of interest
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

#split into segments


segment_shape_list <- lapply(
  X = seq_along_by(1:(n/2),2), function(x){
    
    route_segment_start <- route_point_indices[x]
    route_segment_end <- route_point_indices[x+1]
    
    linestring <- route_shape_point_sf[route_segment_start:route_segment_end,] %>%
      group_by(shape_id) %>%
      arrange(shape_pt_sequence) %>%
      summarize(do_union = F) %>%
      st_cast("LINESTRING")
      
        
    return(linestring)
  }
)

#add labels
shape_segments <- lapply(segment_shape_list,st_sf) %>%
  do.call(rbind,.) %>% 
  mutate(lane_type = segment_end_points_sf$lane_type[c(FALSE,TRUE)])#the false true repeats and grabs the lane types


# get nearest old stops to new route segments -----------------------------

old_to_new_match <- function(i){
  
  shape_segment_coords <- shape_segments[i,] %>% st_segmentize(.5) %>% st_coordinates() %>% .[,1:2]
  
  old_stops_coords <- st_coordinates(old_stops_sf)
  
  #this works and is very fast
  nn2_oldstops <- nn2(
    data = old_stops_coords
    ,query = shape_segment_coords
    ,k = nrow(old_stops_coords)
    ,searchtype = "radius"
    ,radius = 0.00001*(15/1.11)
  ) %>%
    sapply(cbind) %>%
    as_tibble() %>%
    distinct(nn.idx) %>% 
    pull() %>% 
    sort()

  #get old stops
  old_stops_on_segment <- old_stops_sf[nn2_oldstops,]
  
  return(old_stops_on_segment)
  
}

old_stops_on_segment <- lapply(X = seq_along(1:nrow(shape_segments)),old_to_new_match)

names(old_stops_on_segment) <- shape_segments$lane_type


# get old routes along segment -------------------------------------------
old_routes_on_segment <- lapply(old_stops_on_segment,function(x) x %>%
         left_join(old_gtfs_sf$stop_times) %>%
         left_join(st_drop_geometry(old_route_trips_and_shapes_sf)) %>%
         st_drop_geometry() %>%
         distinct(as.numeric(route_short_name)))


# #viz
# old_route_trips_and_shapes_sf %>%
#   group_by(shape_id) %>%
#   slice(1) %>%
#   ungroup() %>%
#   filter(route_short_name %in% old_routes_on_segment$`bi-directional`) %>%
#   ungroup() %>%
#   leaflet() %>%
#   addFeatures() %>%
#   addTiles()


# match VMH to SB_shape_segments ------------------------------------------

#get coordinate matrices
VMH_direction_sf_coords <- do.call(
  rbind
  ,st_geometry(VMH_direction_sf)
)

closest_VMH <- lapply(
  seq_along(1:nrow(shape_segments)), FUN = function(x){
    nn2(
      VMH_direction_sf_coords
      ,shape_segments[x,] %>% st_segmentize(5) %>% st_coordinates() %>% .[,1:2] #segment coordinate matrix
      ,k = 10000
      ,searchtype = "radius"
      ,radius = 0.00001*(10/1.11)
    ) %>%
      sapply(cbind) %>% #combine
      as_tibble() %>% #convert so we can pull
      distinct(nn.idx) %>%
      pull() %>%
      sort() %>%
      VMH_direction_sf[.,]
  }
)  

names(closest_VMH) <- shape_segments$lane_type

closest_VMH %>% lapply(sample_n,10000) %>%
  lapply(function(x){
    x %>%
    leaflet() %>%
    addFeatures() %>%
    addTiles()
  })

#let's test gproject
#this is southbound, so the distance traveled for 9725 should be GREATER than for 1
#okay so this definitely works
# closest_VMH[[2]] %>%
#   .[c(1,1350),]%>%
#   leaflet() %>%
#   addFeatures() %>%
#   addFeatures(shape_segments[2,])%>%
#   addTiles()

# testpoints_sp <- closest_VMH[[2]] %>% .[c(1,1350),] %>% as_Spatial() %>% spTransform(CRS(st_crs(7328)$proj4string))
# 
# 
# segment_sp <- as_Spatial(shape_segments[2,]%>% st_transform(7328))
# 
# gProject(segment_sp,testpoints_sp)


# get dist traveled on shape --------------------------------------------

closest_VMH <- lapply(seq_along(1:length(closest_VMH)),function(i){
  
  VMH_sp <- closest_VMH[[i]] %>%
    as_Spatial() %>% 
    spTransform(CRS(st_crs(7328)$proj4string))
  
  segment_sp <- as_Spatial(shape_segments[i,] %>% st_transform(7328))
  
  segment_length <- gLength(segment_sp)
  
  shape_dist_traveled <- gProject(segment_sp,VMH_sp)
  
  closest_VMH[[i]]$dist_traveled <- shape_dist_traveled
  
  return(closest_VMH[[i]])
}) %>% `names<-`(shape_segments$lane_type)



# get DT with speeds --------------------------------------------

VMH_speed_DT <- lapply(seq_along(1:length(closest_VMH)), function(i){
  
  #pull in
  DT <- data.table(closest_VMH[[i]])
  #get seglen
  seglen <- gLength(as_Spatial(shape_segments[i,] %>% st_transform(7328)))
    #
  #DT <- 1
  grp <- quote(list(Trip,Transit_Day,Vehicle_ID))
  
  #clean DT
  DT <- DT[
    #order by trip, then veh_ID, then transit_Day, then Time
    order(Trip,Vehicle_ID,Transit_Day,Time)
  
  ][
    #remove non BYD
    Vehicle_ID %in% c(1899,1970:1999)
  
  ][
    #set dist_to_next, resetting at each new group
    ,`:=` (dist_to_next = c(diff(dist_traveled),0))
    ,grp
  
  ][
    #set mph for each point to point segment and get cumulative distance traveled, by group
    ,`:=`(mph = c(NA_real_,diff(dist_traveled)/diff(as.integer(Time))/1.466667)
                     ,cum_dist = cumsum(abs(dist_to_next)))
               ,grp
  
  ][
    #flag negative
    ,neg := dist_to_next <= 0
  
  ][
    #create rleid
    ,rl := rleid(neg),by = grp
      
  ][
    #group by both and get length of each run
    ,rl_len := .N,.(Trip,Transit_Day,Vehicle_ID,rl)
        
  ][
    #filter rows that have more than 3 and are NEG
    !(neg == T & rl_len >= 4)
  ][
    #redo calcs
    ,`:=` (dist_to_next = c(diff(dist_traveled),0))
    ,grp
  ][
    ,`:=`(mph = c(NA_real_,diff(dist_traveled)/diff(as.integer(Time))/1.466667)
         ,cum_dist = cumsum(abs(dist_to_next)))
    ,grp
  ][
    #add cum_dist_flag
    ,cum_dist_ok := cum_dist <= seglen | cum_dist-seglen < 20
  ][
    #filter
    cum_dist_ok == T
  ][
    #do these calcs one more time
    ,`:=` (dist_to_next = c(diff(dist_traveled),0))
    ,grp
  ][
    ,`:=`(mph = c(NA_real_,diff(dist_traveled)/diff(as.integer(Time))/1.466667)
          ,cum_dist = cumsum(abs(dist_to_next)))
    ,grp
  ]

  
  DT <- DT[
    #convert to feet, get min and max time
    ,.(miles_traveled = max(cum_dist)/5280 #convert to feet
       ,start_time = min(Time) 
       ,end_time = max(Time)
       ,pings = .N)
    ,grp
     
  ][
    #convert to hours
    ,time := as.numeric((end_time - start_time)/60/60) #convert to hours
  ][
    #convert to speed
    ,speed := miles_traveled/time #get speed
  ][
    #remove records with very low ping #
    pings >= 10
  ]
  
  
  return(DT)
}) %>% `names<-`(shape_segments$lane_type) %>%
  rbindlist(idcol = T)

VMH_speed_DT$direction <- direction


fwrite(VMH_speed_DT, file = paste0("data//processed//VMH_speed_",direction))
fwrite(old_routes_on_segment, file = paste0("data//processed//old_routes_",direction,".csv"))

# start pass count --------------------------------------------------------

pass_count_joined_raw <- fread("data//processed//pass_count_joined.csv")

# add N/S
pass_count_joined_raw[,direction := case_when(
                        ROUTE_ABBR %in% c(12,22,13,14) ~ ifelse(ROUTE_DIRECTION_ID==4,0,1),
                        TRUE ~ ifelse(ROUTE_DIRECTION_ID==6,0,1))]






#fix stop lat long
pass_count_joined_raw[,`:=` (
  LATITUDE = fifelse(
    test = is.na(LATITUDE)
    ,Stop_lat/10000000
    ,LATITUDE/10000000
  )#end fifelse
  ,LONGITUDE = fifelse(
    is.na(LONGITUDE)
    ,Stop_lon/10000000
    ,LONGITUDE/10000000
  )#end fifelse
)]




pass_count_direction_sf <- pass_count_joined_raw[direction == fifelse(direction == "SB",1,0) &
                                                   !is.na(LONGITUDE) &
                                                   !is.na(LATITUDE)] %>%
  st_as_sf(
    coords = c("LONGITUDE","LATITUDE")
    ,crs = 4326
  )


#bastard row names
pass_count_direction_sf$rn <- seq_along(1:nrow(pass_count_direction_sf))


# match pass_count to segments --------------------------------------------

#get coordinate matrices
pass_count_direction_sf_coords <- do.call(
  rbind
  ,st_geometry(pass_count_direction_sf)
)

closest_pass_count <- lapply(
  seq_along(1:nrow(shape_segments)), FUN = function(x){
    nn2(
      pass_count_direction_sf_coords
      ,shape_segments[x,] %>% st_segmentize(5) %>% st_coordinates() %>% .[,1:2] #segment coordinate matrix
      ,k = 10000
      ,searchtype = "radius"
      ,radius = 0.00001*(10/1.11)
    ) %>%
      sapply(cbind) %>% #combine
      as_tibble() %>% #convert so we can pull
      distinct(nn.idx) %>%
      pull() %>%
      sort() %>%
      pass_count_direction_sf[.,]
  }
) %>% `names<-`(shape_segments$lane_type)


closest_pass_count %>% lapply(sample_n,10000) %>%
  lapply(function(x){
    x %>%
      leaflet() %>%
      addFeatures() %>%
      addTiles()
  })

closest_pass_count$`shared-center` %>% filter(TRIP_ID == 237042) %>%
leaflet() %>%
addFeatures() %>%
addTiles()

pass_count_direction_sf %>% filter(TRIP_ID == 237042) %>%
leaflet() %>%
addFeatures(radius = 1) %>%
addTiles()

pass_count_joined_raw %>% filter(TRIP_ID == 237042) %>%
leaflet() %>%
addCircles() %>%
addTiles()

pass_count_joined_raw %>% filter(TRIP_ID == 237042) %>%
  distinct(VEHICLE_ID,CALENDAR_ID)
                              #& CALENDAR_ID == 120171002)
# let's test gproject
# this is northbound, so the distance traveled for 1350 should be MORE than for 1
# okay so this definitely still works
# closest_pass_count[[1]] %>%
#   .[c(1,1350),]%>%
#   leaflet() %>%
#   addFeatures() %>%
#   addFeatures(shape_segments[1,])%>%
#   addTiles()
# 
# testpoints_sp <- closest_pass_count[[1]] %>% .[c(1,1350),] %>% as_Spatial() %>% spTransform(CRS(st_crs(7328)$proj4string))
# 
# 
# segment_sp <- as_Spatial(shape_segments[1,]%>% st_transform(7328))
# 
# gProject(segment_sp,testpoints_sp)


#add distance traveled
closest_pass_count <- lapply(seq_along(1:length(closest_pass_count)),function(i){
  
  pass_count_sp <- closest_pass_count[[i]] %>%
    as_Spatial() %>% 
    spTransform(CRS(st_crs(7328)$proj4string))
  
  segment_sp <- as_Spatial(shape_segments[i,] %>% st_transform(7328))
  
  segment_length <- gLength(segment_sp)
  
  shape_dist_traveled <- gProject(segment_sp,pass_count_sp)
  
  closest_pass_count[[i]]$dist_traveled <- shape_dist_traveled
  
  return(closest_pass_count[[i]])
}) %>% `names<-`(shape_segments$lane_type)

# get DT with speeds --------------------------------------------

pass_count_speed_DT <- lapply(seq_along(1:length(closest_pass_count)), function(i){
  #pull in
  DT <- data.table(closest_pass_count[[i]])
  #get seglen
  seglen <- gLength(as_Spatial(shape_segments[i,] %>% st_transform(7328)))
  
  #DT <- 1
  grp <- quote(list(TRIP_ID,CALENDAR_DATE,VEHICLE_ID))
  
  DT
  
  #clean DT
  DT <- DT[
    #order by trip, then veh_ID, then transit_Day, then Time
    order(TRIP_ID,VEHICLE_ID,CALENDAR_DATE,Time)
    
    ][
      #remove non BYD
      Vehicle_ID %in% c(1899,1970:1999)
      
      ][
        #set dist_to_next, resetting at each new group
        ,`:=` (dist_to_next = c(diff(dist_traveled),0))
        ,grp
        
        ][
          #set mph for each point to point segment and get cumulative distance traveled, by group
          ,`:=`(mph = c(NA_real_,diff(dist_traveled)/diff(as.integer(Time))/1.466667)
                ,cum_dist = cumsum(abs(dist_to_next)))
          ,grp
          
          ][
            #flag negative
            ,neg := dist_to_next <= 0
            
            ][
              #create rleid
              ,rl := rleid(neg),by = grp
              
              ][
                #group by both and get length of each run
                ,rl_len := .N,.(Trip,Transit_Day,Vehicle_ID,rl)
                
                ][
                  #filter rows that have more than 3 and are NEG
                  !(neg == T & rl_len >= 4)
                  ][
                    #redo calcs
                    ,`:=` (dist_to_next = c(diff(dist_traveled),0))
                    ,grp
                    ][
                      ,`:=`(mph = c(NA_real_,diff(dist_traveled)/diff(as.integer(Time))/1.466667)
                            ,cum_dist = cumsum(abs(dist_to_next)))
                      ,grp
                      ][
                        #add cum_dist_flag
                        ,cum_dist_ok := cum_dist <= seglen | cum_dist-seglen < 20
                        ][
                          #filter
                          cum_dist_ok == T
                          ][
                            #do these calcs one more time
                            ,`:=` (dist_to_next = c(diff(dist_traveled),0))
                            ,grp
                            ][
                              ,`:=`(mph = c(NA_real_,diff(dist_traveled)/diff(as.integer(Time))/1.466667)
                                    ,cum_dist = cumsum(abs(dist_to_next)))
                              ,grp
                              ]
  
  
  DT <- DT[
    #convert to feet, get min and max time
    ,.(miles_traveled = max(cum_dist)/5280 #convert to feet
       ,start_time = min(Time) 
       ,end_time = max(Time)
       ,pings = .N)
    ,grp
    
    ][
      #convert to hours
      ,time := as.numeric((end_time - start_time)/60/60) #convert to hours
      ][
        #convert to speed
        ,speed := miles_traveled/time #get speed
        ][
          #remove records with very low ping #
          pings >= 10
          ]
}) %>% `names<-`(shape_segments$lane_type) %>%
  rbindlist(idcol = T)

VMH_speed_DT$direction <- direction

