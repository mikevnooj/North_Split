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
lane_type <- c("shared-center","bi-directional","none")

direction <- "SB"

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
      stop_name %like% paste(endpoint_stop_names[1:2],collapse = "|") ~ "shared-center"
      ,stop_name %like% paste(endpoint_stop_names[3:4],collapse = "|") ~ "bi-directional"
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

#viz
old_route_trips_and_shapes_sf %>%
  group_by(shape_id) %>%
  slice(1) %>%
  ungroup() %>%
  filter(route_short_name %in% old_routes_on_segment) %>%
  ungroup() %>%
  leaflet() %>%
  addFeatures() %>%
  addTiles()


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
closest_VMH[[2]] %>%
  .[c(1,1350),]%>%
  leaflet() %>%
  addFeatures() %>%
  addFeatures(shape_segments[2,])%>%
  addTiles()

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

# get VMH closest to stops ------------------------------------------------
#get rl stops for each segment, to get closest vmh for each stop by trip, day, vehicle

RL_stops <- route_shape_sf %>%
  st_drop_geometry() %>%
  left_join(current_gtfs$stop_times) %>%
  left_join(current_gtfs_sf$stops) %>%
  select(names(current_gtfs_sf$stops)) %>%
  st_sf() %>%
  st_transform(7328)


# there be dragons here ---------------------------------------------------


yuck <- lapply(seq_along(1:nrow(shape_segments)),function(i){
  
  shape_buf <- shape_segments[i,] %>%
    st_transform(7328) %>%
    st_buffer(35)

  stops <- st_intersects(shape_buf,RL_stops)
  
  shape_seg_current_stops <- RL_stops[unlist(stops),]
  
  stop_coords <- shape_seg_current_stops %>% st_transform(4326) %>% st_coordinates()
  
  closest_VMH_coords <- st_coordinates(closest_VMH[[i]])
  
  nn2(
    closest_VMH_coords
    ,stop_coords
    ,searchtype = "radius"
    ,k = 35000
    ,radius = 0.00001*(15/1.11)
  ) %>%
    sapply(cbind) %>% #combine
    as_tibble() %>% #convert so we can pull
    distinct(nn.idx) %>%
    pull() %>%
    sort() %>%
    closest_VMH[[i]][.,]
  
    }) %>% `names<-`(shape_segments$lane_type)


closest_VMH_closest_to_RL_stops$none %>%
  data.table() %>%
  .[Trip == 525] %>% 
  .[order(Time)] %>%
  .[,`:=` (dist_to_next = c(diff(dist_traveled),0)
           ,time_to_next = c(NA_real_,diff(as.integer(Time)))),c("Transit_Day","Vehicle_ID","Trip")] %>%
  .[, `:=` (fps = dist_to_next/time_to_next),c("Transit_Day","Vehicle_ID","Trip")] %>%
  .[, `:=` (mph = fps*3600 / 5280, fps = NULL)] %>%
  .[dist_to_next > 0 & time_to_next > 0] %>%
  select(mph,dist_traveled,dist_to_next,everything()) %>% 
  View()


closest_VMH[[1]] %>% data.table() %>%
  #data.table(key = "Transit_Day,Trip,Vehicle_ID") %>%
  .[,grp := .GRP, by = .(Transit_Day,Trip,Vehicle_ID)] %>%
  .[]


# test --------------------------------------------------------------------

VMH_test <- closest_VMH[[1]] %>% data.table() %>%
  .[,grp := .GRP, .(Transit_Day,Trip,Vehicle_ID)]

split_list <- VMH_test %>% split(.$grp)

vmh_coords_list <- lapply(split_list, function(x){
  x %>% st_sf() %>% st_coordinates()
})

nn2(
  vmh_coords_list[[1]]
  ,stop_coords
  ,searchtype = "radius"
  ,k = 10
  ,radius = 0.00001*(50/1.11)
)

# break ------------------------------------------------------------------


indices <- lapply(vmh_coords_list,function(x){
  nn2(
    x[[1]] #THIS IS WHAT WE NEED FROM EACH GROUP
    ,stop_coords
    ,k = 10
  ) %>%
    sapply(cbind) %>% #combine
    as_tibble() %>% #convert so we can pull
    distinct(nn.idx) %>%
    pull() %>%
    sort()
})


names(split_list) <- NULL
names(indices) <- NULL

indices 

Map(function(x,y) x[y,],split_list,indices)



split_list[[1]] %>%
st_sf() %>%
leaflet() %>%
addFeatures() %>%
addFeatures(RL_stops %>% st_transform(4326),color = "red") %>%
addTiles()
























RL_sp <- RL_stops %>%
  as_Spatial() %>% 
  spTransform(CRS(st_crs(7328)$proj4string))

segment_sp <- as_Spatial(shape_segments[1,] %>% st_transform(7328))

segment_length <- gLength(segment_sp)

shape_dist_traveled <- gProject(segment_sp,VMH_sp)

closest_VMH[[i]]$dist_traveled <- shape_dist_traveled