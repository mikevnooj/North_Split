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

# functions ---------------------------------------------------------------
seq_along_by <- function(x, by=1L, from = 1L) (seq_along(x) - 1L) * by + from

snap_points_to_line <- function(points,line){
  
  points_align <- st_nearest_points(points,line)%>%
    st_cast("POINT")
  
  points_new_geometry <- points_align[c(seq(2, length(points_align),by = 2))]
  
  points_align_end <- points %>%
    st_set_geometry(points_new_geometry)
  
}


# read in current GTFS ----------------------------------------------------
current_gtfs <- read_gtfs("data//GTFS//2006_2//2006_2_GTFS.zip")

# convert to sf
current_gtfs_sf <- gtfs_as_sf(current_gtfs) %>% set_hms_times %>% set_date_service_table() %>% set_servicepattern()

#remove trips with no shapes
current_gtfs_sf$trips %<>%
  filter(!is.na(shape_id))


# create convenience objects ----------------------------------------------
# merge trips and shapes
current_trips_and_shapes_sf <- current_gtfs_sf$shapes %>% 
  right_join(current_gtfs_sf$trips)

# join routes as well
current_route_trips_and_shapes_sf <- current_trips_and_shapes_sf %>%
  right_join(current_gtfs_sf$routes)

#stops df for ease of use
current_stops_sf <- Filter(function(x)!all(is.na(x)),current_gtfs_sf$stops) 

#get SB stops
#pairs are Broad Ripple to 42nd, 38th to Statehouse, and New Jersey to University
endpoint_stop_names_SB_sf <- c("Broad.*SB","42.*SB","38.*SB","State.*SB","New.*SB","Uni.*SB")

#get NB stops, same pairs, in reverse
endpoint_stop_names_NB_sf <- c("Broad.*NB","42.*NB","38.*NB","State.*NB","New.*NB","Uni.*NB")

#get segment ends SB
shape_endpoints_SB_sf <- current_stops_sf %>%
  filter(grepl(paste(endpoint_stop_names_SB_sf,collapse = "|"),stop_name))

#get segment ends NB
shape_endpoints_NB_sf <- current_stops_sf %>%
  filter(grepl(paste(endpoint_stop_names_NB_sf,collapse = "|"),stop_name))

#viz
#see if it worked
shape_endpoints_SB_sf %>% 
  leaflet() %>% 
  addFeatures() %>% 
  addTiles()

shape_endpoints_NB_sf %>% 
  leaflet() %>% 
  addFeatures() %>% 
  addTiles()

#get 90 shapes
SB_90_sf <- current_route_trips_and_shapes_sf %>% filter(route_short_name == 90 & direction_id == 1)

NB_90_sf <- current_route_trips_and_shapes_sf %>% filter(route_short_name == 90 & direction_id == 0)

#get one shape that has all route points of interest
SB_route_point_sf <- current_gtfs$shapes %>%
  st_as_sf(coords = c("shape_pt_lon","shape_pt_lat"),crs = 4326) %>%
  filter(shape_id == SB_90_sf$shape_id[1]) %>%
  arrange(shape_pt_sequence)

NB_route_point_sf <- current_gtfs$shapes %>%
  st_as_sf(coords = c("shape_pt_lon","shape_pt_lat"),crs = 4326) %>%
  filter(shape_id == NB_90_sf$shape_id[1]) %>%
  arrange(shape_pt_sequence)


# match points to endpoints, then get shape segments ----------------------
#then match those points to the endpoints so we can match indices
#use st_nn to make sure we get the route on the way down, not on the way back up
SB_route_point_indices <- st_nn(shape_endpoints_SB_sf,SB_route_point_sf,k = 2) %>%
  lapply(min) %>%
  unlist()

NB_route_point_indices <- st_nn(shape_endpoints_NB_sf,NB_route_point_sf,k = 2) %>%
  lapply(min) %>%
  unlist()

#this will be an input
SB_n <- nrow(shape_endpoints_SB_sf)

NB_n <- nrow(shape_endpoints_NB_sf)

SB_shape_list <- lapply(
  X = seq_along_by(1:(SB_n/2),2), function(x){
    
    SB_route_segment_start <- SB_route_point_indices[x]
    SB_route_segment_end <- SB_route_point_indices[x+1]
    
    linestring <-  SB_route_point_sf[SB_route_segment_start:SB_route_segment_end,] %>%
      arrange(shape_pt_sequence) %>%
      st_coordinates() %>%
      st_linestring() %>%
      st_sfc(crs = 4326)
    return(linestring) 
  }
)

NB_shape_list <- lapply(
  X = seq_along_by(1:(NB_n/2),2), function(x){
    
    NB_route_segment_start <- NB_route_point_indices[x]
    NB_route_segment_end <- NB_route_point_indices[x+1]
    
    linestring <-  NB_route_point_sf[NB_route_segment_start:NB_route_segment_end,] %>%
      arrange(shape_pt_sequence) %>%
      st_coordinates() %>%
      st_linestring() %>%
      st_sfc(crs = 4326)
    return(linestring) 
  }
)

SB_shape_segments <- lapply(SB_shape_list,st_as_sf)%>%
  do.call(rbind,.)%>%
  select(geometry = x) %>%
  mutate(lane_type = c("shared-center","bi-directional","none"))

NB_shape_segments <- lapply(NB_shape_list,st_as_sf)%>%
  do.call(rbind,.)%>%
  select(geometry = x) %>%
  mutate(lane_type = c("shared-center","bi-directional","none"))

#viz
SB_shape_list[[3]] %>%
  leaflet() %>% 
  addTiles() %>%
  addPolylines(color = "red", weight = 3)


SB_90_sf[1,] %>% 
  leaflet() %>% 
  addTiles() %>% 
  addPolylines(weight = 1) %>%
  addFeatures(SB_route_point_sf, radius = 1,label = ~shape_pt_sequence)

NB_shape_list[[3]] %>%
  leaflet() %>% 
  addTiles() %>%
  addPolylines(color = "red", weight = 3)


NB_90_sf[1,] %>% 
  leaflet() %>% 
  addTiles() %>% 
  addPolylines(weight = 1) %>%
  addFeatures(SB_route_point_sf, radius = 1,label = ~shape_pt_sequence)


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

# get nearest old stops to new route --------------------------------------
SB_nn_oldstops <-st_nn(SB_shape_segments,old_stops_sf,k = nrow(old_stops_sf),maxdist = 15,returnDist = T)

NB_nn_oldstops <-st_nn(NB_shape_segments,old_stops_sf,k = nrow(old_stops_sf),maxdist = 15,returnDist = T)

#viz
old_stops_sf[unlist(SB_nn_oldstops$nn),] %>% 
  leaflet() %>%
  addFeatures() %>%
  addTiles()

old_stops_sf[unlist(NB_nn_oldstops$nn),] %>% 
  leaflet() %>%
  addFeatures() %>%
  addTiles()

st_connect(SB_shape_segments,old_stops_sf,ids = SB_nn_oldstops$nn)%>%
  leaflet() %>%
  addFeatures() %>%
  addTiles()

st_connect(NB_shape_segments,old_stops_sf,ids = NB_nn_oldstops$nn)%>%
  leaflet() %>%
  addFeatures() %>%
  addTiles()


# get old routes along red line -------------------------------------------

SB_old_stops_on_rl <- old_stops_sf[unlist(SB_nn_oldstops$nn),]


NB_old_stops_on_rl <- old_stops_sf[unlist(NB_nn_oldstops$nn),]

SB_old_routes_on_rl <- SB_old_stops_on_rl %>%
  left_join(old_gtfs_sf$stop_times) %>%
  left_join(old_route_trips_and_shapes_sf %>% st_drop_geometry()) %>% 
  st_drop_geometry() %>%
  distinct(route_short_name) %>%
  pull()

NB_old_routes_on_rl <- NB_old_stops_on_rl %>%
  left_join(old_gtfs_sf$stop_times) %>%
  left_join(old_route_trips_and_shapes_sf %>% st_drop_geometry()) %>% 
  st_drop_geometry() %>%
  distinct(route_short_name) %>%
  pull()

#viz
old_route_trips_and_shapes_sf %>%
  group_by(shape_id) %>%
  slice(1) %>%
  ungroup() %>%
  filter(route_short_name %in% SB_old_routes_on_rl) %>%
  ungroup() %>%
  leaflet() %>%
  addFeatures() %>%
  addTiles()

old_route_trips_and_shapes_sf %>%
  group_by(shape_id) %>%
  slice(1) %>%
  ungroup() %>%
  filter(route_short_name %in% NB_old_routes_on_rl) %>%
  ungroup() %>%
  leaflet() %>%
  addFeatures() %>%
  addTiles()

# import PassCount and VMH ------------------------------------------------

pass_count_raw <- fread("data//processed//pass_count_raw.csv")

#pass count clean is routes 90, 901, 902
pass_count_clean <- fread("data//processed//pass_count_clean.csv")

#import VMH
VMH_Raw <- fread("data//processed//VMH_Raw.csv")

VMH_Raw[,Time := fasttime::fastPOSIXct(Time,tz = "UTC")
        ][,Transit_Day := as.IDate(Transit_Day)
          ]

SB_VMH_sf <- VMH_Raw[Inbound_Outbound == 1] %>%
  st_as_sf(
    coords = c("Longitude","Latitude")
    ,crs = 4326 
  )

NB_VMH_sf <- VMH_Raw[Inbound_Outbound == 0] %>%
  st_as_sf(
    coords = c("Longitude","Latitude")
    ,crs = 4326 
  )

# bastard row names
SB_VMH_sf$rn <- seq_along(1:nrow(SB_VMH_sf))

NB_VMH_sf$rn <- seq_along(1:nrow(NB_VMH_sf))

# match VMH to SB_shape_segments ------------------------------------------
# get coordinate matrices

SB_VMH_coords <- do.call(
  rbind
  ,st_geometry(SB_VMH_sf)
)#end do.call

#get route segment coords
SB_shared_center_coords <- do.call(rbind, st_geometry(SB_shape_segments[1,]))
SB_bi_direction_coords <- do.call(rbind, st_geometry(SB_shape_segments[2,]))
SB_none_coords <- do.call(rbind, st_geometry(SB_shape_segments[3,]))

# fast nearest neighbour search
SB_shared_center_closest_VMH_ind <- nn2(
  SB_VMH_coords
  ,SB_shared_center_coords
  ,k = 35000
  ,searchtype = "radius"
  ,radius = 0.00001*(20/1.11)
)%>% 
  sapply(cbind) %>%
  as_tibble() %>%
  distinct(nn.idx) %>% #get just the indices
  pull() %>%
  sort()

SB_bi_direction_closest_VMH_ind <- nn2(
  SB_VMH_coords
  ,SB_bi_direction_coords
  ,k = 35000
  ,searchtype = "radius"
  ,radius = 0.00001*(20/1.11)
) %>% 
  sapply(cbind) %>%
  as_tibble() %>%
  distinct(nn.idx) %>% #get just the indices
  pull() %>%
  sort()

SB_none_closest_VMH_ind <- nn2(
  SB_VMH_coords
  ,SB_none_coords
  ,k = 35000
  ,searchtype = "radius"
  ,radius = 0.00001*(20/1.11)
)%>% 
  sapply(cbind) %>%
  as_tibble() %>% #creates df
  distinct(nn.idx) %>% #get just the indices
  pull() %>%
  sort()


SB_shared_center_VMH_sf <- SB_VMH_sf %>%
  as.data.table(keep.rownames = T) %>%
  setkey(rn) %>%
  .[SB_shared_center_closest_VMH_ind] %>%
  st_as_sf()

SB_bi_direction_closest_VMH_sf <- SB_VMH_sf %>%
  as.data.table(keep.rownames = T) %>%
  setkey(rn) %>%
  .[SB_bi_direction_closest_VMH_ind] %>%
  st_as_sf()

SB_none_closest_VMH_sf <- SB_VMH_sf %>%
  as.data.table(keep.rownames = T) %>%
  setkey(rn) %>%
  .[SB_none_closest_VMH_ind] %>%
  st_as_sf()

# viz
SB_shared_center_VMH_sf %>% sample_n(5000) %>% leaflet %>% addTiles() %>% addFeatures()
SB_bi_direction_closest_VMH_sf %>% sample_n(5000) %>% leaflet %>% addTiles() %>% addFeatures()
SB_none_closest_VMH_sf %>% sample_n(5000) %>% leaflet %>% addTiles() %>% addFeatures()

# match vmh to NB_shape_segments ------------------------------------------


# get coordinate matrices

NB_VMH_coords <- do.call(
  rbind
  ,st_geometry(NB_VMH_sf)
)#end do.call

#get route segment coords
NB_shared_center_coords <- do.call(rbind, st_geometry(NB_shape_segments[1,]))
NB_bi_direction_coords <- do.call(rbind, st_geometry(NB_shape_segments[2,]))
NB_none_coords <- do.call(rbind, st_geometry(NB_shape_segments[3,]))

# fast nearest neighbour search
NB_shared_center_closest_VMH_ind <- nn2(
  NB_VMH_coords
  ,NB_shared_center_coords
  ,k = 35000
  ,searchtype = "radius"
  ,radius = 0.00001*(20/1.11)
)%>% 
  sapply(cbind) %>%
  as_tibble() %>%
  distinct(nn.idx) %>% #get just the indices
  pull() %>%
  sort()

NB_bi_direction_closest_VMH_ind <- nn2(
  NB_VMH_coords
  ,NB_bi_direction_coords
  ,k = 35000
  ,searchtype = "radius"
  ,radius = 0.00001*(20/1.11)
) %>% 
  sapply(cbind) %>%
  as_tibble() %>%
  distinct(nn.idx) %>% #get just the indices
  pull() %>%
  sort()

NB_none_closest_VMH_ind <- nn2(
  NB_VMH_coords
  ,NB_none_coords
  ,k = 35000
  ,searchtype = "radius"
  ,radius = 0.00001*(20/1.11)
)%>% 
  sapply(cbind) %>%
  as_tibble() %>% #creates df
  distinct(nn.idx) %>% #get just the indices
  pull() %>%
  sort()


NB_shared_center_VMH_sf <- NB_VMH_sf %>%
  as.data.table(keep.rownames = T) %>%
  setkey(rn) %>%
  .[NB_shared_center_closest_VMH_ind] %>%
  st_as_sf()

NB_bi_direction_closest_VMH_sf <- NB_VMH_sf %>%
  as.data.table(keep.rownames = T) %>%
  setkey(rn) %>%
  .[NB_bi_direction_closest_VMH_ind] %>%
  st_as_sf()

NB_none_closest_VMH_sf <- NB_VMH_sf %>%
  as.data.table(keep.rownames = T) %>%
  setkey(rn) %>%
  .[NB_none_closest_VMH_ind] %>%
  st_as_sf()

# viz
NB_shared_center_VMH_sf %>% sample_n(5000) %>% leaflet %>% addTiles() %>% addFeatures()
NB_bi_direction_closest_VMH_sf %>% sample_n(5000) %>% leaflet %>% addTiles() %>% addFeatures()
NB_none_closest_VMH_sf %>% sample_n(5000) %>% leaflet %>% addTiles() %>% addFeatures()


 # snap points to line -----------------------------------------------------


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

# functions ---------------------------------------------------------------
seq_along_by <- function(x, by=1L, from = 1L) (seq_along(x) - 1L) * by + from

snap_points_to_line <- function(points,line){
  
  points_align <- st_nearest_points(points,line)%>%
    st_cast("POINT")
  
  points_new_geometry <- points_align[c(seq(2, length(points_align),by = 2))]
  
  points_align_end <- points %>%
    st_set_geometry(points_new_geometry)
  
}


# read in current GTFS ----------------------------------------------------
current_gtfs <- read_gtfs("data//GTFS//2006_2//2006_2_GTFS.zip")

# convert to sf
current_gtfs_sf <- gtfs_as_sf(current_gtfs) %>% set_hms_times %>% set_date_service_table() %>% set_servicepattern()

#remove trips with no shapes
current_gtfs_sf$trips %<>%
  filter(!is.na(shape_id))


# create convenience objects ----------------------------------------------
# merge trips and shapes
current_trips_and_shapes_sf <- current_gtfs_sf$shapes %>% 
  right_join(current_gtfs_sf$trips)

# join routes as well
current_route_trips_and_shapes_sf <- current_trips_and_shapes_sf %>%
  right_join(current_gtfs_sf$routes)

#stops df for ease of use
current_stops_sf <- Filter(function(x)!all(is.na(x)),current_gtfs_sf$stops) 

#get SB stops
#pairs are Broad Ripple to 42nd, 38th to Statehouse, and New Jersey to University
endpoint_stop_names_SB_sf <- c("Broad.*SB","42.*SB","38.*SB","State.*SB","New.*SB","Uni.*SB")

#get NB stops, same pairs, in reverse
endpoint_stop_names_NB_sf <- c("Broad.*NB","42.*NB","38.*NB","State.*NB","New.*NB","Uni.*NB")

#get segment ends SB
shape_endpoints_SB_sf <- current_stops_sf %>%
  filter(grepl(paste(endpoint_stop_names_SB_sf,collapse = "|"),stop_name))

#get segment ends NB
shape_endpoints_NB_sf <- current_stops_sf %>%
  filter(grepl(paste(endpoint_stop_names_NB_sf,collapse = "|"),stop_name))

#viz
#see if it worked
shape_endpoints_SB_sf %>% 
  leaflet() %>% 
  addFeatures() %>% 
  addTiles()

shape_endpoints_NB_sf %>% 
  leaflet() %>% 
  addFeatures() %>% 
  addTiles()

#get 90 shapes
SB_90_sf <- current_route_trips_and_shapes_sf %>% filter(route_short_name == 90 & direction_id == 1)

NB_90_sf <- current_route_trips_and_shapes_sf %>% filter(route_short_name == 90 & direction_id == 0)

#get one shape that has all route points of interest
SB_route_point_sf <- current_gtfs$shapes %>%
  st_as_sf(coords = c("shape_pt_lon","shape_pt_lat"),crs = 4326) %>%
  filter(shape_id == SB_90_sf$shape_id[1]) %>%
  arrange(shape_pt_sequence)

NB_route_point_sf <- current_gtfs$shapes %>%
  st_as_sf(coords = c("shape_pt_lon","shape_pt_lat"),crs = 4326) %>%
  filter(shape_id == NB_90_sf$shape_id[1]) %>%
  arrange(shape_pt_sequence)


# match points to endpoints, then get shape segments ----------------------
#then match those points to the endpoints so we can match indices
#use st_nn to make sure we get the route on the way down, not on the way back up
SB_route_point_indices <- st_nn(shape_endpoints_SB_sf,SB_route_point_sf,k = 2) %>%
  lapply(min) %>%
  unlist()

NB_route_point_indices <- st_nn(shape_endpoints_NB_sf,NB_route_point_sf,k = 2) %>%
  lapply(min) %>%
  unlist()

#this will be an input
SB_n <- nrow(shape_endpoints_SB_sf)

NB_n <- nrow(shape_endpoints_NB_sf)

SB_shape_list <- lapply(
  X = seq_along_by(1:(SB_n/2),2), function(x){
    
    SB_route_segment_start <- SB_route_point_indices[x]
    SB_route_segment_end <- SB_route_point_indices[x+1]
    
    linestring <-  SB_route_point_sf[SB_route_segment_start:SB_route_segment_end,] %>%
      arrange(shape_pt_sequence) %>%
      st_coordinates() %>%
      st_linestring() %>%
      st_sfc(crs = 4326)
    return(linestring) 
  }
)

NB_shape_list <- lapply(
  X = seq_along_by(1:(NB_n/2),2), function(x){
    
    NB_route_segment_start <- NB_route_point_indices[x]
    NB_route_segment_end <- NB_route_point_indices[x+1]
    
    linestring <-  NB_route_point_sf[NB_route_segment_start:NB_route_segment_end,] %>%
      arrange(shape_pt_sequence) %>%
      st_coordinates() %>%
      st_linestring() %>%
      st_sfc(crs = 4326)
    return(linestring) 
  }
)

SB_shape_segments <- lapply(SB_shape_list,st_as_sf)%>%
  do.call(rbind,.)%>%
  select(geometry = x) %>%
  mutate(lane_type = c("shared-center","bi-directional","none"))

NB_shape_segments <- lapply(NB_shape_list,st_as_sf)%>%
  do.call(rbind,.)%>%
  select(geometry = x) %>%
  mutate(lane_type = c("shared-center","bi-directional","none"))

#viz
SB_shape_list[[3]] %>%
  leaflet() %>% 
  addTiles() %>%
  addPolylines(color = "red", weight = 3)


SB_90_sf[1,] %>% 
  leaflet() %>% 
  addTiles() %>% 
  addPolylines(weight = 1) %>%
  addFeatures(SB_route_point_sf, radius = 1,label = ~shape_pt_sequence)

NB_shape_list[[3]] %>%
  leaflet() %>% 
  addTiles() %>%
  addPolylines(color = "red", weight = 3)


NB_90_sf[1,] %>% 
  leaflet() %>% 
  addTiles() %>% 
  addPolylines(weight = 1) %>%
  addFeatures(SB_route_point_sf, radius = 1,label = ~shape_pt_sequence)


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

# get nearest old stops to new route --------------------------------------
SB_nn_oldstops <-st_nn(SB_shape_segments,old_stops_sf,k = nrow(old_stops_sf),maxdist = 15,returnDist = T)

NB_nn_oldstops <-st_nn(NB_shape_segments,old_stops_sf,k = nrow(old_stops_sf),maxdist = 15,returnDist = T)

#viz
old_stops_sf[unlist(SB_nn_oldstops$nn),] %>% 
  leaflet() %>%
  addFeatures() %>%
  addTiles()

old_stops_sf[unlist(NB_nn_oldstops$nn),] %>% 
  leaflet() %>%
  addFeatures() %>%
  addTiles()

st_connect(SB_shape_segments,old_stops_sf,ids = SB_nn_oldstops$nn)%>%
  leaflet() %>%
  addFeatures() %>%
  addTiles()

st_connect(NB_shape_segments,old_stops_sf,ids = NB_nn_oldstops$nn)%>%
  leaflet() %>%
  addFeatures() %>%
  addTiles()


# get old routes along red line -------------------------------------------

SB_old_stops_on_rl <- old_stops_sf[unlist(SB_nn_oldstops$nn),]


NB_old_stops_on_rl <- old_stops_sf[unlist(NB_nn_oldstops$nn),]

SB_old_routes_on_rl <- SB_old_stops_on_rl %>%
  left_join(old_gtfs_sf$stop_times) %>%
  left_join(old_route_trips_and_shapes_sf %>% st_drop_geometry()) %>% 
  st_drop_geometry() %>%
  distinct(route_short_name) %>%
  pull()

NB_old_routes_on_rl <- NB_old_stops_on_rl %>%
  left_join(old_gtfs_sf$stop_times) %>%
  left_join(old_route_trips_and_shapes_sf %>% st_drop_geometry()) %>% 
  st_drop_geometry() %>%
  distinct(route_short_name) %>%
  pull()

#viz
old_route_trips_and_shapes_sf %>%
  group_by(shape_id) %>%
  slice(1) %>%
  ungroup() %>%
  filter(route_short_name %in% SB_old_routes_on_rl) %>%
  ungroup() %>%
  leaflet() %>%
  addFeatures() %>%
  addTiles()

old_route_trips_and_shapes_sf %>%
  group_by(shape_id) %>%
  slice(1) %>%
  ungroup() %>%
  filter(route_short_name %in% NB_old_routes_on_rl) %>%
  ungroup() %>%
  leaflet() %>%
  addFeatures() %>%
  addTiles()

# import PassCount and VMH ------------------------------------------------

pass_count_raw <- fread("data//processed//pass_count_raw.csv")

#pass count clean is routes 90, 901, 902
pass_count_clean <- fread("data//processed//pass_count_clean.csv")

#import VMH
VMH_Raw <- fread("data//processed//VMH_Raw.csv")

VMH_Raw[,Time := fasttime::fastPOSIXct(Time,tz = "UTC")
        ][,Transit_Day := as.IDate(Transit_Day)
          ]

SB_VMH_sf <- VMH_Raw[Inbound_Outbound == 1] %>%
  st_as_sf(
    coords = c("Longitude","Latitude")
    ,crs = 4326 
  )

NB_VMH_sf <- VMH_Raw[Inbound_Outbound == 0] %>%
  st_as_sf(
    coords = c("Longitude","Latitude")
    ,crs = 4326 
  )

# bastard row names
SB_VMH_sf$rn <- seq_along(1:nrow(SB_VMH_sf))

NB_VMH_sf$rn <- seq_along(1:nrow(NB_VMH_sf))

# match VMH to SB_shape_segments ------------------------------------------
# get coordinate matrices

SB_VMH_coords <- do.call(
  rbind
  ,st_geometry(SB_VMH_sf)
)#end do.call

#get route segment coords
SB_shared_center_coords <- do.call(rbind, st_geometry(SB_shape_segments[1,]))
SB_bi_direction_coords <- do.call(rbind, st_geometry(SB_shape_segments[2,]))
SB_none_coords <- do.call(rbind, st_geometry(SB_shape_segments[3,]))

# fast nearest neighbour search
SB_shared_center_closest_VMH_ind <- nn2(
  SB_VMH_coords
  ,SB_shared_center_coords
  ,k = 35000
  ,searchtype = "radius"
  ,radius = 0.00001*(20/1.11)
)%>% 
  sapply(cbind) %>%
  as_tibble() %>%
  distinct(nn.idx) %>% #get just the indices
  pull() %>%
  sort()

SB_bi_direction_closest_VMH_ind <- nn2(
  SB_VMH_coords
  ,SB_bi_direction_coords
  ,k = 35000
  ,searchtype = "radius"
  ,radius = 0.00001*(20/1.11)
) %>% 
  sapply(cbind) %>%
  as_tibble() %>%
  distinct(nn.idx) %>% #get just the indices
  pull() %>%
  sort()

SB_none_closest_VMH_ind <- nn2(
  SB_VMH_coords
  ,SB_none_coords
  ,k = 35000
  ,searchtype = "radius"
  ,radius = 0.00001*(20/1.11)
)%>% 
  sapply(cbind) %>%
  as_tibble() %>% #creates df
  distinct(nn.idx) %>% #get just the indices
  pull() %>%
  sort()


SB_shared_center_VMH_sf <- SB_VMH_sf %>%
  as.data.table(keep.rownames = T) %>%
  setkey(rn) %>%
  .[SB_shared_center_closest_VMH_ind] %>%
  st_as_sf()

SB_bi_direction_closest_VMH_sf <- SB_VMH_sf %>%
  as.data.table(keep.rownames = T) %>%
  setkey(rn) %>%
  .[SB_bi_direction_closest_VMH_ind] %>%
  st_as_sf()

SB_none_closest_VMH_sf <- SB_VMH_sf %>%
  as.data.table(keep.rownames = T) %>%
  setkey(rn) %>%
  .[SB_none_closest_VMH_ind] %>%
  st_as_sf()

# viz
SB_shared_center_VMH_sf %>% sample_n(5000) %>% leaflet %>% addTiles() %>% addFeatures()
SB_bi_direction_closest_VMH_sf %>% sample_n(5000) %>% leaflet %>% addTiles() %>% addFeatures()
SB_none_closest_VMH_sf %>% sample_n(5000) %>% leaflet %>% addTiles() %>% addFeatures()

# match vmh to NB_shape_segments ------------------------------------------


# get coordinate matrices

NB_VMH_coords <- do.call(
  rbind
  ,st_geometry(NB_VMH_sf)
)#end do.call

#get route segment coords
NB_shared_center_coords <- do.call(rbind, st_geometry(NB_shape_segments[1,]))
NB_bi_direction_coords <- do.call(rbind, st_geometry(NB_shape_segments[2,]))
NB_none_coords <- do.call(rbind, st_geometry(NB_shape_segments[3,]))

# fast nearest neighbour search
NB_shared_center_closest_VMH_ind <- nn2(
  NB_VMH_coords
  ,NB_shared_center_coords
  ,k = 35000
  ,searchtype = "radius"
  ,radius = 0.00001*(20/1.11)
)%>% 
  sapply(cbind) %>%
  as_tibble() %>%
  distinct(nn.idx) %>% #get just the indices
  pull() %>%
  sort()

NB_bi_direction_closest_VMH_ind <- nn2(
  NB_VMH_coords
  ,NB_bi_direction_coords
  ,k = 35000
  ,searchtype = "radius"
  ,radius = 0.00001*(20/1.11)
) %>% 
  sapply(cbind) %>%
  as_tibble() %>%
  distinct(nn.idx) %>% #get just the indices
  pull() %>%
  sort()

NB_none_closest_VMH_ind <- nn2(
  NB_VMH_coords
  ,NB_none_coords
  ,k = 35000
  ,searchtype = "radius"
  ,radius = 0.00001*(30/1.11)
)%>% 
  sapply(cbind) %>%
  as_tibble() %>% #creates df
  distinct(nn.idx) %>% #get just the indices
  pull() %>%
  sort()


NB_shared_center_VMH_sf <- NB_VMH_sf %>%
  as.data.table(keep.rownames = T) %>%
  setkey(rn) %>%
  .[NB_shared_center_closest_VMH_ind] %>%
  st_as_sf()

NB_bi_direction_closest_VMH_sf <- NB_VMH_sf %>%
  as.data.table(keep.rownames = T) %>%
  setkey(rn) %>%
  .[NB_bi_direction_closest_VMH_ind] %>%
  st_as_sf()

NB_none_closest_VMH_sf <- NB_VMH_sf %>%
  as.data.table(keep.rownames = T) %>%
  setkey(rn) %>%
  .[NB_none_closest_VMH_ind] %>%
  st_as_sf()

# viz
NB_shared_center_VMH_sf %>% sample_n(5000) %>% leaflet %>% addTiles() %>% addFeatures()
NB_bi_direction_closest_VMH_sf %>% sample_n(5000) %>% leaflet %>% addTiles() %>% addFeatures()
NB_none_closest_VMH_sf %>% sample_n(5000) %>% leaflet %>% addTiles() %>% addFeatures()





# snap points to line -----------------------------------------------------

snap_points_to_line(NB_none_closest_VMH_sf[] %>% sample_n(10000),NB_shape_segments[3,]) %>% 
  leaflet() %>% 
  addFeatures(color = "blue") %>% 
  addFeatures(NB_none_closest_VMH_sf[1:5000,], color = "red") %>% 
  addFeatures(NB_VMH_sf %>% st_crop(ymin = 39.70,ymax = 39.763093,xmax = -86.08216,xmin = -86.2158) %>% sample_n(5000), color = "black",label = ~geometry) %>% 
  addTiles()

plot(NB_shape_segments)
getwd()
