# mike nugent, jr.
# data analyst
# indygo
library(DBI)
library(data.table)
library(lubridate)
library(stringr)
library(dplyr)

# import PassCount --------------------------------------------------------
con <- DBI::dbConnect(odbc::odbc(), Driver = "SQL Server", Server = "IPTC-TMDATAMART\\TMDATAMART", 
                      Database = "TMDATAMART", Port = 1433)



tbl(con,sql("select top 10 * from PASSENGER_COUNT
            WHERE CALENDAR_ID > 120180210.0
            AND CALENDAR_ID < 1201802112.0
            AND VEHICLE_ID IS NOT NULL
            ORDER BY VEHICLE_ID,MESSAGE_TIME"))

# read in old routes
old_routes_on_rl <- old_routes_on_segment %>% rbindlist(use.names = T) %>% pull() %>% as.character()

# get routes
ROUTE_raw <- tbl(con,"ROUTE") %>% 
  select(ROUTE_ID, ROUTE_ABBR) %>%
  filter(ROUTE_ABBR %in% !!old_routes_on_rl) %>%
  collect() %>%
  setDT() %>%
  setkey(ROUTE_ID)


#set query
PASSENGER_COUNT_query_1 <- tbl(
  con
  ,sql(
    paste0(
      "SELECT CALENDAR_ID                                     
      ,SCHED_DIST_FROM_LAST_GEO_NODE
      ,BLOCK_STOP_ORDER
      ,LATITUDE
      ,LONGITUDE
      ,MESSAGE_TIME
      ,ARRIVAL_TIME
      ,DEPARTURE_TIME
      ,ROUTE_ID                                      
      ,GEO_NODE_ID
      ,BOARD
      ,ALIGHT
      ,VEHICLE_ID
      ,TRIP_ID
      ,PATTERN_ID
      ,RUN_ID
      ,BLOCK_ID
      ,WORK_PIECE_ID
      ,ROUTE_DIRECTION_ID
      
      from PASSENGER_COUNT
      WHERE CALENDAR_ID > 120171001.0
      and CALENDAR_ID < 120180228.0
      and REVENUE_ID = 'R'
      and PASSENGER_COUNT.TRIP_ID IS NOT NULL
      and PASSENGER_COUNT.VEHICLE_ID IN (SELECT dbo.SCHEDULE.VEHICLE_ID
      FROM dbo.SCHEDULE with (nolock)
      WHERE PASSENGER_COUNT.CALENDAR_ID = dbo.SCHEDULE.CALENDAR_ID
      AND PASSENGER_COUNT.TIME_TABLE_VERSION_ID=dbo.SCHEDULE.TIME_TABLE_VERSION_ID
      AND PASSENGER_COUNT.ROUTE_ID = dbo.SCHEDULE.ROUTE_ID
      AND PASSENGER_COUNT.ROUTE_DIRECTION_ID = dbo.SCHEDULE.ROUTE_DIRECTION_ID
      AND PASSENGER_COUNT.TRIP_ID = dbo.SCHEDULE.TRIP_ID
      AND PASSENGER_COUNT.GEO_NODE_ID = dbo.SCHEDULE.GEO_NODE_ID)"
    )#end paste0
  )#endsql
)#endtbl

#read it in
pass_count_raw <- PASSENGER_COUNT_query_1 %>% collect() %>% setDT()



#write it so we can save it for later use
fwrite(pass_count_raw,"data//processed//pass_count_raw.csv")  
#clean it up

#remove non rl routes
pass_count_clean <- pass_count_raw[ROUTE_ID %in% ROUTE_raw$ROUTE_ID]

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

ROUTE_DIRECTION_raw <- tbl(con,"ROUTE_DIRECTION") %>%
  collect() %>%
  setDT() %>%
  setkey(ROUTE_DIRECTION_ID)

#join it all to pass_count

pass_count_clean[CALENDAR_raw
                 ,on = "CALENDAR_ID"
                 ,names(CALENDAR_raw) := mget(paste0("i.",names(CALENDAR_raw)))
                 ][ROUTE_raw
                   , on = "ROUTE_ID"
                   ,names(ROUTE_raw) := mget(paste0("i.",names(ROUTE_raw)))
                   ][GEO_NODE_raw
                     , on = "GEO_NODE_ID"
                     ,names(GEO_NODE_raw) := mget(paste0("i.",names(GEO_NODE_raw)))
                     ][VEHICLE_ID_raw
                       ,on = "VEHICLE_ID"
                       ,names(VEHICLE_ID_raw) := mget(paste0("i.",names(VEHICLE_ID_raw)))
                       ][ROUTE_DIRECTION_raw
                         ,on = "ROUTE_DIRECTION_ID"
                         ,names(ROUTE_DIRECTION_raw) := mget(paste0("i.",names(ROUTE_DIRECTION_raw)))
                         ]

fwrite(pass_count_clean,"data//processed//pass_count_joined.csv")

#grab VMH
con2 <- DBI::dbConnect(odbc::odbc(), Driver = "SQL Server", Server = "REPSQLP01VW", 
                       Database = "TransitAuthority_IndyGo_Reporting", 
                       Port = 1433)

VMH_Query <- tbl(con2,sql(paste0("select a.Time
,a.Route
,Boards
,Alights
,Stop_Dwell_Time
,Trip
,Speed
,Previous_Stop_Id
,Vehicle_ID
,Stop_Name
,Stop_Id
,Inbound_Outbound
,Arrival_Time
,Departure_Time
,Latitude
,Longitude
,GPSStatus
,StationaryStatus
,StationaryDuration
from avl.Vehicle_Message_History a (nolock)
left join avl.Vehicle_Avl_History b
on a.Avl_History_Id = b.Avl_History_Id
where a.Route like '90%'
and a.Time > '20191001'
and a.Time < DATEADD(day,1,'20200229')
and GPSStatus = 2")))

# read in VMH data --------------------------------------------------------
VMH_Raw <- VMH_Query %>% collect() %>% setDT()

VMH_Raw[,.N,Stop_Dwell_Time]

VMH_Raw[, c("ClockTime","Date") := list(str_sub(Time, 12, 19),str_sub(Time, 1, 10))
        ][, DateTest := ifelse(ClockTime<"03:00:00",1,0)
          ][, Transit_Day := ifelse(DateTest ==1
                                    ,as_date(Date)-1
                                    ,as_date(Date))
            ][,Transit_Day := as_date("1970-01-01")+days(Transit_Day)]


VMH_Raw <- VMH_Raw[Transit_Day >= "2019-10-01" & 
                     Transit_Day <= "2020-02-29"]

fwrite(VMH_Raw,"data//processed//VMH_Raw.csv")

con3 <- DBI::dbConnect(odbc::odbc(), Driver = "SQL Server", Server = "AVAILDWHP01VW",
                       Database = "DW_IndyGo", Port = 1433)

DimRoute90 <- tbl(con3, "DimRoute") %>%
  filter(RouteReportLabel %in% c("90", "901", "902")) %>%
  collect() %>%
  setDT() %>%
  setkey("RouteKey")



VMH_Raw[!Stop_Id == 0 & Alights > 0][Stop_Dwell_Time > 0,.N,Stop_Dwell_Time] %>%
  sample_n(10000) %>% 
  plot()


VMH_Raw <- fread("data//processed//VMH_raw.csv")

VMH_Raw[!Stop_Id == 0 &
  Speed > 0 & Stop_Dwell_Time > 0
  ,.(Speed,Stop_Dwell_Time)
] %>% sample_n(100000) %>%
  plot()

VMH_Raw[!Stop_Id == 0 &
  Speed > 0 & Stop_Dwell_Time > 0
  ,.(Stop_Dwell_Time,Speed)
  ] %>% sample_n(100000) %>%
  plot()





# testing here ------------------------------------------------------------


test_pass_count <- tbl(con,"PASSENGER_COUNT") %>%
  filter(VEHICLE_ID == 590),
         CALENDAR_ID == 120171127) %>%
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


pass_count_joined_raw[is.na(LATITUDE)] %>% View()

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


