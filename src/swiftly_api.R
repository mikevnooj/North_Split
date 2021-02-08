library(httr)
library(data.table)


key <- "7ab8a4d70d25d4fa727bf5fe68d7898e"
startDate <- "08-13-2020"
endDate <- "08-15-2020"

include_all_res <- GET(url = paste0("https://api.goswift.ly/otp/indygo/csv-export")
    ,add_headers(Authorization = key)
    ,query = list(startDate = startDate
                  ,endDate = endDate
                  ,lastStopOfTrip = "includeAll"
                  ,onlyScheduleAdherenceStops = T))


include_all <- fread(content(include_all_res,"text"))
include_all[route_short_name==90,.N,stop_name]


earlies_as_on_time_res <- GET(url = paste0("https://api.goswift.ly/otp/indygo/csv-export")
                       ,add_headers(Authorization = key)
                       ,query = list(startDate = startDate
                                     ,endDate = endDate
                                     ,lastStopOfTrip = "earliesAsOnTime"
                                     ,onlyScheduleAdherenceStops = T))

earlies_as_on_time <- fread(content(earlies_as_on_time_res,"text"))

ignore_last_stop_res <- GET(url = paste0("https://api.goswift.ly/otp/indygo/csv-export")
                                           ,add_headers(Authorization = key)
                                           ,query = list(startDate = startDate
                                                         ,endDate = endDate
                                                         ,lastStopOfTrip = "ignore"
                                                         ,onlyScheduleAdherenceStops = T))

ignore_last_stop <- fread(content(ignore_last_stop_res,"text"))

fwrite(include_all,"data//swiftly//include_all.csv")
fwrite(earlies_as_on_time,"data//swiftly//earlies_as_on_time.csv")
fwrite(ignore_last_stop,"data//swiftly//ignore_last_stop.csv")

route_res <- GET("https://api.goswift.ly/info/indygo/routes"
                 ,add_headers(Authorization = key)
                 ,query = list(route = "90"))


fread(content(route_res, "text"))


speed_map_res <- GET(url = "https://api.goswift.ly/rider-alerts"
                     ,add_headers(Authorization = key)
                     ,query = list(agency = "indygo"
                                   ))

speed_map_res
