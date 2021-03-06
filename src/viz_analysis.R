#mike nugent jr
#data analyst
#indygo
library(data.table)
library(ggplot2)
library(magrittr)
library(timeDate)
library(ggthemes)

VMH_speed_DT <- rbindlist(lapply(c("NB","SB"),function(x)fread(paste0("data//processed/VMH_Speed_",x))))

#add service type
#set the holy days
holidays_sunday_service <- c("USNewYearsDay", "USMemorialDay",
                             "USIndependenceDay", "USLaborDay",
                             "USThanksgivingDay", "USChristmasDay")

holidays_saturday_service <- c("USMLKingsBirthday")

#set sat sun
holidays_sunday <- holiday(2000:2020, holidays_sunday_service)
holidays_saturday <- holiday(2000:2020, holidays_saturday_service)



#add labels
VMH_speed_DT[,lane_type := ifelse(.id == "reversible (college)","college (bi-directional median)",ifelse(.id == "dedicated","downtown (curbside exclusive)","virginia/shelby (none)"))]

VMH_speed_DT_clean <- VMH_speed_DT[speed < Inf & 
                                     Vehicle_ID %in% c(1899,1970:1999)
                                   ][, `:=`(tpercrk = rank(time)/length(time)
                                            ,spercrk = rank(speed)/length(speed)
                                            ,travperc = miles_traveled/max(miles_traveled))
                                     ,.(lane_type,direction)][ 
                                       #tpercrk > .05 
                                         #tpercrk < .95 &
                                         #spercrk < .95  
                                         travperc > .7
                                       ][
                                         #do hms_time
                                         ,hms_time := format(start_time,"%H:%M:%S")
                                       ][
                                         #set peak times
                                           ,Peak := ifelse(hms_time >= "06:00" & hms_time < "09:00","AM",
                                                           ifelse(hms_time >= "15:00" & hms_time < "18:00", "PM","Off-Peak"))
                                       ][
                                         #set service type column
                                         ,Service_Type := dplyr::case_when(Transit_Day %in% as.IDate(holidays_saturday@Data) ~ "Saturday",
                                                                    Transit_Day %in% as.IDate(holidays_sunday@Data) ~ "Sunday",
                                                                    weekdays(Transit_Day) %in% c("Monday","Tuesday","Wednesday","Thursday","Friday") ~ "Weekday",
                                                                    TRUE ~ weekdays(Transit_Day))
                                       ] 

VMH_speed_DT_clean[
  ,.(.N
     ,mean=mean(speed)
     ,median = median(speed)
     ,min=min(speed)
     ,lower=quantile(speed, .25, na.rm=TRUE)
     ,middle=quantile(speed, .50, na.rm=TRUE)
     ,upper=quantile(speed, .75, na.rm=TRUE)
     ,max=max(speed))
  ,.(lane_type,direction)
  ]

VMH_speed_DT_clean[
  ,.(median_speed = round(median(speed),1)
    ,percentile_95 = round(quantile(speed,.95, na.rm = T),1))
  ,.(lane_type,direction)]

direction_medians <- VMH_speed_DT_clean[,median(speed),.(lane_type,direction)]
direction_medians

peak_medians <- VMH_speed_DT_clean[,median(speed),.(lane_type,direction,Peak)]
peak_medians

service_type_medians <- VMH_speed_DT_clean[,median(speed),.(lane_type,direction,Peak,Service_Type)]
service_type_medians

#plot by directind lane type
VMH_speed_DT_clean %>% 
  ggplot(aes(x = speed, color = lane_type,fill = lane_type)) +
  geom_density(alpha = .3,size = .5) +
  facet_wrap(~direction) +
  geom_vline(data = direction_medians,aes(xintercept = V1,color = lane_type))

VMH_speed_DT_clean %>%
  ggplot(aes(x = speed,color = direction, fill = direction))+
  geom_density(alpha = .3,size = .5) +
  facet_wrap(~lane_type) +
  geom_vline(data = direction_medians, aes(xintercept = V1, color = direction))

#same but rel freq histograms
VMH_speed_DT_clean[
  
  ,.N,.(lane_type,direction,speed = floor(speed))][order(speed) 
                                             ][,prop := N/sum(N),.(lane_type,direction)] %>%
  ggplot(aes(x = speed, y = prop,color = lane_type, fill = lane_type)) +
  geom_bar(stat = "identity",alpha = .5,position = "dodge") +
  facet_wrap(~direction) +
  geom_vline(data = direction_medians, aes(xintercept = V1, color = lane_type))


VMH_speed_DT_clean[
  
  ,.N,.(lane_type,direction,speed = floor(speed))][order(speed) 
                                             ][,prop := N/sum(N),.(lane_type,direction)] %>%
  ggplot(aes(x = speed, y = prop,color = direction, fill = direction)) +
  geom_bar(stat = "identity",alpha = .3,position = "identity") +
  facet_wrap(~lane_type) + 
  geom_vline(data = direction_medians, aes(xintercept = V1, color = direction))


#proportion by direction
VMH_speed_DT_clean[  ,.N,.(lane_type,direction,speed = floor(speed))][order(speed) 
][,prop := N/sum(N),.(lane_type,direction)] %>%
  ggplot(aes(x = speed, y = prop,color = lane_type, fill = lane_type)) +
  geom_bar(stat = "identity",alpha = .5,position = "identity") +
  facet_wrap(~direction)

VMH_speed_DT_clean[ ,.N,.(lane_type,direction,speed = floor(speed))][order(speed) ][
  ,prop := N/sum(N),.(lane_type,direction)] %>%
  ggplot(aes(x = speed, y = prop,color = direction, fill = direction)) +
  geom_bar(stat = "identity",alpha = .5,position = "identity") +
  facet_wrap(~lane_type)

#peak by direction on one type of lane
VMH_speed_DT_clean[lane_type == "single bi-directional"
  ,.N,.(lane_type,direction,Peak,speed = floor(speed))
][
  ,prop := N/sum(N),.(lane_type,direction,Peak)
] %>%
  ggplot(aes(x=speed, y = prop,color = Peak, fill = Peak)) + 
  geom_bar(alpha = .3,stat = "identity",position = "dodge")+
  facet_wrap(~direction) + theme_tufte()

VMH_speed_DT_clean[lane_type == "separated uni-directional"
                   ,.N,.(lane_type,direction,Peak,speed = floor(speed))
                   ][
                     ,prop := N/sum(N),.(lane_type,direction,Peak)
                     ] %>%
  ggplot(aes(x=speed, y = prop,color = Peak, fill = Peak)) + 
  geom_bar(alpha = .3,stat = "identity",position = "dodge")+
  facet_wrap(~direction) + theme_tufte()


VMH_speed_DT_clean[lane_type == "none"
                   ,.N,.(lane_type,direction,Peak,speed = floor(speed))
                   ][
                     ,prop := N/sum(N),.(lane_type,direction,Peak)
                     ] %>%
  ggplot(aes(x=speed, y = prop,color = Peak, fill = Peak)) + 
  geom_bar(alpha = .3,stat = "identity",position = "identity")+
  facet_wrap(~direction) + theme_economist(dkpanel = T)

#same but boxplots
VMH_speed_DT_clean %>%
  ggplot(aes(x = speed,y = direction,color = direction, fill = direction)) +
  geom_boxplot(alpha = .5,outlier.alpha = .1) +
  facet_wrap(~lane_type) + theme_solarized_2(light = T)

VMH_speed_DT_clean[lane_type == "single bi-directional"] %>% 
  ggplot(aes(x = speed, color = Peak,fill = Peak)) +
  geom_boxplot(alpha = .3) +
  facet_wrap(~direction)

VMH_speed_DT_clean[lane_type == "none"] %>% 
  ggplot(aes(x = speed, color = Peak,fill = Peak)) +
  geom_boxplot(alpha = .3) +
  facet_wrap(~direction)

VMH_speed_DT_clean[lane_type == "separated uni-directional"] %>% 
  ggplot(aes(x = speed, color = Peak,fill = Peak)) +
  geom_boxplot(alpha = .3) +
  facet_wrap(~direction)

#investigate the hump
VMH_speed_DT_clean[
  lane_type == "none" & speed <= 15
][
  ,.N
  ,direction
]



