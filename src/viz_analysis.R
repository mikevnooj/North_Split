#mike nugent jr
#data analyst
#indygo
library(data.table)
library(ggplot2)

VMH_speed_DT <- rbindlist(lapply(c("NB","SB"),function(x)fread(paste0("data//processed/VMH_Speed_",x))))

VMH_speed_DT_clean <- VMH_speed_DT[speed < Inf & 
                                     Vehicle_ID %in% c(1899,1970:1999)
                                   ][, `:=`(diff = c(dist_traveled[1],diff(dist_traveled))
                                            ,tpercrk = rank(time)/length(time)
                                            ,spercrk = rank(speed)/length(speed)
                                            ,travperc = miles_traveled/max(miles_traveled))
                                     ,.(.id,direction)][ 
                                       #tpercrk > .05 &
                                         #tpercrk < .95 &
                                         #spercrk < .95  
                                         #travperc > .95
                                       ]

medians <- VMH_speed_DT_clean[,median(speed),.(.id,direction)]
medians
VMH_speed_DT_clean %>% View()

VMH_speed_DT_clean %>% 
  ggplot(aes(x = speed, color = .id,fill = .id)) +
  geom_density(alpha = .3,size = .5) +
  facet_wrap(~direction) +
  geom_vline(data = medians,aes(xintercept = V1,color = .id))

VMH_speed_DT_clean %>%
  ggplot(aes(x = speed,color = direction, fill = direction))+
  geom_density(alpha = .3,size = .5) +
  facet_wrap(~.id) +
  geom_vline(data = medians, aes(xintercept = V1, color = direction))

VMH_speed_DT_clean %>%
  ggplot(aes(x = speed,y = .id,color = .id, fill = .id)) +
  geom_boxplot(alpha = .5)




VMH_speed_DT_clean[
  ,.(.N
     ,mean=mean(speed)
     ,median = median(speed)
     ,min=min(speed)
     ,lower=quantile(speed, .25, na.rm=TRUE)
     ,middle=quantile(speed, .50, na.rm=TRUE)
     ,upper=quantile(speed, .75, na.rm=TRUE)
     ,max=max(speed))
  ,.(.id,direction)
  ]


# [time > quantile(time,probs = .1)
#          ][speed < quantile(speed,probs = .99)
#            ][] %>% #clean it up
#   ggplot(aes(x = speed))+
#   geom_density()

