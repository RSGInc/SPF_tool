library(dplyr)
dataDir     <- "E:/Projects/Clients/sandag/TO21_Recalibration/data/HTS_Summaries"
WeightsDir  <- "E:/Projects/Clients/sandag/TO21_Recalibration/data/weights"

hts_trips   <- read.csv(paste(dataDir, "hts_trips.csv", sep = "/"), as.is = T)
trips       <- read.csv(paste(dataDir, "trips.csv", sep = "/"), as.is = T)
weights     <- read.csv(paste(WeightsDir, "tripweights_completeweekdays.csv", sep = "/"), as.is = T)

xwalk <- hts_trips[,c("puid", "travel_dow", "day_hhcomplete", "pday_complete" )]
xwalk <- unique(xwalk)

hts_trips$day_hhcomplete[is.na(hts_trips$day_hhcomplete)] <- 0
hts_trips$pday_complete[is.na(hts_trips$pday_complete)] <- 0
hts_trips$travel_dow[is.na(hts_trips$travel_dow)] <- 0

trips$travel_dow     <- xwalk$travel_dow[match(trips$puid, xwalk$puid)]
trips$day_hhcomplete <- xwalk$day_hhcomplete[match(trips$puid, xwalk$puid)]
trips$pday_complete  <- xwalk$pday_complete[match(trips$puid, xwalk$puid)]

trips <- trips[trips$day_hhcomplete==1 & trips$pday_complete==1 & trips$travel_dow>=1 & trips$travel_dow<=5,]
hts_trips <- hts_trips[hts_trips$day_hhcomplete==1 & hts_trips$pday_complete==1 & hts_trips$travel_dow>=1 & hts_trips$travel_dow<=5,]
weights$puid <- paste(weights$hhid, weights$pernum, weights$daynum, sep = "-")

trips$count <- 1
hts_trips$count <- 1
weights$count <- 1

person_trips <- trips %>%
  group_by(puid) %>%
  summarise(num_trip = sum(count))

person_utrips <- hts_trips %>%
  group_by(puid) %>%
  summarise(num_utrip = sum(count))

wgt_utrips <- weights[,c("puid", "adjusted_multiday_linkedtripweight_456x", "count")] %>%
  group_by(puid) %>%
  summarise(wgt_utrip = sum(count)) %>%
  ungroup()

wgt_trips <- weights[weights$adjusted_multiday_linkedtripweight_456x>0,c("puid", "adjusted_multiday_linkedtripweight_456x", "count")] %>%
  group_by(puid) %>%
  summarise(wgt_trip = sum(count))

trips_comparison <- person_utrips %>%
  left_join(person_trips, by = c("puid" = "puid"))  %>%
  left_join(wgt_utrips[,c("puid", "wgt_utrip")], by = c("puid" = "puid"))%>%
  left_join(wgt_trips[,c("puid", "wgt_trip")], by = c("puid" = "puid"))

########

#sum(trips$day_hhcomplete==1 & trips$pday_complete==1 & trips$travel_dow>=1 & trips$travel_dow<=5)
#sum(hts_trips$day_hhcomplete==1 & hts_trips$pday_complete==1 & hts_trips$travel_dow>=1 & hts_trips$travel_dow<=5)







##