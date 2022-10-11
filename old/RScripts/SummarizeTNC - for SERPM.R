#######################################################
### Script for summarizing SANDAG HTS
### Author: Binny M Paul, binny.paul@rsginc.com
### Sep 2017
### Copy of OHAS processing script
#######################################################
oldw <- getOption("warn")
options(warn = -1)
options(scipen=999)

## Libraries
############

library(plyr)
library(dplyr)
library(weights)
library(reshape)
library(foreign)
library(data.table)
library(tidyr)

## User Inputs
###############
# Directories
WD                   <- "E:/Projects/Clients/sandag/TNC_Data_Processing/SummariesForSERPM"
Survey_Dir           <- "E:/Projects/Clients/sandag/TNC_Data_Processing/Data/SPA_Inputs"
Survey_Processed_Dir <- "E:/Projects/Clients/sandag/TNC_Data_Processing/Data/SPA_Outputs"
SkimDir              <- "E:/Projects/Clients/sandag/TNC_Data_Processing/skim"
SHP_Dir              <- "E:/Projects/Clients/sandag/TO21_Recalibration/SHP/zones"
xwalkDir             <- "E:/projects/clients/sandag/SHP"
raw_survey_dir       <- "E:/Projects/Clients/sandag/TNC_Data_Processing/Data/FromClient/SANDAG_dataset_20191122"

## Read Data
xwalk                <- read.csv(paste(xwalkDir, "geographicXwalk_PMSA.csv", sep = "/"), as.is = T)
zones_xwalk          <- read.csv(paste(SHP_Dir, "zones.csv", sep = "/"), as.is = T)
day                  <- fread(file.path(raw_survey_dir, "ex_day.tsv"), stringsAsFactors = F, integer64 = 'numeric')
rtrip                 <- fread(file.path(raw_survey_dir, "ex_trip.tsv"), stringsAsFactors = F, integer64 = 'numeric')

hh                   <- read.csv(paste(Survey_Dir, "hh.csv", sep = "/"), as.is = T)
per                  <- read.csv(paste(Survey_Dir, "person.csv", sep = "/"), as.is = T)
#day                  <- read.csv(paste(Survey_Dir, "SDRTS_Day_Data_20170731.csv", sep = "/"), as.is = T)

### delete the hh of person record with very high weight on TNR trips
hh                   <-hh[!(hh$SAMPN==195049150),]
per                  <-per[!(per$SAMPN==195049150),]
day                  <-day[!(day$hh_id==195049150),]
rtrip                <-rtrip[!(rtrip$hh_id==195049150),]

setwd(WD)

districtList         <- sort(unique(xwalk$pmsa))

multiDayWeights      <- merge(hh[,c('SAMPN', 'wkdy_hh_weight_all_adults')], per[,c('SAMPN', 'person_id', 'PERNO', 'wkdy_person_weight_all_adults')], by = 'SAMPN', all = TRUE)
multiDayWeights      <- merge(multiDayWeights, day[,c('hh_id', 'person_id', 'day_num', 'travel_date_dow', 'wkdy_day_weight_all_adults')], by.x = c('SAMPN', 'person_id'), by.y = c('hh_id', 'person_id'), all.y = TRUE)
colnames(multiDayWeights) <- c('hhid', 'personid', 'hhweight', 'pernum', 'personweight', 'daynum', 'dow', 'tripweight') #day_weight is named tripweight here
multiDayWeights <- multiDayWeights[multiDayWeights$personid %in% unique(day$person_id),]

# Filter records to use only completed households and persons on weekdays
day <- day[day$survey_complete_day==1 & day$day_complete==1 & day$travel_date_dow>=1 & day$travel_date_dow<=5,]

# Generate person-day weigths for all travel database
# multi day weigths are unique across all trips for a person-day, therefore simple aggregation yields person-day weights
multiDayWeights <- unique(multiDayWeights[,c("hhid", "pernum", "daynum", "tripweight", "hhweight")])

# create hh_day and person_day tables
hhday <- unique(day[,c("hh_id", "day_num", "travel_date_dow")])
hhday <- cbind(hhday, hh[match(hhday$hh_id, hh$SAMPN), -which(names(hhday) %in% c("hh_id"))])

perday <- unique(day[,c("hh_id", "person_num", "day_num", "travel_date_dow", 'wkdy_day_weight_all_adults')])
perday <- cbind(perday, per[match(perday$hh_id*100+perday$person_num, per$person_id), -which(names(perday) %in% c("hh_id", "person_num"))])
perday$puid <- paste(perday$hh_id, perday$person_num, perday$day_num, sep = "-")
perday$uid <- paste(perday$hh_id, perday$day_num, sep = "-")


#filter hh and person file for active participants
#hh  <- hh[hh$SAMPN %in% unique(day$hh_id),]
#per <- per[per$person_id %in% unique(day$person_id),]

#hh <- merge(hh, rhh[,c('hh_id', 'wkdy_hh_weight_all_adults')], by.x = 'SAMPN', by.y = 'hh_id', all.x = TRUE)
#per <- merge(per, rper[,c('person_id', 'wkdy_person_weight_all_adults')], by = 'person_id', all.x = TRUE)

place <- read.csv(paste(Survey_Dir, "place_day1.csv", sep = "/"), as.is = T)
place$puid <- paste(place$SAMPN, place$PERNO, place$DAYNO, sep = "-")
# include only records in perday
place <- place[place$puid %in% perday$puid,]
for (i in 2:7) {
  placeFile  <- paste("place_day", as.character(i), ".csv", sep = "")
  temp       <- read.csv(paste(Survey_Dir, placeFile, sep = "/"), as.is = T)
  temp$puid   <- paste(temp$SAMPN, temp$PERNO, temp$DAYNO, sep = "-")
  temp       <- temp[temp$puid %in% perday$puid,]
  place      <- rbind(place, temp)
}

# day 1 data
dayno                <- 1
dayDir               <- paste(Survey_Processed_Dir, "/day", as.character(dayno), sep = "")
tours                <- read.csv(paste(dayDir, "tours.csv", sep = "/"), as.is = T)
trips                <- read.csv(paste(dayDir, "trips.csv", sep = "/"), as.is = T)
jtours               <- read.csv(paste(dayDir, "unique_joint_tours.csv", sep = "/"), as.is = T)
jutrips               <- read.csv(paste(dayDir, "unique_joint_ultrips.csv", sep = "/"), as.is = T)

tours$DAYNO              <- dayno
trips$DAYNO              <- dayno
jtours$DAYNO             <- dayno
jutrips$DAYNO             <- dayno

# puid
tours$puid              <- paste(tours$HH_ID, tours$PER_ID, tours$DAYNO, sep = "-")
trips$puid              <- paste(trips$HH_ID, trips$PER_ID, trips$DAYNO, sep = "-")
jtours$uid              <- paste(jtours$HH_ID, jtours$DAYNO, sep = "-")
jutrips$uid              <- paste(jutrips$HH_ID, jutrips$DAYNO, sep = "-")

#filter
tours <- tours[tours$puid %in% perday$puid,]
trips <- trips[trips$puid %in% perday$puid,]
jtours <- jtours[jtours$uid %in% unique(perday$uid), ]
jutrips <- jutrips[jutrips$uid %in% unique(perday$uid), ]

for (i in 2:7) {
  dayno  <- i
  dayDir <- paste(Survey_Processed_Dir, "/day", as.character(dayno), sep = "")
  t1     <- read.csv(paste(dayDir, "tours.csv", sep = "/"), as.is = T)
  t2     <- read.csv(paste(dayDir, "trips.csv", sep = "/"), as.is = T)
  t3     <- read.csv(paste(dayDir, "unique_joint_tours.csv", sep = "/"), as.is = T)
  t4     <- read.csv(paste(dayDir, "unique_joint_ultrips.csv", sep = "/"), as.is = T)
  t5     <- read.csv(paste(dayDir, "persons.csv", sep = "/"), as.is = T)
  
  t1$DAYNO <- dayno
  t2$DAYNO <- dayno
  t3$DAYNO <- dayno
  t4$DAYNO <- dayno
  
  # puid
  t1$puid <- paste(t1$HH_ID, t1$PER_ID, t1$DAYNO, sep = "-")
  t2$puid <- paste(t2$HH_ID, t2$PER_ID, t2$DAYNO, sep = "-")
  t3$uid <- paste(t3$HH_ID, t3$DAYNO, sep = "-")
  t4$uid <- paste(t4$HH_ID, t4$DAYNO, sep = "-")
  
  # filter
  t1 <- t1[t1$puid %in% perday$puid,]
  t2 <- t2[t2$puid %in% perday$puid,]
  t3 <- t3[t3$uid %in% unique(perday$uid), ]
  t4 <- t4[t4$uid %in% unique(perday$uid), ]
  
  tours <- rbind(tours, t1)
  trips <- rbind(trips, t2)
  jtours <- rbind(jtours, t3)
  jutrips <- rbind(jutrips, t4)
}

# replace all "nan" in Python outputs with 0
tours[tours=="nan"] <- 0
trips[trips=="nan"] <- 0
jtours[jtours=="nan"] <- 0
jutrips[jutrips=="nan"] <- 0
trips$JTRIP_ID <- as.numeric(trips$JTRIP_ID)

# Compare person type across days
# Person types are recoded by Python script based on activity purposes reported for each day
processedPerson      <- read.csv(paste(Survey_Processed_Dir, "day1", "persons.csv", sep = "/"), as.is = T)

for (i in 2:7) {
  dayno                 <- i
  dayDir                <- paste(Survey_Processed_Dir, "/day", as.character(dayno), sep = "")
  per_nxt               <- read.csv(paste(dayDir, "persons.csv", sep = "/"), as.is = T)
  # US
  processedPerson$ptype <- per_nxt$PERSONTYPE[match(processedPerson$HH_ID*100+processedPerson$PER_ID,per_nxt$HH_ID*100+per_nxt$PER_ID)]
  temp                  <- processedPerson[(processedPerson$PERSONTYPE %in% c(1,2,4,5)) & processedPerson$ptype==3,]
  processedPerson       <- processedPerson[!((processedPerson$PERSONTYPE %in% c(1,2,4,5)) & processedPerson$ptype==3),]
  processedPerson       <- processedPerson[,-which(names(processedPerson) %in% c("ptype"))]
  processedPerson       <- rbind(processedPerson, per_nxt[match(temp$HH_ID*100+temp$PER_ID, per_nxt$HH_ID*100+per_nxt$PER_ID), ])
  # PW
  processedPerson$ptype <- per_nxt$PERSONTYPE[match(processedPerson$HH_ID*100+processedPerson$PER_ID,per_nxt$HH_ID*100+per_nxt$PER_ID)]
  temp                  <- processedPerson[(processedPerson$PERSONTYPE %in% c(4,5)) & processedPerson$ptype==2,]
  processedPerson       <- processedPerson[!((processedPerson$PERSONTYPE %in% c(4,5)) & processedPerson$ptype==2),]
  processedPerson       <- processedPerson[,-which(names(processedPerson) %in% c("ptype"))]
  processedPerson       <- rbind(processedPerson, per_nxt[match(temp$HH_ID*100+temp$PER_ID, per_nxt$HH_ID*100+per_nxt$PER_ID), ])
}

# Copy multi day Weights to all trip and tour files [multi-day weights are unique for each person day]
# name the weight field to be used for each file as "finalweight"
trips$finalweight <- multiDayWeights$tripweight[match(paste(trips$HH_ID, trips$PER_ID, trips$DAYNO, sep = "-"), 
                                                      paste(multiDayWeights$hhid, multiDayWeights$pernum, multiDayWeights$daynum, sep = "-"))]
tours$finalweight <- multiDayWeights$tripweight[match(paste(tours$HH_ID, tours$PER_ID, tours$DAYNO, sep = "-"), 
                                                      paste(multiDayWeights$hhid, multiDayWeights$pernum, multiDayWeights$daynum, sep = "-"))]

r1 <- trips %>% 
  group_by(HH_ID, PER_ID, TOUR_ID) %>%
  summarise(TRIP_TYPE = toString(unique(TRIP_TYPE)))

r1 <- r1 %>%
  separate(TRIP_TYPE, c("type1", "type2", "type3", "type4", 'type5'), ",")

r1[is.na(r1)] = '0'
r1[r1 == ' 0'] = '0'

r1 <- r1 %>%
  mutate(TOUR_TYPE = 'IEEI') %>%
  mutate(TOUR_TYPE = ifelse(type1 %in% c('IE', 'EI') | 
                              type2 %in% c('IE', 'EI') | 
                              type3 %in% c('IE', 'EI') | 
                              type4 %in% c('IE', 'EI') | 
                              type5 %in% c('IE', 'EI'),  'IEEI', TOUR_TYPE)) %>%
  mutate(TOUR_TYPE = ifelse(type1 %in% c('II', '0') & 
                              type2 %in% c('II', '0') & 
                              type3 %in% c('II', '0') & 
                              type4 %in% c('II', '0') & 
                              type5 %in% c('II', '0'),  'II', TOUR_TYPE)) %>%
  mutate(TOUR_TYPE = ifelse(type1 %in% c('EE', '0') & 
                              type2 %in% c('EE', '0') & 
                              type3 %in% c('EE', '0') & 
                              type4 %in% c('EE', '0') & 
                              type5 %in% c('EE', '0'),  'EE', TOUR_TYPE))

tours <- merge(tours, r1[,c("HH_ID", "PER_ID", 'TOUR_ID', 'TOUR_TYPE')], by = c("HH_ID", "PER_ID", 'TOUR_ID'), all.x = TRUE)
trips <- merge(trips, r1[,c("HH_ID", "PER_ID", 'TOUR_ID', 'TOUR_TYPE')], by = c("HH_ID", "PER_ID", 'TOUR_ID'), all.x = TRUE)

tours <- tours %>%
  mutate(TOURPURP_REC = TOURPURP) %>%  # Store original purposes in a separate variable
  mutate(TOURPURP_REC = ifelse(TOURPURP_REC %in% c(4,5,6),  6, TOURPURP_REC)) %>%  #maintenance
  mutate(TOURPURP_REC = ifelse(TOURPURP_REC %in% c(7,8,9,13),  9, TOURPURP_REC)) %>%  #discretionary
  mutate(TOURPURP_REC = ifelse(TOUR_TYPE %in% c('IEEI'),  14, TOURPURP_REC)) %>% #ie-ei
  mutate(TOURPURP_REC = ifelse(TOUR_TYPE %in% c('EE'),  15, TOURPURP_REC)) %>% #visitor
  mutate(TOURPURP_REC = ifelse(TOURPURP_REC %in% c(4,5,6) & JOINT_STATUS %in% c(2,3),  16, TOURPURP_REC)) %>% #joint_maintenance
  mutate(TOURPURP_REC = ifelse(TOURPURP_REC %in% c(7,8,9,13) & JOINT_STATUS %in% c(2,3),  17, TOURPURP_REC)) #joint_discretionary

trips$TOURPURP_REC <- tours$TOURPURP_REC[match(trips$HH_ID*1000+trips$PER_ID*100+trips$TOUR_ID,
                                               tours$HH_ID*1000+tours$PER_ID*100+tours$TOUR_ID)]

#CAPPING WEIGHTS TO A MAX OF 5*MEAN WEIGHT
#trips$finalweight <- pmin(5*mean(trips$finalweight), trips$finalweight)
#tours$finalweight <- pmin(5*mean(tours$finalweight), tours$finalweight)

# copy the weights for joint_trips and joint_tours from the trip and tours file
jtours$finalweight  <- tours$finalweight[match(jtours$DAYNO*1000+jtours$HH_ID*100+jtours$JTOUR_ID,
                                                          tours$DAYNO*1000+tours$HH_ID*100+tours$JTOUR_ID)]
jutrips$finalweight <- trips$finalweight[match(jutrips$DAYNO*1000+jutrips$HH_ID*100+jutrips$JTRIP_ID,
                                                          trips$DAYNO*1000+trips$HH_ID*100+trips$JTRIP_ID)]

tours <- tours %>%
  mutate(OUTBOUND_STOPS_REC = OUTBOUND_STOPS) %>%  # Store original purposes in a separate variable
  mutate(INBOUND_STOPS_REC = INBOUND_STOPS) %>%
  mutate(OUTBOUND_STOPS_REC = ifelse(OUTBOUND_STOPS_REC >= 3,  3, OUTBOUND_STOPS_REC)) %>%
  mutate(INBOUND_STOPS_REC = ifelse(INBOUND_STOPS_REC >= 3,  3, INBOUND_STOPS_REC))

stop_freq_count <-
  tours[tours$HH_ID > 0, ] %>% group_by(TOURMODE, TOURPURP, OUTBOUND_STOPS_REC, INBOUND_STOPS_REC) %>% summarise(TOURS = sum(finalweight))

tours_summary <-
  tours[tours$HH_ID > 0, ] %>% group_by(TOURPURP_REC, TOURMODE) %>% summarise(TOURS = sum(finalweight))

trips_summary <-
  trips[trips$HH_ID > 0, ] %>% group_by(TOURPURP_REC, TOURMODE, TRIPMODE) %>% summarise(TRIPS = sum(finalweight))

write.csv(tours_summary, 'sandag_tours_summary_for_serpm_more_modes.csv', row.names = FALSE)
write.csv(trips_summary, 'sandag_trips_summary_for_serpm_more_modes.csv', row.names = FALSE)
write.csv(stop_freq_count, 'sandag_STOPFREQ_summary_for_serpm_more_modes.csv', row.names = FALSE)