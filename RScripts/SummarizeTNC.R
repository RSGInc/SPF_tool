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
library(reshape2)
library(foreign)
library(data.table)

## User Inputs
###############
# Directories
WD                   <- "E:/Projects/Clients/sandag/TNC_Data_Processing/Summaries"
Survey_Dir           <- "E:/Projects/Clients/sandag/TNC_Data_Processing/Data/SPA_Inputs"
Survey_Processed_Dir <- "E:/Projects/Clients/sandag/TNC_Data_Processing/Data/SPA_Outputs"
SkimDir              <- "E:/Projects/Clients/sandag/TNC_Data_Processing/skim"
SHP_Dir              <- "E:/Projects/Clients/sandag/TO21_Recalibration/SHP/zones"
xwalkDir             <- "E:/projects/clients/sandag/SHP"
raw_survey_dir       <- "E:/Projects/Clients/sandag/TNC_Data_Processing/Data/FromClient/SANDAG_dataset_20191211"

## Read Data
xwalk                <- read.csv(paste(xwalkDir, "geographicXwalk_PMSA.csv", sep = "/"), as.is = T)
zones_xwalk          <- read.csv(paste(SHP_Dir, "zones.csv", sep = "/"), as.is = T)
day                  <- fread(file.path(raw_survey_dir, "ex_day.tsv"), stringsAsFactors = F, integer64 = 'numeric')
rtrip                <- fread(file.path(raw_survey_dir, "ex_trip.tsv"), stringsAsFactors = F, integer64 = 'numeric')

hh                   <- read.csv(paste(Survey_Dir, "hh.csv", sep = "/"), as.is = T)
per                  <- read.csv(paste(Survey_Dir, "person.csv", sep = "/"), as.is = T)
#day                 <- read.csv(paste(Survey_Dir, "SDRTS_Day_Data_20170731.csv", sep = "/"), as.is = T)

### delete the hh of person record with very high weight on TNR trips
hh                   <-hh[!(hh$SAMPN==195049150),]
per                  <-per[!(per$SAMPN==195049150),]
day                  <-day[!(day$hh_id==195049150),]
rtrip                <-rtrip[!(rtrip$hh_id==195049150),]


districtList         <- sort(unique(xwalk$pmsa))

multiDayWeights      <- merge(hh[,c('SAMPN', 'wkdy_hh_weight_all_adults')], per[,c('SAMPN', 'person_id', 'PERNO', 'wkdy_person_weight_all_adults')], by = 'SAMPN', all = TRUE)
multiDayWeights      <- merge(multiDayWeights, day[,c('hh_id', 'person_id', 'day_num', 'travel_date_dow', 'wkdy_day_weight_all_adults')], by.x = c('SAMPN', 'person_id'), by.y = c('hh_id', 'person_id'), all.y = TRUE)
colnames(multiDayWeights) <- c('hhid', 'personid', 'hhweight', 'pernum', 'personweight', 'daynum', 'dow', 'tripweight') #day_weight is named tripweight here
multiDayWeights <- multiDayWeights[multiDayWeights$personid %in% unique(day$person_id),]

# Filter records to use only completed households and persons on weekdays
#day <- day[day$survey_complete_day==1 & day$day_complete==1 & day$travel_date_dow>=1 & day$travel_date_dow<=5,]

# Generate person-day weigths for all travel database
# multi day weigths are unique across all trips for a person-day, therefore simple aggregation yields person-day weights
multiDayWeights <- unique(multiDayWeights[,c("hhid", "pernum", "daynum", "tripweight", "hhweight")])

# create hh_day and person_day tables
hhday <- unique(day[,c("hh_id", "day_num", "travel_date_dow")])
hhday <- cbind(hhday, hh[match(hhday$hh_id, hh$SAMPN), -which(names(hhday) %in% c("hh_id"))])

perday <- unique(day[,c("hh_id", "person_num", "day_num", "travel_date_dow")])
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


#CAPPING WEIGHTS TO A MAX OF 10*MEAN WEIGHT
#trips$finalweight <- pmin(10*mean(trips$finalweight), trips$finalweight)
#tours$finalweight <- pmin(10*mean(tours$finalweight), tours$finalweight)


# copy the weights for joint_trips and joint_tours from the trip and tours file
jtours$finalweight <- tours$finalweight[match(jtours$DAYNO*1000+jtours$HH_ID*100+jtours$JTOUR_ID,
                                                          tours$DAYNO*1000+tours$HH_ID*100+tours$JTOUR_ID)]

hh$finalweight <- hh$wkdy_hh_weight_all_adults
per$finalweight <- per$wkdy_person_weight_all_adults

hhday$finalweight <- hhday$wkdy_hh_weight_all_adult

perday$finalweight <- perday$wkdy_person_weight_all_adults
#perday$finalweight_1 <- perday$wkdy_person_weight_all_adults

perday$finalweight_1 <- multiDayWeights$tripweight[match(paste(perday$hh_id, perday$person_num, perday$day_num, sep = "-"), 
                                                      paste(multiDayWeights$hhid, multiDayWeights$pernum, multiDayWeights$daynum, sep = "-"))]


#jutrips <- jtours


#### Debugging
#hts_trips$puid <- paste(hts_trips$hhid, hts_trips$pernum, hts_trips$daynum, sep = "-")
#write.csv(hts_trips, "hts_trips.csv", row.names = F)
#write.csv(trips, "trips.csv", row.names = F)

DST_SKM <- read.csv(paste(SkimDir, "MD SOV GP DIST.csv", sep = "/"), skip = 5, stringsAsFactors = F, header = F, col.names = c("o", "d", "dist"))

# Define other variables
pertypeCodes <- data.frame(code = c(1,2,3,4,5,6,7,8,"All"), 
                           name = c("FT Worker", "PT Worker", "Univ Stud", "Non Worker", "Retiree", "Driv Stud", "NonDriv Stud", "Pre-School", "All"))


# Prepare files for computing summary statistics
###################################################

setwd(WD)

# -----------------------------------------------------------------
# rename variables in SANDAG HTS to standard format in this script
# -----------------------------------------------------------------
names(hh)[names(hh)=="home_taz"] <- 'HHTAZ'
names(hhday)[names(hhday)=="home_taz"] <- 'HHTAZ'

names(per)[names(per)=="work_taz"] <- 'WTAZ'
names(perday)[names(perday)=="work_taz"] <- 'WTAZ'

names(per)[names(per)=="school_taz"] <- 'STAZ'
names(perday)[names(perday)=="school_taz"] <- 'STAZ'

hh$HHTAZ[is.na(hh$HHTAZ)] <- 0
#hh$HHMAZ[is.na(hh$HHMAZ)] <- 0
hhday$HHTAZ[is.na(hhday$HHTAZ)] <- 0
#hhday$HHMAZ[is.na(hhday$HHMAZ)] <- 0

per$WTAZ[is.na(per$WTAZ)] <- 0
#per$WMAZ[is.na(per$WMAZ)] <- 0
perday$WTAZ[is.na(perday$WTAZ)] <- 0
#perday$WMAZ[is.na(perday$WMAZ)] <- 0

per$STAZ[is.na(per$STAZ)] <- 0
#per$SMAZ[is.na(per$SMAZ)] <- 0
perday$STAZ[is.na(perday$STAZ)] <- 0
#perday$SMAZ[is.na(perday$SMAZ)] <- 0


names(processedPerson)[names(processedPerson)=="HH_ID"] <- "HHID"
names(processedPerson)[names(processedPerson)=="PER_ID"] <- "PERID"


hh$HHVEH[hh$num_vehicles == 0] <- 0
hh$HHVEH[hh$num_vehicles == 1] <- 1
hh$HHVEH[hh$num_vehicles == 2] <- 2
hh$HHVEH[hh$num_vehicles == 3] <- 3
hh$HHVEH[hh$num_vehicles >= 4] <- 4

hh$HHSIZE[hh$num_people == 1] <- 1
hh$HHSIZE[hh$num_people == 2] <- 2
hh$HHSIZE[hh$num_people == 3] <- 3
hh$HHSIZE[hh$num_people == 4] <- 4
hh$HHSIZE[hh$num_people >= 5] <- 5

#Adults in the HH
hh$ADULTS <- hh$num_hh_members_age_18_to_34 + hh$num_hh_members_age_35_to_64 + hh$num_hh_members_age_65_to_74 + hh$num_hh_members_age_75_plus
hh$ADULTS[is.na(hh$ADULTS)] <- 0

# define Districts - PMSA for SANDAG
hh$HDISTRICT <- xwalk$pmsa[match(hh$HHTAZ, xwalk$taz)]

per$PERTYPE <- processedPerson$PERSONTYPE[match(per$SAMPN*100+per$PERNO, processedPerson$HHID*100+processedPerson$PERID)]
per$HHTAZ <- hh$HHTAZ[match(per$SAMPN, hh$SAMPN)]
per$HHAZ <- hh$HHMAZ[match(per$SAMPN, hh$SAMPN)]
per$HDISTRICT <- hh$HDISTRICT[match(per$SAMPN, hh$SAMPN)]
per$WDISTRICT <- xwalk$pmsa[match(per$WTAZ, xwalk$taz)]

per$PERTYPE <- processedPerson$PERSONTYPE[match(per$SAMPN*10+per$PERNO, processedPerson$HHID*10+processedPerson$PERID)]
per$XCORD <- hh$sample_home_lon[match(per$SAMPN, hh$SAMPN)]
per$YCORD <- hh$sample_home_lat[match(per$SAMPN, hh$SAMPN)]
#per$WHOME[is.na(per$WHOME)] <- 0

## copy attributes from per to perday and hh to hhday
names(hhday)[names(hhday)=="hh_id"] <- "SAMPN"
names(hhday)[names(hhday)=="day_num"] <- "DAYNO"
hhday$HHVEH     <- hh$HHVEH[match(hhday$SAMPN, hh$SAMPN)]
hhday$HHSIZE    <- hh$HHSIZE[match(hhday$SAMPN, hh$SAMPN)]
hhday$HDISTRICT <- hh$HDISTRICT[match(hhday$SAMPN, hh$SAMPN)]
hhday$ADULTS    <- hh$ADULTS[match(hhday$SAMPN, hh$SAMPN)]

names(perday)[names(perday)=="hh_id"] <- "SAMPN"
names(perday)[names(perday)=="person_num"] <- "PERNO"
names(perday)[names(perday)=="day_num"] <- "DAYNO"
perday$PERTYPE   <- per$PERTYPE[match(perday$SAMPN*100+perday$PERNO, per$SAMPN*100+per$PERNO)]
perday$HHTAZ     <- per$HHTAZ[match(perday$SAMPN*100+perday$PERNO, per$SAMPN*100+per$PERNO)]
perday$XCORD     <- per$XCORD[match(perday$SAMPN*100+perday$PERNO, per$SAMPN*100+per$PERNO)]
perday$YCORD     <- per$YCORD[match(perday$SAMPN*100+perday$PERNO, per$SAMPN*100+per$PERNO)]
perday$HDISTRICT <- per$HDISTRICT[match(perday$SAMPN*100+perday$PERNO, per$SAMPN*100+per$PERNO)]
perday$WDISTRICT <- per$WDISTRICT[match(perday$SAMPN*100+perday$PERNO, per$SAMPN*100+per$PERNO)]


#--------Compute Summary Statistics-------
#*****************************************

# Auto ownership
autoOwnership <- plyr::count(hh[!is.na(hh$HHVEH),], c("HHVEH"), "finalweight")
write.csv(autoOwnership, "autoOwnership.csv", row.names = TRUE)

# Persons by person type
pertypeDistbn <- plyr::count(per[!is.na(per$PERTYPE),], c("PERTYPE"), "finalweight")
write.csv(pertypeDistbn, "pertypeDistbn.csv", row.names = TRUE)

# Mandatory DC
workers <- per[per$WTAZ>0 & per$PERTYPE<=3 & per$job_type!=3, c("SAMPN", "PERNO", "HHTAZ", "WTAZ", "job_type","PERTYPE", "HDISTRICT", "WDISTRICT", "finalweight")]
workers$WDIST <- DST_SKM$dist[match(paste(workers$HHTAZ, workers$WTAZ, sep = "-"), paste(DST_SKM$o, DST_SKM$d, sep = "-"))]

students <- per[per$STAZ>0, c("SAMPN", "PERNO", "HHTAZ", "STAZ", "PERTYPE", "HDISTRICT", "finalweight")]
students$SDIST <- DST_SKM$dist[match(paste(students$HHTAZ, students$STAZ, sep = "-"), paste(DST_SKM$o, DST_SKM$d, sep = "-"))]

# code distance bins
workers$distbin <- cut(workers$WDIST, breaks = c(seq(0,50, by=1), 9999), labels = F, right = F)
students$distbin <- cut(students$SDIST, breaks = c(seq(0,50, by=1), 9999), labels = F, right = F)
write.csv(workers, "workers_tlfd.csv", row.names = FALSE)

distBinCat <- data.frame(distbin = seq(1,51, by=1))
districtList_df <- data.frame(id = districtList)

# compute TLFDs by district and total
tlfd_work <- ddply(workers[,c("HDISTRICT", "distbin", "finalweight")], c("HDISTRICT", "distbin"), summarise, work = sum((HDISTRICT>0)*finalweight))
tlfd_work <- cast(tlfd_work, distbin~HDISTRICT, value = "work", sum)
work_ditbins <- tlfd_work$distbin
tlfd_work <- transpose(tlfd_work[,!colnames(tlfd_work) %in% c("distbin")])
tlfd_work$id <- row.names(tlfd_work)
tlfd_work <- merge(x = districtList_df, y = tlfd_work, by = "id", all.x = TRUE)
tlfd_work[is.na(tlfd_work)] <- 0
tlfd_work <- transpose(tlfd_work[,!colnames(tlfd_work) %in% c("id")])
tlfd_work <- cbind(data.frame(distbin = work_ditbins), tlfd_work)
tlfd_work$Total <- rowSums(tlfd_work[,!colnames(tlfd_work) %in% c("distbin")])
names(tlfd_work) <- sub("V", "District_", names(tlfd_work))
tlfd_work_df <- merge(x = distBinCat, y = tlfd_work, by = "distbin", all.x = TRUE)
tlfd_work_df[is.na(tlfd_work_df)] <- 0


tlfd_univ <- ddply(students[students$PERTYPE==3,c("HDISTRICT", "distbin", "finalweight")], c("HDISTRICT", "distbin"), summarise, univ = sum((HDISTRICT>0)*finalweight))
tlfd_univ <- cast(tlfd_univ, distbin~HDISTRICT, value = "univ", sum)
univ_ditbins <- tlfd_univ$distbin
tlfd_univ <- transpose(tlfd_univ[,!colnames(tlfd_univ) %in% c("distbin")])
tlfd_univ$id <- row.names(tlfd_univ)
tlfd_univ <- merge(x = districtList_df, y = tlfd_univ, by = "id", all.x = TRUE)
tlfd_univ[is.na(tlfd_univ)] <- 0
tlfd_univ <- transpose(tlfd_univ[,!colnames(tlfd_univ) %in% c("id")])
tlfd_univ <- cbind(data.frame(distbin = univ_ditbins), tlfd_univ)
tlfd_univ$Total <- rowSums(tlfd_univ[,!colnames(tlfd_univ) %in% c("distbin")])
names(tlfd_univ) <- sub("V", "District_", names(tlfd_univ))
tlfd_univ_df <- merge(x = distBinCat, y = tlfd_univ, by = "distbin", all.x = TRUE)
tlfd_univ_df[is.na(tlfd_univ_df)] <- 0


#tlfd_schl <- ddply(students[students$PERTYPE>=6,c("HDISTRICT", "distbin", "finalweight")], c("HDISTRICT", "distbin"), summarise, schl = sum((HDISTRICT>0)*finalweight))
#tlfd_schl <- cast(tlfd_schl, distbin~HDISTRICT, value = "schl", sum)
#schl_ditbins <- tlfd_schl$distbin
#tlfd_schl <- transpose(tlfd_schl[,!colnames(tlfd_schl) %in% c("distbin")])
#tlfd_schl$id <- row.names(tlfd_schl)
#tlfd_schl <- merge(x = districtList_df, y = tlfd_schl, by = "id", all.x = TRUE)
#tlfd_schl[is.na(tlfd_schl)] <- 0
#tlfd_schl <- transpose(tlfd_schl[,!colnames(tlfd_schl) %in% c("id")])
#tlfd_schl <- cbind(data.frame(distbin = schl_ditbins), tlfd_schl)
#tlfd_schl$Total <- rowSums(tlfd_schl[,!colnames(tlfd_schl) %in% c("distbin")])
#names(tlfd_schl) <- sub("V", "District_", names(tlfd_schl))
#tlfd_schl_df <- merge(x = distBinCat, y = tlfd_schl, by = "distbin", all.x = TRUE)
#tlfd_schl_df[is.na(tlfd_schl_df)] <- 0

write.csv(tlfd_work_df, "workTLFD.csv", row.names = F)
write.csv(tlfd_univ_df, "univTLFD.csv", row.names = F)
#write.csv(tlfd_schl_df, "schlTLFD.csv", row.names = F)

cat("\n Average distance to workplace (Total): ", weighted.mean(workers$WDIST, workers$finalweight, na.rm = TRUE))
cat("\n Average distance to university (Total): ", weighted.mean(students$SDIST[students$PERTYPE == 3], students$finalweight[students$PERTYPE == 3], na.rm = TRUE))
#cat("\n Average distance to school (Total): ", weighted.mean(students$SDIST[students$PERTYPE >= 6 & students$PERTYPE <= 7], students$finalweight[students$PERTYPE >= 6 & students$PERTYPE <= 7], na.rm = TRUE))

## Output avg trip lengths for visualizer
district_df <- data.frame(HDISTRICT = c(districtList, "Total"))
workTripLengths <- ddply(workers[,c("HDISTRICT", "WDIST", "finalweight")], c("HDISTRICT"), summarise, work = weighted.mean(WDIST,finalweight))
totalLength     <- data.frame("Total", weighted.mean(workers$WDIST,workers$finalweight))
colnames(totalLength) <- colnames(workTripLengths)
workTripLengths <- rbind(workTripLengths, totalLength)
workTripLengths_df <- merge(x = district_df, y = workTripLengths, by = "HDISTRICT", all.x = TRUE)
workTripLengths_df[is.na(workTripLengths_df)] <- 0

univTripLengths <- ddply(students[students$PERTYPE==3,c("HDISTRICT", "SDIST", "finalweight")], c("HDISTRICT"), summarise, univ = weighted.mean(SDIST,finalweight))
totalLength     <- data.frame("Total", weighted.mean(students$SDIST[students$PERTYPE==3], students$finalweight[students$PERTYPE==3]))
colnames(totalLength) <- colnames(univTripLengths)
univTripLengths <- rbind(univTripLengths, totalLength)
univTripLengths_df <- merge(x = district_df, y = univTripLengths, by = "HDISTRICT", all.x = TRUE)
univTripLengths_df[is.na(univTripLengths_df)] <- 0

#schlTripLengths <- ddply(students[students$PERTYPE>=6,c("HDISTRICT", "SDIST", "finalweight")], c("HDISTRICT"), summarise, schl = weighted.mean(SDIST,finalweight))
#totalLength     <- data.frame("Total", weighted.mean(students$SDIST[students$PERTYPE>=6], students$finalweight[students$PERTYPE>=6]))
#colnames(totalLength) <- colnames(schlTripLengths)
#schlTripLengths <- rbind(schlTripLengths, totalLength)
#schlTripLengths_df <- merge(x = district_df, y = schlTripLengths, by = "HDISTRICT", all.x = TRUE)
#schlTripLengths_df[is.na(schlTripLengths_df)] <- 0

mandTripLengths <- cbind(workTripLengths_df, univTripLengths_df$univ)

#setting school trip length to 0, keeping format the same
mandTripLengths$Schl <- 0

colnames(mandTripLengths) <- c("District", "Work", "Univ", "Schl")
write.csv(mandTripLengths, "mandTripLengths.csv", row.names = F)

# Work from home [for each district and total]
per$worker[per$PERTYPE<=2 | (per$PERTYPE==3 & !is.na(per$WTAZ))] <- 1
per$worker[is.na(per$worker)] <- 0
per$wfh[per$job_type==3] <- 1
per$wfh[is.na(per$wfh)] <- 0
perday$worker[perday$PERTYPE<=2 | (perday$PERTYPE==3 & !is.na(perday$WTAZ))] <- 1
perday$worker[is.na(perday$worker)] <- 0
perday$wfh[perday$job_type==3] <- 1
perday$wfh[is.na(perday$wfh)] <- 0

districtWorkers <- ddply(per[per$worker==1,c("HDISTRICT", "finalweight")], c("HDISTRICT"), summarise, workers = sum(finalweight))
districtWorkers_df <- merge(x = data.frame(HDISTRICT = districtList), y = districtWorkers, by = "HDISTRICT", all.x = TRUE)
districtWorkers_df[is.na(districtWorkers_df)] <- 0

districtWfh     <- ddply(per[per$worker==1 & per$wfh==1,c("HDISTRICT", "finalweight")], c("HDISTRICT"), summarise, wfh = sum(finalweight))
districtWfh_df <- merge(x = data.frame(HDISTRICT = districtList), y = districtWfh, by = "HDISTRICT", all.x = TRUE)
districtWfh_df[is.na(districtWfh_df)] <- 0

wfh_summary     <- cbind(districtWorkers_df, districtWfh_df$wfh)
colnames(wfh_summary) <- c("District", "Workers", "WFH")
totalwfh        <- data.frame("Total", sum((per$worker==1)*per$finalweight), sum((per$worker==1 & per$wfh==1)*per$finalweight))
colnames(totalwfh) <- colnames(wfh_summary)
wfh_summary <- rbind(wfh_summary, totalwfh)
write.csv(wfh_summary, "wfh_summary.csv", row.names = F)

# County-County Flows
countyFlows <- xtabs(finalweight~HDISTRICT+WDISTRICT, data = workers)
countyFlows[is.na(countyFlows)] <- 0
countyFlows <- addmargins(as.table(countyFlows))
countyFlows <- as.data.frame.matrix(countyFlows)
colnames(countyFlows)[colnames(countyFlows)=="Sum"] <- "Total"
colnames(countyFlows) <- paste("District", colnames(countyFlows), sep = "_")
rownames(countyFlows)[rownames(countyFlows)=="Sum"] <- "Total"
rownames(countyFlows) <- paste("District", rownames(countyFlows), sep = "_")
write.csv(countyFlows, "countyFlows.csv", row.names = T)

# Process Tour file
#------------------
#tours$finalweight <- hh$finalweight[match(tours$HH_ID, hh$SAMPN)]
tours <- tours[!is.na(tours$finalweight),]
tours$PERTYPE <- per$PERTYPE[match(tours$HH_ID*100+tours$PER_ID, per$SAMPN*100+per$PERNO)]
tours$DISTMILE <- tours$DIST/5280
tours$HHVEH <- hh$HHVEH[match(tours$HH_ID, hh$SAMPN)]
tours$ADULTS <- hh$ADULTS[match(tours$HH_ID, hh$SAMPN)]
tours$AUTOSUFF[tours$HHVEH == 0] <- 0
tours$AUTOSUFF[tours$HHVEH < tours$ADULTS & tours$HHVEH > 0] <- 1
tours$AUTOSUFF[tours$HHVEH >= tours$ADULTS & tours$HHVEH > 0] <- 2

tours$TOTAL_STOPS <- tours$OUTBOUND_STOPS + tours$INBOUND_STOPS

#RECODE TOURMODE FROM SPA OUTPUTS TO AGGREGATE MODES

tours$TOURMODE[tours$TOURMODE==1]  <- 1   #SOV
tours$TOURMODE[tours$TOURMODE==2]  <- 2   #HOV2
tours$TOURMODE[tours$TOURMODE==3]  <- 3   #HOV3
tours$TOURMODE[tours$TOURMODE==4]  <- 4   #WALK
tours$TOURMODE[tours$TOURMODE==5]  <- 5   #BIKE
tours$TOURMODE[tours$TOURMODE==6]  <- 6   #WALK-TRANSIT
tours$TOURMODE[tours$TOURMODE==7]  <- 7   #PNR-TRANSIT
tours$TOURMODE[tours$TOURMODE==8]  <- 8   #TNC-TRANSIT CODED AS KNR-TRANSIT
tours$TOURMODE[tours$TOURMODE==9]  <- 8   #KNR-TRANSIT
tours$TOURMODE[tours$TOURMODE==10] <- 9   #SCHOOL BUS
tours$TOURMODE[tours$TOURMODE==11] <- 2   #MAAS CODED AS HOV2
tours$TOURMODE[tours$TOURMODE==12] <- 1   #OTHER CODED AS SOV

tours$OTAZ <- place$TAZ[match(tours$HH_ID*10000+tours$PER_ID*1000+tours$ORIG_PLACENO*10+tours$DAYNO, place$SAMPN*10000+place$PERNO*1000+place$PLANO*10+place$DAYNO)]
tours$DTAZ <- place$TAZ[match(tours$HH_ID*10000+tours$PER_ID*1000+tours$DEST_PLACENO*10+tours$DAYNO, place$SAMPN*10000+place$PERNO*1000+place$PLANO*10+place$DAYNO)]
tours$SKIMDIST <- DST_SKM$dist[match(paste(tours$OTAZ, tours$DTAZ, sep = "-"), paste(DST_SKM$o, DST_SKM$d, sep = "-"))]

#tours$oindex<-match(tours$OTAZ, skimLookUp$Lookup)
#tours$dindex<-match(tours$DTAZ, skimLookUp$Lookup)
#tours$SKIMDIST<-skimMat3[cbind(tours$oindex, tours$dindex)]

# Recode time bin windows
tours$ANCHOR_DEPART_BIN[tours$ANCHOR_DEPART_BIN<=4] <- 1
tours$ANCHOR_DEPART_BIN[!is.na(tours$ANCHOR_DEPART_BIN) & tours$ANCHOR_DEPART_BIN>=5 & tours$ANCHOR_DEPART_BIN<=42] <- tours$ANCHOR_DEPART_BIN[!is.na(tours$ANCHOR_DEPART_BIN) & tours$ANCHOR_DEPART_BIN>=5 & tours$ANCHOR_DEPART_BIN<=42]-3
tours$ANCHOR_DEPART_BIN[tours$ANCHOR_DEPART_BIN>=43] <- 40

tours$ANCHOR_ARRIVE_BIN[tours$ANCHOR_ARRIVE_BIN<=4] <- 1
tours$ANCHOR_ARRIVE_BIN[!is.na(tours$ANCHOR_ARRIVE_BIN) & tours$ANCHOR_ARRIVE_BIN>=5 & tours$ANCHOR_ARRIVE_BIN<=42] <- tours$ANCHOR_ARRIVE_BIN[!is.na(tours$ANCHOR_ARRIVE_BIN) & tours$ANCHOR_ARRIVE_BIN>=5 & tours$ANCHOR_ARRIVE_BIN<=42]-3
tours$ANCHOR_ARRIVE_BIN[tours$ANCHOR_ARRIVE_BIN>=43] <- 40

# cap tour duration at 20 hours
tours$TOUR_DUR_BIN[tours$TOUR_DUR_BIN>=40] <- 40

# Recode workrelated tours which are not at work subtour as work tour
tours$TOURPURP[tours$TOURPURP == 10 & tours$IS_SUBTOUR == 0] <- 1

###-----------------------------------------------------------------------------
### Change the tourpurpose on subtours to avoid being counted as normal tours
###-----------------------------------------------------------------------------
tours$TOURPURP[tours$IS_SUBTOUR == 1] <- 20 #this resulted in less walk tours(from 34,347 to 9,509) and less other tour modes as well

# Changes made by BMP on 05/09/17
# remove NaN
tours$OUT_ESCORTING_TYPE[tours$OUT_ESCORTING_TYPE=="NaN"] <- 3
tours$INB_ESCORTING_TYPE[tours$INB_ESCORTING_TYPE=="NaN"] <- 3
tours$OUT_ESCORTEE_TOUR_PURP[tours$OUT_ESCORTEE_TOUR_PURP=="NaN"] <- 0
tours$INB_ESCORTEE_TOUR_PURP[tours$INB_ESCORTEE_TOUR_PURP=="NaN"] <- 0

# Copy school escorting fields from tour to trip file
trips$OUT_ESCORTING_TYPE <- tours$OUT_ESCORTING_TYPE[match(trips$HH_ID*10000+trips$PER_ID*1000+trips$TOUR_ID*10+trips$DAYNO, tours$HH_ID*10000+tours$PER_ID*1000+tours$TOUR_ID*10+tours$DAYNO)]
trips$INB_ESCORTING_TYPE <- tours$INB_ESCORTING_TYPE[match(trips$HH_ID*10000+trips$PER_ID*1000+trips$TOUR_ID*10+trips$DAYNO, tours$HH_ID*10000+tours$PER_ID*1000+tours$TOUR_ID*10+tours$DAYNO)]
trips$OUT_ESCORTEE_TOUR_PURP <- tours$OUT_ESCORTEE_TOUR_PURP[match(trips$HH_ID*10000+trips$PER_ID*1000+trips$TOUR_ID*10+trips$DAYNO, tours$HH_ID*10000+tours$PER_ID*1000+tours$TOUR_ID*10+tours$DAYNO)]
trips$INB_ESCORTEE_TOUR_PURP <- tours$INB_ESCORTEE_TOUR_PURP[match(trips$HH_ID*10000+trips$PER_ID*1000+trips$TOUR_ID*10+trips$DAYNO, tours$HH_ID*10000+tours$PER_ID*1000+tours$TOUR_ID*10+tours$DAYNO)]

## Code tours with out/inbound pure escort as school escort purpose [13]
#tours$TOURPURP[(tours$TOURPURP>3 & tours$TOURPURP<10) & (tours$OUT_ESCORTING_TYPE==2 & tours$OUT_ESCORTEE_TOUR_PURP==3)] <- 13
#tours$TOURPURP[(tours$TOURPURP>3 & tours$TOURPURP<10) & (tours$INB_ESCORTING_TYPE==2 & tours$INB_ESCORTEE_TOUR_PURP==3)] <- 13

# exclude  school escorting stop from ride sharing mandatory tours
tours$OUTBOUND_STOPS[tours$OUT_ESCORTING_TYPE==1 & tours$OUT_ESCORTEE_TOUR_PURP==3 & tours$OUTBOUND_STOPS>0] <- tours$OUTBOUND_STOPS[tours$OUT_ESCORTING_TYPE==1 & tours$OUT_ESCORTEE_TOUR_PURP==3 & tours$OUTBOUND_STOPS>0] - 1
tours$INBBOUND_STOPS[tours$INB_ESCORTING_TYPE==1 & tours$INB_ESCORTEE_TOUR_PURP==3 & tours$INBOUND_STOPS>0] <- tours$INBOUND_STOPS[tours$INB_ESCORTING_TYPE==1 & tours$INB_ESCORTEE_TOUR_PURP==3 & tours$INBOUND_STOPS>0] - 1

# end of changes made by BMP on 05/09/17

# Copy TOURPURP to trip file
trips$TOURPURP <- tours$TOURPURP[match(trips$HH_ID*10000+trips$PER_ID*1000+trips$TOUR_ID*10+trips$DAYNO, tours$HH_ID*10000+tours$PER_ID*1000+tours$TOUR_ID*10+tours$DAYNO)]
tours$TOTAL_STOPS <- tours$OUTBOUND_STOPS + tours$INBOUND_STOPS


#----------------
#jtours$finalweight <- hh$finalweight[match(jtours$HH_ID, hh$SAMPN)]
jtours <- jtours[!is.na(jtours$finalweight),]

#copy tour mode & other attributes
jtours$TOURMODE <- tours$TOURMODE[match(jtours$HH_ID*1000+jtours$JTOUR_ID*10+jtours$DAYNO,
                                                    tours$HH_ID*1000+tours$JTOUR_ID*10+tours$DAYNO)]
jtours$AUTOSUFF <- tours$AUTOSUFF[match(jtours$HH_ID*1000+jtours$JTOUR_ID*10+jtours$DAYNO,
                                                    tours$HH_ID*1000+tours$JTOUR_ID*10+tours$DAYNO)]
jtours$ANCHOR_DEPART_BIN <- tours$ANCHOR_DEPART_BIN[match(jtours$HH_ID*1000+jtours$JTOUR_ID*10+jtours$DAYNO,
                                                                      tours$HH_ID*1000+tours$JTOUR_ID*10+tours$DAYNO)]
jtours$ANCHOR_ARRIVE_BIN <- tours$ANCHOR_ARRIVE_BIN[match(jtours$HH_ID*1000+jtours$JTOUR_ID*10+jtours$DAYNO,
                                                                      tours$HH_ID*1000+tours$JTOUR_ID*10+tours$DAYNO)]
jtours$TOUR_DUR_BIN <- tours$TOUR_DUR_BIN[match(jtours$HH_ID*1000+jtours$JTOUR_ID*10+jtours$DAYNO,
                                                            tours$HH_ID*1000+tours$JTOUR_ID*10+tours$DAYNO)]
jtours$DIST <- tours$DIST[match(jtours$HH_ID*1000+jtours$JTOUR_ID*10+jtours$DAYNO,
                                            tours$HH_ID*1000+tours$JTOUR_ID*10+tours$DAYNO)]
jtours$DISTMILE <- jtours$DIST/5280

jtours$OUTBOUND_STOPS <- tours$OUTBOUND_STOPS[match(jtours$HH_ID*1000+jtours$JTOUR_ID*10+jtours$DAYNO,
                                                                tours$HH_ID*1000+tours$JTOUR_ID*10+tours$DAYNO)]
jtours$INBOUND_STOPS <- tours$INBOUND_STOPS[match(jtours$HH_ID*1000+jtours$JTOUR_ID*10+jtours$DAYNO,
                                                              tours$HH_ID*1000+tours$JTOUR_ID*10+tours$DAYNO)]
jtours$TOTAL_STOPS <- tours$TOTAL_STOPS[match(jtours$HH_ID*1000+jtours$JTOUR_ID*10+jtours$DAYNO,
                                                          tours$HH_ID*1000+tours$JTOUR_ID*10+tours$DAYNO)]
jtours$SKIMDIST <- tours$SKIMDIST[match(jtours$HH_ID*1000+jtours$JTOUR_ID*10+jtours$DAYNO,
                                                    tours$HH_ID*1000+tours$JTOUR_ID*10+tours$DAYNO)]


														  
# Process Trip file
#------------------
#trips$finalweight <- hh$finalweight[match(trips$HH_ID, hh$SAMPN)]
trips <- trips[!is.na(trips$finalweight),]

#trips$JTRIP_ID <- lapply(trips$JTRIP_ID, as.character)
#trips$JTRIP_ID <- lapply(trips$JTRIP_ID, as.integer)

trips$JTRIP_ID <- as.character(trips$JTRIP_ID)
trips$JTRIP_ID <- as.integer(trips$JTRIP_ID)
trips$JTRIP_ID[is.na(trips$JTRIP_ID)] <- 0

#RECODE TRIPMODE FROM SPA OUTPUTS TO AGGREGATE MODES

trips$TRIPMODE[trips$TRIPMODE==1] <- 1   #SOV
trips$TRIPMODE[trips$TRIPMODE==2] <- 2   #HOV2
trips$TRIPMODE[trips$TRIPMODE==3] <- 3   #HOV3
trips$TRIPMODE[trips$TRIPMODE==4] <- 4   #WALK
trips$TRIPMODE[trips$TRIPMODE==5] <- 5   #BIKE
trips$TRIPMODE[trips$TRIPMODE>=6 & trips$TRIPMODE<=9] <- 6 #WALK-TRANSIT
trips$TRIPMODE[trips$TRIPMODE>=10 & trips$TRIPMODE<=13] <- 7 #PNR-TRANSIT
trips$TRIPMODE[trips$TRIPMODE>=14 & trips$TRIPMODE<=17] <- 8 #TNC-TRANSIT CODED AS KNR-TRANSIT
trips$TRIPMODE[trips$TRIPMODE>=18 & trips$TRIPMODE<=21] <- 8 #KNR-TRANSIT
trips$TRIPMODE[trips$TRIPMODE==22] <- 9 #SCHOOL BUS
trips$TRIPMODE[trips$TRIPMODE==23] <- 2 #TNC-SINGLE (MAAS) CODED AS HOV2
trips$TRIPMODE[trips$TRIPMODE==24] <- 3 #TNC-SHARED (MAAS) CODED AS HOV3
trips$TRIPMODE[trips$TRIPMODE==25] <- 2 #TAXI (MAAS) CODED AS HOV2
trips$TRIPMODE[trips$TRIPMODE==26] <- 1 #OTHER CODED AS SOV

#RECODE TOURMODE FROM SPA OUTPUTS TO AGGREGATE MODES

trips$TOURMODE[trips$TOURMODE==1]  <- 1   #SOV
trips$TOURMODE[trips$TOURMODE==2]  <- 2   #HOV2
trips$TOURMODE[trips$TOURMODE==3]  <- 3   #HOV3
trips$TOURMODE[trips$TOURMODE==4]  <- 4   #WALK
trips$TOURMODE[trips$TOURMODE==5]  <- 5   #BIKE
trips$TOURMODE[trips$TOURMODE==6]  <- 6   #WALK-TRANSIT
trips$TOURMODE[trips$TOURMODE==7]  <- 7   #PNR-TRANSIT
trips$TOURMODE[trips$TOURMODE==8]  <- 8   #TNC-TRANSIT CODED AS KNR-TRANSIT
trips$TOURMODE[trips$TOURMODE==9]  <- 8   #KNR-TRANSIT
trips$TOURMODE[trips$TOURMODE==10] <- 9   #SCHOOL BUS
trips$TOURMODE[trips$TOURMODE==11] <- 2   #MAAS CODED AS HOV2
trips$TOURMODE[trips$TOURMODE==12] <- 1   #OTHER CODED AS SOV


# Recode time bin windows
trips$ORIG_DEP_BIN[trips$ORIG_DEP_BIN<=4] <- 1
trips$ORIG_DEP_BIN[!is.na(trips$ORIG_DEP_BIN) & trips$ORIG_DEP_BIN>=5 & trips$ORIG_DEP_BIN<=42] <- trips$ORIG_DEP_BIN[!is.na(trips$ORIG_DEP_BIN) & trips$ORIG_DEP_BIN>=5 & trips$ORIG_DEP_BIN<=42]-3
trips$ORIG_DEP_BIN[trips$ORIG_DEP_BIN>=43] <- 40

trips$ORIG_ARR_BIN[trips$ORIG_ARR_BIN<=4] <- 1
trips$ORIG_ARR_BIN[!is.na(trips$ORIG_ARR_BIN) & trips$ORIG_ARR_BIN>=5 & trips$ORIG_ARR_BIN<=42] <- trips$ORIG_ARR_BIN[!is.na(trips$ORIG_ARR_BIN) & trips$ORIG_ARR_BIN>=5 & trips$ORIG_ARR_BIN<=42]-3
trips$ORIG_ARR_BIN[trips$ORIG_ARR_BIN>=43] <- 40

trips$DEST_DEP_BIN[trips$DEST_DEP_BIN<=4] <- 1
trips$DEST_DEP_BIN[!is.na(trips$DEST_DEP_BIN) & trips$DEST_DEP_BIN>=5 & trips$DEST_DEP_BIN<=42] <- trips$DEST_DEP_BIN[!is.na(trips$DEST_DEP_BIN) & trips$DEST_DEP_BIN>=5 & trips$DEST_DEP_BIN<=42]-3
trips$DEST_DEP_BIN[trips$DEST_DEP_BIN>=43] <- 40

trips$DEST_ARR_BIN[trips$DEST_ARR_BIN<=4] <- 1
trips$DEST_ARR_BIN[!is.na(trips$DEST_ARR_BIN) & trips$DEST_ARR_BIN>=5 & trips$DEST_ARR_BIN<=42] <- trips$DEST_ARR_BIN[!is.na(trips$DEST_ARR_BIN) & trips$DEST_ARR_BIN>=5 & trips$DEST_ARR_BIN<=42]-3
trips$DEST_ARR_BIN[trips$DEST_ARR_BIN>=43] <- 40


#Mark trips which are stops on the tour [not going home and not to primary destination]
trips$stops[trips$DEST_PURP>0 & trips$DEST_PURP<=10 & trips$DEST_IS_TOUR_DEST==0] <- 1
trips$stops[is.na(trips$stops)] <- 0

trips$OTAZ <- place$TAZ[match(trips$HH_ID*10000+trips$PER_ID*1000+trips$ORIG_PLACENO*10+trips$DAYNO, place$SAMPN*10000+place$PERNO*1000+place$PLANO*10+place$DAYNO)]
trips$DTAZ <- place$TAZ[match(trips$HH_ID*10000+trips$PER_ID*1000+trips$DEST_PLACENO*10+trips$DAYNO, place$SAMPN*10000+place$PERNO*1000+place$PLANO*10+place$DAYNO)]

trips$OMAZ <- place$MAZ[match(trips$HH_ID*10000+trips$PER_ID*1000+trips$ORIG_PLACENO*10+trips$DAYNO, place$SAMPN*10000+place$PERNO*1000+place$PLANO*10+place$DAYNO)]
trips$DMAZ <- place$MAZ[match(trips$HH_ID*10000+trips$PER_ID*1000+trips$DEST_PLACENO*10+trips$DAYNO, place$SAMPN*10000+place$PERNO*1000+place$PLANO*10+place$DAYNO)]

trips$O_XCORD <- place$XCORD[match(trips$HH_ID*10000+trips$PER_ID*1000+trips$ORIG_PLACENO*10+trips$DAYNO, place$SAMPN*10000+place$PERNO*1000+place$PLANO*10+place$DAYNO)]
trips$D_XCORD <- place$XCORD[match(trips$HH_ID*10000+trips$PER_ID*1000+trips$DEST_PLACENO*10+trips$DAYNO, place$SAMPN*10000+place$PERNO*1000+place$PLANO*10+place$DAYNO)]

trips$O_YCORD <- place$YCORD[match(trips$HH_ID*10000+trips$PER_ID*1000+trips$ORIG_PLACENO*10+trips$DAYNO, place$SAMPN*10000+place$PERNO*1000+place$PLANO*10+place$DAYNO)]
trips$D_YCORD <- place$YCORD[match(trips$HH_ID*10000+trips$PER_ID*1000+trips$DEST_PLACENO*10+trips$DAYNO, place$SAMPN*10000+place$PERNO*1000+place$PLANO*10+place$DAYNO)]


trips$TOUROTAZ <- tours$OTAZ[match(trips$HH_ID*10000+trips$PER_ID*1000+trips$TOUR_ID*10+trips$DAYNO, 
										tours$HH_ID*10000+tours$PER_ID*1000+tours$TOUR_ID*10+tours$DAYNO)]
trips$TOURDTAZ <- tours$DTAZ[match(trips$HH_ID*10000+trips$PER_ID*1000+trips$TOUR_ID*10+trips$DAYNO, 
                                   tours$HH_ID*10000+tours$PER_ID*1000+tours$TOUR_ID*10+tours$DAYNO)]	

# copy tour dep hour and minute
trips$ANCHOR_DEPART_HOUR <- tours$ANCHOR_DEPART_HOUR[match(trips$HH_ID*10000+trips$PER_ID*1000+trips$TOUR_ID*10+trips$DAYNO, 
                                                           tours$HH_ID*10000+tours$PER_ID*1000+tours$TOUR_ID*10+tours$DAYNO)]
trips$ANCHOR_DEPART_MIN <- tours$ANCHOR_DEPART_MIN[match(trips$HH_ID*10000+trips$PER_ID*1000+trips$TOUR_ID*10+trips$DAYNO, 
                                                         tours$HH_ID*10000+tours$PER_ID*1000+tours$TOUR_ID*10+tours$DAYNO)]
# ------------------------------------
# added by nagendra.dhakar@rsginc.com
trips$ANCHOR_DEPART_BIN <- tours$ANCHOR_DEPART_BIN[match(trips$HH_ID*10000+trips$PER_ID*1000+trips$TOUR_ID*10+trips$DAYNO, 
                                                         tours$HH_ID*10000+tours$PER_ID*1000+tours$TOUR_ID*10+tours$DAYNO)]
trips$ANCHOR_ARRIVE_BIN <- tours$ANCHOR_ARRIVE_BIN[match(trips$HH_ID*10000+trips$PER_ID*1000+trips$TOUR_ID*10+trips$DAYNO, 
                                                         tours$HH_ID*10000+tours$PER_ID*1000+tours$TOUR_ID*10+tours$DAYNO)]
# end of addition
# ------------------------------------
														 
trips$od_dist <- DST_SKM$dist[match(paste(trips$OTAZ, trips$DTAZ, sep = "-"), paste(DST_SKM$o, DST_SKM$d, sep = "-"))]
trips$od_dist[is.na(trips$od_dist)] <- 0

# Write out a trips data with select variable for other SANDAG application [for Joel, Oct 31st 2019]
trips_subset <- trips[,c("HH_ID", "PER_ID", "DAYNO", "TRIP_ID", "SUBTOUR", "FULLY_JOINT", "TRIPMODE", "TOURPURP", 
                         "finalweight", "OTAZ", "DTAZ", "od_dist")]
trips_subset[is.na(trips_subset)] <- 0
write.csv(trips_subset, "trips_with_MAZ.csv", row.names = F)

#create stops table
stops <- trips[trips$stops==1,]

stops$finaldestTAZ[stops$IS_INBOUND==0] <- stops$TOURDTAZ[stops$IS_INBOUND==0]
stops$finaldestTAZ[stops$IS_INBOUND==1] <- stops$TOUROTAZ[stops$IS_INBOUND==1]

stops$od_dist <- DST_SKM$dist[match(paste(stops$OTAZ, stops$finaldestTAZ, sep = "-"), paste(DST_SKM$o, DST_SKM$d, sep = "-"))]
stops$os_dist <- DST_SKM$dist[match(paste(stops$OTAZ, stops$DTAZ, sep = "-"), paste(DST_SKM$o, DST_SKM$d, sep = "-"))]
stops$sd_dist <- DST_SKM$dist[match(paste(stops$DTAZ, stops$finaldestTAZ, sep = "-"), paste(DST_SKM$o, DST_SKM$d, sep = "-"))]
stops$out_dir_dist <- stops$os_dist + stops$sd_dist - stops$od_dist				

#Joint trips		
#---------------------------------------------------------------------------------------------
# create joint trips file from trip file [trips belonging to fully joint tours]
trips$JTOUR_ID <- tours$JTOUR_ID[match(trips$HH_ID*10000+trips$PER_ID*1000+trips$TOUR_ID*10+trips$DAYNO, 
                                       tours$HH_ID*10000+tours$PER_ID*1000+tours$TOUR_ID*10+tours$DAYNO)]
trips$JTOUR_ID[is.na(trips$JTOUR_ID)] <- 0

jtrips <- trips[trips$JTOUR_ID>0 & trips$JTRIP_ID>0,]   #this contains a joint trip for each person

# joint trips should be a household level file, therefore, remove person dimension
jtrips$PER_ID <- NULL
jtrips <- unique(jtrips[,c("DAYNO", "HH_ID", "JTRIP_ID")])
jtrips$uid <- paste(jtrips$DAYNO, jtrips$HH_ID, jtrips$JTRIP_ID, sep = "-")

jutrips$uid <- paste(jutrips$DAYNO, jutrips$HH_ID, jutrips$JTRIP_ID, sep = "-")

jtrips$NUMBER_HH <- jutrips$NUMBER_HH[match(jtrips$uid, jutrips$uid)]
jtrips$PERSON_1 <- jutrips$PERSON_1[match(jtrips$uid, jutrips$uid)]
jtrips$PERSON_2 <- jutrips$PERSON_2[match(jtrips$uid, jutrips$uid)]
jtrips$PERSON_3 <- jutrips$PERSON_3[match(jtrips$uid, jutrips$uid)]
jtrips$PERSON_4 <- jutrips$PERSON_4[match(jtrips$uid, jutrips$uid)]
jtrips$PERSON_5 <- jutrips$PERSON_5[match(jtrips$uid, jutrips$uid)]
jtrips$PERSON_6 <- jutrips$PERSON_6[match(jtrips$uid, jutrips$uid)]
jtrips$PERSON_7 <- jutrips$PERSON_7[match(jtrips$uid, jutrips$uid)]
jtrips$PERSON_8 <- jutrips$PERSON_8[match(jtrips$uid, jutrips$uid)]
jtrips$PERSON_9 <- jutrips$PERSON_9[match(jtrips$uid, jutrips$uid)]
jtrips$COMPOSITION <- jutrips$COMPOSITION[match(jtrips$uid, jutrips$uid)]

jtrips$TOUR_ID <- trips$TOUR_ID[match(jtrips$HH_ID*10000+jtrips$JTRIP_ID*100+jtrips$DAYNO,trips$HH_ID*10000+trips$JTRIP_ID*100+trips$DAYNO)]	
jtrips$JTOUR_ID <- trips$JTOUR_ID[match(jtrips$HH_ID*10000+jtrips$JTRIP_ID*100+jtrips$DAYNO,trips$HH_ID*10000+trips$JTRIP_ID*100+trips$DAYNO)]	
jtrips$finalweight <- trips$finalweight[match(jtrips$HH_ID*10000+jtrips$JTRIP_ID*100+jtrips$DAYNO,trips$HH_ID*10000+trips$JTRIP_ID*100+trips$DAYNO)]	

#remove joint tours from joint_tours with no joint trips in jtrips 
jtours_jtrips <- unique(jtrips[,c("HH_ID", "JTOUR_ID")])
jtours_jtrips$uid <- paste(jtours_jtrips$HH_ID, jtours_jtrips$JTOUR_ID, sep = "-")

jtours$uid <- paste(jtours$HH_ID, jtours$JTOUR_ID, sep = "-")
jtours <- jtours[jtours$uid %in% jtours_jtrips$uid,]
#---------------------------------------------------------------------------------------------


#jtrips$finalweight <- hh$finalweight[match(jtrips$HH_ID, hh$SAMPN)]
jtrips <- jtrips[!is.na(jtrips$finalweight),]

jtrips$stops <- trips$stops[match(jtrips$HH_ID*10000+jtrips$JTRIP_ID*100+jtrips$DAYNO,trips$HH_ID*10000+trips$JTRIP_ID*100+trips$DAYNO)]	
jtrips$TOURPURP <- trips$TOURPURP[match(jtrips$HH_ID*10000+jtrips$JTRIP_ID*100+jtrips$DAYNO,trips$HH_ID*10000+trips$JTRIP_ID*100+trips$DAYNO)]	
jtrips$DEST_PURP <- trips$DEST_PURP[match(jtrips$HH_ID*10000+jtrips$JTRIP_ID*100+jtrips$DAYNO,trips$HH_ID*10000+trips$JTRIP_ID*100+trips$DAYNO)]	
jtrips$TRIPMODE <- trips$TRIPMODE[match(jtrips$HH_ID*10000+jtrips$JTRIP_ID*100+jtrips$DAYNO,trips$HH_ID*10000+trips$JTRIP_ID*100+trips$DAYNO)]
jtrips$TOURMODE <- trips$TOURMODE[match(jtrips$HH_ID*10000+jtrips$JTRIP_ID*100+jtrips$DAYNO,trips$HH_ID*10000+trips$JTRIP_ID*100+trips$DAYNO)]	
jtrips$OTAZ <- trips$OTAZ[match(jtrips$HH_ID*10000+jtrips$JTRIP_ID*100+jtrips$DAYNO,trips$HH_ID*10000+trips$JTRIP_ID*100+trips$DAYNO)]	
jtrips$DTAZ <- trips$DTAZ[match(jtrips$HH_ID*10000+jtrips$JTRIP_ID*100+jtrips$DAYNO,trips$HH_ID*10000+trips$JTRIP_ID*100+trips$DAYNO)]	
jtrips$TOUROTAZ <- trips$TOUROTAZ[match(jtrips$HH_ID*10000+jtrips$JTRIP_ID*100+jtrips$DAYNO,trips$HH_ID*10000+trips$JTRIP_ID*100+trips$DAYNO)]	
jtrips$TOURDTAZ <- trips$TOURDTAZ[match(jtrips$HH_ID*10000+jtrips$JTRIP_ID*100+jtrips$DAYNO,trips$HH_ID*10000+trips$JTRIP_ID*100+trips$DAYNO)]
jtrips$DEST_DEP_BIN <- trips$DEST_DEP_BIN[match(jtrips$HH_ID*10000+jtrips$JTRIP_ID*100+jtrips$DAYNO,trips$HH_ID*10000+trips$JTRIP_ID*100+trips$DAYNO)]
jtrips$ORIG_DEP_BIN <- trips$ORIG_DEP_BIN[match(jtrips$HH_ID*10000+jtrips$JTRIP_ID*100+jtrips$DAYNO,trips$HH_ID*10000+trips$JTRIP_ID*100+trips$DAYNO)]
jtrips$IS_INBOUND <- trips$IS_INBOUND[match(jtrips$HH_ID*10000+jtrips$JTRIP_ID*100+jtrips$DAYNO,trips$HH_ID*10000+trips$JTRIP_ID*100+trips$DAYNO)]	
jtrips$ANCHOR_DEPART_HOUR <- trips$ANCHOR_DEPART_HOUR[match(jtrips$HH_ID*10000+jtrips$JTRIP_ID*100+jtrips$DAYNO,trips$HH_ID*10000+trips$JTRIP_ID*100+trips$DAYNO)]															  
jtrips$ANCHOR_DEPART_MIN <- trips$ANCHOR_DEPART_MIN[match(jtrips$HH_ID*10000+jtrips$JTRIP_ID*100+jtrips$DAYNO,trips$HH_ID*10000+trips$JTRIP_ID*100+trips$DAYNO)]															  

# -----------------------------
# added by nagendra.dhakar@rsginc.com
# for stop purpose and stop departure time distributions

jtrips$DEST_ARR_BIN <- trips$DEST_DEP_BIN[match(jtrips$HH_ID*10000+jtrips$JTRIP_ID*100+jtrips$DAYNO,trips$HH_ID*10000+trips$JTRIP_ID*100+trips$DAYNO)]
jtrips$ORIG_ARR_BIN <- trips$ORIG_DEP_BIN[match(jtrips$HH_ID*10000+jtrips$JTRIP_ID*100+jtrips$DAYNO,trips$HH_ID*10000+trips$JTRIP_ID*100+trips$DAYNO)]
jtrips$ANCHOR_DEPART_BIN <- trips$ANCHOR_DEPART_BIN[match(jtrips$HH_ID*10000+jtrips$JTRIP_ID*100+jtrips$DAYNO,trips$HH_ID*10000+trips$JTRIP_ID*100+trips$DAYNO)]															  
jtrips$ANCHOR_ARRIVE_BIN <- trips$ANCHOR_ARRIVE_BIN[match(jtrips$HH_ID*10000+jtrips$JTRIP_ID*100+jtrips$DAYNO,trips$HH_ID*10000+trips$JTRIP_ID*100+trips$DAYNO)]	
jtrips$DEST_IS_TOUR_ORIG <- trips$DEST_IS_TOUR_ORIG[match(jtrips$HH_ID*10000+jtrips$JTRIP_ID*100+jtrips$DAYNO,trips$HH_ID*10000+trips$JTRIP_ID*100+trips$DAYNO)]	

# end of addition
# -----------------------------

#some joint trips missing in trip file, so couldnt copy their attributes
jtrips <- jtrips[!is.na(jtrips$IS_INBOUND),]

# get person type and age of all memebrs on joint tours
jtrips$PERTYPE_1 <- processedPerson$PERSONTYPE[match(jtrips$HH_ID*100+jtrips$PERSON_1, 
                                                     processedPerson$HHID*100+processedPerson$PERID)]
jtrips$PERTYPE_2 <- processedPerson$PERSONTYPE[match(jtrips$HH_ID*100+jtrips$PERSON_2, 
                                                     processedPerson$HHID*100+processedPerson$PERID)]
jtrips$PERTYPE_3 <- processedPerson$PERSONTYPE[match(jtrips$HH_ID*100+jtrips$PERSON_3, 
                                                     processedPerson$HHID*100+processedPerson$PERID)]
jtrips$PERTYPE_4 <- processedPerson$PERSONTYPE[match(jtrips$HH_ID*100+jtrips$PERSON_4, 
                                                     processedPerson$HHID*100+processedPerson$PERID)]
jtrips$PERTYPE_5 <- processedPerson$PERSONTYPE[match(jtrips$HH_ID*100+jtrips$PERSON_5, 
                                                     processedPerson$HHID*100+processedPerson$PERID)]
jtrips$PERTYPE_6 <- processedPerson$PERSONTYPE[match(jtrips$HH_ID*100+jtrips$PERSON_6, 
                                                     processedPerson$HHID*100+processedPerson$PERID)]
jtrips$PERTYPE_7 <- processedPerson$PERSONTYPE[match(jtrips$HH_ID*100+jtrips$PERSON_7, 
                                                     processedPerson$HHID*100+processedPerson$PERID)]
jtrips$PERTYPE_8 <- processedPerson$PERSONTYPE[match(jtrips$HH_ID*100+jtrips$PERSON_8, 
                                                     processedPerson$HHID*100+processedPerson$PERID)]
jtrips$PERTYPE_9 <- processedPerson$PERSONTYPE[match(jtrips$HH_ID*100+jtrips$PERSON_9, 
                                                     processedPerson$HHID*100+processedPerson$PERID)]

jtrips$PERTYPE_1 <- ifelse(is.na(jtrips$PERTYPE_1), 0, jtrips$PERTYPE_1)
jtrips$PERTYPE_2 <- ifelse(is.na(jtrips$PERTYPE_2), 0, jtrips$PERTYPE_2)
jtrips$PERTYPE_3 <- ifelse(is.na(jtrips$PERTYPE_3), 0, jtrips$PERTYPE_3)
jtrips$PERTYPE_4 <- ifelse(is.na(jtrips$PERTYPE_4), 0, jtrips$PERTYPE_4)
jtrips$PERTYPE_5 <- ifelse(is.na(jtrips$PERTYPE_5), 0, jtrips$PERTYPE_5)
jtrips$PERTYPE_6 <- ifelse(is.na(jtrips$PERTYPE_6), 0, jtrips$PERTYPE_6)
jtrips$PERTYPE_7 <- ifelse(is.na(jtrips$PERTYPE_7), 0, jtrips$PERTYPE_7)
jtrips$PERTYPE_8 <- ifelse(is.na(jtrips$PERTYPE_8), 0, jtrips$PERTYPE_8)
jtrips$PERTYPE_9 <- ifelse(is.na(jtrips$PERTYPE_9), 0, jtrips$PERTYPE_9)


jtrips$AGE_1 <- processedPerson$AGE[match(jtrips$HH_ID*100+jtrips$PERSON_1, 
                                          processedPerson$HHID*100+processedPerson$PERID)]
jtrips$AGE_2 <- processedPerson$AGE[match(jtrips$HH_ID*100+jtrips$PERSON_2, 
                                          processedPerson$HHID*100+processedPerson$PERID)]
jtrips$AGE_3 <- processedPerson$AGE[match(jtrips$HH_ID*100+jtrips$PERSON_3, 
                                          processedPerson$HHID*100+processedPerson$PERID)]
jtrips$AGE_4 <- processedPerson$AGE[match(jtrips$HH_ID*100+jtrips$PERSON_4, 
                                          processedPerson$HHID*100+processedPerson$PERID)]
jtrips$AGE_5 <- processedPerson$AGE[match(jtrips$HH_ID*100+jtrips$PERSON_5, 
                                          processedPerson$HHID*100+processedPerson$PERID)]
jtrips$AGE_6 <- processedPerson$AGE[match(jtrips$HH_ID*100+jtrips$PERSON_6, 
                                          processedPerson$HHID*100+processedPerson$PERID)]
jtrips$AGE_7 <- processedPerson$AGE[match(jtrips$HH_ID*100+jtrips$PERSON_7, 
                                          processedPerson$HHID*100+processedPerson$PERID)]
jtrips$AGE_8 <- processedPerson$AGE[match(jtrips$HH_ID*100+jtrips$PERSON_8, 
                                          processedPerson$HHID*100+processedPerson$PERID)]
jtrips$AGE_9 <- processedPerson$AGE[match(jtrips$HH_ID*100+jtrips$PERSON_9, 
                                          processedPerson$HHID*100+processedPerson$PERID)]

jtrips$AGE_1 <- ifelse(is.na(jtrips$AGE_1), 0, jtrips$AGE_1)
jtrips$AGE_2 <- ifelse(is.na(jtrips$AGE_2), 0, jtrips$AGE_2)
jtrips$AGE_3 <- ifelse(is.na(jtrips$AGE_3), 0, jtrips$AGE_3)
jtrips$AGE_4 <- ifelse(is.na(jtrips$AGE_4), 0, jtrips$AGE_4)
jtrips$AGE_5 <- ifelse(is.na(jtrips$AGE_5), 0, jtrips$AGE_5)
jtrips$AGE_6 <- ifelse(is.na(jtrips$AGE_6), 0, jtrips$AGE_6)
jtrips$AGE_7 <- ifelse(is.na(jtrips$AGE_7), 0, jtrips$AGE_7)
jtrips$AGE_8 <- ifelse(is.na(jtrips$AGE_8), 0, jtrips$AGE_8)
jtrips$AGE_9 <- ifelse(is.na(jtrips$AGE_9), 0, jtrips$AGE_9)

jtrips$MAX_AGE <- pmax(jtrips$AGE_1, jtrips$AGE_2, jtrips$AGE_3, 
                       jtrips$AGE_4, jtrips$AGE_5, jtrips$AGE_6, 
                       jtrips$AGE_7, jtrips$AGE_8, jtrips$AGE_9)

jtrips$MAX_AGE_PERTYPE <- 0
jtrips$MAX_AGE_PERTYPE <- ifelse(jtrips$MAX_AGE==jtrips$AGE_1, jtrips$PERTYPE_1, jtrips$MAX_AGE_PERTYPE)
jtrips$MAX_AGE_PERTYPE <- ifelse(jtrips$MAX_AGE==jtrips$AGE_2, jtrips$PERTYPE_2, jtrips$MAX_AGE_PERTYPE)
jtrips$MAX_AGE_PERTYPE <- ifelse(jtrips$MAX_AGE==jtrips$AGE_3, jtrips$PERTYPE_3, jtrips$MAX_AGE_PERTYPE)
jtrips$MAX_AGE_PERTYPE <- ifelse(jtrips$MAX_AGE==jtrips$AGE_4, jtrips$PERTYPE_4, jtrips$MAX_AGE_PERTYPE)
jtrips$MAX_AGE_PERTYPE <- ifelse(jtrips$MAX_AGE==jtrips$AGE_5, jtrips$PERTYPE_5, jtrips$MAX_AGE_PERTYPE)
jtrips$MAX_AGE_PERTYPE <- ifelse(jtrips$MAX_AGE==jtrips$AGE_6, jtrips$PERTYPE_6, jtrips$MAX_AGE_PERTYPE)
jtrips$MAX_AGE_PERTYPE <- ifelse(jtrips$MAX_AGE==jtrips$AGE_7, jtrips$PERTYPE_7, jtrips$MAX_AGE_PERTYPE)
jtrips$MAX_AGE_PERTYPE <- ifelse(jtrips$MAX_AGE==jtrips$AGE_8, jtrips$PERTYPE_8, jtrips$MAX_AGE_PERTYPE)
jtrips$MAX_AGE_PERTYPE <- ifelse(jtrips$MAX_AGE==jtrips$AGE_9, jtrips$PERTYPE_9, jtrips$MAX_AGE_PERTYPE)


#create stops table
jstops <- jtrips[jtrips$stops==1,]

jstops$finaldestTAZ[jstops$IS_INBOUND==0] <- jstops$TOURDTAZ[jstops$IS_INBOUND==0]
jstops$finaldestTAZ[jstops$IS_INBOUND==1] <- jstops$TOUROTAZ[jstops$IS_INBOUND==1]

jstops$od_dist <- DST_SKM$dist[match(paste(jstops$OTAZ, jstops$finaldestTAZ, sep = "-"), paste(DST_SKM$o, DST_SKM$d, sep = "-"))]
jstops$os_dist <- DST_SKM$dist[match(paste(jstops$OTAZ, jstops$DTAZ, sep = "-"), paste(DST_SKM$o, DST_SKM$d, sep = "-"))]
jstops$sd_dist <- DST_SKM$dist[match(paste(jstops$DTAZ, jstops$finaldestTAZ, sep = "-"), paste(DST_SKM$o, DST_SKM$d, sep = "-"))]

jstops$out_dir_dist <- jstops$os_dist + jstops$sd_dist - jstops$od_dist	


# ------------
# this part is added by nagendra.dhakar@rsginc.com from binny.paul@rsginc.com OHAS summaries

# stop freq model calibration summary
temp_tour1 <- tours[(tours$TOURPURP==20 | tours$TOURPURP<=9) & tours$FULLY_JOINT==0,c("TOURPURP","OUTBOUND_STOPS","INBOUND_STOPS", "finalweight")]
temp_tour2 <- jtours[jtours$JOINT_PURP>=5 & jtours$JOINT_PURP<=9,c("JOINT_PURP","OUTBOUND_STOPS","INBOUND_STOPS", "finalweight")]
colnames(temp_tour2) <- colnames(temp_tour1)
temp_tour <- rbind(temp_tour1,temp_tour2)

# code stop frequency model alternatives
# updated - nagendra.dhakar@rsginc.com
temp_tour$STOP_FREQ_ALT <- 0 #default - this will have 3+ out and 3+ in tours
temp_tour$STOP_FREQ_ALT[temp_tour$OUTBOUND_STOPS==0 & temp_tour$INBOUND_STOPS==0] <- 1
temp_tour$STOP_FREQ_ALT[temp_tour$OUTBOUND_STOPS==0 & temp_tour$INBOUND_STOPS==1] <- 2
temp_tour$STOP_FREQ_ALT[temp_tour$OUTBOUND_STOPS==0 & temp_tour$INBOUND_STOPS==2] <- 3
temp_tour$STOP_FREQ_ALT[temp_tour$OUTBOUND_STOPS==0 & temp_tour$INBOUND_STOPS==3] <- 4

temp_tour$STOP_FREQ_ALT[temp_tour$OUTBOUND_STOPS==1 & temp_tour$INBOUND_STOPS==0] <- 5
temp_tour$STOP_FREQ_ALT[temp_tour$OUTBOUND_STOPS==1 & temp_tour$INBOUND_STOPS==1] <- 6
temp_tour$STOP_FREQ_ALT[temp_tour$OUTBOUND_STOPS==1 & temp_tour$INBOUND_STOPS==2] <- 7
temp_tour$STOP_FREQ_ALT[temp_tour$OUTBOUND_STOPS==1 & temp_tour$INBOUND_STOPS==3] <- 8

temp_tour$STOP_FREQ_ALT[temp_tour$OUTBOUND_STOPS==2 & temp_tour$INBOUND_STOPS==0] <- 9
temp_tour$STOP_FREQ_ALT[temp_tour$OUTBOUND_STOPS==2 & temp_tour$INBOUND_STOPS==1] <- 10
temp_tour$STOP_FREQ_ALT[temp_tour$OUTBOUND_STOPS==2 & temp_tour$INBOUND_STOPS==2] <- 11
temp_tour$STOP_FREQ_ALT[temp_tour$OUTBOUND_STOPS==2 & temp_tour$INBOUND_STOPS==3] <- 12

temp_tour$STOP_FREQ_ALT[temp_tour$OUTBOUND_STOPS==3 & temp_tour$INBOUND_STOPS==0] <- 13
temp_tour$STOP_FREQ_ALT[temp_tour$OUTBOUND_STOPS==3 & temp_tour$INBOUND_STOPS==1] <- 14
temp_tour$STOP_FREQ_ALT[temp_tour$OUTBOUND_STOPS==3 & temp_tour$INBOUND_STOPS==2] <- 15
temp_tour$STOP_FREQ_ALT[temp_tour$OUTBOUND_STOPS==3 & temp_tour$INBOUND_STOPS==3] <- 16

temp_tour$STOP_FREQ_ALT[is.na(temp_tour$STOP_FREQ_ALT)] <- 0
temp_tour$TOTAL_STOPS <- temp_tour$OUTBOUND_STOPS + temp_tour$INBOUND_STOPS

# tours with more missing stops - more than 3 stops in outbound or inbound direction that are not included in above summary
stopFreqModel_missingStops <- xtabs(finalweight~TOTAL_STOPS+TOURPURP, data = temp_tour[(temp_tour$TOURPURP<=9 | temp_tour$TOURPURP==20) & temp_tour$STOP_FREQ_ALT==0,])
write.csv(stopFreqModel_missingStops, "stopFreqModel_missingStops.csv", row.names = T)

stopFreqModel_summary <- xtabs(finalweight~STOP_FREQ_ALT+TOURPURP, data = temp_tour[temp_tour$TOURPURP<=9 | temp_tour$TOURPURP==20,])
write.csv(stopFreqModel_summary, "stopFreqModel_summary.csv", row.names = T)
# end of the part added by nagendra.dhakar@rsginc.com
# -------------


#---------------------------------------------------------------------------------														  

workCounts <- plyr::count(tours, c( "HH_ID", "PER_ID", "DAYNO"), "TOURPURP == 1 & IS_SUBTOUR == 0") #[excluding at work subtours]. joint work tours should be considered individual mandatory tours
atWorkCounts <- plyr::count(tours, c("HH_ID", "PER_ID", "DAYNO"), "(TOURPURP == 20 & IS_SUBTOUR == 1)") #include joint work-subtours as individual subtours
schlCounts <- plyr::count(tours, c("HH_ID", "PER_ID", "DAYNO"), "(TOURPURP == 2 | TOURPURP == 3)") #joint school/university tours should be considered individual mandatory tours
inmCounts <- plyr::count(tours, c("HH_ID", "PER_ID", "DAYNO"), "(TOURPURP>=4 & TOURPURP<=9 & FULLY_JOINT==0 & IS_SUBTOUR == 0) | (TOURPURP==4 & FULLY_JOINT==1)") #joint escort tours should be considered individual non-mandatory tours
itourCounts <- plyr::count(tours, c("HH_ID", "PER_ID", "DAYNO"), "(TOURPURP <= 9 & IS_SUBTOUR == 0 & FULLY_JOINT==0) | (TOURPURP <= 4 & FULLY_JOINT==1)")  #number of individual tours per person [excluding at work subtours]
jtourCounts <- plyr::count(tours, c("HH_ID", "PER_ID", "DAYNO"), "TOURPURP >= 5 & TOURPURP <= 9 & IS_SUBTOUR == 0 & FULLY_JOINT==1")  #number of joint tours per person [excluding at work subtours, also excluding joint escort tours]

# -----------------------
# added for calibration by nagendra.dhakar@rsginc.com
# for indivudal NM tour generation
# different number of tours if use final weights by person than the aggregation of finalweight on tours

#workCounts_temp <- count(tours[tours$TOURPURP==1 & tours$IS_SUBTOUR==0,], c( "HH_ID", "PER_ID", "DAYNO"), "finalweight")
#schlCounts_temp <- count(tours[(tours$TOURPURP==2 | tours$TOURPURP==3),], c( "HH_ID", "PER_ID", "DAYNO"), "finalweight")
#inmCounts_temp <- count(tours[tours$TOURPURP>=4 & tours$TOURPURP<=9 & tours$FULLY_JOINT==0 & tours$IS_SUBTOUR==0,], c( "HH_ID", "PER_ID", "DAYNO"), "finalweight")
#jtourCounts_temp <- count(tours[tours$TOURPURP>=4 & tours$TOURPURP<=9 & tours$FULLY_JOINT==1 & tours$IS_SUBTOUR==0,], c( "HH_ID", "PER_ID", "DAYNO"), "finalweight")

workCounts_temp <- workCounts
schlCounts_temp <- schlCounts
inmCounts_temp <- plyr::count(tours, c("HH_ID", "PER_ID", "DAYNO"), "TOURPURP>4 & TOURPURP<=9 & FULLY_JOINT==0 & IS_SUBTOUR == 0") #need to remove school escort tours
jtourCounts_temp <- jtourCounts
atWorkCounts_temp <- atWorkCounts
escortCounts_temp <- plyr::count(tours, c("HH_ID", "PER_ID", "DAYNO"), "TOURPURP==4 & IS_SUBTOUR == 0") #include both individual and joint escort tours

colnames(workCounts_temp)[4] <- "freq_work"
colnames(schlCounts_temp)[4] <- "freq_schl"
colnames(inmCounts_temp)[4] <- "freq_inm"
colnames(jtourCounts_temp)[4] <- "freq_joint"
colnames(atWorkCounts_temp)[4] <- "freq_atwork"
colnames(escortCounts_temp)[4] <- "freq_escort"

temp <- merge(workCounts_temp, schlCounts_temp, by = c( "HH_ID", "PER_ID", "DAYNO"))
temp1 <- merge(temp, inmCounts_temp, by = c( "HH_ID", "PER_ID", "DAYNO"))
temp1$freq_m <- temp1$freq_work + temp1$freq_schl
temp1 <- temp1[temp1$freq_m>0 | temp1$freq_inm>0,]

#joint tours
jtourCounts_temp[is.na(jtourCounts_temp)] <- 0
temp_joint <- jtourCounts_temp[jtourCounts_temp$freq_joint>0,]

temp2 <- merge(temp1, temp_joint, by = c("HH_ID", "PER_ID", "DAYNO"), all = T)
temp2 <- merge(temp2, atWorkCounts_temp, by = c("HH_ID", "PER_ID", "DAYNO"), all = T)
temp2 <- merge(temp2, escortCounts_temp, by = c("HH_ID", "PER_ID", "DAYNO"), all = T)
temp2[is.na(temp2)] <- 0

#add number of joint tours to non-mandatory
temp2$freq_nm <- temp2$freq_inm + temp2$freq_joint

#total tours
temp2$total_tours <- temp2$freq_nm+temp2$freq_m+temp2$freq_atwork+temp2$freq_escort

temp2$PERTYPE <- perday$PERTYPE[match(temp2$HH_ID*1000+temp2$PER_ID*10+temp2$DAYNO,perday$SAMPN*1000+perday$PERNO*10+perday$DAYNO)]
temp2$finalweight <- perday$finalweight[match(temp2$HH_ID*1000+temp2$PER_ID*10+temp2$DAYNO,perday$SAMPN*1000+perday$PERNO*10+perday$DAYNO)]
temp2$finalweight_1 <- perday$finalweight_1[match(temp2$HH_ID*1000+temp2$PER_ID*10+temp2$DAYNO,perday$SAMPN*1000+perday$PERNO*10+perday$DAYNO)]

persons_mand <- temp2[temp2$freq_m>0,]  #persons with atleast 1 mandatory tours
persons_nomand <- temp2[temp2$freq_m==0,] #active persons with 0 mandatory tours

freq_nmtours_mand <- plyr::count(persons_mand, c("PERTYPE","freq_nm"), "finalweight_1")
freq_nmtours_nomand <- plyr::count(persons_nomand, c("PERTYPE","freq_nm"), "finalweight_1")

#test1 <- count(temp2, c("PERTYPE","freq_inm","freq_m","freq_nm","freq_atwork","freq_escort"), "finalweight")
#write.csv(test1, "tour_rate_debug_hhwt.csv", row.names = F)
#test2 <- count(temp2, c("PERTYPE","freq_inm","freq_m","freq_nm","freq_atwork","freq_escort"), "finalweight_1")
#write.csv(test2, "tour_rate_debug_pdaywt.csv", row.names = F)

write.table("Non-Mandatory Tours for Persons with at-least 1 Mandatory Tour", "indivNMTourFreq.csv", sep = ",", row.names = F, append = F)
write.table(freq_nmtours_mand, "indivNMTourFreq.csv", sep = ",", row.names = F, append = T)
write.table("Non-Mandatory Tours for Active Persons with 0 Mandatory Tour", "indivNMTourFreq.csv", sep = ",", row.names = F, append = T)
write.table(freq_nmtours_nomand, "indivNMTourFreq.csv", sep = ",", row.names = F, append = TRUE)

# end of addition for calibration
# -----------------------


# ----------------------
# added for calibration by nagendra.dhakar@rsginc.com

i4tourCounts <- plyr::count(tours, c("HH_ID", "PER_ID", "DAYNO"), "TOURPURP == 4 & FULLY_JOINT==0 & IS_SUBTOUR == 0")
i5tourCounts <- plyr::count(tours, c("HH_ID", "PER_ID", "DAYNO"), "TOURPURP == 5  & FULLY_JOINT==0 & IS_SUBTOUR == 0")
i6tourCounts <- plyr::count(tours, c("HH_ID", "PER_ID", "DAYNO"), "TOURPURP == 6  & FULLY_JOINT==0 & IS_SUBTOUR == 0")
i7tourCounts <- plyr::count(tours, c("HH_ID", "PER_ID", "DAYNO"), "TOURPURP == 7  & FULLY_JOINT==0 & IS_SUBTOUR == 0")
i8tourCounts <- plyr::count(tours, c("HH_ID", "PER_ID", "DAYNO"), "TOURPURP == 8  & FULLY_JOINT==0 & IS_SUBTOUR == 0")
i9tourCounts <- plyr::count(tours, c("HH_ID", "PER_ID", "DAYNO"), "TOURPURP == 9  & FULLY_JOINT==0 & IS_SUBTOUR == 0")

# end of addition for calibration
# -----------------------

joint5 <- plyr::count(jtours, c("HH_ID", "DAYNO"), "JOINT_PURP==5")
joint6 <- plyr::count(jtours, c("HH_ID", "DAYNO"), "JOINT_PURP==6")
joint7 <- plyr::count(jtours, c("HH_ID", "DAYNO"), "JOINT_PURP==7")
joint8 <- plyr::count(jtours, c("HH_ID", "DAYNO"), "JOINT_PURP==8")
joint9 <- plyr::count(jtours, c("HH_ID", "DAYNO"), "JOINT_PURP==9")

hhday$joint5 <- joint5$freq[match(hhday$SAMPN*10+hhday$DAYNO, joint5$HH_ID*10+joint5$DAYNO)]
hhday$joint6 <- joint6$freq[match(hhday$SAMPN*10+hhday$DAYNO, joint6$HH_ID*10+joint6$DAYNO)]
hhday$joint7 <- joint7$freq[match(hhday$SAMPN*10+hhday$DAYNO, joint7$HH_ID*10+joint7$DAYNO)]
hhday$joint8 <- joint8$freq[match(hhday$SAMPN*10+hhday$DAYNO, joint8$HH_ID*10+joint8$DAYNO)]
hhday$joint9 <- joint9$freq[match(hhday$SAMPN*10+hhday$DAYNO, joint9$HH_ID*10+joint9$DAYNO)]
hhday$jtours <- hhday$joint5+hhday$joint6+hhday$joint7+hhday$joint8+hhday$joint9

hhday$joint5[is.na(hhday$joint5)] <- 0
hhday$joint6[is.na(hhday$joint6)] <- 0
hhday$joint7[is.na(hhday$joint7)] <- 0
hhday$joint8[is.na(hhday$joint8)] <- 0
hhday$joint9[is.na(hhday$joint9)] <- 0
hhday$jtours[is.na(hhday$jtours)] <- 0
hhday$JOINT <- 0
hhday$JOINT[hhday$jtours>0] <- 1

# code JTF category
hhday$jtf[hhday$jtours==0] <- 1 
hhday$jtf[hhday$joint5==1] <- 2
hhday$jtf[hhday$joint6==1] <- 3
hhday$jtf[hhday$joint7==1] <- 4
hhday$jtf[hhday$joint8==1] <- 5
hhday$jtf[hhday$joint9==1] <- 6

hhday$jtf[hhday$joint5>=2] <- 7
hhday$jtf[hhday$joint6>=2] <- 8
hhday$jtf[hhday$joint7>=2] <- 9
hhday$jtf[hhday$joint8>=2] <- 10
hhday$jtf[hhday$joint9>=2] <- 11

hhday$jtf[hhday$joint5>=1 & hhday$joint6>=1] <- 12
hhday$jtf[hhday$joint5>=1 & hhday$joint7>=1] <- 13
hhday$jtf[hhday$joint5>=1 & hhday$joint8>=1] <- 14
hhday$jtf[hhday$joint5>=1 & hhday$joint9>=1] <- 15

hhday$jtf[hhday$joint6>=1 & hhday$joint7>=1] <- 16
hhday$jtf[hhday$joint6>=1 & hhday$joint8>=1] <- 17
hhday$jtf[hhday$joint6>=1 & hhday$joint9>=1] <- 18

hhday$jtf[hhday$joint7>=1 & hhday$joint8>=1] <- 19
hhday$jtf[hhday$joint7>=1 & hhday$joint9>=1] <- 20

hhday$jtf[hhday$joint8>=1 & hhday$joint9>=1] <- 21

perday$workTours <- workCounts$freq[match(perday$SAMPN*1000+perday$PERNO*10+perday$DAYNO, workCounts$HH_ID*1000+workCounts$PER_ID*10+workCounts$DAYNO)]
perday$atWorkTours <- atWorkCounts$freq[match(perday$SAMPN*1000+perday$PERNO*10+perday$DAYNO, atWorkCounts$HH_ID*1000+atWorkCounts$PER_ID*10+atWorkCounts$DAYNO)]
perday$schlTours <- schlCounts$freq[match(perday$SAMPN*1000+perday$PERNO*10+perday$DAYNO, schlCounts$HH_ID*1000+schlCounts$PER_ID*10+schlCounts$DAYNO)]
perday$inmTours <- inmCounts$freq[match(perday$SAMPN*1000+perday$PERNO*10+perday$DAYNO, inmCounts$HH_ID*1000+inmCounts$PER_ID*10+inmCounts$DAYNO)]
perday$inmTours[is.na(perday$inmTours)] <- 0
perday$inumTours <- itourCounts$freq[match(perday$SAMPN*1000+perday$PERNO*10+perday$DAYNO, itourCounts$HH_ID*1000+itourCounts$PER_ID*10+itourCounts$DAYNO)]
perday$inumTours[is.na(perday$inumTours)] <- 0
perday$jnumTours <- jtourCounts$freq[match(perday$SAMPN*1000+perday$PERNO*10+perday$DAYNO, jtourCounts$HH_ID*1000+jtourCounts$PER_ID*10+jtourCounts$DAYNO)]
perday$jnumTours[is.na(perday$jnumTours)] <- 0
perday$numTours <- perday$inumTours + perday$jnumTours

perday$workTours[is.na(perday$workTours)] <- 0
perday$schlTours[is.na(perday$schlTours)] <- 0
perday$atWorkTours[is.na(perday$atWorkTours)] <- 0

# ---------------------------------------------------
# added for calibration by nagendra.dhakar@rsginc.com

perday$i4numTours <- i4tourCounts$freq[match(perday$SAMPN*1000+perday$PERNO*10+perday$DAYNO, i4tourCounts$HH_ID*1000+i4tourCounts$PER_ID*10+perday$DAYNO)]
perday$i4numTours[is.na(perday$i4numTours)] <- 0
perday$i5numTours <- i5tourCounts$freq[match(perday$SAMPN*1000+perday$PERNO*10+perday$DAYNO, i5tourCounts$HH_ID*1000+i5tourCounts$PER_ID*10+perday$DAYNO)]
perday$i5numTours[is.na(perday$i5numTours)] <- 0
perday$i6numTours <- i6tourCounts$freq[match(perday$SAMPN*1000+perday$PERNO*10+perday$DAYNO, i6tourCounts$HH_ID*1000+i6tourCounts$PER_ID*10+perday$DAYNO)]
perday$i6numTours[is.na(perday$i6numTours)] <- 0
perday$i7numTours <- i7tourCounts$freq[match(perday$SAMPN*1000+perday$PERNO*10+perday$DAYNO, i7tourCounts$HH_ID*1000+i7tourCounts$PER_ID*10+perday$DAYNO)]
perday$i7numTours[is.na(perday$i7numTours)] <- 0
perday$i8numTours <- i8tourCounts$freq[match(perday$SAMPN*1000+perday$PERNO*10+perday$DAYNO, i8tourCounts$HH_ID*1000+i8tourCounts$PER_ID*10+perday$DAYNO)]
perday$i8numTours[is.na(perday$i8numTours)] <- 0
perday$i9numTours <- i9tourCounts$freq[match(perday$SAMPN*1000+perday$PERNO*10+perday$DAYNO, i9tourCounts$HH_ID*1000+i9tourCounts$PER_ID*10+perday$DAYNO)]
perday$i9numTours[is.na(perday$i9numTours)] <- 0

# end of addition for calibration
# ---------------------------------------------------

# Individual tours by person type
perday$numTours[is.na(perday$numTours)] <- 0
toursPertypeDistbn <- plyr::count(tours[!is.na(tours$TOURPURP) & tours$TOURPURP<=10 & tours$FULLY_JOINT==0 & tours$IS_SUBTOUR==0,], c("PERTYPE"), "finalweight")
write.csv(toursPertypeDistbn, "toursPertypeDistbn.csv", row.names = TRUE)

# Total tours by person type for visualizer
totaltoursPertypeDistbn <- plyr::count(tours[!is.na(tours$TOURPURP) & tours$TOURPURP<=10,], c("PERTYPE"), "finalweight")  #updated by nagendra.dhakar@rsginc, 01/30/2018 -  removed (tours$IS_SUBTOUR==0)
write.csv(totaltoursPertypeDistbn, "total_tours_by_pertype_vis.csv", row.names = F)


# Total indi NM tours by person type and purpose
tours_pertype_purpose <- plyr::count(tours[tours$TOURPURP>=4 & tours$TOURPURP<=9 & tours$FULLY_JOINT==0,], c("PERTYPE", "TOURPURP"), "finalweight")
write.csv(tours_pertype_purpose, "tours_pertype_purpose.csv", row.names = TRUE)

# ---------------------------------------------------
# added for calibration by nagendra.dhakar@rsginc.com

# code indi NM tour category
perday$i4numTours[perday$i4numTours>=2] <- 2
perday$i5numTours[perday$i5numTours>=2] <- 2
perday$i6numTours[perday$i6numTours>=2] <- 2
perday$i7numTours[perday$i7numTours>=1] <- 1
perday$i8numTours[perday$i8numTours>=1] <- 1
perday$i9numTours[perday$i9numTours>=2] <- 2

tours_pertype_esco <- plyr::count(perday, c("PERTYPE", "i4numTours"), "finalweight_1")
tours_pertype_shop <- plyr::count(perday, c("PERTYPE", "i5numTours"), "finalweight_1")
tours_pertype_main <- plyr::count(perday, c("PERTYPE", "i6numTours"), "finalweight_1")
tours_pertype_eati <- plyr::count(perday, c("PERTYPE", "i7numTours"), "finalweight_1")
tours_pertype_visi <- plyr::count(perday, c("PERTYPE", "i8numTours"), "finalweight_1")
tours_pertype_disc <- plyr::count(perday, c("PERTYPE", "i9numTours"), "finalweight_1")


colnames(tours_pertype_esco) <- c("PERTYPE","inumTours","freq")
colnames(tours_pertype_shop) <- c("PERTYPE","inumTours","freq")
colnames(tours_pertype_main) <- c("PERTYPE","inumTours","freq")
colnames(tours_pertype_eati) <- c("PERTYPE","inumTours","freq")
colnames(tours_pertype_visi) <- c("PERTYPE","inumTours","freq")
colnames(tours_pertype_disc) <- c("PERTYPE","inumTours","freq")

tours_pertype_esco$purpose <- 4
tours_pertype_shop$purpose <- 5
tours_pertype_main$purpose <- 6
tours_pertype_eati$purpose <- 7
tours_pertype_visi$purpose <- 8
tours_pertype_disc$purpose <- 9

indi_nm_tours_pertype <- rbind(tours_pertype_esco,tours_pertype_shop,tours_pertype_main,tours_pertype_eati,tours_pertype_visi,tours_pertype_disc)
write.csv(indi_nm_tours_pertype, "inmtours_pertype_purpose.csv", row.names = F)

# end of addition for calibration
# ---------------------------------------------------

tours_pertype_purpose <- xtabs(freq~PERTYPE+TOURPURP, tours_pertype_purpose)
tours_pertype_purpose[is.na(tours_pertype_purpose)] <- 0
tours_pertype_purpose <- addmargins(as.table(tours_pertype_purpose))
tours_pertype_purpose <- as.data.frame.matrix(tours_pertype_purpose)

totalPersons <- sum(pertypeDistbn$freq)
totalPersons_DF <- data.frame("Total", totalPersons)
colnames(totalPersons_DF) <- colnames(pertypeDistbn)
pertypeDF <- rbind(pertypeDistbn, totalPersons_DF)
nm_tour_rates <- tours_pertype_purpose/pertypeDF$freq
nm_tour_rates$pertype <- row.names(nm_tour_rates)
nm_tour_rates <- melt(nm_tour_rates, id = c("pertype"))
colnames(nm_tour_rates) <- c("pertype", "tour_purp", "tour_rate")
nm_tour_rates$pertype <- as.character(nm_tour_rates$pertype)
nm_tour_rates$tour_purp <- as.character(nm_tour_rates$tour_purp)
nm_tour_rates$pertype[nm_tour_rates$pertype=="Sum"] <- "All"
nm_tour_rates$tour_purp[nm_tour_rates$tour_purp=="Sum"] <- "All"
nm_tour_rates$pertype <- pertypeCodes$name[match(nm_tour_rates$pertype, pertypeCodes$code)]

nm_tour_rates$tour_purp[nm_tour_rates$tour_purp==4] <- "Escorting"
nm_tour_rates$tour_purp[nm_tour_rates$tour_purp==5] <- "Shopping"
nm_tour_rates$tour_purp[nm_tour_rates$tour_purp==6] <- "Maintenance"
nm_tour_rates$tour_purp[nm_tour_rates$tour_purp==7] <- "EatingOut"
nm_tour_rates$tour_purp[nm_tour_rates$tour_purp==8] <- "Visiting"
nm_tour_rates$tour_purp[nm_tour_rates$tour_purp==9] <- "Discretionary"

write.csv(nm_tour_rates, "nm_tour_rates.csv", row.names = F)

# Total tours by purpose X tourtype
t1 <- wtd.hist(tours$TOURPURP[!is.na(tours$TOURPURP) & tours$TOURPURP<=10 & tours$FULLY_JOINT==0 & tours$IS_SUBTOUR==0], breaks = seq(1,10, by=1), freq = NULL, right=FALSE, weight=tours$finalweight[!is.na(tours$TOURPURP) & tours$TOURPURP<=10 & tours$FULLY_JOINT==0 & tours$IS_SUBTOUR==0])
t3 <- wtd.hist(jtours$JOINT_PURP[jtours$JOINT_PURP<10], breaks = seq(1,10, by=1), freq = NULL, right=FALSE, weight=jtours$finalweight[jtours$JOINT_PURP<10])
tours_purpose_type <- data.frame(t1$counts, t3$counts)
colnames(tours_purpose_type) <- c("indi", "joint")
write.csv(tours_purpose_type, "tours_purpose_type.csv", row.names = TRUE)


# DAP by pertype
perday$DAP <- "H"
perday$DAP[perday$workTours > 0 | perday$schlTours > 0] <- "M"
perday$DAP[perday$numTours > 0 & perday$DAP == "H"] <- "N"

#CREATING A NEW INSTANCE OF PERDAY TO REMOVE THE NULL VALUES FROM FINALWEIGHT_1
perday2 <- perday[!is.na(perday$finalweight_1)]

dapSummary <- plyr::count(perday2, c("PERTYPE", "DAP"), "finalweight_1")
write.csv(dapSummary, "dapSummary.csv", row.names = TRUE)

dapSummary_day <- plyr::count(perday2, c("DAYNO","PERTYPE", "DAP"), "finalweight_1")
write.csv(dapSummary_day, "dapSummary_day.csv", row.names = TRUE)

# Prepare DAP summary for visualizer
dapSummary_vis <- xtabs(freq~PERTYPE+DAP, dapSummary)
dapSummary_vis <- addmargins(as.table(dapSummary_vis))
dapSummary_vis <- as.data.frame.matrix(dapSummary_vis)

dapSummary_vis$id <- row.names(dapSummary_vis)
dapSummary_vis <- melt(dapSummary_vis, id = c("id"))
colnames(dapSummary_vis) <- c("PERTYPE", "DAP", "freq")
dapSummary_vis$DAP <- as.character(dapSummary_vis$DAP)
dapSummary_vis$PERTYPE <- as.character(dapSummary_vis$PERTYPE)
dapSummary_vis <- dapSummary_vis[dapSummary_vis$DAP!="Sum",]
dapSummary_vis$PERTYPE[dapSummary_vis$PERTYPE=="Sum"] <- "Total"
write.csv(dapSummary_vis, "dapSummary_vis.csv", row.names = TRUE)

# HHSize X Joint
hhsizeJoint <- plyr::count(hhday[hhday$HHSIZE>=2,], c("HHSIZE", "JOINT"), "finalweight")
write.csv(hhsizeJoint, "hhsizeJoint.csv", row.names = TRUE)

#mandatory tour frequency
perday$mtf <- 0
perday$mtf[perday$workTours == 1] <- 1
perday$mtf[perday$workTours >= 2] <- 2
perday$mtf[perday$schlTours == 1] <- 3
perday$mtf[perday$schlTours >= 2] <- 4
perday$mtf[perday$workTours >= 1 & perday$schlTours >= 1] <- 5

mtfSummary <- plyr::count(perday[perday$mtf > 0,], c("PERTYPE", "mtf"), "finalweight_1")
write.csv(mtfSummary, "mtfSummary.csv")
#write.csv(tours, "tours_test.csv")

# Prepare MTF summary for visualizer
mtfSummary_vis <- xtabs(freq~PERTYPE+mtf, mtfSummary)
mtfSummary_vis <- addmargins(as.table(mtfSummary_vis))
mtfSummary_vis <- as.data.frame.matrix(mtfSummary_vis)

mtfSummary_vis$id <- row.names(mtfSummary_vis)
mtfSummary_vis <- melt(mtfSummary_vis, id = c("id"))
colnames(mtfSummary_vis) <- c("PERTYPE", "MTF", "freq")
mtfSummary_vis$PERTYPE <- as.character(mtfSummary_vis$PERTYPE)
mtfSummary_vis$MTF <- as.character(mtfSummary_vis$MTF)
mtfSummary_vis <- mtfSummary_vis[mtfSummary_vis$MTF!="Sum",]
mtfSummary_vis$PERTYPE[mtfSummary_vis$PERTYPE=="Sum"] <- "Total"
write.csv(mtfSummary_vis, "mtfSummary_vis.csv")

# indi NM summary
inm0Summary <- plyr::count(perday2[perday2$inmTours==0,], c("PERTYPE"), "finalweight_1")
inm1Summary <- plyr::count(perday2[perday2$inmTours==1,], c("PERTYPE"), "finalweight_1")
inm2Summary <- plyr::count(perday2[perday2$inmTours==2,], c("PERTYPE"), "finalweight_1")
inm3Summary <- plyr::count(perday2[perday2$inmTours>=3,], c("PERTYPE"), "finalweight_1")

inmSummary <- data.frame(PERTYPE = c(1,2,3,4,5,6,7,8))
inmSummary$tour0 <- inm0Summary$freq[match(inmSummary$PERTYPE, inm0Summary$PERTYPE)]
inmSummary$tour1 <- inm1Summary$freq[match(inmSummary$PERTYPE, inm1Summary$PERTYPE)]
inmSummary$tour2 <- inm2Summary$freq[match(inmSummary$PERTYPE, inm2Summary$PERTYPE)]
inmSummary$tour3pl <- inm3Summary$freq[match(inmSummary$PERTYPE, inm3Summary$PERTYPE)]

write.table(inmSummary, "innmSummary.csv", col.names=TRUE, sep=",")

# prepare INM summary for visualizer
inmSummary_vis <- melt(inmSummary, id=c("PERTYPE"))
inmSummary_vis$variable <- as.character(inmSummary_vis$variable)
inmSummary_vis$variable[inmSummary_vis$variable=="tour0"] <- "0"
inmSummary_vis$variable[inmSummary_vis$variable=="tour1"] <- "1"
inmSummary_vis$variable[inmSummary_vis$variable=="tour2"] <- "2"
inmSummary_vis$variable[inmSummary_vis$variable=="tour3pl"] <- "3pl"
inmSummary_vis <- xtabs(value~PERTYPE+variable, inmSummary_vis)
inmSummary_vis <- addmargins(as.table(inmSummary_vis))
inmSummary_vis <- as.data.frame.matrix(inmSummary_vis)

inmSummary_vis$id <- row.names(inmSummary_vis)
inmSummary_vis <- melt(inmSummary_vis, id = c("id"))
colnames(inmSummary_vis) <- c("PERTYPE", "nmtours", "freq")
inmSummary_vis$PERTYPE <- as.character(inmSummary_vis$PERTYPE)
inmSummary_vis$nmtours <- as.character(inmSummary_vis$nmtours)
inmSummary_vis <- inmSummary_vis[inmSummary_vis$nmtours!="Sum",]
inmSummary_vis$PERTYPE[inmSummary_vis$PERTYPE=="Sum"] <- "Total"
write.csv(inmSummary_vis, "inmSummary_vis.csv")

# Joint Tour Frequency and composition
jtfSummary <- plyr::count(hhday[!is.na(hhday$jtf),], c("jtf"), "finalweight")
jointComp <- plyr::count(jtours[jtours$JOINT_PURP>=5 & jtours$JOINT_PURP<=9,], c("COMPOSITION"), "finalweight")
jointPartySize <- plyr::count(jtours[jtours$JOINT_PURP>=5 & jtours$JOINT_PURP<=9,], c("NUMBER_HH"), "finalweight")
jointCompPartySize <- plyr::count(jtours[jtours$JOINT_PURP>=5 & jtours$JOINT_PURP<=9,], c("COMPOSITION","NUMBER_HH"), "finalweight")

hhday$jointCat[hhday$jtours==0] <- 0
hhday$jointCat[hhday$jtours==1] <- 1
hhday$jointCat[hhday$jtours>=2] <- 2

jointToursHHSize <- plyr::count(hhday[!is.na(hhday$HHSIZE) & !is.na(hhday$jointCat),], c("HHSIZE", "jointCat"), "finalweight")

write.table(jtfSummary, "jtfSummary.csv", col.names=TRUE, sep=",")
write.table(jointComp, "jtfSummary.csv", col.names=TRUE, sep=",", append=TRUE)
write.table(jointPartySize, "jtfSummary.csv", col.names=TRUE, sep=",", append=TRUE)
write.table(jointCompPartySize, "jtfSummary.csv", col.names=TRUE, sep=",", append=TRUE)
write.table(jointToursHHSize, "jtfSummary.csv", col.names=TRUE, sep=",", append=TRUE)

#cap joint party size to 5+
jointPartySize$freq[jointPartySize$NUMBER_HH==5] <- sum(jointPartySize$freq[jointPartySize$NUMBER_HH>=5])
jointPartySize <- jointPartySize[jointPartySize$NUMBER_HH<=5, ]

jtf <- data.frame(jtf_code = seq(from = 1, to = 21), 
                  alt_name = c("No Joint Tours", "1 Shopping", "1 Maintenance", "1 Eating Out", "1 Visiting", "1 Other Discretionary", 
                               "2 Shopping", "1 Shopping / 1 Maintenance", "1 Shopping / 1 Eating Out", "1 Shopping / 1 Visiting", 
                               "1 Shopping / 1 Other Discretionary", "2 Maintenance", "1 Maintenance / 1 Eating Out", 
                               "1 Maintenance / 1 Visiting", "1 Maintenance / 1 Other Discretionary", "2 Eating Out", "1 Eating Out / 1 Visiting", 
                               "1 Eating Out / 1 Other Discretionary", "2 Visiting", "1 Visiting / 1 Other Discretionary", "2 Other Discretionary"))
jtf$freq <- jtfSummary$freq[match(jtf$jtf_code, jtfSummary$jtf)]
jtf[is.na(jtf)] <- 0

jointComp$COMPOSITION[jointComp$COMPOSITION==1] <- "All Adult"
jointComp$COMPOSITION[jointComp$COMPOSITION==2] <- "All Children"
jointComp$COMPOSITION[jointComp$COMPOSITION==3] <- "Mixed"

jointToursHHSizeProp <- xtabs(freq~jointCat+HHSIZE, jointToursHHSize[jointToursHHSize$HHSIZE>1,])
jointToursHHSizeProp <- addmargins(as.table(jointToursHHSizeProp))
jointToursHHSizeProp <- jointToursHHSizeProp[-4,]  #remove last row 
jointToursHHSizeProp <- prop.table(jointToursHHSizeProp, margin = 2)
jointToursHHSizeProp <- as.data.frame.matrix(jointToursHHSizeProp)
jointToursHHSizeProp <- jointToursHHSizeProp*100
jointToursHHSizeProp$jointTours <- row.names(jointToursHHSizeProp)
jointToursHHSizeProp <- melt(jointToursHHSizeProp, id = c("jointTours"))
colnames(jointToursHHSizeProp) <- c("jointTours", "hhsize", "freq")
jointToursHHSizeProp$hhsize <- as.character(jointToursHHSizeProp$hhsize)
jointToursHHSizeProp$hhsize[jointToursHHSizeProp$hhsize=="Sum"] <- "Total"

jointCompPartySize$COMPOSITION[jointCompPartySize$COMPOSITION==1] <- "All Adult"
jointCompPartySize$COMPOSITION[jointCompPartySize$COMPOSITION==2] <- "All Children"
jointCompPartySize$COMPOSITION[jointCompPartySize$COMPOSITION==3] <- "Mixed"

jointCompPartySizeProp <- xtabs(freq~COMPOSITION+NUMBER_HH, jointCompPartySize)
jointCompPartySizeProp <- addmargins(as.table(jointCompPartySizeProp))
jointCompPartySizeProp <- jointCompPartySizeProp[,-6]  #remove last row 
jointCompPartySizeProp <- prop.table(jointCompPartySizeProp, margin = 1)
jointCompPartySizeProp <- as.data.frame.matrix(jointCompPartySizeProp)
jointCompPartySizeProp <- jointCompPartySizeProp*100
jointCompPartySizeProp$comp <- row.names(jointCompPartySizeProp)
jointCompPartySizeProp <- melt(jointCompPartySizeProp, id = c("comp"))
colnames(jointCompPartySizeProp) <- c("comp", "partysize", "freq")
jointCompPartySizeProp$comp <- as.character(jointCompPartySizeProp$comp)
jointCompPartySizeProp$comp[jointCompPartySizeProp$comp=="Sum"] <- "Total"

# Cap joint comp party size at 5
jointCompPartySizeProp <- jointCompPartySizeProp[jointCompPartySizeProp$partysize!="Sum",]
jointCompPartySizeProp$partysize <- as.numeric(as.character(jointCompPartySizeProp$partysize))
jointCompPartySizeProp$freq[jointCompPartySizeProp$comp=="All Adult" & jointCompPartySizeProp$partysize==5] <- 
  sum(jointCompPartySizeProp$freq[jointCompPartySizeProp$comp=="All Adult" & jointCompPartySizeProp$partysize>=5])
jointCompPartySizeProp$freq[jointCompPartySizeProp$comp=="All Children" & jointCompPartySizeProp$partysize==5] <- 
  sum(jointCompPartySizeProp$freq[jointCompPartySizeProp$comp=="All Children" & jointCompPartySizeProp$partysize>=5])
jointCompPartySizeProp$freq[jointCompPartySizeProp$comp=="Mixed" & jointCompPartySizeProp$partysize==5] <- 
  sum(jointCompPartySizeProp$freq[jointCompPartySizeProp$comp=="Mixed" & jointCompPartySizeProp$partysize>=5])
jointCompPartySizeProp$freq[jointCompPartySizeProp$comp=="Total" & jointCompPartySizeProp$partysize==5] <- 
  sum(jointCompPartySizeProp$freq[jointCompPartySizeProp$comp=="Total" & jointCompPartySizeProp$partysize>=5])

jointCompPartySizeProp <- jointCompPartySizeProp[jointCompPartySizeProp$partysize<=5,]



write.csv(jtf, "jtf.csv", row.names = F)
write.csv(jointComp, "jointComp.csv", row.names = F)
write.csv(jointPartySize, "jointPartySize.csv", row.names = F)
write.csv(jointCompPartySizeProp, "jointCompPartySize.csv", row.names = F)
write.csv(jointToursHHSizeProp, "jointToursHHSize.csv", row.names = F)

# TOD Profile
tod1 <- wtd.hist(tours$ANCHOR_DEPART_BIN[tours$TOURPURP==1 & tours$FULLY_JOINT==0], breaks = seq(1,41, by=1), freq = NULL, right=FALSE, weight = tours$finalweight[tours$TOURPURP==1 & tours$FULLY_JOINT==0])
tod2 <- wtd.hist(tours$ANCHOR_DEPART_BIN[tours$TOURPURP==2 & tours$FULLY_JOINT==0], breaks = seq(1,41, by=1), freq = NULL, right=FALSE, weight = tours$finalweight[tours$TOURPURP==2 & tours$FULLY_JOINT==0])
tod3 <- wtd.hist(tours$ANCHOR_DEPART_BIN[tours$TOURPURP==3 & tours$FULLY_JOINT==0], breaks = seq(1,41, by=1), freq = NULL, right=FALSE, weight = tours$finalweight[tours$TOURPURP==3 & tours$FULLY_JOINT==0])
tod4 <- wtd.hist(tours$ANCHOR_DEPART_BIN[tours$TOURPURP==4 & tours$FULLY_JOINT==0], breaks = seq(1,41, by=1), freq = NULL, right=FALSE, weight = tours$finalweight[tours$TOURPURP==4 & tours$FULLY_JOINT==0])
todi56 <- wtd.hist(tours$ANCHOR_DEPART_BIN[tours$TOURPURP>=5 & tours$TOURPURP<=6 & tours$FULLY_JOINT==0], breaks = seq(1,41, by=1), freq = NULL, right=FALSE, weight = tours$finalweight[tours$TOURPURP>=5 & tours$TOURPURP<=6 & tours$FULLY_JOINT==0])
todi789 <- wtd.hist(tours$ANCHOR_DEPART_BIN[tours$TOURPURP>=7 & tours$TOURPURP<=9 & tours$FULLY_JOINT==0], breaks = seq(1,41, by=1), freq = NULL, right=FALSE, weight = tours$finalweight[tours$TOURPURP>=7 & tours$TOURPURP<=9 & tours$FULLY_JOINT==0])
todj56 <- wtd.hist(jtours$ANCHOR_DEPART_BIN[jtours$JOINT_PURP>=5 & jtours$JOINT_PURP<=6], breaks = seq(1,41, by=1), freq = NULL, right=FALSE, weight = jtours$finalweight[jtours$JOINT_PURP>=5 & jtours$JOINT_PURP<=6])
todj789 <- wtd.hist(jtours$ANCHOR_DEPART_BIN[jtours$JOINT_PURP>=7 & jtours$JOINT_PURP<=9], breaks = seq(1,41, by=1), freq = NULL, right=FALSE, weight = jtours$finalweight[jtours$JOINT_PURP>=7 & jtours$JOINT_PURP<=9])
tod15 <- wtd.hist(tours$ANCHOR_DEPART_BIN[tours$IS_SUBTOUR == 1], breaks = seq(1,41, by=1), freq = NULL, right=FALSE, weight = tours$finalweight[tours$IS_SUBTOUR == 1])

todDepProfile <- data.frame(tod1$counts, tod2$counts, tod3$counts, tod4$counts, todi56$counts, todi789$counts
                         , todj56$counts, todj789$counts, tod15$counts)
colnames(todDepProfile) <- c("work", "univ", "sch", "esc", "imain", "idisc", 
                          "jmain", "jdisc", "atwork")
write.csv(todDepProfile, "todDepProfile.csv")

# prepare input for visualizer
todDepProfile_vis <- todDepProfile
todDepProfile_vis$id <- row.names(todDepProfile_vis)
todDepProfile_vis <- melt(todDepProfile_vis, id = c("id"))
colnames(todDepProfile_vis) <- c("id", "purpose", "freq")

todDepProfile_vis$purpose <- as.character(todDepProfile_vis$purpose)
todDepProfile_vis <- xtabs(freq~id+purpose, todDepProfile_vis)
todDepProfile_vis <- addmargins(as.table(todDepProfile_vis))
todDepProfile_vis <- as.data.frame.matrix(todDepProfile_vis)
todDepProfile_vis$id <- row.names(todDepProfile_vis)
todDepProfile_vis <- melt(todDepProfile_vis, id = c("id"))
colnames(todDepProfile_vis) <- c("timebin", "PURPOSE", "freq")
todDepProfile_vis$PURPOSE <- as.character(todDepProfile_vis$PURPOSE)
todDepProfile_vis$timebin <- as.character(todDepProfile_vis$timebin)
todDepProfile_vis <- todDepProfile_vis[todDepProfile_vis$timebin!="Sum",]
todDepProfile_vis$PURPOSE[todDepProfile_vis$PURPOSE=="Sum"] <- "Total"
todDepProfile_vis$timebin <- as.numeric(todDepProfile_vis$timebin)

arr1 <- wtd.hist(tours$ANCHOR_ARRIVE_BIN[tours$TOURPURP==1 & tours$FULLY_JOINT==0], breaks = seq(1,41, by=1), freq = NULL, right=FALSE, weight = tours$finalweight[tours$TOURPURP==1 & tours$FULLY_JOINT==0])
arr2 <- wtd.hist(tours$ANCHOR_ARRIVE_BIN[tours$TOURPURP==2 & tours$FULLY_JOINT==0], breaks = seq(1,41, by=1), freq = NULL, right=FALSE, weight = tours$finalweight[tours$TOURPURP==2 & tours$FULLY_JOINT==0])
arr3 <- wtd.hist(tours$ANCHOR_ARRIVE_BIN[tours$TOURPURP==3 & tours$FULLY_JOINT==0], breaks = seq(1,41, by=1), freq = NULL, right=FALSE, weight = tours$finalweight[tours$TOURPURP==3 & tours$FULLY_JOINT==0])
arr4 <- wtd.hist(tours$ANCHOR_ARRIVE_BIN[tours$TOURPURP==4 & tours$FULLY_JOINT==0], breaks = seq(1,41, by=1), freq = NULL, right=FALSE, weight = tours$finalweight[tours$TOURPURP==4 & tours$FULLY_JOINT==0])
arri56 <- wtd.hist(tours$ANCHOR_ARRIVE_BIN[tours$TOURPURP>=5 & tours$TOURPURP<=6 & tours$FULLY_JOINT==0], breaks = seq(1,41, by=1), freq = NULL, right=FALSE, weight = tours$finalweight[tours$TOURPURP>=5 & tours$TOURPURP<=6 & tours$FULLY_JOINT==0])
arri789 <- wtd.hist(tours$ANCHOR_ARRIVE_BIN[tours$TOURPURP>=7 & tours$TOURPURP<=9 & tours$FULLY_JOINT==0], breaks = seq(1,41, by=1), freq = NULL, right=FALSE, weight = tours$finalweight[tours$TOURPURP>=7 & tours$TOURPURP<=9 & tours$FULLY_JOINT==0])
arrj56 <- wtd.hist(jtours$ANCHOR_ARRIVE_BIN[jtours$JOINT_PURP>=5 & jtours$JOINT_PURP<=6], breaks = seq(1,41, by=1), freq = NULL, right=FALSE, weight = jtours$finalweight[jtours$JOINT_PURP>=5 & jtours$JOINT_PURP<=6])
arrj789 <- wtd.hist(jtours$ANCHOR_ARRIVE_BIN[jtours$JOINT_PURP>=7 & jtours$JOINT_PURP<=9], breaks = seq(1,41, by=1), freq = NULL, right=FALSE, weight = jtours$finalweight[jtours$JOINT_PURP>=7 & jtours$JOINT_PURP<=9])
arr15 <- wtd.hist(tours$ANCHOR_ARRIVE_BIN[tours$IS_SUBTOUR == 1], breaks = seq(1,41, by=1), freq = NULL, right=FALSE, weight = tours$finalweight[tours$IS_SUBTOUR == 1])


todArrProfile <- data.frame(arr1$counts, arr2$counts, arr3$counts, arr4$counts, arri56$counts, arri789$counts
                            , arrj56$counts, arrj789$counts, arr15$counts)
colnames(todArrProfile) <- c("work", "univ", "sch", "esc", "imain", "idisc", 
                             "jmain", "jdisc", "atwork")
write.csv(todArrProfile, "todArrProfile.csv")

# prepare input for visualizer
todArrProfile_vis <- todArrProfile
todArrProfile_vis$id <- row.names(todArrProfile_vis)
todArrProfile_vis <- melt(todArrProfile_vis, id = c("id"))
colnames(todArrProfile_vis) <- c("id", "purpose", "freq_arr")

todArrProfile_vis$purpose <- as.character(todArrProfile_vis$purpose)
todArrProfile_vis <- xtabs(freq_arr~id+purpose, todArrProfile_vis)
todArrProfile_vis <- addmargins(as.table(todArrProfile_vis))
todArrProfile_vis <- as.data.frame.matrix(todArrProfile_vis)
todArrProfile_vis$id <- row.names(todArrProfile_vis)
todArrProfile_vis <- melt(todArrProfile_vis, id = c("id"))
colnames(todArrProfile_vis) <- c("timebin", "PURPOSE", "freq")
todArrProfile_vis$PURPOSE <- as.character(todArrProfile_vis$PURPOSE)
todArrProfile_vis$timebin <- as.character(todArrProfile_vis$timebin)
todArrProfile_vis <- todArrProfile_vis[todArrProfile_vis$timebin!="Sum",]
todArrProfile_vis$PURPOSE[todArrProfile_vis$PURPOSE=="Sum"] <- "Total"
todArrProfile_vis$timebin <- as.numeric(todArrProfile_vis$timebin)


## Prepare tour depart and arrive distribution for non-mandatory purposes
disc_tours <- tours[tours$TOURPURP>=4 & tours$TOURPURP<=9, c("TOURPURP", "ANCHOR_DEPART_BIN", "ANCHOR_ARRIVE_BIN", "finalweight")]

arrive_depart_disc_tours <- aggregate(disc_tours$finalweight, 
                                      by=list(Purpose = disc_tours$TOURPURP, 
                                              DepPeriod = disc_tours$ANCHOR_DEPART_BIN, 
                                              ArrPeriod = disc_tours$ANCHOR_ARRIVE_BIN), 
                                      sum)
arrive_depart_distbn <- expand.grid(Purpose = seq(4,9), 
                                    DepPeriod = seq(1,40), 
                                    ArrPeriod = seq(1,40))

arrive_depart_distbn$NumTours <- arrive_depart_disc_tours$x[match(paste(arrive_depart_distbn$Purpose, 
                                                                        arrive_depart_distbn$DepPeriod, 
                                                                        arrive_depart_distbn$ArrPeriod, sep = "-"), 
                                                                  paste(arrive_depart_disc_tours$Purpose, 
                                                                        arrive_depart_disc_tours$DepPeriod, 
                                                                        arrive_depart_disc_tours$ArrPeriod, sep = "-"))]

#  set NumTours to zero for illogical arrive-dep combos
arrive_depart_distbn[is.na(arrive_depart_distbn)] <- 0
arrive_depart_distbn$NumTours[arrive_depart_distbn$DepPeriod>arrive_depart_distbn$ArrPeriod] <- 0
arrive_depart_purp_total <- aggregate(arrive_depart_distbn$NumTours, 
                                  by = list(Purpose = arrive_depart_distbn$Purpose), 
                                  sum)

arrive_depart_distbn$PurpTotal <- arrive_depart_purp_total$x[match(arrive_depart_distbn$Purpose, 
                                                                   arrive_depart_purp_total$Purpose)]

arrive_depart_distbn$Percent <- round(arrive_depart_distbn$NumTours/arrive_depart_distbn$PurpTotal, digits = 5) 
#arrive_depart_distbn$Purpose <- arrive_depart_distbn$Purpose - 4 # start purposes from 0

### for each purpose, add the adjustment to time combo with highest share
#for(pp in 4:9){
#  maxPercent <- max(arrive_depart_distbn$Percent[arrive_depart_distbn$Purpose==pp])
#  adjustment <- round(1 - sum(arrive_depart_distbn$Percent[arrive_depart_distbn$Purpose==pp]),5)
#  arrive_depart_distbn$Percent[arrive_depart_distbn$Purpose==pp & arrive_depart_distbn$Percent==maxPercent] <- 
#    arrive_depart_distbn$Percent[arrive_depart_distbn$Purpose==pp & arrive_depart_distbn$Percent==maxPercent] + adjustment
#}
#arrive_depart_distbn$Percent <- round(arrive_depart_distbn$NumTours/arrive_depart_distbn$PurpTotal, digits = 5)

# manual adjustment to ensure percentage sum to one for each purpose
# adding to max precent record
arrive_depart_distbn$Percent[arrive_depart_distbn$Purpose==4 & arrive_depart_distbn$DepPeriod==7 & arrive_depart_distbn$ArrPeriod==8] <- 
  arrive_depart_distbn$Percent[arrive_depart_distbn$Purpose==4 & arrive_depart_distbn$DepPeriod==7 & arrive_depart_distbn$ArrPeriod==8] - 0.00011
arrive_depart_distbn$Percent[arrive_depart_distbn$Purpose==5 & arrive_depart_distbn$DepPeriod==23 & arrive_depart_distbn$ArrPeriod==24] <- 
  arrive_depart_distbn$Percent[arrive_depart_distbn$Purpose==5 & arrive_depart_distbn$DepPeriod==23 & arrive_depart_distbn$ArrPeriod==24] + 0.00001
arrive_depart_distbn$Percent[arrive_depart_distbn$Purpose==6 & arrive_depart_distbn$DepPeriod==24 & arrive_depart_distbn$ArrPeriod==36] <- 
  arrive_depart_distbn$Percent[arrive_depart_distbn$Purpose==6 & arrive_depart_distbn$DepPeriod==24 & arrive_depart_distbn$ArrPeriod==36] + 0.00010
arrive_depart_distbn$Percent[arrive_depart_distbn$Purpose==7 & arrive_depart_distbn$DepPeriod==28 & arrive_depart_distbn$ArrPeriod==32] <- 
  arrive_depart_distbn$Percent[arrive_depart_distbn$Purpose==7 & arrive_depart_distbn$DepPeriod==28 & arrive_depart_distbn$ArrPeriod==32] - 0.00005
arrive_depart_distbn$Percent[arrive_depart_distbn$Purpose==8 & arrive_depart_distbn$DepPeriod==22 & arrive_depart_distbn$ArrPeriod==27] <- 
  arrive_depart_distbn$Percent[arrive_depart_distbn$Purpose==8 & arrive_depart_distbn$DepPeriod==22 & arrive_depart_distbn$ArrPeriod==27] + 0.00010
arrive_depart_distbn$Percent[arrive_depart_distbn$Purpose==9 & arrive_depart_distbn$DepPeriod==25 & arrive_depart_distbn$ArrPeriod==28] <- 
  arrive_depart_distbn$Percent[arrive_depart_distbn$Purpose==9 & arrive_depart_distbn$DepPeriod==25 & arrive_depart_distbn$ArrPeriod==28] - 0.00004

arrive_depart_distbn <- arrive_depart_distbn[ order(arrive_depart_distbn$Purpose, 
                                                                              arrive_depart_distbn$DepPeriod, 
                                                                              arrive_depart_distbn$ArrPeriod),]

arrive_depart_distbn <- arrive_depart_distbn[,c("Purpose","DepPeriod","ArrPeriod","Percent")]
colnames(arrive_depart_distbn) <- c("Purpose","OutboundPeriod","ReturnPeriod","Percent")
write.csv(arrive_depart_distbn, "Non_Mand_Tours_ArrDep_Distbn.csv", row.names = F, quote = F)


##stops by direction, purpose and model tod

tours$start_tod <- 5 # EA: 3 am - 6 am
tours$start_tod <- ifelse(tours$ANCHOR_DEPART_BIN>=4 & tours$ANCHOR_DEPART_BIN<=9, 1, tours$start_tod)   # AM: 6 am - 9 am
tours$start_tod <- ifelse(tours$ANCHOR_DEPART_BIN>=10 & tours$ANCHOR_DEPART_BIN<=22, 2, tours$start_tod) # MD: 9 am - 3:30 pm
tours$start_tod <- ifelse(tours$ANCHOR_DEPART_BIN>=23 & tours$ANCHOR_DEPART_BIN<=29, 3, tours$start_tod) # PM: 3:30 pm - 7 pm
tours$start_tod <- ifelse(tours$ANCHOR_DEPART_BIN>=30 & tours$ANCHOR_DEPART_BIN<=40, 4, tours$start_tod) # EV: 7 pm - 3 am

tours$end_tod <- 5 # EA: 3 am - 6 am
tours$end_tod <- ifelse(tours$ANCHOR_ARRIVE_BIN>=4 & tours$ANCHOR_ARRIVE_BIN<=9, 1, tours$end_tod)   # AM: 6 am - 9 am
tours$end_tod <- ifelse(tours$ANCHOR_ARRIVE_BIN>=10 & tours$ANCHOR_ARRIVE_BIN<=22, 2, tours$end_tod) # MD: 9 am - 3:30 pm
tours$end_tod <- ifelse(tours$ANCHOR_ARRIVE_BIN>=23 & tours$ANCHOR_ARRIVE_BIN<=29, 3, tours$end_tod) # PM: 3:30 pm - 7 pm
tours$end_tod <- ifelse(tours$ANCHOR_ARRIVE_BIN>=30 & tours$ANCHOR_ARRIVE_BIN<=40, 4, tours$end_tod) # EV: 7 pm - 3 am

temp <- tours[tours$FULLY_JOINT==0,]
temp[is.na(temp)] <- 0
stops_ib_tod <- aggregate(INBOUND_STOPS*finalweight~TOURPURP+start_tod+end_tod, data=temp, FUN = sum)
stops_ob_tod <- aggregate(OUTBOUND_STOPS*finalweight~TOURPURP+start_tod+end_tod, data=temp, FUN = sum)
write.csv(stops_ib_tod, "todStopsIB.csv", row.names = F)
write.csv(stops_ob_tod, "todStopsOB.csv", row.names = F)

#joint tours
jtours$start_tod <- 5 # EA: 3 am - 6 am
jtours$start_tod <- ifelse(jtours$ANCHOR_DEPART_BIN>=4 & jtours$ANCHOR_DEPART_BIN<=9, 1, jtours$start_tod)   # AM: 6 am - 9 am
jtours$start_tod <- ifelse(jtours$ANCHOR_DEPART_BIN>=10 & jtours$ANCHOR_DEPART_BIN<=22, 2, jtours$start_tod) # MD: 9 am - 3:30 pm
jtours$start_tod <- ifelse(jtours$ANCHOR_DEPART_BIN>=23 & jtours$ANCHOR_DEPART_BIN<=29, 3, jtours$start_tod) # PM: 3:30 pm - 7 pm
jtours$start_tod <- ifelse(jtours$ANCHOR_DEPART_BIN>=30 & jtours$ANCHOR_DEPART_BIN<=40, 4, jtours$start_tod) # EV: 7 pm - 3 am

jtours$end_tod <- 5 # EA: 3 am - 6 am
jtours$end_tod <- ifelse(jtours$ANCHOR_ARRIVE_BIN>=4 & jtours$ANCHOR_ARRIVE_BIN<=9, 1, jtours$end_tod)   # AM: 6 am - 9 am
jtours$end_tod <- ifelse(jtours$ANCHOR_ARRIVE_BIN>=10 & jtours$ANCHOR_ARRIVE_BIN<=22, 2, jtours$end_tod) # MD: 9 am - 3:30 pm
jtours$end_tod <- ifelse(jtours$ANCHOR_ARRIVE_BIN>=23 & jtours$ANCHOR_ARRIVE_BIN<=29, 3, jtours$end_tod) # PM: 3:30 pm - 7 pm
jtours$end_tod <- ifelse(jtours$ANCHOR_ARRIVE_BIN>=30 & jtours$ANCHOR_ARRIVE_BIN<=40, 4, jtours$end_tod) # EV: 7 pm - 3 am

temp <- jtours
temp[is.na(temp)] <- 0
jstops_ib_tod <- aggregate(INBOUND_STOPS*finalweight~JOINT_PURP+start_tod+end_tod, data=temp, FUN = sum)
jstops_ob_tod <- aggregate(OUTBOUND_STOPS*finalweight~JOINT_PURP+start_tod+end_tod, data=temp, FUN = sum)
write.csv(jstops_ib_tod, "todStopsIB_joint.csv", row.names = F)
write.csv(jstops_ob_tod, "todStopsOB_joint.csv", row.names = F)


# filter out records with missing or negative tour duration
xtours <- tours[!is.na(tours$TOUR_DUR_BIN) & tours$TOUR_DUR_BIN>0 & tours$TOUR_DUR_BIN!="NaN",]
xjtours <- jtours[!is.na(jtours$TOUR_DUR_BIN) & jtours$TOUR_DUR_BIN>0 & jtours$TOUR_DUR_BIN!="NaN",]

dur1 <- wtd.hist(xtours$TOUR_DUR_BIN[xtours$TOURPURP==1 & xtours$FULLY_JOINT==0], breaks = seq(1,41, by=1), freq = NULL, right=FALSE, weight = xtours$finalweight[xtours$TOURPURP==1 & xtours$FULLY_JOINT==0])
dur2 <- wtd.hist(xtours$TOUR_DUR_BIN[xtours$TOURPURP==2 & xtours$FULLY_JOINT==0], breaks = seq(1,41, by=1), freq = NULL, right=FALSE, weight = xtours$finalweight[xtours$TOURPURP==2 & xtours$FULLY_JOINT==0])
dur3 <- wtd.hist(xtours$TOUR_DUR_BIN[xtours$TOURPURP==3 & xtours$FULLY_JOINT==0], breaks = seq(1,41, by=1), freq = NULL, right=FALSE, weight = xtours$finalweight[xtours$TOURPURP==3 & xtours$FULLY_JOINT==0])
dur4 <- wtd.hist(xtours$TOUR_DUR_BIN[xtours$TOURPURP==4 & xtours$FULLY_JOINT==0], breaks = seq(1,41, by=1), freq = NULL, right=FALSE, weight = xtours$finalweight[xtours$TOURPURP==4 & xtours$FULLY_JOINT==0])
duri56 <- wtd.hist(xtours$TOUR_DUR_BIN[xtours$TOURPURP>=5 & xtours$TOURPURP<=6 & xtours$FULLY_JOINT==0], breaks = seq(1,41, by=1), freq = NULL, right=FALSE, weight = xtours$finalweight[xtours$TOURPURP>=5 & xtours$TOURPURP<=6 & xtours$FULLY_JOINT==0])
duri789 <- wtd.hist(xtours$TOUR_DUR_BIN[xtours$TOURPURP>=7 & xtours$TOURPURP<=9 & xtours$FULLY_JOINT==0], breaks = seq(1,41, by=1), freq = NULL, right=FALSE, weight = xtours$finalweight[xtours$TOURPURP>=7 & xtours$TOURPURP<=9 & xtours$FULLY_JOINT==0])
durj56 <- wtd.hist(xjtours$TOUR_DUR_BIN[xjtours$JOINT_PURP>=5 & xjtours$JOINT_PURP<=6], breaks = seq(1,41, by=1), freq = NULL, right=FALSE, weight = xjtours$finalweight[xjtours$JOINT_PURP>=5 & xjtours$JOINT_PURP<=6])
durj789 <- wtd.hist(xjtours$TOUR_DUR_BIN[xjtours$JOINT_PURP>=7 & xjtours$JOINT_PURP<=9], breaks = seq(1,41, by=1), freq = NULL, right=FALSE, weight = xjtours$finalweight[xjtours$JOINT_PURP>=7 & xjtours$JOINT_PURP<=9])
dur15 <- wtd.hist(xtours$TOUR_DUR_BIN[xtours$IS_SUBTOUR == 1], breaks = seq(1,41, by=1), freq = NULL, right=FALSE, weight = xtours$finalweight[xtours$IS_SUBTOUR == 1])

todDurProfile <- data.frame(dur1$counts, dur2$counts, dur3$counts, dur4$counts, duri56$counts, duri789$counts
                            , durj56$counts, durj789$counts, dur15$counts)
colnames(todDurProfile) <- c("work", "univ", "sch", "esc", "imain", "idisc", 
                             "jmain", "jdisc", "atwork")
write.csv(todDurProfile, "todDurProfile.csv")

# prepare input for visualizer
todDurProfile_vis <- todDurProfile
todDurProfile_vis$id <- row.names(todDurProfile_vis)
todDurProfile_vis <- melt(todDurProfile_vis, id = c("id"))
colnames(todDurProfile_vis) <- c("id", "purpose", "freq_dur")

todDurProfile_vis$purpose <- as.character(todDurProfile_vis$purpose)
todDurProfile_vis <- xtabs(freq_dur~id+purpose, todDurProfile_vis)
todDurProfile_vis <- addmargins(as.table(todDurProfile_vis))
todDurProfile_vis <- as.data.frame.matrix(todDurProfile_vis)
todDurProfile_vis$id <- row.names(todDurProfile_vis)
todDurProfile_vis <- melt(todDurProfile_vis, id = c("id"))
colnames(todDurProfile_vis) <- c("timebin", "PURPOSE", "freq")
todDurProfile_vis$PURPOSE <- as.character(todDurProfile_vis$PURPOSE)
todDurProfile_vis$timebin <- as.character(todDurProfile_vis$timebin)
todDurProfile_vis <- todDurProfile_vis[todDurProfile_vis$timebin!="Sum",]
todDurProfile_vis$PURPOSE[todDurProfile_vis$PURPOSE=="Sum"] <- "Total"
todDurProfile_vis$timebin <- as.numeric(todDurProfile_vis$timebin)

todDepProfile_vis <- todDepProfile_vis[order(todDepProfile_vis$timebin, todDepProfile_vis$PURPOSE), ]
todArrProfile_vis <- todArrProfile_vis[order(todArrProfile_vis$timebin, todArrProfile_vis$PURPOSE), ]
todDurProfile_vis <- todDurProfile_vis[order(todDurProfile_vis$timebin, todDurProfile_vis$PURPOSE), ]
todProfile_vis <- data.frame(todDepProfile_vis, todArrProfile_vis$freq, todDurProfile_vis$freq)
colnames(todProfile_vis) <- c("id", "purpose", "freq_dep", "freq_arr", "freq_dur")
write.csv(todProfile_vis, "todProfile_vis.csv", row.names = F)

'
# Non Mand Tour lengths
tourdist4 <- wtd.hist(tours$DISTMILE[!is.na(tours$DISTMILE) & tours$TOURPURP==4 & tours$IS_SUBTOUR==0], breaks = c(seq(0,40, by=1), 9999), freq = NULL, right=FALSE, weight = tours$finalweight[!is.na(tours$DISTMILE) & tours$TOURPURP==4 & tours$IS_SUBTOUR==0])
tourdist5 <- wtd.hist(tours$DISTMILE[!is.na(tours$DISTMILE) & tours$TOURPURP==5 & tours$IS_SUBTOUR==0], breaks = c(seq(0,40, by=1), 9999), freq = NULL, right=FALSE, weight = tours$finalweight[!is.na(tours$DISTMILE) & tours$TOURPURP==5 & tours$IS_SUBTOUR==0])
tourdist6 <- wtd.hist(tours$DISTMILE[!is.na(tours$DISTMILE) & tours$TOURPURP==6 & tours$IS_SUBTOUR==0], breaks = c(seq(0,40, by=1), 9999), freq = NULL, right=FALSE, weight = tours$finalweight[!is.na(tours$DISTMILE) & tours$TOURPURP==6 & tours$IS_SUBTOUR==0])
tourdist7 <- wtd.hist(tours$DISTMILE[!is.na(tours$DISTMILE) & tours$TOURPURP==7 & tours$IS_SUBTOUR==0], breaks = c(seq(0,40, by=1), 9999), freq = NULL, right=FALSE, weight = tours$finalweight[!is.na(tours$DISTMILE) & tours$TOURPURP==7 & tours$IS_SUBTOUR==0])
tourdist8 <- wtd.hist(tours$DISTMILE[!is.na(tours$DISTMILE) & tours$TOURPURP==8 & tours$IS_SUBTOUR==0], breaks = c(seq(0,40, by=1), 9999), freq = NULL, right=FALSE, weight = tours$finalweight[!is.na(tours$DISTMILE) & tours$TOURPURP==8 & tours$IS_SUBTOUR==0])
tourdist9 <- wtd.hist(tours$DISTMILE[!is.na(tours$DISTMILE) & tours$TOURPURP==9 & tours$IS_SUBTOUR==0], breaks = c(seq(0,40, by=1), 9999), freq = NULL, right=FALSE, weight = tours$finalweight[!is.na(tours$DISTMILE) & tours$TOURPURP==9 & tours$IS_SUBTOUR==0])
tourdist10 <- wtd.hist(tours$DISTMILE[!is.na(tours$DISTMILE) & tours$TOURPURP==10 & tours$IS_SUBTOUR==1], breaks = c(seq(0,40, by=1), 9999), freq = NULL, right=FALSE, weight = tours$finalweight[!is.na(tours$DISTMILE) & tours$TOURPURP==10 & tours$IS_SUBTOUR==1])

tourDistProfile <- data.frame(tourdist4$counts, tourdist5$counts, tourdist6$counts, tourdist7$counts, tourdist8$counts, tourdist9$counts, tourdist10$counts)
colnames(tourDistProfile) <- c("esco", "shop", "main", "eati", "visi", "disc", "atwork")
write.csv(tourDistProfile, "tourDistProfile.csv")
'

#put TAXI mode in SR2
#tours$TOURMODE <- ifelse(tours$TOURMODE==10,2, tours$TOURMODE)

# Tour Mode X Auto Suff
tmode1_as0 <- wtd.hist(tours$TOURMODE[!is.na(tours$TOURMODE) & tours$TOURPURP==1 & tours$FULLY_JOINT==0 & tours$AUTOSUFF==0], breaks = seq(1,12, by=1), freq = NULL, right=FALSE, weight = tours$finalweight[!is.na(tours$TOURMODE) & tours$TOURPURP==1 & tours$FULLY_JOINT==0 & tours$AUTOSUFF==0])
tmode2_as0 <- wtd.hist(tours$TOURMODE[!is.na(tours$TOURMODE) & tours$TOURPURP==2 & tours$FULLY_JOINT==0 & tours$AUTOSUFF==0], breaks = seq(1,12, by=1), freq = NULL, right=FALSE, weight = tours$finalweight[!is.na(tours$TOURMODE) & tours$TOURPURP==2 & tours$FULLY_JOINT==0 & tours$AUTOSUFF==0])
tmode3_as0 <- wtd.hist(tours$TOURMODE[!is.na(tours$TOURMODE) & tours$TOURPURP==3 & tours$FULLY_JOINT==0 & tours$AUTOSUFF==0], breaks = seq(1,12, by=1), freq = NULL, right=FALSE, weight = tours$finalweight[!is.na(tours$TOURMODE) & tours$TOURPURP==3 & tours$FULLY_JOINT==0 & tours$AUTOSUFF==0])
tmode4_as0 <- wtd.hist(tours$TOURMODE[!is.na(tours$TOURMODE) & tours$TOURPURP>=4 & tours$TOURPURP<=6 & tours$FULLY_JOINT==0 & tours$AUTOSUFF==0], breaks = seq(1,12, by=1), freq = NULL, right=FALSE, weight = tours$finalweight[!is.na(tours$TOURMODE) & tours$TOURPURP>=4 & tours$TOURPURP<=6 & tours$FULLY_JOINT==0 & tours$AUTOSUFF==0])
tmode5_as0 <- wtd.hist(tours$TOURMODE[!is.na(tours$TOURMODE) & tours$TOURPURP>=7 & tours$TOURPURP<=9 & tours$FULLY_JOINT==0 & tours$AUTOSUFF==0], breaks = seq(1,12, by=1), freq = NULL, right=FALSE, weight = tours$finalweight[!is.na(tours$TOURMODE) & tours$TOURPURP>=7 & tours$TOURPURP<=9 & tours$FULLY_JOINT==0 & tours$AUTOSUFF==0])
tmode6_as0 <- wtd.hist(jtours$TOURMODE[jtours$JOINT_PURP>=4 & jtours$JOINT_PURP<=6 & jtours$AUTOSUFF==0], breaks = seq(1,12, by=1), freq = NULL, right=FALSE, weight = jtours$finalweight[jtours$JOINT_PURP>=4 & jtours$JOINT_PURP<=6 & jtours$AUTOSUFF==0])
tmode7_as0 <- wtd.hist(jtours$TOURMODE[jtours$JOINT_PURP>=7 & jtours$JOINT_PURP<=9 & jtours$AUTOSUFF==0], breaks = seq(1,12, by=1), freq = NULL, right=FALSE, weight = jtours$finalweight[jtours$JOINT_PURP>=7 & jtours$JOINT_PURP<=9 & jtours$AUTOSUFF==0])
tmode8_as0 <- wtd.hist(tours$TOURMODE[!is.na(tours$TOURMODE) & tours$IS_SUBTOUR==1 & tours$AUTOSUFF==0 & tours$FULLY_JOINT==0], breaks = seq(1,12, by=1), freq = NULL, right=FALSE, weight = tours$finalweight[!is.na(tours$TOURMODE) & tours$IS_SUBTOUR==1 & tours$AUTOSUFF==0 & tours$FULLY_JOINT==0])

tmodeAS0Profile <- data.frame(tmode1_as0$counts, tmode2_as0$counts, tmode3_as0$counts, tmode4_as0$counts,
                              tmode5_as0$counts, tmode6_as0$counts, tmode7_as0$counts, tmode8_as0$counts)
colnames(tmodeAS0Profile) <- c("work", "univ", "sch", "imain", "idisc", "jmain", "jdisc", "atwork")
write.csv(tmodeAS0Profile, "tmodeAS0Profile.csv")

# Prepeare data for visualizer
tmodeAS0Profile_vis <- tmodeAS0Profile[1:9,]
tmodeAS0Profile_vis$id <- row.names(tmodeAS0Profile_vis)
tmodeAS0Profile_vis <- melt(tmodeAS0Profile_vis, id = c("id"))
colnames(tmodeAS0Profile_vis) <- c("id", "purpose", "freq_as0")

tmodeAS0Profile_vis <- xtabs(freq_as0~id+purpose, tmodeAS0Profile_vis)
tmodeAS0Profile_vis[is.na(tmodeAS0Profile_vis)] <- 0
tmodeAS0Profile_vis <- addmargins(as.table(tmodeAS0Profile_vis))
tmodeAS0Profile_vis <- as.data.frame.matrix(tmodeAS0Profile_vis)

tmodeAS0Profile_vis$id <- row.names(tmodeAS0Profile_vis)
tmodeAS0Profile_vis <- melt(tmodeAS0Profile_vis, id = c("id"))
colnames(tmodeAS0Profile_vis) <- c("id", "purpose", "freq_as0")
tmodeAS0Profile_vis$id <- as.character(tmodeAS0Profile_vis$id)
tmodeAS0Profile_vis$purpose <- as.character(tmodeAS0Profile_vis$purpose)
tmodeAS0Profile_vis <- tmodeAS0Profile_vis[tmodeAS0Profile_vis$id!="Sum",]
tmodeAS0Profile_vis$purpose[tmodeAS0Profile_vis$purpose=="Sum"] <- "Total"


tmode1_as1 <- wtd.hist(tours$TOURMODE[!is.na(tours$TOURMODE) & tours$TOURPURP==1 & tours$FULLY_JOINT==0 & tours$AUTOSUFF==1], breaks = seq(1,12, by=1), freq = NULL, right=FALSE, weight = tours$finalweight[!is.na(tours$TOURMODE) & tours$TOURPURP==1 & tours$FULLY_JOINT==0 & tours$AUTOSUFF==1])
tmode2_as1 <- wtd.hist(tours$TOURMODE[!is.na(tours$TOURMODE) & tours$TOURPURP==2 & tours$FULLY_JOINT==0 & tours$AUTOSUFF==1], breaks = seq(1,12, by=1), freq = NULL, right=FALSE, weight = tours$finalweight[!is.na(tours$TOURMODE) & tours$TOURPURP==2 & tours$FULLY_JOINT==0 & tours$AUTOSUFF==1])
tmode3_as1 <- wtd.hist(tours$TOURMODE[!is.na(tours$TOURMODE) & tours$TOURPURP==3 & tours$FULLY_JOINT==0 & tours$AUTOSUFF==1], breaks = seq(1,12, by=1), freq = NULL, right=FALSE, weight = tours$finalweight[!is.na(tours$TOURMODE) & tours$TOURPURP==3 & tours$FULLY_JOINT==0 & tours$AUTOSUFF==1])
tmode4_as1 <- wtd.hist(tours$TOURMODE[!is.na(tours$TOURMODE) & tours$TOURPURP>=4 & tours$TOURPURP<=6 & tours$FULLY_JOINT==0 & tours$AUTOSUFF==1], breaks = seq(1,12, by=1), freq = NULL, right=FALSE, weight = tours$finalweight[!is.na(tours$TOURMODE) & tours$TOURPURP>=4 & tours$TOURPURP<=6 & tours$FULLY_JOINT==0 & tours$AUTOSUFF==1])
tmode5_as1 <- wtd.hist(tours$TOURMODE[!is.na(tours$TOURMODE) & tours$TOURPURP>=7 & tours$TOURPURP<=9 & tours$FULLY_JOINT==0 & tours$AUTOSUFF==1], breaks = seq(1,12, by=1), freq = NULL, right=FALSE, weight = tours$finalweight[!is.na(tours$TOURMODE) & tours$TOURPURP>=7 & tours$TOURPURP<=9 & tours$FULLY_JOINT==0 & tours$AUTOSUFF==1])
tmode6_as1 <- wtd.hist(jtours$TOURMODE[jtours$JOINT_PURP>=4 & jtours$JOINT_PURP<=6 & jtours$AUTOSUFF==1], breaks = seq(1,12, by=1), freq = NULL, right=FALSE, weight = jtours$finalweight[jtours$JOINT_PURP>=4 & jtours$JOINT_PURP<=6 & jtours$AUTOSUFF==1])
tmode7_as1 <- wtd.hist(jtours$TOURMODE[jtours$JOINT_PURP>=7 & jtours$JOINT_PURP<=9 & jtours$AUTOSUFF==1], breaks = seq(1,12, by=1), freq = NULL, right=FALSE, weight = jtours$finalweight[jtours$JOINT_PURP>=7 & jtours$JOINT_PURP<=9 & jtours$AUTOSUFF==1])
tmode8_as1 <- wtd.hist(tours$TOURMODE[!is.na(tours$TOURMODE) & tours$IS_SUBTOUR==1 & tours$AUTOSUFF==1 & tours$FULLY_JOINT==0], breaks = seq(1,12, by=1), freq = NULL, right=FALSE, weight = tours$finalweight[!is.na(tours$TOURMODE) & tours$IS_SUBTOUR==1 & tours$AUTOSUFF==1 & tours$FULLY_JOINT==0])

tmodeAS1Profile <- data.frame(tmode1_as1$counts, tmode2_as1$counts, tmode3_as1$counts, tmode4_as1$counts,
                              tmode5_as1$counts, tmode6_as1$counts, tmode7_as1$counts, tmode8_as1$counts)
colnames(tmodeAS1Profile) <- c("work", "univ", "sch", "imain", "idisc", "jmain", "jdisc", "atwork")
write.csv(tmodeAS1Profile, "tmodeAS1Profile.csv")

# Prepeare data for visualizer
tmodeAS1Profile_vis <- tmodeAS1Profile[1:9,]
tmodeAS1Profile_vis$id <- row.names(tmodeAS1Profile_vis)
tmodeAS1Profile_vis <- melt(tmodeAS1Profile_vis, id = c("id"))
colnames(tmodeAS1Profile_vis) <- c("id", "purpose", "freq_as1")

tmodeAS1Profile_vis <- xtabs(freq_as1~id+purpose, tmodeAS1Profile_vis)
tmodeAS1Profile_vis[is.na(tmodeAS1Profile_vis)] <- 0
tmodeAS1Profile_vis <- addmargins(as.table(tmodeAS1Profile_vis))
tmodeAS1Profile_vis <- as.data.frame.matrix(tmodeAS1Profile_vis)

tmodeAS1Profile_vis$id <- row.names(tmodeAS1Profile_vis)
tmodeAS1Profile_vis <- melt(tmodeAS1Profile_vis, id = c("id"))
colnames(tmodeAS1Profile_vis) <- c("id", "purpose", "freq_as1")
tmodeAS1Profile_vis$id <- as.character(tmodeAS1Profile_vis$id)
tmodeAS1Profile_vis$purpose <- as.character(tmodeAS1Profile_vis$purpose)
tmodeAS1Profile_vis <- tmodeAS1Profile_vis[tmodeAS1Profile_vis$id!="Sum",]
tmodeAS1Profile_vis$purpose[tmodeAS1Profile_vis$purpose=="Sum"] <- "Total"

tmode1_as2 <- wtd.hist(tours$TOURMODE[!is.na(tours$TOURMODE) & tours$TOURPURP==1 & tours$FULLY_JOINT==0 & tours$AUTOSUFF==2], breaks = seq(1,12, by=1), freq = NULL, right=FALSE, weight = tours$finalweight[!is.na(tours$TOURMODE) & tours$TOURPURP==1 & tours$FULLY_JOINT==0 & tours$AUTOSUFF==2])
tmode2_as2 <- wtd.hist(tours$TOURMODE[!is.na(tours$TOURMODE) & tours$TOURPURP==2 & tours$FULLY_JOINT==0 & tours$AUTOSUFF==2], breaks = seq(1,12, by=1), freq = NULL, right=FALSE, weight = tours$finalweight[!is.na(tours$TOURMODE) & tours$TOURPURP==2 & tours$FULLY_JOINT==0 & tours$AUTOSUFF==2])
tmode3_as2 <- wtd.hist(tours$TOURMODE[!is.na(tours$TOURMODE) & tours$TOURPURP==3 & tours$FULLY_JOINT==0 & tours$AUTOSUFF==2], breaks = seq(1,12, by=1), freq = NULL, right=FALSE, weight = tours$finalweight[!is.na(tours$TOURMODE) & tours$TOURPURP==3 & tours$FULLY_JOINT==0 & tours$AUTOSUFF==2])
tmode4_as2 <- wtd.hist(tours$TOURMODE[!is.na(tours$TOURMODE) & tours$TOURPURP>=4 & tours$TOURPURP<=6 & tours$FULLY_JOINT==0 & tours$AUTOSUFF==2], breaks = seq(1,12, by=1), freq = NULL, right=FALSE, weight = tours$finalweight[!is.na(tours$TOURMODE) & tours$TOURPURP>=4 & tours$TOURPURP<=6 & tours$FULLY_JOINT==0 & tours$AUTOSUFF==2])
tmode5_as2 <- wtd.hist(tours$TOURMODE[!is.na(tours$TOURMODE) & tours$TOURPURP>=7 & tours$TOURPURP<=9 & tours$FULLY_JOINT==0 & tours$AUTOSUFF==2], breaks = seq(1,12, by=1), freq = NULL, right=FALSE, weight = tours$finalweight[!is.na(tours$TOURMODE) & tours$TOURPURP>=7 & tours$TOURPURP<=9 & tours$FULLY_JOINT==0 & tours$AUTOSUFF==2])
tmode6_as2 <- wtd.hist(jtours$TOURMODE[jtours$JOINT_PURP>=4 & jtours$JOINT_PURP<=6 & jtours$AUTOSUFF==2], breaks = seq(1,12, by=1), freq = NULL, right=FALSE, weight = jtours$finalweight[jtours$JOINT_PURP>=4 & jtours$JOINT_PURP<=6 & jtours$AUTOSUFF==2])
tmode7_as2 <- wtd.hist(jtours$TOURMODE[jtours$JOINT_PURP>=7 & jtours$JOINT_PURP<=9 & jtours$AUTOSUFF==2], breaks = seq(1,12, by=1), freq = NULL, right=FALSE, weight = jtours$finalweight[jtours$JOINT_PURP>=7 & jtours$JOINT_PURP<=9 & jtours$AUTOSUFF==2])
tmode8_as2 <- wtd.hist(tours$TOURMODE[!is.na(tours$TOURMODE) & tours$IS_SUBTOUR==1 & tours$AUTOSUFF==2 & tours$FULLY_JOINT==0], breaks = seq(1,12, by=1), freq = NULL, right=FALSE, weight = tours$finalweight[!is.na(tours$TOURMODE) & tours$IS_SUBTOUR==1 & tours$AUTOSUFF==2 & tours$FULLY_JOINT==0])

tmodeAS2Profile <- data.frame(tmode1_as2$counts, tmode2_as2$counts, tmode3_as2$counts, tmode4_as2$counts,
                              tmode5_as2$counts, tmode6_as2$counts, tmode7_as2$counts, tmode8_as2$counts)
colnames(tmodeAS2Profile) <- c("work", "univ", "sch", "imain", "idisc", "jmain", "jdisc", "atwork")
write.csv(tmodeAS2Profile, "tmodeAS2Profile.csv")

# Prepeare data for visualizer
tmodeAS2Profile_vis <- tmodeAS2Profile[1:9,]
tmodeAS2Profile_vis$id <- row.names(tmodeAS2Profile_vis)
tmodeAS2Profile_vis <- melt(tmodeAS2Profile_vis, id = c("id"))
colnames(tmodeAS2Profile_vis) <- c("id", "purpose", "freq_as2")

tmodeAS2Profile_vis <- xtabs(freq_as2~id+purpose, tmodeAS2Profile_vis)
tmodeAS2Profile_vis[is.na(tmodeAS2Profile_vis)] <- 0
tmodeAS2Profile_vis <- addmargins(as.table(tmodeAS2Profile_vis))
tmodeAS2Profile_vis <- as.data.frame.matrix(tmodeAS2Profile_vis)

tmodeAS2Profile_vis$id <- row.names(tmodeAS2Profile_vis)
tmodeAS2Profile_vis <- melt(tmodeAS2Profile_vis, id = c("id"))
colnames(tmodeAS2Profile_vis) <- c("id", "purpose", "freq_as2")
tmodeAS2Profile_vis$id <- as.character(tmodeAS2Profile_vis$id)
tmodeAS2Profile_vis$purpose <- as.character(tmodeAS2Profile_vis$purpose)
tmodeAS2Profile_vis <- tmodeAS2Profile_vis[tmodeAS2Profile_vis$id!="Sum",]
tmodeAS2Profile_vis$purpose[tmodeAS2Profile_vis$purpose=="Sum"] <- "Total"


# Combine three AS groups
tmodeProfile_vis <- data.frame(tmodeAS0Profile_vis, tmodeAS1Profile_vis$freq_as1, tmodeAS2Profile_vis$freq_as2)
colnames(tmodeProfile_vis) <- c("id", "purpose", "freq_as0", "freq_as1", "freq_as2")
tmodeProfile_vis$freq_all <- tmodeProfile_vis$freq_as0 + tmodeProfile_vis$freq_as1 + tmodeProfile_vis$freq_as2
write.csv(tmodeProfile_vis, "tmodeProfile_vis.csv", row.names = F)


###########
#tour mode by time period - for Wu, Sun
#calculate time of day
#tours$tod <- 5 # EA: 3 am - 6 am
#tours$tod <- ifelse(tours$PRIMDEST_ARRIVE_BIN>=4 & tours$PRIMDEST_ARRIVE_BIN<=9, 1, tours$tod)   # AM: 6 am - 9 am
#tours$tod <- ifelse(tours$PRIMDEST_ARRIVE_BIN>=10 & tours$PRIMDEST_ARRIVE_BIN<=22, 2, tours$tod) # MD: 9 am - 3:30 pm
#tours$tod <- ifelse(tours$PRIMDEST_ARRIVE_BIN>=23 & tours$PRIMDEST_ARRIVE_BIN<=29, 3, tours$tod) # PM: 3:30 pm - 7 pm
#tours$tod <- ifelse(tours$PRIMDEST_ARRIVE_BIN>=30 & tours$PRIMDEST_ARRIVE_BIN<=40, 4, tours$tod) # EV: 7 pm - 3 am

#order of the tod calculations matter. AM and PM are the focus, so set them later so that they are not overwritten.
tours$temp_anchor_depart_bin <- tours$ANCHOR_DEPART_BIN
tours$temp_anchor_arrive_bin <- tours$ANCHOR_ARRIVE_BIN
tours[is.na(tours$temp_anchor_depart_bin),]$temp_anchor_depart_bin <- 0 #if unavaialble set to start of the day
tours[is.na(tours$temp_anchor_arrive_bin),]$temp_anchor_arrive_bin <- 40 #if unavailable set to end of the day

tours$tod <- 5 # EA: 3 am - 6 am
tours$tod <- ifelse(tours$temp_anchor_depart_bin<=22 & tours$temp_anchor_arrive_bin>=10, 2, tours$tod) # MD: 9 am - 3:30 pm
tours$tod <- ifelse(tours$temp_anchor_depart_bin<=40 & tours$temp_anchor_arrive_bin>=30, 4, tours$tod) # EV: 7 pm - 3 am
tours$tod <- ifelse(tours$temp_anchor_depart_bin<=9 & tours$temp_anchor_arrive_bin>=4, 1, tours$tod)   # AM: 6 am - 9 am
tours$tod <- ifelse(tours$temp_anchor_depart_bin<=29 & tours$temp_anchor_arrive_bin>=23, 3, tours$tod) # PM: 3:30 pm - 7 pm
tours$tod <- ifelse(tours$temp_anchor_depart_bin<=9 & tours$temp_anchor_arrive_bin>=23,6, tours$tod) # both AM and PM

tours$tod <- ifelse(is.na(tours$tod),5,tours$tod)
tours$num_tours <- tours$finalweight

#jtours$temp_dest_arr_bin <- round(0.5*(jtours$ANCHOR_DEPART_BIN + jtours$ANCHOR_ARRIVE_BIN))
#jtours$tod <- 5 # EA: 3 am - 6 am
#jtours$tod <- ifelse(jtours$temp_dest_arr_bin>=4 & jtours$temp_dest_arr_bin<=9, 1, jtours$tod)   # AM: 6 am - 9 am
#jtours$tod <- ifelse(jtours$temp_dest_arr_bin>=10 & jtours$temp_dest_arr_bin<=22, 2, jtours$tod) # MD: 9 am - 3:30 pm
#jtours$tod <- ifelse(jtours$temp_dest_arr_bin>=23 & jtours$temp_dest_arr_bin<=29, 3, jtours$tod) # PM: 3:30 pm - 7 pm
#jtours$tod <- ifelse(jtours$temp_dest_arr_bin>=30 & jtours$temp_dest_arr_bin<=40, 4, jtours$tod) # EV: 7 pm - 3 am

jtours$temp_anchor_depart_bin <- jtours$ANCHOR_DEPART_BIN
jtours$temp_anchor_arrive_bin <- jtours$ANCHOR_ARRIVE_BIN
jtours[is.na(jtours$temp_anchor_depart_bin),]$temp_anchor_depart_bin <- 0 #if unavaialble set to start of the day
jtours[is.na(jtours$temp_anchor_arrive_bin),]$temp_anchor_arrive_bin <- 40 #if unavailable set to end of the day

jtours$tod <- 5 # EA: 3 am - 6 am
jtours$tod <- ifelse(jtours$temp_anchor_depart_bin<=22 & jtours$temp_anchor_arrive_bin>=10, 2, jtours$tod) # MD: 9 am - 3:30 pm
jtours$tod <- ifelse(jtours$temp_anchor_depart_bin<=40 & jtours$temp_anchor_arrive_bin>=30, 2, jtours$tod) # EV: 7 pm - 3 am
jtours$tod <- ifelse(jtours$temp_anchor_depart_bin<=9 & jtours$temp_anchor_arrive_bin>=4, 1, jtours$tod)   # AM: 6 am - 9 am
jtours$tod <- ifelse(jtours$temp_anchor_depart_bin<=29 & jtours$temp_anchor_arrive_bin>=23, 3, jtours$tod) # PM: 3:30 pm - 7 pm
jtours$tod <- ifelse(jtours$temp_anchor_depart_bin<=9 & jtours$temp_anchor_arrive_bin>=23,6 , jtours$tod) # both AM and PM
jtours$tod <- ifelse(is.na(jtours$tod),5,jtours$tod)
jtours$num_tours <- jtours$finalweight
#jtours$num_tours <- jtours$finalweight * jtours$NUMBER_HH

itours_summary <- aggregate(num_tours~tod+TOURPURP+TOURMODE+AUTOSUFF, data=tours[tours$FULLY_JOINT==0,], FUN=sum)
jtours_summary <- aggregate(num_tours~tod+JOINT_PURP+TOURMODE+AUTOSUFF, data=jtours, FUN=sum)

write.csv(itours_summary, "itours_tourmode_summary.csv", row.names = F)
write.csv(jtours_summary, "jtours_tourmode_summary.csv", row.names = F)

#for off peak (op = ea+md+ev)
tours$tod <- 2 # EA: 3 am - 6 am
tours$tod <- ifelse(tours$temp_anchor_depart_bin<=9 & tours$temp_anchor_arrive_bin>=4, 1, tours$tod)   # AM: 6 am - 9 am
tours$tod <- ifelse(tours$temp_anchor_depart_bin<=29 & tours$temp_anchor_arrive_bin>=23, 3, tours$tod) # PM: 3:30 pm - 7 pm
tours$tod <- ifelse(tours$temp_anchor_depart_bin<=3, 2, tours$tod) # EA: 3 am - 6 am
tours$tod <- ifelse(tours$temp_anchor_depart_bin<=22 & tours$temp_anchor_arrive_bin>=10, 2, tours$tod) # MD: 9 am - 3:30 pm
tours$tod <- ifelse(tours$temp_anchor_depart_bin<=40 & tours$temp_anchor_arrive_bin>=30, 2, tours$tod) # EV: 7 pm - 3 am

tours$tod <- ifelse((tours$temp_anchor_depart_bin<=3 & tours$temp_anchor_arrive_bin>=10 &  tours$temp_anchor_arrive_bin<=22) |
                      (tours$temp_anchor_depart_bin<=3 & tours$temp_anchor_arrive_bin>=30) |
                      (tours$temp_anchor_depart_bin>=10 & tours$temp_anchor_depart_bin<=22 & tours$temp_anchor_arrive_bin>=30), 6, tours$tod) # OP (ea+md+ev)


jtours$tod <- 2 # EA: 3 am - 6 am
jtours$tod <- ifelse(jtours$temp_anchor_depart_bin<=9 & jtours$temp_anchor_arrive_bin>=4, 1, jtours$tod)   # AM: 6 am - 9 am
jtours$tod <- ifelse(jtours$temp_anchor_depart_bin<=29 & jtours$temp_anchor_arrive_bin>=23, 3, jtours$tod) # PM: 3:30 pm - 7 pm
jtours$tod <- ifelse(jtours$temp_anchor_depart_bin<=3, 2, jtours$tod) # EA: 3 am - 6 am
jtours$tod <- ifelse(jtours$temp_anchor_depart_bin<=22 & jtours$temp_anchor_arrive_bin>=10, 2, jtours$tod) # MD: 9 am - 3:30 pm
jtours$tod <- ifelse(jtours$temp_anchor_depart_bin<=40 & jtours$temp_anchor_arrive_bin>=30, 2, jtours$tod) # EV: 7 pm - 3 am

jtours$tod <- ifelse((jtours$temp_anchor_depart_bin<=3 & jtours$temp_anchor_arrive_bin>=10 &  jtours$temp_anchor_arrive_bin<=22) |
                      (jtours$temp_anchor_depart_bin<=3 & jtours$temp_anchor_arrive_bin>=30) |
                      (jtours$temp_anchor_depart_bin>=10 & jtours$temp_anchor_depart_bin<=22 & jtours$temp_anchor_arrive_bin>=30), 6, jtours$tod) # OP (ea+md+ev)

itours_summary_op <- aggregate(num_tours~tod+TOURPURP+TOURMODE+AUTOSUFF, data=tours[tours$FULLY_JOINT==0,], FUN=sum)
jtours_summary_op <- aggregate(num_tours~tod+JOINT_PURP+TOURMODE+AUTOSUFF, data=jtours, FUN=sum)

write.csv(itours_summary_op, "itours_tourmode_summary_op.csv", row.names = F)
write.csv(jtours_summary_op, "jtours_tourmode_summary_op.csv", row.names = F)

###########

# Non-mandatory tour distance profile
tourdist4 <- wtd.hist(tours$SKIMDIST[tours$TOURPURP==4 & tours$FULLY_JOINT==0], breaks = c(seq(0,40, by=1), 9999), freq = NULL, right=FALSE, weight = tours$finalweight[tours$TOURPURP==4 & tours$FULLY_JOINT==0])
tourdisti56 <- wtd.hist(tours$SKIMDIST[tours$TOURPURP>=5 & tours$TOURPURP<=6 & tours$FULLY_JOINT==0], breaks = c(seq(0,40, by=1), 9999), freq = NULL, right=FALSE, weight = tours$finalweight[tours$TOURPURP>=5 & tours$TOURPURP<=6 & tours$FULLY_JOINT==0])
tourdisti789 <- wtd.hist(tours$SKIMDIST[tours$TOURPURP>=7 & tours$TOURPURP<=9 & tours$FULLY_JOINT==0], breaks = c(seq(0,40, by=1), 9999), freq = NULL, right=FALSE, weight = tours$finalweight[tours$TOURPURP>=7 & tours$TOURPURP<=9 & tours$FULLY_JOINT==0])
tourdistj56 <- wtd.hist(jtours$SKIMDIST[jtours$JOINT_PURP>=5 & jtours$JOINT_PURP<=6], breaks = c(seq(0,40, by=1), 9999), freq = NULL, right=FALSE, weight = jtours$finalweight[jtours$JOINT_PURP>=5 & jtours$JOINT_PURP<=6])
tourdistj789 <- wtd.hist(jtours$SKIMDIST[jtours$JOINT_PURP>=7 & jtours$JOINT_PURP<=9], breaks = c(seq(0,40, by=1), 9999), freq = NULL, right=FALSE, weight = jtours$finalweight[jtours$JOINT_PURP>=7 & jtours$JOINT_PURP<=9])
tourdist10 <- wtd.hist(tours$SKIMDIST[tours$IS_SUBTOUR == 1], breaks = c(seq(0,40, by=1), 9999), freq = NULL, right=FALSE, weight = tours$finalweight[tours$IS_SUBTOUR == 1])

tourDistProfile <- data.frame(tourdist4$counts, tourdisti56$counts, tourdisti789$counts, tourdistj56$counts, tourdistj789$counts, tourdist10$counts)
colnames(tourDistProfile) <- c("esco", "imain", "idisc", "jmain", "jdisc", "atwork")
write.csv(tourDistProfile, "nonMandTourDistProfile.csv")

#prepare input for visualizer
tourDistProfile_vis <- tourDistProfile
tourDistProfile_vis$id <- row.names(tourDistProfile_vis)
tourDistProfile_vis <- melt(tourDistProfile_vis, id = c("id"))
colnames(tourDistProfile_vis) <- c("id", "purpose", "freq")

tourDistProfile_vis <- xtabs(freq~id+purpose, tourDistProfile_vis)
tourDistProfile_vis <- addmargins(as.table(tourDistProfile_vis))
tourDistProfile_vis <- as.data.frame.matrix(tourDistProfile_vis)
tourDistProfile_vis$id <- row.names(tourDistProfile_vis)
tourDistProfile_vis <- melt(tourDistProfile_vis, id = c("id"))
colnames(tourDistProfile_vis) <- c("distbin", "PURPOSE", "freq")
tourDistProfile_vis$PURPOSE <- as.character(tourDistProfile_vis$PURPOSE)
tourDistProfile_vis$distbin <- as.character(tourDistProfile_vis$distbin)
tourDistProfile_vis <- tourDistProfile_vis[tourDistProfile_vis$distbin!="Sum",]
tourDistProfile_vis$PURPOSE[tourDistProfile_vis$PURPOSE=="Sum"] <- "Total"
tourDistProfile_vis$distbin <- as.numeric(tourDistProfile_vis$distbin)

write.csv(tourDistProfile_vis, "tourDistProfile_vis.csv", row.names = F)

cat("\n Average Tour Distance [esco]: ", weighted.mean(tours$SKIMDIST[tours$TOURPURP==4 & tours$FULLY_JOINT==0], tours$finalweight[tours$TOURPURP==4 & tours$FULLY_JOINT==0], na.rm = TRUE))
cat("\n Average Tour Distance [imain]: ", weighted.mean(tours$SKIMDIST[tours$TOURPURP>=5 & tours$TOURPURP<=6 & tours$FULLY_JOINT==0], tours$finalweight[tours$TOURPURP>=5 & tours$TOURPURP<=6 & tours$FULLY_JOINT==0], na.rm = TRUE))
cat("\n Average Tour Distance [idisc]: ", weighted.mean(tours$SKIMDIST[tours$TOURPURP>=7 & tours$TOURPURP<=9 & tours$FULLY_JOINT==0], tours$finalweight[tours$TOURPURP>=7 & tours$TOURPURP<=9 & tours$FULLY_JOINT==0], na.rm = TRUE))
cat("\n Average Tour Distance [jmain]: ", weighted.mean(jtours$SKIMDIST[jtours$JOINT_PURP>=5 & jtours$JOINT_PURP<=6], jtours$finalweight[jtours$JOINT_PURP>=5 & jtours$JOINT_PURP<=6], na.rm = TRUE))
cat("\n Average Tour Distance [jdisc]: ", weighted.mean(jtours$SKIMDIST[jtours$JOINT_PURP>=7 & jtours$JOINT_PURP<=9], jtours$finalweight[jtours$JOINT_PURP>=7 & jtours$JOINT_PURP<=9], na.rm = TRUE))
cat("\n Average Tour Distance [atwork]: ", weighted.mean(tours$SKIMDIST[tours$IS_SUBTOUR == 1], tours$finalweight[tours$IS_SUBTOUR == 1], na.rm = TRUE))

## Output average trips lengths for visualizer

avgTripLengths <- c(weighted.mean(tours$SKIMDIST[tours$TOURPURP==4 & tours$FULLY_JOINT==0], tours$finalweight[tours$TOURPURP==4 & tours$FULLY_JOINT==0], na.rm = TRUE),
                    weighted.mean(tours$SKIMDIST[tours$TOURPURP>=5 & tours$TOURPURP<=6 & tours$FULLY_JOINT==0], tours$finalweight[tours$TOURPURP>=5 & tours$TOURPURP<=6 & tours$FULLY_JOINT==0], na.rm = TRUE),
                    weighted.mean(tours$SKIMDIST[tours$TOURPURP>=7 & tours$TOURPURP<=9 & tours$FULLY_JOINT==0], tours$finalweight[tours$TOURPURP>=7 & tours$TOURPURP<=9 & tours$FULLY_JOINT==0], na.rm = TRUE),
                    weighted.mean(jtours$SKIMDIST[jtours$JOINT_PURP>=5 & jtours$JOINT_PURP<=6], jtours$finalweight[jtours$JOINT_PURP>=5 & jtours$JOINT_PURP<=6], na.rm = TRUE),
                    weighted.mean(jtours$SKIMDIST[jtours$JOINT_PURP>=7 & jtours$JOINT_PURP<=9], jtours$finalweight[jtours$JOINT_PURP>=7 & jtours$JOINT_PURP<=9], na.rm = TRUE),
                    weighted.mean(tours$SKIMDIST[tours$IS_SUBTOUR == 1], tours$finalweight[tours$IS_SUBTOUR == 1], na.rm = TRUE))
                    #weighted.mean(tours$SKIMDIST, tours$finalweight, na.rm = TRUE))

totAvgNonMand <- weighted.mean(c(tours$SKIMDIST[(tours$TOURPURP %in% c(4,5,6,7,8,9) & tours$FULLY_JOINT==0) | tours$IS_SUBTOUR == 1], 
                                 jtours$SKIMDIST[jtours$JOINT_PURP %in% c(5,6,7,8,9)]),
                               c(tours$finalweight[(tours$TOURPURP %in% c(4,5,6,7,8,9) & tours$FULLY_JOINT==0) | tours$IS_SUBTOUR == 1], 
                                 jtours$finalweight[jtours$JOINT_PURP %in% c(5,6,7,8,9)]), na.rm = T)

avgTripLengths <- c(avgTripLengths, totAvgNonMand)


nonMandTourPurpose <- c("esco", "imain", "idisc", "jmain", "jdisc", "atwork", "Total")

nonMandTripLengths <- data.frame(purpose = nonMandTourPurpose, avgTripLength = avgTripLengths)

write.csv(nonMandTripLengths, "nonMandTripLengths.csv", row.names = F)

# STop Frequency
#Outbound
stopfreq1 <- wtd.hist(tours$OUTBOUND_STOPS[tours$TOURPURP==1 & tours$FULLY_JOINT==0], breaks = c(seq(0,3, by=1), 9999), freq = NULL, right=FALSE, weight = tours$finalweight[tours$TOURPURP==1 & tours$FULLY_JOINT==0])
stopfreq2 <- wtd.hist(tours$OUTBOUND_STOPS[tours$TOURPURP==2 & tours$FULLY_JOINT==0], breaks = c(seq(0,3, by=1), 9999), freq = NULL, right=FALSE, weight = tours$finalweight[tours$TOURPURP==2 & tours$FULLY_JOINT==0])
#stopfreq3 <- wtd.hist(tours$OUTBOUND_STOPS[tours$TOURPURP==3 & tours$FULLY_JOINT==0], breaks = c(seq(0,3, by=1), 9999), freq = NULL, right=FALSE, weight = tours$finalweight[tours$TOURPURP==3 & tours$FULLY_JOINT==0])
stopfreq4 <- wtd.hist(tours$OUTBOUND_STOPS[tours$TOURPURP==4 & tours$FULLY_JOINT==0], breaks = c(seq(0,3, by=1), 9999), freq = NULL, right=FALSE, weight = tours$finalweight[tours$TOURPURP==4 & tours$FULLY_JOINT==0])
stopfreqi56 <- wtd.hist(tours$OUTBOUND_STOPS[tours$TOURPURP>=5 & tours$TOURPURP<=6 & tours$FULLY_JOINT==0], breaks = c(seq(0,3, by=1), 9999), freq = NULL, right=FALSE, weight = tours$finalweight[tours$TOURPURP>=5 & tours$TOURPURP<=6 & tours$FULLY_JOINT==0])
stopfreqi789 <- wtd.hist(tours$OUTBOUND_STOPS[tours$TOURPURP>=7 & tours$TOURPURP<=9 & tours$FULLY_JOINT==0], breaks = c(seq(0,3, by=1), 9999), freq = NULL, right=FALSE, weight = tours$finalweight[tours$TOURPURP>=7 & tours$TOURPURP<=9 & tours$FULLY_JOINT==0])
stopfreqj56 <- wtd.hist(jtours$OUTBOUND_STOPS[jtours$JOINT_PURP>=5 & jtours$JOINT_PURP<=6], breaks = c(seq(0,3, by=1), 9999), freq = NULL, right=FALSE, weight = jtours$finalweight[jtours$JOINT_PURP>=5 & jtours$JOINT_PURP<=6])
stopfreqj789 <- wtd.hist(jtours$OUTBOUND_STOPS[jtours$JOINT_PURP>=7 & jtours$JOINT_PURP<=9], breaks = c(seq(0,3, by=1), 9999), freq = NULL, right=FALSE, weight = jtours$finalweight[jtours$JOINT_PURP>=7 & jtours$JOINT_PURP<=9])
stopfreq10 <- wtd.hist(tours$OUTBOUND_STOPS[tours$IS_SUBTOUR == 1], breaks = c(seq(0,3, by=1), 9999), freq = NULL, right=FALSE, weight = tours$finalweight[tours$IS_SUBTOUR == 1])

stopFreq <- data.frame(stopfreq1$counts, stopfreq2$counts, stopfreq4$counts, stopfreqi56$counts
                          , stopfreqi789$counts, stopfreqj56$counts, stopfreqj789$counts, stopfreq10$counts)
colnames(stopFreq) <- c("work", "univ", "esco","imain", "idisc", "jmain", "jdisc", "atwork")
write.csv(stopFreq, "stopFreqOutProfile.csv")

# prepare stop frequency input for visualizer
stopFreqout_vis <- stopFreq
stopFreqout_vis$id <- row.names(stopFreqout_vis)
stopFreqout_vis <- melt(stopFreqout_vis, id = c("id"))
colnames(stopFreqout_vis) <- c("id", "purpose", "freq")

stopFreqout_vis <- xtabs(freq~purpose+id, stopFreqout_vis)
stopFreqout_vis <- addmargins(as.table(stopFreqout_vis))
stopFreqout_vis <- as.data.frame.matrix(stopFreqout_vis)
stopFreqout_vis$id <- row.names(stopFreqout_vis)
stopFreqout_vis <- melt(stopFreqout_vis, id = c("id"))
colnames(stopFreqout_vis) <- c("purpose", "nstops", "freq")
stopFreqout_vis$purpose <- as.character(stopFreqout_vis$purpose)
stopFreqout_vis$nstops <- as.character(stopFreqout_vis$nstops)
stopFreqout_vis <- stopFreqout_vis[stopFreqout_vis$nstops!="Sum",]
stopFreqout_vis$purpose[stopFreqout_vis$purpose=="Sum"] <- "Total"


#Inbound
stopfreq1 <- wtd.hist(tours$INBOUND_STOPS[tours$TOURPURP==1 & tours$FULLY_JOINT==0], breaks = c(seq(0,3, by=1), 9999), freq = NULL, right=FALSE, weight = tours$finalweight[tours$TOURPURP==1 & tours$FULLY_JOINT==0])
stopfreq2 <- wtd.hist(tours$INBOUND_STOPS[tours$TOURPURP==2 & tours$FULLY_JOINT==0], breaks = c(seq(0,3, by=1), 9999), freq = NULL, right=FALSE, weight = tours$finalweight[tours$TOURPURP==2 & tours$FULLY_JOINT==0])
#stopfreq3 <- wtd.hist(tours$INBOUND_STOPS[tours$TOURPURP==3 & tours$FULLY_JOINT==0], breaks = c(seq(0,3, by=1), 9999), freq = NULL, right=FALSE, weight = tours$finalweight[tours$TOURPURP==3 & tours$FULLY_JOINT==0])
stopfreq4 <- wtd.hist(tours$INBOUND_STOPS[tours$TOURPURP==4 & tours$FULLY_JOINT==0], breaks = c(seq(0,3, by=1), 9999), freq = NULL, right=FALSE, weight = tours$finalweight[tours$TOURPURP==4 & tours$FULLY_JOINT==0])
stopfreqi56 <- wtd.hist(tours$INBOUND_STOPS[tours$TOURPURP>=5 & tours$TOURPURP<=6 & tours$FULLY_JOINT==0], breaks = c(seq(0,3, by=1), 9999), freq = NULL, right=FALSE, weight = tours$finalweight[tours$TOURPURP>=5 & tours$TOURPURP<=6 & tours$FULLY_JOINT==0])
stopfreqi789 <- wtd.hist(tours$INBOUND_STOPS[tours$TOURPURP>=7 & tours$TOURPURP<=9 & tours$FULLY_JOINT==0], breaks = c(seq(0,3, by=1), 9999), freq = NULL, right=FALSE, weight = tours$finalweight[tours$TOURPURP>=7 & tours$TOURPURP<=9 & tours$FULLY_JOINT==0])
stopfreqj56 <- wtd.hist(jtours$INBOUND_STOPS[jtours$JOINT_PURP>=5 & jtours$JOINT_PURP<=6], breaks = c(seq(0,3, by=1), 9999), freq = NULL, right=FALSE, weight = jtours$finalweight[jtours$JOINT_PURP>=5 & jtours$JOINT_PURP<=6])
stopfreqj789 <- wtd.hist(jtours$INBOUND_STOPS[jtours$JOINT_PURP>=7 & jtours$JOINT_PURP<=9], breaks = c(seq(0,3, by=1), 9999), freq = NULL, right=FALSE, weight = jtours$finalweight[jtours$JOINT_PURP>=7 & jtours$JOINT_PURP<=9])
stopfreq10 <- wtd.hist(tours$INBOUND_STOPS[tours$IS_SUBTOUR == 1], breaks = c(seq(0,3, by=1), 9999), freq = NULL, right=FALSE, weight = tours$finalweight[tours$IS_SUBTOUR == 1])

stopFreq <- data.frame(stopfreq1$counts, stopfreq2$counts, stopfreq4$counts, stopfreqi56$counts
                       , stopfreqi789$counts, stopfreqj56$counts, stopfreqj789$counts, stopfreq10$counts)
colnames(stopFreq) <- c("work", "univ", "esco","imain", "idisc", "jmain", "jdisc", "atwork")
write.csv(stopFreq, "stopFreqInbProfile.csv")

# prepare stop frequency input for visualizer
stopFreqinb_vis <- stopFreq
stopFreqinb_vis$id <- row.names(stopFreqinb_vis)
stopFreqinb_vis <- melt(stopFreqinb_vis, id = c("id"))
colnames(stopFreqinb_vis) <- c("id", "purpose", "freq")

stopFreqinb_vis <- xtabs(freq~purpose+id, stopFreqinb_vis)
stopFreqinb_vis <- addmargins(as.table(stopFreqinb_vis))
stopFreqinb_vis <- as.data.frame.matrix(stopFreqinb_vis)
stopFreqinb_vis$id <- row.names(stopFreqinb_vis)
stopFreqinb_vis <- melt(stopFreqinb_vis, id = c("id"))
colnames(stopFreqinb_vis) <- c("purpose", "nstops", "freq")
stopFreqinb_vis$purpose <- as.character(stopFreqinb_vis$purpose)
stopFreqinb_vis$nstops <- as.character(stopFreqinb_vis$nstops)
stopFreqinb_vis <- stopFreqinb_vis[stopFreqinb_vis$nstops!="Sum",]
stopFreqinb_vis$purpose[stopFreqinb_vis$purpose=="Sum"] <- "Total"


stopfreqDir_vis <- data.frame(stopFreqout_vis, stopFreqinb_vis$freq)
colnames(stopfreqDir_vis) <- c("purpose", "nstops", "freq_out", "freq_inb")
write.csv(stopfreqDir_vis, "stopfreqDir_vis.csv", row.names = F)


#Total
stopfreq1 <- wtd.hist(tours$TOTAL_STOPS[tours$TOURPURP==1 & tours$FULLY_JOINT==0], breaks = c(seq(0,6, by=1), 9999), freq = NULL, right=FALSE, weight = tours$finalweight[tours$TOURPURP==1 & tours$FULLY_JOINT==0])
stopfreq2 <- wtd.hist(tours$TOTAL_STOPS[tours$TOURPURP==2 & tours$FULLY_JOINT==0], breaks = c(seq(0,6, by=1), 9999), freq = NULL, right=FALSE, weight = tours$finalweight[tours$TOURPURP==2 & tours$FULLY_JOINT==0])
#stopfreq3 <- wtd.hist(tours$TOTAL_STOPS[tours$TOURPURP==3 & tours$FULLY_JOINT==0], breaks = c(seq(0,6, by=1), 9999), freq = NULL, right=FALSE, weight = tours$finalweight[tours$TOURPURP==3 & tours$FULLY_JOINT==0])
stopfreq4 <- wtd.hist(tours$TOTAL_STOPS[tours$TOURPURP==4 & tours$FULLY_JOINT==0], breaks = c(seq(0,6, by=1), 9999), freq = NULL, right=FALSE, weight = tours$finalweight[tours$TOURPURP==4 & tours$FULLY_JOINT==0])
stopfreqi56 <- wtd.hist(tours$TOTAL_STOPS[tours$TOURPURP>=5 & tours$TOURPURP<=6 & tours$FULLY_JOINT==0], breaks = c(seq(0,6, by=1), 9999), freq = NULL, right=FALSE, weight = tours$finalweight[tours$TOURPURP>=5 & tours$TOURPURP<=6 & tours$FULLY_JOINT==0])
stopfreqi789 <- wtd.hist(tours$TOTAL_STOPS[tours$TOURPURP>=7 & tours$TOURPURP<=9 & tours$FULLY_JOINT==0], breaks = c(seq(0,6, by=1), 9999), freq = NULL, right=FALSE, weight = tours$finalweight[tours$TOURPURP>=7 & tours$TOURPURP<=9 & tours$FULLY_JOINT==0])
stopfreqj56 <- wtd.hist(jtours$TOTAL_STOPS[jtours$JOINT_PURP>=5 & jtours$JOINT_PURP<=6], breaks = c(seq(0,6, by=1), 9999), freq = NULL, right=FALSE, weight = jtours$finalweight[jtours$JOINT_PURP>=5 & jtours$JOINT_PURP<=6])
stopfreqj789 <- wtd.hist(jtours$TOTAL_STOPS[jtours$JOINT_PURP>=7 & jtours$JOINT_PURP<=9], breaks = c(seq(0,6, by=1), 9999), freq = NULL, right=FALSE, weight = jtours$finalweight[jtours$JOINT_PURP>=7 & jtours$JOINT_PURP<=9])
stopfreq10 <- wtd.hist(tours$TOTAL_STOPS[tours$IS_SUBTOUR == 1 & tours$FULLY_JOINT==0], breaks = c(seq(0,6, by=1), 9999), freq = NULL, right=FALSE, weight = tours$finalweight[tours$IS_SUBTOUR == 1 & tours$FULLY_JOINT==0])

stopFreq <- data.frame(stopfreq1$counts, stopfreq2$counts, stopfreq4$counts, stopfreqi56$counts
                       , stopfreqi789$counts, stopfreqj56$counts, stopfreqj789$counts, stopfreq10$counts)
colnames(stopFreq) <- c("work", "univ", "esco","imain", "idisc", "jmain", "jdisc", "atwork")
write.csv(stopFreq, "stopFreqTotProfile.csv")

# prepare stop frequency input for visualizer
stopFreq_vis <- stopFreq
stopFreq_vis$id <- row.names(stopFreq_vis)
stopFreq_vis <- melt(stopFreq_vis, id = c("id"))
colnames(stopFreq_vis) <- c("id", "purpose", "freq")

stopFreq_vis <- xtabs(freq~purpose+id, stopFreq_vis)
stopFreq_vis <- addmargins(as.table(stopFreq_vis))
stopFreq_vis <- as.data.frame.matrix(stopFreq_vis)
stopFreq_vis$id <- row.names(stopFreq_vis)
stopFreq_vis <- melt(stopFreq_vis, id = c("id"))
colnames(stopFreq_vis) <- c("purpose", "nstops", "freq")
stopFreq_vis$purpose <- as.character(stopFreq_vis$purpose)
stopFreq_vis$nstops <- as.character(stopFreq_vis$nstops)
stopFreq_vis <- stopFreq_vis[stopFreq_vis$nstops!="Sum",]
stopFreq_vis$purpose[stopFreq_vis$purpose=="Sum"] <- "Total"

write.csv(stopFreq_vis, "stopfreq_total_vis.csv", row.names = F)

#Stop purpose X TourPurpose
stopfreq1 <- wtd.hist(stops$DEST_PURP[stops$TOURPURP==1 & stops$FULLY_JOINT==0], breaks = c(seq(1,10, by=1), 9999), freq = NULL, right=FALSE, weight = stops$finalweight[stops$TOURPURP==1 & stops$FULLY_JOINT==0])
stopfreq2 <- wtd.hist(stops$DEST_PURP[stops$TOURPURP==2 & stops$FULLY_JOINT==0], breaks = c(seq(1,10, by=1), 9999), freq = NULL, right=FALSE, weight = stops$finalweight[stops$TOURPURP==2 & stops$FULLY_JOINT==0])
#stopfreq3 <- wtd.hist(stops$DEST_PURP[stops$TOURPURP==3 & stops$FULLY_JOINT==0], breaks = c(seq(1,10, by=1), 9999), freq = NULL, right=FALSE, weight = stops$finalweight[stops$TOURPURP==3 & stops$FULLY_JOINT==0])
stopfreq4 <- wtd.hist(stops$DEST_PURP[stops$TOURPURP==4 & stops$FULLY_JOINT==0], breaks = c(seq(1,10, by=1), 9999), freq = NULL, right=FALSE, weight = stops$finalweight[stops$TOURPURP==4 & stops$FULLY_JOINT==0])
stopfreqi56 <- wtd.hist(stops$DEST_PURP[stops$TOURPURP>=5 & stops$TOURPURP<=6 & stops$FULLY_JOINT==0], breaks = c(seq(1,10, by=1), 9999), freq = NULL, right=FALSE, weight = stops$finalweight[stops$TOURPURP>=5 & stops$TOURPURP<=6 & stops$FULLY_JOINT==0])
stopfreqi789 <- wtd.hist(stops$DEST_PURP[stops$TOURPURP>=7 & stops$TOURPURP<=9 & stops$FULLY_JOINT==0], breaks = c(seq(1,10, by=1), 9999), freq = NULL, right=FALSE, weight = stops$finalweight[stops$TOURPURP>=7 & stops$TOURPURP<=9 & stops$FULLY_JOINT==0])
stopfreqj56 <- wtd.hist(jstops$DEST_PURP[jstops$TOURPURP>=5 & jstops$TOURPURP<=6], breaks = c(seq(1,10, by=1), 9999), freq = NULL, right=FALSE, weight = jstops$finalweight[jstops$TOURPURP>=5 & jstops$TOURPURP<=6])
stopfreqj789 <- wtd.hist(jstops$DEST_PURP[jstops$TOURPURP>=7 & jstops$TOURPURP<=9], breaks = c(seq(1,10, by=1), 9999), freq = NULL, right=FALSE, weight = jstops$finalweight[jstops$TOURPURP>=7 & jstops$TOURPURP<=9])
stopfreq10 <- wtd.hist(stops$DEST_PURP[stops$SUBTOUR==1], breaks = c(seq(1,10, by=1), 9999), freq = NULL, right=FALSE, weight = stops$finalweight[stops$SUBTOUR==1])

stopFreq <- data.frame(stopfreq1$counts, stopfreq2$counts, stopfreq4$counts, stopfreqi56$counts
                       , stopfreqi789$counts, stopfreqj56$counts, stopfreqj789$counts, stopfreq10$counts)
colnames(stopFreq) <- c("work", "univ", "esco","imain", "idisc", "jmain", "jdisc", "atwork")
write.csv(stopFreq, "stopPurposeByTourPurpose.csv")

# prepare stop frequency input for visualizer
stopFreq_vis <- stopFreq
stopFreq_vis$id <- row.names(stopFreq_vis)
stopFreq_vis <- melt(stopFreq_vis, id = c("id"))
colnames(stopFreq_vis) <- c("stop_purp", "purpose", "freq")

stopFreq_vis <- xtabs(freq~purpose+stop_purp, stopFreq_vis)
stopFreq_vis <- addmargins(as.table(stopFreq_vis))
stopFreq_vis <- as.data.frame.matrix(stopFreq_vis)
stopFreq_vis$purpose <- row.names(stopFreq_vis)
stopFreq_vis <- melt(stopFreq_vis, id = c("purpose"))
colnames(stopFreq_vis) <- c("purpose", "stop_purp", "freq")
stopFreq_vis$purpose <- as.character(stopFreq_vis$purpose)
stopFreq_vis$stop_purp <- as.character(stopFreq_vis$stop_purp)
stopFreq_vis <- stopFreq_vis[stopFreq_vis$stop_purp!="Sum",]
stopFreq_vis$purpose[stopFreq_vis$purpose=="Sum"] <- "Total"

write.csv(stopFreq_vis, "stoppurpose_tourpurpose_vis.csv", row.names = F)

#Out of direction - Stop Location
stopfreq1 <- wtd.hist(stops$out_dir_dist[stops$TOURPURP==1 & stops$FULLY_JOINT==0], breaks = c(-9999,seq(0,40, by=1), 9999), freq = NULL, right=FALSE, weight = stops$finalweight[stops$TOURPURP==1 & stops$FULLY_JOINT==0])
stopfreq2 <- wtd.hist(stops$out_dir_dist[stops$TOURPURP==2 & stops$FULLY_JOINT==0], breaks = c(-9999,seq(0,40, by=1), 9999), freq = NULL, right=FALSE, weight = stops$finalweight[stops$TOURPURP==2 & stops$FULLY_JOINT==0])
#stopfreq3 <- wtd.hist(stops$out_dir_dist[stops$TOURPURP==3 & stops$FULLY_JOINT==0], breaks = c(-9999,seq(0,40, by=1), 9999), freq = NULL, right=FALSE, weight = stops$finalweight[stops$TOURPURP==3 & stops$FULLY_JOINT==0])
stopfreq4 <- wtd.hist(stops$out_dir_dist[stops$TOURPURP==4 & stops$FULLY_JOINT==0], breaks = c(-9999,seq(0,40, by=1), 9999), freq = NULL, right=FALSE, weight = stops$finalweight[stops$TOURPURP==4 & stops$FULLY_JOINT==0])
stopfreqi56 <- wtd.hist(stops$out_dir_dist[stops$TOURPURP>=5 & stops$TOURPURP<=6 & stops$FULLY_JOINT==0], breaks = c(-9999,seq(0,40, by=1), 9999), freq = NULL, right=FALSE, weight = stops$finalweight[stops$TOURPURP>=5 & stops$TOURPURP<=6 & stops$FULLY_JOINT==0])
stopfreqi789 <- wtd.hist(stops$out_dir_dist[stops$TOURPURP>=7 & stops$TOURPURP<=9 & stops$FULLY_JOINT==0], breaks = c(-9999,seq(0,40, by=1), 9999), freq = NULL, right=FALSE, weight = stops$finalweight[stops$TOURPURP>=7 & stops$TOURPURP<=9 & stops$FULLY_JOINT==0])
stopfreqj56 <- wtd.hist(jstops$out_dir_dist[jstops$TOURPURP>=5 & jstops$TOURPURP<=6], breaks = c(-9999,seq(0,40, by=1), 9999), freq = NULL, right=FALSE, weight = jstops$finalweight[jstops$TOURPURP>=5 & jstops$TOURPURP<=6])
stopfreqj789 <- wtd.hist(jstops$out_dir_dist[jstops$TOURPURP>=7 & jstops$TOURPURP<=9], breaks = c(-9999,seq(0,40, by=1), 9999), freq = NULL, right=FALSE, weight = jstops$finalweight[jstops$TOURPURP>=7 & jstops$TOURPURP<=9])
stopfreq10 <- wtd.hist(stops$out_dir_dist[stops$SUBTOUR==1], breaks = c(-9999,seq(0,40, by=1), 9999), freq = NULL, right=FALSE, weight = stops$finalweight[stops$SUBTOUR==1])

stopFreq <- data.frame(stopfreq1$counts, stopfreq2$counts, stopfreq4$counts, stopfreqi56$counts
                       , stopfreqi789$counts, stopfreqj56$counts, stopfreqj789$counts, stopfreq10$counts)
colnames(stopFreq) <- c("work", "univ", "esco","imain", "idisc", "jmain", "jdisc", "atwork")
write.csv(stopFreq, "stopOutOfDirectionDC.csv")

# prepare stop location input for visualizer
stopDC_vis <- stopFreq
stopDC_vis$id <- row.names(stopDC_vis)
stopDC_vis <- melt(stopDC_vis, id = c("id"))
colnames(stopDC_vis) <- c("id", "purpose", "freq")

stopDC_vis <- xtabs(freq~id+purpose, stopDC_vis)
stopDC_vis <- addmargins(as.table(stopDC_vis))
stopDC_vis <- as.data.frame.matrix(stopDC_vis)
stopDC_vis$id <- row.names(stopDC_vis)
stopDC_vis <- melt(stopDC_vis, id = c("id"))
colnames(stopDC_vis) <- c("distbin", "PURPOSE", "freq")
stopDC_vis$PURPOSE <- as.character(stopDC_vis$PURPOSE)
stopDC_vis$distbin <- as.character(stopDC_vis$distbin)
stopDC_vis <- stopDC_vis[stopDC_vis$distbin!="Sum",]
stopDC_vis$PURPOSE[stopDC_vis$PURPOSE=="Sum"] <- "Total"
stopDC_vis$distbin <- as.numeric(stopDC_vis$distbin)

write.csv(stopDC_vis, "stopDC_vis.csv", row.names = F)

# compute average out of dir distance for visualizer
avgDistances <- c(weighted.mean(stops$out_dir_dist[stops$TOURPURP==1 & stops$FULLY_JOINT==0], weight = stops$finalweight[stops$TOURPURP==1 & stops$FULLY_JOINT==0], na.rm = TRUE),
                  weighted.mean(stops$out_dir_dist[stops$TOURPURP==2 & stops$FULLY_JOINT==0], weight = stops$finalweight[stops$TOURPURP==2 & stops$FULLY_JOINT==0], na.rm = TRUE),
                  0,
                  weighted.mean(stops$out_dir_dist[stops$TOURPURP==4 & stops$FULLY_JOINT==0], weight = stops$finalweight[stops$TOURPURP==4 & stops$FULLY_JOINT==0], na.rm = TRUE),
                  weighted.mean(stops$out_dir_dist[stops$TOURPURP>=5 & stops$TOURPURP<=6 & stops$FULLY_JOINT==0], weight = stops$finalweight[stops$TOURPURP>=5 & stops$TOURPURP<=6 & stops$FULLY_JOINT==0], na.rm = TRUE),
                  weighted.mean(stops$out_dir_dist[stops$TOURPURP>=7 & stops$TOURPURP<=9 & stops$FULLY_JOINT==0], weight = stops$finalweight[stops$TOURPURP>=7 & stops$TOURPURP<=9 & stops$FULLY_JOINT==0], na.rm = TRUE),
                  weighted.mean(jstops$out_dir_dist[jstops$TOURPURP>=5 & jstops$TOURPURP<=6], weight = jstops$finalweight[jstops$TOURPURP>=5 & jstops$TOURPURP<=6], na.rm = TRUE),
                  weighted.mean(jstops$out_dir_dist[jstops$TOURPURP>=7 & jstops$TOURPURP<=9], weight = jstops$finalweight[jstops$TOURPURP>=7 & jstops$TOURPURP<=9], na.rm = TRUE),
                  weighted.mean(stops$out_dir_dist[stops$SUBTOUR==1], weight = stops$finalweight[stops$SUBTOUR==1], na.rm = TRUE),
                  weighted.mean(stops$out_dir_dist, weight = stops$finalweight, na.rm = TRUE))

purp <- c("work", "univ", "sch", "esco","imain", "idisc", "jmain", "jdisc", "atwork", "total")

avgStopOutofDirectionDist <- data.frame(purpose = purp, avgDist = avgDistances)

write.csv(avgStopOutofDirectionDist, "avgStopOutofDirectionDist_vis.csv", row.names = F)


#Stop Departure Time
stopfreq1 <- wtd.hist(stops$DEST_DEP_BIN[stops$TOURPURP==1 & stops$FULLY_JOINT==0], breaks = c(seq(1,40, by=1), 9999), freq = NULL, right=FALSE, weight = stops$finalweight[stops$TOURPURP==1 & stops$FULLY_JOINT==0])
stopfreq2 <- wtd.hist(stops$DEST_DEP_BIN[stops$TOURPURP==2 & stops$FULLY_JOINT==0], breaks = c(seq(1,40, by=1), 9999), freq = NULL, right=FALSE, weight = stops$finalweight[stops$TOURPURP==2 & stops$FULLY_JOINT==0])
#stopfreq3 <- wtd.hist(stops$DEST_DEP_BIN[stops$TOURPURP==3 & stops$FULLY_JOINT==0], breaks = c(seq(1,40, by=1), 9999), freq = NULL, right=FALSE, weight = stops$finalweight[stops$TOURPURP==3 & stops$FULLY_JOINT==0])
stopfreq4 <- wtd.hist(stops$DEST_DEP_BIN[stops$TOURPURP==4 & stops$FULLY_JOINT==0], breaks = c(seq(1,40, by=1), 9999), freq = NULL, right=FALSE, weight = stops$finalweight[stops$TOURPURP==4 & stops$FULLY_JOINT==0])
stopfreqi56 <- wtd.hist(stops$DEST_DEP_BIN[stops$TOURPURP>=5 & stops$TOURPURP<=6 & stops$FULLY_JOINT==0], breaks = c(seq(1,40, by=1), 9999), freq = NULL, right=FALSE, weight = stops$finalweight[stops$TOURPURP>=5 & stops$TOURPURP<=6 & stops$FULLY_JOINT==0])
stopfreqi789 <- wtd.hist(stops$DEST_DEP_BIN[stops$TOURPURP>=7 & stops$TOURPURP<=9 & stops$FULLY_JOINT==0], breaks = c(seq(1,40, by=1), 9999), freq = NULL, right=FALSE, weight = stops$finalweight[stops$TOURPURP>=7 & stops$TOURPURP<=9 & stops$FULLY_JOINT==0])
stopfreqj56 <- wtd.hist(jstops$DEST_DEP_BIN[jstops$TOURPURP>=5 & jstops$TOURPURP<=6], breaks = c(seq(1,40, by=1), 9999), freq = NULL, right=FALSE, weight = jstops$finalweight[jstops$TOURPURP>=5 & jstops$TOURPURP<=6])
stopfreqj789 <- wtd.hist(jstops$DEST_DEP_BIN[jstops$TOURPURP>=7 & jstops$TOURPURP<=9], breaks = c(seq(1,40, by=1), 9999), freq = NULL, right=FALSE, weight = jstops$finalweight[jstops$TOURPURP>=7 & jstops$TOURPURP<=9])
stopfreq10 <- wtd.hist(stops$DEST_DEP_BIN[stops$SUBTOUR==1], breaks = c(seq(1,40, by=1), 9999), freq = NULL, right=FALSE, weight = stops$finalweight[stops$SUBTOUR==1])

stopFreq <- data.frame(stopfreq1$counts, stopfreq2$counts, stopfreq4$counts, stopfreqi56$counts
                       , stopfreqi789$counts, stopfreqj56$counts, stopfreqj789$counts, stopfreq10$counts)
colnames(stopFreq) <- c("work", "univ", "esco","imain", "idisc", "jmain", "jdisc", "atwork")
write.csv(stopFreq, "stopDeparture.csv")

# prepare stop departure input for visualizer
stopDep_vis <- stopFreq
stopDep_vis$id <- row.names(stopDep_vis)
stopDep_vis <- melt(stopDep_vis, id = c("id"))
colnames(stopDep_vis) <- c("id", "purpose", "freq_stop")

stopDep_vis$purpose <- as.character(stopDep_vis$purpose)
stopDep_vis <- xtabs(freq_stop~id+purpose, stopDep_vis)
stopDep_vis <- addmargins(as.table(stopDep_vis))
stopDep_vis <- as.data.frame.matrix(stopDep_vis)
stopDep_vis$id <- row.names(stopDep_vis)
stopDep_vis <- melt(stopDep_vis, id = c("id"))
colnames(stopDep_vis) <- c("timebin", "PURPOSE", "freq")
stopDep_vis$PURPOSE <- as.character(stopDep_vis$PURPOSE)
stopDep_vis$timebin <- as.character(stopDep_vis$timebin)
stopDep_vis <- stopDep_vis[stopDep_vis$timebin!="Sum",]
stopDep_vis$PURPOSE[stopDep_vis$PURPOSE=="Sum"] <- "Total"
stopDep_vis$timebin <- as.numeric(stopDep_vis$timebin)

#Trip Departure Time
stopfreq1 <- wtd.hist(trips$ORIG_DEP_BIN[trips$TOURPURP==1 & trips$FULLY_JOINT==0], breaks = c(seq(1,40, by=1), 9999), freq = NULL, right=FALSE, weight = trips$finalweight[trips$TOURPURP==1 & trips$FULLY_JOINT==0])
stopfreq2 <- wtd.hist(trips$ORIG_DEP_BIN[trips$TOURPURP==2 & trips$FULLY_JOINT==0], breaks = c(seq(1,40, by=1), 9999), freq = NULL, right=FALSE, weight = trips$finalweight[trips$TOURPURP==2 & trips$FULLY_JOINT==0])
#stopfreq3 <- wtd.hist(trips$ORIG_DEP_BIN[trips$TOURPURP==3 & trips$FULLY_JOINT==0], breaks = c(seq(1,40, by=1), 9999), freq = NULL, right=FALSE, weight = trips$finalweight[trips$TOURPURP==3 & trips$FULLY_JOINT==0])
stopfreq4 <- wtd.hist(trips$ORIG_DEP_BIN[trips$TOURPURP==4 & trips$FULLY_JOINT==0], breaks = c(seq(1,40, by=1), 9999), freq = NULL, right=FALSE, weight = trips$finalweight[trips$TOURPURP==4 & trips$FULLY_JOINT==0])
stopfreqi56 <- wtd.hist(trips$ORIG_DEP_BIN[trips$TOURPURP>=5 & trips$TOURPURP<=6 & trips$FULLY_JOINT==0], breaks = c(seq(1,40, by=1), 9999), freq = NULL, right=FALSE, weight = trips$finalweight[trips$TOURPURP>=5 & trips$TOURPURP<=6 & trips$FULLY_JOINT==0])
stopfreqi789 <- wtd.hist(trips$ORIG_DEP_BIN[trips$TOURPURP>=7 & trips$TOURPURP<=9 & trips$FULLY_JOINT==0], breaks = c(seq(1,40, by=1), 9999), freq = NULL, right=FALSE, weight = trips$finalweight[trips$TOURPURP>=7 & trips$TOURPURP<=9 & trips$FULLY_JOINT==0])
stopfreqj56 <- wtd.hist(jtrips$ORIG_DEP_BIN[jtrips$TOURPURP>=5 & jtrips$TOURPURP<=6], breaks = c(seq(1,40, by=1), 9999), freq = NULL, right=FALSE, weight = jtrips$finalweight[jtrips$TOURPURP>=5 & jtrips$TOURPURP<=6])
stopfreqj789 <- wtd.hist(jtrips$ORIG_DEP_BIN[jtrips$TOURPURP>=7 & jtrips$TOURPURP<=9], breaks = c(seq(1,40, by=1), 9999), freq = NULL, right=FALSE, weight = jtrips$finalweight[jtrips$TOURPURP>=7 & jtrips$TOURPURP<=9])
stopfreq10 <- wtd.hist(trips$ORIG_DEP_BIN[trips$SUBTOUR==1], breaks = c(seq(1,40, by=1), 9999), freq = NULL, right=FALSE, weight = trips$finalweight[trips$SUBTOUR==1])

stopFreq <- data.frame(stopfreq1$counts, stopfreq2$counts, stopfreq4$counts, stopfreqi56$counts
                       , stopfreqi789$counts, stopfreqj56$counts, stopfreqj789$counts, stopfreq10$counts)
colnames(stopFreq) <- c("work", "univ", "esco","imain", "idisc", "jmain", "jdisc", "atwork")
write.csv(stopFreq, "tripDeparture.csv")

# prepare stop departure input for visualizer
tripDep_vis <- stopFreq
tripDep_vis$id <- row.names(tripDep_vis)
tripDep_vis <- melt(tripDep_vis, id = c("id"))
colnames(tripDep_vis) <- c("id", "purpose", "freq_trip")

tripDep_vis$purpose <- as.character(tripDep_vis$purpose)
tripDep_vis <- xtabs(freq_trip~id+purpose, tripDep_vis)
tripDep_vis <- addmargins(as.table(tripDep_vis))
tripDep_vis <- as.data.frame.matrix(tripDep_vis)
tripDep_vis$id <- row.names(tripDep_vis)
tripDep_vis <- melt(tripDep_vis, id = c("id"))
colnames(tripDep_vis) <- c("timebin", "PURPOSE", "freq")
tripDep_vis$PURPOSE <- as.character(tripDep_vis$PURPOSE)
tripDep_vis$timebin <- as.character(tripDep_vis$timebin)
tripDep_vis <- tripDep_vis[tripDep_vis$timebin!="Sum",]
tripDep_vis$PURPOSE[tripDep_vis$PURPOSE=="Sum"] <- "Total"
tripDep_vis$timebin <- as.numeric(tripDep_vis$timebin)

stopTripDep_vis <- data.frame(stopDep_vis, tripDep_vis$freq)
colnames(stopTripDep_vis) <- c("timebin", "purpose", "freq_stop", "freq_trip")
write.csv(stopTripDep_vis, "stopTripDep_vis.csv", row.names = F)

#Trip Mode Summary

#avg occupancy for SR3+ mode - for Joel (02/26/2019 - shared AV implementation meeting at SANDAG)
trips_sr3 <- trips[trips$TRIPMODE==3,]
trips_sr3_indiv <- trips_sr3[trips_sr3$FULLY_JOINT==0,]
trips_sr3_indiv$trips_occ <- trips_sr3_indiv$finalweight * trips_sr3_indiv$AUTO_OCC
av_occ_sr3_indiv <- sum(trips_sr3$trips_occ)/sum(trips_sr3$finalweight)

#put TAXI mode into SR2 for both tourmode and tripmode
trips$TRIPMODE <- ifelse(trips$TRIPMODE==19, 3, trips$TRIPMODE)
trips$TOURMODE <- ifelse(trips$TOURMODE==10, 2, trips$TOURMODE)

#Work
tripmode1 <- wtd.hist(trips$TRIPMODE[!is.na(trips$TRIPMODE) & trips$TRIPMODE>0 & trips$TOURPURP==1 & trips$FULLY_JOINT==0 & trips$TOURMODE==1], breaks = seq(1,26, by=1), freq = NULL, right=FALSE, weight = trips$finalweight[!is.na(trips$TRIPMODE) & trips$TRIPMODE>0  & trips$TOURPURP==1 & trips$FULLY_JOINT==0 & trips$TOURMODE==1])
tripmode2 <- wtd.hist(trips$TRIPMODE[!is.na(trips$TRIPMODE) & trips$TRIPMODE>0 & trips$TOURPURP==1 & trips$FULLY_JOINT==0 & trips$TOURMODE==2], breaks = seq(1,26, by=1), freq = NULL, right=FALSE, weight = trips$finalweight[!is.na(trips$TRIPMODE) & trips$TRIPMODE>0  & trips$TOURPURP==1 & trips$FULLY_JOINT==0 & trips$TOURMODE==2])
tripmode3 <- wtd.hist(trips$TRIPMODE[!is.na(trips$TRIPMODE) & trips$TRIPMODE>0 & trips$TOURPURP==1 & trips$FULLY_JOINT==0 & trips$TOURMODE==3], breaks = seq(1,26, by=1), freq = NULL, right=FALSE, weight = trips$finalweight[!is.na(trips$TRIPMODE) & trips$TRIPMODE>0  & trips$TOURPURP==1 & trips$FULLY_JOINT==0 & trips$TOURMODE==3])
tripmode4 <- wtd.hist(trips$TRIPMODE[!is.na(trips$TRIPMODE) & trips$TRIPMODE>0 & trips$TOURPURP==1 & trips$FULLY_JOINT==0 & trips$TOURMODE==4], breaks = seq(1,26, by=1), freq = NULL, right=FALSE, weight = trips$finalweight[!is.na(trips$TRIPMODE) & trips$TRIPMODE>0  & trips$TOURPURP==1 & trips$FULLY_JOINT==0 & trips$TOURMODE==4])
tripmode5 <- wtd.hist(trips$TRIPMODE[!is.na(trips$TRIPMODE) & trips$TRIPMODE>0 & trips$TOURPURP==1 & trips$FULLY_JOINT==0 & trips$TOURMODE==5], breaks = seq(1,26, by=1), freq = NULL, right=FALSE, weight = trips$finalweight[!is.na(trips$TRIPMODE) & trips$TRIPMODE>0  & trips$TOURPURP==1 & trips$FULLY_JOINT==0 & trips$TOURMODE==5])
tripmode6 <- wtd.hist(trips$TRIPMODE[!is.na(trips$TRIPMODE) & trips$TRIPMODE>0 & trips$TOURPURP==1 & trips$FULLY_JOINT==0 & trips$TOURMODE==6], breaks = seq(1,26, by=1), freq = NULL, right=FALSE, weight = trips$finalweight[!is.na(trips$TRIPMODE) & trips$TRIPMODE>0  & trips$TOURPURP==1 & trips$FULLY_JOINT==0 & trips$TOURMODE==6])
tripmode7 <- wtd.hist(trips$TRIPMODE[!is.na(trips$TRIPMODE) & trips$TRIPMODE>0 & trips$TOURPURP==1 & trips$FULLY_JOINT==0 & trips$TOURMODE==7], breaks = seq(1,26, by=1), freq = NULL, right=FALSE, weight = trips$finalweight[!is.na(trips$TRIPMODE) & trips$TRIPMODE>0  & trips$TOURPURP==1 & trips$FULLY_JOINT==0 & trips$TOURMODE==7])
tripmode8 <- wtd.hist(trips$TRIPMODE[!is.na(trips$TRIPMODE) & trips$TRIPMODE>0 & trips$TOURPURP==1 & trips$FULLY_JOINT==0 & trips$TOURMODE==8], breaks = seq(1,26, by=1), freq = NULL, right=FALSE, weight = trips$finalweight[!is.na(trips$TRIPMODE) & trips$TRIPMODE>0  & trips$TOURPURP==1 & trips$FULLY_JOINT==0 & trips$TOURMODE==8])
tripmode9 <- wtd.hist(trips$TRIPMODE[!is.na(trips$TRIPMODE) & trips$TRIPMODE>0 & trips$TOURPURP==1 & trips$FULLY_JOINT==0 & trips$TOURMODE==9], breaks = seq(1,26, by=1), freq = NULL, right=FALSE, weight = trips$finalweight[!is.na(trips$TRIPMODE) & trips$TRIPMODE>0  & trips$TOURPURP==1 & trips$FULLY_JOINT==0 & trips$TOURMODE==9])

tripModeProfile <- data.frame(tripmode1$counts, tripmode2$counts, tripmode3$counts, tripmode4$counts,
                              tripmode5$counts, tripmode6$counts, tripmode7$counts, tripmode8$counts, tripmode9$counts)
colnames(tripModeProfile) <- c("tourmode1", "tourmode2", "tourmode3", "tourmode4", "tourmode5", "tourmode6", "tourmode7", "tourmode8", "tourmode9")
write.csv(tripModeProfile, "tripModeProfile_Work.csv")

# Prepeare data for visualizer
tripModeProfile1_vis <- tripModeProfile[1:9,]
tripModeProfile1_vis$id <- row.names(tripModeProfile1_vis)
tripModeProfile1_vis <- melt(tripModeProfile1_vis, id = c("id"))
colnames(tripModeProfile1_vis) <- c("id", "purpose", "freq1")

tripModeProfile1_vis <- xtabs(freq1~id+purpose, tripModeProfile1_vis)
tripModeProfile1_vis[is.na(tripModeProfile1_vis)] <- 0
tripModeProfile1_vis <- addmargins(as.table(tripModeProfile1_vis))
tripModeProfile1_vis <- as.data.frame.matrix(tripModeProfile1_vis)

tripModeProfile1_vis$id <- row.names(tripModeProfile1_vis)
tripModeProfile1_vis <- melt(tripModeProfile1_vis, id = c("id"))
colnames(tripModeProfile1_vis) <- c("id", "purpose", "freq1")
tripModeProfile1_vis$id <- as.character(tripModeProfile1_vis$id)
tripModeProfile1_vis$purpose <- as.character(tripModeProfile1_vis$purpose)
tripModeProfile1_vis <- tripModeProfile1_vis[tripModeProfile1_vis$id!="Sum",]
tripModeProfile1_vis$purpose[tripModeProfile1_vis$purpose=="Sum"] <- "Total"
                                                              
#Univ                                                         
tripmode1 <- wtd.hist(trips$TRIPMODE[!is.na(trips$TRIPMODE) & trips$TRIPMODE>0  & trips$TOURPURP==2 & trips$FULLY_JOINT==0 & trips$TOURMODE==1], breaks = seq(1,26, by=1), freq = NULL, right=FALSE, weight = trips$finalweight[!is.na(trips$TRIPMODE) & trips$TRIPMODE>0  & trips$TOURPURP==2 & trips$FULLY_JOINT==0 & trips$TOURMODE==1])
tripmode2 <- wtd.hist(trips$TRIPMODE[!is.na(trips$TRIPMODE) & trips$TRIPMODE>0  & trips$TOURPURP==2 & trips$FULLY_JOINT==0 & trips$TOURMODE==2], breaks = seq(1,26, by=1), freq = NULL, right=FALSE, weight = trips$finalweight[!is.na(trips$TRIPMODE) & trips$TRIPMODE>0  & trips$TOURPURP==2 & trips$FULLY_JOINT==0 & trips$TOURMODE==2])
tripmode3 <- wtd.hist(trips$TRIPMODE[!is.na(trips$TRIPMODE) & trips$TRIPMODE>0  & trips$TOURPURP==2 & trips$FULLY_JOINT==0 & trips$TOURMODE==3], breaks = seq(1,26, by=1), freq = NULL, right=FALSE, weight = trips$finalweight[!is.na(trips$TRIPMODE) & trips$TRIPMODE>0  & trips$TOURPURP==2 & trips$FULLY_JOINT==0 & trips$TOURMODE==3])
tripmode4 <- wtd.hist(trips$TRIPMODE[!is.na(trips$TRIPMODE) & trips$TRIPMODE>0  & trips$TOURPURP==2 & trips$FULLY_JOINT==0 & trips$TOURMODE==4], breaks = seq(1,26, by=1), freq = NULL, right=FALSE, weight = trips$finalweight[!is.na(trips$TRIPMODE) & trips$TRIPMODE>0  & trips$TOURPURP==2 & trips$FULLY_JOINT==0 & trips$TOURMODE==4])
tripmode5 <- wtd.hist(trips$TRIPMODE[!is.na(trips$TRIPMODE) & trips$TRIPMODE>0  & trips$TOURPURP==2 & trips$FULLY_JOINT==0 & trips$TOURMODE==5], breaks = seq(1,26, by=1), freq = NULL, right=FALSE, weight = trips$finalweight[!is.na(trips$TRIPMODE) & trips$TRIPMODE>0  & trips$TOURPURP==2 & trips$FULLY_JOINT==0 & trips$TOURMODE==5])
tripmode6 <- wtd.hist(trips$TRIPMODE[!is.na(trips$TRIPMODE) & trips$TRIPMODE>0  & trips$TOURPURP==2 & trips$FULLY_JOINT==0 & trips$TOURMODE==6], breaks = seq(1,26, by=1), freq = NULL, right=FALSE, weight = trips$finalweight[!is.na(trips$TRIPMODE) & trips$TRIPMODE>0  & trips$TOURPURP==2 & trips$FULLY_JOINT==0 & trips$TOURMODE==6])
tripmode7 <- wtd.hist(trips$TRIPMODE[!is.na(trips$TRIPMODE) & trips$TRIPMODE>0  & trips$TOURPURP==2 & trips$FULLY_JOINT==0 & trips$TOURMODE==7], breaks = seq(1,26, by=1), freq = NULL, right=FALSE, weight = trips$finalweight[!is.na(trips$TRIPMODE) & trips$TRIPMODE>0  & trips$TOURPURP==2 & trips$FULLY_JOINT==0 & trips$TOURMODE==7])
tripmode8 <- wtd.hist(trips$TRIPMODE[!is.na(trips$TRIPMODE) & trips$TRIPMODE>0  & trips$TOURPURP==2 & trips$FULLY_JOINT==0 & trips$TOURMODE==8], breaks = seq(1,26, by=1), freq = NULL, right=FALSE, weight = trips$finalweight[!is.na(trips$TRIPMODE) & trips$TRIPMODE>0  & trips$TOURPURP==2 & trips$FULLY_JOINT==0 & trips$TOURMODE==8])
tripmode9 <- wtd.hist(trips$TRIPMODE[!is.na(trips$TRIPMODE) & trips$TRIPMODE>0  & trips$TOURPURP==2 & trips$FULLY_JOINT==0 & trips$TOURMODE==9], breaks = seq(1,26, by=1), freq = NULL, right=FALSE, weight = trips$finalweight[!is.na(trips$TRIPMODE) & trips$TRIPMODE>0  & trips$TOURPURP==2 & trips$FULLY_JOINT==0 & trips$TOURMODE==9])

tripModeProfile <- data.frame(tripmode1$counts, tripmode2$counts, tripmode3$counts, tripmode4$counts,
                              tripmode5$counts, tripmode6$counts, tripmode7$counts, tripmode8$counts, tripmode9$counts)
colnames(tripModeProfile) <- c("tourmode1", "tourmode2", "tourmode3", "tourmode4", "tourmode5", "tourmode6", "tourmode7", "tourmode8", "tourmode9")
write.csv(tripModeProfile, "tripModeProfile_Univ.csv")

tripModeProfile2_vis <- tripModeProfile[1:9,]
tripModeProfile2_vis$id <- row.names(tripModeProfile2_vis)
tripModeProfile2_vis <- melt(tripModeProfile2_vis, id = c("id"))
colnames(tripModeProfile2_vis) <- c("id", "purpose", "freq2")

tripModeProfile2_vis <- xtabs(freq2~id+purpose, tripModeProfile2_vis)
tripModeProfile2_vis[is.na(tripModeProfile2_vis)] <- 0
tripModeProfile2_vis <- addmargins(as.table(tripModeProfile2_vis))
tripModeProfile2_vis <- as.data.frame.matrix(tripModeProfile2_vis)

tripModeProfile2_vis$id <- row.names(tripModeProfile2_vis)
tripModeProfile2_vis <- melt(tripModeProfile2_vis, id = c("id"))
colnames(tripModeProfile2_vis) <- c("id", "purpose", "freq2")
tripModeProfile2_vis$id <- as.character(tripModeProfile2_vis$id)
tripModeProfile2_vis$purpose <- as.character(tripModeProfile2_vis$purpose)
tripModeProfile2_vis <- tripModeProfile2_vis[tripModeProfile2_vis$id!="Sum",]
tripModeProfile2_vis$purpose[tripModeProfile2_vis$purpose=="Sum"] <- "Total"
                                                             
##School                                                       
#tripmode1 <- wtd.hist(trips$TRIPMODE[!is.na(trips$TRIPMODE) & trips$TRIPMODE>0  & trips$TOURPURP==3 & trips$FULLY_JOINT==0 & trips$TOURMODE==1], breaks = seq(1,12, by=1), freq = NULL, right=FALSE, weight = trips$finalweight[!is.na(trips$TRIPMODE) & trips$TRIPMODE>0  & trips$TOURPURP==3 & trips$FULLY_JOINT==0 & trips$TOURMODE==1])
#tripmode2 <- wtd.hist(trips$TRIPMODE[!is.na(trips$TRIPMODE) & trips$TRIPMODE>0  & trips$TOURPURP==3 & trips$FULLY_JOINT==0 & trips$TOURMODE==2], breaks = seq(1,12, by=1), freq = NULL, right=FALSE, weight = trips$finalweight[!is.na(trips$TRIPMODE) & trips$TRIPMODE>0  & trips$TOURPURP==3 & trips$FULLY_JOINT==0 & trips$TOURMODE==2])
#tripmode3 <- wtd.hist(trips$TRIPMODE[!is.na(trips$TRIPMODE) & trips$TRIPMODE>0  & trips$TOURPURP==3 & trips$FULLY_JOINT==0 & trips$TOURMODE==3], breaks = seq(1,12, by=1), freq = NULL, right=FALSE, weight = trips$finalweight[!is.na(trips$TRIPMODE) & trips$TRIPMODE>0  & trips$TOURPURP==3 & trips$FULLY_JOINT==0 & trips$TOURMODE==3])
#tripmode4 <- wtd.hist(trips$TRIPMODE[!is.na(trips$TRIPMODE) & trips$TRIPMODE>0  & trips$TOURPURP==3 & trips$FULLY_JOINT==0 & trips$TOURMODE==4], breaks = seq(1,12, by=1), freq = NULL, right=FALSE, weight = trips$finalweight[!is.na(trips$TRIPMODE) & trips$TRIPMODE>0  & trips$TOURPURP==3 & trips$FULLY_JOINT==0 & trips$TOURMODE==4])
#tripmode5 <- wtd.hist(trips$TRIPMODE[!is.na(trips$TRIPMODE) & trips$TRIPMODE>0  & trips$TOURPURP==3 & trips$FULLY_JOINT==0 & trips$TOURMODE==5], breaks = seq(1,12, by=1), freq = NULL, right=FALSE, weight = trips$finalweight[!is.na(trips$TRIPMODE) & trips$TRIPMODE>0  & trips$TOURPURP==3 & trips$FULLY_JOINT==0 & trips$TOURMODE==5])
#tripmode6 <- wtd.hist(trips$TRIPMODE[!is.na(trips$TRIPMODE) & trips$TRIPMODE>0  & trips$TOURPURP==3 & trips$FULLY_JOINT==0 & trips$TOURMODE==6], breaks = seq(1,12, by=1), freq = NULL, right=FALSE, weight = trips$finalweight[!is.na(trips$TRIPMODE) & trips$TRIPMODE>0  & trips$TOURPURP==3 & trips$FULLY_JOINT==0 & trips$TOURMODE==6])
#tripmode7 <- wtd.hist(trips$TRIPMODE[!is.na(trips$TRIPMODE) & trips$TRIPMODE>0  & trips$TOURPURP==3 & trips$FULLY_JOINT==0 & trips$TOURMODE==7], breaks = seq(1,12, by=1), freq = NULL, right=FALSE, weight = trips$finalweight[!is.na(trips$TRIPMODE) & trips$TRIPMODE>0  & trips$TOURPURP==3 & trips$FULLY_JOINT==0 & trips$TOURMODE==7])
#tripmode8 <- wtd.hist(trips$TRIPMODE[!is.na(trips$TRIPMODE) & trips$TRIPMODE>0  & trips$TOURPURP==3 & trips$FULLY_JOINT==0 & trips$TOURMODE==8], breaks = seq(1,12, by=1), freq = NULL, right=FALSE, weight = trips$finalweight[!is.na(trips$TRIPMODE) & trips$TRIPMODE>0  & trips$TOURPURP==3 & trips$FULLY_JOINT==0 & trips$TOURMODE==8])
#tripmode9 <- wtd.hist(trips$TRIPMODE[!is.na(trips$TRIPMODE) & trips$TRIPMODE>0  & trips$TOURPURP==3 & trips$FULLY_JOINT==0 & trips$TOURMODE==9], breaks = seq(1,12, by=1), freq = NULL, right=FALSE, weight = trips$finalweight[!is.na(trips$TRIPMODE) & trips$TRIPMODE>0  & trips$TOURPURP==3 & trips$FULLY_JOINT==0 & trips$TOURMODE==9])
#
#tripModeProfile <- data.frame(tripmode1$counts, tripmode2$counts, tripmode3$counts, tripmode4$counts,
#                              tripmode5$counts, tripmode6$counts, tripmode7$counts, tripmode8$counts, tripmode9$counts)
#colnames(tripModeProfile) <- c("tourmode1", "tourmode2", "tourmode3", "tourmode4", "tourmode5", "tourmode6", "tourmode7", "tourmode8", "tourmode9")
#write.csv(tripModeProfile, "tripModeProfile_Schl.csv")
#
#tripModeProfile3_vis <- tripModeProfile[1:9,]
#tripModeProfile3_vis$id <- row.names(tripModeProfile3_vis)
#tripModeProfile3_vis <- melt(tripModeProfile3_vis, id = c("id"))
#colnames(tripModeProfile3_vis) <- c("id", "purpose", "freq3")
#
#tripModeProfile3_vis <- xtabs(freq3~id+purpose, tripModeProfile3_vis)
#tripModeProfile3_vis[is.na(tripModeProfile3_vis)] <- 0
#tripModeProfile3_vis <- addmargins(as.table(tripModeProfile3_vis))
#tripModeProfile3_vis <- as.data.frame.matrix(tripModeProfile3_vis)
#
#tripModeProfile3_vis$id <- row.names(tripModeProfile3_vis)
#tripModeProfile3_vis <- melt(tripModeProfile3_vis, id = c("id"))
#colnames(tripModeProfile3_vis) <- c("id", "purpose", "freq3")
#tripModeProfile3_vis$id <- as.character(tripModeProfile3_vis$id)
#tripModeProfile3_vis$purpose <- as.character(tripModeProfile3_vis$purpose)
#tripModeProfile3_vis <- tripModeProfile3_vis[tripModeProfile3_vis$id!="Sum",]
#tripModeProfile3_vis$purpose[tripModeProfile3_vis$purpose=="Sum"] <- "Total"
                                                             
#iMain                                                        
tripmode1 <- wtd.hist(trips$TRIPMODE[!is.na(trips$TRIPMODE) & trips$TRIPMODE>0  & trips$TOURPURP>=4 & trips$TOURPURP<=6 & trips$FULLY_JOINT==0 & trips$TOURMODE==1], breaks = seq(1,26, by=1), freq = NULL, right=FALSE, weight = trips$finalweight[!is.na(trips$TRIPMODE) & trips$TRIPMODE>0  & trips$TOURPURP>=4 & trips$TOURPURP<=6 & trips$FULLY_JOINT==0 & trips$TOURMODE==1])
tripmode2 <- wtd.hist(trips$TRIPMODE[!is.na(trips$TRIPMODE) & trips$TRIPMODE>0  & trips$TOURPURP>=4 & trips$TOURPURP<=6 & trips$FULLY_JOINT==0 & trips$TOURMODE==2], breaks = seq(1,26, by=1), freq = NULL, right=FALSE, weight = trips$finalweight[!is.na(trips$TRIPMODE) & trips$TRIPMODE>0  & trips$TOURPURP>=4 & trips$TOURPURP<=6 & trips$FULLY_JOINT==0 & trips$TOURMODE==2])
tripmode3 <- wtd.hist(trips$TRIPMODE[!is.na(trips$TRIPMODE) & trips$TRIPMODE>0  & trips$TOURPURP>=4 & trips$TOURPURP<=6 & trips$FULLY_JOINT==0 & trips$TOURMODE==3], breaks = seq(1,26, by=1), freq = NULL, right=FALSE, weight = trips$finalweight[!is.na(trips$TRIPMODE) & trips$TRIPMODE>0  & trips$TOURPURP>=4 & trips$TOURPURP<=6 & trips$FULLY_JOINT==0 & trips$TOURMODE==3])
tripmode4 <- wtd.hist(trips$TRIPMODE[!is.na(trips$TRIPMODE) & trips$TRIPMODE>0  & trips$TOURPURP>=4 & trips$TOURPURP<=6 & trips$FULLY_JOINT==0 & trips$TOURMODE==4], breaks = seq(1,26, by=1), freq = NULL, right=FALSE, weight = trips$finalweight[!is.na(trips$TRIPMODE) & trips$TRIPMODE>0  & trips$TOURPURP>=4 & trips$TOURPURP<=6 & trips$FULLY_JOINT==0 & trips$TOURMODE==4])
tripmode5 <- wtd.hist(trips$TRIPMODE[!is.na(trips$TRIPMODE) & trips$TRIPMODE>0  & trips$TOURPURP>=4 & trips$TOURPURP<=6 & trips$FULLY_JOINT==0 & trips$TOURMODE==5], breaks = seq(1,26, by=1), freq = NULL, right=FALSE, weight = trips$finalweight[!is.na(trips$TRIPMODE) & trips$TRIPMODE>0  & trips$TOURPURP>=4 & trips$TOURPURP<=6 & trips$FULLY_JOINT==0 & trips$TOURMODE==5])
tripmode6 <- wtd.hist(trips$TRIPMODE[!is.na(trips$TRIPMODE) & trips$TRIPMODE>0  & trips$TOURPURP>=4 & trips$TOURPURP<=6 & trips$FULLY_JOINT==0 & trips$TOURMODE==6], breaks = seq(1,26, by=1), freq = NULL, right=FALSE, weight = trips$finalweight[!is.na(trips$TRIPMODE) & trips$TRIPMODE>0  & trips$TOURPURP>=4 & trips$TOURPURP<=6 & trips$FULLY_JOINT==0 & trips$TOURMODE==6])
tripmode7 <- wtd.hist(trips$TRIPMODE[!is.na(trips$TRIPMODE) & trips$TRIPMODE>0  & trips$TOURPURP>=4 & trips$TOURPURP<=6 & trips$FULLY_JOINT==0 & trips$TOURMODE==7], breaks = seq(1,26, by=1), freq = NULL, right=FALSE, weight = trips$finalweight[!is.na(trips$TRIPMODE) & trips$TRIPMODE>0  & trips$TOURPURP>=4 & trips$TOURPURP<=6 & trips$FULLY_JOINT==0 & trips$TOURMODE==7])
tripmode8 <- wtd.hist(trips$TRIPMODE[!is.na(trips$TRIPMODE) & trips$TRIPMODE>0  & trips$TOURPURP>=4 & trips$TOURPURP<=6 & trips$FULLY_JOINT==0 & trips$TOURMODE==8], breaks = seq(1,26, by=1), freq = NULL, right=FALSE, weight = trips$finalweight[!is.na(trips$TRIPMODE) & trips$TRIPMODE>0  & trips$TOURPURP>=4 & trips$TOURPURP<=6 & trips$FULLY_JOINT==0 & trips$TOURMODE==8])
tripmode9 <- wtd.hist(trips$TRIPMODE[!is.na(trips$TRIPMODE) & trips$TRIPMODE>0  & trips$TOURPURP>=4 & trips$TOURPURP<=6 & trips$FULLY_JOINT==0 & trips$TOURMODE==9], breaks = seq(1,26, by=1), freq = NULL, right=FALSE, weight = trips$finalweight[!is.na(trips$TRIPMODE) & trips$TRIPMODE>0  & trips$TOURPURP>=4 & trips$TOURPURP<=6 & trips$FULLY_JOINT==0 & trips$TOURMODE==9])

tripModeProfile <- data.frame(tripmode1$counts, tripmode2$counts, tripmode3$counts, tripmode4$counts,
                              tripmode5$counts, tripmode6$counts, tripmode7$counts, tripmode8$counts, tripmode9$counts)
colnames(tripModeProfile) <- c("tourmode1", "tourmode2", "tourmode3", "tourmode4", "tourmode5", "tourmode6", "tourmode7", "tourmode8", "tourmode9")
write.csv(tripModeProfile, "tripModeProfile_iMain.csv")

tripModeProfile4_vis <- tripModeProfile[1:9,]
tripModeProfile4_vis$id <- row.names(tripModeProfile4_vis)
tripModeProfile4_vis <- melt(tripModeProfile4_vis, id = c("id"))
colnames(tripModeProfile4_vis) <- c("id", "purpose", "freq4")

tripModeProfile4_vis <- xtabs(freq4~id+purpose, tripModeProfile4_vis)
tripModeProfile4_vis[is.na(tripModeProfile4_vis)] <- 0
tripModeProfile4_vis <- addmargins(as.table(tripModeProfile4_vis))
tripModeProfile4_vis <- as.data.frame.matrix(tripModeProfile4_vis)

tripModeProfile4_vis$id <- row.names(tripModeProfile4_vis)
tripModeProfile4_vis <- melt(tripModeProfile4_vis, id = c("id"))
colnames(tripModeProfile4_vis) <- c("id", "purpose", "freq4")
tripModeProfile4_vis$id <- as.character(tripModeProfile4_vis$id)
tripModeProfile4_vis$purpose <- as.character(tripModeProfile4_vis$purpose)
tripModeProfile4_vis <- tripModeProfile4_vis[tripModeProfile4_vis$id!="Sum",]
tripModeProfile4_vis$purpose[tripModeProfile4_vis$purpose=="Sum"] <- "Total"

#iDisc                                                        
tripmode1 <- wtd.hist(trips$TRIPMODE[!is.na(trips$TRIPMODE) & trips$TRIPMODE>0  & trips$TOURPURP>=7 & trips$TOURPURP<=9 & trips$FULLY_JOINT==0 & trips$TOURMODE==1], breaks = seq(1,26, by=1), freq = NULL, right=FALSE, weight = trips$finalweight[!is.na(trips$TRIPMODE) & trips$TRIPMODE>0  & trips$TOURPURP>=7 & trips$TOURPURP<=9 & trips$FULLY_JOINT==0 & trips$TOURMODE==1])
tripmode2 <- wtd.hist(trips$TRIPMODE[!is.na(trips$TRIPMODE) & trips$TRIPMODE>0  & trips$TOURPURP>=7 & trips$TOURPURP<=9 & trips$FULLY_JOINT==0 & trips$TOURMODE==2], breaks = seq(1,26, by=1), freq = NULL, right=FALSE, weight = trips$finalweight[!is.na(trips$TRIPMODE) & trips$TRIPMODE>0  & trips$TOURPURP>=7 & trips$TOURPURP<=9 & trips$FULLY_JOINT==0 & trips$TOURMODE==2])
tripmode3 <- wtd.hist(trips$TRIPMODE[!is.na(trips$TRIPMODE) & trips$TRIPMODE>0  & trips$TOURPURP>=7 & trips$TOURPURP<=9 & trips$FULLY_JOINT==0 & trips$TOURMODE==3], breaks = seq(1,26, by=1), freq = NULL, right=FALSE, weight = trips$finalweight[!is.na(trips$TRIPMODE) & trips$TRIPMODE>0  & trips$TOURPURP>=7 & trips$TOURPURP<=9 & trips$FULLY_JOINT==0 & trips$TOURMODE==3])
tripmode4 <- wtd.hist(trips$TRIPMODE[!is.na(trips$TRIPMODE) & trips$TRIPMODE>0  & trips$TOURPURP>=7 & trips$TOURPURP<=9 & trips$FULLY_JOINT==0 & trips$TOURMODE==4], breaks = seq(1,26, by=1), freq = NULL, right=FALSE, weight = trips$finalweight[!is.na(trips$TRIPMODE) & trips$TRIPMODE>0  & trips$TOURPURP>=7 & trips$TOURPURP<=9 & trips$FULLY_JOINT==0 & trips$TOURMODE==4])
tripmode5 <- wtd.hist(trips$TRIPMODE[!is.na(trips$TRIPMODE) & trips$TRIPMODE>0  & trips$TOURPURP>=7 & trips$TOURPURP<=9 & trips$FULLY_JOINT==0 & trips$TOURMODE==5], breaks = seq(1,26, by=1), freq = NULL, right=FALSE, weight = trips$finalweight[!is.na(trips$TRIPMODE) & trips$TRIPMODE>0  & trips$TOURPURP>=7 & trips$TOURPURP<=9 & trips$FULLY_JOINT==0 & trips$TOURMODE==5])
tripmode6 <- wtd.hist(trips$TRIPMODE[!is.na(trips$TRIPMODE) & trips$TRIPMODE>0  & trips$TOURPURP>=7 & trips$TOURPURP<=9 & trips$FULLY_JOINT==0 & trips$TOURMODE==6], breaks = seq(1,26, by=1), freq = NULL, right=FALSE, weight = trips$finalweight[!is.na(trips$TRIPMODE) & trips$TRIPMODE>0  & trips$TOURPURP>=7 & trips$TOURPURP<=9 & trips$FULLY_JOINT==0 & trips$TOURMODE==6])
tripmode7 <- wtd.hist(trips$TRIPMODE[!is.na(trips$TRIPMODE) & trips$TRIPMODE>0  & trips$TOURPURP>=7 & trips$TOURPURP<=9 & trips$FULLY_JOINT==0 & trips$TOURMODE==7], breaks = seq(1,26, by=1), freq = NULL, right=FALSE, weight = trips$finalweight[!is.na(trips$TRIPMODE) & trips$TRIPMODE>0  & trips$TOURPURP>=7 & trips$TOURPURP<=9 & trips$FULLY_JOINT==0 & trips$TOURMODE==7])
tripmode8 <- wtd.hist(trips$TRIPMODE[!is.na(trips$TRIPMODE) & trips$TRIPMODE>0  & trips$TOURPURP>=7 & trips$TOURPURP<=9 & trips$FULLY_JOINT==0 & trips$TOURMODE==8], breaks = seq(1,26, by=1), freq = NULL, right=FALSE, weight = trips$finalweight[!is.na(trips$TRIPMODE) & trips$TRIPMODE>0  & trips$TOURPURP>=7 & trips$TOURPURP<=9 & trips$FULLY_JOINT==0 & trips$TOURMODE==8])
tripmode9 <- wtd.hist(trips$TRIPMODE[!is.na(trips$TRIPMODE) & trips$TRIPMODE>0  & trips$TOURPURP>=7 & trips$TOURPURP<=9 & trips$FULLY_JOINT==0 & trips$TOURMODE==9], breaks = seq(1,26, by=1), freq = NULL, right=FALSE, weight = trips$finalweight[!is.na(trips$TRIPMODE) & trips$TRIPMODE>0  & trips$TOURPURP>=7 & trips$TOURPURP<=9 & trips$FULLY_JOINT==0 & trips$TOURMODE==9])

tripModeProfile <- data.frame(tripmode1$counts, tripmode2$counts, tripmode3$counts, tripmode4$counts,
                              tripmode5$counts, tripmode6$counts, tripmode7$counts, tripmode8$counts, tripmode9$counts)
colnames(tripModeProfile) <- c("tourmode1", "tourmode2", "tourmode3", "tourmode4", "tourmode5", "tourmode6", "tourmode7", "tourmode8", "tourmode9")
write.csv(tripModeProfile, "tripModeProfile_iDisc.csv")

tripModeProfile5_vis <- tripModeProfile[1:9,]
tripModeProfile5_vis$id <- row.names(tripModeProfile5_vis)
tripModeProfile5_vis <- melt(tripModeProfile5_vis, id = c("id"))
colnames(tripModeProfile5_vis) <- c("id", "purpose", "freq5")

tripModeProfile5_vis <- xtabs(freq5~id+purpose, tripModeProfile5_vis)
tripModeProfile5_vis[is.na(tripModeProfile5_vis)] <- 0
tripModeProfile5_vis <- addmargins(as.table(tripModeProfile5_vis))
tripModeProfile5_vis <- as.data.frame.matrix(tripModeProfile5_vis)

tripModeProfile5_vis$id <- row.names(tripModeProfile5_vis)
tripModeProfile5_vis <- melt(tripModeProfile5_vis, id = c("id"))
colnames(tripModeProfile5_vis) <- c("id", "purpose", "freq5")
tripModeProfile5_vis$id <- as.character(tripModeProfile5_vis$id)
tripModeProfile5_vis$purpose <- as.character(tripModeProfile5_vis$purpose)
tripModeProfile5_vis <- tripModeProfile5_vis[tripModeProfile5_vis$id!="Sum",]
tripModeProfile5_vis$purpose[tripModeProfile5_vis$purpose=="Sum"] <- "Total"

#jMain                                                        
tripmode1 <- wtd.hist(jtrips$TRIPMODE[!is.na(jtrips$TRIPMODE) & jtrips$TRIPMODE>0  & jtrips$TOURPURP>=4 & jtrips$TOURPURP<=6 & jtrips$TOURMODE==1], breaks = seq(1,26, by=1), freq = NULL, right=FALSE, weight = jtrips$finalweight[!is.na(jtrips$TRIPMODE) & jtrips$TRIPMODE>0  & jtrips$TOURPURP>=4 & jtrips$TOURPURP<=6 & jtrips$TOURMODE==1])
tripmode2 <- wtd.hist(jtrips$TRIPMODE[!is.na(jtrips$TRIPMODE) & jtrips$TRIPMODE>0  & jtrips$TOURPURP>=4 & jtrips$TOURPURP<=6 & jtrips$TOURMODE==2], breaks = seq(1,26, by=1), freq = NULL, right=FALSE, weight = jtrips$finalweight[!is.na(jtrips$TRIPMODE) & jtrips$TRIPMODE>0  & jtrips$TOURPURP>=4 & jtrips$TOURPURP<=6 & jtrips$TOURMODE==2])
tripmode3 <- wtd.hist(jtrips$TRIPMODE[!is.na(jtrips$TRIPMODE) & jtrips$TRIPMODE>0  & jtrips$TOURPURP>=4 & jtrips$TOURPURP<=6 & jtrips$TOURMODE==3], breaks = seq(1,26, by=1), freq = NULL, right=FALSE, weight = jtrips$finalweight[!is.na(jtrips$TRIPMODE) & jtrips$TRIPMODE>0  & jtrips$TOURPURP>=4 & jtrips$TOURPURP<=6 & jtrips$TOURMODE==3])
tripmode4 <- wtd.hist(jtrips$TRIPMODE[!is.na(jtrips$TRIPMODE) & jtrips$TRIPMODE>0  & jtrips$TOURPURP>=4 & jtrips$TOURPURP<=6 & jtrips$TOURMODE==4], breaks = seq(1,26, by=1), freq = NULL, right=FALSE, weight = jtrips$finalweight[!is.na(jtrips$TRIPMODE) & jtrips$TRIPMODE>0  & jtrips$TOURPURP>=4 & jtrips$TOURPURP<=6 & jtrips$TOURMODE==4])
tripmode5 <- wtd.hist(jtrips$TRIPMODE[!is.na(jtrips$TRIPMODE) & jtrips$TRIPMODE>0  & jtrips$TOURPURP>=4 & jtrips$TOURPURP<=6 & jtrips$TOURMODE==5], breaks = seq(1,26, by=1), freq = NULL, right=FALSE, weight = jtrips$finalweight[!is.na(jtrips$TRIPMODE) & jtrips$TRIPMODE>0  & jtrips$TOURPURP>=4 & jtrips$TOURPURP<=6 & jtrips$TOURMODE==5])
tripmode6 <- wtd.hist(jtrips$TRIPMODE[!is.na(jtrips$TRIPMODE) & jtrips$TRIPMODE>0  & jtrips$TOURPURP>=4 & jtrips$TOURPURP<=6 & jtrips$TOURMODE==6], breaks = seq(1,26, by=1), freq = NULL, right=FALSE, weight = jtrips$finalweight[!is.na(jtrips$TRIPMODE) & jtrips$TRIPMODE>0  & jtrips$TOURPURP>=4 & jtrips$TOURPURP<=6 & jtrips$TOURMODE==6])
tripmode7 <- wtd.hist(jtrips$TRIPMODE[!is.na(jtrips$TRIPMODE) & jtrips$TRIPMODE>0  & jtrips$TOURPURP>=4 & jtrips$TOURPURP<=6 & jtrips$TOURMODE==7], breaks = seq(1,26, by=1), freq = NULL, right=FALSE, weight = jtrips$finalweight[!is.na(jtrips$TRIPMODE) & jtrips$TRIPMODE>0  & jtrips$TOURPURP>=4 & jtrips$TOURPURP<=6 & jtrips$TOURMODE==7])
tripmode8 <- wtd.hist(jtrips$TRIPMODE[!is.na(jtrips$TRIPMODE) & jtrips$TRIPMODE>0  & jtrips$TOURPURP>=4 & jtrips$TOURPURP<=6 & jtrips$TOURMODE==8], breaks = seq(1,26, by=1), freq = NULL, right=FALSE, weight = jtrips$finalweight[!is.na(jtrips$TRIPMODE) & jtrips$TRIPMODE>0  & jtrips$TOURPURP>=4 & jtrips$TOURPURP<=6 & jtrips$TOURMODE==8])
tripmode9 <- wtd.hist(jtrips$TRIPMODE[!is.na(jtrips$TRIPMODE) & jtrips$TRIPMODE>0  & jtrips$TOURPURP>=4 & jtrips$TOURPURP<=6 & jtrips$TOURMODE==9], breaks = seq(1,26, by=1), freq = NULL, right=FALSE, weight = jtrips$finalweight[!is.na(jtrips$TRIPMODE) & jtrips$TRIPMODE>0  & jtrips$TOURPURP>=4 & jtrips$TOURPURP<=6 & jtrips$TOURMODE==9])

tripModeProfile <- data.frame(tripmode1$counts, tripmode2$counts, tripmode3$counts, tripmode4$counts,
                              tripmode5$counts, tripmode6$counts, tripmode7$counts, tripmode8$counts, tripmode9$counts)
colnames(tripModeProfile) <- c("tourmode1", "tourmode2", "tourmode3", "tourmode4", "tourmode5", "tourmode6", "tourmode7", "tourmode8", "tourmode9")
write.csv(tripModeProfile, "tripModeProfile_jMain.csv")

tripModeProfile6_vis <- tripModeProfile[1:9,]
tripModeProfile6_vis$id <- row.names(tripModeProfile6_vis)
tripModeProfile6_vis <- melt(tripModeProfile6_vis, id = c("id"))
colnames(tripModeProfile6_vis) <- c("id", "purpose", "freq6")

tripModeProfile6_vis <- xtabs(freq6~id+purpose, tripModeProfile6_vis)
tripModeProfile6_vis[is.na(tripModeProfile6_vis)] <- 0
tripModeProfile6_vis <- addmargins(as.table(tripModeProfile6_vis))
tripModeProfile6_vis <- as.data.frame.matrix(tripModeProfile6_vis)

tripModeProfile6_vis$id <- row.names(tripModeProfile6_vis)
tripModeProfile6_vis <- melt(tripModeProfile6_vis, id = c("id"))
colnames(tripModeProfile6_vis) <- c("id", "purpose", "freq6")
tripModeProfile6_vis$id <- as.character(tripModeProfile6_vis$id)
tripModeProfile6_vis$purpose <- as.character(tripModeProfile6_vis$purpose)
tripModeProfile6_vis <- tripModeProfile6_vis[tripModeProfile6_vis$id!="Sum",]
tripModeProfile6_vis$purpose[tripModeProfile6_vis$purpose=="Sum"] <- "Total"

#jDisc                                                        
tripmode1 <- wtd.hist(jtrips$TRIPMODE[!is.na(jtrips$TRIPMODE) & jtrips$TRIPMODE>0  & jtrips$TOURPURP>=7 & jtrips$TOURPURP<=9 & jtrips$TOURMODE==1], breaks = seq(1,26, by=1), freq = NULL, right=FALSE, weight = jtrips$finalweight[!is.na(jtrips$TRIPMODE) & jtrips$TRIPMODE>0  & jtrips$TOURPURP>=7 & jtrips$TOURPURP<=9 & jtrips$TOURMODE==1])
tripmode2 <- wtd.hist(jtrips$TRIPMODE[!is.na(jtrips$TRIPMODE) & jtrips$TRIPMODE>0  & jtrips$TOURPURP>=7 & jtrips$TOURPURP<=9 & jtrips$TOURMODE==2], breaks = seq(1,26, by=1), freq = NULL, right=FALSE, weight = jtrips$finalweight[!is.na(jtrips$TRIPMODE) & jtrips$TRIPMODE>0  & jtrips$TOURPURP>=7 & jtrips$TOURPURP<=9 & jtrips$TOURMODE==2])
tripmode3 <- wtd.hist(jtrips$TRIPMODE[!is.na(jtrips$TRIPMODE) & jtrips$TRIPMODE>0  & jtrips$TOURPURP>=7 & jtrips$TOURPURP<=9 & jtrips$TOURMODE==3], breaks = seq(1,26, by=1), freq = NULL, right=FALSE, weight = jtrips$finalweight[!is.na(jtrips$TRIPMODE) & jtrips$TRIPMODE>0  & jtrips$TOURPURP>=7 & jtrips$TOURPURP<=9 & jtrips$TOURMODE==3])
tripmode4 <- wtd.hist(jtrips$TRIPMODE[!is.na(jtrips$TRIPMODE) & jtrips$TRIPMODE>0  & jtrips$TOURPURP>=7 & jtrips$TOURPURP<=9 & jtrips$TOURMODE==4], breaks = seq(1,26, by=1), freq = NULL, right=FALSE, weight = jtrips$finalweight[!is.na(jtrips$TRIPMODE) & jtrips$TRIPMODE>0  & jtrips$TOURPURP>=7 & jtrips$TOURPURP<=9 & jtrips$TOURMODE==4])
tripmode5 <- wtd.hist(jtrips$TRIPMODE[!is.na(jtrips$TRIPMODE) & jtrips$TRIPMODE>0  & jtrips$TOURPURP>=7 & jtrips$TOURPURP<=9 & jtrips$TOURMODE==5], breaks = seq(1,26, by=1), freq = NULL, right=FALSE, weight = jtrips$finalweight[!is.na(jtrips$TRIPMODE) & jtrips$TRIPMODE>0  & jtrips$TOURPURP>=7 & jtrips$TOURPURP<=9 & jtrips$TOURMODE==5])
tripmode6 <- wtd.hist(jtrips$TRIPMODE[!is.na(jtrips$TRIPMODE) & jtrips$TRIPMODE>0  & jtrips$TOURPURP>=7 & jtrips$TOURPURP<=9 & jtrips$TOURMODE==6], breaks = seq(1,26, by=1), freq = NULL, right=FALSE, weight = jtrips$finalweight[!is.na(jtrips$TRIPMODE) & jtrips$TRIPMODE>0  & jtrips$TOURPURP>=7 & jtrips$TOURPURP<=9 & jtrips$TOURMODE==6])
tripmode7 <- wtd.hist(jtrips$TRIPMODE[!is.na(jtrips$TRIPMODE) & jtrips$TRIPMODE>0  & jtrips$TOURPURP>=7 & jtrips$TOURPURP<=9 & jtrips$TOURMODE==7], breaks = seq(1,26, by=1), freq = NULL, right=FALSE, weight = jtrips$finalweight[!is.na(jtrips$TRIPMODE) & jtrips$TRIPMODE>0  & jtrips$TOURPURP>=7 & jtrips$TOURPURP<=9 & jtrips$TOURMODE==7])
tripmode8 <- wtd.hist(jtrips$TRIPMODE[!is.na(jtrips$TRIPMODE) & jtrips$TRIPMODE>0  & jtrips$TOURPURP>=7 & jtrips$TOURPURP<=9 & jtrips$TOURMODE==8], breaks = seq(1,26, by=1), freq = NULL, right=FALSE, weight = jtrips$finalweight[!is.na(jtrips$TRIPMODE) & jtrips$TRIPMODE>0  & jtrips$TOURPURP>=7 & jtrips$TOURPURP<=9 & jtrips$TOURMODE==8])
tripmode9 <- wtd.hist(jtrips$TRIPMODE[!is.na(jtrips$TRIPMODE) & jtrips$TRIPMODE>0  & jtrips$TOURPURP>=7 & jtrips$TOURPURP<=9 & jtrips$TOURMODE==9], breaks = seq(1,26, by=1), freq = NULL, right=FALSE, weight = jtrips$finalweight[!is.na(jtrips$TRIPMODE) & jtrips$TRIPMODE>0  & jtrips$TOURPURP>=7 & jtrips$TOURPURP<=9 & jtrips$TOURMODE==9])

tripModeProfile <- data.frame(tripmode1$counts, tripmode2$counts, tripmode3$counts, tripmode4$counts,
                              tripmode5$counts, tripmode6$counts, tripmode7$counts, tripmode8$counts, tripmode9$counts)
colnames(tripModeProfile) <- c("tourmode1", "tourmode2", "tourmode3", "tourmode4", "tourmode5", "tourmode6", "tourmode7", "tourmode8", "tourmode9")
write.csv(tripModeProfile, "tripModeProfile_jDisc.csv")

tripModeProfile7_vis <- tripModeProfile[1:9,]
tripModeProfile7_vis$id <- row.names(tripModeProfile7_vis)
tripModeProfile7_vis <- melt(tripModeProfile7_vis, id = c("id"))
colnames(tripModeProfile7_vis) <- c("id", "purpose", "freq7")

tripModeProfile7_vis <- xtabs(freq7~id+purpose, tripModeProfile7_vis)
tripModeProfile7_vis[is.na(tripModeProfile7_vis)] <- 0
tripModeProfile7_vis <- addmargins(as.table(tripModeProfile7_vis))
tripModeProfile7_vis <- as.data.frame.matrix(tripModeProfile7_vis)

tripModeProfile7_vis$id <- row.names(tripModeProfile7_vis)
tripModeProfile7_vis <- melt(tripModeProfile7_vis, id = c("id"))
colnames(tripModeProfile7_vis) <- c("id", "purpose", "freq7")
tripModeProfile7_vis$id <- as.character(tripModeProfile7_vis$id)
tripModeProfile7_vis$purpose <- as.character(tripModeProfile7_vis$purpose)
tripModeProfile7_vis <- tripModeProfile7_vis[tripModeProfile7_vis$id!="Sum",]
tripModeProfile7_vis$purpose[tripModeProfile7_vis$purpose=="Sum"] <- "Total"

#At-work                                                       
tripmode1 <- wtd.hist(trips$TRIPMODE[!is.na(trips$TRIPMODE) & trips$TRIPMODE>0  & trips$SUBTOUR==1 & trips$TOURMODE==1], breaks = seq(1,26, by=1), freq = NULL, right=FALSE, weight = trips$finalweight[!is.na(trips$TRIPMODE) & trips$TRIPMODE>0  & trips$SUBTOUR==1 & trips$TOURMODE==1])
tripmode2 <- wtd.hist(trips$TRIPMODE[!is.na(trips$TRIPMODE) & trips$TRIPMODE>0  & trips$SUBTOUR==1 & trips$TOURMODE==2], breaks = seq(1,26, by=1), freq = NULL, right=FALSE, weight = trips$finalweight[!is.na(trips$TRIPMODE) & trips$TRIPMODE>0  & trips$SUBTOUR==1 & trips$TOURMODE==2])
tripmode3 <- wtd.hist(trips$TRIPMODE[!is.na(trips$TRIPMODE) & trips$TRIPMODE>0  & trips$SUBTOUR==1 & trips$TOURMODE==3], breaks = seq(1,26, by=1), freq = NULL, right=FALSE, weight = trips$finalweight[!is.na(trips$TRIPMODE) & trips$TRIPMODE>0  & trips$SUBTOUR==1 & trips$TOURMODE==3])
tripmode4 <- wtd.hist(trips$TRIPMODE[!is.na(trips$TRIPMODE) & trips$TRIPMODE>0  & trips$SUBTOUR==1 & trips$TOURMODE==4], breaks = seq(1,26, by=1), freq = NULL, right=FALSE, weight = trips$finalweight[!is.na(trips$TRIPMODE) & trips$TRIPMODE>0  & trips$SUBTOUR==1 & trips$TOURMODE==4])
tripmode5 <- wtd.hist(trips$TRIPMODE[!is.na(trips$TRIPMODE) & trips$TRIPMODE>0  & trips$SUBTOUR==1 & trips$TOURMODE==5], breaks = seq(1,26, by=1), freq = NULL, right=FALSE, weight = trips$finalweight[!is.na(trips$TRIPMODE) & trips$TRIPMODE>0  & trips$SUBTOUR==1 & trips$TOURMODE==5])
tripmode6 <- wtd.hist(trips$TRIPMODE[!is.na(trips$TRIPMODE) & trips$TRIPMODE>0  & trips$SUBTOUR==1 & trips$TOURMODE==6], breaks = seq(1,26, by=1), freq = NULL, right=FALSE, weight = trips$finalweight[!is.na(trips$TRIPMODE) & trips$TRIPMODE>0  & trips$SUBTOUR==1 & trips$TOURMODE==6])
tripmode7 <- wtd.hist(trips$TRIPMODE[!is.na(trips$TRIPMODE) & trips$TRIPMODE>0  & trips$SUBTOUR==1 & trips$TOURMODE==7], breaks = seq(1,26, by=1), freq = NULL, right=FALSE, weight = trips$finalweight[!is.na(trips$TRIPMODE) & trips$TRIPMODE>0  & trips$SUBTOUR==1 & trips$TOURMODE==7])
tripmode8 <- wtd.hist(trips$TRIPMODE[!is.na(trips$TRIPMODE) & trips$TRIPMODE>0  & trips$SUBTOUR==1 & trips$TOURMODE==8], breaks = seq(1,26, by=1), freq = NULL, right=FALSE, weight = trips$finalweight[!is.na(trips$TRIPMODE) & trips$TRIPMODE>0  & trips$SUBTOUR==1 & trips$TOURMODE==8])
tripmode9 <- wtd.hist(trips$TRIPMODE[!is.na(trips$TRIPMODE) & trips$TRIPMODE>0  & trips$SUBTOUR==1 & trips$TOURMODE==9], breaks = seq(1,26, by=1), freq = NULL, right=FALSE, weight = trips$finalweight[!is.na(trips$TRIPMODE) & trips$TRIPMODE>0  & trips$SUBTOUR==1 & trips$TOURMODE==9])

tripModeProfile <- data.frame(tripmode1$counts, tripmode2$counts, tripmode3$counts, tripmode4$counts,
                              tripmode5$counts, tripmode6$counts, tripmode7$counts, tripmode8$counts, tripmode9$counts)
colnames(tripModeProfile) <- c("tourmode1", "tourmode2", "tourmode3", "tourmode4", "tourmode5", "tourmode6", "tourmode7", "tourmode8", "tourmode9")
write.csv(tripModeProfile, "tripModeProfile_ATW.csv")

tripModeProfile8_vis <- tripModeProfile[1:9,]
tripModeProfile8_vis$id <- row.names(tripModeProfile8_vis)
tripModeProfile8_vis <- melt(tripModeProfile8_vis, id = c("id"))
colnames(tripModeProfile8_vis) <- c("id", "purpose", "freq8")

tripModeProfile8_vis <- xtabs(freq8~id+purpose, tripModeProfile8_vis)
tripModeProfile8_vis[is.na(tripModeProfile8_vis)] <- 0
tripModeProfile8_vis <- addmargins(as.table(tripModeProfile8_vis))
tripModeProfile8_vis <- as.data.frame.matrix(tripModeProfile8_vis)

tripModeProfile8_vis$id <- row.names(tripModeProfile8_vis)
tripModeProfile8_vis <- melt(tripModeProfile8_vis, id = c("id"))
colnames(tripModeProfile8_vis) <- c("id", "purpose", "freq8")
tripModeProfile8_vis$id <- as.character(tripModeProfile8_vis$id)
tripModeProfile8_vis$purpose <- as.character(tripModeProfile8_vis$purpose)
tripModeProfile8_vis <- tripModeProfile8_vis[tripModeProfile8_vis$id!="Sum",]
tripModeProfile8_vis$purpose[tripModeProfile8_vis$purpose=="Sum"] <- "Total"

#Total                                                       
tripmode1 <- wtd.hist(trips$TRIPMODE[!is.na(trips$TRIPMODE) & trips$TRIPMODE>0  & trips$TOURMODE==1], breaks = seq(1,26, by=1), freq = NULL, right=FALSE, weight = trips$finalweight[!is.na(trips$TRIPMODE) & trips$TRIPMODE>0  & trips$TOURMODE==1])
tripmode2 <- wtd.hist(trips$TRIPMODE[!is.na(trips$TRIPMODE) & trips$TRIPMODE>0  & trips$TOURMODE==2], breaks = seq(1,26, by=1), freq = NULL, right=FALSE, weight = trips$finalweight[!is.na(trips$TRIPMODE) & trips$TRIPMODE>0  & trips$TOURMODE==2])
tripmode3 <- wtd.hist(trips$TRIPMODE[!is.na(trips$TRIPMODE) & trips$TRIPMODE>0  & trips$TOURMODE==3], breaks = seq(1,26, by=1), freq = NULL, right=FALSE, weight = trips$finalweight[!is.na(trips$TRIPMODE) & trips$TRIPMODE>0  & trips$TOURMODE==3])
tripmode4 <- wtd.hist(trips$TRIPMODE[!is.na(trips$TRIPMODE) & trips$TRIPMODE>0  & trips$TOURMODE==4], breaks = seq(1,26, by=1), freq = NULL, right=FALSE, weight = trips$finalweight[!is.na(trips$TRIPMODE) & trips$TRIPMODE>0  & trips$TOURMODE==4])
tripmode5 <- wtd.hist(trips$TRIPMODE[!is.na(trips$TRIPMODE) & trips$TRIPMODE>0  & trips$TOURMODE==5], breaks = seq(1,26, by=1), freq = NULL, right=FALSE, weight = trips$finalweight[!is.na(trips$TRIPMODE) & trips$TRIPMODE>0  & trips$TOURMODE==5])
tripmode6 <- wtd.hist(trips$TRIPMODE[!is.na(trips$TRIPMODE) & trips$TRIPMODE>0  & trips$TOURMODE==6], breaks = seq(1,26, by=1), freq = NULL, right=FALSE, weight = trips$finalweight[!is.na(trips$TRIPMODE) & trips$TRIPMODE>0  & trips$TOURMODE==6])
tripmode7 <- wtd.hist(trips$TRIPMODE[!is.na(trips$TRIPMODE) & trips$TRIPMODE>0  & trips$TOURMODE==7], breaks = seq(1,26, by=1), freq = NULL, right=FALSE, weight = trips$finalweight[!is.na(trips$TRIPMODE) & trips$TRIPMODE>0  & trips$TOURMODE==7])
tripmode8 <- wtd.hist(trips$TRIPMODE[!is.na(trips$TRIPMODE) & trips$TRIPMODE>0  & trips$TOURMODE==8], breaks = seq(1,26, by=1), freq = NULL, right=FALSE, weight = trips$finalweight[!is.na(trips$TRIPMODE) & trips$TRIPMODE>0  & trips$TOURMODE==8])
tripmode9 <- wtd.hist(trips$TRIPMODE[!is.na(trips$TRIPMODE) & trips$TRIPMODE>0  & trips$TOURMODE==9], breaks = seq(1,26, by=1), freq = NULL, right=FALSE, weight = trips$finalweight[!is.na(trips$TRIPMODE) & trips$TRIPMODE>0  & trips$TOURMODE==9])

tripModeProfile <- data.frame(tripmode1$counts, tripmode2$counts, tripmode3$counts, tripmode4$counts,
                              tripmode5$counts, tripmode6$counts, tripmode7$counts, tripmode8$counts, tripmode9$counts)
colnames(tripModeProfile) <- c("tourmode1", "tourmode2", "tourmode3", "tourmode4", "tourmode5", "tourmode6", "tourmode7", "tourmode8", "tourmode9")
write.csv(tripModeProfile, "tripModeProfile_Total.csv")

tripModeProfile9_vis <- tripModeProfile[1:9,]
tripModeProfile9_vis$id <- row.names(tripModeProfile9_vis)
tripModeProfile9_vis <- melt(tripModeProfile9_vis, id = c("id"))
colnames(tripModeProfile9_vis) <- c("id", "purpose", "freq9")

tripModeProfile9_vis <- xtabs(freq9~id+purpose, tripModeProfile9_vis)
tripModeProfile9_vis[is.na(tripModeProfile9_vis)] <- 0
tripModeProfile9_vis <- addmargins(as.table(tripModeProfile9_vis))
tripModeProfile9_vis <- as.data.frame.matrix(tripModeProfile9_vis)

tripModeProfile9_vis$id <- row.names(tripModeProfile9_vis)
tripModeProfile9_vis <- melt(tripModeProfile9_vis, id = c("id"))
colnames(tripModeProfile9_vis) <- c("id", "purpose", "freq9")
tripModeProfile9_vis$id <- as.character(tripModeProfile9_vis$id)
tripModeProfile9_vis$purpose <- as.character(tripModeProfile9_vis$purpose)
tripModeProfile9_vis <- tripModeProfile9_vis[tripModeProfile9_vis$id!="Sum",]
tripModeProfile9_vis$purpose[tripModeProfile9_vis$purpose=="Sum"] <- "Total"


# combine all tripmode profile for visualizer
tripModeProfile_vis <- data.frame(tripModeProfile1_vis, tripModeProfile2_vis$freq2
                                  , tripModeProfile4_vis$freq4, tripModeProfile5_vis$freq5, tripModeProfile6_vis$freq6
                                  , tripModeProfile7_vis$freq7, tripModeProfile8_vis$freq8, tripModeProfile9_vis$freq9)
colnames(tripModeProfile_vis) <- c("tripmode", "tourmode", "work", "univ", "imain", "idisc", "jmain", "jdisc", "atwork", "total")

temp <- melt(tripModeProfile_vis, id = c("tripmode", "tourmode"))
#tripModeProfile_vis <- cast(temp, tripmode+variable~tourmode)
#write.csv(tripModeProfile_vis, "tripModeProfile_vis.csv", row.names = F)
temp$grp_var <- paste(temp$variable, temp$tourmode, sep = "")

# rename tour mode to standard names
temp$tourmode[temp$tourmode=="tourmode1"] <- 'Auto SOV'
temp$tourmode[temp$tourmode=="tourmode2"] <- 'Auto 2 Person'
temp$tourmode[temp$tourmode=="tourmode3"] <- 'Auto 3+ Person'
temp$tourmode[temp$tourmode=="tourmode4"] <- 'Walk'
temp$tourmode[temp$tourmode=="tourmode5"] <- 'Bike/Moped'
temp$tourmode[temp$tourmode=="tourmode6"] <- 'Walk-Transit'
temp$tourmode[temp$tourmode=="tourmode7"] <- 'PNR-Transit'
temp$tourmode[temp$tourmode=="tourmode8"] <- 'KNR-Transit'
temp$tourmode[temp$tourmode=="tourmode9"] <- 'School Bus'

colnames(temp) <- c("tripmode","tourmode","purpose","value","grp_var")

write.csv(temp, "tripModeProfile_vis.csv", row.names = F)

###########
#trip mode by time period - for Wu, Sun
#calculate time of day
trips$tod <- 5 # EA: 3 am - 6 am
trips$tod <- ifelse(trips$ORIG_DEP_BIN>=4 & trips$ORIG_DEP_BIN<=9, 1, trips$tod)   # AM: 6 am - 9 am
trips$tod <- ifelse(trips$ORIG_DEP_BIN>=10 & trips$ORIG_DEP_BIN<=22, 2, trips$tod) # MD: 9 am - 3:30 pm
trips$tod <- ifelse(trips$ORIG_DEP_BIN>=23 & trips$ORIG_DEP_BIN<=29, 3, trips$tod) # PM: 3:30 pm - 7 pm
trips$tod <- ifelse(trips$ORIG_DEP_BIN>=30 & trips$ORIG_DEP_BIN<=40, 4, trips$tod) # EV: 7 pm - 3 am
trips$tod <- ifelse(is.na(trips$tod),5,trips$tod)
trips$num_trips <- trips$finalweight

jtrips$tod <- 5 # EA: 3 am - 6 am
jtrips$tod <- ifelse(jtrips$ORIG_DEP_BIN>=4 & jtrips$ORIG_DEP_BIN<=9, 1, jtrips$tod)   # AM: 6 am - 9 am
jtrips$tod <- ifelse(jtrips$ORIG_DEP_BIN>=10 & jtrips$ORIG_DEP_BIN<=22, 2, jtrips$tod) # MD: 9 am - 3:30 pm
jtrips$tod <- ifelse(jtrips$ORIG_DEP_BIN>=23 & jtrips$ORIG_DEP_BIN<=29, 3, jtrips$tod) # PM: 3:30 pm - 7 pm
jtrips$tod <- ifelse(jtrips$ORIG_DEP_BIN>=30 & jtrips$ORIG_DEP_BIN<=40, 4, jtrips$tod) # EV: 7 pm - 3 am
jtrips$tod <- ifelse(is.na(jtrips$tod),5,jtrips$tod)
#jtrips$num_trips <- jtrips$finalweight # * jtrips$NUMBER_HH
jtrips$num_trips <- jtrips$finalweight * jtrips$NUMBER_HH

itrips_summary <- aggregate(num_trips~tod+TOURPURP+TOURMODE+TRIPMODE, data=trips[trips$FULLY_JOINT==0 & trips$TOURPURP!=3,], FUN=sum)
jtrips_summary <- aggregate(num_trips~tod+TOURPURP+TOURMODE+TRIPMODE, data=jtrips[jtrips$TOURPURP!=3,], FUN=sum)

write.csv(itrips_summary, "itrips_tripmode_summary.csv", row.names = F)
write.csv(jtrips_summary, "jtrips_tripmode_summary.csv", row.names = F)
###########

# Total number of stops, trips & tours
cat("Total number of stops : ", sum(stops$finalweight[stops$FULLY_JOINT==0]) + sum(jstops$finalweight))
cat("Total number of trips : ", sum(trips$finalweight[trips$FULLY_JOINT==0]) + sum(jtrips$finalweight))
cat("Total number of tours : ", sum(tours$finalweight[tours$TOURPURP<=10]))

# output total numbers in a file
total_population <- sum(pertypeDistbn$freq)
total_households <- sum(hh$finalweight)
total_tours <- sum(tours$finalweight[(tours$TOURPURP<=9 | tours$TOURPURP==20)]) #updated by nagendra.dhakar@rsginc.com, 01/30/2017, add subtours as well
total_trips <- sum(trips$finalweight[trips$FULLY_JOINT==0]) + sum(jtrips$finalweight)
total_stops <- sum(stops$finalweight[stops$FULLY_JOINT==0]) + sum(jstops$finalweight)
total_population_for_rates <- sum(perday$finalweight_1, na.rm=TRUE)

trips$num_travel[trips$TRIPMODE==1] <- 1
trips$num_travel[trips$TRIPMODE==2] <- 2
trips$num_travel[trips$TRIPMODE==3] <- 3.5
trips$num_travel[is.na(trips$num_travel)] <- 0

total_vmt <- sum((trips$finalweight[trips$TRIPMODE>0 & trips$TRIPMODE<=3 & !is.na(trips$od_dist)]*trips$od_dist[trips$TRIPMODE>0 & trips$TRIPMODE<=3 & !is.na(trips$od_dist)])/trips$num_travel[trips$TRIPMODE>0 & trips$TRIPMODE<=3 & !is.na(trips$od_dist)])


totals_var <- c("total_population", "total_households", "total_tours", "total_trips", "total_stops", "total_vmt", "total_population_for_rates")
totals_val <- c(total_population,total_households, total_tours, total_trips, total_stops, total_vmt, total_population_for_rates)

totals_df <- data.frame(name = totals_var, value = totals_val)

write.csv(totals_df, "totals.csv", row.names = F)

# HH Size distribution
hhSizeDist <- plyr::count(hh[!is.na(hh$HHSIZE),], c("HHSIZE"), "finalweight")
write.csv(hhSizeDist, "hhSizeDist.csv", row.names = F)

# Active Persons by person type
actpertypeDistbn <- plyr::count(perday[(!is.na(perday$PERTYPE) & (perday$DAP!="H")),], c("PERTYPE"), "finalweight_1")
write.csv(actpertypeDistbn, "activePertypeDistbn.csv", row.names = TRUE)



# write out files for debugging
write.csv(hh, "hh_debug.csv", row.names = F)
write.csv(per, "per_debug.csv", row.names = F)
write.csv(hhday, "hhday_debug.csv", row.names = F)
write.csv(perday, "perday_debug.csv", row.names = F)
write.csv(place, "place_debug.csv", row.names = F)
write.csv(tours, "tours_debug.csv", row.names = F)
write.csv(trips, "trips_debug.csv", row.names = F)
write.csv(jtours, "jtours_debug.csv", row.names = F)
write.csv(jtrips, "jtrips_debug.csv", row.names = F)
write.csv(jutrips, "jutrips_debug.csv", row.names = F)


### write out trip files for creating trip departure/arrival lookups
write.csv(trips, "trips.csv", row.names = F)
write.csv(jtrips, "jtrips.csv", row.names = F)


### IE/EI trip mode distrribution
#   IE/EI trips defined as trips with a missing TAZ but non-missing coordinate
#trips_ie <- trips[(((is.na(trips$DTAZ) | (trips$DTAZ>0 & trips$DTAZ<=12)) & trips$OTAZ>0 & !(trips$OTAZ<=12)) | 
#                  ((is.na(trips$OTAZ) | (trips$OTAZ>0 & trips$OTAZ<=12)) & trips$DTAZ>0 & !(trips$DTAZ<=12))) & 
#                  (!is.na(trips$ORIG_X) & !is.na(trips$ORIG_Y) & !is.na(trips$DEST_X) & !is.na(trips$DEST_Y)),]
#
trips_ie <- trips
trips_ie$dest_ext <- 0
trips_ie$dest_ext[!is.na(trips_ie$DTAZ) & (trips$DTAZ>0 & trips$DTAZ<=12)] <- 1
trips_ie$dest_ext[is.na(trips_ie$DTAZ) & (!is.na(trips$DEST_X) & !is.na(trips$DEST_Y))] <- 1

trips_ie$orig_ext <- 0
trips_ie$orig_ext[!is.na(trips_ie$OTAZ) & (trips$OTAZ>0 & trips$OTAZ<=12)] <- 1
trips_ie$orig_ext[is.na(trips_ie$OTAZ) & (!is.na(trips$ORIG_X) & !is.na(trips$ORIG_Y))] <- 1

trips_ie <- trips_ie[((trips_ie$orig_ext==0 & trips_ie$dest_ext==1) | (trips_ie$orig_ext==1 & trips_ie$dest_ext==0)), ]

#keep only trips from valid tours
trips_ie <- trips_ie[trips_ie$TOURPURP %in% c(1,2,3,4,5,6,7,8,9,10, 13),]

# remove all walk trips tagged as IE trips (all ocean trips or reporting error in mode)
trips_ie <- trips_ie[!trips_ie$TRIPMODE==4,]

#View(trips_ie[trips_ie$TOURPURP==13,c("HH_ID", "PER_ID", "TOUR_ID", "TRIP_ID","OTAZ", "ORIG_X", "ORIG_Y", "DTAZ" ,"DEST_X", "DEST_Y", "TRIPMODE", "ORIG_PURP", "DEST_PURP", "TOURPURP", "finalweight")])
out <- trips_ie[(trips_ie$TOURPURP==13),c("HH_ID", "PER_ID", "TOUR_ID", "TRIP_ID","OTAZ", "ORIG_X", "ORIG_Y", "DTAZ" ,"DEST_X", "DEST_Y", "TRIPMODE", "ORIG_PURP", "DEST_PURP", "TOURPURP", "finalweight")]
write.csv(out, "out_13.csv", row.names = F)

tripmode_ie <- wtd.hist(trips_ie$TRIPMODE[!is.na(trips_ie$TRIPMODE) & trips_ie$TRIPMODE>0 & trips_ie$TRIPMODE<=9], breaks = seq(1,12, by=1), freq = NULL, right=FALSE, weight = trips_ie$finalweight[!is.na(trips_ie$TRIPMODE) & trips_ie$TRIPMODE>0 & trips_ie$TRIPMODE<=9])
write.csv(tripmode_ie$counts, "tripmode_ie.csv", row.names = F)


