#######################################################
### Script for summarizing SANDAG HTS
### Author: Binny M Paul, binny.paul@rsginc.com
### Sep 2017
### Copy of OHAS processing script
#######################################################
rm(list=ls())
oldw <- getOption("warn")
options(warn = -1)
options(scipen=999)

## Libraries
############

library(plyr)
library(weights)
library(reshape)
library(data.table)
library(stringr)
library(geosphere)
library(dplyr)

#### User Inputs ####
# Directories
# WD: Visualizer Output folder
# Survey_Dir: HH, Person, place data (by day) - SPA inputs
# Survey_Processed_Dir: Spa outputs (in folders by day)
# skims_dir: Skim data folder
# SHP_Dir: TAZ shapefile folder
# WeightsDir: Weights
# xwalkDir: XWalk folder

WD                   <- "C:\\gitclones\\Dubai_survey_processing"
RawSurvey_Dir        <- "data\\raw"
Survey_Dir           <- "data\\preprocessed"
Survey_Processed_Dir <- "data\\spa_output"
WeightsDir           <- "data\\weights"
outdir               <- "data\\visualizer\\summaries"
setwd(WD)

# skims_dir            <- "C:\\Users\\andrew.rohne\\OneDrive - Resource Systems Group, Inc\\SANDAG_Viz\\visualizer\\data\\skims"
# SHP_Dir              <- "C:\\Users\\andrew.rohne\\OneDrive - Resource Systems Group, Inc\\SANDAG_Viz\\visualizer\\data\\SHP"
# xwalkDir             <- "C:\\Users\\andrew.rohne\\OneDrive - Resource Systems Group, Inc\\SANDAG_Viz\\visualizer\\data\\SHP"
# skims_filename = "skims.omx"

## Read Data
# xwalk                <- read.csv(paste(xwalkDir, "TAZ_Dist_xwalk.csv", sep = "/"), as.is = T)
# colnames(xwalk) = c("TAZ", "pmsa")
# districtList         <- sort(unique(xwalk$pmsa))
hh      = fread(file.path(Survey_Dir, "household.csv"))
per     = fread(file.path(Survey_Dir, "person.csv"))
day     = fread(file.path(RawSurvey_Dir, "day.csv"))
place   = fread(file.path(Survey_Dir, "place.csv"))
trips_pre   = fread(file.path(Survey_Dir, "trip.csv"))

processedPerson = fread(file.path(Survey_Processed_Dir, "persons.csv"))
tours   = fread(file.path(Survey_Processed_Dir, "tours.csv"))
trips   = fread(file.path(Survey_Processed_Dir, "trips.csv"))
jtours  = fread(file.path(Survey_Processed_Dir, "unique_joint_tours.csv"))
jutrips = fread(file.path(Survey_Processed_Dir, "unique_joint_ultrips.csv"))



zones = fread(file.path(RawSurvey_Dir, 'zone.csv'))
raw_trips = fread(file.path(RawSurvey_Dir, 'trip.csv'))


# Code book
codebook_categories = fread(file.path('configs', 'db_specs', 'codebook_categories.csv'))
# codebook_dictionary = fread(file.path('configs', 'db_specs', 'codebook_dictionary.csv'))
codebook_dictionary = fread(file.path('configs', 'preprocess_specs', 'preprocess_mapping.csv'))[,!'Expression']

# Create faux xwalk
zones[ , NAME_GROUP := tstrsplit(NAME,'_\\(')[1]]
xwalk = zones[ , .(zone_id, OLDZONE, NEWZONE, NAME, NAME_GROUP)]
districtList = unique(xwalk$NAME_GROUP)

#### Create faux skims ####
zone_ids = unique(zones$OLDZONE)[!is.na(unique(zones$OLDZONE))]

# Capture as many distances from the survey as possible
DST_SKM = copy(place)
DST_SKM[ , (c('o','o_X', 'o_Y')) := data.table::shift(.(TAZ, XCORD, YCORD)), by=.(SAMPN, PERNO, DAYNO)]
DST_SKM[ , (c('d', 'd_X', 'd_Y')) := .(TAZ, XCORD, YCORD)]
DST_SKM = unique(DST_SKM[!is.na(o_X),  .(o, d, o_X, o_Y, d_X, d_Y, DISTANCE)])
DST_SKM = DST_SKM[, lapply(.SD, mean), by=.(o,d), .SDcols=c('DISTANCE', 'o_X', 'o_Y', 'd_X', 'd_Y')]


# Pull in all possible combinations & fill in the missing ones using straight distance
DST_SKM = DST_SKM[as.data.table(expand.grid(o=zone_ids, d=zone_ids)), on=.(o, d)]

DST_SKM_NA = DST_SKM[is.na(DISTANCE)]
DST_SKM_NA[zones, on=.(o==OLDZONE), `:=` (o_X=i.X, o_Y=i.Y)]
DST_SKM_NA[zones, on=.(d==OLDZONE), `:=` (d_X=i.X, d_Y=i.Y)]
DST_SKM_NA[ , DISTANCE := distHaversine(DST_SKM_NA[,.(o_X, o_Y)], DST_SKM_NA[,.(d_X, d_Y)]) / 1000]
DST_SKM = rbind(DST_SKM[!is.na(DISTANCE),], DST_SKM_NA)
DST_SKM[duplicated(DST_SKM[,.(o,d)])]
setnames(DST_SKM, 'DISTANCE','dist')

#
#### setup stuff? ####
setnames(day, 'household_id', 'hh_id')
ndays = max(day[, .N, by = .(hh_id, person_num, day_date)]$N)

# colnames(perday)[which(colnames(perday) == "HH_ID")] = "hh_id"
# colnames(perday)[which(colnames(perday) == "PER_ID")] = "person_num"
perday = copy(day)
perday$day_num = 1


# Filter records to use only completed households and persons on weekdays
#day = day[day$hh_day_complete == 1 & day$travel_dow >= 1 & day$travel_dow <= 5,]
#day <- day[day$day_iscomplete==1 & day$day_hhcomplete==1 & day$travel_dow>=1 & day$travel_dow<=5,]
#day <- day[day$day_iscomplete==1 & day$day_hhcomplete==1 & day$travel_dow==1,]

# Generate person-day weigths for all travel database
# multi day weigths are unique across all trips for a person-day, therefore simple aggregation yields person-day weights
#multiDayWeights <- unique(multiDayWeights[,c("hhid", "pernum", "daynum", "adjusted_multiday_tripweight_456x")])

# create hh_day and person_day tables
hhday <- unique(perday[,c("hh_id", "day_num")])
hhday <- cbind(hhday, hh[match(hhday$hh_id, hh$SAMPN), -which(names(hhday) %in% c("hh_id"))])


hh$HHEXPFAC = hh$HH_WEIGHT
per$PEREXPFAC = per$PER_WEIGHT
#ASR: hh and person weights good here

perday$puid <- paste(perday$hh_id, perday$person_num, perday$day_num, sep = "-")
perday$uid <- paste(perday$hh_id, perday$day_num, sep = "-")
perday$day_id = paste(10000 * perday$hh_id + 100 * perday$person_num + perday$day_num)
perday[per, on = .(person_id), per_weight := i.PER_WEIGHT]



per_day_wts = aggregate(day_num ~ hh_id + person_num, data = perday, length)
colnames(per_day_wts)[which(colnames(per_day_wts) == "day_num")] = "days"
perday$obs_days = per_day_wts$days[match(perday$hh_id * 100 + perday$person_num, per_day_wts$hh_id * 100 + per_day_wts$person_num)]
perday$finalweight = perday$per_weight / perday$obs_days

per$hhperid = 100 * per$SAMPN + per$PERNO
perday$hhperid = perday$hh_id * 100 + perday$person_num


hh_day_wts = aggregate(day_num ~ hh_id, data = hhday, length)
colnames(hh_day_wts)[which(colnames(hh_day_wts) == "day_num")] = "days"

hhday$obs_days = hh_day_wts$days[match(hhday$hh_id, hh_day_wts$hh_id)]


hhday[hh, on=.(hh_id=household_id), HHEXPFAC := i.HHEXPFAC]
hhday$finalweight = hhday$HHEXPFAC / hhday$obs_days


place$puid <- paste(place$SAMPN, place$PERNO, "1", sep = "-")
place[ , DAY_NUM := DAYNO]


# day 1 data
dayno                <- 1
tours$DAYNO          <- dayno
trips$DAYNO          <- dayno
jtours$DAYNO         <- dayno
jutrips$DAYNO         <- dayno


# puid
tours$puid           <- paste(tours$HH_ID, tours$PER_ID, tours$DAYNO, sep = "-")
trips$puid           <- paste(trips$HH_ID, trips$PER_ID, trips$DAYNO, sep = "-")
jtours$uid           <- paste(jtours$HH_ID, jtours$DAYNO, sep = "-")
jutrips$uid           <- paste(jutrips$HH_ID, jutrips$DAYNO, sep = "-")

print(paste("Read", nrow(tours), "tours and ", nrow(trips), "trips for day 1"))
#filter
tours <- tours[tours$puid %in% perday$puid,]
trips <- trips[trips$puid %in% perday$puid,]
jtours <- jtours[jtours$uid %in% unique(perday$uid), ]
jutrips <- jutrips[jutrips$uid %in% unique(perday$uid), ]

print(paste("Retained", nrow(tours), "tours and ", nrow(trips), "trips for day 1"))


# replace all "nan" in Python outputs with 0
tours = tours[, lapply(.SD, function(x) ifelse(x=='nan',0,x))]
trips = trips[, lapply(.SD, function(x) ifelse(x=='nan',0,x))]
jtours = jtours[, lapply(.SD, function(x) ifelse(x=='nan',0,x))]
jutrips = jutrips[, lapply(.SD, function(x) ifelse(x=='nan',0,x))]


trips$JTRIP_ID <- as.numeric(trips$JTRIP_ID)
trips$OMAZ = 0
trips$DMAZ = 0
tours$OMAZ = 0
tours$DMAZ = 0


#### TOUR MODE UPDATES ####
#tours$TOURMODE[tours$TOURMODE>=1 & tours$TOURMODE<=2] <- 1
#tours$TOURMODE[tours$TOURMODE>=3 & tours$TOURMODE<=4] <- 2
#tours$TOURMODE[tours$TOURMODE>=5 & tours$TOURMODE<=6] <- 3
#tours$TOURMODE[tours$TOURMODE==7] <- 4
#tours$TOURMODE[tours$TOURMODE==8] <- 5
# tours$TOURMODE[tours$TOURMODE>=6 & tours$TOURMODE<=8] <- 6 # Walk Transit
# tours$TOURMODE[tours$TOURMODE>=9 & tours$TOURMODE<=11] <- 7 #PNR Transit
# tours$TOURMODE[tours$TOURMODE>=12 & tours$TOURMODE<=14] <- 8 #KNR Transit
# tours$TOURMODE[tours$TOURMODE>=15 & tours$TOURMODE<=17] <- 9 #TNC Transit
# tours$TOURMODE[tours$TOURMODE == 20] <- 10 # Taxi
# tours$TOURMODE[tours$TOURMODE == 18] <- 11 # TNC-Single
# tours$TOURMODE[tours$TOURMODE == 18] <- 12 # TNC-Shared
# tours$TOURMODE[tours$TOURMODE>=21] <- 13

# Compare person type across days
# Person types are recoded by Python script based on activity purposes reported for each day


trips$finalweight = place$TRIP_WEIGHT[match(paste(trips$HH_ID, trips$PER_ID, trips$DAYNO, trips$DEST_PLACENO, sep = "-"), 
                                            paste(place$SAMPN, place$PERNO, place$DAY_NUM, place$PLANO, sep = "-"))]

tour_weights = setDT(trips)[,.(tour_weight = mean(finalweight)), by = .(HH_ID, PER_ID, TOUR_ID, DAYNO)]
tours$finalweight = tour_weights$tour_weight[match(paste(tours$HH_ID, tours$PER_ID, tours$TOUR_ID, tours$DAYNO, sep = "-"),
                                                   paste(tour_weights$HH_ID, tour_weights$PER_ID, tour_weights$TOUR_ID, tour_weights$DAYNO, sep = "-"))]

hh$finalweight <- hh$HHEXPFAC
per$finalweight <- per$PEREXPFAC

perday$finalweight_1 = perday$finalweight

jutrips$finalweight = trips$finalweight[match(paste(jutrips$HH_ID, jutrips$JTRIP_ID, jutrips$DAYNO, sep = "-"), 
                                              paste(trips$HH_ID, trips$JTRIP_ID, trips$DAYNO, sep = "-"))]

jutrips$finalweight[is.na(jutrips$finalweight)] = 0

jtours$finalweight = tours$finalweight[match(paste(jtours$HH_ID, jtours$JTOUR_ID, jtours$DAYNO, sep = "-"),
                                                    paste(tours$HH_ID, tours$JTOUR_ID, tours$DAYNO, sep = "-"))]

trips$finalweight[is.na(trips$finalweight)] = 0
tours$finalweight[is.na(tours$finalweight)] = 0
jtours$finalweight[is.na(jtours$finalweight)] = 0
jutrips$finalweight[is.na(jutrips$finalweight)] = 0

#### Debugging ####
#hts_trips$puid <- paste(hts_trips$hhid, hts_trips$pernum, hts_trips$daynum, sep = "-")
#write.csv(hts_trips, "hts_trips.csv", row.names = F)
#write.csv(trips, "trips.csv", row.names = F)

# Fix trip and tour times
tours$ANCHOR_DEPART_HOUR = tours$ANCHOR_DEPART_HOUR + 3
tours$ANCHOR_DEPART_HOUR[which(tours$ANCHOR_DEPART_HOUR >= 24)] = tours$ANCHOR_DEPART_HOUR[which(tours$ANCHOR_DEPART_HOUR >= 24)] - 24
tours$ANCHOR_DEPART_BIN = tours$ANCHOR_DEPART_BIN + 6
tours$ANCHOR_DEPART_BIN[which(tours$ANCHOR_DEPART_BIN > 48)] = tours$ANCHOR_DEPART_BIN[which(tours$ANCHOR_DEPART_BIN > 48)] - 48

tours$PRIMDEST_ARRIVE_HOUR = tours$PRIMDEST_ARRIVE_HOUR + 3
tours$PRIMDEST_ARRIVE_HOUR[which(tours$PRIMDEST_ARRIVE_HOUR >= 24)] = tours$PRIMDEST_ARRIVE_HOUR[which(tours$PRIMDEST_ARRIVE_HOUR >= 24)] - 24
tours$PRIMDEST_ARRIVE_BIN = tours$PRIMDEST_ARRIVE_BIN + 6
tours$PRIMDEST_ARRIVE_BIN[which(tours$PRIMDEST_ARRIVE_BIN > 48)] = tours$PRIMDEST_ARRIVE_BIN[which(tours$PRIMDEST_ARRIVE_BIN > 48)] - 48

tours$ANCHOR_ARRIVE_HOUR = tours$ANCHOR_ARRIVE_HOUR + 3
tours$ANCHOR_ARRIVE_HOUR[which(tours$ANCHOR_ARRIVE_HOUR >= 24)] = tours$ANCHOR_ARRIVE_HOUR[which(tours$ANCHOR_ARRIVE_HOUR >= 24)] - 24
tours$ANCHOR_ARRIVE_BIN = tours$ANCHOR_ARRIVE_BIN + 6
tours$ANCHOR_ARRIVE_BIN[which(tours$ANCHOR_ARRIVE_BIN > 48)] = tours$ANCHOR_ARRIVE_BIN[which(tours$ANCHOR_ARRIVE_BIN > 48)] - 48

tours$PRIMDEST_DEPART_HOUR = tours$PRIMDEST_DEPART_HOUR + 3
tours$PRIMDEST_DEPART_HOUR[which(tours$PRIMDEST_DEPART_HOUR >= 24)] = tours$PRIMDEST_DEPART_HOUR[which(tours$PRIMDEST_DEPART_HOUR >= 24)] - 24
tours$PRIMDEST_DEPART_BIN = tours$PRIMDEST_DEPART_BIN + 6
tours$PRIMDEST_DEPART_BIN[which(tours$PRIMDEST_DEPART_BIN > 48)] = tours$PRIMDEST_DEPART_BIN[which(tours$PRIMDEST_DEPART_BIN > 48)] - 48


trips$ORIG_ARR_HR = trips$ORIG_ARR_HR + 3
trips$ORIG_ARR_HR[which(trips$ORIG_ARR_HR >= 24)] = trips$ORIG_ARR_HR[which(trips$ORIG_ARR_HR >= 24)] - 24
trips$ORIG_ARR_BIN = trips$ORIG_ARR_BIN + 6
trips$ORIG_ARR_BIN[which(trips$ORIG_ARR_BIN > 48)] = trips$ORIG_ARR_BIN[which(trips$ORIG_ARR_BIN > 48)] - 48

trips$ORIG_DEP_HR = trips$ORIG_DEP_HR + 3
trips$ORIG_DEP_HR[which(trips$ORIG_DEP_HR >= 24)] = trips$ORIG_DEP_HR[which(trips$ORIG_DEP_HR >= 24)] - 24
trips$ORIG_DEP_BIN = trips$ORIG_DEP_BIN + 6
trips$ORIG_DEP_BIN[which(trips$ORIG_DEP_BIN > 48)] = trips$ORIG_DEP_BIN[which(trips$ORIG_DEP_BIN > 48)] - 48

trips$DEST_ARR_HR = trips$DEST_ARR_HR + 3
trips$DEST_ARR_HR[which(trips$DEST_ARR_HR >= 24)] = trips$DEST_ARR_HR[which(trips$DEST_ARR_HR >= 24)] - 24
trips$DEST_ARR_BIN = trips$DEST_ARR_BIN + 6
trips$DEST_ARR_BIN[which(trips$DEST_ARR_BIN > 48)] = trips$DEST_ARR_BIN[which(trips$DEST_ARR_BIN > 48)] - 48
  
trips$DEST_DEP_HR = trips$DEST_DEP_HR + 3
trips$DEST_DEP_HR[which(trips$DEST_DEP_HR >= 24)] = trips$DEST_DEP_HR[which(trips$DEST_DEP_HR >= 24)] - 24
trips$DEST_DEP_BIN = trips$DEST_DEP_BIN + 6
trips$DEST_DEP_BIN[which(trips$DEST_DEP_BIN > 48)] = trips$DEST_DEP_BIN[which(trips$DEST_DEP_BIN > 48)] - 48

#### Prepare files for computing summary statistics ####
## rename variables in SANDAG HTS to standard format in this script
names(hh)[names(hh)=="HH_ZONE_ID"] <- 'HHTAZ'
names(hhday)[names(hhday)=="HH_ZONE_ID"] <- 'HHTAZ'
#names(hh)[names(hh)=="HOME_MAZ"] <- 'HHMAZ'
#names(hhday)[names(hhday)=="HOME_MAZ"] <- 'HHMAZ'
hh$HHMAZ = 0
hhday$HHMAZ = 0

names(per)[names(per)=="PER_WK_ZONE_ID"] <- 'WTAZ'
perday$WTAZ = per$WTAZ[match(paste(perday$hh_id, perday$person_num, sep = "-"),
                             paste(per$SAMPN, per$PERNO, sep = "-"))]
#names(perday)[names(perday)=="PER_WK_ZONE_ID"] <- 'WTAZ'
#names(per)[names(per)=="WORK_MAZ"] <- 'WMAZ'
#names(perday)[names(perday)=="WORK_MAZ"] <- 'WMAZ'
per$WMAZ = 0
perday$WMAZ = 0

names(per)[names(per)=="PER_SCHL_ZONE_ID"] <- 'STAZ'
perday$STAZ = per$STAZ[match(paste(perday$hh_id, perday$person_num, sep = "-"),
                             paste(per$SAMPN, per$PERNO, sep = "-"))]
#names(perday)[names(perday)=="PER_SCHL_ZONE_ID"] <- 'STAZ'
#names(per)[names(per)=="SCHOOL_MAZ"] <- 'SMAZ'
#names(perday)[names(perday)=="SCHOOL_MAZ"] <- 'SMAZ'
per$SMAZ = 0
perday$SMAZ = 0

hh$HHTAZ[is.na(hh$HHTAZ)] <- 0
hh$HHMAZ[is.na(hh$HHMAZ)] <- 0
hhday$HHTAZ[is.na(hhday$HHTAZ)] <- 0
hhday$HHMAZ[is.na(hhday$HHMAZ)] <- 0

per$WTAZ[is.na(per$WTAZ)] <- 0
per$WMAZ[is.na(per$WMAZ)] <- 0
perday$WTAZ[is.na(perday$WTAZ)] <- 0
perday$WMAZ[is.na(perday$WMAZ)] <- 0

per$STAZ[is.na(per$STAZ)] <- 0
per$SMAZ[is.na(per$SMAZ)] <- 0
perday$STAZ[is.na(perday$STAZ)] <- 0
perday$SMAZ[is.na(perday$SMAZ)] <- 0

# names(processedPerson)[names(processedPerson)=="HH_ID"] <- "HHID"
# names(processedPerson)[names(processedPerson)=="PER_ID"] <- "PERID"

hh$HHVEH[hh$HH_VEH == 0] <- 0
hh$HHVEH[hh$HH_VEH == 1] <- 1
hh$HHVEH[hh$HH_VEH == 2] <- 2
hh$HHVEH[hh$HH_VEH == 3] <- 3
hh$HHVEH[hh$HH_VEH >= 4] <- 4

hh$HHSIZE[hh$HH_SIZE == 1] <- 1
hh$HHSIZE[hh$HH_SIZE == 2] <- 2
hh$HHSIZE[hh$HH_SIZE == 3] <- 3
hh$HHSIZE[hh$HH_SIZE == 4] <- 4
hh$HHSIZE[hh$HH_SIZE >= 5] <- 5

#Adults in the HH
adults <- plyr::count(per[!is.na(per$AGE_CAT),], c("SAMPN"), "AGE_CAT>=4")
hh$ADULTS <- adults$freq[match(hh$SAMPN, adults$SAMPN)]
hh$ADULTS[is.na(hh$ADULTS)] <- 0

# define Districts
hh[xwalk, on = .(HHTAZ=OLDZONE), HDISTRICT := i.NAME_GROUP]

#per$PERTYPE <- processedPerson$ptype[match(per$SAMPN*100+per$PERNO, processedPerson$HHID*100+processedPerson$PERID)]
per$HHTAZ <- hh$HHTAZ[match(per$SAMPN, hh$SAMPN)]
per$HHMAZ <- hh$HHMAZ[match(per$SAMPN, hh$SAMPN)]
per$HDISTRICT <- hh$HDISTRICT[match(per$SAMPN, hh$SAMPN)]

per[xwalk, on = .(WTAZ=OLDZONE), WDISTRICT := i.NAME_GROUP]


per$XCORD <- hh$HXCORD[match(per$SAMPN, hh$SAMPN)]
per$YCORD <- hh$HYCORD[match(per$SAMPN, hh$SAMPN)]
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
#perday$PERTYPE   <- per$PERTYPE[match(paste(perday$SAMPN, perday$PERNO, sep = "-"), 
#                                      paste(per$SAMPN, per$PERNO, sep = "-"))]

perday$HHTAZ     <- per$HHTAZ[match(perday$SAMPN*100+perday$PERNO, per$SAMPN*100+per$PERNO)]
perday$XCORD     <- per$XCORD[match(perday$SAMPN*100+perday$PERNO, per$SAMPN*100+per$PERNO)]
perday$YCORD     <- per$YCORD[match(perday$SAMPN*100+perday$PERNO, per$SAMPN*100+per$PERNO)]
perday$HDISTRICT <- per$HDISTRICT[match(perday$SAMPN*100+perday$PERNO, per$SAMPN*100+per$PERNO)]
perday$WDISTRICT <- per$WDISTRICT[match(perday$SAMPN*100+perday$PERNO, per$SAMPN*100+per$PERNO)]

#### Compute Summary Statistics ####

# Define other variables
pertypeCodes = with(codebook_categories[grepl('Person type', Description)],
                     data.table(code=Values,
                                name=Labels))

# pertypeCodes = rbind(pertypeCodes, data.table(code=c('All',0), name=c('All','All')))


pertypeCodes = rbind(pertypeCodes, data.table(code=c(0, 'All'), name=c('NA','All')))
per[PERTYPE %in% c(23,24),]

# pertypeCodes <- data.frame(code = c(1,2,3,4,5,6,7,8,"All"), 
#                            name = c("FT Worker", "PT Worker", "Univ Stud", "Non Worker", "Retiree", "Driv Stud", "NonDriv Stud", "Pre-School", "All"))


# Auto ownership
autoOwnership <- plyr::count(hh[!is.na(hh$HHVEH),], c("HHVEH"), "finalweight")
fwrite(autoOwnership, file.path(outdir,"autoOwnership.csv"), row.names = FALSE)


perday[per, on = .(person_id), PERTYPE := i.PERTYPE]
pertypeDistbn <- plyr::count(perday, c("PERTYPE"), "finalweight")
fwrite(pertypeDistbn, file.path(outdir,"pertypeDistbn.csv"), row.names = FALSE)

perday$PER_EMPLY_LOC_TYPE = per$PER_EMPLY_LOC_TYPE[match(paste(perday$SAMPN, perday$PERNO, sep = "-"),
                                                         paste(per$SAMPN, per$PERNO, sep = "-"))]

# Mandatory DC
perday[per, on = .(person_id), c('PER_EMPLY_LOC_TYPE', 'EMPLY') := .(i.EMPLY_LOC_TYPE,i.EMPLY)]

# Get dist to work
workers <- perday[perday$WTAZ>0 & perday$EMPLY<=2 & perday$PER_EMPLY_LOC_TYPE!=3,
                  c("SAMPN", "PERNO", "HHTAZ", "WTAZ", "PER_EMPLY_LOC_TYPE","PERTYPE", "HDISTRICT", "WDISTRICT", "finalweight")]
wxydist = trips_pre[hh, on='SAMPN', .(SAMPN, PERNO, HXCORD, HYCORD, WXCORD, WYCORD)]
wxydist = unique(wxydist[workers[,.(SAMPN, PERNO)], on=.(SAMPN, PERNO), ])

# Fix missing cords
xymissing = wxydist[is.na(HXCORD) | is.na(HYCORD) | is.na(WXCORD) | is.na(WYCORD)]
hxyupdates = zones[workers[xymissing, on=.(SAMPN, PERNO)], on=.(OLDZONE==HHTAZ)][,.(SAMPN, PERNO, X, Y)]
wxydist[hxyupdates, on=.(SAMPN,PERNO), `:=` (HXCORD=i.X, HYCORD=i.Y)]
stopifnot(nrow(wxydist[is.na(HXCORD) | is.na(HYCORD) | is.na(WXCORD) | is.na(WYCORD)])==0)

# get dist
wxydist$WDIST = distHaversine(wxydist[, .(HXCORD, HYCORD)], wxydist[, .(WXCORD, WYCORD)]) / 1000
workers = workers[wxydist, on=.(SAMPN, PERNO)]
# workers$WDIST <- DST_SKM$dist[match(paste(workers$HHTAZ, workers$WTAZ, sep = "-"), paste(DST_SKM$o, DST_SKM$d, sep = "-"))]



# Get dist to school
students <- perday[perday$STAZ>0, c("SAMPN", "PERNO", "HHTAZ", "STAZ", "PERTYPE", "HDISTRICT", "finalweight")]
sxydist = trips_pre[hh, on='SAMPN', .(SAMPN, PERNO, HXCORD, HYCORD, SXCORD, SYCORD)]
sxydist = unique(sxydist[students[,.(SAMPN, PERNO)], on=.(SAMPN, PERNO), ])

# Fix missing cords
xymissing = sxydist[is.na(HXCORD) | is.na(HYCORD) | is.na(SXCORD) | is.na(SYCORD)]
hxyupdates = zones[students[xymissing, on=.(SAMPN, PERNO)], on=.(OLDZONE==HHTAZ)][,.(SAMPN, PERNO, X, Y)]
sxyupdates = zones[students[xymissing, on=.(SAMPN, PERNO)], on=.(OLDZONE==STAZ)][,.(SAMPN, PERNO, X, Y)]

sxydist[hxyupdates, on=.(SAMPN,PERNO), `:=` (HXCORD=i.X, HYCORD=i.Y)]
sxydist[sxyupdates, on=.(SAMPN,PERNO), `:=` (SXCORD=i.X, SYCORD=i.Y)]

stopifnot(nrow(sxydist[is.na(HXCORD) | is.na(HYCORD) | is.na(SXCORD) | is.na(SYCORD)])==0)

# Get dist
sxydist$SDIST = distHaversine(sxydist[, .(SXCORD, HYCORD)], sxydist[, .(SXCORD, SYCORD)]) / 1000 # Meters to km
students = students[sxydist, on=.(SAMPN, PERNO)]
# students$SDIST <- DST_SKM$dist[match(paste(students$HHTAZ, students$STAZ, sep = "-"), paste(DST_SKM$o, DST_SKM$d, sep = "-"))]


# code distance bins
workers$distbin <- cut(workers$WDIST, breaks = c(seq(0,50, by=1), 9999), labels = F, right = F)
students$distbin <- cut(students$SDIST, breaks = c(seq(0,50, by=1), 9999), labels = F, right = F)
fwrite(workers, file.path(outdir,"workers_tlfd.csv"), row.names = FALSE)


# district stuff
distBinCat <- data.frame(distbin = seq(1,51, by=1))
districtList_df <- data.frame(id = districtList)


# compute TLFDs by district and total
tlfd_work <- ddply(workers[,c("HDISTRICT", "distbin", "finalweight")], c("HDISTRICT", "distbin"), summarise, work = sum((HDISTRICT>0)*finalweight))
tlfd_work <- cast(tlfd_work, distbin~HDISTRICT, value = "work", sum)
work_ditbins <- tlfd_work$distbin
tlfd_work <- transpose(tlfd_work[,!colnames(tlfd_work) %in% c("distbin")], keep.names = 'id')
# tlfd_work$id <- row.names(tlfd_work)
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
tlfd_univ <- transpose(tlfd_univ[,!colnames(tlfd_univ) %in% c("distbin")], keep.names = 'id')
# tlfd_univ$id <- row.names(tlfd_univ)
tlfd_univ <- merge(x = districtList_df, y = tlfd_univ, by = "id", all.x = TRUE)
tlfd_univ[is.na(tlfd_univ)] <- 0
tlfd_univ <- transpose(tlfd_univ[,!colnames(tlfd_univ) %in% c("id")])
tlfd_univ <- cbind(data.frame(distbin = univ_ditbins), tlfd_univ)
tlfd_univ$Total <- rowSums(tlfd_univ[,!colnames(tlfd_univ) %in% c("distbin")])
names(tlfd_univ) <- sub("V", "District_", names(tlfd_univ))
tlfd_univ_df <- merge(x = distBinCat, y = tlfd_univ, by = "distbin", all.x = TRUE)
tlfd_univ_df[is.na(tlfd_univ_df)] <- 0


tlfd_schl <- ddply(students[students$PERTYPE>=6,c("HDISTRICT", "distbin", "finalweight")], c("HDISTRICT", "distbin"), summarise, schl = sum((HDISTRICT>0)*finalweight))
tlfd_schl <- cast(tlfd_schl, distbin~HDISTRICT, value = "schl", sum)
schl_ditbins <- tlfd_schl$distbin
tlfd_schl <- transpose(tlfd_schl[,!colnames(tlfd_schl) %in% c("distbin")], keep.names = 'id')
# tlfd_schl$id <- row.names(tlfd_schl)
tlfd_schl <- merge(x = districtList_df, y = tlfd_schl, by = "id", all.x = TRUE)
tlfd_schl[is.na(tlfd_schl)] <- 0
tlfd_schl <- transpose(tlfd_schl[,!colnames(tlfd_schl) %in% c("id")])
tlfd_schl <- cbind(data.frame(distbin = schl_ditbins), tlfd_schl)
tlfd_schl$Total <- rowSums(tlfd_schl[,!colnames(tlfd_schl) %in% c("distbin")])
names(tlfd_schl) <- sub("V", "District_", names(tlfd_schl))
tlfd_schl_df <- merge(x = distBinCat, y = tlfd_schl, by = "distbin", all.x = TRUE)
tlfd_schl_df[is.na(tlfd_schl_df)] <- 0

fwrite(tlfd_work_df, file.path(outdir, "workTLFD.csv"), row.names = F)
fwrite(tlfd_univ_df, file.path(outdir, "univTLFD.csv"), row.names = F)
fwrite(tlfd_schl_df, file.path(outdir, "schlTLFD.csv"), row.names = F)

cat("\n Average distance to workplace (Total): ", weighted.mean(workers$WDIST, workers$finalweight, na.rm = TRUE))
cat("\n Average distance to university (Total): ", weighted.mean(students$SDIST[students$PERTYPE == 3], students$finalweight[students$PERTYPE == 3], na.rm = TRUE))
cat("\n Average distance to school (Total): ", weighted.mean(students$SDIST[students$PERTYPE >= 6 & students$PERTYPE <= 7], students$finalweight[students$PERTYPE >= 6 & students$PERTYPE <= 7], na.rm = TRUE))

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

schlTripLengths <- ddply(students[students$PERTYPE>=6,c("HDISTRICT", "SDIST", "finalweight")], c("HDISTRICT"), summarise, schl = weighted.mean(SDIST,finalweight))
totalLength     <- data.frame("Total", weighted.mean(students$SDIST[students$PERTYPE>=6], students$finalweight[students$PERTYPE>=6]))
colnames(totalLength) <- colnames(schlTripLengths)
schlTripLengths <- rbind(schlTripLengths, totalLength)
schlTripLengths_df <- merge(x = district_df, y = schlTripLengths, by = "HDISTRICT", all.x = TRUE)
schlTripLengths_df[is.na(schlTripLengths_df)] <- 0

mandTripLengths <- cbind(workTripLengths_df, univTripLengths_df$univ, schlTripLengths_df$schl)
colnames(mandTripLengths) <- c("District", "Work", "Univ", "Schl")

fwrite(mandTripLengths, file.path(outdir, "mandTripLengths.csv"), row.names = F)

# Work from home [for each district and total]
# perday$PERTYPE = perday$PERSONTYPE

#per$worker[per$PERTYPE<=2 | (per$PERTYPE==3 & !is.na(per$WTAZ))] <- 1
#per$worker[is.na(per$worker)] <- 0
per[EMPLY_LOC_TYPE==3, wfh := 1]
per$wfh[is.na(per$wfh)] <- 0
perday$worker[perday$PERTYPE<=2 | (perday$PERTYPE==3 & !is.na(perday$WTAZ))] <- 1
perday$worker[is.na(perday$worker)] <- 0
perday$wfh = per$wfh[match(paste(perday$SAMPN, perday$PERNO, sep = "-"),
                           paste(per$SAMPN, per$PERNO, sep = "-"))]
#perday$wfh[perday$PER_EMPLY_LOC_TYPE==3] <- 1
perday$wfh[is.na(perday$wfh)] <- 0

districtWorkers <- ddply(perday[perday$worker==1,c("HDISTRICT", "finalweight")], c("HDISTRICT"), summarise, workers = sum(finalweight))
districtWorkers_df <- merge(x = data.frame(HDISTRICT = districtList), y = districtWorkers, by = "HDISTRICT", all.x = TRUE)
districtWorkers_df[is.na(districtWorkers_df)] <- 0

districtWfh     <- ddply(perday[perday$worker==1 & perday$wfh==1,c("HDISTRICT", "finalweight")], c("HDISTRICT"), summarise, wfh = sum(finalweight))
districtWfh_df <- merge(x = data.frame(HDISTRICT = districtList), y = districtWfh, by = "HDISTRICT", all.x = TRUE)
districtWfh_df[is.na(districtWfh_df)] <- 0

wfh_summary     <- cbind(districtWorkers_df, districtWfh_df$wfh)
colnames(wfh_summary) <- c("District", "Workers", "WFH")
totalwfh        <- data.frame("Total", sum((perday$worker==1)*perday$finalweight), sum((perday$worker==1 & perday$wfh==1)*perday$finalweight))
colnames(totalwfh) <- colnames(wfh_summary)
wfh_summary <- rbind(wfh_summary, totalwfh)
write.csv(wfh_summary, file.path(outdir, "wfh_summary.csv"), row.names = F)

# County-County Flows
countyFlows <- xtabs(finalweight~HDISTRICT+WDISTRICT, data = workers[workers$finalweight > 0,])
countyFlows[is.na(countyFlows)] <- 0
countyFlows <- addmargins(as.table(countyFlows))
countyFlows <- as.data.frame.matrix(countyFlows)
colnames(countyFlows)[colnames(countyFlows)=="Sum"] <- "Total"
colnames(countyFlows) <- paste("District", colnames(countyFlows), sep = "_")
rownames(countyFlows)[rownames(countyFlows)=="Sum"] <- "Total"
rownames(countyFlows) <- paste("District", rownames(countyFlows), sep = "_")
fwrite(countyFlows, file.path(outdir, "countyFlows.csv"), row.names = T)

# trips$finalweight = perday$finalweight[match(paste(trips$HH_ID, trips$PER_ID, trips$DAYNO, sep = "-"),
#                                              paste(perday$SAMPN, perday$PERNO, perday$DAYNO, sep = "-"))]
# tours$finalweight = perday$finalweight[match(paste(tours$HH_ID, tours$PER_ID, tours$DAYNO, sep = "-"),
#                                              paste(perday$SAMPN, perday$PERNO, perday$DAYNO, sep = "-"))]
# 
# jtours$finalweight = hhday$finalweight[match(paste(jtours$HH_ID, jtours$DAYNO, sep = "-"),
#                                               paste(hhday$SAMPN, hhday$DAYNO, sep = "-"))]
# jutrips$finalweight = hhday$finalweight[match(paste(jutrips$HH_ID, jutrips$DAYNO, sep = "-"),
#                                                paste(hhday$SAMPN, hhday$DAYNO, sep = "-"))]



#### Process Tour file ####
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

tours$OTAZ <- tours$ORIG_TAZ #place$TAZ[match(tours$HH_ID*10000+tours$PER_ID*1000+tours$ORIG_PLACENO*10+tours$DAYNO, place$SAMPN*10000+place$PERNO*1000+place$PLANO*10+place$DAYNO)]
tours$DTAZ <- tours$DEST_TAZ #place$TAZ[match(tours$HH_ID*10000+tours$PER_ID*1000+tours$DEST_PLACENO*10+tours$DAYNO, place$SAMPN*10000+place$PERNO*1000+place$PLANO*10+place$DAYNO)]

tours[ , SKIMDIST := DIST]

# tours$SKIMDIST <- DST_SKM$dist[match(paste(tours$OTAZ, tours$DTAZ, sep = "-"), paste(DST_SKM$o, DST_SKM$d, sep = "-"))]

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


#### Change the tourpurpose on subtours to avoid being counted as normal tours ####
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
# jtours$DISTMILE <- jtours$DIST/5280/

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

# TRIP MODE UPDATES ####
#trips$TRIPMODE[trips$TRIPMODE>=1 & trips$TRIPMODE<=2] <- 1
#trips$TRIPMODE[trips$TRIPMODE>=3 & trips$TRIPMODE<=4] <- 2
#trips$TRIPMODE[trips$TRIPMODE>=5 & trips$TRIPMODE<=6] <- 3
#trips$TRIPMODE[trips$TRIPMODE==7] <- 4
#trips$TRIPMODE[trips$TRIPMODE==8] <- 5
# trips$TRIPMODE[trips$TRIPMODE>=7 & trips$TRIPMODE<=8] <- 6 # Walk Transit
# trips$TRIPMODE[trips$TRIPMODE>=9 & trips$TRIPMODE<=11] <- 7 #PNR Transit
# trips$TRIPMODE[trips$TRIPMODE>=12 & trips$TRIPMODE<=14] <- 8 #KNR Transit
# trips$TRIPMODE[trips$TRIPMODE>=15 & trips$TRIPMODE<=17] <- 9 #TNC Transit
# trips$TRIPMODE[trips$TRIPMODE == 20] <- 10 # Taxi
# trips$TRIPMODE[trips$TRIPMODE == 18] <- 11 # TNC-Single
# trips$TRIPMODE[trips$TRIPMODE == 18] <- 12 # TNC-Shared
# trips$TRIPMODE[trips$TRIPMODE>=21] <- 13

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
trips$stops <- 0
# trips$stops[trips$DEST_PURP>0 & trips$DEST_PURP<=10 & trips$DEST_IS_TOUR_DEST==0] <- 1
trips[trips$DEST_PURP>0 & trips$DEST_IS_TOUR_DEST==0, 'stops'] <- 1


trips$OTAZ = trips$ORIG_TAZ #<- place$TAZ[match(trips$HH_ID*10000+trips$PER_ID*1000+trips$ORIG_PLACENO*10+trips$DAYNO, place$SAMPN*10000+place$PERNO*1000+place$PLANO*10+place$DAYNO)]
trips$DTAZ = trips$DEST_TAZ # <- place$TAZ[match(trips$HH_ID*10000+trips$PER_ID*1000+trips$DEST_PLACENO*10+trips$DAYNO, place$SAMPN*10000+place$PERNO*1000+place$PLANO*10+place$DAYNO)]

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

trips[ , od_dist := DIST]
# trips$od_dist <- DST_SKM$dist[match(paste(trips$OTAZ, trips$DTAZ, sep = "-"), paste(DST_SKM$o, DST_SKM$d, sep = "-"))]
trips$od_dist[is.na(trips$od_dist)] <- 0
# 
# # Write out a trips data with select variable for other SANDAG application [for Joel, Oct 31st 2019]
# trips_subset <- trips[,c("HH_ID", "PER_ID", "DAYNO", "TRIP_ID", "SUBTOUR", "FULLY_JOINT", "TRIPMODE", "TOURPURP", 
#                          "finalweight", "OMAZ", "DMAZ", "OTAZ", "DTAZ", "od_dist")]
# trips_subset[is.na(trips_subset)] <- 0
# write.csv(trips_subset, "trips_with_MAZ.csv", row.names = F)

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

#this contains a joint trip for each person
jtrips <- trips[trips$JTOUR_ID>0 & trips$JTRIP_ID>0,]

# joint trips should be a household level file, therefore, remove person dimension
jtrips$PER_ID <- NULL
jtrips <- unique(jtrips[,c("DAYNO", "HH_ID", "JTRIP_ID")])
jtrips$uid <- paste(jtrips$DAYNO, jtrips$HH_ID, jtrips$JTRIP_ID, sep = "-")
jutrips$uid <- paste(jutrips$DAYNO, jutrips$HH_ID, jutrips$JTRIP_ID, sep = "-")


jtrips[!(jtrips$uid %in% jutrips$uid)]


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


# Compare person type across days
# Person types are recoded by Python script based on activity purposes reported for each day
# 
# for (i in 2:ndays) {
#   dayno                 <- i
#   dayDir                <- paste(Survey_Processed_Dir, "/day", as.character(dayno), sep = "")
#   per_nxt               <- read.csv(paste(dayDir, "persons.csv", sep = "/"), as.is = T)
#   # US
#   processedPerson$ptype <- per_nxt$PERSONTYPE[match(processedPerson$HH_ID*100+processedPerson$PER_ID,per_nxt$HH_ID*100+per_nxt$PER_ID)]
#   temp                  <- processedPerson[(processedPerson$PERSONTYPE %in% c(1,2,4,5)) & processedPerson$ptype==3,]
#   processedPerson       <- processedPerson[!((processedPerson$PERSONTYPE %in% c(1,2,4,5)) & processedPerson$ptype==3),]
#   processedPerson       <- processedPerson[,-which(names(processedPerson) %in% c("ptype"))]
#   processedPerson       <- rbind(processedPerson, per_nxt[match(temp$HH_ID*100+temp$PER_ID, per_nxt$HH_ID*100+per_nxt$PER_ID), ])
#   # PW
#   processedPerson$ptype <- per_nxt$PERSONTYPE[match(processedPerson$HH_ID*100+processedPerson$PER_ID,per_nxt$HH_ID*100+per_nxt$PER_ID)]
#   temp                  <- processedPerson[(processedPerson$PERSONTYPE %in% c(4,5)) & processedPerson$ptype==2,]
#   processedPerson       <- processedPerson[!((processedPerson$PERSONTYPE %in% c(4,5)) & processedPerson$ptype==2),]
#   #processedPerson       <- processedPerson[,-which(names(processedPerson) %in% c("ptype"))]
#   processedPerson       <- rbind(processedPerson, per_nxt[match(temp$HH_ID*100+temp$PER_ID, per_nxt$HH_ID*100+per_nxt$PER_ID), ])
# }

# get person type and age of all memebrs on joint tours
processedPerson[, `:=` (PERID = PER_ID, HHID = HH_ID)]

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
stopFreqModel_missingStops <- xtabs(finalweight~TOTAL_STOPS+TOURPURP,
                                    data = temp_tour[(temp_tour$TOURPURP<=9 | temp_tour$TOURPURP==20) & temp_tour$STOP_FREQ_ALT==0,])
write.csv(stopFreqModel_missingStops, file.path(outdir, "stopFreqModel_missingStops.csv"), row.names = T)

stopFreqModel_summary <- xtabs(finalweight~STOP_FREQ_ALT+TOURPURP, data = temp_tour[temp_tour$TOURPURP<=9 | temp_tour$TOURPURP==20,])
stopFreqModel_summary = as.data.table(as.data.frame.matrix(stopFreqModel_summary))
fwrite(stopFreqModel_summary, file.path(outdir, "stopFreqModel_summary.csv"), row.names = T)
# end of the part added by nagendra.dhakar@rsginc.com

#---------------------------------------------------------------------------------														  
# tours[TOURPURP == 1 & IS_SUBTOUR == 0, .N, by=.(HH_ID, PER_ID, DAYNO)]

workCounts <- plyr::count(tours, c( "HH_ID", "PER_ID", "DAYNO"), "TOURPURP == 1 & IS_SUBTOUR == 0") #[excluding at work subtours]. joint work tours should be considered individual mandatory tours
atWorkCounts <- plyr::count(tours, c("HH_ID", "PER_ID", "DAYNO"), "(TOURPURP == 20 & IS_SUBTOUR == 1)") #include joint work-subtours as individual subtours
schlCounts <- plyr::count(tours, c("HH_ID", "PER_ID", "DAYNO"), "(TOURPURP == 2 | TOURPURP == 3)") #joint school/university tours should be considered individual mandatory tours
inmCounts <- plyr::count(tours, c("HH_ID", "PER_ID", "DAYNO"), "(TOURPURP>=4 & TOURPURP<=9 & FULLY_JOINT==0 & IS_SUBTOUR == 0) | (TOURPURP==4 & FULLY_JOINT==1)") #joint escort tours should be considered individual non-mandatory tours
itourCounts <- plyr::count(tours, c("HH_ID", "PER_ID", "DAYNO"), "(TOURPURP <= 9 & IS_SUBTOUR == 0 & FULLY_JOINT==0) | (TOURPURP <= 4 & FULLY_JOINT==1)")  #number of individual tours per person [excluding at work subtours]
jtourCounts <- plyr::count(tours, c("HH_ID", "PER_ID", "DAYNO"), "TOURPURP >= 5 & TOURPURP <= 9 & IS_SUBTOUR == 0 & FULLY_JOINT==1")  #number of joint tours per person [excluding at work subtours, also excluding joint escort tours]

xworkCounts = workCounts
xworkCounts$PERTYPE = per$PERTYPE[match(paste(xworkCounts$HH_ID, xworkCounts$PER_ID, sep = "-"),
                                        paste(per$SAMPN, per$PERNO, sep = "-"))]

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


indivNMTourFreq_file = file.path(outdir, "indivNMTourFreq_1plusMand.csv")
indivNMTourFreq_NoMand_file = file.path(outdir, "indivNMTourFreq_NoMand.csv")
# indivNMTourFreq_file = file.path(outdir, "indivNMTourFreq.csv")
# write.table("Non-Mandatory Tours for Persons with at-least 1 Mandatory Tour", indivNMTourFreq_file, sep = ",", row.names = F, append = F)
# write.table(freq_nmtours_mand, indivNMTourFreq_file, sep = ",", row.names = F, append = T)
# write.table("Non-Mandatory Tours for Active Persons with 0 Mandatory Tour", indivNMTourFreq_file, sep = ",", row.names = F, append = T)
# write.table(freq_nmtours_nomand, indivNMTourFreq_file, sep = ",", row.names = F, append = TRUE)

fwrite(freq_nmtours_mand, indivNMTourFreq_file)
fwrite(freq_nmtours_nomand, indivNMTourFreq_NoMand_file)


# fwrite(freq_nmtours_mand,  file.path(outdir, "freq_nmtours_mand.csv"))
# fwrite(freq_nmtours_nomand,  file.path(outdir, "freq_nmtours_mand.csv"))


# end of addition for calibration

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
tours$PERTYPE = tours$PERSONTYPE
trips$PERTYPE = trips$PERSONTYPE
perday$numTours[is.na(perday$numTours)] <- 0
toursPertypeDistbn <- plyr::count(tours[!is.na(tours$TOURPURP) & tours$TOURPURP<=10 & tours$FULLY_JOINT==0 & tours$IS_SUBTOUR==0,], c("PERTYPE"), "finalweight")
fwrite(toursPertypeDistbn, file.path(outdir, "toursPertypeDistbn.csv"), row.names = FALSE)

# Total tours by person type for visualizer
totaltoursPertypeDistbn <- plyr::count(tours[!is.na(tours$TOURPURP) & tours$TOURPURP<=10,], c("PERTYPE"), "finalweight")  #updated by nagendra.dhakar@rsginc, 01/30/2018 -  removed (tours$IS_SUBTOUR==0)
fwrite(totaltoursPertypeDistbn, file.path(outdir, "total_tours_by_pertype_vis.csv"), row.names = F)


# Total indi NM tours by person type and purpose
# tours_pertype_purpose <- plyr::count(tours[tours$TOURPURP>=4 & tours$TOURPURP<=9 & tours$FULLY_JOINT==0,], c("PERTYPE", "TOURPURP"), "finalweight")
tours_pertype_purpose <- plyr::count(tours[tours$FULLY_JOINT==0,], c("PERTYPE", "TOURPURP"), "finalweight")
fwrite(tours_pertype_purpose, file.path(outdir, "tours_pertype_purpose.csv"), row.names = FALSE)

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
fwrite(indi_nm_tours_pertype, file.path(outdir, "inmtours_pertype_purpose.csv"), row.names = F)

# end of addition for calibration
# ---------------------------------------------------

tours_pertype_purpose_tab <- xtabs(freq~PERTYPE+TOURPURP, tours_pertype_purpose)
tours_pertype_purpose_tab[is.na(tours_pertype_purpose_tab)] <- 0
tours_pertype_purpose_tab <- addmargins(as.table(tours_pertype_purpose_tab))
# tours_pertype_purpose_tab <- as.data.frame.matrix(tours_pertype_purpose_tab)

totalPersons <- sum(pertypeDistbn$freq)
totalPersons_DF <- data.frame("Total", totalPersons)
colnames(totalPersons_DF) <- colnames(pertypeDistbn)
pertypeDF <- rbind(pertypeDistbn, totalPersons_DF)

nm_tour_rates <- tours_pertype_purpose_tab / pertypeDF$freq
# nm_tour_rates$pertype <- row.names(nm_tour_rates)
nm_tour_rates <- melt(nm_tour_rates)
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

fwrite(nm_tour_rates, file.path(outdir, "nm_tour_rates.csv"), row.names = F)

# Total tours by purpose X tourtype
t1 <- wtd.hist(tours$TOURPURP[!is.na(tours$TOURPURP) & tours$TOURPURP<=10 & tours$FULLY_JOINT==0 & tours$IS_SUBTOUR==0], breaks = seq(1,10, by=1), freq = NULL, right=FALSE, weight=tours$finalweight[!is.na(tours$TOURPURP) & tours$TOURPURP<=10 & tours$FULLY_JOINT==0 & tours$IS_SUBTOUR==0])
t3 <- wtd.hist(jtours$JOINT_PURP[jtours$JOINT_PURP<10], breaks = seq(1,10, by=1), freq = NULL, right=FALSE, weight=jtours$finalweight[jtours$JOINT_PURP<10])
tours_purpose_type <- data.frame(t1$counts, t3$counts)
colnames(tours_purpose_type) <- c("indi", "joint")

fwrite(tours_purpose_type, file.path(outdir, "tours_purpose_type.csv"), row.names = FALSE)


# DAP by pertype
perday$DAP <- "H"
perday$DAP[perday$workTours > 0 | perday$schlTours > 0] <- "M"
perday$DAP[perday$numTours > 0 & perday$DAP == "H"] <- "N"
perday$DAP[(perday$PERTYPE == 4 | perday$PERTYPE == 5) & perday$DAP == "M"] = "N"

dapSummary <- plyr::count(perday, c("PERTYPE", "DAP"), "finalweight")
fwrite(dapSummary, file.path(outdir, "dapSummary.csv"), row.names = FALSE)

dapSummary_day <- plyr::count(perday, c("DAYNO","PERTYPE", "DAP"), "finalweight")

fwrite(dapSummary_day, file.path(outdir, "dapSummary_day.csv"), row.names = FALSE)


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

###
# dapSummary_vis <- dapSummary_vis[dapSummary_vis$PERTYPE!='Total', ]

fwrite(dapSummary_vis, file.path(outdir, "dapSummary_vis.csv"), row.names = FALSE)

# HHSize X Joint
hhsizeJoint <- plyr::count(hhday[hhday$HHSIZE>=2,], c("HHSIZE", "JOINT"), "finalweight")

fwrite(hhsizeJoint, file.path(outdir, "hhsizeJoint.csv"), row.names = FALSE)

#### mandatory tour frequency ####
perday$mtf <- 0
perday$mtf[perday$workTours == 1] <- 1
perday$mtf[perday$workTours >= 2] <- 2
perday$mtf[perday$schlTours == 1] <- 3
perday$mtf[perday$schlTours >= 2] <- 4
perday$mtf[perday$workTours >= 1 & perday$schlTours >= 1] <- 5

mtfSummary <- plyr::count(perday[perday$mtf > 0,], c("PERTYPE", "mtf"), "finalweight_1")
fwrite(mtfSummary, file.path(outdir, "mtfSummary.csv"))
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

fwrite(mtfSummary_vis, file.path(outdir, "mtfSummary_vis.csv"))

# indi NM summary
inm0Summary <- plyr::count(perday[perday$inmTours==0,], c("PERTYPE"), "finalweight_1")
inm1Summary <- plyr::count(perday[perday$inmTours==1,], c("PERTYPE"), "finalweight_1")
inm2Summary <- plyr::count(perday[perday$inmTours==2,], c("PERTYPE"), "finalweight_1")
inm3Summary <- plyr::count(perday[perday$inmTours>=3,], c("PERTYPE"), "finalweight_1")

inmSummary <- data.frame(PERTYPE = c(1,2,3,4,5,6,7,8))
inmSummary$tour0 <- inm0Summary$freq[match(inmSummary$PERTYPE, inm0Summary$PERTYPE)]
inmSummary$tour1 <- inm1Summary$freq[match(inmSummary$PERTYPE, inm1Summary$PERTYPE)]
inmSummary$tour2 <- inm2Summary$freq[match(inmSummary$PERTYPE, inm2Summary$PERTYPE)]
inmSummary$tour3pl <- inm3Summary$freq[match(inmSummary$PERTYPE, inm3Summary$PERTYPE)]

fwrite(inmSummary, file.path(outdir, "innmSummary.csv"), col.names=TRUE, sep=",")

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

fwrite(inmSummary_vis, file.path(outdir, "inmSummary_vis.csv"))

# Joint Tour Frequency and composition
jtfSummary <- plyr::count(hhday[!is.na(hhday$jtf),], c("jtf"), "finalweight")
jointComp <- plyr::count(jtours[jtours$JOINT_PURP>=5 & jtours$JOINT_PURP<=9,], c("COMPOSITION"), "finalweight")
jointPartySize <- plyr::count(jtours[jtours$JOINT_PURP>=5 & jtours$JOINT_PURP<=9,], c("NUMBER_HH"), "finalweight")
jointCompPartySize <- plyr::count(jtours[jtours$JOINT_PURP>=5 & jtours$JOINT_PURP<=9,], c("COMPOSITION","NUMBER_HH"), "finalweight")

hhday$jointCat[hhday$jtours==0] <- 0
hhday$jointCat[hhday$jtours==1] <- 1
hhday$jointCat[hhday$jtours>=2] <- 2

jointToursHHSize <- plyr::count(hhday[!is.na(hhday$HHSIZE) & !is.na(hhday$jointCat),], c("HHSIZE", "jointCat"), "finalweight")


write.table(jtfSummary, file.path(outdir, "jtfSummary.csv"), col.names=TRUE, sep=",")
write.table(jointComp, file.path(outdir, "jtfSummary.csv"), col.names=TRUE, sep=",", append=TRUE)
write.table(jointPartySize, file.path(outdir, "jtfSummary.csv"), col.names=TRUE, sep=",", append=TRUE)
write.table(jointCompPartySize, file.path(outdir, "jtfSummary.csv"), col.names=TRUE, sep=",", append=TRUE)
write.table(jointToursHHSize, file.path(outdir, "jtfSummary.csv"), col.names=TRUE, sep=",", append=TRUE)


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



fwrite(jtf, file.path(outdir, "jtf.csv"), row.names = F)
fwrite(jointComp, file.path(outdir, "jointComp.csv"), row.names = F)
fwrite(jointPartySize, file.path(outdir, "jointPartySize.csv"), row.names = F)
fwrite(jointCompPartySizeProp, file.path(outdir, "jointCompPartySize.csv"), row.names = F)
fwrite(jointToursHHSizeProp, file.path(outdir, "jointToursHHSize.csv"), row.names = F)


#### TOD Profile ####
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
fwrite(todDepProfile, file.path(outdir, "todDepProfile.csv"))

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
fwrite(todArrProfile, file.path(outdir, "todArrProfile.csv"))

#### prepare input for visualizer ####
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
fwrite(arrive_depart_distbn, file.path(outdir, "Non_Mand_Tours_ArrDep_Distbn.csv"), row.names = F, quote = F)


#### stops by direction, purpose and model tod ####

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
fwrite(stops_ib_tod, file.path(outdir, "todStopsIB.csv"), row.names = F)
fwrite(stops_ob_tod, file.path(outdir, "todStopsOB.csv"), row.names = F)

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
fwrite(jstops_ib_tod, file.path(outdir, "todStopsIB_joint.csv"), row.names = F)
fwrite(jstops_ob_tod, file.path(outdir, "todStopsOB_joint.csv"), row.names = F)


#### filter out records with missing or negative tour duration ####
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
fwrite(todDurProfile, file.path(outdir, "todDurProfile.csv"))

#### prepare input for visualizer ####
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

fwrite(todProfile_vis, file.path(outdir, "todProfile_vis.csv"), row.names = F)


##### Non Mand Tour lengths ####

# NO ESCORT IN DUBAI???
# tourdist4 <- wtd.hist(tours$DIST[!is.na(tours$DIST) & tours$TOURPURP==4 & tours$IS_SUBTOUR==0],
#                       breaks = c(seq(0,40, by=1), 9999), freq = NULL, right=FALSE,
#                       weight = tours$finalweight[!is.na(tours$DIST) & tours$TOURPURP==4 & tours$IS_SUBTOUR==0])
tourdist5 <- wtd.hist(tours$DIST[!is.na(tours$DIST) & tours$TOURPURP==5 & tours$IS_SUBTOUR==0],
                      breaks = c(seq(0,40, by=1), 9999), freq = NULL, right=FALSE, 
                      weight = tours$finalweight[!is.na(tours$DIST) & tours$TOURPURP==5 & tours$IS_SUBTOUR==0])
tourdist6 <- wtd.hist(tours$DIST[!is.na(tours$DIST) & tours$TOURPURP==6 & tours$IS_SUBTOUR==0],
                      breaks = c(seq(0,40, by=1), 9999), freq = NULL, right=FALSE, 
                      weight = tours$finalweight[!is.na(tours$DIST) & tours$TOURPURP==6 & tours$IS_SUBTOUR==0])
tourdist7 <- wtd.hist(tours$DIST[!is.na(tours$DIST) & tours$TOURPURP==7 & tours$IS_SUBTOUR==0], 
                      breaks = c(seq(0,40, by=1), 9999), freq = NULL, right=FALSE,
                      weight = tours$finalweight[!is.na(tours$DIST) & tours$TOURPURP==7 & tours$IS_SUBTOUR==0])
tourdist8 <- wtd.hist(tours$DIST[!is.na(tours$DIST) & tours$TOURPURP==8 & tours$IS_SUBTOUR==0],
                      breaks = c(seq(0,40, by=1), 9999), freq = NULL, right=FALSE,
                      weight = tours$finalweight[!is.na(tours$DIST) & tours$TOURPURP==8 & tours$IS_SUBTOUR==0])
tourdist9 <- wtd.hist(tours$DIST[!is.na(tours$DIST) & tours$TOURPURP==9 & tours$IS_SUBTOUR==0],
                      breaks = c(seq(0,40, by=1), 9999), freq = NULL, right=FALSE,
                      weight = tours$finalweight[!is.na(tours$DIST) & tours$TOURPURP==9 & tours$IS_SUBTOUR==0])
#tourdist10 <- wtd.hist(tours$DISTMILE[!is.na(tours$DISTMILE) & tours$TOURPURP==10 & tours$IS_SUBTOUR==1], breaks = c(seq(0,40, by=1), 9999), freq = NULL, right=FALSE, weight = tours$finalweight[!is.na(tours$DISTMILE) & tours$TOURPURP==10 & tours$IS_SUBTOUR==1])
#FIXME ASR: Why is there no TOURPURP 10?
tourdist10 = tourdist9
tourdist10$counts = 0
tourDistProfile <- data.frame(#tourdist4$counts
                              tourdist5$counts*0,
                              tourdist5$counts, tourdist6$counts, tourdist7$counts, tourdist8$counts, tourdist9$counts, tourdist10$counts)
colnames(tourDistProfile) <- c("esco", "shop", "main", "eati", "visi", "disc", "atwork")

fwrite(tourDistProfile, file.path(outdir, "tourDistProfile.csv"))


#put TAXI mode in SR2
#tours$TOURMODE <- ifelse(tours$TOURMODE==10,2, tours$TOURMODE)

#### Tour Mode X Auto Suff ####
#

tmode1_as0 <- wtd.hist(tours$TOURMODE[!is.na(tours$TOURMODE) & tours$TOURPURP==1 & tours$FULLY_JOINT==0 & tours$AUTOSUFF==0],
                       breaks = seq(1,15, by=1), freq = NULL, right=FALSE,
                       weight = tours$finalweight[!is.na(tours$TOURMODE) & tours$TOURPURP==1 & tours$FULLY_JOINT==0 & tours$AUTOSUFF==0])
tmode2_as0 <- wtd.hist(tours$TOURMODE[!is.na(tours$TOURMODE) & tours$TOURPURP==2 & tours$FULLY_JOINT==0 & tours$AUTOSUFF==0],
                       breaks = seq(1,15, by=1), freq = NULL, right=FALSE, 
                       weight = tours$finalweight[!is.na(tours$TOURMODE) & tours$TOURPURP==2 & tours$FULLY_JOINT==0 & tours$AUTOSUFF==0])
tmode3_as0 <- wtd.hist(tours$TOURMODE[!is.na(tours$TOURMODE) & tours$TOURPURP==3 & tours$FULLY_JOINT==0 & tours$AUTOSUFF==0],
                       breaks = seq(1,15, by=1), freq = NULL, right=FALSE, 
                       weight = tours$finalweight[!is.na(tours$TOURMODE) & tours$TOURPURP==3 & tours$FULLY_JOINT==0 & tours$AUTOSUFF==0])
tmode4_as0 <- wtd.hist(tours$TOURMODE[!is.na(tours$TOURMODE) & tours$TOURPURP>=4 & tours$TOURPURP<=6 & tours$FULLY_JOINT==0 & tours$AUTOSUFF==0],
                       breaks = seq(1,15, by=1), freq = NULL, right=FALSE, 
                       weight = tours$finalweight[!is.na(tours$TOURMODE) & tours$TOURPURP>=4 & tours$TOURPURP<=6 & tours$FULLY_JOINT==0 & tours$AUTOSUFF==0])
tmode5_as0 <- wtd.hist(tours$TOURMODE[!is.na(tours$TOURMODE) & tours$TOURPURP>=7 & tours$TOURPURP<=9 & tours$FULLY_JOINT==0 & tours$AUTOSUFF==0],
                       breaks = seq(1,15, by=1), freq = NULL, right=FALSE, 
                       weight = tours$finalweight[!is.na(tours$TOURMODE) & tours$TOURPURP>=7 & tours$TOURPURP<=9 & tours$FULLY_JOINT==0 & tours$AUTOSUFF==0])
tmode6_as0 <- wtd.hist(jtours$TOURMODE[jtours$JOINT_PURP>=4 & jtours$JOINT_PURP<=6 & jtours$AUTOSUFF==0], 
                       breaks = seq(1,15, by=1), freq = NULL, right=FALSE, 
                       weight = jtours$finalweight[jtours$JOINT_PURP>=4 & jtours$JOINT_PURP<=6 & jtours$AUTOSUFF==0])
tmode7_as0 <- wtd.hist(jtours$TOURMODE[jtours$JOINT_PURP>=7 & jtours$JOINT_PURP<=9 & jtours$AUTOSUFF==0],
                       breaks = seq(1,15, by=1), freq = NULL, right=FALSE,
                       weight = jtours$finalweight[jtours$JOINT_PURP>=7 & jtours$JOINT_PURP<=9 & jtours$AUTOSUFF==0])
tmode8_as0 <- wtd.hist(tours$TOURMODE[!is.na(tours$TOURMODE) & tours$IS_SUBTOUR==1 & tours$AUTOSUFF==0 & tours$FULLY_JOINT==0],
                       breaks = seq(1,15, by=1), freq = NULL, right=FALSE,
                       weight = tours$finalweight[!is.na(tours$TOURMODE) & tours$IS_SUBTOUR==1 & tours$AUTOSUFF==0 & tours$FULLY_JOINT==0])

tmodeAS0Profile <- data.frame(tmode1_as0$counts, tmode2_as0$counts, tmode3_as0$counts, tmode4_as0$counts,
                              tmode5_as0$counts, tmode6_as0$counts, tmode7_as0$counts, tmode8_as0$counts)
colnames(tmodeAS0Profile) <- c("work", "univ", "sch", "imain", "idisc", "jmain", "jdisc", "atwork")

fwrite(tmodeAS0Profile, file.path(outdir, "tmodeAS0Profile.csv"))

# Prepeare data for visualizer
tmodeAS0Profile_vis <- tmodeAS0Profile #[1:9,]
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


tmode1_as1 <- wtd.hist(tours$TOURMODE[!is.na(tours$TOURMODE) & tours$TOURPURP==1 & tours$FULLY_JOINT==0 & tours$AUTOSUFF==1],
                       breaks = seq(1,15, by=1), freq = NULL, right=FALSE, 
                       weight = tours$finalweight[!is.na(tours$TOURMODE) & tours$TOURPURP==1 & tours$FULLY_JOINT==0 & tours$AUTOSUFF==1])
tmode2_as1 <- wtd.hist(tours$TOURMODE[!is.na(tours$TOURMODE) & tours$TOURPURP==2 & tours$FULLY_JOINT==0 & tours$AUTOSUFF==1],
                       breaks = seq(1,15, by=1), freq = NULL, right=FALSE, 
                       weight = tours$finalweight[!is.na(tours$TOURMODE) & tours$TOURPURP==2 & tours$FULLY_JOINT==0 & tours$AUTOSUFF==1])
tmode3_as1 <- wtd.hist(tours$TOURMODE[!is.na(tours$TOURMODE) & tours$TOURPURP==3 & tours$FULLY_JOINT==0 & tours$AUTOSUFF==1],
                       breaks = seq(1,15, by=1), freq = NULL, right=FALSE, 
                       weight = tours$finalweight[!is.na(tours$TOURMODE) & tours$TOURPURP==3 & tours$FULLY_JOINT==0 & tours$AUTOSUFF==1])
tmode4_as1 <- wtd.hist(tours$TOURMODE[!is.na(tours$TOURMODE) & tours$TOURPURP>=4 & tours$TOURPURP<=6 & tours$FULLY_JOINT==0 & tours$AUTOSUFF==1],
                       breaks = seq(1,15, by=1), freq = NULL, right=FALSE, 
                       weight = tours$finalweight[!is.na(tours$TOURMODE) & tours$TOURPURP>=4 & tours$TOURPURP<=6 & tours$FULLY_JOINT==0 & tours$AUTOSUFF==1])
tmode5_as1 <- wtd.hist(tours$TOURMODE[!is.na(tours$TOURMODE) & tours$TOURPURP>=7 & tours$TOURPURP<=9 & tours$FULLY_JOINT==0 & tours$AUTOSUFF==1],
                       breaks = seq(1,15, by=1), freq = NULL, right=FALSE, 
                       weight = tours$finalweight[!is.na(tours$TOURMODE) & tours$TOURPURP>=7 & tours$TOURPURP<=9 & tours$FULLY_JOINT==0 & tours$AUTOSUFF==1])
tmode6_as1 <- wtd.hist(jtours$TOURMODE[jtours$JOINT_PURP>=4 & jtours$JOINT_PURP<=6 & jtours$AUTOSUFF==1],
                       breaks = seq(1,15, by=1), freq = NULL, right=FALSE, 
                       weight = jtours$finalweight[jtours$JOINT_PURP>=4 & jtours$JOINT_PURP<=6 & jtours$AUTOSUFF==1])
tmode7_as1 <- wtd.hist(jtours$TOURMODE[jtours$JOINT_PURP>=7 & jtours$JOINT_PURP<=9 & jtours$AUTOSUFF==1],
                       breaks = seq(1,15, by=1), freq = NULL, right=FALSE, 
                       weight = jtours$finalweight[jtours$JOINT_PURP>=7 & jtours$JOINT_PURP<=9 & jtours$AUTOSUFF==1])
tmode8_as1 <- wtd.hist(tours$TOURMODE[!is.na(tours$TOURMODE) & tours$IS_SUBTOUR==1 & tours$AUTOSUFF==1 & tours$FULLY_JOINT==0],
                       breaks = seq(1,15, by=1), freq = NULL, right=FALSE,
                       weight = tours$finalweight[!is.na(tours$TOURMODE) & tours$IS_SUBTOUR==1 & tours$AUTOSUFF==1 & tours$FULLY_JOINT==0])

tmodeAS1Profile <- data.frame(tmode1_as1$counts, tmode2_as1$counts, tmode3_as1$counts, tmode4_as1$counts,
                              tmode5_as1$counts, tmode6_as1$counts, tmode7_as1$counts, tmode8_as1$counts)
colnames(tmodeAS1Profile) <- c("work", "univ", "sch", "imain", "idisc", "jmain", "jdisc", "atwork")
fwrite(tmodeAS1Profile, file.path(outdir, "tmodeAS1Profile.csv"))

# Prepeare data for visualizer
tmodeAS1Profile_vis <- tmodeAS1Profile #[1:9,]
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


tmode1_as2 <- wtd.hist(tours$TOURMODE[!is.na(tours$TOURMODE) & tours$TOURPURP==1 & tours$FULLY_JOINT==0 & tours$AUTOSUFF==2],
                       breaks = seq(1,15, by=1), freq = NULL, right=FALSE,
                       weight = tours$finalweight[!is.na(tours$TOURMODE) & tours$TOURPURP==1 & tours$FULLY_JOINT==0 & tours$AUTOSUFF==2])
tmode2_as2 <- wtd.hist(tours$TOURMODE[!is.na(tours$TOURMODE) & tours$TOURPURP==2 & tours$FULLY_JOINT==0 & tours$AUTOSUFF==2],
                       breaks = seq(1,15, by=1), freq = NULL, right=FALSE, 
                       weight = tours$finalweight[!is.na(tours$TOURMODE) & tours$TOURPURP==2 & tours$FULLY_JOINT==0 & tours$AUTOSUFF==2])
tmode3_as2 <- wtd.hist(tours$TOURMODE[!is.na(tours$TOURMODE) & tours$TOURPURP==3 & tours$FULLY_JOINT==0 & tours$AUTOSUFF==2],
                       breaks = seq(1,15, by=1), freq = NULL, right=FALSE, 
                       weight = tours$finalweight[!is.na(tours$TOURMODE) & tours$TOURPURP==3 & tours$FULLY_JOINT==0 & tours$AUTOSUFF==2])
tmode4_as2 <- wtd.hist(tours$TOURMODE[!is.na(tours$TOURMODE) & tours$TOURPURP>=4 & tours$TOURPURP<=6 & tours$FULLY_JOINT==0 & tours$AUTOSUFF==2],
                       breaks = seq(1,15, by=1), freq = NULL, right=FALSE, 
                       weight = tours$finalweight[!is.na(tours$TOURMODE) & tours$TOURPURP>=4 & tours$TOURPURP<=6 & tours$FULLY_JOINT==0 & tours$AUTOSUFF==2])
tmode5_as2 <- wtd.hist(tours$TOURMODE[!is.na(tours$TOURMODE) & tours$TOURPURP>=7 & tours$TOURPURP<=9 & tours$FULLY_JOINT==0 & tours$AUTOSUFF==2], 
                       breaks = seq(1,15, by=1), freq = NULL, right=FALSE, 
                       weight = tours$finalweight[!is.na(tours$TOURMODE) & tours$TOURPURP>=7 & tours$TOURPURP<=9 & tours$FULLY_JOINT==0 & tours$AUTOSUFF==2])
tmode6_as2 <- wtd.hist(jtours$TOURMODE[jtours$JOINT_PURP>=4 & jtours$JOINT_PURP<=6 & jtours$AUTOSUFF==2],
                       breaks = seq(1,15, by=1), freq = NULL, right=FALSE, 
                       weight = jtours$finalweight[jtours$JOINT_PURP>=4 & jtours$JOINT_PURP<=6 & jtours$AUTOSUFF==2])
tmode7_as2 <- wtd.hist(jtours$TOURMODE[jtours$JOINT_PURP>=7 & jtours$JOINT_PURP<=9 & jtours$AUTOSUFF==2],
                       breaks = seq(1,15, by=1), freq = NULL, right=FALSE, 
                       weight = jtours$finalweight[jtours$JOINT_PURP>=7 & jtours$JOINT_PURP<=9 & jtours$AUTOSUFF==2])
tmode8_as2 <- wtd.hist(tours$TOURMODE[!is.na(tours$TOURMODE) & tours$IS_SUBTOUR==1 & tours$AUTOSUFF==2 & tours$FULLY_JOINT==0],
                       breaks = seq(1,15, by=1), freq = NULL, right=FALSE,
                       weight = tours$finalweight[!is.na(tours$TOURMODE) & tours$IS_SUBTOUR==1 & tours$AUTOSUFF==2 & tours$FULLY_JOINT==0])

tmodeAS2Profile <- data.frame(tmode1_as2$counts, tmode2_as2$counts, tmode3_as2$counts, tmode4_as2$counts,
                              tmode5_as2$counts, tmode6_as2$counts, tmode7_as2$counts, tmode8_as2$counts)
colnames(tmodeAS2Profile) <- c("work", "univ", "sch", "imain", "idisc", "jmain", "jdisc", "atwork")
fwrite(tmodeAS2Profile, file.path(outdir, "tmodeAS2Profile.csv"))

# Prepeare data for visualizer
tmodeAS2Profile_vis <- tmodeAS2Profile #[1:12,]
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
tmodeProfile_vis <- data.frame(tmodeAS0Profile_vis,
                               tmodeAS1Profile_vis$freq_as1, 
                               tmodeAS2Profile_vis$freq_as2)


colnames(tmodeProfile_vis) <- c("id", "purpose", "freq_as0", "freq_as1", "freq_as2")
tmodeProfile_vis$freq_all <- tmodeProfile_vis$freq_as0 + tmodeProfile_vis$freq_as1 + tmodeProfile_vis$freq_as2
fwrite(tmodeProfile_vis, file.path(outdir, "tmodeProfile_vis.csv"), row.names = F)


#### tour mode by time period - for Wu, Sun ####
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

fwrite(itours_summary, file.path(outdir, "itours_tourmode_summary.csv"), row.names = F)
fwrite(jtours_summary, file.path(outdir, "jtours_tourmode_summary.csv"), row.names = F)

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

fwrite(itours_summary_op, file.path(outdir, "itours_tourmode_summary_op.csv"), row.names = F)
fwrite(jtours_summary_op, file.path(outdir, "jtours_tourmode_summary_op.csv"), row.names = F)

#### Non-mandatory tour distance profile ####
# tourdist4 <- wtd.hist(tours$SKIMDIST[tours$TOURPURP==4 & tours$FULLY_JOINT==0], breaks = c(seq(0,40, by=1), 9999), freq = NULL, right=FALSE, weight = tours$finalweight[tours$TOURPURP==4 & tours$FULLY_JOINT==0])
tourdisti56 <- wtd.hist(tours$SKIMDIST[tours$TOURPURP>=5 & tours$TOURPURP<=6 & tours$FULLY_JOINT==0], breaks = c(seq(0,40, by=1), 9999), freq = NULL, right=FALSE, weight = tours$finalweight[tours$TOURPURP>=5 & tours$TOURPURP<=6 & tours$FULLY_JOINT==0])
tourdisti789 <- wtd.hist(tours$SKIMDIST[tours$TOURPURP>=7 & tours$TOURPURP<=9 & tours$FULLY_JOINT==0], breaks = c(seq(0,40, by=1), 9999), freq = NULL, right=FALSE, weight = tours$finalweight[tours$TOURPURP>=7 & tours$TOURPURP<=9 & tours$FULLY_JOINT==0])
tourdistj56 <- wtd.hist(jtours$SKIMDIST[jtours$JOINT_PURP>=5 & jtours$JOINT_PURP<=6], breaks = c(seq(0,40, by=1), 9999), freq = NULL, right=FALSE, weight = jtours$finalweight[jtours$JOINT_PURP>=5 & jtours$JOINT_PURP<=6])
tourdistj789 <- wtd.hist(jtours$SKIMDIST[jtours$JOINT_PURP>=7 & jtours$JOINT_PURP<=9], breaks = c(seq(0,40, by=1), 9999), freq = NULL, right=FALSE, weight = jtours$finalweight[jtours$JOINT_PURP>=7 & jtours$JOINT_PURP<=9])
tourdist10 <- wtd.hist(tours$SKIMDIST[tours$IS_SUBTOUR == 1], breaks = c(seq(0,40, by=1), 9999), freq = NULL, right=FALSE, weight = tours$finalweight[tours$IS_SUBTOUR == 1])

tourDistProfile <- data.frame(# tourdist4$counts,
                              0*tourdisti56$counts,
                              tourdisti56$counts,
                              tourdisti789$counts,
                              tourdistj56$counts,
                              tourdistj789$counts, 
                              tourdist10$counts)
colnames(tourDistProfile) <- c("esco", "imain", "idisc", "jmain", "jdisc", "atwork")
fwrite(tourDistProfile, file.path(outdir, "nonMandTourDistProfile.csv"))

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

fwrite(tourDistProfile_vis, file.path(outdir, "tourDistProfile_vis.csv"), row.names = F)

cat("\n Average Tour Distance [esco]: ", weighted.mean(tours$SKIMDIST[tours$TOURPURP==4 & tours$FULLY_JOINT==0], tours$finalweight[tours$TOURPURP==4 & tours$FULLY_JOINT==0], na.rm = TRUE))
cat("\n Average Tour Distance [imain]: ", weighted.mean(tours$SKIMDIST[tours$TOURPURP>=5 & tours$TOURPURP<=6 & tours$FULLY_JOINT==0], tours$finalweight[tours$TOURPURP>=5 & tours$TOURPURP<=6 & tours$FULLY_JOINT==0], na.rm = TRUE))
cat("\n Average Tour Distance [idisc]: ", weighted.mean(tours$SKIMDIST[tours$TOURPURP>=7 & tours$TOURPURP<=9 & tours$FULLY_JOINT==0], tours$finalweight[tours$TOURPURP>=7 & tours$TOURPURP<=9 & tours$FULLY_JOINT==0], na.rm = TRUE))
cat("\n Average Tour Distance [jmain]: ", weighted.mean(jtours$SKIMDIST[jtours$JOINT_PURP>=5 & jtours$JOINT_PURP<=6], jtours$finalweight[jtours$JOINT_PURP>=5 & jtours$JOINT_PURP<=6], na.rm = TRUE))
cat("\n Average Tour Distance [jdisc]: ", weighted.mean(jtours$SKIMDIST[jtours$JOINT_PURP>=7 & jtours$JOINT_PURP<=9], jtours$finalweight[jtours$JOINT_PURP>=7 & jtours$JOINT_PURP<=9], na.rm = TRUE))
cat("\n Average Tour Distance [atwork]: ", weighted.mean(tours$SKIMDIST[tours$IS_SUBTOUR == 1], tours$finalweight[tours$IS_SUBTOUR == 1], na.rm = TRUE))

#### Output average trips lengths for visualizer ####

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

fwrite(nonMandTripLengths, file.path(outdir, "nonMandTripLengths.csv"), row.names = F)

#### STop Frequency ####
#Outbound
stopfreq1 <- wtd.hist(tours$OUTBOUND_STOPS[tours$TOURPURP==1 & tours$FULLY_JOINT==0], breaks = c(seq(0,3, by=1), 9999), freq = NULL, right=FALSE, weight = tours$finalweight[tours$TOURPURP==1 & tours$FULLY_JOINT==0])
stopfreq2 <- wtd.hist(tours$OUTBOUND_STOPS[tours$TOURPURP==2 & tours$FULLY_JOINT==0], breaks = c(seq(0,3, by=1), 9999), freq = NULL, right=FALSE, weight = tours$finalweight[tours$TOURPURP==2 & tours$FULLY_JOINT==0])
stopfreq3 <- wtd.hist(tours$OUTBOUND_STOPS[tours$TOURPURP==3 & tours$FULLY_JOINT==0], breaks = c(seq(0,3, by=1), 9999), freq = NULL, right=FALSE, weight = tours$finalweight[tours$TOURPURP==3 & tours$FULLY_JOINT==0])
# stopfreq4 <- wtd.hist(tours$OUTBOUND_STOPS[tours$TOURPURP==4 & tours$FULLY_JOINT==0], breaks = c(seq(0,3, by=1), 9999), freq = NULL, right=FALSE, weight = tours$finalweight[tours$TOURPURP==4 & tours$FULLY_JOINT==0])
stopfreqi56 <- wtd.hist(tours$OUTBOUND_STOPS[tours$TOURPURP>=5 & tours$TOURPURP<=6 & tours$FULLY_JOINT==0], breaks = c(seq(0,3, by=1), 9999), freq = NULL, right=FALSE, weight = tours$finalweight[tours$TOURPURP>=5 & tours$TOURPURP<=6 & tours$FULLY_JOINT==0])
stopfreqi789 <- wtd.hist(tours$OUTBOUND_STOPS[tours$TOURPURP>=7 & tours$TOURPURP<=9 & tours$FULLY_JOINT==0], breaks = c(seq(0,3, by=1), 9999), freq = NULL, right=FALSE, weight = tours$finalweight[tours$TOURPURP>=7 & tours$TOURPURP<=9 & tours$FULLY_JOINT==0])
stopfreqj56 <- wtd.hist(jtours$OUTBOUND_STOPS[jtours$JOINT_PURP>=5 & jtours$JOINT_PURP<=6], breaks = c(seq(0,3, by=1), 9999), freq = NULL, right=FALSE, weight = jtours$finalweight[jtours$JOINT_PURP>=5 & jtours$JOINT_PURP<=6])
stopfreqj789 <- wtd.hist(jtours$OUTBOUND_STOPS[jtours$JOINT_PURP>=7 & jtours$JOINT_PURP<=9], breaks = c(seq(0,3, by=1), 9999), freq = NULL, right=FALSE, weight = jtours$finalweight[jtours$JOINT_PURP>=7 & jtours$JOINT_PURP<=9])
stopfreq10 <- wtd.hist(tours$OUTBOUND_STOPS[tours$IS_SUBTOUR == 1], breaks = c(seq(0,3, by=1), 9999), freq = NULL, right=FALSE, weight = tours$finalweight[tours$IS_SUBTOUR == 1])

stopFreq <- data.frame(stopfreq1$counts,
                       stopfreq2$counts, 
                       stopfreq3$counts,
                       stopfreq3$counts*0,
                       # stopfreq4$counts,
                       stopfreqi56$counts
                          , stopfreqi789$counts, stopfreqj56$counts, stopfreqj789$counts, stopfreq10$counts)
colnames(stopFreq) <- c("work", "univ", "sch", "esco","imain", "idisc", "jmain", "jdisc", "atwork")
fwrite(stopFreq, file.path(outdir, "stopFreqOutProfile.csv"))

#### prepare stop frequency input for visualizer ####
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
stopfreq3 <- wtd.hist(tours$INBOUND_STOPS[tours$TOURPURP==3 & tours$FULLY_JOINT==0], breaks = c(seq(0,3, by=1), 9999), freq = NULL, right=FALSE, weight = tours$finalweight[tours$TOURPURP==3 & tours$FULLY_JOINT==0])
# stopfreq4 <- wtd.hist(tours$INBOUND_STOPS[tours$TOURPURP==4 & tours$FULLY_JOINT==0], breaks = c(seq(0,3, by=1), 9999), freq = NULL, right=FALSE, weight = tours$finalweight[tours$TOURPURP==4 & tours$FULLY_JOINT==0])
stopfreqi56 <- wtd.hist(tours$INBOUND_STOPS[tours$TOURPURP>=5 & tours$TOURPURP<=6 & tours$FULLY_JOINT==0], breaks = c(seq(0,3, by=1), 9999), freq = NULL, right=FALSE, weight = tours$finalweight[tours$TOURPURP>=5 & tours$TOURPURP<=6 & tours$FULLY_JOINT==0])
stopfreqi789 <- wtd.hist(tours$INBOUND_STOPS[tours$TOURPURP>=7 & tours$TOURPURP<=9 & tours$FULLY_JOINT==0], breaks = c(seq(0,3, by=1), 9999), freq = NULL, right=FALSE, weight = tours$finalweight[tours$TOURPURP>=7 & tours$TOURPURP<=9 & tours$FULLY_JOINT==0])
stopfreqj56 <- wtd.hist(jtours$INBOUND_STOPS[jtours$JOINT_PURP>=5 & jtours$JOINT_PURP<=6], breaks = c(seq(0,3, by=1), 9999), freq = NULL, right=FALSE, weight = jtours$finalweight[jtours$JOINT_PURP>=5 & jtours$JOINT_PURP<=6])
stopfreqj789 <- wtd.hist(jtours$INBOUND_STOPS[jtours$JOINT_PURP>=7 & jtours$JOINT_PURP<=9], breaks = c(seq(0,3, by=1), 9999), freq = NULL, right=FALSE, weight = jtours$finalweight[jtours$JOINT_PURP>=7 & jtours$JOINT_PURP<=9])
stopfreq10 <- wtd.hist(tours$INBOUND_STOPS[tours$IS_SUBTOUR == 1], breaks = c(seq(0,3, by=1), 9999), freq = NULL, right=FALSE, weight = tours$finalweight[tours$IS_SUBTOUR == 1])

stopFreq <- data.frame(stopfreq1$counts,
                       stopfreq2$counts, 
                       stopfreq3$counts, 
                       # stopfreq4$counts,
                       stopfreq3$counts*0,
                       stopfreqi56$counts, 
                       stopfreqi789$counts,
                       stopfreqj56$counts, 
                       stopfreqj789$counts,
                       stopfreq10$counts)
colnames(stopFreq) <- c("work", "univ", "sch", "esco","imain", "idisc", "jmain", "jdisc", "atwork")
fwrite(stopFreq, file.path(outdir, "stopFreqInbProfile.csv"))

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
fwrite(stopfreqDir_vis, file.path(outdir, "stopfreqDir_vis.csv"), row.names = F)


#Total
stopfreq1 <- wtd.hist(tours$TOTAL_STOPS[tours$TOURPURP==1 & tours$FULLY_JOINT==0], breaks = c(seq(0,6, by=1), 9999), freq = NULL, right=FALSE, weight = tours$finalweight[tours$TOURPURP==1 & tours$FULLY_JOINT==0])
stopfreq2 <- wtd.hist(tours$TOTAL_STOPS[tours$TOURPURP==2 & tours$FULLY_JOINT==0], breaks = c(seq(0,6, by=1), 9999), freq = NULL, right=FALSE, weight = tours$finalweight[tours$TOURPURP==2 & tours$FULLY_JOINT==0])
stopfreq3 <- wtd.hist(tours$TOTAL_STOPS[tours$TOURPURP==3 & tours$FULLY_JOINT==0], breaks = c(seq(0,6, by=1), 9999), freq = NULL, right=FALSE, weight = tours$finalweight[tours$TOURPURP==3 & tours$FULLY_JOINT==0])
# stopfreq4 <- wtd.hist(tours$TOTAL_STOPS[tours$TOURPURP==4 & tours$FULLY_JOINT==0], breaks = c(seq(0,6, by=1), 9999), freq = NULL, right=FALSE, weight = tours$finalweight[tours$TOURPURP==4 & tours$FULLY_JOINT==0])
stopfreqi56 <- wtd.hist(tours$TOTAL_STOPS[tours$TOURPURP>=5 & tours$TOURPURP<=6 & tours$FULLY_JOINT==0], breaks = c(seq(0,6, by=1), 9999), freq = NULL, right=FALSE, weight = tours$finalweight[tours$TOURPURP>=5 & tours$TOURPURP<=6 & tours$FULLY_JOINT==0])
stopfreqi789 <- wtd.hist(tours$TOTAL_STOPS[tours$TOURPURP>=7 & tours$TOURPURP<=9 & tours$FULLY_JOINT==0], breaks = c(seq(0,6, by=1), 9999), freq = NULL, right=FALSE, weight = tours$finalweight[tours$TOURPURP>=7 & tours$TOURPURP<=9 & tours$FULLY_JOINT==0])
stopfreqj56 <- wtd.hist(jtours$TOTAL_STOPS[jtours$JOINT_PURP>=5 & jtours$JOINT_PURP<=6], breaks = c(seq(0,6, by=1), 9999), freq = NULL, right=FALSE, weight = jtours$finalweight[jtours$JOINT_PURP>=5 & jtours$JOINT_PURP<=6])
stopfreqj789 <- wtd.hist(jtours$TOTAL_STOPS[jtours$JOINT_PURP>=7 & jtours$JOINT_PURP<=9], breaks = c(seq(0,6, by=1), 9999), freq = NULL, right=FALSE, weight = jtours$finalweight[jtours$JOINT_PURP>=7 & jtours$JOINT_PURP<=9])
stopfreq10 <- wtd.hist(tours$TOTAL_STOPS[tours$IS_SUBTOUR == 1 & tours$FULLY_JOINT==0], breaks = c(seq(0,6, by=1), 9999), freq = NULL, right=FALSE, weight = tours$finalweight[tours$IS_SUBTOUR == 1 & tours$FULLY_JOINT==0])

stopFreq <- data.frame(stopfreq1$counts,
                       stopfreq2$counts,
                       stopfreq3$counts,
                       stopfreq3$counts*0,
                       # stopfreq4$counts, 
                       stopfreqi56$counts, 
                       stopfreqi789$counts,
                       stopfreqj56$counts, 
                       stopfreqj789$counts,
                       stopfreq10$counts)
colnames(stopFreq) <- c("work", "univ", "sch", "esco","imain", "idisc", "jmain", "jdisc", "atwork")
fwrite(stopFreq, file.path(outdir, "stopFreqTotProfile.csv"))

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

fwrite(stopFreq_vis, file.path(outdir, "stopfreq_total_vis.csv"), row.names = F)

#### Stop purpose X TourPurpose ####
stopfreq1 <- wtd.hist(stops$DEST_PURP[stops$TOURPURP==1 & stops$FULLY_JOINT==0], breaks = c(seq(1,10, by=1), 9999), freq = NULL, right=FALSE, weight = stops$finalweight[stops$TOURPURP==1 & stops$FULLY_JOINT==0])
stopfreq2 <- wtd.hist(stops$DEST_PURP[stops$TOURPURP==2 & stops$FULLY_JOINT==0], breaks = c(seq(1,10, by=1), 9999), freq = NULL, right=FALSE, weight = stops$finalweight[stops$TOURPURP==2 & stops$FULLY_JOINT==0])
stopfreq3 <- wtd.hist(stops$DEST_PURP[stops$TOURPURP==3 & stops$FULLY_JOINT==0], breaks = c(seq(1,10, by=1), 9999), freq = NULL, right=FALSE, weight = stops$finalweight[stops$TOURPURP==3 & stops$FULLY_JOINT==0])
# stopfreq4 <- wtd.hist(stops$DEST_PURP[stops$TOURPURP==4 & stops$FULLY_JOINT==0], breaks = c(seq(1,10, by=1), 9999), freq = NULL, right=FALSE, weight = stops$finalweight[stops$TOURPURP==4 & stops$FULLY_JOINT==0])
stopfreqi56 <- wtd.hist(stops$DEST_PURP[stops$TOURPURP>=5 & stops$TOURPURP<=6 & stops$FULLY_JOINT==0], breaks = c(seq(1,10, by=1), 9999), freq = NULL, right=FALSE, weight = stops$finalweight[stops$TOURPURP>=5 & stops$TOURPURP<=6 & stops$FULLY_JOINT==0])
stopfreqi789 <- wtd.hist(stops$DEST_PURP[stops$TOURPURP>=7 & stops$TOURPURP<=9 & stops$FULLY_JOINT==0], breaks = c(seq(1,10, by=1), 9999), freq = NULL, right=FALSE, weight = stops$finalweight[stops$TOURPURP>=7 & stops$TOURPURP<=9 & stops$FULLY_JOINT==0])
stopfreqj56 <- wtd.hist(jstops$DEST_PURP[jstops$TOURPURP>=5 & jstops$TOURPURP<=6], breaks = c(seq(1,10, by=1), 9999), freq = NULL, right=FALSE, weight = jstops$finalweight[jstops$TOURPURP>=5 & jstops$TOURPURP<=6])
# stopfreqj789 <- wtd.hist(jstops$DEST_PURP[jstops$TOURPURP>=7 & jstops$TOURPURP<=9], breaks = c(seq(1,10, by=1), 9999), freq = NULL, right=FALSE, weight = jstops$finalweight[jstops$TOURPURP>=7 & jstops$TOURPURP<=9])
stopfreq10 <- wtd.hist(stops$DEST_PURP[stops$SUBTOUR==1], breaks = c(seq(1,10, by=1), 9999), freq = NULL, right=FALSE, weight = stops$finalweight[stops$SUBTOUR==1])

stopFreq <- data.frame(stopfreq1$counts,
                       stopfreq2$counts,
                       stopfreq3$counts,
                       stopfreq3$counts*0, # stopfreq4$counts,
                       stopfreqi56$counts,
                       stopfreqi789$counts,
                       stopfreqj56$counts,
                       stopfreq3$counts*0, # stopfreqj789$counts,
                       stopfreq10$counts)
colnames(stopFreq) <- c("work", "univ", "sch", "esco","imain", "idisc", "jmain", "jdisc", "atwork")
fwrite(stopFreq, file.path(outdir, "stopPurposeByTourPurpose.csv"))

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

fwrite(stopFreq_vis, file.path(outdir, "stoppurpose_tourpurpose_vis.csv"), row.names = F)

#Out of direction - Stop Location
stopfreq1 <- wtd.hist(stops$out_dir_dist[stops$TOURPURP==1 & stops$FULLY_JOINT==0], breaks = c(-9999,seq(0,40, by=1), 9999), freq = NULL, right=FALSE, weight = stops$finalweight[stops$TOURPURP==1 & stops$FULLY_JOINT==0])
stopfreq2 <- wtd.hist(stops$out_dir_dist[stops$TOURPURP==2 & stops$FULLY_JOINT==0], breaks = c(-9999,seq(0,40, by=1), 9999), freq = NULL, right=FALSE, weight = stops$finalweight[stops$TOURPURP==2 & stops$FULLY_JOINT==0])
stopfreq3 <- wtd.hist(stops$out_dir_dist[stops$TOURPURP==3 & stops$FULLY_JOINT==0], breaks = c(-9999,seq(0,40, by=1), 9999), freq = NULL, right=FALSE, weight = stops$finalweight[stops$TOURPURP==3 & stops$FULLY_JOINT==0])
# stopfreq4 <- wtd.hist(stops$out_dir_dist[stops$TOURPURP==4 & stops$FULLY_JOINT==0], breaks = c(-9999,seq(0,40, by=1), 9999), freq = NULL, right=FALSE, weight = stops$finalweight[stops$TOURPURP==4 & stops$FULLY_JOINT==0])
stopfreqi56 <- wtd.hist(stops$out_dir_dist[stops$TOURPURP>=5 & stops$TOURPURP<=6 & stops$FULLY_JOINT==0], breaks = c(-9999,seq(0,40, by=1), 9999), freq = NULL, right=FALSE, weight = stops$finalweight[stops$TOURPURP>=5 & stops$TOURPURP<=6 & stops$FULLY_JOINT==0])
stopfreqi789 <- wtd.hist(stops$out_dir_dist[stops$TOURPURP>=7 & stops$TOURPURP<=9 & stops$FULLY_JOINT==0], breaks = c(-9999,seq(0,40, by=1), 9999), freq = NULL, right=FALSE, weight = stops$finalweight[stops$TOURPURP>=7 & stops$TOURPURP<=9 & stops$FULLY_JOINT==0])
stopfreqj56 <- wtd.hist(jstops$out_dir_dist[jstops$TOURPURP>=5 & jstops$TOURPURP<=6], breaks = c(-9999,seq(0,40, by=1), 9999), freq = NULL, right=FALSE, weight = jstops$finalweight[jstops$TOURPURP>=5 & jstops$TOURPURP<=6])
# stopfreqj789 <- wtd.hist(jstops$out_dir_dist[jstops$TOURPURP>=7 & jstops$TOURPURP<=9], breaks = c(-9999,seq(0,40, by=1), 9999), freq = NULL, right=FALSE, weight = jstops$finalweight[jstops$TOURPURP>=7 & jstops$TOURPURP<=9])
stopfreq10 <- wtd.hist(stops$out_dir_dist[stops$SUBTOUR==1], breaks = c(-9999,seq(0,40, by=1), 9999), freq = NULL, right=FALSE, weight = stops$finalweight[stops$SUBTOUR==1])

stopFreq <- data.frame(stopfreq1$counts,
                       stopfreq2$counts,
                       stopfreq3$counts, 
                       stopfreq3$counts*0, 
                       # stopfreq4$counts, 
                       stopfreqi56$counts,
                       stopfreqi789$counts,
                       stopfreqj56$counts,
                       stopfreq3$counts*0,# stopfreqj789$counts,
                       stopfreq10$counts)
colnames(stopFreq) <- c("work", "univ", "sch", "esco","imain", "idisc", "jmain", "jdisc", "atwork")
fwrite(stopFreq, file.path(outdir, "stopOutOfDirectionDC.csv"))

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

fwrite(stopDC_vis, file.path(outdir, "stopDC_vis.csv"), row.names = F)

#### compute average out of dir distance for visualizer ####
avgDistances <- c(weighted.mean(stops$out_dir_dist[stops$TOURPURP==1 & stops$FULLY_JOINT==0], weight = stops$finalweight[stops$TOURPURP==1 & stops$FULLY_JOINT==0], na.rm = TRUE),
                  weighted.mean(stops$out_dir_dist[stops$TOURPURP==2 & stops$FULLY_JOINT==0], weight = stops$finalweight[stops$TOURPURP==2 & stops$FULLY_JOINT==0], na.rm = TRUE),
                  weighted.mean(stops$out_dir_dist[stops$TOURPURP==3 & stops$FULLY_JOINT==0], weight = stops$finalweight[stops$TOURPURP==3 & stops$FULLY_JOINT==0], na.rm = TRUE),
                  weighted.mean(stops$out_dir_dist[stops$TOURPURP==4 & stops$FULLY_JOINT==0], weight = stops$finalweight[stops$TOURPURP==4 & stops$FULLY_JOINT==0], na.rm = TRUE),
                  weighted.mean(stops$out_dir_dist[stops$TOURPURP>=5 & stops$TOURPURP<=6 & stops$FULLY_JOINT==0], weight = stops$finalweight[stops$TOURPURP>=5 & stops$TOURPURP<=6 & stops$FULLY_JOINT==0], na.rm = TRUE),
                  weighted.mean(stops$out_dir_dist[stops$TOURPURP>=7 & stops$TOURPURP<=9 & stops$FULLY_JOINT==0], weight = stops$finalweight[stops$TOURPURP>=7 & stops$TOURPURP<=9 & stops$FULLY_JOINT==0], na.rm = TRUE),
                  weighted.mean(jstops$out_dir_dist[jstops$TOURPURP>=5 & jstops$TOURPURP<=6], weight = jstops$finalweight[jstops$TOURPURP>=5 & jstops$TOURPURP<=6], na.rm = TRUE),
                  weighted.mean(jstops$out_dir_dist[jstops$TOURPURP>=7 & jstops$TOURPURP<=9], weight = jstops$finalweight[jstops$TOURPURP>=7 & jstops$TOURPURP<=9], na.rm = TRUE),
                  weighted.mean(stops$out_dir_dist[stops$SUBTOUR==1], weight = stops$finalweight[stops$SUBTOUR==1], na.rm = TRUE),
                  weighted.mean(stops$out_dir_dist, weight = stops$finalweight, na.rm = TRUE))

purp <- c("work", "univ", "sch", "esco","imain", "idisc", "jmain", "jdisc", "atwork", "total")

avgStopOutofDirectionDist <- data.frame(purpose = purp, avgDist = avgDistances)

fwrite(avgStopOutofDirectionDist, file.path(outdir, "avgStopOutofDirectionDist_vis.csv"), row.names = F)


#### Stop Departure Time ####
stopfreq1 <- wtd.hist(stops$DEST_DEP_BIN[stops$TOURPURP==1 & stops$FULLY_JOINT==0], breaks = c(seq(1,40, by=1), 9999), freq = NULL, right=FALSE, weight = stops$finalweight[stops$TOURPURP==1 & stops$FULLY_JOINT==0])
stopfreq2 <- wtd.hist(stops$DEST_DEP_BIN[stops$TOURPURP==2 & stops$FULLY_JOINT==0], breaks = c(seq(1,40, by=1), 9999), freq = NULL, right=FALSE, weight = stops$finalweight[stops$TOURPURP==2 & stops$FULLY_JOINT==0])
stopfreq3 <- wtd.hist(stops$DEST_DEP_BIN[stops$TOURPURP==3 & stops$FULLY_JOINT==0], breaks = c(seq(1,40, by=1), 9999), freq = NULL, right=FALSE, weight = stops$finalweight[stops$TOURPURP==3 & stops$FULLY_JOINT==0])
# stopfreq4 <- wtd.hist(stops$DEST_DEP_BIN[stops$TOURPURP==4 & stops$FULLY_JOINT==0], breaks = c(seq(1,40, by=1), 9999), freq = NULL, right=FALSE, weight = stops$finalweight[stops$TOURPURP==4 & stops$FULLY_JOINT==0])
stopfreqi56 <- wtd.hist(stops$DEST_DEP_BIN[stops$TOURPURP>=5 & stops$TOURPURP<=6 & stops$FULLY_JOINT==0], breaks = c(seq(1,40, by=1), 9999), freq = NULL, right=FALSE, weight = stops$finalweight[stops$TOURPURP>=5 & stops$TOURPURP<=6 & stops$FULLY_JOINT==0])
stopfreqi789 <- wtd.hist(stops$DEST_DEP_BIN[stops$TOURPURP>=7 & stops$TOURPURP<=9 & stops$FULLY_JOINT==0], breaks = c(seq(1,40, by=1), 9999), freq = NULL, right=FALSE, weight = stops$finalweight[stops$TOURPURP>=7 & stops$TOURPURP<=9 & stops$FULLY_JOINT==0])
stopfreqj56 <- wtd.hist(jstops$DEST_DEP_BIN[jstops$TOURPURP>=5 & jstops$TOURPURP<=6], breaks = c(seq(1,40, by=1), 9999), freq = NULL, right=FALSE, weight = jstops$finalweight[jstops$TOURPURP>=5 & jstops$TOURPURP<=6])
# stopfreqj789 <- wtd.hist(jstops$DEST_DEP_BIN[jstops$TOURPURP>=7 & jstops$TOURPURP<=9], breaks = c(seq(1,40, by=1), 9999), freq = NULL, right=FALSE, weight = jstops$finalweight[jstops$TOURPURP>=7 & jstops$TOURPURP<=9])
stopfreq10 <- wtd.hist(stops$DEST_DEP_BIN[stops$SUBTOUR==1], breaks = c(seq(1,40, by=1), 9999), freq = NULL, right=FALSE, weight = stops$finalweight[stops$SUBTOUR==1])

stopFreq <- data.frame(stopfreq1$counts, 
                       stopfreq2$counts,
                       stopfreq3$counts,
                       stopfreq3$counts*0, # stopfreq4$counts,
                       stopfreqi56$counts,
                       stopfreqi789$counts,
                       stopfreqj56$counts,
                       stopfreq3$counts*0, # stopfreqj789$counts,
                       stopfreq10$counts)
colnames(stopFreq) <- c("work", "univ", "sch", "esco","imain", "idisc", "jmain", "jdisc", "atwork")
fwrite(stopFreq, file.path(outdir, "stopDeparture.csv"))

#### prepare stop departure input for visualizer ####
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
stopfreq3 <- wtd.hist(trips$ORIG_DEP_BIN[trips$TOURPURP==3 & trips$FULLY_JOINT==0], breaks = c(seq(1,40, by=1), 9999), freq = NULL, right=FALSE, weight = trips$finalweight[trips$TOURPURP==3 & trips$FULLY_JOINT==0])
# stopfreq4 <- wtd.hist(trips$ORIG_DEP_BIN[trips$TOURPURP==4 & trips$FULLY_JOINT==0], breaks = c(seq(1,40, by=1), 9999), freq = NULL, right=FALSE, weight = trips$finalweight[trips$TOURPURP==4 & trips$FULLY_JOINT==0])
stopfreqi56 <- wtd.hist(trips$ORIG_DEP_BIN[trips$TOURPURP>=5 & trips$TOURPURP<=6 & trips$FULLY_JOINT==0], breaks = c(seq(1,40, by=1), 9999), freq = NULL, right=FALSE, weight = trips$finalweight[trips$TOURPURP>=5 & trips$TOURPURP<=6 & trips$FULLY_JOINT==0])
stopfreqi789 <- wtd.hist(trips$ORIG_DEP_BIN[trips$TOURPURP>=7 & trips$TOURPURP<=9 & trips$FULLY_JOINT==0], breaks = c(seq(1,40, by=1), 9999), freq = NULL, right=FALSE, weight = trips$finalweight[trips$TOURPURP>=7 & trips$TOURPURP<=9 & trips$FULLY_JOINT==0])
stopfreqj56 <- wtd.hist(jtrips$ORIG_DEP_BIN[jtrips$TOURPURP>=5 & jtrips$TOURPURP<=6], breaks = c(seq(1,40, by=1), 9999), freq = NULL, right=FALSE, weight = jtrips$finalweight[jtrips$TOURPURP>=5 & jtrips$TOURPURP<=6])
stopfreqj789 <- wtd.hist(jtrips$ORIG_DEP_BIN[jtrips$TOURPURP>=7 & jtrips$TOURPURP<=9], breaks = c(seq(1,40, by=1), 9999), freq = NULL, right=FALSE, weight = jtrips$finalweight[jtrips$TOURPURP>=7 & jtrips$TOURPURP<=9])
stopfreq10 <- wtd.hist(trips$ORIG_DEP_BIN[trips$SUBTOUR==1], breaks = c(seq(1,40, by=1), 9999), freq = NULL, right=FALSE, weight = trips$finalweight[trips$SUBTOUR==1])

stopFreq <- data.frame(stopfreq1$counts, stopfreq2$counts, 
                       stopfreq3$counts,
                       stopfreq3$counts*0,
                       # stopfreq4$counts, 
                       stopfreqi56$counts
                       , stopfreqi789$counts, stopfreqj56$counts, stopfreqj789$counts, stopfreq10$counts)
colnames(stopFreq) <- c("work", "univ", "sch", "esco","imain", "idisc", "jmain", "jdisc", "atwork")
fwrite(stopFreq, file.path(outdir, "tripDeparture.csv"))

#### prepare stop departure input for visualizer ####
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


stopTripDep_vis <- merge(stopDep_vis, 
                         tripDep_vis,
                         by=c('timebin','PURPOSE'), 
                         suffixes = c('_stop','_trip'),
                         all=TRUE)

# stopTripDep_vis <- data.frame(stopDep_vis[1:10,],
#                               tripDep_vis[1:10,]$freq)
stopTripDep_vis[is.na(stopTripDep_vis$freq_trip),'freq_trip'] <- 0


colnames(stopTripDep_vis) <- c("timebin", "purpose", "freq_stop", "freq_trip")
fwrite(stopTripDep_vis, file.path(outdir, "stopTripDep_vis.csv"), row.names = F)

#### Trip Mode Summary ####

#avg occupancy for SR3+ mode - for Joel (02/26/2019 - shared AV implementation meeting at SANDAG)
# trips_sr3 <- trips[trips$TRIPMODE==3,]
# trips_sr3_indiv <- trips_sr3[trips_sr3$FULLY_JOINT==0,]
# trips_sr3_indiv$trips_occ <- trips_sr3_indiv$finalweight * trips_sr3_indiv$AUTO_OCC
# av_occ_sr3_indiv <- sum(trips_sr3_1$trips_occ)/sum(trips_sr3_1$finalweight)

#put TAXI mode into SR2 for both tourmode and tripmode
#trips$TRIPMODE <- ifelse(trips$TRIPMODE==19, 3, trips$TRIPMODE)
#trips$TOURMODE <- ifelse(trips$TOURMODE==10, 2, trips$TOURMODE)

#### trip Work ####
tripmode1 <- wtd.hist(trips$TRIPMODE[!is.na(trips$TRIPMODE) & trips$TRIPMODE>0 & trips$TOURPURP==1 & trips$FULLY_JOINT==0 & trips$TOURMODE==1], breaks = seq(1,14, by=1), freq = NULL, right=FALSE, weight = trips$finalweight[!is.na(trips$TRIPMODE) & trips$TRIPMODE>0  & trips$TOURPURP==1 & trips$FULLY_JOINT==0 & trips$TOURMODE==1])
tripmode2 <- wtd.hist(trips$TRIPMODE[!is.na(trips$TRIPMODE) & trips$TRIPMODE>0 & trips$TOURPURP==1 & trips$FULLY_JOINT==0 & trips$TOURMODE==2], breaks = seq(1,14, by=1), freq = NULL, right=FALSE, weight = trips$finalweight[!is.na(trips$TRIPMODE) & trips$TRIPMODE>0  & trips$TOURPURP==1 & trips$FULLY_JOINT==0 & trips$TOURMODE==2])
tripmode3 <- wtd.hist(trips$TRIPMODE[!is.na(trips$TRIPMODE) & trips$TRIPMODE>0 & trips$TOURPURP==1 & trips$FULLY_JOINT==0 & trips$TOURMODE==3], breaks = seq(1,14, by=1), freq = NULL, right=FALSE, weight = trips$finalweight[!is.na(trips$TRIPMODE) & trips$TRIPMODE>0  & trips$TOURPURP==1 & trips$FULLY_JOINT==0 & trips$TOURMODE==3])
tripmode4 <- wtd.hist(trips$TRIPMODE[!is.na(trips$TRIPMODE) & trips$TRIPMODE>0 & trips$TOURPURP==1 & trips$FULLY_JOINT==0 & trips$TOURMODE==4], breaks = seq(1,14, by=1), freq = NULL, right=FALSE, weight = trips$finalweight[!is.na(trips$TRIPMODE) & trips$TRIPMODE>0  & trips$TOURPURP==1 & trips$FULLY_JOINT==0 & trips$TOURMODE==4])
tripmode5 <- wtd.hist(trips$TRIPMODE[!is.na(trips$TRIPMODE) & trips$TRIPMODE>0 & trips$TOURPURP==1 & trips$FULLY_JOINT==0 & trips$TOURMODE==5], breaks = seq(1,14, by=1), freq = NULL, right=FALSE, weight = trips$finalweight[!is.na(trips$TRIPMODE) & trips$TRIPMODE>0  & trips$TOURPURP==1 & trips$FULLY_JOINT==0 & trips$TOURMODE==5])
tripmode6 <- wtd.hist(trips$TRIPMODE[!is.na(trips$TRIPMODE) & trips$TRIPMODE>0 & trips$TOURPURP==1 & trips$FULLY_JOINT==0 & trips$TOURMODE==6], breaks = seq(1,14, by=1), freq = NULL, right=FALSE, weight = trips$finalweight[!is.na(trips$TRIPMODE) & trips$TRIPMODE>0  & trips$TOURPURP==1 & trips$FULLY_JOINT==0 & trips$TOURMODE==6])
tripmode7 <- wtd.hist(trips$TRIPMODE[!is.na(trips$TRIPMODE) & trips$TRIPMODE>0 & trips$TOURPURP==1 & trips$FULLY_JOINT==0 & trips$TOURMODE==7], breaks = seq(1,14, by=1), freq = NULL, right=FALSE, weight = trips$finalweight[!is.na(trips$TRIPMODE) & trips$TRIPMODE>0  & trips$TOURPURP==1 & trips$FULLY_JOINT==0 & trips$TOURMODE==7])
tripmode8 <- wtd.hist(trips$TRIPMODE[!is.na(trips$TRIPMODE) & trips$TRIPMODE>0 & trips$TOURPURP==1 & trips$FULLY_JOINT==0 & trips$TOURMODE==8], breaks = seq(1,14, by=1), freq = NULL, right=FALSE, weight = trips$finalweight[!is.na(trips$TRIPMODE) & trips$TRIPMODE>0  & trips$TOURPURP==1 & trips$FULLY_JOINT==0 & trips$TOURMODE==8])
tripmode9 <- wtd.hist(trips$TRIPMODE[!is.na(trips$TRIPMODE) & trips$TRIPMODE>0 & trips$TOURPURP==1 & trips$FULLY_JOINT==0 & trips$TOURMODE==9], breaks = seq(1,14, by=1), freq = NULL, right=FALSE, weight = trips$finalweight[!is.na(trips$TRIPMODE) & trips$TRIPMODE>0  & trips$TOURPURP==1 & trips$FULLY_JOINT==0 & trips$TOURMODE==9])
tripmode10 <- wtd.hist(trips$TRIPMODE[!is.na(trips$TRIPMODE) & trips$TRIPMODE>0  & trips$TOURPURP==1 & trips$FULLY_JOINT==0 & trips$TOURMODE==10], breaks = seq(1,14, by=1), freq = NULL, right=FALSE, weight = trips$finalweight[!is.na(trips$TRIPMODE) & trips$TRIPMODE>0  & trips$TOURPURP==1 & trips$FULLY_JOINT==0 & trips$TOURMODE==10])
tripmode11 <- wtd.hist(trips$TRIPMODE[!is.na(trips$TRIPMODE) & trips$TRIPMODE>0  & trips$TOURPURP==1 & trips$FULLY_JOINT==0 & trips$TOURMODE==11], breaks = seq(1,14, by=1), freq = NULL, right=FALSE, weight = trips$finalweight[!is.na(trips$TRIPMODE) & trips$TRIPMODE>0  & trips$TOURPURP==1 & trips$FULLY_JOINT==0 & trips$TOURMODE==11])
tripmode12 <- wtd.hist(trips$TRIPMODE[!is.na(trips$TRIPMODE) & trips$TRIPMODE>0  & trips$TOURPURP==1 & trips$FULLY_JOINT==0 & trips$TOURMODE==12], breaks = seq(1,14, by=1), freq = NULL, right=FALSE, weight = trips$finalweight[!is.na(trips$TRIPMODE) & trips$TRIPMODE>0  & trips$TOURPURP==1 & trips$FULLY_JOINT==0 & trips$TOURMODE==12])
tripmode13 <- wtd.hist(trips$TRIPMODE[!is.na(trips$TRIPMODE) & trips$TRIPMODE>0  & trips$TOURPURP==1 & trips$FULLY_JOINT==0 & trips$TOURMODE==13], breaks = seq(1,14, by=1), freq = NULL, right=FALSE, weight = trips$finalweight[!is.na(trips$TRIPMODE) & trips$TRIPMODE>0  & trips$TOURPURP==1 & trips$FULLY_JOINT==0 & trips$TOURMODE==13])

tripModeProfile <- data.frame(tripmode1$counts, tripmode2$counts, tripmode3$counts, tripmode4$counts,
                              tripmode5$counts, tripmode6$counts, tripmode7$counts, tripmode8$counts, tripmode9$counts,
                              tripmode10$counts, tripmode11$counts, tripmode12$counts, tripmode13$counts)
colnames(tripModeProfile) <- c("tourmode1", "tourmode2", "tourmode3", "tourmode4", "tourmode5", "tourmode6", "tourmode7", "tourmode8", "tourmode9", "tourmode10", "tourmode11", "tourmode12", "tourmode13")

fwrite(tripModeProfile, file.path(outdir, "tripModeProfile_Work.csv"))

# Prepeare data for visualizer
tripModeProfile1_vis <- tripModeProfile #[1:9,]
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
                                                              
#### Univ ####
tripmode1 <- wtd.hist(trips$TRIPMODE[!is.na(trips$TRIPMODE) & trips$TRIPMODE>0  & trips$TOURPURP==2 & trips$FULLY_JOINT==0 & trips$TOURMODE==1], breaks = seq(1,14, by=1), freq = NULL, right=FALSE, weight = trips$finalweight[!is.na(trips$TRIPMODE) & trips$TRIPMODE>0  & trips$TOURPURP==2 & trips$FULLY_JOINT==0 & trips$TOURMODE==1])
tripmode2 <- wtd.hist(trips$TRIPMODE[!is.na(trips$TRIPMODE) & trips$TRIPMODE>0  & trips$TOURPURP==2 & trips$FULLY_JOINT==0 & trips$TOURMODE==2], breaks = seq(1,14, by=1), freq = NULL, right=FALSE, weight = trips$finalweight[!is.na(trips$TRIPMODE) & trips$TRIPMODE>0  & trips$TOURPURP==2 & trips$FULLY_JOINT==0 & trips$TOURMODE==2])
tripmode3 <- wtd.hist(trips$TRIPMODE[!is.na(trips$TRIPMODE) & trips$TRIPMODE>0  & trips$TOURPURP==2 & trips$FULLY_JOINT==0 & trips$TOURMODE==3], breaks = seq(1,14, by=1), freq = NULL, right=FALSE, weight = trips$finalweight[!is.na(trips$TRIPMODE) & trips$TRIPMODE>0  & trips$TOURPURP==2 & trips$FULLY_JOINT==0 & trips$TOURMODE==3])
tripmode4 <- wtd.hist(trips$TRIPMODE[!is.na(trips$TRIPMODE) & trips$TRIPMODE>0  & trips$TOURPURP==2 & trips$FULLY_JOINT==0 & trips$TOURMODE==4], breaks = seq(1,14, by=1), freq = NULL, right=FALSE, weight = trips$finalweight[!is.na(trips$TRIPMODE) & trips$TRIPMODE>0  & trips$TOURPURP==2 & trips$FULLY_JOINT==0 & trips$TOURMODE==4])
tripmode5 <- wtd.hist(trips$TRIPMODE[!is.na(trips$TRIPMODE) & trips$TRIPMODE>0  & trips$TOURPURP==2 & trips$FULLY_JOINT==0 & trips$TOURMODE==5], breaks = seq(1,14, by=1), freq = NULL, right=FALSE, weight = trips$finalweight[!is.na(trips$TRIPMODE) & trips$TRIPMODE>0  & trips$TOURPURP==2 & trips$FULLY_JOINT==0 & trips$TOURMODE==5])
tripmode6 <- wtd.hist(trips$TRIPMODE[!is.na(trips$TRIPMODE) & trips$TRIPMODE>0  & trips$TOURPURP==2 & trips$FULLY_JOINT==0 & trips$TOURMODE==6], breaks = seq(1,14, by=1), freq = NULL, right=FALSE, weight = trips$finalweight[!is.na(trips$TRIPMODE) & trips$TRIPMODE>0  & trips$TOURPURP==2 & trips$FULLY_JOINT==0 & trips$TOURMODE==6])
tripmode7 <- wtd.hist(trips$TRIPMODE[!is.na(trips$TRIPMODE) & trips$TRIPMODE>0  & trips$TOURPURP==2 & trips$FULLY_JOINT==0 & trips$TOURMODE==7], breaks = seq(1,14, by=1), freq = NULL, right=FALSE, weight = trips$finalweight[!is.na(trips$TRIPMODE) & trips$TRIPMODE>0  & trips$TOURPURP==2 & trips$FULLY_JOINT==0 & trips$TOURMODE==7])
tripmode8 <- wtd.hist(trips$TRIPMODE[!is.na(trips$TRIPMODE) & trips$TRIPMODE>0  & trips$TOURPURP==2 & trips$FULLY_JOINT==0 & trips$TOURMODE==8], breaks = seq(1,14, by=1), freq = NULL, right=FALSE, weight = trips$finalweight[!is.na(trips$TRIPMODE) & trips$TRIPMODE>0  & trips$TOURPURP==2 & trips$FULLY_JOINT==0 & trips$TOURMODE==8])
tripmode9 <- wtd.hist(trips$TRIPMODE[!is.na(trips$TRIPMODE) & trips$TRIPMODE>0  & trips$TOURPURP==2 & trips$FULLY_JOINT==0 & trips$TOURMODE==9], breaks = seq(1,14, by=1), freq = NULL, right=FALSE, weight = trips$finalweight[!is.na(trips$TRIPMODE) & trips$TRIPMODE>0  & trips$TOURPURP==2 & trips$FULLY_JOINT==0 & trips$TOURMODE==9])
tripmode10 <- wtd.hist(trips$TRIPMODE[!is.na(trips$TRIPMODE) & trips$TRIPMODE>0  & trips$TOURPURP==2 & trips$FULLY_JOINT==0 & trips$TOURMODE==10], breaks = seq(1,14, by=1), freq = NULL, right=FALSE, weight = trips$finalweight[!is.na(trips$TRIPMODE) & trips$TRIPMODE>0  & trips$TOURPURP==2 & trips$FULLY_JOINT==0 & trips$TOURMODE==10])
tripmode11 <- wtd.hist(trips$TRIPMODE[!is.na(trips$TRIPMODE) & trips$TRIPMODE>0  & trips$TOURPURP==2 & trips$FULLY_JOINT==0 & trips$TOURMODE==11], breaks = seq(1,14, by=1), freq = NULL, right=FALSE, weight = trips$finalweight[!is.na(trips$TRIPMODE) & trips$TRIPMODE>0  & trips$TOURPURP==2 & trips$FULLY_JOINT==0 & trips$TOURMODE==11])
tripmode12 <- wtd.hist(trips$TRIPMODE[!is.na(trips$TRIPMODE) & trips$TRIPMODE>0  & trips$TOURPURP==2 & trips$FULLY_JOINT==0 & trips$TOURMODE==12], breaks = seq(1,14, by=1), freq = NULL, right=FALSE, weight = trips$finalweight[!is.na(trips$TRIPMODE) & trips$TRIPMODE>0  & trips$TOURPURP==2 & trips$FULLY_JOINT==0 & trips$TOURMODE==12])
tripmode13 <- wtd.hist(trips$TRIPMODE[!is.na(trips$TRIPMODE) & trips$TRIPMODE>0  & trips$TOURPURP==2 & trips$FULLY_JOINT==0 & trips$TOURMODE==13], breaks = seq(1,14, by=1), freq = NULL, right=FALSE, weight = trips$finalweight[!is.na(trips$TRIPMODE) & trips$TRIPMODE>0  & trips$TOURPURP==2 & trips$FULLY_JOINT==0 & trips$TOURMODE==13])

tripModeProfile <- data.frame(tripmode1$counts, tripmode2$counts, tripmode3$counts, tripmode4$counts,
                              tripmode5$counts, tripmode6$counts, tripmode7$counts, tripmode8$counts, tripmode9$counts,
                              tripmode10$counts, tripmode11$counts, tripmode12$counts, tripmode13$counts)
colnames(tripModeProfile) <- c("tourmode1", "tourmode2", "tourmode3", "tourmode4", "tourmode5", "tourmode6", "tourmode7", "tourmode8", "tourmode9", "tourmode10", "tourmode11", "tourmode12", "tourmode13")

fwrite(tripModeProfile, file.path(outdir, "tripModeProfile_Univ.csv"))

tripModeProfile2_vis <- tripModeProfile #[1:9,]
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
                                                             
#### School ####
tripmode1 <- wtd.hist(trips$TRIPMODE[!is.na(trips$TRIPMODE) & trips$TRIPMODE>0  & trips$TOURPURP==3 & trips$FULLY_JOINT==0 & trips$TOURMODE==1], breaks = seq(1,14, by=1), freq = NULL, right=FALSE, weight = trips$finalweight[!is.na(trips$TRIPMODE) & trips$TRIPMODE>0  & trips$TOURPURP==3 & trips$FULLY_JOINT==0 & trips$TOURMODE==1])
tripmode2 <- wtd.hist(trips$TRIPMODE[!is.na(trips$TRIPMODE) & trips$TRIPMODE>0  & trips$TOURPURP==3 & trips$FULLY_JOINT==0 & trips$TOURMODE==2], breaks = seq(1,14, by=1), freq = NULL, right=FALSE, weight = trips$finalweight[!is.na(trips$TRIPMODE) & trips$TRIPMODE>0  & trips$TOURPURP==3 & trips$FULLY_JOINT==0 & trips$TOURMODE==2])
tripmode3 <- wtd.hist(trips$TRIPMODE[!is.na(trips$TRIPMODE) & trips$TRIPMODE>0  & trips$TOURPURP==3 & trips$FULLY_JOINT==0 & trips$TOURMODE==3], breaks = seq(1,14, by=1), freq = NULL, right=FALSE, weight = trips$finalweight[!is.na(trips$TRIPMODE) & trips$TRIPMODE>0  & trips$TOURPURP==3 & trips$FULLY_JOINT==0 & trips$TOURMODE==3])
tripmode4 <- wtd.hist(trips$TRIPMODE[!is.na(trips$TRIPMODE) & trips$TRIPMODE>0  & trips$TOURPURP==3 & trips$FULLY_JOINT==0 & trips$TOURMODE==4], breaks = seq(1,14, by=1), freq = NULL, right=FALSE, weight = trips$finalweight[!is.na(trips$TRIPMODE) & trips$TRIPMODE>0  & trips$TOURPURP==3 & trips$FULLY_JOINT==0 & trips$TOURMODE==4])
tripmode5 <- wtd.hist(trips$TRIPMODE[!is.na(trips$TRIPMODE) & trips$TRIPMODE>0  & trips$TOURPURP==3 & trips$FULLY_JOINT==0 & trips$TOURMODE==5], breaks = seq(1,14, by=1), freq = NULL, right=FALSE, weight = trips$finalweight[!is.na(trips$TRIPMODE) & trips$TRIPMODE>0  & trips$TOURPURP==3 & trips$FULLY_JOINT==0 & trips$TOURMODE==5])
tripmode6 <- wtd.hist(trips$TRIPMODE[!is.na(trips$TRIPMODE) & trips$TRIPMODE>0  & trips$TOURPURP==3 & trips$FULLY_JOINT==0 & trips$TOURMODE==6], breaks = seq(1,14, by=1), freq = NULL, right=FALSE, weight = trips$finalweight[!is.na(trips$TRIPMODE) & trips$TRIPMODE>0  & trips$TOURPURP==3 & trips$FULLY_JOINT==0 & trips$TOURMODE==6])
tripmode7 <- wtd.hist(trips$TRIPMODE[!is.na(trips$TRIPMODE) & trips$TRIPMODE>0  & trips$TOURPURP==3 & trips$FULLY_JOINT==0 & trips$TOURMODE==7], breaks = seq(1,14, by=1), freq = NULL, right=FALSE, weight = trips$finalweight[!is.na(trips$TRIPMODE) & trips$TRIPMODE>0  & trips$TOURPURP==3 & trips$FULLY_JOINT==0 & trips$TOURMODE==7])
tripmode8 <- wtd.hist(trips$TRIPMODE[!is.na(trips$TRIPMODE) & trips$TRIPMODE>0  & trips$TOURPURP==3 & trips$FULLY_JOINT==0 & trips$TOURMODE==8], breaks = seq(1,14, by=1), freq = NULL, right=FALSE, weight = trips$finalweight[!is.na(trips$TRIPMODE) & trips$TRIPMODE>0  & trips$TOURPURP==3 & trips$FULLY_JOINT==0 & trips$TOURMODE==8])
tripmode9 <- wtd.hist(trips$TRIPMODE[!is.na(trips$TRIPMODE) & trips$TRIPMODE>0  & trips$TOURPURP==3 & trips$FULLY_JOINT==0 & trips$TOURMODE==9], breaks = seq(1,14, by=1), freq = NULL, right=FALSE, weight = trips$finalweight[!is.na(trips$TRIPMODE) & trips$TRIPMODE>0  & trips$TOURPURP==3 & trips$FULLY_JOINT==0 & trips$TOURMODE==9])
tripmode10 <- wtd.hist(trips$TRIPMODE[!is.na(trips$TRIPMODE) & trips$TRIPMODE>0  & trips$TOURPURP==3 & trips$FULLY_JOINT==0 & trips$TOURMODE==10], breaks = seq(1,14, by=1), freq = NULL, right=FALSE, weight = trips$finalweight[!is.na(trips$TRIPMODE) & trips$TRIPMODE>0  & trips$TOURPURP==3 & trips$FULLY_JOINT==0 & trips$TOURMODE==10])
tripmode11 <- wtd.hist(trips$TRIPMODE[!is.na(trips$TRIPMODE) & trips$TRIPMODE>0  & trips$TOURPURP==3 & trips$FULLY_JOINT==0 & trips$TOURMODE==11], breaks = seq(1,14, by=1), freq = NULL, right=FALSE, weight = trips$finalweight[!is.na(trips$TRIPMODE) & trips$TRIPMODE>0  & trips$TOURPURP==3 & trips$FULLY_JOINT==0 & trips$TOURMODE==11])
tripmode12 <- wtd.hist(trips$TRIPMODE[!is.na(trips$TRIPMODE) & trips$TRIPMODE>0  & trips$TOURPURP==3 & trips$FULLY_JOINT==0 & trips$TOURMODE==12], breaks = seq(1,14, by=1), freq = NULL, right=FALSE, weight = trips$finalweight[!is.na(trips$TRIPMODE) & trips$TRIPMODE>0  & trips$TOURPURP==3 & trips$FULLY_JOINT==0 & trips$TOURMODE==12])
tripmode13 <- wtd.hist(trips$TRIPMODE[!is.na(trips$TRIPMODE) & trips$TRIPMODE>0  & trips$TOURPURP==3 & trips$FULLY_JOINT==0 & trips$TOURMODE==13], breaks = seq(1,14, by=1), freq = NULL, right=FALSE, weight = trips$finalweight[!is.na(trips$TRIPMODE) & trips$TRIPMODE>0  & trips$TOURPURP==3 & trips$FULLY_JOINT==0 & trips$TOURMODE==13])

tripModeProfile <- data.frame(tripmode1$counts, tripmode2$counts, tripmode3$counts, tripmode4$counts,
                              tripmode5$counts, tripmode6$counts, tripmode7$counts, tripmode8$counts, tripmode9$counts,
                              tripmode10$counts, tripmode11$counts, tripmode12$counts, tripmode13$counts)
colnames(tripModeProfile) <- c("tourmode1", "tourmode2", "tourmode3", "tourmode4", "tourmode5", "tourmode6", "tourmode7", "tourmode8", "tourmode9", "tourmode10", "tourmode11", "tourmode12", "tourmode13")

fwrite(tripModeProfile, file.path(outdir, "tripModeProfile_Schl.csv"))

tripModeProfile3_vis <- tripModeProfile #[1:9,]
tripModeProfile3_vis$id <- row.names(tripModeProfile3_vis)
tripModeProfile3_vis <- melt(tripModeProfile3_vis, id = c("id"))
colnames(tripModeProfile3_vis) <- c("id", "purpose", "freq3")

tripModeProfile3_vis <- xtabs(freq3~id+purpose, tripModeProfile3_vis)
tripModeProfile3_vis[is.na(tripModeProfile3_vis)] <- 0
tripModeProfile3_vis <- addmargins(as.table(tripModeProfile3_vis))
tripModeProfile3_vis <- as.data.frame.matrix(tripModeProfile3_vis)

tripModeProfile3_vis$id <- row.names(tripModeProfile3_vis)
tripModeProfile3_vis <- melt(tripModeProfile3_vis, id = c("id"))
colnames(tripModeProfile3_vis) <- c("id", "purpose", "freq3")
tripModeProfile3_vis$id <- as.character(tripModeProfile3_vis$id)
tripModeProfile3_vis$purpose <- as.character(tripModeProfile3_vis$purpose)
tripModeProfile3_vis <- tripModeProfile3_vis[tripModeProfile3_vis$id!="Sum",]
tripModeProfile3_vis$purpose[tripModeProfile3_vis$purpose=="Sum"] <- "Total"
                                                             
#### iMain ####                                                     
tripmode1 <- wtd.hist(trips$TRIPMODE[!is.na(trips$TRIPMODE) & trips$TRIPMODE>0  & trips$TOURPURP>=4 & trips$TOURPURP<=6 & trips$FULLY_JOINT==0 & trips$TOURMODE==1], breaks = seq(1,14, by=1), freq = NULL, right=FALSE, weight = trips$finalweight[!is.na(trips$TRIPMODE) & trips$TRIPMODE>0  & trips$TOURPURP>=4 & trips$TOURPURP<=6 & trips$FULLY_JOINT==0 & trips$TOURMODE==1])
tripmode2 <- wtd.hist(trips$TRIPMODE[!is.na(trips$TRIPMODE) & trips$TRIPMODE>0  & trips$TOURPURP>=4 & trips$TOURPURP<=6 & trips$FULLY_JOINT==0 & trips$TOURMODE==2], breaks = seq(1,14, by=1), freq = NULL, right=FALSE, weight = trips$finalweight[!is.na(trips$TRIPMODE) & trips$TRIPMODE>0  & trips$TOURPURP>=4 & trips$TOURPURP<=6 & trips$FULLY_JOINT==0 & trips$TOURMODE==2])
tripmode3 <- wtd.hist(trips$TRIPMODE[!is.na(trips$TRIPMODE) & trips$TRIPMODE>0  & trips$TOURPURP>=4 & trips$TOURPURP<=6 & trips$FULLY_JOINT==0 & trips$TOURMODE==3], breaks = seq(1,14, by=1), freq = NULL, right=FALSE, weight = trips$finalweight[!is.na(trips$TRIPMODE) & trips$TRIPMODE>0  & trips$TOURPURP>=4 & trips$TOURPURP<=6 & trips$FULLY_JOINT==0 & trips$TOURMODE==3])
tripmode4 <- wtd.hist(trips$TRIPMODE[!is.na(trips$TRIPMODE) & trips$TRIPMODE>0  & trips$TOURPURP>=4 & trips$TOURPURP<=6 & trips$FULLY_JOINT==0 & trips$TOURMODE==4], breaks = seq(1,14, by=1), freq = NULL, right=FALSE, weight = trips$finalweight[!is.na(trips$TRIPMODE) & trips$TRIPMODE>0  & trips$TOURPURP>=4 & trips$TOURPURP<=6 & trips$FULLY_JOINT==0 & trips$TOURMODE==4])
tripmode5 <- wtd.hist(trips$TRIPMODE[!is.na(trips$TRIPMODE) & trips$TRIPMODE>0  & trips$TOURPURP>=4 & trips$TOURPURP<=6 & trips$FULLY_JOINT==0 & trips$TOURMODE==5], breaks = seq(1,14, by=1), freq = NULL, right=FALSE, weight = trips$finalweight[!is.na(trips$TRIPMODE) & trips$TRIPMODE>0  & trips$TOURPURP>=4 & trips$TOURPURP<=6 & trips$FULLY_JOINT==0 & trips$TOURMODE==5])
tripmode6 <- wtd.hist(trips$TRIPMODE[!is.na(trips$TRIPMODE) & trips$TRIPMODE>0  & trips$TOURPURP>=4 & trips$TOURPURP<=6 & trips$FULLY_JOINT==0 & trips$TOURMODE==6], breaks = seq(1,14, by=1), freq = NULL, right=FALSE, weight = trips$finalweight[!is.na(trips$TRIPMODE) & trips$TRIPMODE>0  & trips$TOURPURP>=4 & trips$TOURPURP<=6 & trips$FULLY_JOINT==0 & trips$TOURMODE==6])
tripmode7 <- wtd.hist(trips$TRIPMODE[!is.na(trips$TRIPMODE) & trips$TRIPMODE>0  & trips$TOURPURP>=4 & trips$TOURPURP<=6 & trips$FULLY_JOINT==0 & trips$TOURMODE==7], breaks = seq(1,14, by=1), freq = NULL, right=FALSE, weight = trips$finalweight[!is.na(trips$TRIPMODE) & trips$TRIPMODE>0  & trips$TOURPURP>=4 & trips$TOURPURP<=6 & trips$FULLY_JOINT==0 & trips$TOURMODE==7])
tripmode8 <- wtd.hist(trips$TRIPMODE[!is.na(trips$TRIPMODE) & trips$TRIPMODE>0  & trips$TOURPURP>=4 & trips$TOURPURP<=6 & trips$FULLY_JOINT==0 & trips$TOURMODE==8], breaks = seq(1,14, by=1), freq = NULL, right=FALSE, weight = trips$finalweight[!is.na(trips$TRIPMODE) & trips$TRIPMODE>0  & trips$TOURPURP>=4 & trips$TOURPURP<=6 & trips$FULLY_JOINT==0 & trips$TOURMODE==8])
tripmode9 <- wtd.hist(trips$TRIPMODE[!is.na(trips$TRIPMODE) & trips$TRIPMODE>0  & trips$TOURPURP>=4 & trips$TOURPURP<=6 & trips$FULLY_JOINT==0 & trips$TOURMODE==9], breaks = seq(1,14, by=1), freq = NULL, right=FALSE, weight = trips$finalweight[!is.na(trips$TRIPMODE) & trips$TRIPMODE>0  & trips$TOURPURP>=4 & trips$TOURPURP<=6 & trips$FULLY_JOINT==0 & trips$TOURMODE==9])
tripmode10 <- wtd.hist(trips$TRIPMODE[!is.na(trips$TRIPMODE) & trips$TRIPMODE>0  & trips$TOURPURP>=4 & trips$TOURPURP<=6 & trips$FULLY_JOINT==0 & trips$TOURMODE==10], breaks = seq(1,14, by=1), freq = NULL, right=FALSE, weight = trips$finalweight[!is.na(trips$TRIPMODE) & trips$TRIPMODE>0  & trips$TOURPURP>=4 & trips$TOURPURP<=6 & trips$FULLY_JOINT==0  & trips$TOURMODE==10])
tripmode11 <- wtd.hist(trips$TRIPMODE[!is.na(trips$TRIPMODE) & trips$TRIPMODE>0  & trips$TOURPURP>=4 & trips$TOURPURP<=6 & trips$FULLY_JOINT==0 & trips$TOURMODE==11], breaks = seq(1,14, by=1), freq = NULL, right=FALSE, weight = trips$finalweight[!is.na(trips$TRIPMODE) & trips$TRIPMODE>0  & trips$TOURPURP>=4 & trips$TOURPURP<=6 & trips$FULLY_JOINT==0  & trips$TOURMODE==11])
tripmode12 <- wtd.hist(trips$TRIPMODE[!is.na(trips$TRIPMODE) & trips$TRIPMODE>0  & trips$TOURPURP>=4 & trips$TOURPURP<=6 & trips$FULLY_JOINT==0 & trips$TOURMODE==12], breaks = seq(1,14, by=1), freq = NULL, right=FALSE, weight = trips$finalweight[!is.na(trips$TRIPMODE) & trips$TRIPMODE>0  & trips$TOURPURP>=4 & trips$TOURPURP<=6 & trips$FULLY_JOINT==0  & trips$TOURMODE==12])
tripmode13 <- wtd.hist(trips$TRIPMODE[!is.na(trips$TRIPMODE) & trips$TRIPMODE>0  & trips$TOURPURP>=4 & trips$TOURPURP<=6 & trips$FULLY_JOINT==0 & trips$TOURMODE==13], breaks = seq(1,14, by=1), freq = NULL, right=FALSE, weight = trips$finalweight[!is.na(trips$TRIPMODE) & trips$TRIPMODE>0  & trips$TOURPURP>=4 & trips$TOURPURP<=6 & trips$FULLY_JOINT==0  & trips$TOURMODE==13])

tripModeProfile <- data.frame(tripmode1$counts, tripmode2$counts, tripmode3$counts, tripmode4$counts,
                              tripmode5$counts, tripmode6$counts, tripmode7$counts, tripmode8$counts, tripmode9$counts,
                              tripmode10$counts, tripmode11$counts, tripmode12$counts, tripmode13$counts)
colnames(tripModeProfile) <- c("tourmode1", "tourmode2", "tourmode3", "tourmode4", "tourmode5", "tourmode6", "tourmode7", "tourmode8", "tourmode9", "tourmode10", "tourmode11", "tourmode12", "tourmode13")

fwrite(tripModeProfile, file.path(outdir, "tripModeProfile_iMain.csv"))

tripModeProfile4_vis <- tripModeProfile #[1:9,]
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

#### trip iDisc ####
tripmode1 <- wtd.hist(trips$TRIPMODE[!is.na(trips$TRIPMODE) & trips$TRIPMODE>0  & trips$TOURPURP>=7 & trips$TOURPURP<=9 & trips$FULLY_JOINT==0 & trips$TOURMODE==1], breaks = seq(1,14, by=1), freq = NULL, right=FALSE, weight = trips$finalweight[!is.na(trips$TRIPMODE) & trips$TRIPMODE>0  & trips$TOURPURP>=7 & trips$TOURPURP<=9 & trips$FULLY_JOINT==0 & trips$TOURMODE==1])
tripmode2 <- wtd.hist(trips$TRIPMODE[!is.na(trips$TRIPMODE) & trips$TRIPMODE>0  & trips$TOURPURP>=7 & trips$TOURPURP<=9 & trips$FULLY_JOINT==0 & trips$TOURMODE==2], breaks = seq(1,14, by=1), freq = NULL, right=FALSE, weight = trips$finalweight[!is.na(trips$TRIPMODE) & trips$TRIPMODE>0  & trips$TOURPURP>=7 & trips$TOURPURP<=9 & trips$FULLY_JOINT==0 & trips$TOURMODE==2])
tripmode3 <- wtd.hist(trips$TRIPMODE[!is.na(trips$TRIPMODE) & trips$TRIPMODE>0  & trips$TOURPURP>=7 & trips$TOURPURP<=9 & trips$FULLY_JOINT==0 & trips$TOURMODE==3], breaks = seq(1,14, by=1), freq = NULL, right=FALSE, weight = trips$finalweight[!is.na(trips$TRIPMODE) & trips$TRIPMODE>0  & trips$TOURPURP>=7 & trips$TOURPURP<=9 & trips$FULLY_JOINT==0 & trips$TOURMODE==3])
tripmode4 <- wtd.hist(trips$TRIPMODE[!is.na(trips$TRIPMODE) & trips$TRIPMODE>0  & trips$TOURPURP>=7 & trips$TOURPURP<=9 & trips$FULLY_JOINT==0 & trips$TOURMODE==4], breaks = seq(1,14, by=1), freq = NULL, right=FALSE, weight = trips$finalweight[!is.na(trips$TRIPMODE) & trips$TRIPMODE>0  & trips$TOURPURP>=7 & trips$TOURPURP<=9 & trips$FULLY_JOINT==0 & trips$TOURMODE==4])
tripmode5 <- wtd.hist(trips$TRIPMODE[!is.na(trips$TRIPMODE) & trips$TRIPMODE>0  & trips$TOURPURP>=7 & trips$TOURPURP<=9 & trips$FULLY_JOINT==0 & trips$TOURMODE==5], breaks = seq(1,14, by=1), freq = NULL, right=FALSE, weight = trips$finalweight[!is.na(trips$TRIPMODE) & trips$TRIPMODE>0  & trips$TOURPURP>=7 & trips$TOURPURP<=9 & trips$FULLY_JOINT==0 & trips$TOURMODE==5])
tripmode6 <- wtd.hist(trips$TRIPMODE[!is.na(trips$TRIPMODE) & trips$TRIPMODE>0  & trips$TOURPURP>=7 & trips$TOURPURP<=9 & trips$FULLY_JOINT==0 & trips$TOURMODE==6], breaks = seq(1,14, by=1), freq = NULL, right=FALSE, weight = trips$finalweight[!is.na(trips$TRIPMODE) & trips$TRIPMODE>0  & trips$TOURPURP>=7 & trips$TOURPURP<=9 & trips$FULLY_JOINT==0 & trips$TOURMODE==6])
tripmode7 <- wtd.hist(trips$TRIPMODE[!is.na(trips$TRIPMODE) & trips$TRIPMODE>0  & trips$TOURPURP>=7 & trips$TOURPURP<=9 & trips$FULLY_JOINT==0 & trips$TOURMODE==7], breaks = seq(1,14, by=1), freq = NULL, right=FALSE, weight = trips$finalweight[!is.na(trips$TRIPMODE) & trips$TRIPMODE>0  & trips$TOURPURP>=7 & trips$TOURPURP<=9 & trips$FULLY_JOINT==0 & trips$TOURMODE==7])
tripmode8 <- wtd.hist(trips$TRIPMODE[!is.na(trips$TRIPMODE) & trips$TRIPMODE>0  & trips$TOURPURP>=7 & trips$TOURPURP<=9 & trips$FULLY_JOINT==0 & trips$TOURMODE==8], breaks = seq(1,14, by=1), freq = NULL, right=FALSE, weight = trips$finalweight[!is.na(trips$TRIPMODE) & trips$TRIPMODE>0  & trips$TOURPURP>=7 & trips$TOURPURP<=9 & trips$FULLY_JOINT==0 & trips$TOURMODE==8])
tripmode9 <- wtd.hist(trips$TRIPMODE[!is.na(trips$TRIPMODE) & trips$TRIPMODE>0  & trips$TOURPURP>=7 & trips$TOURPURP<=9 & trips$FULLY_JOINT==0 & trips$TOURMODE==9], breaks = seq(1,14, by=1), freq = NULL, right=FALSE, weight = trips$finalweight[!is.na(trips$TRIPMODE) & trips$TRIPMODE>0  & trips$TOURPURP>=7 & trips$TOURPURP<=9 & trips$FULLY_JOINT==0 & trips$TOURMODE==9])
tripmode10 <- wtd.hist(trips$TRIPMODE[!is.na(trips$TRIPMODE) & trips$TRIPMODE>0  & trips$TOURPURP>=7 & trips$TOURPURP<=9 & trips$FULLY_JOINT==0  & trips$TOURMODE==10], breaks = seq(1,14, by=1), freq = NULL, right=FALSE, weight = trips$finalweight[!is.na(trips$TRIPMODE) & trips$TRIPMODE>0  & trips$TOURPURP>=7 & trips$TOURPURP<=9 & trips$FULLY_JOINT==0  & trips$TOURMODE==10])
tripmode11 <- wtd.hist(trips$TRIPMODE[!is.na(trips$TRIPMODE) & trips$TRIPMODE>0  & trips$TOURPURP>=7 & trips$TOURPURP<=9 & trips$FULLY_JOINT==0  & trips$TOURMODE==11], breaks = seq(1,14, by=1), freq = NULL, right=FALSE, weight = trips$finalweight[!is.na(trips$TRIPMODE) & trips$TRIPMODE>0  & trips$TOURPURP>=7 & trips$TOURPURP<=9 & trips$FULLY_JOINT==0  & trips$TOURMODE==11])
tripmode12 <- wtd.hist(trips$TRIPMODE[!is.na(trips$TRIPMODE) & trips$TRIPMODE>0  & trips$TOURPURP>=7 & trips$TOURPURP<=9 & trips$FULLY_JOINT==0  & trips$TOURMODE==12], breaks = seq(1,14, by=1), freq = NULL, right=FALSE, weight = trips$finalweight[!is.na(trips$TRIPMODE) & trips$TRIPMODE>0  & trips$TOURPURP>=7 & trips$TOURPURP<=9 & trips$FULLY_JOINT==0  & trips$TOURMODE==12])
tripmode13 <- wtd.hist(trips$TRIPMODE[!is.na(trips$TRIPMODE) & trips$TRIPMODE>0  & trips$TOURPURP>=7 & trips$TOURPURP<=9 & trips$FULLY_JOINT==0  & trips$TOURMODE==13], breaks = seq(1,14, by=1), freq = NULL, right=FALSE, weight = trips$finalweight[!is.na(trips$TRIPMODE) & trips$TRIPMODE>0  & trips$TOURPURP>=7 & trips$TOURPURP<=9 & trips$FULLY_JOINT==0  & trips$TOURMODE==13])

tripModeProfile <- data.frame(tripmode1$counts, tripmode2$counts, tripmode3$counts, tripmode4$counts,
                              tripmode5$counts, tripmode6$counts, tripmode7$counts, tripmode8$counts, tripmode9$counts,
                              tripmode10$counts, tripmode11$counts, tripmode12$counts, tripmode13$counts)
colnames(tripModeProfile) <- c("tourmode1", "tourmode2", "tourmode3", "tourmode4", "tourmode5", "tourmode6", "tourmode7", "tourmode8", "tourmode9", "tourmode10", "tourmode11", "tourmode12", "tourmode13")

fwrite(tripModeProfile, file.path(outdir, "tripModeProfile_iDisc.csv"))

tripModeProfile5_vis <- tripModeProfile #[1:9,]
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

#### trip jMain ####                                                      
tripmode1 <- wtd.hist(jtrips$TRIPMODE[!is.na(jtrips$TRIPMODE) & jtrips$TRIPMODE>0  & jtrips$TOURPURP>=4 & jtrips$TOURPURP<=6 & jtrips$TOURMODE==1], breaks = seq(1,14, by=1), freq = NULL, right=FALSE, weight = jtrips$finalweight[!is.na(jtrips$TRIPMODE) & jtrips$TRIPMODE>0  & jtrips$TOURPURP>=4 & jtrips$TOURPURP<=6 & jtrips$TOURMODE==1])
tripmode2 <- wtd.hist(jtrips$TRIPMODE[!is.na(jtrips$TRIPMODE) & jtrips$TRIPMODE>0  & jtrips$TOURPURP>=4 & jtrips$TOURPURP<=6 & jtrips$TOURMODE==2], breaks = seq(1,14, by=1), freq = NULL, right=FALSE, weight = jtrips$finalweight[!is.na(jtrips$TRIPMODE) & jtrips$TRIPMODE>0  & jtrips$TOURPURP>=4 & jtrips$TOURPURP<=6 & jtrips$TOURMODE==2])
tripmode3 <- wtd.hist(jtrips$TRIPMODE[!is.na(jtrips$TRIPMODE) & jtrips$TRIPMODE>0  & jtrips$TOURPURP>=4 & jtrips$TOURPURP<=6 & jtrips$TOURMODE==3], breaks = seq(1,14, by=1), freq = NULL, right=FALSE, weight = jtrips$finalweight[!is.na(jtrips$TRIPMODE) & jtrips$TRIPMODE>0  & jtrips$TOURPURP>=4 & jtrips$TOURPURP<=6 & jtrips$TOURMODE==3])
tripmode4 <- wtd.hist(jtrips$TRIPMODE[!is.na(jtrips$TRIPMODE) & jtrips$TRIPMODE>0  & jtrips$TOURPURP>=4 & jtrips$TOURPURP<=6 & jtrips$TOURMODE==4], breaks = seq(1,14, by=1), freq = NULL, right=FALSE, weight = jtrips$finalweight[!is.na(jtrips$TRIPMODE) & jtrips$TRIPMODE>0  & jtrips$TOURPURP>=4 & jtrips$TOURPURP<=6 & jtrips$TOURMODE==4])
tripmode5 <- wtd.hist(jtrips$TRIPMODE[!is.na(jtrips$TRIPMODE) & jtrips$TRIPMODE>0  & jtrips$TOURPURP>=4 & jtrips$TOURPURP<=6 & jtrips$TOURMODE==5], breaks = seq(1,14, by=1), freq = NULL, right=FALSE, weight = jtrips$finalweight[!is.na(jtrips$TRIPMODE) & jtrips$TRIPMODE>0  & jtrips$TOURPURP>=4 & jtrips$TOURPURP<=6 & jtrips$TOURMODE==5])
tripmode6 <- wtd.hist(jtrips$TRIPMODE[!is.na(jtrips$TRIPMODE) & jtrips$TRIPMODE>0  & jtrips$TOURPURP>=4 & jtrips$TOURPURP<=6 & jtrips$TOURMODE==6], breaks = seq(1,14, by=1), freq = NULL, right=FALSE, weight = jtrips$finalweight[!is.na(jtrips$TRIPMODE) & jtrips$TRIPMODE>0  & jtrips$TOURPURP>=4 & jtrips$TOURPURP<=6 & jtrips$TOURMODE==6])
tripmode7 <- wtd.hist(jtrips$TRIPMODE[!is.na(jtrips$TRIPMODE) & jtrips$TRIPMODE>0  & jtrips$TOURPURP>=4 & jtrips$TOURPURP<=6 & jtrips$TOURMODE==7], breaks = seq(1,14, by=1), freq = NULL, right=FALSE, weight = jtrips$finalweight[!is.na(jtrips$TRIPMODE) & jtrips$TRIPMODE>0  & jtrips$TOURPURP>=4 & jtrips$TOURPURP<=6 & jtrips$TOURMODE==7])
tripmode8 <- wtd.hist(jtrips$TRIPMODE[!is.na(jtrips$TRIPMODE) & jtrips$TRIPMODE>0  & jtrips$TOURPURP>=4 & jtrips$TOURPURP<=6 & jtrips$TOURMODE==8], breaks = seq(1,14, by=1), freq = NULL, right=FALSE, weight = jtrips$finalweight[!is.na(jtrips$TRIPMODE) & jtrips$TRIPMODE>0  & jtrips$TOURPURP>=4 & jtrips$TOURPURP<=6 & jtrips$TOURMODE==8])
tripmode9 <- wtd.hist(jtrips$TRIPMODE[!is.na(jtrips$TRIPMODE) & jtrips$TRIPMODE>0  & jtrips$TOURPURP>=4 & jtrips$TOURPURP<=6 & jtrips$TOURMODE==9], breaks = seq(1,14, by=1), freq = NULL, right=FALSE, weight = jtrips$finalweight[!is.na(jtrips$TRIPMODE) & jtrips$TRIPMODE>0  & jtrips$TOURPURP>=4 & jtrips$TOURPURP<=6 & jtrips$TOURMODE==9])
tripmode10 <- wtd.hist(jtrips$TRIPMODE[!is.na(jtrips$TRIPMODE) & jtrips$TRIPMODE>0  & jtrips$TOURPURP>=4 & jtrips$TOURPURP<=6  & jtrips$TOURMODE==10], breaks = seq(1,14, by=1), freq = NULL, right=FALSE, weight = jtrips$finalweight[!is.na(jtrips$TRIPMODE) & jtrips$TRIPMODE>0  & jtrips$TOURPURP>=4 & jtrips$TOURPURP<=6  & jtrips$TOURMODE==10])
tripmode11 <- wtd.hist(jtrips$TRIPMODE[!is.na(jtrips$TRIPMODE) & jtrips$TRIPMODE>0  & jtrips$TOURPURP>=4 & jtrips$TOURPURP<=6  & jtrips$TOURMODE==11], breaks = seq(1,14, by=1), freq = NULL, right=FALSE, weight = jtrips$finalweight[!is.na(jtrips$TRIPMODE) & jtrips$TRIPMODE>0  & jtrips$TOURPURP>=4 & jtrips$TOURPURP<=6  & jtrips$TOURMODE==11])
tripmode12 <- wtd.hist(jtrips$TRIPMODE[!is.na(jtrips$TRIPMODE) & jtrips$TRIPMODE>0  & jtrips$TOURPURP>=4 & jtrips$TOURPURP<=6  & jtrips$TOURMODE==12], breaks = seq(1,14, by=1), freq = NULL, right=FALSE, weight = jtrips$finalweight[!is.na(jtrips$TRIPMODE) & jtrips$TRIPMODE>0  & jtrips$TOURPURP>=4 & jtrips$TOURPURP<=6  & jtrips$TOURMODE==12])
tripmode13 <- wtd.hist(jtrips$TRIPMODE[!is.na(jtrips$TRIPMODE) & jtrips$TRIPMODE>0  & jtrips$TOURPURP>=4 & jtrips$TOURPURP<=6  & jtrips$TOURMODE==13], breaks = seq(1,14, by=1), freq = NULL, right=FALSE, weight = jtrips$finalweight[!is.na(jtrips$TRIPMODE) & jtrips$TRIPMODE>0  & jtrips$TOURPURP>=4 & jtrips$TOURPURP<=6  & jtrips$TOURMODE==13])

tripModeProfile <- data.frame(tripmode1$counts, tripmode2$counts, tripmode3$counts, tripmode4$counts,
                              tripmode5$counts, tripmode6$counts, tripmode7$counts, tripmode8$counts, tripmode9$counts,
                              tripmode10$counts, tripmode11$counts, tripmode12$counts, tripmode13$counts)
colnames(tripModeProfile) <- c("tourmode1", "tourmode2", "tourmode3", "tourmode4", "tourmode5", "tourmode6", "tourmode7", "tourmode8", "tourmode9", "tourmode10", "tourmode11", "tourmode12", "tourmode13")

fwrite(tripModeProfile, file.path(outdir, "tripModeProfile_jMain.csv"))

tripModeProfile6_vis <- tripModeProfile #[1:9,]
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

#### jDisc ####                                                       
tripmode1 <- wtd.hist(jtrips$TRIPMODE[!is.na(jtrips$TRIPMODE) & jtrips$TRIPMODE>0  & jtrips$TOURPURP>=7 & jtrips$TOURPURP<=9 & jtrips$TOURMODE==1], breaks = seq(1,14, by=1), freq = NULL, right=FALSE, weight = jtrips$finalweight[!is.na(jtrips$TRIPMODE) & jtrips$TRIPMODE>0  & jtrips$TOURPURP>=7 & jtrips$TOURPURP<=9 & jtrips$TOURMODE==1])
tripmode2 <- wtd.hist(jtrips$TRIPMODE[!is.na(jtrips$TRIPMODE) & jtrips$TRIPMODE>0  & jtrips$TOURPURP>=7 & jtrips$TOURPURP<=9 & jtrips$TOURMODE==2], breaks = seq(1,14, by=1), freq = NULL, right=FALSE, weight = jtrips$finalweight[!is.na(jtrips$TRIPMODE) & jtrips$TRIPMODE>0  & jtrips$TOURPURP>=7 & jtrips$TOURPURP<=9 & jtrips$TOURMODE==2])
tripmode3 <- wtd.hist(jtrips$TRIPMODE[!is.na(jtrips$TRIPMODE) & jtrips$TRIPMODE>0  & jtrips$TOURPURP>=7 & jtrips$TOURPURP<=9 & jtrips$TOURMODE==3], breaks = seq(1,14, by=1), freq = NULL, right=FALSE, weight = jtrips$finalweight[!is.na(jtrips$TRIPMODE) & jtrips$TRIPMODE>0  & jtrips$TOURPURP>=7 & jtrips$TOURPURP<=9 & jtrips$TOURMODE==3])
tripmode4 <- wtd.hist(jtrips$TRIPMODE[!is.na(jtrips$TRIPMODE) & jtrips$TRIPMODE>0  & jtrips$TOURPURP>=7 & jtrips$TOURPURP<=9 & jtrips$TOURMODE==4], breaks = seq(1,14, by=1), freq = NULL, right=FALSE, weight = jtrips$finalweight[!is.na(jtrips$TRIPMODE) & jtrips$TRIPMODE>0  & jtrips$TOURPURP>=7 & jtrips$TOURPURP<=9 & jtrips$TOURMODE==4])
tripmode5 <- wtd.hist(jtrips$TRIPMODE[!is.na(jtrips$TRIPMODE) & jtrips$TRIPMODE>0  & jtrips$TOURPURP>=7 & jtrips$TOURPURP<=9 & jtrips$TOURMODE==5], breaks = seq(1,14, by=1), freq = NULL, right=FALSE, weight = jtrips$finalweight[!is.na(jtrips$TRIPMODE) & jtrips$TRIPMODE>0  & jtrips$TOURPURP>=7 & jtrips$TOURPURP<=9 & jtrips$TOURMODE==5])
tripmode6 <- wtd.hist(jtrips$TRIPMODE[!is.na(jtrips$TRIPMODE) & jtrips$TRIPMODE>0  & jtrips$TOURPURP>=7 & jtrips$TOURPURP<=9 & jtrips$TOURMODE==6], breaks = seq(1,14, by=1), freq = NULL, right=FALSE, weight = jtrips$finalweight[!is.na(jtrips$TRIPMODE) & jtrips$TRIPMODE>0  & jtrips$TOURPURP>=7 & jtrips$TOURPURP<=9 & jtrips$TOURMODE==6])
tripmode7 <- wtd.hist(jtrips$TRIPMODE[!is.na(jtrips$TRIPMODE) & jtrips$TRIPMODE>0  & jtrips$TOURPURP>=7 & jtrips$TOURPURP<=9 & jtrips$TOURMODE==7], breaks = seq(1,14, by=1), freq = NULL, right=FALSE, weight = jtrips$finalweight[!is.na(jtrips$TRIPMODE) & jtrips$TRIPMODE>0  & jtrips$TOURPURP>=7 & jtrips$TOURPURP<=9 & jtrips$TOURMODE==7])
tripmode8 <- wtd.hist(jtrips$TRIPMODE[!is.na(jtrips$TRIPMODE) & jtrips$TRIPMODE>0  & jtrips$TOURPURP>=7 & jtrips$TOURPURP<=9 & jtrips$TOURMODE==8], breaks = seq(1,14, by=1), freq = NULL, right=FALSE, weight = jtrips$finalweight[!is.na(jtrips$TRIPMODE) & jtrips$TRIPMODE>0  & jtrips$TOURPURP>=7 & jtrips$TOURPURP<=9 & jtrips$TOURMODE==8])
tripmode9 <- wtd.hist(jtrips$TRIPMODE[!is.na(jtrips$TRIPMODE) & jtrips$TRIPMODE>0  & jtrips$TOURPURP>=7 & jtrips$TOURPURP<=9 & jtrips$TOURMODE==9], breaks = seq(1,14, by=1), freq = NULL, right=FALSE, weight = jtrips$finalweight[!is.na(jtrips$TRIPMODE) & jtrips$TRIPMODE>0  & jtrips$TOURPURP>=7 & jtrips$TOURPURP<=9 & jtrips$TOURMODE==9])
tripmode10 <- wtd.hist(jtrips$TRIPMODE[!is.na(jtrips$TRIPMODE) & jtrips$TRIPMODE>0  & jtrips$TOURPURP>=7 & jtrips$TOURPURP<=9  & jtrips$TOURMODE==10], breaks = seq(1,14, by=1), freq = NULL, right=FALSE, weight = jtrips$finalweight[!is.na(jtrips$TRIPMODE) & jtrips$TRIPMODE>0  & jtrips$TOURPURP>=7 & jtrips$TOURPURP<=9  & jtrips$TOURMODE==10])
tripmode11 <- wtd.hist(jtrips$TRIPMODE[!is.na(jtrips$TRIPMODE) & jtrips$TRIPMODE>0  & jtrips$TOURPURP>=7 & jtrips$TOURPURP<=9  & jtrips$TOURMODE==11], breaks = seq(1,14, by=1), freq = NULL, right=FALSE, weight = jtrips$finalweight[!is.na(jtrips$TRIPMODE) & jtrips$TRIPMODE>0  & jtrips$TOURPURP>=7 & jtrips$TOURPURP<=9  & jtrips$TOURMODE==11])
tripmode12 <- wtd.hist(jtrips$TRIPMODE[!is.na(jtrips$TRIPMODE) & jtrips$TRIPMODE>0  & jtrips$TOURPURP>=7 & jtrips$TOURPURP<=9  & jtrips$TOURMODE==12], breaks = seq(1,14, by=1), freq = NULL, right=FALSE, weight = jtrips$finalweight[!is.na(jtrips$TRIPMODE) & jtrips$TRIPMODE>0  & jtrips$TOURPURP>=7 & jtrips$TOURPURP<=9  & jtrips$TOURMODE==12])
tripmode13 <- wtd.hist(jtrips$TRIPMODE[!is.na(jtrips$TRIPMODE) & jtrips$TRIPMODE>0  & jtrips$TOURPURP>=7 & jtrips$TOURPURP<=9  & jtrips$TOURMODE==13], breaks = seq(1,14, by=1), freq = NULL, right=FALSE, weight = jtrips$finalweight[!is.na(jtrips$TRIPMODE) & jtrips$TRIPMODE>0  & jtrips$TOURPURP>=7 & jtrips$TOURPURP<=9  & jtrips$TOURMODE==13])

tripModeProfile <- data.frame(tripmode1$counts, tripmode2$counts, tripmode3$counts, tripmode4$counts,
                              tripmode5$counts, tripmode6$counts, tripmode7$counts, tripmode8$counts, tripmode9$counts,
                              tripmode10$counts, tripmode11$counts, tripmode12$counts, tripmode13$counts)
colnames(tripModeProfile) <- c("tourmode1", "tourmode2", "tourmode3", "tourmode4", "tourmode5", "tourmode6", "tourmode7", "tourmode8", "tourmode9", "tourmode10", "tourmode11", "tourmode12", "tourmode13")
fwrite(tripModeProfile, file.path(outdir, "tripModeProfile_jDisc.csv"))

tripModeProfile7_vis <- tripModeProfile #[1:9,]
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

#### trip At-work ####
tripmode1 <- wtd.hist(trips$TRIPMODE[!is.na(trips$TRIPMODE) & trips$TRIPMODE>0  & trips$SUBTOUR==1 & trips$TOURMODE==1], breaks = seq(1,14, by=1), freq = NULL, right=FALSE, weight = trips$finalweight[!is.na(trips$TRIPMODE) & trips$TRIPMODE>0  & trips$SUBTOUR==1 & trips$TOURMODE==1])
tripmode2 <- wtd.hist(trips$TRIPMODE[!is.na(trips$TRIPMODE) & trips$TRIPMODE>0  & trips$SUBTOUR==1 & trips$TOURMODE==2], breaks = seq(1,14, by=1), freq = NULL, right=FALSE, weight = trips$finalweight[!is.na(trips$TRIPMODE) & trips$TRIPMODE>0  & trips$SUBTOUR==1 & trips$TOURMODE==2])
tripmode3 <- wtd.hist(trips$TRIPMODE[!is.na(trips$TRIPMODE) & trips$TRIPMODE>0  & trips$SUBTOUR==1 & trips$TOURMODE==3], breaks = seq(1,14, by=1), freq = NULL, right=FALSE, weight = trips$finalweight[!is.na(trips$TRIPMODE) & trips$TRIPMODE>0  & trips$SUBTOUR==1 & trips$TOURMODE==3])
tripmode4 <- wtd.hist(trips$TRIPMODE[!is.na(trips$TRIPMODE) & trips$TRIPMODE>0  & trips$SUBTOUR==1 & trips$TOURMODE==4], breaks = seq(1,14, by=1), freq = NULL, right=FALSE, weight = trips$finalweight[!is.na(trips$TRIPMODE) & trips$TRIPMODE>0  & trips$SUBTOUR==1 & trips$TOURMODE==4])
tripmode5 <- wtd.hist(trips$TRIPMODE[!is.na(trips$TRIPMODE) & trips$TRIPMODE>0  & trips$SUBTOUR==1 & trips$TOURMODE==5], breaks = seq(1,14, by=1), freq = NULL, right=FALSE, weight = trips$finalweight[!is.na(trips$TRIPMODE) & trips$TRIPMODE>0  & trips$SUBTOUR==1 & trips$TOURMODE==5])
tripmode6 <- wtd.hist(trips$TRIPMODE[!is.na(trips$TRIPMODE) & trips$TRIPMODE>0  & trips$SUBTOUR==1 & trips$TOURMODE==6], breaks = seq(1,14, by=1), freq = NULL, right=FALSE, weight = trips$finalweight[!is.na(trips$TRIPMODE) & trips$TRIPMODE>0  & trips$SUBTOUR==1 & trips$TOURMODE==6])
tripmode7 <- wtd.hist(trips$TRIPMODE[!is.na(trips$TRIPMODE) & trips$TRIPMODE>0  & trips$SUBTOUR==1 & trips$TOURMODE==7], breaks = seq(1,14, by=1), freq = NULL, right=FALSE, weight = trips$finalweight[!is.na(trips$TRIPMODE) & trips$TRIPMODE>0  & trips$SUBTOUR==1 & trips$TOURMODE==7])
tripmode8 <- wtd.hist(trips$TRIPMODE[!is.na(trips$TRIPMODE) & trips$TRIPMODE>0  & trips$SUBTOUR==1 & trips$TOURMODE==8], breaks = seq(1,14, by=1), freq = NULL, right=FALSE, weight = trips$finalweight[!is.na(trips$TRIPMODE) & trips$TRIPMODE>0  & trips$SUBTOUR==1 & trips$TOURMODE==8])
tripmode9 <- wtd.hist(trips$TRIPMODE[!is.na(trips$TRIPMODE) & trips$TRIPMODE>0  & trips$SUBTOUR==1 & trips$TOURMODE==9], breaks = seq(1,14, by=1), freq = NULL, right=FALSE, weight = trips$finalweight[!is.na(trips$TRIPMODE) & trips$TRIPMODE>0  & trips$SUBTOUR==1 & trips$TOURMODE==9])
tripmode10 <- wtd.hist(trips$TRIPMODE[!is.na(trips$TRIPMODE) & trips$TRIPMODE>0 & trips$SUBTOUR==1  & trips$TOURMODE==10], breaks = seq(1,14, by=1), freq = NULL, right=FALSE, weight = trips$finalweight[!is.na(trips$TRIPMODE) & trips$TRIPMODE>0 & trips$SUBTOUR==1  & trips$TOURMODE==10])
tripmode11 <- wtd.hist(trips$TRIPMODE[!is.na(trips$TRIPMODE) & trips$TRIPMODE>0 & trips$SUBTOUR==1  & trips$TOURMODE==11], breaks = seq(1,14, by=1), freq = NULL, right=FALSE, weight = trips$finalweight[!is.na(trips$TRIPMODE) & trips$TRIPMODE>0 & trips$SUBTOUR==1  & trips$TOURMODE==11])
tripmode12 <- wtd.hist(trips$TRIPMODE[!is.na(trips$TRIPMODE) & trips$TRIPMODE>0 & trips$SUBTOUR==1  & trips$TOURMODE==12], breaks = seq(1,14, by=1), freq = NULL, right=FALSE, weight = trips$finalweight[!is.na(trips$TRIPMODE) & trips$TRIPMODE>0 & trips$SUBTOUR==1  & trips$TOURMODE==12])
tripmode13 <- wtd.hist(trips$TRIPMODE[!is.na(trips$TRIPMODE) & trips$TRIPMODE>0 & trips$SUBTOUR==1  & trips$TOURMODE==13], breaks = seq(1,14, by=1), freq = NULL, right=FALSE, weight = trips$finalweight[!is.na(trips$TRIPMODE) & trips$TRIPMODE>0 & trips$SUBTOUR==1  & trips$TOURMODE==13])
tripModeProfile <- data.frame(tripmode1$counts, tripmode2$counts, tripmode3$counts, tripmode4$counts,
                              tripmode5$counts, tripmode6$counts, tripmode7$counts, tripmode8$counts, tripmode9$counts,
                              tripmode10$counts, tripmode11$counts, tripmode12$counts, tripmode13$counts)
colnames(tripModeProfile) <- c("tourmode1", "tourmode2", "tourmode3", "tourmode4", "tourmode5", "tourmode6", "tourmode7", "tourmode8", "tourmode9", "tourmode10", "tourmode11", "tourmode12", "tourmode13")
fwrite(tripModeProfile, file.path(outdir, "tripModeProfile_ATW.csv"))

tripModeProfile8_vis <- tripModeProfile #[1:9,]
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

#### trip Total ####
tripmode1 <- wtd.hist(trips$TRIPMODE[!is.na(trips$TRIPMODE) & trips$TRIPMODE>0  & trips$TOURMODE==1], breaks = seq(1,14, by=1), freq = NULL, right=FALSE, weight = trips$finalweight[!is.na(trips$TRIPMODE) & trips$TRIPMODE>0  & trips$TOURMODE==1])
tripmode2 <- wtd.hist(trips$TRIPMODE[!is.na(trips$TRIPMODE) & trips$TRIPMODE>0  & trips$TOURMODE==2], breaks = seq(1,14, by=1), freq = NULL, right=FALSE, weight = trips$finalweight[!is.na(trips$TRIPMODE) & trips$TRIPMODE>0  & trips$TOURMODE==2])
tripmode3 <- wtd.hist(trips$TRIPMODE[!is.na(trips$TRIPMODE) & trips$TRIPMODE>0  & trips$TOURMODE==3], breaks = seq(1,14, by=1), freq = NULL, right=FALSE, weight = trips$finalweight[!is.na(trips$TRIPMODE) & trips$TRIPMODE>0  & trips$TOURMODE==3])
tripmode4 <- wtd.hist(trips$TRIPMODE[!is.na(trips$TRIPMODE) & trips$TRIPMODE>0  & trips$TOURMODE==4], breaks = seq(1,14, by=1), freq = NULL, right=FALSE, weight = trips$finalweight[!is.na(trips$TRIPMODE) & trips$TRIPMODE>0  & trips$TOURMODE==4])
tripmode5 <- wtd.hist(trips$TRIPMODE[!is.na(trips$TRIPMODE) & trips$TRIPMODE>0  & trips$TOURMODE==5], breaks = seq(1,14, by=1), freq = NULL, right=FALSE, weight = trips$finalweight[!is.na(trips$TRIPMODE) & trips$TRIPMODE>0  & trips$TOURMODE==5])
tripmode6 <- wtd.hist(trips$TRIPMODE[!is.na(trips$TRIPMODE) & trips$TRIPMODE>0  & trips$TOURMODE==6], breaks = seq(1,14, by=1), freq = NULL, right=FALSE, weight = trips$finalweight[!is.na(trips$TRIPMODE) & trips$TRIPMODE>0  & trips$TOURMODE==6])
tripmode7 <- wtd.hist(trips$TRIPMODE[!is.na(trips$TRIPMODE) & trips$TRIPMODE>0  & trips$TOURMODE==7], breaks = seq(1,14, by=1), freq = NULL, right=FALSE, weight = trips$finalweight[!is.na(trips$TRIPMODE) & trips$TRIPMODE>0  & trips$TOURMODE==7])
tripmode8 <- wtd.hist(trips$TRIPMODE[!is.na(trips$TRIPMODE) & trips$TRIPMODE>0  & trips$TOURMODE==8], breaks = seq(1,14, by=1), freq = NULL, right=FALSE, weight = trips$finalweight[!is.na(trips$TRIPMODE) & trips$TRIPMODE>0  & trips$TOURMODE==8])
tripmode9 <- wtd.hist(trips$TRIPMODE[!is.na(trips$TRIPMODE) & trips$TRIPMODE>0  & trips$TOURMODE==9], breaks = seq(1,14, by=1), freq = NULL, right=FALSE, weight = trips$finalweight[!is.na(trips$TRIPMODE) & trips$TRIPMODE>0  & trips$TOURMODE==9])
tripmode10 <- wtd.hist(trips$TRIPMODE[!is.na(trips$TRIPMODE) & trips$TRIPMODE>0  & trips$TOURMODE==10], breaks = seq(1,14, by=1), freq = NULL, right=FALSE, weight = trips$finalweight[!is.na(trips$TRIPMODE) & trips$TRIPMODE>0  & trips$TOURMODE==10])
tripmode11 <- wtd.hist(trips$TRIPMODE[!is.na(trips$TRIPMODE) & trips$TRIPMODE>0  & trips$TOURMODE==11], breaks = seq(1,14, by=1), freq = NULL, right=FALSE, weight = trips$finalweight[!is.na(trips$TRIPMODE) & trips$TRIPMODE>0  & trips$TOURMODE==11])
tripmode12 <- wtd.hist(trips$TRIPMODE[!is.na(trips$TRIPMODE) & trips$TRIPMODE>0  & trips$TOURMODE==12], breaks = seq(1,14, by=1), freq = NULL, right=FALSE, weight = trips$finalweight[!is.na(trips$TRIPMODE) & trips$TRIPMODE>0  & trips$TOURMODE==12])
tripmode13 <- wtd.hist(trips$TRIPMODE[!is.na(trips$TRIPMODE) & trips$TRIPMODE>0  & trips$TOURMODE==13], breaks = seq(1,14, by=1), freq = NULL, right=FALSE, weight = trips$finalweight[!is.na(trips$TRIPMODE) & trips$TRIPMODE>0  & trips$TOURMODE==13])

tripModeProfile <- data.frame(tripmode1$counts, tripmode2$counts, tripmode3$counts, tripmode4$counts,
                              tripmode5$counts, tripmode6$counts, tripmode7$counts, tripmode8$counts, tripmode9$counts,
                              tripmode10$counts, tripmode11$counts, tripmode12$counts, tripmode13$counts)
colnames(tripModeProfile) <- c("tourmode1", "tourmode2", "tourmode3", "tourmode4", "tourmode5", "tourmode6", "tourmode7", "tourmode8", "tourmode9", "tourmode10", "tourmode11", "tourmode12", "tourmode13")

fwrite(tripModeProfile, file.path(outdir, "tripModeProfile_Total.csv"))

tripModeProfile9_vis <- tripModeProfile #[1:9,]
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
tripModeProfile_vis <- data.frame(tripModeProfile1_vis, tripModeProfile2_vis$freq2, tripModeProfile3_vis$freq3
                                  , tripModeProfile4_vis$freq4, tripModeProfile5_vis$freq5, tripModeProfile6_vis$freq6
                                  , tripModeProfile7_vis$freq7, tripModeProfile8_vis$freq8, tripModeProfile9_vis$freq9)
colnames(tripModeProfile_vis) <- c("tripmode", "tourmode", "work", "univ", "schl", "imain", "idisc", "jmain", "jdisc", "atwork", "total")

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

fwrite(temp, file.path(outdir, "tripModeProfile_vis.csv"), row.names = F)

#### trip mode by time period - for Wu, Sun ####
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

itrips_summary <- aggregate(num_trips~tod+TOURPURP+TOURMODE+TRIPMODE, data=trips[trips$FULLY_JOINT==0,], FUN=sum)
jtrips_summary <- aggregate(num_trips~tod+TOURPURP+TOURMODE+TRIPMODE, data=jtrips, FUN=sum)

fwrite(itrips_summary, file.path(outdir, "itrips_tripmode_summary.csv"), row.names = F)
fwrite(jtrips_summary, file.path(outdir, "jtrips_tripmode_summary.csv"), row.names = F)

#### Total number of stops, trips & tours ####
cat("Total number of stops : ", sum(stops$finalweight[stops$FULLY_JOINT==0]) + sum(jstops$finalweight))
cat("Total number of trips : ", sum(trips$finalweight[trips$FULLY_JOINT==0]) + sum(jtrips$finalweight))
cat("Total number of tours : ", sum(tours$finalweight[tours$TOURPURP<=10]))

# output total numbers in a file
total_population <- sum(pertypeDistbn$freq) 
total_households <- sum(hh$finalweight)
total_tours <- sum(tours$finalweight[(tours$TOURPURP<=9 | tours$TOURPURP==20)]) #updated by nagendra.dhakar@rsginc.com, 01/30/2017, add subtours as well
total_trips <- sum(trips$finalweight[trips$FULLY_JOINT==0]) + sum(jtrips$finalweight)
total_stops <- sum(stops$finalweight[stops$FULLY_JOINT==0]) + sum(jstops$finalweight)
total_population_for_rates <- sum(perday$finalweight_1)

trips$num_travel[trips$TRIPMODE==1] <- 1
trips$num_travel[trips$TRIPMODE==2] <- 2
trips$num_travel[trips$TRIPMODE==3] <- 3.5
trips$num_travel[is.na(trips$num_travel)] <- 0

total_vmt <- sum((trips$finalweight[trips$TRIPMODE>0 & trips$TRIPMODE<=3 & !is.na(trips$od_dist)]*trips$od_dist[trips$TRIPMODE>0 & trips$TRIPMODE<=3 & !is.na(trips$od_dist)])/trips$num_travel[trips$TRIPMODE>0 & trips$TRIPMODE<=3 & !is.na(trips$od_dist)])


totals_var <- c("total_population", "total_households", "total_tours", "total_trips", "total_stops", "total_vmt", "total_population_for_rates")
totals_val <- c(total_population,total_households, total_tours, total_trips, total_stops, total_vmt, total_population_for_rates)

totals_df <- data.frame(name = totals_var, value = totals_val)

fwrite(totals_df, file.path(outdir, "totals.csv"), row.names = F)

# HH Size distribution
hhSizeDist <- plyr::count(hh[!is.na(hh$HHSIZE),], c("HHSIZE"), "finalweight")
fwrite(hhSizeDist, file.path(outdir, "hhSizeDist.csv"), row.names = F)

# Active Persons by person type
actpertypeDistbn <- plyr::count(perday[(!is.na(perday$PERTYPE) & (perday$DAP!="H")),], c("PERTYPE"), "finalweight_1")
fwrite(actpertypeDistbn, file.path(outdir, "activePertypeDistbn.csv"), row.names = FALSE)



# write out files for debugging
# write.csv(hh, "hh_debug.csv", row.names = F)
# write.csv(per, "per_debug.csv", row.names = F)
# write.csv(hhday, "hhday_debug.csv", row.names = F)
# write.csv(perday, "perday_debug.csv", row.names = F)
# write.csv(place, "place_debug.csv", row.names = F)
# write.csv(tours, "tours_debug.csv", row.names = F)
# write.csv(trips, "trips_debug.csv", row.names = F)
# write.csv(jtours, "jtours_debug.csv", row.names = F)
# write.csv(jtrips, "jtrips_debug.csv", row.names = F)
# write.csv(jutrips, "jutrips_debug.csv", row.names = F)


### write out trip files for creating trip departure/arrival lookups
fwrite(trips, file.path(outdir, "trips.csv"), row.names = F)
fwrite(jtrips,file.path(outdir,  "jtrips.csv"), row.names = F)


#### IE/EI trip mode distrribution ####
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
fwrite(out, file.path(outdir, "out_13.csv"), row.names = F)

tripmode_ie <- wtd.hist(trips_ie$TRIPMODE[!is.na(trips_ie$TRIPMODE) & trips_ie$TRIPMODE>0 & trips_ie$TRIPMODE<=9],
                        breaks = seq(1,14, by=1), freq = NULL, right=FALSE,
                        weight = trips_ie$finalweight[!is.na(trips_ie$TRIPMODE) & trips_ie$TRIPMODE>0 & trips_ie$TRIPMODE<=9])
write.csv(tripmode_ie$counts, file.path(outdir, "tripmode_ie.csv"), row.names = F)


#### Generate school escorting summaries ####
## These summaries are created for the full OHAS data of Oregon state
## Therefore, clear workspace and reload the required files


## INPUTS


# Edited ASR to use all days instead of one
## READ DATA
#hh_raw <- read.csv(paste(Survey_Dir, "HH_SPA_INPUT.csv", sep = "/"), stringsAsFactors=FALSE)
#hh     <- read.csv(paste(Survey_Processed_Dir,"day1", "households.csv", sep = "/"), stringsAsFactors=FALSE)
#per    <- read.csv(paste(Survey_Processed_Dir, "day1", "persons.csv", sep = "/"), stringsAsFactors=FALSE)
#tours  <- read.csv(paste(Survey_Processed_Dir, "day1", "tours.csv", sep = "/"), stringsAsFactors=FALSE)
#trips  <- read.csv(paste(Survey_Processed_Dir, "day1", "trips.csv", sep = "/"), stringsAsFactors=FALSE)

#tours$HHEXPFACT <- hh_raw$HHEXPFAC[match(tours$HH_ID, hh_raw$SAMPN)]
#trips$HHEXPFACT <- hh_raw$HHEXPFAC[match(trips$HH_ID, hh_raw$SAMPN)]

tours$HHEXPFACT = hhday$finalweight[match(paste(tours$HH_ID, tours$DAYNO, sep = "-"),
                                          paste(hhday$SAMPN, hhday$DAYNO, sep = "-"))]

tours$OUT_CHAUFFUER_ID <- as.numeric(tours$OUT_CHAUFFUER_ID)
tours$INB_CHAUFFUER_ID <- as.numeric(tours$INB_CHAUFFUER_ID)

# tours_sample <- select(tours, HH_ID, PER_ID, TOUR_ID, TOURPURP, ESCORTED_TOUR, CHAUFFUER_ID, ESCORTING_TOUR, 
#                        PERSONTYPE, OUT_ESCORT_TYPE, OUT_CHAUFFUER_ID, OUT_CHAUFFUER_PURP, OUT_CHAUFFUER_PTYPE, 
#                        INB_ESCORT_TYPE, INB_CHAUFFUER_ID, INB_CHAUFFUER_PURP, INB_CHAUFFUER_PTYPE, HHEXPFACT)
# tours_sample <- filter(tours_sample, TOURPURP==3 & PERSONTYPE>=6)
# out_sample1 <- filter(tours_sample, !is.na(OUT_ESCORT_TYPE) & !is.na(PERSONTYPE) & OUT_ESCORT_TYPE!="NaN")
# inb_sample1 <- filter(tours_sample, !is.na(INB_ESCORT_TYPE) & !is.na(PERSONTYPE) & INB_ESCORT_TYPE!="NaN")

tours_sample <- tours[TOURPURP==3 & PERSONTYPE>=6, .(HH_ID, PER_ID, TOUR_ID, TOURPURP, ESCORTED_TOUR, CHAUFFUER_ID, ESCORTING_TOUR, 
                                                     PERSONTYPE, OUT_ESCORT_TYPE, OUT_CHAUFFUER_ID, OUT_CHAUFFUER_PURP, OUT_CHAUFFUER_PTYPE, 
                                                     INB_ESCORT_TYPE, INB_CHAUFFUER_ID, INB_CHAUFFUER_PURP, INB_CHAUFFUER_PTYPE, HHEXPFACT)]

out_sample1 <- tours_sample[!is.na(OUT_ESCORT_TYPE) & !is.na(PERSONTYPE) & OUT_ESCORT_TYPE!="NaN",]
inb_sample1 <- tours_sample[!is.na(INB_ESCORT_TYPE) & !is.na(PERSONTYPE) & INB_ESCORT_TYPE!="NaN",]

out_table1  <- xtabs(HHEXPFACT~OUT_ESCORT_TYPE+PERSONTYPE, data = out_sample1)
inb_table1  <- xtabs(HHEXPFACT~INB_ESCORT_TYPE+PERSONTYPE, data = inb_sample1)

# out_sample2 <- filter(tours_sample, !is.na(OUT_ESCORT_TYPE) & !is.na(OUT_CHAUFFUER_PTYPE) & OUT_ESCORT_TYPE!="NaN" & OUT_CHAUFFUER_PTYPE!="NaN")
# inb_sample2 <- filter(tours_sample, !is.na(INB_ESCORT_TYPE) & !is.na(INB_CHAUFFUER_PTYPE) & INB_ESCORT_TYPE!="NaN" & INB_CHAUFFUER_PTYPE!="NaN")
out_sample2 <- tours_sample[!is.na(OUT_ESCORT_TYPE) & !is.na(OUT_CHAUFFUER_PTYPE) & OUT_ESCORT_TYPE!="NaN" & OUT_CHAUFFUER_PTYPE!="NaN",]
inb_sample2 <- tours_sample[!is.na(INB_ESCORT_TYPE) & !is.na(INB_CHAUFFUER_PTYPE) & INB_ESCORT_TYPE!="NaN" & INB_CHAUFFUER_PTYPE!="NaN",]

out_table2  <- xtabs(HHEXPFACT~OUT_ESCORT_TYPE+OUT_CHAUFFUER_PTYPE, data = out_sample2)
inb_table2  <- xtabs(HHEXPFACT~INB_ESCORT_TYPE+INB_CHAUFFUER_PTYPE, data = inb_sample2)


#### summary of non-mandatory stops on pure escorting tours ####
pure_escort_tours <- tours[(tours$OUT_ESCORTING_TYPE==2 & tours$OUT_ESCORTEE_TOUR_PURP==3) |
                             (tours$INB_ESCORTING_TYPE==2 & tours$INB_ESCORTEE_TOUR_PURP==3) ,]


library(dplyr)
#trips$tempid = trips$HH_ID*1000 + trips$PER_ID*100 + trips$TOUR_ID
nonescortstops <- trips %>%
  mutate(nonescortstop = ifelse(DEST_PURP>0 & DEST_PURP<=10 & DEST_PURP!=4 & DEST_IS_TOUR_DEST==0, 1, 0)) %>%
  mutate(tempid = HH_ID*1000+ PER_ID*100+ TOUR_ID) %>%
  dplyr::group_by(tempid) %>%
  summarise(nonescortstops = sum(nonescortstop)) %>%
  ungroup()

# pure_escort_tours <- pure_escort_tours %>%
#   mutate(tempid = HH_ID*1000+ PER_ID*100+ TOUR_ID) %>%
#   left_join(nonescortstops, by = c("tempid"))

# summary of pure school escorting tours [outbound or inbound]
# frequency of non-escorting stops excluding the primary destination by chauffeur tour purpose
# xtabs(HHEXPFACT~TOURPURP+nonescortstops, data = pure_escort_tours)


#### Worker summary ####
# summary of worker with a child which went to school
# by escort type, can be separated by outbound and inbound direction

#get list of active workers with at least one work tour
active_workers <- tours %>%
  filter(TOURPURP %in% c(1,10)) %>%   #work and work-related
  filter(PERSONTYPE %in% c(1,2)) %>%  #full and part-time worker
  group_by(HH_ID, PER_ID) %>%
  summarise(PERSONTYPE=max(PERSONTYPE), HHEXPFACT=max(HHEXPFACT)) %>%
  ungroup()

workers <- per[per$PERSONTYPE %in% c(1,2), ]

#get list of students with at least one school tour
active_students <- tours %>%
  filter(TOURPURP %in% c(3)) %>%   #school tour
  filter(PERSONTYPE %in% c(6,7,8)) %>%  #all school students
  group_by(HH_ID, PER_ID) %>%
  summarise(PERSONTYPE=max(PERSONTYPE)) %>%
  ungroup()

students <- per[per$PERSONTYPE %in% c(6,7,8), ] 

hh_active_student <- active_students %>%
  group_by(HH_ID) %>%
  mutate(active_student=1) %>%
  summarise(active_student = max(active_student)) %>%
  ungroup()

#tag active workers with active students in household
active_workers <- active_workers %>%
  left_join(hh_active_student, by = c("HH_ID")) %>%
  mutate(active_student=ifelse(is.na(active_student), 0, active_student))

#list of workers who did ride share or pure escort for school student
out_rs_workers <- tours %>%
  dplyr::select(HH_ID, PER_ID, TOUR_ID, TOURPURP, ESCORTED_TOUR, CHAUFFUER_ID, 
         OUT_ESCORT_TYPE, OUT_CHAUFFUER_ID, OUT_CHAUFFUER_PURP, OUT_CHAUFFUER_PTYPE) %>%
  dplyr::filter(TOURPURP==3 & OUT_ESCORT_TYPE==1) %>%
  dplyr::group_by(HH_ID, OUT_CHAUFFUER_ID) %>%
  dplyr::mutate(num_escort = 1) %>%
  dplyr::summarise(out_rs_escort = sum(num_escort))

out_pe_workers <- tours %>%
  dplyr::select(HH_ID, PER_ID, TOUR_ID, TOURPURP, ESCORTED_TOUR, CHAUFFUER_ID, 
         OUT_ESCORT_TYPE, OUT_CHAUFFUER_ID, OUT_CHAUFFUER_PURP, OUT_CHAUFFUER_PTYPE) %>%
  dplyr::filter(TOURPURP==3 & OUT_ESCORT_TYPE==2) %>%
  dplyr::group_by(HH_ID, OUT_CHAUFFUER_ID) %>%
  dplyr::mutate(num_escort = 1) %>%
  dplyr::summarise(out_pe_escort = sum(num_escort))

inb_rs_workers <- tours %>%
  dplyr::select(HH_ID, PER_ID, TOUR_ID, TOURPURP, ESCORTED_TOUR, CHAUFFUER_ID, 
         INB_ESCORT_TYPE, INB_CHAUFFUER_ID, INB_CHAUFFUER_PURP, INB_CHAUFFUER_PTYPE) %>%
  dplyr::filter(TOURPURP==3 & INB_ESCORT_TYPE==1) %>%
  dplyr::group_by(HH_ID, INB_CHAUFFUER_ID) %>%
  dplyr::mutate(num_escort = 1) %>%
  dplyr::summarise(inb_rs_escort = sum(num_escort))

inb_pe_workers <- tours %>%
  dplyr::select(HH_ID, PER_ID, TOUR_ID, TOURPURP, ESCORTED_TOUR, CHAUFFUER_ID, 
         INB_ESCORT_TYPE, INB_CHAUFFUER_ID, INB_CHAUFFUER_PURP, INB_CHAUFFUER_PTYPE) %>%
  dplyr::filter(TOURPURP==3 & INB_ESCORT_TYPE==2) %>%
  dplyr::group_by(HH_ID, INB_CHAUFFUER_ID) %>%
  dplyr::mutate(num_escort = 1) %>%
  dplyr::summarise(inb_pe_escort = sum(num_escort))

active_workers <- active_workers %>%
  left_join(out_rs_workers, by = c("HH_ID"="HH_ID", "PER_ID"="OUT_CHAUFFUER_ID")) %>%
  left_join(out_pe_workers, by = c("HH_ID"="HH_ID", "PER_ID"="OUT_CHAUFFUER_ID")) %>%
  left_join(inb_rs_workers, by = c("HH_ID"="HH_ID", "PER_ID"="INB_CHAUFFUER_ID")) %>%
  left_join(inb_pe_workers, by = c("HH_ID"="HH_ID", "PER_ID"="INB_CHAUFFUER_ID"))

active_workers[is.na(active_workers)] <- 0

active_workers <- active_workers %>%
  mutate(out_escort_type = 3) %>%
  mutate(out_escort_type = ifelse(out_rs_escort>0, 1, out_escort_type)) %>%
  mutate(out_escort_type = ifelse(out_pe_escort>0, 2, out_escort_type)) %>%
  mutate(inb_escort_type = 3) %>%
  mutate(inb_escort_type = ifelse(inb_rs_escort>0, 1, inb_escort_type)) %>%
  mutate(inb_escort_type = ifelse(inb_pe_escort>0, 2, inb_escort_type))

temp <- filter(active_workers, active_student==1)
worker_table <- xtabs(HHEXPFACT~out_escort_type+inb_escort_type, data = temp)

## add marginal totals to all final tables
out_table1   <- addmargins(as.table(out_table1))
inb_table1   <- addmargins(as.table(inb_table1))
  # out_table2   <- addmargins(as.table(out_table2))
  # inb_table2   <- addmargins(as.table(inb_table2))

worker_table <- addmargins(as.table(worker_table))

## reshape data in required form for visualizer
out_table1 <- as.data.frame.matrix(out_table1)
out_table1$id <- row.names(out_table1)
out_table1 <- melt(out_table1, id = c("id"))
colnames(out_table1) <- c("esc_type", "child_type", "freq_out")
out_table1$esc_type <- as.character(out_table1$esc_type)
out_table1$child_type <- as.character(out_table1$child_type)
out_table1 <- out_table1[out_table1$esc_type!="Sum",]
out_table1$child_type[out_table1$child_type=="Sum"] <- "Total"

inb_table1 <- as.data.frame.matrix(inb_table1)
inb_table1$id <- row.names(inb_table1)
inb_table1 <- melt(inb_table1, id = c("id"))
colnames(inb_table1) <- c("esc_type", "child_type", "freq_inb")
inb_table1$esc_type <- as.character(inb_table1$esc_type)
inb_table1$child_type <- as.character(inb_table1$child_type)
inb_table1 <- inb_table1[inb_table1$esc_type!="Sum",]
inb_table1$child_type[inb_table1$child_type=="Sum"] <- "Total"

table1 <- out_table1
table1$freq_inb <- inb_table1$freq_inb
table1$esc_type[table1$esc_type=='1'] <- "Ride Share"
table1$esc_type[table1$esc_type=='2'] <- "Pure Escort"
table1$esc_type[table1$esc_type=='3'] <- "No Escort"
table1$child_type[table1$child_type=='6'] <- 'Driv Student'
table1$child_type[table1$child_type=='7'] <- 'Non-DrivStudent'
table1$child_type[table1$child_type=='8'] <- 'Pre-Schooler'


# out_table2 <- as.data.frame.matrix(out_table2)
# out_table2$id <- row.names(out_table2)
# out_table2 <- melt(out_table2, id = c("id"))
# colnames(out_table2) <- c("esc_type", "chauffeur", "freq_out")
# out_table2$esc_type <- as.character(out_table2$esc_type)
# out_table2$chauffeur <- as.character(out_table2$chauffeur)
# out_table2 <- out_table2[out_table2$esc_type!="Sum",]
# out_table2$chauffeur[out_table2$chauffeur=="Sum"] <- "Total"
# 
# inb_table2 <- as.data.frame.matrix(inb_table2)
# inb_table2$id <- row.names(inb_table2)
# inb_table2 <- melt(inb_table2, id = c("id"))
# colnames(inb_table2) <- c("esc_type", "chauffeur", "freq_inb")
# inb_table2$esc_type <- as.character(inb_table2$esc_type)
# inb_table2$chauffeur <- as.character(inb_table2$chauffeur)
# inb_table2 <- inb_table2[inb_table2$esc_type!="Sum",]
# inb_table2$chauffeur[inb_table2$chauffeur=="Sum"] <- "Total"
# 
# table2 <- inb_table2[,1:2]
# table2$freq_out <- out_table2$freq_out[match(paste(table2$esc_type, table2$chauffeur, sep = "-"), paste(out_table2$esc_type, out_table2$chauffeur, sep = "-"))]
# table2$freq_inb <- inb_table2$freq_inb[match(paste(table2$esc_type, table2$chauffeur, sep = "-"), paste(inb_table2$esc_type, inb_table2$chauffeur, sep = "-"))]
# #table2 <- out_table2
# #table2$freq_inb <- inb_table2$freq_inb
# table2$esc_type[table2$esc_type=="1"] <- "Ride Share"
# table2$esc_type[table2$esc_type=="2"] <- "Pure Escort"
# table2$esc_type[table2$esc_type=="3"] <- "No Escort"
# table2$chauffeur[table2$chauffeur=='1'] <- "FT Worker"
# table2$chauffeur[table2$chauffeur=='2'] <- "PT Worker"
# table2$chauffeur[table2$chauffeur=='3'] <- "Univ Stud"
# table2$chauffeur[table2$chauffeur=='4'] <- "Non-Worker"
# table2$chauffeur[table2$chauffeur=='5'] <- "Retiree"
# table2$chauffeur[table2$chauffeur=='6'] <- "Driv Student"

worker_table <- as.data.frame.matrix(worker_table)
colnames(worker_table)[which(colnames(worker_table) == "1")] = "Ride Share"
colnames(worker_table)[which(colnames(worker_table) == "2")] = "Pure Escort"
colnames(worker_table)[which(colnames(worker_table) == "3")] = "No Escort"
colnames(worker_table)[which(colnames(worker_table) == "Sum")] = "Total"
#colnames(worker_table) <- c("Ride Share", "Pure Escort", "No Escort", "Total")
worker_table$DropOff <- row.names(worker_table)
worker_table$DropOff[worker_table$DropOff=="1"] <- "Ride Share"
worker_table$DropOff[worker_table$DropOff=="2"] <- "Pure Escort"
worker_table$DropOff[worker_table$DropOff=="3"] <- "No Escort"
worker_table$DropOff[worker_table$DropOff=="Sum"] <- "Total"

#worker_table <- worker_table[, c("DropOff", "Ride Share","Pure Escort","No Escort","Total")]

## write outputs
fwrite(table1, file.path(outdir, "esctype_by_childtype.csv"), row.names = F)
# fwrite(table2, file.path(outdir, "esctype_by_chauffeurtype.csv"), row.names = F)
fwrite(worker_table, file.path(outdir, "worker_school_escorting.csv"), row.names = F)


# detach("package:dplyr", unload=TRUE)


#### Stop purpose lookup proportions ####
stops_temp <- stops[, c("stops", "TOURPURP", "IS_INBOUND", "ANCHOR_DEPART_HOUR", "ANCHOR_DEPART_MIN", 
                        "PERSONTYPE", "DEST_PURP", "finalweight", "FULLY_JOINT", "SUBTOUR")]
stops_temp <- stops_temp[!is.na(stops_temp$ANCHOR_DEPART_HOUR), ]
stops_temp$TOD <- 1 #by default only 1 TOD
# only work tours have two TODs which are different for outbound and inbound leg
# outbound worktour: TOD1 - 4:00 to 9:00,  TOD2 - 9:00  to 24:00
# inbound worktout : TOD1 - 4:30 to 15:00, TOD2 - 15:00 to 24:00
stops_temp$TOD[stops_temp$ANCHOR_DEPART_HOUR>=9 & stops_temp$TOURPURP==1 & stops_temp$IS_INBOUND==0]<-2
stops_temp$TOD[stops_temp$ANCHOR_DEPART_HOUR>=15 & stops_temp$TOURPURP==1 & stops_temp$IS_INBOUND==1]<-2

# joint stops
jstops_temp <- jstops[, c("stops", "TOURPURP", "IS_INBOUND", "ANCHOR_DEPART_HOUR", "ANCHOR_DEPART_MIN", 
                          "MAX_AGE_PERTYPE", "DEST_PURP", "finalweight")]
jstops_temp <- jstops_temp[!is.na(jstops_temp$ANCHOR_DEPART_HOUR), ]
jstops_temp$TOD <- 1 #by default only 1 TOD

stops_temp$weight1 = ifelse((stops_temp$DEST_PURP==1 & stops_temp$FULLY_JOINT==0),stops_temp$finalweight, 0)
stops_temp$weight2 = ifelse((stops_temp$DEST_PURP==2 & stops_temp$FULLY_JOINT==0),stops_temp$finalweight, 0)
stops_temp$weight3 = ifelse((stops_temp$DEST_PURP==3 & stops_temp$FULLY_JOINT==0),stops_temp$finalweight, 0)
stops_temp$weight4 = ifelse((stops_temp$DEST_PURP==4 & stops_temp$FULLY_JOINT==0),stops_temp$finalweight, 0)
stops_temp$weight5 = ifelse((stops_temp$DEST_PURP==5 & stops_temp$FULLY_JOINT==0),stops_temp$finalweight, 0)
stops_temp$weight6 = ifelse((stops_temp$DEST_PURP==6 & stops_temp$FULLY_JOINT==0),stops_temp$finalweight, 0)
stops_temp$weight7 = ifelse((stops_temp$DEST_PURP==7 & stops_temp$FULLY_JOINT==0),stops_temp$finalweight, 0)
stops_temp$weight8 = ifelse((stops_temp$DEST_PURP==8 & stops_temp$FULLY_JOINT==0),stops_temp$finalweight, 0)
stops_temp$weight9 = ifelse((stops_temp$DEST_PURP==9 & stops_temp$FULLY_JOINT==0),stops_temp$finalweight, 0)

# recode tour purpose to separate out at work subtours
stops_temp$TOURPURP <- ifelse(stops_temp$SUBTOUR==1, 10, stops_temp$TOURPURP)

jstops_temp$weight5 = ifelse((jstops_temp$DEST_PURP==5),jstops_temp$finalweight, 0)
jstops_temp$weight6 = ifelse((jstops_temp$DEST_PURP==6),jstops_temp$finalweight, 0)
jstops_temp$weight7 = ifelse((jstops_temp$DEST_PURP==7),jstops_temp$finalweight, 0)
jstops_temp$weight8 = ifelse((jstops_temp$DEST_PURP==8),jstops_temp$finalweight, 0)
jstops_temp$weight9 = ifelse((jstops_temp$DEST_PURP==9),jstops_temp$finalweight, 0)

#stop_prop <- aggregate(stops_temp[,c("weight1", "weight2", "weight3", "weight4", "weight5", "weight6", "weight7", "weight8")], by=c("TOURPURP"), FUN = sum)
stop_prop1 <- aggregate(weight1~TOURPURP+IS_INBOUND+TOD+PERSONTYPE, data = stops_temp[((stops_temp$TOURPURP>=1 & stops_temp$TOURPURP<=10) | stops_temp$TOURPURP==13),], FUN = sum)
stop_prop2 <- aggregate(weight2~TOURPURP+IS_INBOUND+TOD+PERSONTYPE, data = stops_temp[((stops_temp$TOURPURP>=1 & stops_temp$TOURPURP<=10) | stops_temp$TOURPURP==13),], FUN = sum)
stop_prop3 <- aggregate(weight3~TOURPURP+IS_INBOUND+TOD+PERSONTYPE, data = stops_temp[((stops_temp$TOURPURP>=1 & stops_temp$TOURPURP<=10) | stops_temp$TOURPURP==13),], FUN = sum)
stop_prop4 <- aggregate(weight4~TOURPURP+IS_INBOUND+TOD+PERSONTYPE, data = stops_temp[((stops_temp$TOURPURP>=1 & stops_temp$TOURPURP<=10) | stops_temp$TOURPURP==13),], FUN = sum)
stop_prop5 <- aggregate(weight5~TOURPURP+IS_INBOUND+TOD+PERSONTYPE, data = stops_temp[((stops_temp$TOURPURP>=1 & stops_temp$TOURPURP<=10) | stops_temp$TOURPURP==13),], FUN = sum)
stop_prop6 <- aggregate(weight6~TOURPURP+IS_INBOUND+TOD+PERSONTYPE, data = stops_temp[((stops_temp$TOURPURP>=1 & stops_temp$TOURPURP<=10) | stops_temp$TOURPURP==13),], FUN = sum)
stop_prop7 <- aggregate(weight7~TOURPURP+IS_INBOUND+TOD+PERSONTYPE, data = stops_temp[((stops_temp$TOURPURP>=1 & stops_temp$TOURPURP<=10) | stops_temp$TOURPURP==13),], FUN = sum)
stop_prop8 <- aggregate(weight8~TOURPURP+IS_INBOUND+TOD+PERSONTYPE, data = stops_temp[((stops_temp$TOURPURP>=1 & stops_temp$TOURPURP<=10) | stops_temp$TOURPURP==13),], FUN = sum)
stop_prop9 <- aggregate(weight9~TOURPURP+IS_INBOUND+TOD+PERSONTYPE, data = stops_temp[((stops_temp$TOURPURP>=1 & stops_temp$TOURPURP<=10) | stops_temp$TOURPURP==13),], FUN = sum)

stop_prop <- data.frame(stop_prop1, stop_prop2$weight2, stop_prop3$weight3, stop_prop4$weight4, stop_prop5$weight5
                        , stop_prop6$weight6, stop_prop7$weight7, stop_prop8$weight8, stop_prop9$weight9)
colnames(stop_prop) <- c("TOURPURP", "IS_INBOUND", "TOD", "PERSONTYPE", "work", "univ", "scho", "esco", "shop", "main", "eati", "visi", "disc")

#only purpose 5 to 9 are allowed for joint tours in the model, so only these purpose joint tours are included in stop purpose lookups
#Stop purpose X Tour Purpose summary shows joint tours of all purposes [possible discrepancies in the data]

jstop_prop5 <- aggregate(weight5~TOURPURP+IS_INBOUND+TOD+MAX_AGE_PERTYPE, data = jstops_temp[jstops_temp$TOURPURP>=5 & jstops_temp$TOURPURP<=9,], FUN = sum)
jstop_prop6 <- aggregate(weight6~TOURPURP+IS_INBOUND+TOD+MAX_AGE_PERTYPE, data = jstops_temp[jstops_temp$TOURPURP>=5 & jstops_temp$TOURPURP<=9,], FUN = sum)
jstop_prop7 <- aggregate(weight7~TOURPURP+IS_INBOUND+TOD+MAX_AGE_PERTYPE, data = jstops_temp[jstops_temp$TOURPURP>=5 & jstops_temp$TOURPURP<=9,], FUN = sum)
jstop_prop8 <- aggregate(weight8~TOURPURP+IS_INBOUND+TOD+MAX_AGE_PERTYPE, data = jstops_temp[jstops_temp$TOURPURP>=5 & jstops_temp$TOURPURP<=9,], FUN = sum)
jstop_prop9 <- aggregate(weight9~TOURPURP+IS_INBOUND+TOD+MAX_AGE_PERTYPE, data = jstops_temp[jstops_temp$TOURPURP>=5 & jstops_temp$TOURPURP<=9,], FUN = sum)

jstop_prop <- data.frame(jstop_prop5, jstop_prop6$weight6, jstop_prop7$weight7, jstop_prop8$weight8, jstop_prop9$weight9)
colnames(jstop_prop) <- c("TOURPURP","IS_INBOUND","TOD","MAX_AGE_PERTYPE","shop", "main", "eati", "visi", "disc")

fwrite(stop_prop, file.path(outdir, "istop_purpose_lookups.csv"), row.names = F)
fwrite(jstop_prop, file.path(outdir, "jstop_purpose_lookups.csv"), row.names = F)

#### stop depart/arrive proportions ####
#View(stops_temp[,c("IS_INBOUND","TRIP_ID", "stop_id", "ORIG_ARR_BIN", 
#                   "ORIG_DEP_BIN", "DEST_ARR_BIN", "DEST_DEP_BIN", "TRIP_DUR_BIN")])
#
#View(trips[trips$HH_ID==1000395 & trips$PER_ID==2,c("HH_ID","PER_ID","TOUR_ID", "IS_INBOUND","TRIP_ID", "DEST_PURP", "ORIG_ARR_HR", "ORIG_ARR_MIN", 
#                                                    "ORIG_DEP_HR", "ORIG_DEP_MIN", "DEST_ARR_HR", "DEST_ARR_MIN", "DEST_DEP_HR", 
#                                                    "DEST_DEP_MIN", "TRIP_DUR_BIN", "ORIG_ARR_BIN", "ORIG_DEP_BIN", "DEST_ARR_BIN", "DEST_DEP_BIN")])  

#assign stop_id to stops in outbound and inbound direction [stops on individual tours]
stops = as.data.frame(stops)
istops_temp1 <- ddply(stops[stops$TOURPURP<=10,], .(HH_ID, PER_ID, TOUR_ID, IS_INBOUND), mutate, STOP_ID = seq_along(TRIP_ID))
istops_temp1$STOP_ID[istops_temp1$STOP_ID>4] <- 4

istop_arr_dep <- xtabs(finalweight~TOURPURP+IS_INBOUND+STOP_ID+DEST_ARR_BIN+DEST_DEP_BIN, data = istops_temp1)
istop_arr_dep <- data.frame(istop_arr_dep, stringsAsFactors = F)
istop_arr_dep$TOURPURP     <- as.numeric(as.character(istop_arr_dep$TOURPURP))
istop_arr_dep$IS_INBOUND   <- as.numeric(as.character(istop_arr_dep$IS_INBOUND))
istop_arr_dep$STOP_ID      <- as.numeric(as.character(istop_arr_dep$STOP_ID))
istop_arr_dep$DEST_ARR_BIN <- as.numeric(as.character(istop_arr_dep$DEST_ARR_BIN))

istop_arr_dep$tempid <- istop_arr_dep$TOURPURP*10000+istop_arr_dep$IS_INBOUND*1000+istop_arr_dep$STOP_ID*100+istop_arr_dep$DEST_ARR_BIN
istop_arr_dep$tempid <- as.factor(istop_arr_dep$tempid)
istop_arr_dep_dist <- reshape(istop_arr_dep[,c("tempid", "DEST_DEP_BIN", "Freq")], idvar = "tempid", timevar = "DEST_DEP_BIN", direction = "wide")

istop_arr_dep_dist$tempid <- as.numeric(as.character((istop_arr_dep_dist$tempid)))
istop_arr_dep_dist$TOURPURP <- round(istop_arr_dep_dist$tempid/10000)
istop_arr_dep_dist$IS_INBOUND <- round((istop_arr_dep_dist$tempid-10000*istop_arr_dep_dist$TOURPURP)/1000)
istop_arr_dep_dist$STOP_ID <- round((istop_arr_dep_dist$tempid-10000*istop_arr_dep_dist$TOURPURP-1000*istop_arr_dep_dist$IS_INBOUND)/100)
istop_arr_dep_dist$DEST_ARR_BIN <- istop_arr_dep_dist$tempid-10000*istop_arr_dep_dist$TOURPURP-1000*istop_arr_dep_dist$IS_INBOUND-100*istop_arr_dep_dist$STOP_ID

#assign stop_id to stops in outbound and inbound direction [stops on joint tours]
jstops_temp1 <- ddply(jstops[jstops$TOURPURP<=10,], .(HH_ID, TOUR_ID, IS_INBOUND), mutate, STOP_ID = seq_along(JTRIP_ID))
jstops_temp1$STOP_ID[jstops_temp1$STOP_ID>4] <- 4

jstop_arr_dep <- xtabs(finalweight~TOURPURP+IS_INBOUND+STOP_ID+DEST_ARR_BIN+DEST_DEP_BIN, data = jstops_temp1)
jstop_arr_dep <- data.frame(jstop_arr_dep, stringsAsFactors = F)
jstop_arr_dep$TOURPURP     <- as.numeric(as.character(jstop_arr_dep$TOURPURP))
jstop_arr_dep$IS_INBOUND   <- as.numeric(as.character(jstop_arr_dep$IS_INBOUND))
jstop_arr_dep$STOP_ID      <- as.numeric(as.character(jstop_arr_dep$STOP_ID))
jstop_arr_dep$DEST_ARR_BIN <- as.numeric(as.character(jstop_arr_dep$DEST_ARR_BIN))

jstop_arr_dep$tempid <- jstop_arr_dep$TOURPURP*10000+jstop_arr_dep$IS_INBOUND*1000+jstop_arr_dep$STOP_ID*100+jstop_arr_dep$DEST_ARR_BIN
jstop_arr_dep$tempid <- as.factor(jstop_arr_dep$tempid)
jstop_arr_dep_dist <- reshape(jstop_arr_dep[,c("tempid", "DEST_DEP_BIN", "Freq")], idvar = "tempid", timevar = "DEST_DEP_BIN", direction = "wide")

jstop_arr_dep_dist$tempid <- as.numeric(as.character((jstop_arr_dep_dist$tempid)))
jstop_arr_dep_dist$TOURPURP <- round(jstop_arr_dep_dist$tempid/10000)
jstop_arr_dep_dist$IS_INBOUND <- round((jstop_arr_dep_dist$tempid-10000*jstop_arr_dep_dist$TOURPURP)/1000)
jstop_arr_dep_dist$STOP_ID <- round((jstop_arr_dep_dist$tempid-10000*jstop_arr_dep_dist$TOURPURP-1000*jstop_arr_dep_dist$IS_INBOUND)/100)
jstop_arr_dep_dist$DEST_ARR_BIN <- jstop_arr_dep_dist$tempid-10000*jstop_arr_dep_dist$TOURPURP-1000*jstop_arr_dep_dist$IS_INBOUND-100*jstop_arr_dep_dist$STOP_ID


## write stop arrival departure distributions
fwrite(istop_arr_dep_dist, file.path(outdir, "istop_arr_dep_distributions.csv"), row.names = F)
fwrite(jstop_arr_dep_dist, file.path(outdir, "jstop_arr_dep_distributions.csv"), row.names = F)



#finish


# Turn back warnings;
options(warn = oldw)
rm(list=ls())
gc()

