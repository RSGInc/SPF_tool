#######################################################
### Script for summarizing SANDAG HTS
### Author: Binny M Paul, binny.paul@rsginc.com
### Sep 2017
### Copy of OHAS processing script
#######################################################

## Libraries
############

library(plyr)
library(weights)
library(reshape)

## User Inputs
###############
# Directories
WD                   <- "E:/Projects/Clients/sandag/TO21_Recalibration/data/HTS_Summaries"
Survey_Dir           <- "E:/Projects/Clients/sandag/TO21_Recalibration/SANDAG_Data_Proc/processed/new"
Survey_Processed_Dir <- "E:/Projects/Clients/sandag/TO21_Recalibration/SANDAG_Data_Proc/processed/output"
SkimDir              <- "E:/Projects/Clients/sandag/TO21_Recalibration/skim"
SHP_Dir              <- "E:/Projects/Clients/sandag/TO21_Recalibration/SHP/zones"
WeightsDir           <- "E:/Projects/Clients/sandag/TO21_Recalibration/data/weights"

## Read Data
zones_xwalk          <- read.csv(paste(SHP_Dir, "zones.csv", sep = "/"), as.is = T)
hh                   <- read.csv(paste(Survey_Dir, "hh.csv", sep = "/"), as.is = T)
per                  <- read.csv(paste(Survey_Dir, "person.csv", sep = "/"), as.is = T)
day                  <- read.csv(paste(Survey_Dir, "SDRTS_Day_Data_20170731.csv", sep = "/"), as.is = T)
multiDayWeights      <- read.csv(paste(WeightsDir, "tripweights_completeweekdays.csv", sep = "/"), as.is = T)
#hts_trips            <- read.csv("E:/Projects/Clients/sandag/TO21_Recalibration/data/latest/SDRTS_Trip_Data_20170731.csv", as.is = T)

# Filter records to use only completed households and persons on weekdays
day <- day[day$day_iscomplete==1 & day$day_hhcomplete==1 & day$travel_dow>=1 & day$travel_dow<=5,]

# Generate person-day weigths for all travel database
# multi day weigths are unique across all trips for a person-day, therefore simple aggregation yields person-day weights
multiDayWeights <- unique(multiDayWeights[,c("hhid", "pernum", "daynum", "adjusted_multiday_tripweight_456x")])

# create hh_day and person_day tables
hhday <- unique(day[,c("hhid", "daynum", "travel_dow", "multiday_weight_456x")])
hhday <- cbind(hhday, hh[match(hhday$hhid, hh$SAMPN), -which(names(hhday) %in% c("hhid"))])

perday <- unique(day[,c("hhid", "pernum", "daynum", "travel_dow", "multiday_weight_456x")])
perday <- cbind(perday, per[match(perday$hhid*100+perday$pernum, per$SAMPN*100+per$PERNO), -which(names(perday) %in% c("hhid", "pernum"))])
perday$puid <- paste(perday$hhid, perday$pernum, perday$daynum, sep = "-")
perday$uid <- paste(perday$hhid, perday$daynum, sep = "-")

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
unique_joint_tours   <- read.csv(paste(dayDir, "unique_joint_tours.csv", sep = "/"), as.is = T)
jtrips               <- read.csv(paste(dayDir, "unique_joint_ultrips.csv", sep = "/"), as.is = T)

tours$DAYNO              <- dayno
trips$DAYNO              <- dayno
unique_joint_tours$DAYNO <- dayno
jtrips$DAYNO             <- dayno

# puid
tours$puid              <- paste(tours$HH_ID, tours$PER_ID, tours$DAYNO, sep = "-")
trips$puid              <- paste(trips$HH_ID, trips$PER_ID, trips$DAYNO, sep = "-")
unique_joint_tours$uid  <- paste(unique_joint_tours$HH_ID, unique_joint_tours$DAYNO, sep = "-")
jtrips$uid              <- paste(jtrips$HH_ID, jtrips$DAYNO, sep = "-")

#filter
tours <- tours[tours$puid %in% perday$puid,]
trips <- trips[trips$puid %in% perday$puid,]
unique_joint_tours <- unique_joint_tours[unique_joint_tours$uid %in% unique(perday$uid), ]
jtrips <- jtrips[jtrips$uid %in% unique(perday$uid), ]

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
  unique_joint_tours <- rbind(unique_joint_tours, t3)
  jtrips <- rbind(jtrips, t4)
}

# replace all "nan" in Python outputs with 0
tours[tours=="nan"] <- 0
trips[trips=="nan"] <- 0
unique_joint_tours[unique_joint_tours=="nan"] <- 0
jtrips[jtrips=="nan"] <- 0
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
trips$finalweight <- multiDayWeights$adjusted_multiday_tripweight_456x[match(paste(trips$HH_ID, trips$PER_ID, trips$DAYNO, sep = "-"), 
                                                                             paste(multiDayWeights$hhid, multiDayWeights$pernum, multiDayWeights$daynum, sep = "-"))]
tours$finalweight <- multiDayWeights$adjusted_multiday_tripweight_456x[match(paste(tours$HH_ID, tours$PER_ID, tours$DAYNO, sep = "-"), 
                                                                             paste(multiDayWeights$hhid, multiDayWeights$pernum, multiDayWeights$daynum, sep = "-"))]

# copy the weights for joint_trips and joint_tours from the trip and tours file
jtrips$finalweight <- trips$finalweight[match(jtrips$DAYNO*10000+jtrips$HH_ID*1000+jtrips$JTRIP_ID,trips$DAYNO*10000+trips$HH_ID*1000+trips$JTRIP_ID)]

unique_joint_tours$finalweight <- tours$finalweight[match(unique_joint_tours$DAYNO*1000+unique_joint_tours$HH_ID*100+unique_joint_tours$JTOUR_ID,
                                                    tours$DAYNO*1000+tours$HH_ID*100+tours$JTOUR_ID)]

hh$finalweight <- hh$hh_final_weight_456x
per$finalweight <- per$hh_final_weight_456x
hhday$finalweight <- hhday$multiday_weight_456x
perday$finalweight <- perday$multiday_weight_456x

jutrips <- unique_joint_tours


#### Debugging
#hts_trips$puid <- paste(hts_trips$hhid, hts_trips$pernum, hts_trips$daynum, sep = "-")
#write.csv(hts_trips, "hts_trips.csv", row.names = F)
#write.csv(trips, "trips.csv", row.names = F)

DST_SKM <- read.csv(paste(SkimDir, "MD_TOLL_DST_SKM.csv", sep = "/"), stringsAsFactors = F, header = F, col.names = c("o", "d", "dist"))

# Define other variables
pertypeCodes <- data.frame(code = c(1,2,3,4,5,6,7,8,"All"), 
                           name = c("FT Worker", "PT Worker", "Univ Stud", "Non Worker", "Retiree", "Driv Stud", "NonDriv Stud", "Pre-School", "All"))


# Prepare files for computing summary statistics
###################################################

setwd(WD)

# -----------------------------------------------------------------
# rename variables in SANDAG HTS to standard format in this script
# -----------------------------------------------------------------
names(hh)[names(hh)=="HOME_TAZ"] <- 'HHTAZ'
names(hhday)[names(hhday)=="HOME_TAZ"] <- 'HHTAZ'

names(per)[names(per)=="WORK_TAZ"] <- 'WTAZ'
names(perday)[names(perday)=="WORK_TAZ"] <- 'WTAZ'
names(per)[names(per)=="SCHOOL_TAZ"] <- 'STAZ'
names(perday)[names(perday)=="SCHOOL_TAZ"] <- 'STAZ'


names(processedPerson)[names(processedPerson)=="HH_ID"] <- "HHID"
names(processedPerson)[names(processedPerson)=="PER_ID"] <- "PERID"


hh$HHVEH[hh$vehicle_count == 0] <- 0
hh$HHVEH[hh$vehicle_count == 1] <- 1
hh$HHVEH[hh$vehicle_count == 2] <- 2
hh$HHVEH[hh$vehicle_count == 3] <- 3
hh$HHVEH[hh$vehicle_count >= 4] <- 4

hh$HHSIZE[hh$hhsize == 1] <- 1
hh$HHSIZE[hh$hhsize == 2] <- 2
hh$HHSIZE[hh$hhsize == 3] <- 3
hh$HHSIZE[hh$hhsize == 4] <- 4
hh$HHSIZE[hh$hhsize >= 5] <- 5

#Adults in the HH
adults <- count(per[!is.na(per$AGE_CAT),], c("SAMPN"), "AGE_CAT>=4")
hh$ADULTS <- adults$freq[match(hh$SAMPN, adults$SAMPN)]
hh$ADULTS[is.na(hh$ADULTS)] <- 0

# define HCOUNTY [TAZ<=100, TAZ>100]
hh$HCOUNTY[hh$HHTAZ<=100] <- 1
hh$HCOUNTY[hh$HHTAZ>100] <- 2

per$PERTYPE <- processedPerson$PERSONTYPE[match(per$SAMPN*10+per$PERNO, processedPerson$HHID*10+processedPerson$PERID)]
per$HHTAZ <- hh$HHTAZ[match(per$SAMPN, hh$SAMPN)]
per$XCORD <- hh$home_lng[match(per$SAMPN, hh$SAMPN)]
per$YCORD <- hh$home_lat[match(per$SAMPN, hh$SAMPN)]
per$HCOUNTY <- hh$HCOUNTY[match(per$SAMPN, hh$SAMPN)]
per$WCOUNTY[per$WTAZ<=100] <- 1
per$WCOUNTY[per$WTAZ>100] <- 2
#per$WHOME[is.na(per$WHOME)] <- 0

## copy attributes from per to perday and hh to hhday
names(hhday)[names(hhday)=="hhid"] <- "SAMPN"
names(hhday)[names(hhday)=="daynum"] <- "DAYNO"
hhday$HHVEH   <- hh$HHVEH[match(hhday$SAMPN, hh$SAMPN)]
hhday$HHSIZE  <- hh$HHSIZE[match(hhday$SAMPN, hh$SAMPN)]
hhday$HCOUNTY <- hh$HCOUNTY[match(hhday$SAMPN, hh$SAMPN)]
hhday$ADULTS  <- hh$ADULTS[match(hhday$SAMPN, hh$SAMPN)]

names(perday)[names(perday)=="hhid"] <- "SAMPN"
names(perday)[names(perday)=="pernum"] <- "PERNO"
names(perday)[names(perday)=="daynum"] <- "DAYNO"
perday$PERTYPE <- per$PERTYPE[match(perday$SAMPN*100+perday$PERNO, per$SAMPN*100+per$PERNO)]
perday$HHTAZ   <- per$HHTAZ[match(perday$SAMPN*100+perday$PERNO, per$SAMPN*100+per$PERNO)]
perday$XCORD   <- per$XCORD[match(perday$SAMPN*100+perday$PERNO, per$SAMPN*100+per$PERNO)]
perday$YCORD   <- per$YCORD[match(perday$SAMPN*100+perday$PERNO, per$SAMPN*100+per$PERNO)]
perday$HCOUNTY <- per$HCOUNTY[match(perday$SAMPN*100+perday$PERNO, per$SAMPN*100+per$PERNO)]
perday$WCOUNTY <- per$WCOUNTY[match(perday$SAMPN*100+perday$PERNO, per$SAMPN*100+per$PERNO)]


#--------Compute Summary Statistics-------
#*****************************************

# Auto ownership
autoOwnership <- count(hh[!is.na(hh$HHVEH),], c("HHVEH"), "finalweight")
write.csv(autoOwnership, "autoOwnership.csv", row.names = TRUE)

# Persons by person type
pertypeDistbn <- count(per[!is.na(per$PERTYPE),], c("PERTYPE"), "finalweight")
write.csv(pertypeDistbn, "pertypeDistbn.csv", row.names = TRUE)

# Mandatory DC
workers <- per[!is.na(per$WTAZ), c("SAMPN", "PERNO", "HHTAZ", "WTAZ", "job_type","PERTYPE", "HCOUNTY", "finalweight")]
workers$WDIST <- DST_SKM$dist[match(paste(workers$HHTAZ, workers$WTAZ, sep = "-"), paste(DST_SKM$o, DST_SKM$d, sep = "-"))]

#workers$hwindex<-match(workers$HHTAZ, skimLookUp$Lookup)
#workers$windex<-match(workers$WTAZ, skimLookUp$Lookup)
#workers$WDIST<-skimMat3[cbind(workers$hwindex, workers$windex)]

students <- per[!is.na(per$STAZ), c("SAMPN", "PERNO", "HHTAZ", "STAZ", "PERTYPE", "HCOUNTY", "finalweight")]
students$SDIST <- DST_SKM$dist[match(paste(students$HHTAZ, students$STAZ, sep = "-"), paste(DST_SKM$o, DST_SKM$d, sep = "-"))]

#students$hsindex<-match(students$HHTAZ, skimLookUp$Lookup)
#students$sindex<-match(students$STAZ, skimLookUp$Lookup)
#students$SDIST<-skimMat3[cbind(students$hsindex, students$sindex)]


tlfd_work_Jackson <- wtd.hist(workers$WDIST[!is.na(workers$WDIST) & workers$job_type!=3 & workers$HCOUNTY==1], breaks = c(seq(0,40, by=1), 9999), freq = NULL, right=FALSE, weight = workers$finalweight[!is.na(workers$WDIST) & workers$job_type!=3 & workers$HCOUNTY==1])
tlfd_work_Josephine <- wtd.hist(workers$WDIST[!is.na(workers$WDIST) & workers$job_type!=3 & workers$HCOUNTY==2], breaks = c(seq(0,40, by=1), 9999), freq = NULL, right=FALSE, weight = workers$finalweight[!is.na(workers$WDIST) & workers$job_type!=3 & workers$HCOUNTY==2])
tlfd_work_Total <- wtd.hist(workers$WDIST[!is.na(workers$WDIST) & workers$job_type!=3], breaks = c(seq(0,40, by=1), 9999), freq = NULL, right=FALSE, weight = workers$finalweight[!is.na(workers$WDIST) & workers$job_type!=3])

tlfd_univ_Jackson <- wtd.hist(students$SDIST[!is.na(students$SDIST) & students$HCOUNTY==1 & students$PERTYPE==3], breaks = c(seq(0,40, by=1), 9999), freq = NULL, right=FALSE, weight=students$finalweight[!is.na(students$SDIST) & students$HCOUNTY==1 & students$PERTYPE==3])
tlfd_univ_Josephine <- wtd.hist(students$SDIST[!is.na(students$SDIST) & students$HCOUNTY==2 & students$PERTYPE==3], breaks = c(seq(0,40, by=1), 9999), freq = NULL, right=FALSE, weight=students$finalweight[!is.na(students$SDIST) & students$HCOUNTY==2 & students$PERTYPE==3])
tlfd_univ_Total <- wtd.hist(students$SDIST[!is.na(students$SDIST) & students$PERTYPE==3], breaks = c(seq(0,40, by=1), 9999), freq = NULL, right=FALSE, weight=students$finalweight[!is.na(students$SDIST) & students$PERTYPE==3])

tlfd_schl_Jackson <- wtd.hist(students$SDIST[!is.na(students$SDIST) & students$HCOUNTY==1 & students$PERTYPE>=6], breaks = c(seq(0,40, by=1), 9999), freq = NULL, right=FALSE, weight=students$finalweight[!is.na(students$SDIST) & students$HCOUNTY==1 & students$PERTYPE>=6])
tlfd_schl_Josephine <- wtd.hist(students$SDIST[!is.na(students$SDIST) & students$HCOUNTY==2 & students$PERTYPE>=6], breaks = c(seq(0,40, by=1), 9999), freq = NULL, right=FALSE, weight=students$finalweight[!is.na(students$SDIST) & students$HCOUNTY==2 & students$PERTYPE>=6])
tlfd_schl_Total <- wtd.hist(students$SDIST[!is.na(students$SDIST) & students$PERTYPE>=6], breaks = c(seq(0,40, by=1), 9999), freq = NULL, right=FALSE, weight=students$finalweight[!is.na(students$SDIST) & students$PERTYPE>=6])

OHASmandatoryDC <- data.frame(tlfd_work_Jackson$count, tlfd_univ_Jackson$count, tlfd_schl_Jackson$count, 
                              tlfd_work_Josephine$count, tlfd_univ_Josephine$count, tlfd_schl_Josephine$count, 
                              tlfd_work_Total$count, tlfd_univ_Total$count, tlfd_schl_Total$count)
colnames(OHASmandatoryDC) <- c("Work_Jackson", "University_Jackson", "School_Jackson", 
                               "Work_Josephine", "University_Josephine", "School_Josephine",
                               "Work_Total", "University_Total", "School_Total")

cat("\n Average distance to workplace (Jackson): ", weighted.mean(workers$WDIST[workers$HCOUNTY == 1 & workers$job_type != 3], workers$finalweight[workers$HCOUNTY == 1 & workers$job_type != 3], na.rm = TRUE))
cat("\n Average distance to university (Jackson): ", weighted.mean(students$SDIST[students$HCOUNTY == 1 & students$PERTYPE == 3], students$finalweight[students$HCOUNTY == 1 & students$PERTYPE == 3], na.rm = TRUE))
cat("\n Average distance to school (Jackson): ", weighted.mean(students$SDIST[students$HCOUNTY == 1 & students$PERTYPE >= 6 & students$PERTYPE <= 7], students$finalweight[students$HCOUNTY == 1 & students$PERTYPE >= 6 & students$PERTYPE <= 7], na.rm = TRUE))

cat("\n Average distance to workplace (Josephine): ", weighted.mean(workers$WDIST[workers$HCOUNTY == 2 & workers$job_type != 3], workers$finalweight[workers$HCOUNTY == 2 & workers$job_type != 3], na.rm = TRUE))
cat("\n Average distance to university (Josephine): ", weighted.mean(students$SDIST[students$HCOUNTY == 2 & students$PERTYPE == 3], students$finalweight[students$HCOUNTY == 2 & students$PERTYPE == 3], na.rm = TRUE))
cat("\n Average distance to school (Josephine): ", weighted.mean(students$SDIST[students$HCOUNTY == 2 & students$PERTYPE >= 6 & students$PERTYPE <= 7], students$finalweight[students$HCOUNTY == 2 & students$PERTYPE >= 6 & students$PERTYPE <= 7], na.rm = TRUE))

cat("\n Average distance to workplace (Total): ", weighted.mean(workers$WDIST[workers$job_type != 3], workers$finalweight[workers$job_type != 3], na.rm = TRUE))
cat("\n Average distance to university (Total): ", weighted.mean(students$SDIST[students$PERTYPE == 3], students$finalweight[students$PERTYPE == 3], na.rm = TRUE))
cat("\n Average distance to school (Total): ", weighted.mean(students$SDIST[students$PERTYPE >= 6 & students$PERTYPE <= 7], students$finalweight[students$PERTYPE >= 6 & students$PERTYPE <= 7], na.rm = TRUE))


write.csv(OHASmandatoryDC, "mandatoryTLFD.csv", row.names = TRUE)

## Output avg trip lengths for visualizer
workTripLengths <- c(weighted.mean(workers$WDIST[workers$HCOUNTY == 1 & workers$job_type != 3], workers$finalweight[workers$HCOUNTY == 1 & workers$job_type != 3], na.rm = TRUE),
                     weighted.mean(workers$WDIST[workers$HCOUNTY == 2 & workers$job_type != 3], workers$finalweight[workers$HCOUNTY == 2 & workers$job_type != 3], na.rm = TRUE),
                     weighted.mean(workers$WDIST[workers$job_type != 3], workers$finalweight[workers$job_type != 3], na.rm = TRUE))

univTripLengths <- c(weighted.mean(students$SDIST[students$HCOUNTY == 1 & students$PERTYPE == 3], students$finalweight[students$HCOUNTY == 1 & students$PERTYPE == 3], na.rm = TRUE),
                     weighted.mean(students$SDIST[students$HCOUNTY == 2 & students$PERTYPE == 3], students$finalweight[students$HCOUNTY == 2 & students$PERTYPE == 3], na.rm = TRUE),
                     weighted.mean(students$SDIST[students$PERTYPE == 3], students$finalweight[students$PERTYPE == 3], na.rm = TRUE))

schlTripLengths <- c(weighted.mean(students$SDIST[students$HCOUNTY == 1 & students$PERTYPE >= 6 & students$PERTYPE <= 7], students$finalweight[students$HCOUNTY == 1 & students$PERTYPE >= 6 & students$PERTYPE <= 7], na.rm = TRUE),
                     weighted.mean(students$SDIST[students$HCOUNTY == 2 & students$PERTYPE >= 6 & students$PERTYPE <= 7], students$finalweight[students$HCOUNTY == 2 & students$PERTYPE >= 6 & students$PERTYPE <= 7], na.rm = TRUE),
                     weighted.mean(students$SDIST[students$PERTYPE >= 6 & students$PERTYPE <= 7], students$finalweight[students$PERTYPE >= 6 & students$PERTYPE <= 7], na.rm = TRUE))

region <- c("Jackson", "Josephine", "Total")

mandTripLengths <- data.frame(region = region, work = workTripLengths, univ = univTripLengths, schl = schlTripLengths)

write.csv(mandTripLengths, "mandTripLengths.csv", row.names = F)

per$worker[per$PERTYPE==1 | per$PERTYPE==2] <- 1
per$whm[per$job_type==3] <- 1
perday$worker[perday$PERTYPE==1 | perday$PERTYPE==2] <- 1
perday$whm[perday$job_type==3] <- 1
jackworkers <- sum(perday$worker[perday$HCOUNTY==1] * perday$finalweight[perday$HCOUNTY==1], na.rm=TRUE)
jackwhm <- sum(perday$worker[perday$HCOUNTY==1 & perday$whm==1] * perday$finalweight[perday$HCOUNTY==1], na.rm=TRUE)

joseworkers <- sum(perday$worker[perday$HCOUNTY==2] * perday$finalweight[perday$HCOUNTY==2], na.rm=TRUE)
josewhm <- sum(perday$worker[perday$HCOUNTY==2 & perday$whm==1] * perday$finalweight[perday$HCOUNTY==2], na.rm=TRUE)

cat("\n Total workers in Jackson county", jackworkers)
cat("\n Working from home in Jackson county", jackwhm)
cat("\n Total workers in Josephine county", joseworkers)
cat("\n Working from home in Josephine county", josewhm)

wfh_summary <- data.frame(county = c("Jackson", "Josephine"), workers = c(jackworkers, joseworkers), wfh = c(jackwhm, josewhm))
write.csv(wfh_summary, "wfh_summary.csv", row.names = F)

# County-County Flows
josephine_josephine<-sum(per$finalweight[is.na(per$HHTAZ) == 0 & per$HCOUNTY==1 & per$WCOUNTY==1 & (per$PERTYPE==1 | per$PERTYPE==2)], na.rm=TRUE)
josephine_josephine

josephine_jackson<-sum(per$finalweight[is.na(per$HHTAZ) == 0 & per$HCOUNTY==2 & per$WCOUNTY==1 & (per$PERTYPE==1 | per$PERTYPE==2)], na.rm=TRUE)
josephine_jackson

jackson_josephine<-sum(per$finalweight[is.na(per$HHTAZ) == 0 & per$HCOUNTY==1 & per$WCOUNTY==2 & (per$PERTYPE==1 | per$PERTYPE==2)], na.rm=TRUE)
jackson_josephine

jackson_jackson<-sum(per$finalweight[is.na(per$HHTAZ) == 0 & per$HCOUNTY==2 & per$WCOUNTY==2 & (per$PERTYPE==1 | per$PERTYPE==2)], na.rm=TRUE)
jackson_jackson

countyFlows <- data.frame(county = c("Jackson", "Josephine", "Total"), 
                          Jackson = c(jackson_jackson, josephine_jackson, jackson_jackson + josephine_jackson), 
                          Josephine = c(jackson_josephine, josephine_josephine, jackson_josephine + josephine_josephine), 
                          Total = c(jackson_jackson+jackson_josephine, josephine_jackson+josephine_josephine, jackson_jackson + josephine_jackson+jackson_josephine + josephine_josephine))
write.csv(countyFlows, "countyFlows.csv", row.names = F)

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
tours$AUTOSUFF[tours$HHVEH >= tours$ADULTS] <- 2

tours$TOTAL_STOPS <- tours$OUTBOUND_STOPS + tours$INBOUND_STOPS

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


# Recode workrelated tours which are not at work subtour as work tour
tours$TOURPURP[tours$TOURPURP == 10 & tours$IS_SUBTOUR == 0] <- 1

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

# Code tours with out/inbound pure escort as school escort purpose [13]
tours$TOURPURP[(tours$TOURPURP>3 & tours$TOURPURP<10) & (tours$OUT_ESCORTING_TYPE==2 & tours$OUT_ESCORTEE_TOUR_PURP==3)] <- 13
tours$TOURPURP[(tours$TOURPURP>3 & tours$TOURPURP<10) & (tours$INB_ESCORTING_TYPE==2 & tours$INB_ESCORTEE_TOUR_PURP==3)] <- 13

# exclude  school escorting stop from ride sharing mandatory tours
tours$OUTBOUND_STOPS[tours$OUT_ESCORTING_TYPE==1 & tours$OUT_ESCORTEE_TOUR_PURP==3 & tours$OUTBOUND_STOPS>0] <- tours$OUTBOUND_STOPS[tours$OUT_ESCORTING_TYPE==1 & tours$OUT_ESCORTEE_TOUR_PURP==3 & tours$OUTBOUND_STOPS>0] - 1
tours$INBBOUND_STOPS[tours$INB_ESCORTING_TYPE==1 & tours$INB_ESCORTEE_TOUR_PURP==3 & tours$INBOUND_STOPS>0] <- tours$INBOUND_STOPS[tours$INB_ESCORTING_TYPE==1 & tours$INB_ESCORTEE_TOUR_PURP==3 & tours$INBOUND_STOPS>0] - 1

# end of changes made by BMP on 05/09/17

# Copy TOURPURP to trip file
trips$TOURPURP <- tours$TOURPURP[match(trips$HH_ID*10000+trips$PER_ID*1000+trips$TOUR_ID*10+trips$DAYNO, tours$HH_ID*10000+tours$PER_ID*1000+tours$TOUR_ID*10+tours$DAYNO)]

tours$TOTAL_STOPS <- tours$OUTBOUND_STOPS + tours$INBOUND_STOPS

#----------------
#unique_joint_tours$finalweight <- hh$finalweight[match(unique_joint_tours$HH_ID, hh$SAMPN)]
unique_joint_tours <- unique_joint_tours[!is.na(unique_joint_tours$finalweight),]

#copy tour mode & other attributes
unique_joint_tours$TOURMODE <- tours$TOURMODE[match(unique_joint_tours$HH_ID*1000+unique_joint_tours$JTOUR_ID*10+unique_joint_tours$DAYNO,
                                                    tours$HH_ID*1000+tours$JTOUR_ID*10+tours$DAYNO)]
unique_joint_tours$AUTOSUFF <- tours$AUTOSUFF[match(unique_joint_tours$HH_ID*1000+unique_joint_tours$JTOUR_ID*10+unique_joint_tours$DAYNO,
                                                    tours$HH_ID*1000+tours$JTOUR_ID*10+tours$DAYNO)]
unique_joint_tours$ANCHOR_DEPART_BIN <- tours$ANCHOR_DEPART_BIN[match(unique_joint_tours$HH_ID*1000+unique_joint_tours$JTOUR_ID*10+unique_joint_tours$DAYNO,
                                                                      tours$HH_ID*1000+tours$JTOUR_ID*10+tours$DAYNO)]
unique_joint_tours$ANCHOR_ARRIVE_BIN <- tours$ANCHOR_ARRIVE_BIN[match(unique_joint_tours$HH_ID*1000+unique_joint_tours$JTOUR_ID*10+unique_joint_tours$DAYNO,
                                                                      tours$HH_ID*1000+tours$JTOUR_ID*10+tours$DAYNO)]
unique_joint_tours$TOUR_DUR_BIN <- tours$TOUR_DUR_BIN[match(unique_joint_tours$HH_ID*1000+unique_joint_tours$JTOUR_ID*10+unique_joint_tours$DAYNO,
                                                            tours$HH_ID*1000+tours$JTOUR_ID*10+tours$DAYNO)]
unique_joint_tours$DIST <- tours$DIST[match(unique_joint_tours$HH_ID*1000+unique_joint_tours$JTOUR_ID*10+unique_joint_tours$DAYNO,
                                            tours$HH_ID*1000+tours$JTOUR_ID*10+tours$DAYNO)]
unique_joint_tours$DISTMILE <- unique_joint_tours$DIST/5280

unique_joint_tours$OUTBOUND_STOPS <- tours$OUTBOUND_STOPS[match(unique_joint_tours$HH_ID*1000+unique_joint_tours$JTOUR_ID*10+unique_joint_tours$DAYNO,
                                                                tours$HH_ID*1000+tours$JTOUR_ID*10+tours$DAYNO)]
unique_joint_tours$INBOUND_STOPS <- tours$INBOUND_STOPS[match(unique_joint_tours$HH_ID*1000+unique_joint_tours$JTOUR_ID*10+unique_joint_tours$DAYNO,
                                                              tours$HH_ID*1000+tours$JTOUR_ID*10+tours$DAYNO)]
unique_joint_tours$TOTAL_STOPS <- tours$TOTAL_STOPS[match(unique_joint_tours$HH_ID*1000+unique_joint_tours$JTOUR_ID*10+unique_joint_tours$DAYNO,
                                                          tours$HH_ID*1000+tours$JTOUR_ID*10+tours$DAYNO)]
unique_joint_tours$SKIMDIST <- tours$SKIMDIST[match(unique_joint_tours$HH_ID*1000+unique_joint_tours$JTOUR_ID*10+unique_joint_tours$DAYNO,
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

trips$TRIPMODE[trips$TRIPMODE>=1 & trips$TRIPMODE<=2] <- 1
trips$TRIPMODE[trips$TRIPMODE>=3 & trips$TRIPMODE<=4] <- 2
trips$TRIPMODE[trips$TRIPMODE>=5 & trips$TRIPMODE<=6] <- 3
trips$TRIPMODE[trips$TRIPMODE==7] <- 4
trips$TRIPMODE[trips$TRIPMODE==8] <- 5
trips$TRIPMODE[trips$TRIPMODE>=9 & trips$TRIPMODE<=12] <- 6
trips$TRIPMODE[trips$TRIPMODE>=13 & trips$TRIPMODE<=16] <- 7
trips$TRIPMODE[trips$TRIPMODE>=17 & trips$TRIPMODE<=20] <- 8
trips$TRIPMODE[trips$TRIPMODE>=21] <- trips$TRIPMODE[trips$TRIPMODE>=21] - 12

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

trips$TOUROTAZ <- tours$OTAZ[match(trips$HH_ID*10000+trips$PER_ID*1000+trips$TOUR_ID*10+trips$DAYNO, 
										tours$HH_ID*10000+tours$PER_ID*1000+tours$TOUR_ID*10+tours$DAYNO)]
trips$TOURDTAZ <- tours$DTAZ[match(trips$HH_ID*10000+trips$PER_ID*1000+trips$TOUR_ID*10+trips$DAYNO, 
                                   tours$HH_ID*10000+tours$PER_ID*1000+tours$TOUR_ID*10+tours$DAYNO)]	

# changes made by BMP on 05/09/17
# change the purpose on school escorting stops on ride sharing tours
trips$DEST_PURP[(trips$OUT_ESCORTING_TYPE==1 & trips$OUT_ESCORTEE_TOUR_PURP==3 & trips$IS_INBOUND==0 & trips$DEST_PURP==4)] <- 13
trips$DEST_PURP[(trips$INB_ESCORTING_TYPE==1 & trips$INB_ESCORTEE_TOUR_PURP==3 & trips$IS_INBOUND==1 & trips$DEST_PURP==4)] <- 13

# change the purpose on non-school escorting stops on escort leg of pure school escorting tours
trips$DEST_PURP[(trips$OUT_ESCORTING_TYPE==1 & trips$OUT_ESCORTEE_TOUR_PURP==3 & trips$IS_INBOUND==0 & trips$DEST_PURP!=4 & trips$DEST_IS_TOUR_DEST==0)] <- 14
trips$DEST_PURP[(trips$INB_ESCORTING_TYPE==1 & trips$INB_ESCORTEE_TOUR_PURP==3 & trips$IS_INBOUND==1 & trips$DEST_PURP!=4 & trips$DEST_IS_TOUR_ORIG==0)] <- 14

# copy tour dep hour and minute
trips$ANCHOR_DEPART_HOUR <- tours$ANCHOR_DEPART_HOUR[match(trips$HH_ID*10000+trips$PER_ID*1000+trips$TOUR_ID*10+trips$DAYNO, 
                                                           tours$HH_ID*10000+tours$PER_ID*1000+tours$TOUR_ID*10+tours$DAYNO)]
trips$ANCHOR_DEPART_MIN <- tours$ANCHOR_DEPART_MIN[match(trips$HH_ID*10000+trips$PER_ID*1000+trips$TOUR_ID*10+trips$DAYNO, 
                                                         tours$HH_ID*10000+tours$PER_ID*1000+tours$TOUR_ID*10+tours$DAYNO)]


# end of changes made by BMP on 05/09/17

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
jtours <- unique(jtrips[,c("DAYNO", "HH_ID", "JTOUR_ID")])
jtours$uid <- paste(jtours$DAYNO, jtours$HH_ID, jtours$JTOUR_ID, sep = "-")

unique_joint_tours$uid <- paste(unique_joint_tours$DAYNO, unique_joint_tours$HH_ID, unique_joint_tours$JTOUR_ID, sep = "-")
unique_joint_tours <- unique_joint_tours[unique_joint_tours$uid %in% jtours$uid,]
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

#---------------------------------------------------------------------------------														  


workCounts <- count(tours, c("DAYNO", "HH_ID", "PER_ID"), "TOURPURP == 1 & IS_SUBTOUR == 0") #[excluding at work subtours]
atWorkCounts <- count(tours, c("DAYNO", "HH_ID", "PER_ID"), "TOURPURP == 10 & FULLY_JOINT==0 & IS_SUBTOUR == 1")
schlCounts <- count(tours, c("DAYNO", "HH_ID", "PER_ID"), "TOURPURP == 2 | TOURPURP == 3")
inmCounts <- count(tours, c("DAYNO", "HH_ID", "PER_ID"), "TOURPURP>=4 & TOURPURP<=9 & FULLY_JOINT==0 & IS_SUBTOUR == 0")
tourCounts <- count(tours, c("DAYNO", "HH_ID", "PER_ID"), "TOURPURP <= 10 & IS_SUBTOUR == 0 & FULLY_JOINT==0")  #number of individual tours per person [excluding at work subtours]
joint5 <- count(unique_joint_tours, c("DAYNO", "HH_ID"), "JOINT_PURP==5")
joint6 <- count(unique_joint_tours, c("DAYNO", "HH_ID"), "JOINT_PURP==6")
joint7 <- count(unique_joint_tours, c("DAYNO", "HH_ID"), "JOINT_PURP==7")
joint8 <- count(unique_joint_tours, c("DAYNO", "HH_ID"), "JOINT_PURP==8")
joint9 <- count(unique_joint_tours, c("DAYNO", "HH_ID"), "JOINT_PURP==9")

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
perday$atWorkTours <- atWorkCounts$freq[match(perday$SAMPN*1000+perday$PERNO*10+perday$DAYNO, atWorkCounts$HH_ID*1000+atWorkCounts$PER_ID*10+workCounts$DAYNO)]
perday$schlTours <- schlCounts$freq[match(perday$SAMPN*1000+perday$PERNO*10+perday$DAYNO, schlCounts$HH_ID*1000+schlCounts$PER_ID*10+workCounts$DAYNO)]
perday$inmTours <- inmCounts$freq[match(perday$SAMPN*1000+perday$PERNO*10+perday$DAYNO, inmCounts$HH_ID*1000+inmCounts$PER_ID*10+workCounts$DAYNO)]
perday$inmTours[is.na(perday$inmTours)] <- 0
perday$numTours <- tourCounts$freq[match(perday$SAMPN*1000+perday$PERNO*10+perday$DAYNO, tourCounts$HH_ID*1000+tourCounts$PER_ID*10+workCounts$DAYNO)]
perday$numTours[is.na(perday$numTours)] <- 0

perday$workTours[is.na(perday$workTours)] <- 0
perday$schlTours[is.na(perday$schlTours)] <- 0
perday$atWorkTours[is.na(perday$atWorkTours)] <- 0

# Individual tours by person type
perday$numTours[is.na(perday$numTours)] <- 0
toursPertypeDistbn <- count(tours[!is.na(tours$TOURPURP) & tours$TOURPURP<=10 & tours$FULLY_JOINT==0 & tours$IS_SUBTOUR==0,], c("PERTYPE"), "finalweight")
write.csv(toursPertypeDistbn, "toursPertypeDistbn.csv", row.names = TRUE)

# Total tours by person type for visualizer
totaltoursPertypeDistbn <- count(tours[!is.na(tours$TOURPURP) & tours$TOURPURP<=10 & tours$IS_SUBTOUR==0,], c("PERTYPE"), "finalweight")
write.csv(totaltoursPertypeDistbn, "total_tours_by_pertype_vis.csv", row.names = F)


# Total indi NM tours by person type and purpose
tours_pertype_purpose <- count(tours[tours$TOURPURP>=4 & tours$TOURPURP<=9 & tours$FULLY_JOINT==0,], c("PERTYPE", "TOURPURP"), "finalweight")
write.csv(tours_pertype_purpose, "tours_pertype_purpose.csv", row.names = TRUE)

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

#t1 <- count(tours[!is.na(tours$TOURPURP) & !is.na(tours$JOINT_STATUS) & tours$TOURPURP<=10 & tours$JOINT_STATUS==1 & tours$IS_SUBTOUR==0, ], c("TOURPURP"), "finalweight")
#t2 <- count(tours[!is.na(tours$TOURPURP) & !is.na(tours$JOINT_STATUS) & tours$TOURPURP<=10 & tours$JOINT_STATUS==2 & tours$IS_SUBTOUR==0, ], c("TOURPURP"), "finalweight")
#t3 <- count(unique_joint_tours, c("JOINT_PURP"), "finalweight")
#t3 <- count(tours[!is.na(tours$TOURPURP) & !is.na(tours$JOINT_STATUS) & tours$TOURPURP<=10 & tours$JOINT_STATUS==3 & tours$IS_SUBTOUR==0, ], c("TOURPURP"), "finalweight")
#tours_purpose_type <- cbind(t1, t2$freq, t3$freq[1:9])
#write.csv(tours_purpose_type, "tours_purpose_type.csv", row.names = TRUE)

t1 <- wtd.hist(tours$TOURPURP[!is.na(tours$TOURPURP) & tours$TOURPURP<=10 & tours$FULLY_JOINT==0 & tours$IS_SUBTOUR==0], breaks = seq(1,10, by=1), freq = NULL, right=FALSE, weight=tours$finalweight[!is.na(tours$TOURPURP) & tours$TOURPURP<=10 & tours$FULLY_JOINT==0 & tours$IS_SUBTOUR==0])
t3 <- wtd.hist(unique_joint_tours$JOINT_PURP[unique_joint_tours$JOINT_PURP<10], breaks = seq(1,10, by=1), freq = NULL, right=FALSE, weight=unique_joint_tours$finalweight[unique_joint_tours$JOINT_PURP<10])
tours_purpose_type <- data.frame(t1$counts, t3$counts)
colnames(tours_purpose_type) <- c("indi", "joint")
write.csv(tours_purpose_type, "tours_purpose_type.csv", row.names = TRUE)


# DAP by pertype
perday$DAP <- "H"
perday$DAP[perday$workTours > 0 | perday$schlTours > 0] <- "M"
perday$DAP[perday$numTours > 0 & perday$DAP == "H"] <- "N"
dapSummary <- count(perday, c("PERTYPE", "DAP"), "finalweight")
write.csv(dapSummary, "dapSummary.csv", row.names = TRUE)

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
hhsizeJoint <- count(hhday[hhday$HHSIZE>=2,], c("HHSIZE", "JOINT"), "finalweight")
write.csv(hhsizeJoint, "hhsizeJoint.csv", row.names = TRUE)

#mandatory tour frequency
perday$mtf <- 0
perday$mtf[perday$workTours == 1] <- 1
perday$mtf[perday$workTours >= 2] <- 2
perday$mtf[perday$schlTours == 1] <- 3
perday$mtf[perday$schlTours >= 2] <- 4
perday$mtf[perday$workTours >= 1 & perday$schlTours >= 1] <- 5

mtfSummary <- count(perday[perday$mtf > 0,], c("PERTYPE", "mtf"), "finalweight")
write.csv(mtfSummary, "mtfSummary.csv")
write.csv(tours, "tours_test.csv")

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
inm0Summary <- count(perday[perday$inmTours==0,], c("PERTYPE"), "finalweight")
inm1Summary <- count(perday[perday$inmTours==1,], c("PERTYPE"), "finalweight")
inm2Summary <- count(perday[perday$inmTours==2,], c("PERTYPE"), "finalweight")
inm3Summary <- count(perday[perday$inmTours>=3,], c("PERTYPE"), "finalweight")

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
jtfSummary <- count(hhday[!is.na(hhday$jtf),], c("jtf"), "finalweight")
jointComp <- count(unique_joint_tours[unique_joint_tours$JOINT_PURP>=5 & unique_joint_tours$JOINT_PURP<=9,], c("COMPOSITION"), "finalweight")
jointPartySize <- count(unique_joint_tours[unique_joint_tours$JOINT_PURP>=5 & unique_joint_tours$JOINT_PURP<=9,], c("NUMBER_HH"), "finalweight")
jointCompPartySize <- count(unique_joint_tours[unique_joint_tours$JOINT_PURP>=5 & unique_joint_tours$JOINT_PURP<=9,], c("COMPOSITION","NUMBER_HH"), "finalweight")

hhday$jointCat[hhday$jtours==0] <- 0
hhday$jointCat[hhday$jtours==1] <- 1
hhday$jointCat[hhday$jtours>=2] <- 2

jointToursHHSize <- count(hhday[!is.na(hhday$HHSIZE) & !is.na(hhday$jointCat),], c("HHSIZE", "jointCat"), "finalweight")

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
todj56 <- wtd.hist(unique_joint_tours$ANCHOR_DEPART_BIN[unique_joint_tours$JOINT_PURP>=5 & unique_joint_tours$JOINT_PURP<=6], breaks = seq(1,41, by=1), freq = NULL, right=FALSE, weight = unique_joint_tours$finalweight[unique_joint_tours$JOINT_PURP>=5 & unique_joint_tours$JOINT_PURP<=6])
todj789 <- wtd.hist(unique_joint_tours$ANCHOR_DEPART_BIN[unique_joint_tours$JOINT_PURP>=7 & unique_joint_tours$JOINT_PURP<=9], breaks = seq(1,41, by=1), freq = NULL, right=FALSE, weight = unique_joint_tours$finalweight[unique_joint_tours$JOINT_PURP>=7 & unique_joint_tours$JOINT_PURP<=9])
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
arrj56 <- wtd.hist(unique_joint_tours$ANCHOR_ARRIVE_BIN[unique_joint_tours$JOINT_PURP>=5 & unique_joint_tours$JOINT_PURP<=6], breaks = seq(1,41, by=1), freq = NULL, right=FALSE, weight = unique_joint_tours$finalweight[unique_joint_tours$JOINT_PURP>=5 & unique_joint_tours$JOINT_PURP<=6])
arrj789 <- wtd.hist(unique_joint_tours$ANCHOR_ARRIVE_BIN[unique_joint_tours$JOINT_PURP>=7 & unique_joint_tours$JOINT_PURP<=9], breaks = seq(1,41, by=1), freq = NULL, right=FALSE, weight = unique_joint_tours$finalweight[unique_joint_tours$JOINT_PURP>=7 & unique_joint_tours$JOINT_PURP<=9])
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

# filter out records with missing or negative tour duration
xtours <- tours[!is.na(tours$TOUR_DUR_BIN) & tours$TOUR_DUR_BIN>0 & tours$TOUR_DUR_BIN!="NaN",]
xunique_joint_tours <- unique_joint_tours[!is.na(unique_joint_tours$TOUR_DUR_BIN) & unique_joint_tours$TOUR_DUR_BIN>0 & unique_joint_tours$TOUR_DUR_BIN!="NaN",]

dur1 <- wtd.hist(xtours$TOUR_DUR_BIN[xtours$TOURPURP==1 & xtours$FULLY_JOINT==0], breaks = seq(1,41, by=1), freq = NULL, right=FALSE, weight = xtours$finalweight[xtours$TOURPURP==1 & xtours$FULLY_JOINT==0])
dur2 <- wtd.hist(xtours$TOUR_DUR_BIN[xtours$TOURPURP==2 & xtours$FULLY_JOINT==0], breaks = seq(1,41, by=1), freq = NULL, right=FALSE, weight = xtours$finalweight[xtours$TOURPURP==2 & xtours$FULLY_JOINT==0])
dur3 <- wtd.hist(xtours$TOUR_DUR_BIN[xtours$TOURPURP==3 & xtours$FULLY_JOINT==0], breaks = seq(1,41, by=1), freq = NULL, right=FALSE, weight = xtours$finalweight[xtours$TOURPURP==3 & xtours$FULLY_JOINT==0])
dur4 <- wtd.hist(xtours$TOUR_DUR_BIN[xtours$TOURPURP==4 & xtours$FULLY_JOINT==0], breaks = seq(1,41, by=1), freq = NULL, right=FALSE, weight = xtours$finalweight[xtours$TOURPURP==4 & xtours$FULLY_JOINT==0])
duri56 <- wtd.hist(xtours$TOUR_DUR_BIN[xtours$TOURPURP>=5 & xtours$TOURPURP<=6 & xtours$FULLY_JOINT==0], breaks = seq(1,41, by=1), freq = NULL, right=FALSE, weight = xtours$finalweight[xtours$TOURPURP>=5 & xtours$TOURPURP<=6 & xtours$FULLY_JOINT==0])
duri789 <- wtd.hist(xtours$TOUR_DUR_BIN[xtours$TOURPURP>=7 & xtours$TOURPURP<=9 & xtours$FULLY_JOINT==0], breaks = seq(1,41, by=1), freq = NULL, right=FALSE, weight = xtours$finalweight[xtours$TOURPURP>=7 & xtours$TOURPURP<=9 & xtours$FULLY_JOINT==0])
durj56 <- wtd.hist(xunique_joint_tours$TOUR_DUR_BIN[xunique_joint_tours$JOINT_PURP>=5 & xunique_joint_tours$JOINT_PURP<=6], breaks = seq(1,41, by=1), freq = NULL, right=FALSE, weight = xunique_joint_tours$finalweight[xunique_joint_tours$JOINT_PURP>=5 & xunique_joint_tours$JOINT_PURP<=6])
durj789 <- wtd.hist(xunique_joint_tours$TOUR_DUR_BIN[xunique_joint_tours$JOINT_PURP>=7 & xunique_joint_tours$JOINT_PURP<=9], breaks = seq(1,41, by=1), freq = NULL, right=FALSE, weight = xunique_joint_tours$finalweight[xunique_joint_tours$JOINT_PURP>=7 & xunique_joint_tours$JOINT_PURP<=9])
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
# Tour Mode X Auto Suff
tmode1_as0 <- wtd.hist(tours$TOURMODE[!is.na(tours$TOURMODE) & tours$TOURPURP==1 & tours$FULLY_JOINT==0 & tours$AUTOSUFF==0], breaks = seq(1,12, by=1), freq = NULL, right=FALSE, weight = tours$finalweight[!is.na(tours$TOURMODE) & tours$TOURPURP==1 & tours$FULLY_JOINT==0 & tours$AUTOSUFF==0])
tmode2_as0 <- wtd.hist(tours$TOURMODE[!is.na(tours$TOURMODE) & tours$TOURPURP==2 & tours$FULLY_JOINT==0 & tours$AUTOSUFF==0], breaks = seq(1,12, by=1), freq = NULL, right=FALSE, weight = tours$finalweight[!is.na(tours$TOURMODE) & tours$TOURPURP==2 & tours$FULLY_JOINT==0 & tours$AUTOSUFF==0])
tmode3_as0 <- wtd.hist(tours$TOURMODE[!is.na(tours$TOURMODE) & tours$TOURPURP==3 & tours$FULLY_JOINT==0 & tours$AUTOSUFF==0], breaks = seq(1,12, by=1), freq = NULL, right=FALSE, weight = tours$finalweight[!is.na(tours$TOURMODE) & tours$TOURPURP==3 & tours$FULLY_JOINT==0 & tours$AUTOSUFF==0])
tmode4_as0 <- wtd.hist(tours$TOURMODE[!is.na(tours$TOURMODE) & tours$TOURPURP>=5 & tours$TOURPURP<=6 & tours$FULLY_JOINT==0 & tours$AUTOSUFF==0], breaks = seq(1,12, by=1), freq = NULL, right=FALSE, weight = tours$finalweight[!is.na(tours$TOURMODE) & tours$TOURPURP>=5 & tours$TOURPURP<=6 & tours$FULLY_JOINT==0 & tours$AUTOSUFF==0])
tmode5_as0 <- wtd.hist(tours$TOURMODE[!is.na(tours$TOURMODE) & tours$TOURPURP>=7 & tours$TOURPURP<=9 & tours$FULLY_JOINT==0 & tours$AUTOSUFF==0], breaks = seq(1,12, by=1), freq = NULL, right=FALSE, weight = tours$finalweight[!is.na(tours$TOURMODE) & tours$TOURPURP>=7 & tours$TOURPURP<=9 & tours$FULLY_JOINT==0 & tours$AUTOSUFF==0])
tmode6_as0 <- wtd.hist(unique_joint_tours$TOURMODE[unique_joint_tours$JOINT_PURP>=5 & unique_joint_tours$JOINT_PURP<=6 & unique_joint_tours$AUTOSUFF==0], breaks = seq(1,12, by=1), freq = NULL, right=FALSE, weight = unique_joint_tours$finalweight[unique_joint_tours$JOINT_PURP>=5 & unique_joint_tours$JOINT_PURP<=6 & unique_joint_tours$AUTOSUFF==0])
tmode7_as0 <- wtd.hist(unique_joint_tours$TOURMODE[unique_joint_tours$JOINT_PURP>=7 & unique_joint_tours$JOINT_PURP<=9 & unique_joint_tours$AUTOSUFF==0], breaks = seq(1,12, by=1), freq = NULL, right=FALSE, weight = unique_joint_tours$finalweight[unique_joint_tours$JOINT_PURP>=7 & unique_joint_tours$JOINT_PURP<=9 & unique_joint_tours$AUTOSUFF==0])
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
tmode4_as1 <- wtd.hist(tours$TOURMODE[!is.na(tours$TOURMODE) & tours$TOURPURP>=5 & tours$TOURPURP<=6 & tours$FULLY_JOINT==0 & tours$AUTOSUFF==1], breaks = seq(1,12, by=1), freq = NULL, right=FALSE, weight = tours$finalweight[!is.na(tours$TOURMODE) & tours$TOURPURP>=5 & tours$TOURPURP<=6 & tours$FULLY_JOINT==0 & tours$AUTOSUFF==1])
tmode5_as1 <- wtd.hist(tours$TOURMODE[!is.na(tours$TOURMODE) & tours$TOURPURP>=7 & tours$TOURPURP<=9 & tours$FULLY_JOINT==0 & tours$AUTOSUFF==1], breaks = seq(1,12, by=1), freq = NULL, right=FALSE, weight = tours$finalweight[!is.na(tours$TOURMODE) & tours$TOURPURP>=7 & tours$TOURPURP<=9 & tours$FULLY_JOINT==0 & tours$AUTOSUFF==1])
tmode6_as1 <- wtd.hist(unique_joint_tours$TOURMODE[unique_joint_tours$JOINT_PURP>=5 & unique_joint_tours$JOINT_PURP<=6 & unique_joint_tours$AUTOSUFF==1], breaks = seq(1,12, by=1), freq = NULL, right=FALSE, weight = unique_joint_tours$finalweight[unique_joint_tours$JOINT_PURP>=5 & unique_joint_tours$JOINT_PURP<=6 & unique_joint_tours$AUTOSUFF==1])
tmode7_as1 <- wtd.hist(unique_joint_tours$TOURMODE[unique_joint_tours$JOINT_PURP>=7 & unique_joint_tours$JOINT_PURP<=9 & unique_joint_tours$AUTOSUFF==1], breaks = seq(1,12, by=1), freq = NULL, right=FALSE, weight = unique_joint_tours$finalweight[unique_joint_tours$JOINT_PURP>=7 & unique_joint_tours$JOINT_PURP<=9 & unique_joint_tours$AUTOSUFF==1])
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
tmode4_as2 <- wtd.hist(tours$TOURMODE[!is.na(tours$TOURMODE) & tours$TOURPURP>=5 & tours$TOURPURP<=6 & tours$FULLY_JOINT==0 & tours$AUTOSUFF==2], breaks = seq(1,12, by=1), freq = NULL, right=FALSE, weight = tours$finalweight[!is.na(tours$TOURMODE) & tours$TOURPURP>=5 & tours$TOURPURP<=6 & tours$FULLY_JOINT==0 & tours$AUTOSUFF==2])
tmode5_as2 <- wtd.hist(tours$TOURMODE[!is.na(tours$TOURMODE) & tours$TOURPURP>=7 & tours$TOURPURP<=9 & tours$FULLY_JOINT==0 & tours$AUTOSUFF==2], breaks = seq(1,12, by=1), freq = NULL, right=FALSE, weight = tours$finalweight[!is.na(tours$TOURMODE) & tours$TOURPURP>=7 & tours$TOURPURP<=9 & tours$FULLY_JOINT==0 & tours$AUTOSUFF==2])
tmode6_as2 <- wtd.hist(unique_joint_tours$TOURMODE[unique_joint_tours$JOINT_PURP>=5 & unique_joint_tours$JOINT_PURP<=6 & unique_joint_tours$AUTOSUFF==2], breaks = seq(1,12, by=1), freq = NULL, right=FALSE, weight = unique_joint_tours$finalweight[unique_joint_tours$JOINT_PURP>=5 & unique_joint_tours$JOINT_PURP<=6 & unique_joint_tours$AUTOSUFF==2])
tmode7_as2 <- wtd.hist(unique_joint_tours$TOURMODE[unique_joint_tours$JOINT_PURP>=7 & unique_joint_tours$JOINT_PURP<=9 & unique_joint_tours$AUTOSUFF==2], breaks = seq(1,12, by=1), freq = NULL, right=FALSE, weight = unique_joint_tours$finalweight[unique_joint_tours$JOINT_PURP>=7 & unique_joint_tours$JOINT_PURP<=9 & unique_joint_tours$AUTOSUFF==2])
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

# Non-mandatory tour distance profile
tourdist4 <- wtd.hist(tours$SKIMDIST[tours$TOURPURP==4 & tours$FULLY_JOINT==0], breaks = c(seq(0,40, by=1), 9999), freq = NULL, right=FALSE, weight = tours$finalweight[tours$TOURPURP==4 & tours$FULLY_JOINT==0])
tourdisti56 <- wtd.hist(tours$SKIMDIST[tours$TOURPURP>=5 & tours$TOURPURP<=6 & tours$FULLY_JOINT==0], breaks = c(seq(0,40, by=1), 9999), freq = NULL, right=FALSE, weight = tours$finalweight[tours$TOURPURP>=5 & tours$TOURPURP<=6 & tours$FULLY_JOINT==0])
tourdisti789 <- wtd.hist(tours$SKIMDIST[tours$TOURPURP>=7 & tours$TOURPURP<=9 & tours$FULLY_JOINT==0], breaks = c(seq(0,40, by=1), 9999), freq = NULL, right=FALSE, weight = tours$finalweight[tours$TOURPURP>=7 & tours$TOURPURP<=9 & tours$FULLY_JOINT==0])
tourdistj56 <- wtd.hist(unique_joint_tours$SKIMDIST[unique_joint_tours$JOINT_PURP>=5 & unique_joint_tours$JOINT_PURP<=6], breaks = c(seq(0,40, by=1), 9999), freq = NULL, right=FALSE, weight = unique_joint_tours$finalweight[unique_joint_tours$JOINT_PURP>=5 & unique_joint_tours$JOINT_PURP<=6])
tourdistj789 <- wtd.hist(unique_joint_tours$SKIMDIST[unique_joint_tours$JOINT_PURP>=7 & unique_joint_tours$JOINT_PURP<=9], breaks = c(seq(0,40, by=1), 9999), freq = NULL, right=FALSE, weight = unique_joint_tours$finalweight[unique_joint_tours$JOINT_PURP>=7 & unique_joint_tours$JOINT_PURP<=9])
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
cat("\n Average Tour Distance [jmain]: ", weighted.mean(unique_joint_tours$SKIMDIST[unique_joint_tours$JOINT_PURP>=5 & unique_joint_tours$JOINT_PURP<=6], unique_joint_tours$finalweight[unique_joint_tours$JOINT_PURP>=5 & unique_joint_tours$JOINT_PURP<=6], na.rm = TRUE))
cat("\n Average Tour Distance [jdisc]: ", weighted.mean(unique_joint_tours$SKIMDIST[unique_joint_tours$JOINT_PURP>=7 & unique_joint_tours$JOINT_PURP<=9], unique_joint_tours$finalweight[unique_joint_tours$JOINT_PURP>=7 & unique_joint_tours$JOINT_PURP<=9], na.rm = TRUE))
cat("\n Average Tour Distance [atwork]: ", weighted.mean(tours$SKIMDIST[tours$IS_SUBTOUR == 1], tours$finalweight[tours$IS_SUBTOUR == 1], na.rm = TRUE))

## Output average trips lengths for visualizer

avgTripLengths <- c(weighted.mean(tours$SKIMDIST[tours$TOURPURP==4 & tours$FULLY_JOINT==0], tours$finalweight[tours$TOURPURP==4 & tours$FULLY_JOINT==0], na.rm = TRUE),
                    weighted.mean(tours$SKIMDIST[tours$TOURPURP>=5 & tours$TOURPURP<=6 & tours$FULLY_JOINT==0], tours$finalweight[tours$TOURPURP>=5 & tours$TOURPURP<=6 & tours$FULLY_JOINT==0], na.rm = TRUE),
                    weighted.mean(tours$SKIMDIST[tours$TOURPURP>=7 & tours$TOURPURP<=9 & tours$FULLY_JOINT==0], tours$finalweight[tours$TOURPURP>=7 & tours$TOURPURP<=9 & tours$FULLY_JOINT==0], na.rm = TRUE),
                    weighted.mean(unique_joint_tours$SKIMDIST[unique_joint_tours$JOINT_PURP>=5 & unique_joint_tours$JOINT_PURP<=6], unique_joint_tours$finalweight[unique_joint_tours$JOINT_PURP>=5 & unique_joint_tours$JOINT_PURP<=6], na.rm = TRUE),
                    weighted.mean(unique_joint_tours$SKIMDIST[unique_joint_tours$JOINT_PURP>=7 & unique_joint_tours$JOINT_PURP<=9], unique_joint_tours$finalweight[unique_joint_tours$JOINT_PURP>=7 & unique_joint_tours$JOINT_PURP<=9], na.rm = TRUE),
                    weighted.mean(tours$SKIMDIST[tours$IS_SUBTOUR == 1], tours$finalweight[tours$IS_SUBTOUR == 1], na.rm = TRUE),
                    weighted.mean(tours$SKIMDIST, tours$finalweight, na.rm = TRUE))

nonMandTourPurpose <- c("esco", "imain", "idisc", "jmain", "jdisc", "atwork", "Total")

nonMandTripLengths <- data.frame(purpose = nonMandTourPurpose, avgTripLength = avgTripLengths)

write.csv(nonMandTripLengths, "nonMandTripLengths.csv", row.names = F)



# STop Frequency
#Outbound
stopfreq1 <- wtd.hist(tours$OUTBOUND_STOPS[tours$TOURPURP==1 & tours$FULLY_JOINT==0], breaks = c(seq(0,3, by=1), 9999), freq = NULL, right=FALSE, weight = tours$finalweight[tours$TOURPURP==1 & tours$FULLY_JOINT==0])
stopfreq2 <- wtd.hist(tours$OUTBOUND_STOPS[tours$TOURPURP==2 & tours$FULLY_JOINT==0], breaks = c(seq(0,3, by=1), 9999), freq = NULL, right=FALSE, weight = tours$finalweight[tours$TOURPURP==2 & tours$FULLY_JOINT==0])
stopfreq3 <- wtd.hist(tours$OUTBOUND_STOPS[tours$TOURPURP==3 & tours$FULLY_JOINT==0], breaks = c(seq(0,3, by=1), 9999), freq = NULL, right=FALSE, weight = tours$finalweight[tours$TOURPURP==3 & tours$FULLY_JOINT==0])
stopfreq4 <- wtd.hist(tours$OUTBOUND_STOPS[tours$TOURPURP==4 & tours$FULLY_JOINT==0], breaks = c(seq(0,3, by=1), 9999), freq = NULL, right=FALSE, weight = tours$finalweight[tours$TOURPURP==4 & tours$FULLY_JOINT==0])
stopfreqi56 <- wtd.hist(tours$OUTBOUND_STOPS[tours$TOURPURP>=5 & tours$TOURPURP<=6 & tours$FULLY_JOINT==0], breaks = c(seq(0,3, by=1), 9999), freq = NULL, right=FALSE, weight = tours$finalweight[tours$TOURPURP>=5 & tours$TOURPURP<=6 & tours$FULLY_JOINT==0])
stopfreqi789 <- wtd.hist(tours$OUTBOUND_STOPS[tours$TOURPURP>=7 & tours$TOURPURP<=9 & tours$FULLY_JOINT==0], breaks = c(seq(0,3, by=1), 9999), freq = NULL, right=FALSE, weight = tours$finalweight[tours$TOURPURP>=7 & tours$TOURPURP<=9 & tours$FULLY_JOINT==0])
stopfreqj56 <- wtd.hist(unique_joint_tours$OUTBOUND_STOPS[unique_joint_tours$JOINT_PURP>=5 & unique_joint_tours$JOINT_PURP<=6], breaks = c(seq(0,3, by=1), 9999), freq = NULL, right=FALSE, weight = unique_joint_tours$finalweight[unique_joint_tours$JOINT_PURP>=5 & unique_joint_tours$JOINT_PURP<=6])
stopfreqj789 <- wtd.hist(unique_joint_tours$OUTBOUND_STOPS[unique_joint_tours$JOINT_PURP>=7 & unique_joint_tours$JOINT_PURP<=9], breaks = c(seq(0,3, by=1), 9999), freq = NULL, right=FALSE, weight = unique_joint_tours$finalweight[unique_joint_tours$JOINT_PURP>=7 & unique_joint_tours$JOINT_PURP<=9])
stopfreq10 <- wtd.hist(tours$OUTBOUND_STOPS[tours$IS_SUBTOUR == 1], breaks = c(seq(0,3, by=1), 9999), freq = NULL, right=FALSE, weight = tours$finalweight[tours$IS_SUBTOUR == 1])

stopFreq <- data.frame(stopfreq1$counts, stopfreq2$counts, stopfreq3$counts, stopfreq4$counts, stopfreqi56$counts
                          , stopfreqi789$counts, stopfreqj56$counts, stopfreqj789$counts, stopfreq10$counts)
colnames(stopFreq) <- c("work", "univ", "sch", "esco","imain", "idisc", "jmain", "jdisc", "atwork")
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
stopfreq3 <- wtd.hist(tours$INBOUND_STOPS[tours$TOURPURP==3 & tours$FULLY_JOINT==0], breaks = c(seq(0,3, by=1), 9999), freq = NULL, right=FALSE, weight = tours$finalweight[tours$TOURPURP==3 & tours$FULLY_JOINT==0])
stopfreq4 <- wtd.hist(tours$INBOUND_STOPS[tours$TOURPURP==4 & tours$FULLY_JOINT==0], breaks = c(seq(0,3, by=1), 9999), freq = NULL, right=FALSE, weight = tours$finalweight[tours$TOURPURP==4 & tours$FULLY_JOINT==0])
stopfreqi56 <- wtd.hist(tours$INBOUND_STOPS[tours$TOURPURP>=5 & tours$TOURPURP<=6 & tours$FULLY_JOINT==0], breaks = c(seq(0,3, by=1), 9999), freq = NULL, right=FALSE, weight = tours$finalweight[tours$TOURPURP>=5 & tours$TOURPURP<=6 & tours$FULLY_JOINT==0])
stopfreqi789 <- wtd.hist(tours$INBOUND_STOPS[tours$TOURPURP>=7 & tours$TOURPURP<=9 & tours$FULLY_JOINT==0], breaks = c(seq(0,3, by=1), 9999), freq = NULL, right=FALSE, weight = tours$finalweight[tours$TOURPURP>=7 & tours$TOURPURP<=9 & tours$FULLY_JOINT==0])
stopfreqj56 <- wtd.hist(unique_joint_tours$INBOUND_STOPS[unique_joint_tours$JOINT_PURP>=5 & unique_joint_tours$JOINT_PURP<=6], breaks = c(seq(0,3, by=1), 9999), freq = NULL, right=FALSE, weight = unique_joint_tours$finalweight[unique_joint_tours$JOINT_PURP>=5 & unique_joint_tours$JOINT_PURP<=6])
stopfreqj789 <- wtd.hist(unique_joint_tours$INBOUND_STOPS[unique_joint_tours$JOINT_PURP>=7 & unique_joint_tours$JOINT_PURP<=9], breaks = c(seq(0,3, by=1), 9999), freq = NULL, right=FALSE, weight = unique_joint_tours$finalweight[unique_joint_tours$JOINT_PURP>=7 & unique_joint_tours$JOINT_PURP<=9])
stopfreq10 <- wtd.hist(tours$INBOUND_STOPS[tours$IS_SUBTOUR == 1], breaks = c(seq(0,3, by=1), 9999), freq = NULL, right=FALSE, weight = tours$finalweight[tours$IS_SUBTOUR == 1])

stopFreq <- data.frame(stopfreq1$counts, stopfreq2$counts, stopfreq3$counts, stopfreq4$counts, stopfreqi56$counts
                       , stopfreqi789$counts, stopfreqj56$counts, stopfreqj789$counts, stopfreq10$counts)
colnames(stopFreq) <- c("work", "univ", "sch", "esco","imain", "idisc", "jmain", "jdisc", "atwork")
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
stopfreq3 <- wtd.hist(tours$TOTAL_STOPS[tours$TOURPURP==3 & tours$FULLY_JOINT==0], breaks = c(seq(0,6, by=1), 9999), freq = NULL, right=FALSE, weight = tours$finalweight[tours$TOURPURP==3 & tours$FULLY_JOINT==0])
stopfreq4 <- wtd.hist(tours$TOTAL_STOPS[tours$TOURPURP==4 & tours$FULLY_JOINT==0], breaks = c(seq(0,6, by=1), 9999), freq = NULL, right=FALSE, weight = tours$finalweight[tours$TOURPURP==4 & tours$FULLY_JOINT==0])
stopfreqi56 <- wtd.hist(tours$TOTAL_STOPS[tours$TOURPURP>=5 & tours$TOURPURP<=6 & tours$FULLY_JOINT==0], breaks = c(seq(0,6, by=1), 9999), freq = NULL, right=FALSE, weight = tours$finalweight[tours$TOURPURP>=5 & tours$TOURPURP<=6 & tours$FULLY_JOINT==0])
stopfreqi789 <- wtd.hist(tours$TOTAL_STOPS[tours$TOURPURP>=7 & tours$TOURPURP<=9 & tours$FULLY_JOINT==0], breaks = c(seq(0,6, by=1), 9999), freq = NULL, right=FALSE, weight = tours$finalweight[tours$TOURPURP>=7 & tours$TOURPURP<=9 & tours$FULLY_JOINT==0])
stopfreqj56 <- wtd.hist(unique_joint_tours$TOTAL_STOPS[unique_joint_tours$JOINT_PURP>=5 & unique_joint_tours$JOINT_PURP<=6], breaks = c(seq(0,6, by=1), 9999), freq = NULL, right=FALSE, weight = unique_joint_tours$finalweight[unique_joint_tours$JOINT_PURP>=5 & unique_joint_tours$JOINT_PURP<=6])
stopfreqj789 <- wtd.hist(unique_joint_tours$TOTAL_STOPS[unique_joint_tours$JOINT_PURP>=7 & unique_joint_tours$JOINT_PURP<=9], breaks = c(seq(0,6, by=1), 9999), freq = NULL, right=FALSE, weight = unique_joint_tours$finalweight[unique_joint_tours$JOINT_PURP>=7 & unique_joint_tours$JOINT_PURP<=9])
stopfreq10 <- wtd.hist(tours$TOTAL_STOPS[tours$IS_SUBTOUR == 1], breaks = c(seq(0,6, by=1), 9999), freq = NULL, right=FALSE, weight = tours$finalweight[tours$IS_SUBTOUR == 1])

stopFreq <- data.frame(stopfreq1$counts, stopfreq2$counts, stopfreq3$counts, stopfreq4$counts, stopfreqi56$counts
                       , stopfreqi789$counts, stopfreqj56$counts, stopfreqj789$counts, stopfreq10$counts)
colnames(stopFreq) <- c("work", "univ", "sch", "esco","imain", "idisc", "jmain", "jdisc", "atwork")
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
stopfreq3 <- wtd.hist(stops$DEST_PURP[stops$TOURPURP==3 & stops$FULLY_JOINT==0], breaks = c(seq(1,10, by=1), 9999), freq = NULL, right=FALSE, weight = stops$finalweight[stops$TOURPURP==3 & stops$FULLY_JOINT==0])
stopfreq4 <- wtd.hist(stops$DEST_PURP[stops$TOURPURP==4 & stops$FULLY_JOINT==0], breaks = c(seq(1,10, by=1), 9999), freq = NULL, right=FALSE, weight = stops$finalweight[stops$TOURPURP==4 & stops$FULLY_JOINT==0])
stopfreqi56 <- wtd.hist(stops$DEST_PURP[stops$TOURPURP>=5 & stops$TOURPURP<=6 & stops$FULLY_JOINT==0], breaks = c(seq(1,10, by=1), 9999), freq = NULL, right=FALSE, weight = stops$finalweight[stops$TOURPURP>=5 & stops$TOURPURP<=6 & stops$FULLY_JOINT==0])
stopfreqi789 <- wtd.hist(stops$DEST_PURP[stops$TOURPURP>=7 & stops$TOURPURP<=9 & stops$FULLY_JOINT==0], breaks = c(seq(1,10, by=1), 9999), freq = NULL, right=FALSE, weight = stops$finalweight[stops$TOURPURP>=7 & stops$TOURPURP<=9 & stops$FULLY_JOINT==0])
stopfreqj56 <- wtd.hist(jstops$DEST_PURP[jstops$TOURPURP>=5 & jstops$TOURPURP<=6], breaks = c(seq(1,10, by=1), 9999), freq = NULL, right=FALSE, weight = jstops$finalweight[jstops$TOURPURP>=5 & jstops$TOURPURP<=6])
stopfreqj789 <- wtd.hist(jstops$DEST_PURP[jstops$TOURPURP>=7 & jstops$TOURPURP<=9], breaks = c(seq(1,10, by=1), 9999), freq = NULL, right=FALSE, weight = jstops$finalweight[jstops$TOURPURP>=7 & jstops$TOURPURP<=9])
stopfreq10 <- wtd.hist(stops$DEST_PURP[stops$SUBTOUR==1], breaks = c(seq(1,10, by=1), 9999), freq = NULL, right=FALSE, weight = stops$finalweight[stops$SUBTOUR==1])

stopFreq <- data.frame(stopfreq1$counts, stopfreq2$counts, stopfreq3$counts, stopfreq4$counts, stopfreqi56$counts
                       , stopfreqi789$counts, stopfreqj56$counts, stopfreqj789$counts, stopfreq10$counts)
colnames(stopFreq) <- c("work", "univ", "sch", "esco","imain", "idisc", "jmain", "jdisc", "atwork")
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
stopfreq3 <- wtd.hist(stops$out_dir_dist[stops$TOURPURP==3 & stops$FULLY_JOINT==0], breaks = c(-9999,seq(0,40, by=1), 9999), freq = NULL, right=FALSE, weight = stops$finalweight[stops$TOURPURP==3 & stops$FULLY_JOINT==0])
stopfreq4 <- wtd.hist(stops$out_dir_dist[stops$TOURPURP==4 & stops$FULLY_JOINT==0], breaks = c(-9999,seq(0,40, by=1), 9999), freq = NULL, right=FALSE, weight = stops$finalweight[stops$TOURPURP==4 & stops$FULLY_JOINT==0])
stopfreqi56 <- wtd.hist(stops$out_dir_dist[stops$TOURPURP>=5 & stops$TOURPURP<=6 & stops$FULLY_JOINT==0], breaks = c(-9999,seq(0,40, by=1), 9999), freq = NULL, right=FALSE, weight = stops$finalweight[stops$TOURPURP>=5 & stops$TOURPURP<=6 & stops$FULLY_JOINT==0])
stopfreqi789 <- wtd.hist(stops$out_dir_dist[stops$TOURPURP>=7 & stops$TOURPURP<=9 & stops$FULLY_JOINT==0], breaks = c(-9999,seq(0,40, by=1), 9999), freq = NULL, right=FALSE, weight = stops$finalweight[stops$TOURPURP>=7 & stops$TOURPURP<=9 & stops$FULLY_JOINT==0])
stopfreqj56 <- wtd.hist(jstops$out_dir_dist[jstops$TOURPURP>=5 & jstops$TOURPURP<=6], breaks = c(-9999,seq(0,40, by=1), 9999), freq = NULL, right=FALSE, weight = jstops$finalweight[jstops$TOURPURP>=5 & jstops$TOURPURP<=6])
stopfreqj789 <- wtd.hist(jstops$out_dir_dist[jstops$TOURPURP>=7 & jstops$TOURPURP<=9], breaks = c(-9999,seq(0,40, by=1), 9999), freq = NULL, right=FALSE, weight = jstops$finalweight[jstops$TOURPURP>=7 & jstops$TOURPURP<=9])
stopfreq10 <- wtd.hist(stops$out_dir_dist[stops$SUBTOUR==1], breaks = c(-9999,seq(0,40, by=1), 9999), freq = NULL, right=FALSE, weight = stops$finalweight[stops$SUBTOUR==1])

stopFreq <- data.frame(stopfreq1$counts, stopfreq2$counts, stopfreq3$counts, stopfreq4$counts, stopfreqi56$counts
                       , stopfreqi789$counts, stopfreqj56$counts, stopfreqj789$counts, stopfreq10$counts)
colnames(stopFreq) <- c("work", "univ", "sch", "esco","imain", "idisc", "jmain", "jdisc", "atwork")
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
                  weighted.mean(stops$out_dir_dist[stops$TOURPURP==3 & stops$FULLY_JOINT==0], weight = stops$finalweight[stops$TOURPURP==3 & stops$FULLY_JOINT==0], na.rm = TRUE),
                  weighted.mean(stops$out_dir_dist[stops$TOURPURP==4 & stops$FULLY_JOINT==0], weight = stops$finalweight[stops$TOURPURP==4 & stops$FULLY_JOINT==0], na.rm = TRUE),
                  weighted.mean(stops$out_dir_dist[stops$TOURPURP>=5 & stops$TOURPURP<=6 & stops$FULLY_JOINT==0], weight = stops$finalweight[stops$TOURPURP>=5 & stops$TOURPURP<=6 & stops$FULLY_JOINT==0], na.rm = TRUE),
                  weighted.mean(stops$out_dir_dist[stops$TOURPURP>=7 & stops$TOURPURP<=9 & stops$FULLY_JOINT==0], weight = stops$finalweight[stops$TOURPURP>=7 & stops$TOURPURP<=9 & stops$FULLY_JOINT==0], na.rm = TRUE),
                  weighted.mean(jstops$out_dir_dist[jstops$TOURPURP>=5 & jstops$TOURPURP<=6], weight = jstops$finalweight[jstops$TOURPURP>=5 & jstops$TOURPURP<=6], na.rm = TRUE),
                  weighted.mean(jstops$out_dir_dist[jstops$TOURPURP>=7 & jstops$TOURPURP<=9], weight = jstops$finalweight[jstops$TOURPURP>=7 & jstops$TOURPURP<=9], na.rm = TRUE),
                  weighted.mean(stops$out_dir_dist[stops$SUBTOUR==1], weight = stops$finalweight[stops$SUBTOUR==1], na.rm = TRUE))

purp <- c("work", "univ", "sch", "esco","imain", "idisc", "jmain", "jdisc", "atwork")

avgStopOutofDirectionDist <- data.frame(purpose = purp, avgDist = avgDistances)

write.csv(avgStopOutofDirectionDist, "avgStopOutofDirectionDist_vis.csv", row.names = F)


#Stop Departure Time
stopfreq1 <- wtd.hist(stops$DEST_DEP_BIN[stops$TOURPURP==1 & stops$FULLY_JOINT==0], breaks = c(seq(1,40, by=1), 9999), freq = NULL, right=FALSE, weight = stops$finalweight[stops$TOURPURP==1 & stops$FULLY_JOINT==0])
stopfreq2 <- wtd.hist(stops$DEST_DEP_BIN[stops$TOURPURP==2 & stops$FULLY_JOINT==0], breaks = c(seq(1,40, by=1), 9999), freq = NULL, right=FALSE, weight = stops$finalweight[stops$TOURPURP==2 & stops$FULLY_JOINT==0])
stopfreq3 <- wtd.hist(stops$DEST_DEP_BIN[stops$TOURPURP==3 & stops$FULLY_JOINT==0], breaks = c(seq(1,40, by=1), 9999), freq = NULL, right=FALSE, weight = stops$finalweight[stops$TOURPURP==3 & stops$FULLY_JOINT==0])
stopfreq4 <- wtd.hist(stops$DEST_DEP_BIN[stops$TOURPURP==4 & stops$FULLY_JOINT==0], breaks = c(seq(1,40, by=1), 9999), freq = NULL, right=FALSE, weight = stops$finalweight[stops$TOURPURP==4 & stops$FULLY_JOINT==0])
stopfreqi56 <- wtd.hist(stops$DEST_DEP_BIN[stops$TOURPURP>=5 & stops$TOURPURP<=6 & stops$FULLY_JOINT==0], breaks = c(seq(1,40, by=1), 9999), freq = NULL, right=FALSE, weight = stops$finalweight[stops$TOURPURP>=5 & stops$TOURPURP<=6 & stops$FULLY_JOINT==0])
stopfreqi789 <- wtd.hist(stops$DEST_DEP_BIN[stops$TOURPURP>=7 & stops$TOURPURP<=9 & stops$FULLY_JOINT==0], breaks = c(seq(1,40, by=1), 9999), freq = NULL, right=FALSE, weight = stops$finalweight[stops$TOURPURP>=7 & stops$TOURPURP<=9 & stops$FULLY_JOINT==0])
stopfreqj56 <- wtd.hist(jstops$DEST_DEP_BIN[jstops$TOURPURP>=5 & jstops$TOURPURP<=6], breaks = c(seq(1,40, by=1), 9999), freq = NULL, right=FALSE, weight = jstops$finalweight[jstops$TOURPURP>=5 & jstops$TOURPURP<=6])
stopfreqj789 <- wtd.hist(jstops$DEST_DEP_BIN[jstops$TOURPURP>=7 & jstops$TOURPURP<=9], breaks = c(seq(1,40, by=1), 9999), freq = NULL, right=FALSE, weight = jstops$finalweight[jstops$TOURPURP>=7 & jstops$TOURPURP<=9])
stopfreq10 <- wtd.hist(stops$DEST_DEP_BIN[stops$SUBTOUR==1], breaks = c(seq(1,40, by=1), 9999), freq = NULL, right=FALSE, weight = stops$finalweight[stops$SUBTOUR==1])

stopFreq <- data.frame(stopfreq1$counts, stopfreq2$counts, stopfreq3$counts, stopfreq4$counts, stopfreqi56$counts
                       , stopfreqi789$counts, stopfreqj56$counts, stopfreqj789$counts, stopfreq10$counts)
colnames(stopFreq) <- c("work", "univ", "sch", "esco","imain", "idisc", "jmain", "jdisc", "atwork")
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
stopfreq3 <- wtd.hist(trips$ORIG_DEP_BIN[trips$TOURPURP==3 & trips$FULLY_JOINT==0], breaks = c(seq(1,40, by=1), 9999), freq = NULL, right=FALSE, weight = trips$finalweight[trips$TOURPURP==3 & trips$FULLY_JOINT==0])
stopfreq4 <- wtd.hist(trips$ORIG_DEP_BIN[trips$TOURPURP==4 & trips$FULLY_JOINT==0], breaks = c(seq(1,40, by=1), 9999), freq = NULL, right=FALSE, weight = trips$finalweight[trips$TOURPURP==4 & trips$FULLY_JOINT==0])
stopfreqi56 <- wtd.hist(trips$ORIG_DEP_BIN[trips$TOURPURP>=5 & trips$TOURPURP<=6 & trips$FULLY_JOINT==0], breaks = c(seq(1,40, by=1), 9999), freq = NULL, right=FALSE, weight = trips$finalweight[trips$TOURPURP>=5 & trips$TOURPURP<=6 & trips$FULLY_JOINT==0])
stopfreqi789 <- wtd.hist(trips$ORIG_DEP_BIN[trips$TOURPURP>=7 & trips$TOURPURP<=9 & trips$FULLY_JOINT==0], breaks = c(seq(1,40, by=1), 9999), freq = NULL, right=FALSE, weight = trips$finalweight[trips$TOURPURP>=7 & trips$TOURPURP<=9 & trips$FULLY_JOINT==0])
stopfreqj56 <- wtd.hist(jtrips$ORIG_DEP_BIN[jtrips$TOURPURP>=5 & jtrips$TOURPURP<=6], breaks = c(seq(1,40, by=1), 9999), freq = NULL, right=FALSE, weight = jtrips$finalweight[jtrips$TOURPURP>=5 & jtrips$TOURPURP<=6])
stopfreqj789 <- wtd.hist(jtrips$ORIG_DEP_BIN[jtrips$TOURPURP>=7 & jtrips$TOURPURP<=9], breaks = c(seq(1,40, by=1), 9999), freq = NULL, right=FALSE, weight = jtrips$finalweight[jtrips$TOURPURP>=7 & jtrips$TOURPURP<=9])
stopfreq10 <- wtd.hist(trips$ORIG_DEP_BIN[trips$SUBTOUR==1], breaks = c(seq(1,40, by=1), 9999), freq = NULL, right=FALSE, weight = trips$finalweight[trips$SUBTOUR==1])

stopFreq <- data.frame(stopfreq1$counts, stopfreq2$counts, stopfreq3$counts, stopfreq4$counts, stopfreqi56$counts
                       , stopfreqi789$counts, stopfreqj56$counts, stopfreqj789$counts, stopfreq10$counts)
colnames(stopFreq) <- c("work", "univ", "sch", "esco","imain", "idisc", "jmain", "jdisc", "atwork")
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
#Work
tripmode1 <- wtd.hist(trips$TRIPMODE[!is.na(trips$TRIPMODE) & trips$TRIPMODE>0 & trips$TOURPURP==1 & trips$FULLY_JOINT==0 & trips$TOURMODE==1], breaks = seq(1,12, by=1), freq = NULL, right=FALSE, weight = trips$finalweight[!is.na(trips$TRIPMODE) & trips$TRIPMODE>0  & trips$TOURPURP==1 & trips$FULLY_JOINT==0 & trips$TOURMODE==1])
tripmode2 <- wtd.hist(trips$TRIPMODE[!is.na(trips$TRIPMODE) & trips$TRIPMODE>0 & trips$TOURPURP==1 & trips$FULLY_JOINT==0 & trips$TOURMODE==2], breaks = seq(1,12, by=1), freq = NULL, right=FALSE, weight = trips$finalweight[!is.na(trips$TRIPMODE) & trips$TRIPMODE>0  & trips$TOURPURP==1 & trips$FULLY_JOINT==0 & trips$TOURMODE==2])
tripmode3 <- wtd.hist(trips$TRIPMODE[!is.na(trips$TRIPMODE) & trips$TRIPMODE>0 & trips$TOURPURP==1 & trips$FULLY_JOINT==0 & trips$TOURMODE==3], breaks = seq(1,12, by=1), freq = NULL, right=FALSE, weight = trips$finalweight[!is.na(trips$TRIPMODE) & trips$TRIPMODE>0  & trips$TOURPURP==1 & trips$FULLY_JOINT==0 & trips$TOURMODE==3])
tripmode4 <- wtd.hist(trips$TRIPMODE[!is.na(trips$TRIPMODE) & trips$TRIPMODE>0 & trips$TOURPURP==1 & trips$FULLY_JOINT==0 & trips$TOURMODE==4], breaks = seq(1,12, by=1), freq = NULL, right=FALSE, weight = trips$finalweight[!is.na(trips$TRIPMODE) & trips$TRIPMODE>0  & trips$TOURPURP==1 & trips$FULLY_JOINT==0 & trips$TOURMODE==4])
tripmode5 <- wtd.hist(trips$TRIPMODE[!is.na(trips$TRIPMODE) & trips$TRIPMODE>0 & trips$TOURPURP==1 & trips$FULLY_JOINT==0 & trips$TOURMODE==5], breaks = seq(1,12, by=1), freq = NULL, right=FALSE, weight = trips$finalweight[!is.na(trips$TRIPMODE) & trips$TRIPMODE>0  & trips$TOURPURP==1 & trips$FULLY_JOINT==0 & trips$TOURMODE==5])
tripmode6 <- wtd.hist(trips$TRIPMODE[!is.na(trips$TRIPMODE) & trips$TRIPMODE>0 & trips$TOURPURP==1 & trips$FULLY_JOINT==0 & trips$TOURMODE==6], breaks = seq(1,12, by=1), freq = NULL, right=FALSE, weight = trips$finalweight[!is.na(trips$TRIPMODE) & trips$TRIPMODE>0  & trips$TOURPURP==1 & trips$FULLY_JOINT==0 & trips$TOURMODE==6])
tripmode7 <- wtd.hist(trips$TRIPMODE[!is.na(trips$TRIPMODE) & trips$TRIPMODE>0 & trips$TOURPURP==1 & trips$FULLY_JOINT==0 & trips$TOURMODE==7], breaks = seq(1,12, by=1), freq = NULL, right=FALSE, weight = trips$finalweight[!is.na(trips$TRIPMODE) & trips$TRIPMODE>0  & trips$TOURPURP==1 & trips$FULLY_JOINT==0 & trips$TOURMODE==7])
tripmode8 <- wtd.hist(trips$TRIPMODE[!is.na(trips$TRIPMODE) & trips$TRIPMODE>0 & trips$TOURPURP==1 & trips$FULLY_JOINT==0 & trips$TOURMODE==8], breaks = seq(1,12, by=1), freq = NULL, right=FALSE, weight = trips$finalweight[!is.na(trips$TRIPMODE) & trips$TRIPMODE>0  & trips$TOURPURP==1 & trips$FULLY_JOINT==0 & trips$TOURMODE==8])
tripmode9 <- wtd.hist(trips$TRIPMODE[!is.na(trips$TRIPMODE) & trips$TRIPMODE>0 & trips$TOURPURP==1 & trips$FULLY_JOINT==0 & trips$TOURMODE==9], breaks = seq(1,12, by=1), freq = NULL, right=FALSE, weight = trips$finalweight[!is.na(trips$TRIPMODE) & trips$TRIPMODE>0  & trips$TOURPURP==1 & trips$FULLY_JOINT==0 & trips$TOURMODE==9])

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
tripmode1 <- wtd.hist(trips$TRIPMODE[!is.na(trips$TRIPMODE) & trips$TRIPMODE>0  & trips$TOURPURP==2 & trips$FULLY_JOINT==0 & trips$TOURMODE==1], breaks = seq(1,12, by=1), freq = NULL, right=FALSE, weight = trips$finalweight[!is.na(trips$TRIPMODE) & trips$TRIPMODE>0  & trips$TOURPURP==2 & trips$FULLY_JOINT==0 & trips$TOURMODE==1])
tripmode2 <- wtd.hist(trips$TRIPMODE[!is.na(trips$TRIPMODE) & trips$TRIPMODE>0  & trips$TOURPURP==2 & trips$FULLY_JOINT==0 & trips$TOURMODE==2], breaks = seq(1,12, by=1), freq = NULL, right=FALSE, weight = trips$finalweight[!is.na(trips$TRIPMODE) & trips$TRIPMODE>0  & trips$TOURPURP==2 & trips$FULLY_JOINT==0 & trips$TOURMODE==2])
tripmode3 <- wtd.hist(trips$TRIPMODE[!is.na(trips$TRIPMODE) & trips$TRIPMODE>0  & trips$TOURPURP==2 & trips$FULLY_JOINT==0 & trips$TOURMODE==3], breaks = seq(1,12, by=1), freq = NULL, right=FALSE, weight = trips$finalweight[!is.na(trips$TRIPMODE) & trips$TRIPMODE>0  & trips$TOURPURP==2 & trips$FULLY_JOINT==0 & trips$TOURMODE==3])
tripmode4 <- wtd.hist(trips$TRIPMODE[!is.na(trips$TRIPMODE) & trips$TRIPMODE>0  & trips$TOURPURP==2 & trips$FULLY_JOINT==0 & trips$TOURMODE==4], breaks = seq(1,12, by=1), freq = NULL, right=FALSE, weight = trips$finalweight[!is.na(trips$TRIPMODE) & trips$TRIPMODE>0  & trips$TOURPURP==2 & trips$FULLY_JOINT==0 & trips$TOURMODE==4])
tripmode5 <- wtd.hist(trips$TRIPMODE[!is.na(trips$TRIPMODE) & trips$TRIPMODE>0  & trips$TOURPURP==2 & trips$FULLY_JOINT==0 & trips$TOURMODE==5], breaks = seq(1,12, by=1), freq = NULL, right=FALSE, weight = trips$finalweight[!is.na(trips$TRIPMODE) & trips$TRIPMODE>0  & trips$TOURPURP==2 & trips$FULLY_JOINT==0 & trips$TOURMODE==5])
tripmode6 <- wtd.hist(trips$TRIPMODE[!is.na(trips$TRIPMODE) & trips$TRIPMODE>0  & trips$TOURPURP==2 & trips$FULLY_JOINT==0 & trips$TOURMODE==6], breaks = seq(1,12, by=1), freq = NULL, right=FALSE, weight = trips$finalweight[!is.na(trips$TRIPMODE) & trips$TRIPMODE>0  & trips$TOURPURP==2 & trips$FULLY_JOINT==0 & trips$TOURMODE==6])
tripmode7 <- wtd.hist(trips$TRIPMODE[!is.na(trips$TRIPMODE) & trips$TRIPMODE>0  & trips$TOURPURP==2 & trips$FULLY_JOINT==0 & trips$TOURMODE==7], breaks = seq(1,12, by=1), freq = NULL, right=FALSE, weight = trips$finalweight[!is.na(trips$TRIPMODE) & trips$TRIPMODE>0  & trips$TOURPURP==2 & trips$FULLY_JOINT==0 & trips$TOURMODE==7])
tripmode8 <- wtd.hist(trips$TRIPMODE[!is.na(trips$TRIPMODE) & trips$TRIPMODE>0  & trips$TOURPURP==2 & trips$FULLY_JOINT==0 & trips$TOURMODE==8], breaks = seq(1,12, by=1), freq = NULL, right=FALSE, weight = trips$finalweight[!is.na(trips$TRIPMODE) & trips$TRIPMODE>0  & trips$TOURPURP==2 & trips$FULLY_JOINT==0 & trips$TOURMODE==8])
tripmode9 <- wtd.hist(trips$TRIPMODE[!is.na(trips$TRIPMODE) & trips$TRIPMODE>0  & trips$TOURPURP==2 & trips$FULLY_JOINT==0 & trips$TOURMODE==9], breaks = seq(1,12, by=1), freq = NULL, right=FALSE, weight = trips$finalweight[!is.na(trips$TRIPMODE) & trips$TRIPMODE>0  & trips$TOURPURP==2 & trips$FULLY_JOINT==0 & trips$TOURMODE==9])

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
                                                             
#School                                                       
tripmode1 <- wtd.hist(trips$TRIPMODE[!is.na(trips$TRIPMODE) & trips$TRIPMODE>0  & trips$TOURPURP==3 & trips$FULLY_JOINT==0 & trips$TOURMODE==1], breaks = seq(1,12, by=1), freq = NULL, right=FALSE, weight = trips$finalweight[!is.na(trips$TRIPMODE) & trips$TRIPMODE>0  & trips$TOURPURP==3 & trips$FULLY_JOINT==0 & trips$TOURMODE==1])
tripmode2 <- wtd.hist(trips$TRIPMODE[!is.na(trips$TRIPMODE) & trips$TRIPMODE>0  & trips$TOURPURP==3 & trips$FULLY_JOINT==0 & trips$TOURMODE==2], breaks = seq(1,12, by=1), freq = NULL, right=FALSE, weight = trips$finalweight[!is.na(trips$TRIPMODE) & trips$TRIPMODE>0  & trips$TOURPURP==3 & trips$FULLY_JOINT==0 & trips$TOURMODE==2])
tripmode3 <- wtd.hist(trips$TRIPMODE[!is.na(trips$TRIPMODE) & trips$TRIPMODE>0  & trips$TOURPURP==3 & trips$FULLY_JOINT==0 & trips$TOURMODE==3], breaks = seq(1,12, by=1), freq = NULL, right=FALSE, weight = trips$finalweight[!is.na(trips$TRIPMODE) & trips$TRIPMODE>0  & trips$TOURPURP==3 & trips$FULLY_JOINT==0 & trips$TOURMODE==3])
tripmode4 <- wtd.hist(trips$TRIPMODE[!is.na(trips$TRIPMODE) & trips$TRIPMODE>0  & trips$TOURPURP==3 & trips$FULLY_JOINT==0 & trips$TOURMODE==4], breaks = seq(1,12, by=1), freq = NULL, right=FALSE, weight = trips$finalweight[!is.na(trips$TRIPMODE) & trips$TRIPMODE>0  & trips$TOURPURP==3 & trips$FULLY_JOINT==0 & trips$TOURMODE==4])
tripmode5 <- wtd.hist(trips$TRIPMODE[!is.na(trips$TRIPMODE) & trips$TRIPMODE>0  & trips$TOURPURP==3 & trips$FULLY_JOINT==0 & trips$TOURMODE==5], breaks = seq(1,12, by=1), freq = NULL, right=FALSE, weight = trips$finalweight[!is.na(trips$TRIPMODE) & trips$TRIPMODE>0  & trips$TOURPURP==3 & trips$FULLY_JOINT==0 & trips$TOURMODE==5])
tripmode6 <- wtd.hist(trips$TRIPMODE[!is.na(trips$TRIPMODE) & trips$TRIPMODE>0  & trips$TOURPURP==3 & trips$FULLY_JOINT==0 & trips$TOURMODE==6], breaks = seq(1,12, by=1), freq = NULL, right=FALSE, weight = trips$finalweight[!is.na(trips$TRIPMODE) & trips$TRIPMODE>0  & trips$TOURPURP==3 & trips$FULLY_JOINT==0 & trips$TOURMODE==6])
tripmode7 <- wtd.hist(trips$TRIPMODE[!is.na(trips$TRIPMODE) & trips$TRIPMODE>0  & trips$TOURPURP==3 & trips$FULLY_JOINT==0 & trips$TOURMODE==7], breaks = seq(1,12, by=1), freq = NULL, right=FALSE, weight = trips$finalweight[!is.na(trips$TRIPMODE) & trips$TRIPMODE>0  & trips$TOURPURP==3 & trips$FULLY_JOINT==0 & trips$TOURMODE==7])
tripmode8 <- wtd.hist(trips$TRIPMODE[!is.na(trips$TRIPMODE) & trips$TRIPMODE>0  & trips$TOURPURP==3 & trips$FULLY_JOINT==0 & trips$TOURMODE==8], breaks = seq(1,12, by=1), freq = NULL, right=FALSE, weight = trips$finalweight[!is.na(trips$TRIPMODE) & trips$TRIPMODE>0  & trips$TOURPURP==3 & trips$FULLY_JOINT==0 & trips$TOURMODE==8])
tripmode9 <- wtd.hist(trips$TRIPMODE[!is.na(trips$TRIPMODE) & trips$TRIPMODE>0  & trips$TOURPURP==3 & trips$FULLY_JOINT==0 & trips$TOURMODE==9], breaks = seq(1,12, by=1), freq = NULL, right=FALSE, weight = trips$finalweight[!is.na(trips$TRIPMODE) & trips$TRIPMODE>0  & trips$TOURPURP==3 & trips$FULLY_JOINT==0 & trips$TOURMODE==9])

tripModeProfile <- data.frame(tripmode1$counts, tripmode2$counts, tripmode3$counts, tripmode4$counts,
                              tripmode5$counts, tripmode6$counts, tripmode7$counts, tripmode8$counts, tripmode9$counts)
colnames(tripModeProfile) <- c("tourmode1", "tourmode2", "tourmode3", "tourmode4", "tourmode5", "tourmode6", "tourmode7", "tourmode8", "tourmode9")
write.csv(tripModeProfile, "tripModeProfile_Schl.csv")

tripModeProfile3_vis <- tripModeProfile[1:9,]
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
                                                             
#iMain                                                        
tripmode1 <- wtd.hist(trips$TRIPMODE[!is.na(trips$TRIPMODE) & trips$TRIPMODE>0  & trips$TOURPURP>=5 & trips$TOURPURP<=6 & trips$FULLY_JOINT==0 & trips$TOURMODE==1], breaks = seq(1,12, by=1), freq = NULL, right=FALSE, weight = trips$finalweight[!is.na(trips$TRIPMODE) & trips$TRIPMODE>0  & trips$TOURPURP>=5 & trips$TOURPURP<=6 & trips$FULLY_JOINT==0 & trips$TOURMODE==1])
tripmode2 <- wtd.hist(trips$TRIPMODE[!is.na(trips$TRIPMODE) & trips$TRIPMODE>0  & trips$TOURPURP>=5 & trips$TOURPURP<=6 & trips$FULLY_JOINT==0 & trips$TOURMODE==2], breaks = seq(1,12, by=1), freq = NULL, right=FALSE, weight = trips$finalweight[!is.na(trips$TRIPMODE) & trips$TRIPMODE>0  & trips$TOURPURP>=5 & trips$TOURPURP<=6 & trips$FULLY_JOINT==0 & trips$TOURMODE==2])
tripmode3 <- wtd.hist(trips$TRIPMODE[!is.na(trips$TRIPMODE) & trips$TRIPMODE>0  & trips$TOURPURP>=5 & trips$TOURPURP<=6 & trips$FULLY_JOINT==0 & trips$TOURMODE==3], breaks = seq(1,12, by=1), freq = NULL, right=FALSE, weight = trips$finalweight[!is.na(trips$TRIPMODE) & trips$TRIPMODE>0  & trips$TOURPURP>=5 & trips$TOURPURP<=6 & trips$FULLY_JOINT==0 & trips$TOURMODE==3])
tripmode4 <- wtd.hist(trips$TRIPMODE[!is.na(trips$TRIPMODE) & trips$TRIPMODE>0  & trips$TOURPURP>=5 & trips$TOURPURP<=6 & trips$FULLY_JOINT==0 & trips$TOURMODE==4], breaks = seq(1,12, by=1), freq = NULL, right=FALSE, weight = trips$finalweight[!is.na(trips$TRIPMODE) & trips$TRIPMODE>0  & trips$TOURPURP>=5 & trips$TOURPURP<=6 & trips$FULLY_JOINT==0 & trips$TOURMODE==4])
tripmode5 <- wtd.hist(trips$TRIPMODE[!is.na(trips$TRIPMODE) & trips$TRIPMODE>0  & trips$TOURPURP>=5 & trips$TOURPURP<=6 & trips$FULLY_JOINT==0 & trips$TOURMODE==5], breaks = seq(1,12, by=1), freq = NULL, right=FALSE, weight = trips$finalweight[!is.na(trips$TRIPMODE) & trips$TRIPMODE>0  & trips$TOURPURP>=5 & trips$TOURPURP<=6 & trips$FULLY_JOINT==0 & trips$TOURMODE==5])
tripmode6 <- wtd.hist(trips$TRIPMODE[!is.na(trips$TRIPMODE) & trips$TRIPMODE>0  & trips$TOURPURP>=5 & trips$TOURPURP<=6 & trips$FULLY_JOINT==0 & trips$TOURMODE==6], breaks = seq(1,12, by=1), freq = NULL, right=FALSE, weight = trips$finalweight[!is.na(trips$TRIPMODE) & trips$TRIPMODE>0  & trips$TOURPURP>=5 & trips$TOURPURP<=6 & trips$FULLY_JOINT==0 & trips$TOURMODE==6])
tripmode7 <- wtd.hist(trips$TRIPMODE[!is.na(trips$TRIPMODE) & trips$TRIPMODE>0  & trips$TOURPURP>=5 & trips$TOURPURP<=6 & trips$FULLY_JOINT==0 & trips$TOURMODE==7], breaks = seq(1,12, by=1), freq = NULL, right=FALSE, weight = trips$finalweight[!is.na(trips$TRIPMODE) & trips$TRIPMODE>0  & trips$TOURPURP>=5 & trips$TOURPURP<=6 & trips$FULLY_JOINT==0 & trips$TOURMODE==7])
tripmode8 <- wtd.hist(trips$TRIPMODE[!is.na(trips$TRIPMODE) & trips$TRIPMODE>0  & trips$TOURPURP>=5 & trips$TOURPURP<=6 & trips$FULLY_JOINT==0 & trips$TOURMODE==8], breaks = seq(1,12, by=1), freq = NULL, right=FALSE, weight = trips$finalweight[!is.na(trips$TRIPMODE) & trips$TRIPMODE>0  & trips$TOURPURP>=5 & trips$TOURPURP<=6 & trips$FULLY_JOINT==0 & trips$TOURMODE==8])
tripmode9 <- wtd.hist(trips$TRIPMODE[!is.na(trips$TRIPMODE) & trips$TRIPMODE>0  & trips$TOURPURP>=5 & trips$TOURPURP<=6 & trips$FULLY_JOINT==0 & trips$TOURMODE==9], breaks = seq(1,12, by=1), freq = NULL, right=FALSE, weight = trips$finalweight[!is.na(trips$TRIPMODE) & trips$TRIPMODE>0  & trips$TOURPURP>=5 & trips$TOURPURP<=6 & trips$FULLY_JOINT==0 & trips$TOURMODE==9])

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
tripmode1 <- wtd.hist(trips$TRIPMODE[!is.na(trips$TRIPMODE) & trips$TRIPMODE>0  & trips$TOURPURP>=7 & trips$TOURPURP<=9 & trips$FULLY_JOINT==0 & trips$TOURMODE==1], breaks = seq(1,12, by=1), freq = NULL, right=FALSE, weight = trips$finalweight[!is.na(trips$TRIPMODE) & trips$TRIPMODE>0  & trips$TOURPURP>=7 & trips$TOURPURP<=9 & trips$FULLY_JOINT==0 & trips$TOURMODE==1])
tripmode2 <- wtd.hist(trips$TRIPMODE[!is.na(trips$TRIPMODE) & trips$TRIPMODE>0  & trips$TOURPURP>=7 & trips$TOURPURP<=9 & trips$FULLY_JOINT==0 & trips$TOURMODE==2], breaks = seq(1,12, by=1), freq = NULL, right=FALSE, weight = trips$finalweight[!is.na(trips$TRIPMODE) & trips$TRIPMODE>0  & trips$TOURPURP>=7 & trips$TOURPURP<=9 & trips$FULLY_JOINT==0 & trips$TOURMODE==2])
tripmode3 <- wtd.hist(trips$TRIPMODE[!is.na(trips$TRIPMODE) & trips$TRIPMODE>0  & trips$TOURPURP>=7 & trips$TOURPURP<=9 & trips$FULLY_JOINT==0 & trips$TOURMODE==3], breaks = seq(1,12, by=1), freq = NULL, right=FALSE, weight = trips$finalweight[!is.na(trips$TRIPMODE) & trips$TRIPMODE>0  & trips$TOURPURP>=7 & trips$TOURPURP<=9 & trips$FULLY_JOINT==0 & trips$TOURMODE==3])
tripmode4 <- wtd.hist(trips$TRIPMODE[!is.na(trips$TRIPMODE) & trips$TRIPMODE>0  & trips$TOURPURP>=7 & trips$TOURPURP<=9 & trips$FULLY_JOINT==0 & trips$TOURMODE==4], breaks = seq(1,12, by=1), freq = NULL, right=FALSE, weight = trips$finalweight[!is.na(trips$TRIPMODE) & trips$TRIPMODE>0  & trips$TOURPURP>=7 & trips$TOURPURP<=9 & trips$FULLY_JOINT==0 & trips$TOURMODE==4])
tripmode5 <- wtd.hist(trips$TRIPMODE[!is.na(trips$TRIPMODE) & trips$TRIPMODE>0  & trips$TOURPURP>=7 & trips$TOURPURP<=9 & trips$FULLY_JOINT==0 & trips$TOURMODE==5], breaks = seq(1,12, by=1), freq = NULL, right=FALSE, weight = trips$finalweight[!is.na(trips$TRIPMODE) & trips$TRIPMODE>0  & trips$TOURPURP>=7 & trips$TOURPURP<=9 & trips$FULLY_JOINT==0 & trips$TOURMODE==5])
tripmode6 <- wtd.hist(trips$TRIPMODE[!is.na(trips$TRIPMODE) & trips$TRIPMODE>0  & trips$TOURPURP>=7 & trips$TOURPURP<=9 & trips$FULLY_JOINT==0 & trips$TOURMODE==6], breaks = seq(1,12, by=1), freq = NULL, right=FALSE, weight = trips$finalweight[!is.na(trips$TRIPMODE) & trips$TRIPMODE>0  & trips$TOURPURP>=7 & trips$TOURPURP<=9 & trips$FULLY_JOINT==0 & trips$TOURMODE==6])
tripmode7 <- wtd.hist(trips$TRIPMODE[!is.na(trips$TRIPMODE) & trips$TRIPMODE>0  & trips$TOURPURP>=7 & trips$TOURPURP<=9 & trips$FULLY_JOINT==0 & trips$TOURMODE==7], breaks = seq(1,12, by=1), freq = NULL, right=FALSE, weight = trips$finalweight[!is.na(trips$TRIPMODE) & trips$TRIPMODE>0  & trips$TOURPURP>=7 & trips$TOURPURP<=9 & trips$FULLY_JOINT==0 & trips$TOURMODE==7])
tripmode8 <- wtd.hist(trips$TRIPMODE[!is.na(trips$TRIPMODE) & trips$TRIPMODE>0  & trips$TOURPURP>=7 & trips$TOURPURP<=9 & trips$FULLY_JOINT==0 & trips$TOURMODE==8], breaks = seq(1,12, by=1), freq = NULL, right=FALSE, weight = trips$finalweight[!is.na(trips$TRIPMODE) & trips$TRIPMODE>0  & trips$TOURPURP>=7 & trips$TOURPURP<=9 & trips$FULLY_JOINT==0 & trips$TOURMODE==8])
tripmode9 <- wtd.hist(trips$TRIPMODE[!is.na(trips$TRIPMODE) & trips$TRIPMODE>0  & trips$TOURPURP>=7 & trips$TOURPURP<=9 & trips$FULLY_JOINT==0 & trips$TOURMODE==9], breaks = seq(1,12, by=1), freq = NULL, right=FALSE, weight = trips$finalweight[!is.na(trips$TRIPMODE) & trips$TRIPMODE>0  & trips$TOURPURP>=7 & trips$TOURPURP<=9 & trips$FULLY_JOINT==0 & trips$TOURMODE==9])

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
tripmode1 <- wtd.hist(jtrips$TRIPMODE[!is.na(jtrips$TRIPMODE) & jtrips$TRIPMODE>0  & jtrips$TOURPURP>=5 & jtrips$TOURPURP<=6 & jtrips$TOURMODE==1], breaks = seq(1,12, by=1), freq = NULL, right=FALSE, weight = jtrips$finalweight[!is.na(jtrips$TRIPMODE) & jtrips$TRIPMODE>0  & jtrips$TOURPURP>=5 & jtrips$TOURPURP<=6 & jtrips$TOURMODE==1])
tripmode2 <- wtd.hist(jtrips$TRIPMODE[!is.na(jtrips$TRIPMODE) & jtrips$TRIPMODE>0  & jtrips$TOURPURP>=5 & jtrips$TOURPURP<=6 & jtrips$TOURMODE==2], breaks = seq(1,12, by=1), freq = NULL, right=FALSE, weight = jtrips$finalweight[!is.na(jtrips$TRIPMODE) & jtrips$TRIPMODE>0  & jtrips$TOURPURP>=5 & jtrips$TOURPURP<=6 & jtrips$TOURMODE==2])
tripmode3 <- wtd.hist(jtrips$TRIPMODE[!is.na(jtrips$TRIPMODE) & jtrips$TRIPMODE>0  & jtrips$TOURPURP>=5 & jtrips$TOURPURP<=6 & jtrips$TOURMODE==3], breaks = seq(1,12, by=1), freq = NULL, right=FALSE, weight = jtrips$finalweight[!is.na(jtrips$TRIPMODE) & jtrips$TRIPMODE>0  & jtrips$TOURPURP>=5 & jtrips$TOURPURP<=6 & jtrips$TOURMODE==3])
tripmode4 <- wtd.hist(jtrips$TRIPMODE[!is.na(jtrips$TRIPMODE) & jtrips$TRIPMODE>0  & jtrips$TOURPURP>=5 & jtrips$TOURPURP<=6 & jtrips$TOURMODE==4], breaks = seq(1,12, by=1), freq = NULL, right=FALSE, weight = jtrips$finalweight[!is.na(jtrips$TRIPMODE) & jtrips$TRIPMODE>0  & jtrips$TOURPURP>=5 & jtrips$TOURPURP<=6 & jtrips$TOURMODE==4])
tripmode5 <- wtd.hist(jtrips$TRIPMODE[!is.na(jtrips$TRIPMODE) & jtrips$TRIPMODE>0  & jtrips$TOURPURP>=5 & jtrips$TOURPURP<=6 & jtrips$TOURMODE==5], breaks = seq(1,12, by=1), freq = NULL, right=FALSE, weight = jtrips$finalweight[!is.na(jtrips$TRIPMODE) & jtrips$TRIPMODE>0  & jtrips$TOURPURP>=5 & jtrips$TOURPURP<=6 & jtrips$TOURMODE==5])
tripmode6 <- wtd.hist(jtrips$TRIPMODE[!is.na(jtrips$TRIPMODE) & jtrips$TRIPMODE>0  & jtrips$TOURPURP>=5 & jtrips$TOURPURP<=6 & jtrips$TOURMODE==6], breaks = seq(1,12, by=1), freq = NULL, right=FALSE, weight = jtrips$finalweight[!is.na(jtrips$TRIPMODE) & jtrips$TRIPMODE>0  & jtrips$TOURPURP>=5 & jtrips$TOURPURP<=6 & jtrips$TOURMODE==6])
tripmode7 <- wtd.hist(jtrips$TRIPMODE[!is.na(jtrips$TRIPMODE) & jtrips$TRIPMODE>0  & jtrips$TOURPURP>=5 & jtrips$TOURPURP<=6 & jtrips$TOURMODE==7], breaks = seq(1,12, by=1), freq = NULL, right=FALSE, weight = jtrips$finalweight[!is.na(jtrips$TRIPMODE) & jtrips$TRIPMODE>0  & jtrips$TOURPURP>=5 & jtrips$TOURPURP<=6 & jtrips$TOURMODE==7])
tripmode8 <- wtd.hist(jtrips$TRIPMODE[!is.na(jtrips$TRIPMODE) & jtrips$TRIPMODE>0  & jtrips$TOURPURP>=5 & jtrips$TOURPURP<=6 & jtrips$TOURMODE==8], breaks = seq(1,12, by=1), freq = NULL, right=FALSE, weight = jtrips$finalweight[!is.na(jtrips$TRIPMODE) & jtrips$TRIPMODE>0  & jtrips$TOURPURP>=5 & jtrips$TOURPURP<=6 & jtrips$TOURMODE==8])
tripmode9 <- wtd.hist(jtrips$TRIPMODE[!is.na(jtrips$TRIPMODE) & jtrips$TRIPMODE>0  & jtrips$TOURPURP>=5 & jtrips$TOURPURP<=6 & jtrips$TOURMODE==9], breaks = seq(1,12, by=1), freq = NULL, right=FALSE, weight = jtrips$finalweight[!is.na(jtrips$TRIPMODE) & jtrips$TRIPMODE>0  & jtrips$TOURPURP>=5 & jtrips$TOURPURP<=6 & jtrips$TOURMODE==9])

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
tripmode1 <- wtd.hist(jtrips$TRIPMODE[!is.na(jtrips$TRIPMODE) & jtrips$TRIPMODE>0  & jtrips$TOURPURP>=7 & jtrips$TOURPURP<=9 & jtrips$TOURMODE==1], breaks = seq(1,12, by=1), freq = NULL, right=FALSE, weight = jtrips$finalweight[!is.na(jtrips$TRIPMODE) & jtrips$TRIPMODE>0  & jtrips$TOURPURP>=7 & jtrips$TOURPURP<=9 & jtrips$TOURMODE==1])
tripmode2 <- wtd.hist(jtrips$TRIPMODE[!is.na(jtrips$TRIPMODE) & jtrips$TRIPMODE>0  & jtrips$TOURPURP>=7 & jtrips$TOURPURP<=9 & jtrips$TOURMODE==2], breaks = seq(1,12, by=1), freq = NULL, right=FALSE, weight = jtrips$finalweight[!is.na(jtrips$TRIPMODE) & jtrips$TRIPMODE>0  & jtrips$TOURPURP>=7 & jtrips$TOURPURP<=9 & jtrips$TOURMODE==2])
tripmode3 <- wtd.hist(jtrips$TRIPMODE[!is.na(jtrips$TRIPMODE) & jtrips$TRIPMODE>0  & jtrips$TOURPURP>=7 & jtrips$TOURPURP<=9 & jtrips$TOURMODE==3], breaks = seq(1,12, by=1), freq = NULL, right=FALSE, weight = jtrips$finalweight[!is.na(jtrips$TRIPMODE) & jtrips$TRIPMODE>0  & jtrips$TOURPURP>=7 & jtrips$TOURPURP<=9 & jtrips$TOURMODE==3])
tripmode4 <- wtd.hist(jtrips$TRIPMODE[!is.na(jtrips$TRIPMODE) & jtrips$TRIPMODE>0  & jtrips$TOURPURP>=7 & jtrips$TOURPURP<=9 & jtrips$TOURMODE==4], breaks = seq(1,12, by=1), freq = NULL, right=FALSE, weight = jtrips$finalweight[!is.na(jtrips$TRIPMODE) & jtrips$TRIPMODE>0  & jtrips$TOURPURP>=7 & jtrips$TOURPURP<=9 & jtrips$TOURMODE==4])
tripmode5 <- wtd.hist(jtrips$TRIPMODE[!is.na(jtrips$TRIPMODE) & jtrips$TRIPMODE>0  & jtrips$TOURPURP>=7 & jtrips$TOURPURP<=9 & jtrips$TOURMODE==5], breaks = seq(1,12, by=1), freq = NULL, right=FALSE, weight = jtrips$finalweight[!is.na(jtrips$TRIPMODE) & jtrips$TRIPMODE>0  & jtrips$TOURPURP>=7 & jtrips$TOURPURP<=9 & jtrips$TOURMODE==5])
tripmode6 <- wtd.hist(jtrips$TRIPMODE[!is.na(jtrips$TRIPMODE) & jtrips$TRIPMODE>0  & jtrips$TOURPURP>=7 & jtrips$TOURPURP<=9 & jtrips$TOURMODE==6], breaks = seq(1,12, by=1), freq = NULL, right=FALSE, weight = jtrips$finalweight[!is.na(jtrips$TRIPMODE) & jtrips$TRIPMODE>0  & jtrips$TOURPURP>=7 & jtrips$TOURPURP<=9 & jtrips$TOURMODE==6])
tripmode7 <- wtd.hist(jtrips$TRIPMODE[!is.na(jtrips$TRIPMODE) & jtrips$TRIPMODE>0  & jtrips$TOURPURP>=7 & jtrips$TOURPURP<=9 & jtrips$TOURMODE==7], breaks = seq(1,12, by=1), freq = NULL, right=FALSE, weight = jtrips$finalweight[!is.na(jtrips$TRIPMODE) & jtrips$TRIPMODE>0  & jtrips$TOURPURP>=7 & jtrips$TOURPURP<=9 & jtrips$TOURMODE==7])
tripmode8 <- wtd.hist(jtrips$TRIPMODE[!is.na(jtrips$TRIPMODE) & jtrips$TRIPMODE>0  & jtrips$TOURPURP>=7 & jtrips$TOURPURP<=9 & jtrips$TOURMODE==8], breaks = seq(1,12, by=1), freq = NULL, right=FALSE, weight = jtrips$finalweight[!is.na(jtrips$TRIPMODE) & jtrips$TRIPMODE>0  & jtrips$TOURPURP>=7 & jtrips$TOURPURP<=9 & jtrips$TOURMODE==8])
tripmode9 <- wtd.hist(jtrips$TRIPMODE[!is.na(jtrips$TRIPMODE) & jtrips$TRIPMODE>0  & jtrips$TOURPURP>=7 & jtrips$TOURPURP<=9 & jtrips$TOURMODE==9], breaks = seq(1,12, by=1), freq = NULL, right=FALSE, weight = jtrips$finalweight[!is.na(jtrips$TRIPMODE) & jtrips$TRIPMODE>0  & jtrips$TOURPURP>=7 & jtrips$TOURPURP<=9 & jtrips$TOURMODE==9])

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
tripmode1 <- wtd.hist(trips$TRIPMODE[!is.na(trips$TRIPMODE) & trips$TRIPMODE>0  & trips$SUBTOUR==1 & trips$TOURMODE==1], breaks = seq(1,12, by=1), freq = NULL, right=FALSE, weight = trips$finalweight[!is.na(trips$TRIPMODE) & trips$TRIPMODE>0  & trips$SUBTOUR==1 & trips$TOURMODE==1])
tripmode2 <- wtd.hist(trips$TRIPMODE[!is.na(trips$TRIPMODE) & trips$TRIPMODE>0  & trips$SUBTOUR==1 & trips$TOURMODE==2], breaks = seq(1,12, by=1), freq = NULL, right=FALSE, weight = trips$finalweight[!is.na(trips$TRIPMODE) & trips$TRIPMODE>0  & trips$SUBTOUR==1 & trips$TOURMODE==2])
tripmode3 <- wtd.hist(trips$TRIPMODE[!is.na(trips$TRIPMODE) & trips$TRIPMODE>0  & trips$SUBTOUR==1 & trips$TOURMODE==3], breaks = seq(1,12, by=1), freq = NULL, right=FALSE, weight = trips$finalweight[!is.na(trips$TRIPMODE) & trips$TRIPMODE>0  & trips$SUBTOUR==1 & trips$TOURMODE==3])
tripmode4 <- wtd.hist(trips$TRIPMODE[!is.na(trips$TRIPMODE) & trips$TRIPMODE>0  & trips$SUBTOUR==1 & trips$TOURMODE==4], breaks = seq(1,12, by=1), freq = NULL, right=FALSE, weight = trips$finalweight[!is.na(trips$TRIPMODE) & trips$TRIPMODE>0  & trips$SUBTOUR==1 & trips$TOURMODE==4])
tripmode5 <- wtd.hist(trips$TRIPMODE[!is.na(trips$TRIPMODE) & trips$TRIPMODE>0  & trips$SUBTOUR==1 & trips$TOURMODE==5], breaks = seq(1,12, by=1), freq = NULL, right=FALSE, weight = trips$finalweight[!is.na(trips$TRIPMODE) & trips$TRIPMODE>0  & trips$SUBTOUR==1 & trips$TOURMODE==5])
tripmode6 <- wtd.hist(trips$TRIPMODE[!is.na(trips$TRIPMODE) & trips$TRIPMODE>0  & trips$SUBTOUR==1 & trips$TOURMODE==6], breaks = seq(1,12, by=1), freq = NULL, right=FALSE, weight = trips$finalweight[!is.na(trips$TRIPMODE) & trips$TRIPMODE>0  & trips$SUBTOUR==1 & trips$TOURMODE==6])
tripmode7 <- wtd.hist(trips$TRIPMODE[!is.na(trips$TRIPMODE) & trips$TRIPMODE>0  & trips$SUBTOUR==1 & trips$TOURMODE==7], breaks = seq(1,12, by=1), freq = NULL, right=FALSE, weight = trips$finalweight[!is.na(trips$TRIPMODE) & trips$TRIPMODE>0  & trips$SUBTOUR==1 & trips$TOURMODE==7])
tripmode8 <- wtd.hist(trips$TRIPMODE[!is.na(trips$TRIPMODE) & trips$TRIPMODE>0  & trips$SUBTOUR==1 & trips$TOURMODE==8], breaks = seq(1,12, by=1), freq = NULL, right=FALSE, weight = trips$finalweight[!is.na(trips$TRIPMODE) & trips$TRIPMODE>0  & trips$SUBTOUR==1 & trips$TOURMODE==8])
tripmode9 <- wtd.hist(trips$TRIPMODE[!is.na(trips$TRIPMODE) & trips$TRIPMODE>0  & trips$SUBTOUR==1 & trips$TOURMODE==9], breaks = seq(1,12, by=1), freq = NULL, right=FALSE, weight = trips$finalweight[!is.na(trips$TRIPMODE) & trips$TRIPMODE>0  & trips$SUBTOUR==1 & trips$TOURMODE==9])

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
tripmode1 <- wtd.hist(trips$TRIPMODE[!is.na(trips$TRIPMODE) & trips$TRIPMODE>0  & trips$TOURMODE==1], breaks = seq(1,12, by=1), freq = NULL, right=FALSE, weight = trips$finalweight[!is.na(trips$TRIPMODE) & trips$TRIPMODE>0  & trips$TOURMODE==1])
tripmode2 <- wtd.hist(trips$TRIPMODE[!is.na(trips$TRIPMODE) & trips$TRIPMODE>0  & trips$TOURMODE==2], breaks = seq(1,12, by=1), freq = NULL, right=FALSE, weight = trips$finalweight[!is.na(trips$TRIPMODE) & trips$TRIPMODE>0  & trips$TOURMODE==2])
tripmode3 <- wtd.hist(trips$TRIPMODE[!is.na(trips$TRIPMODE) & trips$TRIPMODE>0  & trips$TOURMODE==3], breaks = seq(1,12, by=1), freq = NULL, right=FALSE, weight = trips$finalweight[!is.na(trips$TRIPMODE) & trips$TRIPMODE>0  & trips$TOURMODE==3])
tripmode4 <- wtd.hist(trips$TRIPMODE[!is.na(trips$TRIPMODE) & trips$TRIPMODE>0  & trips$TOURMODE==4], breaks = seq(1,12, by=1), freq = NULL, right=FALSE, weight = trips$finalweight[!is.na(trips$TRIPMODE) & trips$TRIPMODE>0  & trips$TOURMODE==4])
tripmode5 <- wtd.hist(trips$TRIPMODE[!is.na(trips$TRIPMODE) & trips$TRIPMODE>0  & trips$TOURMODE==5], breaks = seq(1,12, by=1), freq = NULL, right=FALSE, weight = trips$finalweight[!is.na(trips$TRIPMODE) & trips$TRIPMODE>0  & trips$TOURMODE==5])
tripmode6 <- wtd.hist(trips$TRIPMODE[!is.na(trips$TRIPMODE) & trips$TRIPMODE>0  & trips$TOURMODE==6], breaks = seq(1,12, by=1), freq = NULL, right=FALSE, weight = trips$finalweight[!is.na(trips$TRIPMODE) & trips$TRIPMODE>0  & trips$TOURMODE==6])
tripmode7 <- wtd.hist(trips$TRIPMODE[!is.na(trips$TRIPMODE) & trips$TRIPMODE>0  & trips$TOURMODE==7], breaks = seq(1,12, by=1), freq = NULL, right=FALSE, weight = trips$finalweight[!is.na(trips$TRIPMODE) & trips$TRIPMODE>0  & trips$TOURMODE==7])
tripmode8 <- wtd.hist(trips$TRIPMODE[!is.na(trips$TRIPMODE) & trips$TRIPMODE>0  & trips$TOURMODE==8], breaks = seq(1,12, by=1), freq = NULL, right=FALSE, weight = trips$finalweight[!is.na(trips$TRIPMODE) & trips$TRIPMODE>0  & trips$TOURMODE==8])
tripmode9 <- wtd.hist(trips$TRIPMODE[!is.na(trips$TRIPMODE) & trips$TRIPMODE>0  & trips$TOURMODE==9], breaks = seq(1,12, by=1), freq = NULL, right=FALSE, weight = trips$finalweight[!is.na(trips$TRIPMODE) & trips$TRIPMODE>0  & trips$TOURMODE==9])

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

write.csv(temp, "tripModeProfile_vis.csv", row.names = F)

# Total number of stops, trips & tours
cat("Total number of stops : ", sum(stops$finalweight[stops$FULLY_JOINT==0]) + sum(jstops$finalweight))
cat("Total number of trips : ", sum(trips$finalweight[trips$FULLY_JOINT==0]) + sum(jtrips$finalweight))
cat("Total number of tours : ", sum(tours$finalweight[tours$FULLY_JOINT==0]) + sum(unique_joint_tours$finalweight))

# output total numbers in a file
total_population <- sum(pertypeDistbn$freq)
total_households <- sum(hh$finalweight)
total_tours <- sum(tours$finalweight[tours$FULLY_JOINT==0]) + sum(unique_joint_tours$finalweight)
total_trips <- sum(trips$finalweight[trips$FULLY_JOINT==0]) + sum(jtrips$finalweight)
total_stops <- sum(stops$finalweight[stops$FULLY_JOINT==0]) + sum(jstops$finalweight)

totals_var <- c("total_population", "total_households", "total_tours", "total_trips", "total_stops")
totals_val <- c(total_population,total_households, total_tours, total_trips, total_stops)

totals_df <- data.frame(name = totals_var, value = totals_val)

write.csv(totals_df, "totals.csv", row.names = F)

# HH Size distribution
hhSizeDist <- count(hh[!is.na(hh$HHSIZE),], c("HHSIZE"), "finalweight")
write.csv(hhSizeDist, "hhSizeDist.csv", row.names = F)




####*********************************************************************
#### Generate school escorting summaries
####*********************************************************************
#
### These summaries are created for the full OHAS data of Oregon state
### Therefore, clear workspace and reload the required files
#
#library(dplyr)
### INPUTS
#
### READ DATA
#hh_raw <- read.csv(paste(Survey_Dir, "hh.csv", sep = "/"), stringsAsFactors=FALSE)
#hh     <- read.csv(paste(Survey_Processed_Dir, "households.csv", sep = "/"), stringsAsFactors=FALSE)
#per    <- read.csv(paste(Survey_Processed_Dir, "persons.csv", sep = "/"), stringsAsFactors=FALSE)
#tours  <- read.csv(paste(Survey_Processed_Dir, "tours.csv", sep = "/"), stringsAsFactors=FALSE)
#trips  <- read.csv(paste(Survey_Processed_Dir, "trips.csv", sep = "/"), stringsAsFactors=FALSE)
#
#tours$HHEXPFACT <- hh_raw$hh_final_weight_456x[match(tours$HH_ID, hh_raw$SAMPN)]
#trips$HHEXPFACT <- hh_raw$hh_final_weight_456x[match(trips$HH_ID, hh_raw$SAMPN)]
#
#tours$OUT_CHAUFFUER_ID <- as.numeric(tours$OUT_CHAUFFUER_ID)
#tours$INB_CHAUFFUER_ID <- as.numeric(tours$INB_CHAUFFUER_ID)
#
#tours_sample <- select(tours, HH_ID, PER_ID, TOUR_ID, TOURPURP, ESCORTED_TOUR, CHAUFFUER_ID, ESCORTING_TOUR, 
#                       PERSONTYPE, OUT_ESCORT_TYPE, OUT_CHAUFFUER_ID, OUT_CHAUFFUER_PURP, OUT_CHAUFFUER_PTYPE, 
#                       INB_ESCORT_TYPE, INB_CHAUFFUER_ID, INB_CHAUFFUER_PURP, INB_CHAUFFUER_PTYPE, HHEXPFACT)
#tours_sample <- filter(tours_sample, TOURPURP==3 & PERSONTYPE>=6)
#
#
#
#out_sample1 <- filter(tours_sample, !is.na(OUT_ESCORT_TYPE) & !is.na(PERSONTYPE) & OUT_ESCORT_TYPE!="NaN")
#inb_sample1 <- filter(tours_sample, !is.na(INB_ESCORT_TYPE) & !is.na(PERSONTYPE) & INB_ESCORT_TYPE!="NaN")
#out_table1  <- xtabs(HHEXPFACT~OUT_ESCORT_TYPE+PERSONTYPE, data = out_sample1)
#inb_table1  <- xtabs(HHEXPFACT~INB_ESCORT_TYPE+PERSONTYPE, data = inb_sample1)
#
#out_sample2 <- filter(tours_sample, !is.na(OUT_ESCORT_TYPE) & !is.na(OUT_CHAUFFUER_PTYPE) & OUT_ESCORT_TYPE!="NaN" & OUT_CHAUFFUER_PTYPE!="NaN")
#inb_sample2 <- filter(tours_sample, !is.na(INB_ESCORT_TYPE) & !is.na(INB_CHAUFFUER_PTYPE) & INB_ESCORT_TYPE!="NaN" & INB_CHAUFFUER_PTYPE!="NaN")
#out_table2  <- xtabs(HHEXPFACT~OUT_ESCORT_TYPE+OUT_CHAUFFUER_PTYPE, data = out_sample2)
#inb_table2  <- xtabs(HHEXPFACT~INB_ESCORT_TYPE+INB_CHAUFFUER_PTYPE, data = inb_sample2)
#
#
## summary of non-mandatory stops on pure escorting tours
#pure_escort_tours <- tours[(tours$OUT_ESCORTING_TYPE==2 & tours$OUT_ESCORTEE_TOUR_PURP==3) |
#                             (tours$INB_ESCORTING_TYPE==2 & tours$INB_ESCORTEE_TOUR_PURP==3) ,]
#nonescortstops <- trips %>%
#  mutate(nonescortstop = ifelse(DEST_PURP>0 & DEST_PURP<=10 & DEST_PURP!=4 & DEST_IS_TOUR_DEST==0, 1, 0)) %>%
#  mutate(tempid = HH_ID*1000+ PER_ID*100+ TOUR_ID) %>%
#  group_by(tempid) %>%
#  summarise(nonescortstops = sum(nonescortstop)) %>%
#  ungroup()
#
#pure_escort_tours <- pure_escort_tours %>%
#  mutate(tempid = HH_ID*1000+ PER_ID*100+ TOUR_ID) %>%
#  left_join(nonescortstops, by = c("tempid"))
#
## summary of pure school escorting tours [outbound or inbound]
## frequency of non-escorting stops excluding the primary destination by chauffeur tour purpose
#xtabs(HHEXPFACT~TOURPURP+nonescortstops, data = pure_escort_tours)
#
#
## Worker summary
## summary of worker with a child which went to school
## by escort type, can be separated by outbound and inbound direction
#
##get list of active workers with at least one work tour
#active_workers <- tours %>%
#  filter(TOURPURP %in% c(1,10)) %>%   #work and work-related
#  filter(PERSONTYPE %in% c(1,2)) %>%  #full and part-time worker
#  group_by(HH_ID, PER_ID) %>%
#  summarise(PERSONTYPE=max(PERSONTYPE), HHEXPFACT=max(HHEXPFACT)) %>%
#  ungroup()
#
#workers <- per[per$PERSONTYPE %in% c(1,2), ]
#
##get list of students with at least one school tour
#active_students <- tours %>%
#  filter(TOURPURP %in% c(3)) %>%   #school tour
#  filter(PERSONTYPE %in% c(6,7,8)) %>%  #all school students
#  group_by(HH_ID, PER_ID) %>%
#  summarise(PERSONTYPE=max(PERSONTYPE)) %>%
#  ungroup()
#
#students <- per[per$PERSONTYPE %in% c(6,7,8), ] 
#
#hh_active_student <- active_students %>%
#  group_by(HH_ID) %>%
#  mutate(active_student=1) %>%
#  summarise(active_student = max(active_student)) %>%
#  ungroup()
#
##tag active workers with active students in household
#active_workers <- active_workers %>%
#  left_join(hh_active_student, by = c("HH_ID")) %>%
#  mutate(active_student=ifelse(is.na(active_student), 0, active_student))
#
##list of workers who did ride share or pure escort for school student
#out_rs_workers <- tours %>%
#  select(HH_ID, PER_ID, TOUR_ID, TOURPURP, ESCORTED_TOUR, CHAUFFUER_ID, 
#         OUT_ESCORT_TYPE, OUT_CHAUFFUER_ID, OUT_CHAUFFUER_PURP, OUT_CHAUFFUER_PTYPE) %>%
#  filter(TOURPURP==3 & OUT_ESCORT_TYPE==1) %>%
#  group_by(HH_ID, OUT_CHAUFFUER_ID) %>%
#  mutate(num_escort = 1) %>%
#  summarise(out_rs_escort = sum(num_escort))
#
#out_pe_workers <- tours %>%
#  select(HH_ID, PER_ID, TOUR_ID, TOURPURP, ESCORTED_TOUR, CHAUFFUER_ID, 
#         OUT_ESCORT_TYPE, OUT_CHAUFFUER_ID, OUT_CHAUFFUER_PURP, OUT_CHAUFFUER_PTYPE) %>%
#  filter(TOURPURP==3 & OUT_ESCORT_TYPE==2) %>%
#  group_by(HH_ID, OUT_CHAUFFUER_ID) %>%
#  mutate(num_escort = 1) %>%
#  summarise(out_pe_escort = sum(num_escort))
#
#inb_rs_workers <- tours %>%
#  select(HH_ID, PER_ID, TOUR_ID, TOURPURP, ESCORTED_TOUR, CHAUFFUER_ID, 
#         INB_ESCORT_TYPE, INB_CHAUFFUER_ID, INB_CHAUFFUER_PURP, INB_CHAUFFUER_PTYPE) %>%
#  filter(TOURPURP==3 & INB_ESCORT_TYPE==1) %>%
#  group_by(HH_ID, INB_CHAUFFUER_ID) %>%
#  mutate(num_escort = 1) %>%
#  summarise(inb_rs_escort = sum(num_escort))
#
#inb_pe_workers <- tours %>%
#  select(HH_ID, PER_ID, TOUR_ID, TOURPURP, ESCORTED_TOUR, CHAUFFUER_ID, 
#         INB_ESCORT_TYPE, INB_CHAUFFUER_ID, INB_CHAUFFUER_PURP, INB_CHAUFFUER_PTYPE) %>%
#  filter(TOURPURP==3 & INB_ESCORT_TYPE==2) %>%
#  group_by(HH_ID, INB_CHAUFFUER_ID) %>%
#  mutate(num_escort = 1) %>%
#  summarise(inb_pe_escort = sum(num_escort))
#
#active_workers <- active_workers %>%
#  left_join(out_rs_workers, by = c("HH_ID"="HH_ID", "PER_ID"="OUT_CHAUFFUER_ID")) %>%
#  left_join(out_pe_workers, by = c("HH_ID"="HH_ID", "PER_ID"="OUT_CHAUFFUER_ID")) %>%
#  left_join(inb_rs_workers, by = c("HH_ID"="HH_ID", "PER_ID"="INB_CHAUFFUER_ID")) %>%
#  left_join(inb_pe_workers, by = c("HH_ID"="HH_ID", "PER_ID"="INB_CHAUFFUER_ID"))
#
#active_workers[is.na(active_workers)] <- 0
#
#active_workers <- active_workers %>%
#  mutate(out_escort_type = 3) %>%
#  mutate(out_escort_type = ifelse(out_rs_escort>0, 1, out_escort_type)) %>%
#  mutate(out_escort_type = ifelse(out_pe_escort>0, 2, out_escort_type)) %>%
#  mutate(inb_escort_type = 3) %>%
#  mutate(inb_escort_type = ifelse(inb_rs_escort>0, 1, inb_escort_type)) %>%
#  mutate(inb_escort_type = ifelse(inb_pe_escort>0, 2, inb_escort_type))
#
#temp <- filter(active_workers, active_student==1)
#worker_table <- xtabs(HHEXPFACT~out_escort_type+inb_escort_type, data = temp)
#
### add marginal totals to all final tables
#out_table1   <- addmargins(as.table(out_table1))
#inb_table1   <- addmargins(as.table(inb_table1))
#out_table2   <- addmargins(as.table(out_table2))
#inb_table2   <- addmargins(as.table(inb_table2))
#worker_table <- addmargins(as.table(worker_table))
#
### reshape data in required form for visualizer
#out_table1 <- as.data.frame.matrix(out_table1)
#out_table1$id <- row.names(out_table1)
#out_table1 <- melt(out_table1, id = c("id"))
#colnames(out_table1) <- c("esc_type", "child_type", "freq_out")
#out_table1$esc_type <- as.character(out_table1$esc_type)
#out_table1$child_type <- as.character(out_table1$child_type)
#out_table1 <- out_table1[out_table1$esc_type!="Sum",]
#out_table1$child_type[out_table1$child_type=="Sum"] <- "Total"
#
#inb_table1 <- as.data.frame.matrix(inb_table1)
#inb_table1$id <- row.names(inb_table1)
#inb_table1 <- melt(inb_table1, id = c("id"))
#colnames(inb_table1) <- c("esc_type", "child_type", "freq_inb")
#inb_table1$esc_type <- as.character(inb_table1$esc_type)
#inb_table1$child_type <- as.character(inb_table1$child_type)
#inb_table1 <- inb_table1[inb_table1$esc_type!="Sum",]
#inb_table1$child_type[inb_table1$child_type=="Sum"] <- "Total"
#
#table1 <- out_table1
#table1$freq_inb <- inb_table1$freq_inb
#table1$esc_type[table1$esc_type=='1'] <- "Ride Share"
#table1$esc_type[table1$esc_type=='2'] <- "Pure Escort"
#table1$esc_type[table1$esc_type=='3'] <- "No Escort"
#table1$child_type[table1$child_type=='6'] <- 'Driv Student'
#table1$child_type[table1$child_type=='7'] <- 'Non-DrivStudent'
#table1$child_type[table1$child_type=='8'] <- 'Pre-Schooler'
#
#
#out_table2 <- as.data.frame.matrix(out_table2)
#out_table2$id <- row.names(out_table2)
#out_table2 <- melt(out_table2, id = c("id"))
#colnames(out_table2) <- c("esc_type", "chauffeur", "freq_out")
#out_table2$esc_type <- as.character(out_table2$esc_type)
#out_table2$chauffeur <- as.character(out_table2$chauffeur)
#out_table2 <- out_table2[out_table2$esc_type!="Sum",]
#out_table2$chauffeur[out_table2$chauffeur=="Sum"] <- "Total"
#
#inb_table2 <- as.data.frame.matrix(inb_table2)
#inb_table2$id <- row.names(inb_table2)
#inb_table2 <- melt(inb_table2, id = c("id"))
#colnames(inb_table2) <- c("esc_type", "chauffeur", "freq_inb")
#inb_table2$esc_type <- as.character(inb_table2$esc_type)
#inb_table2$chauffeur <- as.character(inb_table2$chauffeur)
#inb_table2 <- inb_table2[inb_table2$esc_type!="Sum",]
#inb_table2$chauffeur[inb_table2$chauffeur=="Sum"] <- "Total"
#
#table2 <- inb_table2[,1:2]
#table2$freq_out <- out_table2$freq_out[match(paste(table2$esc_type, table2$chauffeur, sep = "-"), paste(out_table2$esc_type, out_table2$chauffeur, sep = "-"))]
#table2$freq_inb <- inb_table2$freq_inb[match(paste(table2$esc_type, table2$chauffeur, sep = "-"), paste(inb_table2$esc_type, inb_table2$chauffeur, sep = "-"))]
##table2 <- out_table2
##table2$freq_inb <- inb_table2$freq_inb
#table2$esc_type[table2$esc_type=="1"] <- "Ride Share"
#table2$esc_type[table2$esc_type=="2"] <- "Pure Escort"
#table2$esc_type[table2$esc_type=="3"] <- "No Escort"
#table2$chauffeur[table2$chauffeur=='1'] <- "FT Worker"
#table2$chauffeur[table2$chauffeur=='2'] <- "PT Worker"
#table2$chauffeur[table2$chauffeur=='3'] <- "Univ Stud"
#table2$chauffeur[table2$chauffeur=='4'] <- "Non-Worker"
#table2$chauffeur[table2$chauffeur=='5'] <- "Retiree"
#table2$chauffeur[table2$chauffeur=='6'] <- "Driv Student"
#
#worker_table <- as.data.frame.matrix(worker_table)
#colnames(worker_table) <- c("Ride Share", "Pure Escort", "No Escort", "Total")
#worker_table$DropOff <- row.names(worker_table)
#worker_table$DropOff[worker_table$DropOff=="1"] <- "Ride Share"
#worker_table$DropOff[worker_table$DropOff=="2"] <- "Pure Escort"
#worker_table$DropOff[worker_table$DropOff=="3"] <- "No Escort"
#worker_table$DropOff[worker_table$DropOff=="Sum"] <- "Total"
#
#worker_table <- worker_table[, c("DropOff", "Ride Share","Pure Escort","No Escort","Total")]
#
### write outputs
#write.csv(table1, "esctype_by_childtype.csv", row.names = F)
#write.csv(table2, "esctype_by_chauffeurtype.csv", row.names = F)
#write.csv(worker_table, "worker_school_escorting.csv", row.names = F)
#
#
#detach("package:dplyr", unload=TRUE)
##finish



