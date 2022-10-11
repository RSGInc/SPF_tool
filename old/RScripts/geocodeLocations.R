###############################################################################
# Script to geocode locations to MAZ/TAZ in SANDAG HH Survey
# August 24th 2017, binny.mathewpaul@rsginc.com
###############################################################################

# LIBRARIES
###########
if (!"rgeos" %in% installed.packages()) install.packages("rgeos", repos='http://cran.us.r-project.org')
library(rgeos)
if (!"sp" %in% installed.packages()) install.packages("sp", repos='http://cran.us.r-project.org')
library(sp)
if (!"rgdal" %in% installed.packages()) install.packages("rgdal", repos='http://cran.us.r-project.org')
library(rgdal)

CRS_Census <- "+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0"

# INPUTS
#########
dataDir    <- "E:/Projects/Clients/sandag/TO21_Recalibration/data/latest"
SHPDir     <- "E:/Projects/Clients/sandag/TO21_Recalibration/SHP"
SkimDir    <- "E:/Projects/Clients/sandag/TO21_Recalibration/skim"

trip       <- read.csv(paste(dataDir, "SDRTS_Trip_Data_20170731.csv", sep = "/"), stringsAsFactors = F)
hh         <- read.csv(paste(dataDir, "SDRTS_Household_Data_20170731.csv", sep = "/"), stringsAsFactors = F)
person     <- read.csv(paste(dataDir, "SDRTS_Person_Data_20170731.csv", sep = "/"), stringsAsFactors = F)
DST_SKM    <- read.csv(paste(SkimDir, "MD_TOLL_DST_SKM.csv", sep = "/"), stringsAsFactors = F, header = F, col.names = c("o", "d", "dist"))
MGRA_SHP   <- readOGR(paste(SHPDir, "MGRA", sep = "/"), layer="mgraSR13") #MGRA
MGRA_SHP   <- spTransform(MGRA_SHP, CRS_Census)
TAZ_SHP    <- readOGR(paste(SHPDir, "zones", sep = "/"), layer="zones") #TAZ
TAZ_SHP    <- spTransform(TAZ_SHP, CRS_Census)

# FUNCTIONS 
############

# Overlay
overlay <- function(points, polygon){
  proj4string(points) <- proj4string(polygon) # use same projection
  pointsDF <- over(points,polygon)
  return(pointsDF)
}

getMAZ <- function(lng, lat){
  xy_locations <- data.frame(x=lng, y=lat)
  xy_locations <- SpatialPoints(xy_locations)
  
  proj4string(xy_locations) = proj4string(MGRA_SHP)
  mazTemp <- overlay(xy_locations, MGRA_SHP)
  
  return(mazTemp$MGRA)
}

getTAZ <- function(lng, lat){
  xy_locations <- data.frame(x=lng, y=lat)
  xy_locations <- SpatialPoints(xy_locations)
  
  proj4string(xy_locations) = proj4string(TAZ_SHP)
  tazTemp <- overlay(xy_locations, TAZ_SHP)
  
  return(tazTemp$ZONE)
}


# GEOCODE LOCATIONS
####################

## Recode NAs to zero
trip$origin_lat[is.na(trip$origin_lat)] <- 0
trip$origin_lng[is.na(trip$origin_lng)] <- 0
trip$destination_lng[is.na(trip$destination_lng)] <- 0
trip$destination_lat[is.na(trip$destination_lat)] <- 0
hh$home_lng[is.na(hh$home_lng)] <- 0
hh$home_lat[is.na(hh$home_lat)] <- 0
person$work_lng[is.na(person$work_lng)] <- 0
person$work_lat[is.na(person$work_lat)] <- 0
person$secondwork_lng[is.na(person$secondwork_lng)] <- 0
person$secondwork_lat[is.na(person$secondwork_lat)] <- 0
person$mainschool_lng[is.na(person$mainschool_lng)] <- 0
person$mainschool_lat[is.na(person$mainschool_lat)] <- 0
person$secondschool_lng[is.na(person$secondschool_lng)] <- 0
person$secondschool_lat[is.na(person$secondschool_lat)] <- 0
person$secondhome_lng[is.na(person$secondhome_lng)] <- 0
person$secondhome_lat[is.na(person$secondhome_lat)] <- 0

## Trip Origin
trip$O_TAZ <- getTAZ(trip$origin_lng, trip$origin_lat)
trip$O_MAZ <- getMAZ(trip$origin_lng, trip$origin_lat)
trip$O_TAZ <- as.numeric(as.character(trip$O_TAZ))
trip$O_MAZ <- as.numeric(as.character(trip$O_MAZ))

## Trip Destination
trip$D_TAZ <- getTAZ(trip$destination_lng, trip$destination_lat)
trip$D_MAZ <- getMAZ(trip$destination_lng, trip$destination_lat)
trip$D_TAZ <- as.numeric(as.character(trip$D_TAZ))
trip$D_MAZ <- as.numeric(as.character(trip$D_MAZ))

## Home Location
hh$HOME_TAZ <- getTAZ(hh$home_lng, hh$home_lat)
hh$HOME_MAZ <- getMAZ(hh$home_lng, hh$home_lat)
hh$HOME_TAZ <- as.numeric(as.character(hh$HOME_TAZ))
hh$HOME_MAZ <- as.numeric(as.character(hh$HOME_MAZ))

## Main Work Location
person$WORK_TAZ <- getTAZ(person$work_lng, person$work_lat)
person$WORK_MAZ <- getMAZ(person$work_lng, person$work_lat)
person$WORK_TAZ <- as.numeric(as.character(person$WORK_TAZ))
person$WORK_MAZ <- as.numeric(as.character(person$WORK_MAZ))

## Second work location
person$WORK2_TAZ <- getTAZ(person$secondwork_lng, person$secondwork_lat)
person$WORK2_MAZ <- getMAZ(person$secondwork_lng, person$secondwork_lat)
person$WORK2_TAZ <- as.numeric(as.character(person$WORK2_TAZ))
person$WORK2_MAZ <- as.numeric(as.character(person$WORK2_MAZ))

## Main School Location
person$SCHOOL_TAZ <- getTAZ(person$mainschool_lng, person$mainschool_lat)
person$SCHOOL_MAZ <- getMAZ(person$mainschool_lng, person$mainschool_lat)
person$SCHOOL_TAZ <- as.numeric(as.character(person$SCHOOL_TAZ))
person$SCHOOL_MAZ <- as.numeric(as.character(person$SCHOOL_MAZ))

## Second School Location
person$SCHOOL2_TAZ <- getTAZ(person$secondschool_lng, person$secondschool_lat)
person$SCHOOL2_MAZ <- getMAZ(person$secondschool_lng, person$secondschool_lat)
person$SCHOOL2_TAZ <- as.numeric(as.character(person$SCHOOL2_TAZ))
person$SCHOOL2_MAZ <- as.numeric(as.character(person$SCHOOL2_MAZ))

## Second Home Location
person$HOME2_TAZ <- getTAZ(person$secondhome_lng, person$secondhome_lat)
person$HOME2_MAZ <- getMAZ(person$secondhome_lng, person$secondhome_lat)
person$HOME2_TAZ <- as.numeric(as.character(person$HOME2_TAZ))
person$HOME2_MAZ <- as.numeric(as.character(person$HOME2_MAZ))

## Copy trip distance
trip$Distance <- DST_SKM$dist[match(paste(trip$O_TAZ, trip$D_TAZ, sep = "-"), paste(DST_SKM$o, DST_SKM$d, sep = "-"))]

# write outputs
#write.csv(trip[is.na(trip$O_TAZ),c("origin_lat", "origin_lng")], "trip_NA.csv", row.names = F)
#write.csv(trip[is.na(trip$O_MAZ),c("origin_lat", "origin_lng")], "trip_NA.csv", row.names = F)

write.csv(trip, paste(dataDir, "trip_geocoded.csv", sep = "/"), row.names = F)
write.csv(hh, paste(dataDir, "hh_geocoded.csv", sep = "/"), row.names = F)
write.csv(person, paste(dataDir, "person_geocoded.csv", sep = "/"), row.names = F)


#finish