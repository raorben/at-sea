##############################################################################################
# Script to extract, concatenate, and segment NOAA SWFSC Seabird Strip transect data         #
# from cleaned, concatenated datafiles. Also to combine this data with USGS                  #
# aerial strip transect data in a unified table                                              #
##############################################################################################
# Modified from 
# R code written by Trevor Joyce, 27 April 2018
# for questions and comments please contact: twjoyce@ucsd.edu

#################    DATA KEY    #############################################################
# Fields in the exported datafile are as follows:

#surveyID	- Survey name and year
#transectID	- surveyID & unique numeric transect identifier
#segmentID - transectID & unique numeric segment identifier 
#platform	- Type of craft used to conduct survey (i.e., boat or aerial)
#year	- Year corresponding to the midpoint of each segment
#month	- Month corresponding to the midpoint of each segment	
#day	- Day corresponding to the midpoint of each segment	
#hours	- Hours within days corresponding to the midpoint of each segment
#minutes	- Minutes within days corresponding to the midpoint of each segment
#seconds - Seconds within days corresponding to the midpoint of each segment
#seaState	- Beaufort sea state 
#segLengthKM - Segment length in kilometers (<=2.4km)
#segWidth_Sm_KM - Segment width in kilometers (<=0.3km) for small seabirds (e.g., alcids, storm-petrels, and phalaropes)
#segWidth_Lg_KM	- Segment width in kilometers (<=0.3km) for large seabirds (e.g., all other species)
#longitude - longitude corresponding to the geographic midpoint of each transect (on transect line)
#latitude - latitude corresponding to the geographic midpoint of each transect (on transect line)	
#BFAL	- count of the total number of black-footed albatross observed within each segment
#COMU - count of the total number of common murres observed within each segment
#...

library(dplyr)
library(stringr)
library(geosphere)
library(trakR)

if(Sys.info()[7]=="rachaelorben") {usr<-"/Users/rachaelorben";
gitdir<-"/git_repos/at-sea/";
dir<-"/Library/CloudStorage/Box-Box/Seabird Oceanography Lab/Current_Research/MOSAIC_Seabird At-Sea Observations/"}

if(Sys.info()[7]=="kennerlw") dir<-"/Volumes/GoogleDrive/My Drive/Seabird_Oceanography_Lab/At-SeaSurveys/HALO/Raw Dat"

source(paste0(usr,gitdir,"seabird_functions.R"))

#### SEABIRD_CODES ####

sp<-read.csv(paste0(usr,dir,"data/SeaLog-Species_CodeList.csv"))
names(sp)<-c("Species_Name","Code","Sci_name","Animal","Unidentified_YN")
head(sp)

#### obs ####
#This block of code imports seabird strip transect survey data (obs)
#and harmonizes inconsistencies in column structures within the raw data (function: SEABIRD_IMPORT)

Files<-list.files(paste0(usr,dir,"Analysis/processed_data/"),pattern = "_survey_data.rds",full.names = T,recursive = T)

obs<-NULL
for (j in 1:length(Files)){
  dat<-readRDS(Files[j])
  obs<-rbind(obs,dat)
}

names(obs)
#COL_NAMES=c("CRUISE","YEAR","MONTH","DAY","LM_YEAR","LM_MONTH","LM_DAY","LAT","LONG",
#            "BEAUFORT","SHIP_COURSE","OBS_COND","OBS_SIDE","OBS_CODE_1","EVENT_CODE",
#            "HOUR","MIN","SEC","LM_HOUR","LM_MIN","LM_SEC","SPP_CODE","SPP_NUMBER",
#            "DISTANCE","ASSOC","BEHAV","FLIGHT_DIR","AGE","SEX","COMMENTS","GMT_OFFSET","TEXT"))

#standardize missing  value codes
obs[is.na(obs$Latitude) | obs$Latitude==99.999,"Latitude"]<-NA
obs[is.na(obs$Longitude) | obs$Longitude==999.999,"Longitude"]<-NA

summary(obs)

#Eliminate "9999" codes in Count (signifying NA or not counted - primarily used for fish/dolphins)
obs[!is.na(obs$Count) & obs$Count ==9999,"Count"]<-NA

#Preserve original Latitude and Longitude (some of which will be replaced by LAT_MAM, LONG_MAM)
obs$lat_bird<-obs$Latitude
obs$lon_bird<-obs$Longitude


# Checks for LAT/LON = NAs ----------------------------------------------
# IF missing locations are identified see Section 2. in Pacific Marine Bird Modeling_v0.9_Abbreviated_for_Tammy
# 2. Fill in OBSERVATION coordinates (LAT and LONG) containing NAs with information from MARMAM table
# Section deleted here since no on-effort sightings were identified without locations

obs%>%filter(is.na(Latitude))
obs%>%filter(is.na(Longitude))

# 3. Add TRANSECT ID information to OBSERVATION dataframe based on ON/OFF Tx and changes in "Condition"
unique(obs$Condition) #"5" "4" "3" "1" "2" NA 

#flag condition changes
#flag observer changes
#flag time gaps in GPS crumbs and On/Off
#calculate turning angles and flag angles greater than 10 over a X min period. 

#drop the EVENT_CODE==0 rows because they mess up the subsequent assignment of TRANSECT_ID
#Definitions from SEABIRD SURVEY INSTRUCTION MANUAL California Current Ecosystem Survey – CCES - 2018
#0 - Automatic Time/Position Update: This code will automatically be entered by the computer approximately every 10 minutes and a time and position stamp will follow.
#1 - Begin Transect: Use this code to indicate the beginning of an on-effort transect. 
    #You will need to begin a new transect when any of the environmental conditions change, 
    #when you change observers, or when the ship makes a major course change (> 10°) for any reason. 
#2 - On Effort Sighting: Use this code to indicate a sighting of any type while you are on effort. 
    #A sighting will include birds, bird flocks, mammals, fish, turtles and sharks.
#3 - End Transect: Use this code to indicate the end of an on-effort transect. The most common reasons for ending the 
    #effort period will be a major course change (> 10°) for any reason, a change of observers, a change in environmental 
    #conditions, deviation from the track to pursue a marine mammal school, or the end of the day. 
#4 - Cumulative Total: You may encounter a situation when the bird abundance is so high that it becomes impossible to 
    #count and enter individual birds in real time. In these situations, it will be necessary to count birds in blocks 
    #(as groups of 10's for example) and enter these numbers periodically, at regular intervals (every 5 minutes for example).
    #Should such a situation arise, the critical data are species identification and species number for each time period. 
    #You *may* record accurate data for remaining fields, but if time does not allow for this, use distance code 3 
    #(you should not count any birds if they are farther than 300 meters from the ship), behavior code 6 (for "other", 
    #and note in the comments field that for this situation you are using this code to indicate that no data on behavior were recorded), and codes for “unknown” for association, age, and sex (3, 1, and 1, respectively). Use code 1 for flight direction to indicate that no flight direction data were recorded.
    #Because our project will generally be designed to survey birds in oceanic areas (i.e. where bird densities are 
    #relatively low) you should rarely need to use this code.
#5 - Off-Effort Sighting: As is the case for an on-effort sighting, you should use this code to indicate a sighting 

#obs=obs[obs$EVENT_CODE!=0,]
unique(obs$Condition)

#makes Condition a numeric column, calculates lagged difference 
obs$Condition_num<-as.numeric(obs$Condition)
obs<-obs %>% group_by(DayID) %>% 
  dplyr::mutate(condchang_flag = (lag(Condition_num,1)- Condition_num))
obs$condchang_flag[obs$condchang_flag>0]<-1 #flags changes

obs$Condition-lag(obs$Condition)
# Create a sequential TRANSECT_ID code that can avoids merge duplication problems inherent in TRANSECT_CODE
obs[obs$EVENT_CODE==1,"TRANSECT_ID"]=seq(1:nrow(obs[obs$EVENT_CODE==1,]))
for(i in 2:nrow(obs))
{if(obs$EVENT_CODE[i]%in%c(2,3))
{obs[i,"TRANSECT_ID"]=obs[i-1,"TRANSECT_ID"]}}
#set NAs to 0 avoids errors in selecting transects
obs[is.na(obs$TRANSECT_ID),"TRANSECT_ID"]=0

# Create a table of start LAT, LONG, and LOCAL_DATE_TIME of each TRANSECT 
TRANSECTS_start=obs[obs$EVENT_CODE==1,c("TRANSECT_ID","LAT","LONG","LOCAL_DATE_TIME")]
colnames(TRANSECTS_start)=c("TRANSECT_ID","LAT_1","LONG_1","LOCAL_DATE_TIME_1")

# Create a table of end LAT, LONG, and LOCAL_DATE_TIME of each TRANSECT 
TRANSECTS_stop=obs[obs$EVENT_CODE==3,c("TRANSECT_ID","LAT","LONG","LOCAL_DATE_TIME")]
colnames(TRANSECTS_stop)=c("TRANSECT_ID","LAT_2","LONG_2","LOCAL_DATE_TIME_2")

# Merge based on TRANSECT_ID to create a horizontal table containing the start and stop coordinates and times of each transect
TRANSECTS_temp=merge(TRANSECTS_start,TRANSECTS_stop, by="TRANSECT_ID", all.x=TRUE)

# Calculate DURATION of each TRANSECT in seconds
TRANSECTS_temp$DURATION=(TRANSECTS_temp$LOCAL_DATE_TIME_2-TRANSECTS_temp$LOCAL_DATE_TIME_1)*24*60*60 # units: seconds
TRANSECTS_temp[!is.na(TRANSECTS_temp$DURATION) & TRANSECTS_temp$DURATION==0,"DURATION"]=0.00001
TRANSECTS_temp=na.omit(TRANSECTS_temp)

# Calculate the geodesic LENGTH of each TRANSECT in meters and based on this measurement to approximate velocity in m/s
TRANSECTS_temp$LENGTH=distRhumb(TRANSECTS_temp[,c("LONG_1","LAT_1")],TRANSECTS_temp[,c("LONG_2","LAT_2")])
TRANSECTS_temp$SHIP_VEL_GPS=TRANSECTS_temp$LENGTH/TRANSECTS_temp$DURATION

# Calculate the geodesic LENGTH of each TRANSECT in meters and based on this measurement to approximate velocity in m/s
TRANSECTS_temp$HEADING_TRANSECT=geosphere::bearingRhumb(TRANSECTS_temp[,c("LONG_1","LAT_1")],
                                                        TRANSECTS_temp[,c("LONG_2","LAT_2")])


# Merge the start and stop LAT, LONG, LOCAL_DATE_TIME, LENGTH (m), and DURATION (sec) of each TRANSECT 
# to all records sharing that unique TRANSECT_ID
obs=merge(obs,TRANSECTS_temp,by="TRANSECT_ID",all.x=TRUE)
obs=obs[order(obs$SEQ_ID),]

#Clean-up temporary tables
remove(TRANSECTS_temp,TRANSECTS_start,TRANSECTS_stop)

#calculate distance from each OBSERVATION to the starting coordinates of transect (LONG_1,LAT_1)
obs[!is.na(obs$LONG_1) & !is.na(obs$LONG),"DIST_ORIGIN"]=geosphere::distRhumb(obs[!is.na(obs$LONG_1) & !is.na(obs$LONG),c("LONG_1","LAT_1")],
                                                                                                         obs[!is.na(obs$LONG_1) & !is.na(obs$LONG),c("LONG","LAT")])


#calculate HEADING from each OBSERVATION to the starting coordinates of transect (LONG_1,LAT_1)
obs[!is.na(obs$LONG_1) 
             & !is.na(obs$LONG),"HEADING_ORIGIN"] <- geosphere::bearingRhumb(obs[!is.na(obs$LONG_1) 
                                                                                                   & !is.na(obs$LONG),c("LONG_1","LAT_1")],
                                                                                      obs[!is.na(obs$LONG_1) 
                                                                                                   & !is.na(obs$LONG),c("LONG","LAT")])



# calculate the difference between the overall HEADING_TRANSECT (start to end coordinates)
# and the HEADING from starting coordinates of a transect (LONG_1,LAT_1) 
# to each OBSERVATION within that transect (HEADING_ORIGIN)
obs$HEADING_DIFF <- obs$HEADING_TRANSECT - obs$HEADING_ORIGIN

# calculate the perpendicular distance (DIST_PERP) of each OBSERVATION from the TRANSECT line,
# where DIST_ORIGIN represents the hypotenuse and HEADING_DIFF 
# represents the interior angle (THETA) in a right triangle
obs$DIST_PERP <- obs$DIST_ORIGIN * abs(sin(obs$HEADING_DIFF * pi/180))


# calculate the mean and median perpendicular distance (DIST_PERP) within each TRANSECT
for(i in unique(obs$TRANSECT_ID)){
  obs[obs$TRANSECT_ID == i,"DIST_PERP_MED"] <- median(obs[obs$TRANSECT_ID == i,"DIST_PERP"],na.rm = T)
  obs[obs$TRANSECT_ID == i,"DIST_PERP_MEAN"] <- mean(obs[obs$TRANSECT_ID == i,"DIST_PERP"],na.rm = T)
}
remove(i)

#Set SPP_NUMBERs == 0 to NA (relict of 1988 sampling only)
obs[!is.na(obs$SPP_NUMBER) & obs$SPP_NUMBER==0 ,c("SPP_NUMBER")]=NA

# Standardize the length of SPP_CODE

obs$SPP_CODE=str_trim(as.character(obs$SPP_CODE))

obs=obs[order(obs$SEQ_ID),]
obs=obs[!duplicated(obs$SEQ_ID),]
rownames(obs)=obs$SEQ_ID

#### TRANSECTS ####

#For each FLOCK_ID string together and take counts of the different seabird and marine mammals in
#association with each feeding flock
TRANSECTS=obs[obs$EVENT_CODE==1,]

#Calculate TRANSECT mid-points by averaging LAT_1,LONG_1 and LAT_2,LONG_2, substitute one or the other end point if either
#value is NA (approximate coordinates are better than no coordinates for plotting)
TRANSECTS$LAT_MID=ifelse(!is.na(TRANSECTS$LAT_1) & !is.na(TRANSECTS$LAT_2),(TRANSECTS$LAT_1+TRANSECTS$LAT_2)/2,
                         ifelse(!is.na(TRANSECTS$LAT_1),TRANSECTS$LAT_1,TRANSECTS$LAT_2))
TRANSECTS$LONG_MID=ifelse(!is.na(TRANSECTS$LONG_1) & !is.na(TRANSECTS$LONG_2),(TRANSECTS$LONG_1+TRANSECTS$LONG_2)/2,
                          ifelse(!is.na(TRANSECTS$LONG_1),TRANSECTS$LONG_1,TRANSECTS$LONG_2))

#Assign each transect to a 1 degree LAT_LONG_CODE grid cell for displaying gridded mean densities at a variety of resolutions
TRANSECTS$LAT_LONG_CODE=ifelse(TRANSECTS$LAT_MID>=0,paste(floor(TRANSECTS$LAT_MID),"_",ceiling(TRANSECTS$LONG_MID),sep=""),
                               paste("-",ceiling(TRANSECTS$LAT_MID)*-1,"_",ceiling(TRANSECTS$LONG_MID),sep=""))

#Select the first LAT,LONG, and LOCAL_DATE_TIME of each CRUISE_DAY and merge this back to TRANSECTS
CRUISE_DAY=obs[!duplicated(obs$CRUISE_DAY) & !is.na(obs$LAT),c("CRUISE_DAY","LAT","LONG","LOCAL_DATE_TIME")]
colnames(CRUISE_DAY)=c("CRUISE_DAY","LAT_3","LONG_3","LOCAL_DATE_TIME_3")
TRANSECTS=merge(TRANSECTS, CRUISE_DAY,by="CRUISE_DAY",all.x=TRUE)

#Select the last LAT,LONG, and LOCAL_DATE_TIME of each CRUISE_DAY and merge this back to TRANSECTS
CRUISE_DAY=obs[rev(!duplicated(rev(obs$CRUISE_DAY))) & !is.na(obs$LAT),c("CRUISE_DAY","LAT","LONG","LOCAL_DATE_TIME")]
colnames(CRUISE_DAY)=c("CRUISE_DAY","LAT_4","LONG_4","LOCAL_DATE_TIME_4")
TRANSECTS=merge(TRANSECTS, CRUISE_DAY,by="CRUISE_DAY",all.x=TRUE)

remove(CRUISE_DAY)

#Re-order TRANSECTS by SEQ_ID
TRANSECTS=TRANSECTS[order(TRANSECTS$SEQ_ID),]

#Calculate variable strip WIDTHS and AREAS based on OBS_COND and bird size class (LG - shearwaters, boobies, etc. 
#SM - phalaropes, storm-petrels, small auklets)
TRANSECTS$WIDTH_LG=ifelse(TRANSECTS$OBS_COND>=3,300,200)
TRANSECTS$WIDTH_LG=ifelse(TRANSECTS$OBS_COND>=3,300,200)
TRANSECTS$WIDTH_SM=ifelse(TRANSECTS$OBS_COND>=4,300,ifelse(TRANSECTS$OBS_COND>=2,200,100))
TRANSECTS$AREA_LG=TRANSECTS$WIDTH_LG*TRANSECTS$LENGTH
TRANSECTS$AREA_SM=TRANSECTS$WIDTH_SM*TRANSECTS$LENGTH

#Compile text variable separated by commas of all SPP_CODE, SPP_NUMBER, and DISTANCE falling within each TRANSECT_ID
for(i in unique(TRANSECTS$TRANSECT_ID))
{ TRANSECTS[TRANSECTS$TRANSECT_ID==i,
            "SPP_CODE_ALL"]=paste(obs[obs$TRANSECT_ID==i
                                               & !is.na(obs$SPP_NUMBER),"SPP_CODE"],collapse=",")
TRANSECTS[TRANSECTS$TRANSECT_ID==i,
          "SPP_NUMBER_ALL"]=paste(obs[obs$TRANSECT_ID==i
                                               & !is.na(obs$SPP_NUMBER),"SPP_NUMBER"],collapse=",")
TRANSECTS[TRANSECTS$TRANSECT_ID==i,
          "DISTANCE_ALL"]=paste(obs[obs$TRANSECT_ID==i
                                             & !is.na(obs$SPP_NUMBER),"DISTANCE"],collapse=",")}


#Create a list object TRANSECTS_ALL composed of dataframes corresponding to each strip transect observation 
#in a given TRANSECT with associated LAT,LONG, and distance information. This list can be efficiently queried using lapply.
TRANSECTS_ALL=mapply(cbind, 
                     SPP_CODE=strsplit(TRANSECTS$SPP_CODE_ALL,","),
                     SPP_NUMBER=strsplit(TRANSECTS$SPP_NUMBER_ALL,","),
                     DISTANCE=strsplit(TRANSECTS$DISTANCE_ALL,","),
                     LONG_MID=TRANSECTS$LONG_MID,
                     LAT_MID=TRANSECTS$LAT_MID,
                     OBS_COND=TRANSECTS$OBS_COND,
                     WIDTH_LG=TRANSECTS$WIDTH_LG,
                     WIDTH_SM=TRANSECTS$WIDTH_SM)

TRANSECTS_ALL=lapply(TRANSECTS_ALL,function(x) as.data.frame(x,stringsAsFactors=F))

SPP_CODES_LG_temp = SEABIRD_CODES[SEABIRD_CODES$AOU_CODE!="" & SEABIRD_CODES$Size == "Lg","SPP_CODE"][which(SEABIRD_CODES[SEABIRD_CODES$AOU_CODE!="" & SEABIRD_CODES$Size == "Lg","SPP_CODE"]%in%unique(obs[obs$DISTANCE<=3,"SPP_CODE"]))]
SPP_CODES_SM_temp = SEABIRD_CODES[SEABIRD_CODES$AOU_CODE!="" & SEABIRD_CODES$Size == "Sm","SPP_CODE"][which(SEABIRD_CODES[SEABIRD_CODES$AOU_CODE!="" & SEABIRD_CODES$Size == "Sm","SPP_CODE"]%in%unique(obs[obs$DISTANCE<=3,"SPP_CODE"]))]
SPP_CODES_AMBIG_temp = SEABIRD_CODES[SEABIRD_CODES$AOU_CODE=="","SPP_CODE"][which(SEABIRD_CODES[SEABIRD_CODES$AOU_CODE=="","SPP_CODE"]%in%unique(obs[obs$DISTANCE<=3,"SPP_CODE"]))]

#Sum SPP_NUMBERs for different species within each TRANSECT_ID
for(i in SPP_CODES_LG_temp)
{TRANSECTS[,paste(i,"_COUNT",sep="")]=do.call(rbind, lapply(TRANSECTS_ALL,function(x) 
  sum(as.numeric(x[x$SPP_CODE%in%c(i) & x$DISTANCE <= (as.numeric(x$WIDTH_LG)/100),"SPP_NUMBER"]),na.rm=T)))}

#Sum SPP_NUMBERs for different species within each TRANSECT_ID
for(i in SPP_CODES_SM_temp)
{TRANSECTS[,paste(i,"_COUNT",sep="")]=do.call(rbind, lapply(TRANSECTS_ALL,function(x) 
  sum(as.numeric(x[x$SPP_CODE%in%c(i) & x$DISTANCE <= (as.numeric(x$WIDTH_SM)/100),"SPP_NUMBER"]),na.rm=T)))}

#Sum SPP_NUMBERs for different species within each TRANSECT_ID
for(i in SPP_CODES_AMBIG_temp)
{TRANSECTS[,paste(i,"_COUNT",sep="")]=do.call(rbind, lapply(TRANSECTS_ALL,function(x) 
  sum(as.numeric(x[x$SPP_CODE%in%c(i) & x$DISTANCE <= (as.numeric(x$WIDTH_LG)/100),"SPP_NUMBER"]),na.rm=T)))}


#### SEGMENTS ####
#This block of code subdivides transects into SEGMENTS of 4000m or less, 
#it then assigns seabirds to those SEGMENTS

SEGMENTS=data.frame()

#duplicate each TRANSECTS row by the number of multiples of SEGMENT length (i.e. 4000m) and then append to SEGMENTS
#also remove all columns past SPP_CODE_ALL (these will be added again on a per SEGMENT basis)
for(i in which(!is.na(TRANSECTS$LENGTH) 
               & TRANSECTS$LENGTH > 0
               & !is.na(TRANSECTS$DIST_PERP_MEAN) 
               & TRANSECTS$DIST_PERP_MEAN <= 300))
{SEGMENTS=rbind(SEGMENTS,
                cbind(TRANSECTS[i,1:(which(colnames(TRANSECTS)=="SPP_CODE_ALL")-1)][rep(1,ceiling(TRANSECTS[i,"LENGTH"]/4000)),],
                      data.frame(SEG_ID=seq(1,ceiling(TRANSECTS[i,"LENGTH"]/4000)))))}


#Assign segment lengths (SEG_LENGTH) either the full length if less than 4000m or 4000m and sliver 
for(i in unique(SEGMENTS$TRANSECT_ID)){ 
  if(SEGMENTS[SEGMENTS$TRANSECT_ID==i,"LENGTH"][1]<=4000){
    SEGMENTS[SEGMENTS$TRANSECT_ID==i,"SEG_LENGTH"]=SEGMENTS[SEGMENTS$TRANSECT_ID==i,"LENGTH"][1]
  }
  
  if(SEGMENTS[SEGMENTS$TRANSECT_ID==i,"LENGTH"][1]>4000){ 
    # Create a vector of segments lengths where the vector length is 
    # equal to the number of times the TRANSECT length 
    # evenly divides into the SEGMENT length (i.e. 4000m) 
    # and add any modulus (%%) at the end 
    SEG_LENGTH_temp  = c(rep(4000,SEGMENTS[SEGMENTS$TRANSECT_ID==i,"LENGTH"][1]%/%4000),
                         SEGMENTS[SEGMENTS$TRANSECT_ID==i,"LENGTH"][1]%%4000)
    
    # Randomize the order of segment lengths (sample without replacement)
    SEG_LENGTH_temp = sample(SEG_LENGTH_temp,replace = F)
    
    #Assign each SEGMENT in TRANSECT_ID i a SEGMENT_LENGTH
    SEGMENTS[SEGMENTS$TRANSECT_ID==i,"SEG_LENGTH"]=SEG_LENGTH_temp
  }
  
  # If a modulus segment ("sliver") after subdividing a transect of >4000m
  # is less than 1/2 of target segment length (i.e.,2000m)
  # add this segment to the prior or the subsequent segment
  # (short segments will then be removed outside this loop)
  if(any(SEGMENTS[SEGMENTS$TRANSECT_ID==i,"LENGTH"]>4000
         & SEGMENTS[SEGMENTS$TRANSECT_ID==i,"SEG_LENGTH"]<2000)){
    
    # identify index numbers of short segments
    SLIVER_INDEX_temp = which(SEGMENTS$TRANSECT_ID==i & SEGMENTS$SEG_LENGTH<2000)
    
    # if sliver is the first segment in TRANSECT_ID==i add sliver SEG_LENGTH to the subsequent segment SEG_LENGTH
    if(SEGMENTS[SLIVER_INDEX_temp,"SEG_ID"]==1){
      SEGMENTS[SLIVER_INDEX_temp+1,"SEG_LENGTH"] <- SEGMENTS[SLIVER_INDEX_temp+1,"SEG_LENGTH"] + SEGMENTS[SLIVER_INDEX_temp,"SEG_LENGTH"]
    }
    
    # if sliver is not the first segment in TRANSECT_ID==i add sliver SEG_LENGTH to the prior segment SEG_LENGTH
    if(SEGMENTS[SLIVER_INDEX_temp,"SEG_ID"]>1){
      SEGMENTS[SLIVER_INDEX_temp-1,"SEG_LENGTH"] <- SEGMENTS[SLIVER_INDEX_temp-1,"SEG_LENGTH"] + SEGMENTS[SLIVER_INDEX_temp,"SEG_LENGTH"]
    }
    
  }
  
}

# Clean-up temporary variables
remove(SEG_LENGTH_temp, SLIVER_INDEX_temp, i)

# remove short SEGMENTS (<1/2 of target segment length i.e.,2000m)
SEGMENTS <- SEGMENTS[(-1*which(SEGMENTS$LENGTH > 4000 
                               & SEGMENTS$SEG_LENGTH < 2000)), ]

# reorder SEG_ID's after removing sliver segments 
for(i in unique(SEGMENTS$TRANSECT_ID)){ 
  SEGMENTS[SEGMENTS$TRANSECT_ID == i,"SEG_ID"] <- seq(1,nrow(SEGMENTS[SEGMENTS$TRANSECT_ID == i,])) 
}  

#Calculate the distance between the start of each TRANSECT and the end of each SEGMENT by summing segment lengths cumulatively (SEG_LENGTH_END)
library(dplyr)
SEGMENTS = mutate(group_by(SEGMENTS,TRANSECT_ID), SEG_LENGTH_END=cumsum(SEG_LENGTH))

#Calculate the distance between the start of each TRANSECT and the start of each SEGMENT by subtracting SEG_LENGTH from the SEG_LENGTH_END variable 
SEGMENTS$SEG_LENGTH_START=SEGMENTS$SEG_LENGTH_END-SEGMENTS$SEG_LENGTH

#Calculate the distance between the start of each TRANSECT and the midpoint of each SEGMENT by subtracting half a SEG_LENGTH from the SEG_LENGTH_END variable 
SEGMENTS$SEG_LENGTH_MID=SEGMENTS$SEG_LENGTH_END-(SEGMENTS$SEG_LENGTH/2)

#Calculate proportion of the total TRANSECT (SEGMENTS$LENGTH) to the midpoint of each SEGMENT
SEGMENTS$SEG_LENGTH_PROP=SEGMENTS$SEG_LENGTH_MID/SEGMENTS$LENGTH

#Calculate midpoint time of each SEGMENT as the starting time 
# plus the proportion of the total duration of each TRANSECT elapsed at the midpoint of each SEGMENT
SEGMENTS$SEG_DATE_TIME_MID=SEGMENTS$LOCAL_DATE_TIME_1 + abs(SEGMENTS$LOCAL_DATE_TIME_2-SEGMENTS$LOCAL_DATE_TIME_1)*SEGMENTS$SEG_LENGTH_PROP


#Calculate the start (LONG_5,LAT_5) and end (LONG_6,LAT_6) coordinates of each segment using a 
#SEG_LENGTH and initial bearing between the TRANSECT start and end coordinates
library(geosphere)

#convert from 0:360 to -180:180 reference system (avoids warnings from geospheres, although results are the same)
SEGMENTS$LONG_1=ifelse(SEGMENTS$LONG_1>180,SEGMENTS$LONG_1-360,SEGMENTS$LONG_1)
SEGMENTS$LONG_2=ifelse(SEGMENTS$LONG_2>180,SEGMENTS$LONG_2-360,SEGMENTS$LONG_2)

#Calculate the start (LONG_5,LAT_5) and end (LONG_6,LAT_6) coordinates of segments where SEG_ID=1 using ("LONG_1","LAT_1") as starting coordinates 
SEGMENTS[,c("LONG_5","LAT_5","LONG_6","LAT_6")]=NA
SEGMENTS[SEGMENTS$SEG_ID==1,c("LONG_5","LAT_5")]=SEGMENTS[SEGMENTS$SEG_ID==1,c("LONG_1","LAT_1")]
SEGMENTS[SEGMENTS$SEG_ID==1,c("LONG_6","LAT_6")]=destPointRhumb(p = SEGMENTS[SEGMENTS$SEG_ID==1,c("LONG_1","LAT_1")], 
                                                                b = bearingRhumb(SEGMENTS[SEGMENTS$SEG_ID==1,c("LONG_1","LAT_1")], 
                                                                                 SEGMENTS[SEGMENTS$SEG_ID==1,c("LONG_2","LAT_2")]), 
                                                                d = SEGMENTS[SEGMENTS$SEG_ID==1,"SEG_LENGTH"])

#Calculate the start (LONG_5,LAT_5) and end (LONG_6,LAT_6) coordinates of segments where SEG_ID=2:22 using ("LONG_6","LAT_6") from previous segment as the starting coordinate
for(i in 2:max(SEGMENTS$SEG_ID))
{SEGMENTS[SEGMENTS$SEG_ID==i,c("LONG_5","LAT_5")]=SEGMENTS[which(SEGMENTS$SEG_ID==i)-1,c("LONG_6","LAT_6")]
SEGMENTS[SEGMENTS$SEG_ID==i,c("LONG_6","LAT_6")]=destPointRhumb(p = SEGMENTS[SEGMENTS$SEG_ID==i,c("LONG_5","LAT_5")], 
                                                                b = bearingRhumb(SEGMENTS[SEGMENTS$SEG_ID==i,c("LONG_5","LAT_5")], 
                                                                                 SEGMENTS[SEGMENTS$SEG_ID==i,c("LONG_2","LAT_2")]), 
                                                                d = SEGMENTS[SEGMENTS$SEG_ID==i,"SEG_LENGTH"])
}

#Create unique SEG_ID's by pasting SEG_ID (number 1:22)
SEGMENTS$SEG_ID=paste(SEGMENTS$TRANSECT_ID,SEGMENTS$SEG_ID,sep="_")



#Compile text variable separated by commas of all SPP_CODE, SPP_NUMBER, and DISTANCE falling within each SEG_ID
for(i in 1:nrow(SEGMENTS))
{ SEGMENTS[i,"SPP_CODE_ALL"]=paste(obs[obs$TRANSECT_ID%in%SEGMENTS$TRANSECT_ID[i]
                                                & obs$DIST_ORIGIN>=SEGMENTS$SEG_LENGTH_START[i]
                                                & obs$DIST_ORIGIN<SEGMENTS$SEG_LENGTH_END[i]
                                                & !is.na(obs$SPP_NUMBER),"SPP_CODE"],collapse=",")
SEGMENTS[i,"SPP_NUMBER_ALL"]=paste(obs[obs$TRANSECT_ID%in%SEGMENTS$TRANSECT_ID[i]
                                                & obs$DIST_ORIGIN>=SEGMENTS$SEG_LENGTH_START[i]
                                                & obs$DIST_ORIGIN<SEGMENTS$SEG_LENGTH_END[i]
                                                & !is.na(obs$SPP_NUMBER),"SPP_NUMBER"],collapse=",")
SEGMENTS[i,"DISTANCE_ALL"]=paste(obs[obs$TRANSECT_ID%in%SEGMENTS$TRANSECT_ID[i]
                                              & obs$DIST_ORIGIN>=SEGMENTS$SEG_LENGTH_START[i]
                                              & obs$DIST_ORIGIN<SEGMENTS$SEG_LENGTH_END[i]
                                              & !is.na(obs$SPP_NUMBER),"DISTANCE"],collapse=",")
SEGMENTS[i,"DIST_ORIGIN_ALL"]=paste(round(obs[obs$TRANSECT_ID%in%SEGMENTS$TRANSECT_ID[i]
                                                       & obs$DIST_ORIGIN>=SEGMENTS$SEG_LENGTH_START[i]
                                                       & obs$DIST_ORIGIN<SEGMENTS$SEG_LENGTH_END[i]
                                                       & !is.na(obs$SPP_NUMBER),"DIST_ORIGIN"],0),collapse=",")}

#Create a list object SEGMENTS_ALL composed of dataframes corresponding to each strip transect observation 
#in a given SEGMENT with associated LAT,LONG, and distance information. This list can be efficiently queried using lapply.
SEGMENTS_ALL=mapply(cbind, 
                    SPP_CODE=strsplit(SEGMENTS$SPP_CODE_ALL,","),
                    SPP_NUMBER=strsplit(SEGMENTS$SPP_NUMBER_ALL,","),
                    DISTANCE=strsplit(SEGMENTS$DISTANCE_ALL,","),
                    LONG_MID=SEGMENTS$LONG_MID,
                    LAT_MID=SEGMENTS$LAT_MID,
                    OBS_COND=SEGMENTS$OBS_COND,
                    WIDTH_LG=SEGMENTS$WIDTH_LG,
                    WIDTH_SM=SEGMENTS$WIDTH_SM)

SEGMENTS_ALL=lapply(SEGMENTS_ALL,function(x) as.data.frame(x,stringsAsFactors=F))


SPP_CODES_LG_temp = SEABIRD_CODES[SEABIRD_CODES$AOU_CODE!="" & SEABIRD_CODES$Size == "Lg","SPP_CODE"][which(SEABIRD_CODES[SEABIRD_CODES$AOU_CODE!="" & SEABIRD_CODES$Size == "Lg","SPP_CODE"]%in%unique(obs[obs$DISTANCE<=3,"SPP_CODE"]))]
SPP_CODES_SM_temp = SEABIRD_CODES[SEABIRD_CODES$AOU_CODE!="" & SEABIRD_CODES$Size == "Sm","SPP_CODE"][which(SEABIRD_CODES[SEABIRD_CODES$AOU_CODE!="" & SEABIRD_CODES$Size == "Sm","SPP_CODE"]%in%unique(obs[obs$DISTANCE<=3,"SPP_CODE"]))]
SPP_CODES_AMBIG_temp = SEABIRD_CODES[SEABIRD_CODES$AOU_CODE=="","SPP_CODE"][which(SEABIRD_CODES[SEABIRD_CODES$AOU_CODE=="","SPP_CODE"]%in%unique(obs[obs$DISTANCE<=3,"SPP_CODE"]))]

#Calculate counts within each SPP_CODE organized by individual species with AOU codes, 
#followed by taxonomically ambiguous codes
for(i in SPP_CODES_LG_temp)
{SEGMENTS[,i]=do.call(rbind, lapply(SEGMENTS_ALL,function(x) 
  sum(as.numeric(x[x$SPP_CODE%in%c(i) & x$DISTANCE <= (as.numeric(x$WIDTH_LG)/100),"SPP_NUMBER"]),na.rm=T)))}


#Calculate counts within each SPP_CODE organized by individual species with AOU codes, 
#followed by taxonomically ambiguous codes
for(i in SPP_CODES_SM_temp)
{SEGMENTS[,i]=do.call(rbind, lapply(SEGMENTS_ALL,function(x) 
  sum(as.numeric(x[x$SPP_CODE%in%c(i) & x$DISTANCE <= (as.numeric(x$WIDTH_SM)/100),"SPP_NUMBER"]),na.rm=T)))}


#Calculate counts within each SPP_CODE organized by individual species with AOU codes, 
#followed by taxonomically ambiguous codes
for(i in SPP_CODES_AMBIG_temp)
{SEGMENTS[,i]=do.call(rbind, lapply(SEGMENTS_ALL,function(x) 
  sum(as.numeric(x[x$SPP_CODE%in%c(i) & x$DISTANCE <= (as.numeric(x$WIDTH_LG)/100),"SPP_NUMBER"]),na.rm=T)))}

#### EXPORT ####

#Gather necessary columns from SEGMENTS to recreate example data table structure provided by Jeffery Leirness
#surveyID	transectID	segmentID	platform	year	month	day	hours	minutes	seconds	seaState	
#segLengthKM	segWidthKM	longitude	latitude
EXPORT=subset(SEGMENTS,select=c(CRUISE,TRANSECT_ID,SEG_ID,
                                LM_YEAR,LM_MONTH,LM_DAY,LM_HOUR,LM_MIN,LM_SEC,SEG_DATE_TIME_MID,
                                BEAUFORT,SEG_LENGTH,WIDTH_LG,WIDTH_SM,
                                LONG_5,LAT_5,LONG_6,LAT_6))

#Create surveyID column with names of surveys by year
EXPORT[EXPORT$LM_YEAR%in%c(1996,2001,2008),"surveyID"]="ORCAWALE"
EXPORT[EXPORT$LM_YEAR%in%c(2005),"surveyID"]="CSCAPE"
EXPORT[EXPORT$LM_YEAR%in%c(2014),"surveyID"]="CalCurCEAS"
EXPORT$surveyID=paste(EXPORT$surveyID,EXPORT$LM_YEAR,sep="")

#Create transectID column by pasting together surveyID column with unique TRANSECT_ID column
EXPORT$transectID=paste(EXPORT$surveyID,sprintf("%04d",EXPORT$TRANSECT_ID),sep="_")

#Create segmentID column by pasting together transectID column with unique SEG_ID column
library(stringr)
EXPORT$segmentID=paste(EXPORT$transectID,sprintf("%02d",as.numeric(str_split_fixed(EXPORT$SEG_ID,pattern="_",n=2)[,2])),sep="_")

#Generate year	month	day	hours	minutes	seconds from the SEG_DATE_TIME_MID
library(chron)
EXPORT$year=as.numeric(as.character(years(as.chron(EXPORT$SEG_DATE_TIME_MID,origin=c(month = 12, day = 30, year = 1899)))))
EXPORT$month=as.numeric(months(as.chron(EXPORT$SEG_DATE_TIME_MID,origin=c(month = 12, day = 30, year = 1899))))
EXPORT$day=as.numeric(as.character(days(as.chron(EXPORT$SEG_DATE_TIME_MID,origin=c(month = 12, day = 30, year = 1899)))))
EXPORT$hours=as.numeric(as.character(hours(as.chron(EXPORT$SEG_DATE_TIME_MID,origin=c(month = 12, day = 30, year = 1899)))))
EXPORT$minutes=as.numeric(as.character(minutes(as.chron(EXPORT$SEG_DATE_TIME_MID,origin=c(month = 12, day = 30, year = 1899)))))
EXPORT$seconds=as.numeric(as.character(seconds(as.chron(EXPORT$SEG_DATE_TIME_MID,origin=c(month = 12, day = 30, year = 1899)))))

#Add a platform column to distinguish from aerial surveys
EXPORT$platform="boat"

#Relabel the BEAUFORT to seaState
EXPORT$seaState=EXPORT$BEAUFORT

#Convert to SEG_LENGTH in meters to segLengthKM in kilometers
EXPORT$segLengthKM=EXPORT$SEG_LENGTH/1000

#Convert to WIDTH_SM in meters to segWidth_Sm_KM in kilometers
EXPORT$segWidth_Sm_KM=EXPORT$WIDTH_SM/1000

#Convert to WIDTH_LG in meters to segWidth_Lg_KM in kilometers
EXPORT$segWidth_Lg_KM=EXPORT$WIDTH_LG/1000

#Calculate longitude	latitude midpoints of each segment from end coordinates (LONG_5,LAT_5,LONG_6,LAT_6)
library(geosphere)
EXPORT$longitude=destPointRhumb(p = EXPORT[,c("LONG_5","LAT_5")], 
                                b = bearingRhumb(EXPORT[,c("LONG_5","LAT_5")], 
                                                 EXPORT[,c("LONG_6","LAT_6")]), 
                                d = EXPORT[,"SEG_LENGTH"]/2)[,1]
EXPORT$latitude=destPointRhumb(p = EXPORT[,c("LONG_5","LAT_5")], 
                               b = bearingRhumb(EXPORT[,c("LONG_5","LAT_5")], 
                                                EXPORT[,c("LONG_6","LAT_6")]), 
                               d = EXPORT[,"SEG_LENGTH"]/2)[,2]

#Subset columns 
EXPORT = subset(EXPORT,select=c(surveyID,transectID,segmentID,platform,year,month,day,hours,minutes,seconds,seaState,segLengthKM,segWidth_Sm_KM,segWidth_Lg_KM,longitude,latitude))


# Assign AOU species codes to large seabirds
# paste(paste("EXPORT$",SEABIRD_CODES[SEABIRD_CODES$SPP_CODE %in% SPP_CODES_LG_temp,"AOU_CODE"]," = SEGMENTS$",SPP_CODES_LG_temp,sep = ""),collapse = " ")
EXPORT$RTLO = SEGMENTS$LORT 
EXPORT$COLO = SEGMENTS$LOCO 
EXPORT$PALO = SEGMENTS$LOPA 
EXPORT$WAAL = SEGMENTS$ALWN 
EXPORT$LAAL = SEGMENTS$ALLA 
EXPORT$BFAL = SEGMENTS$ALBF 
EXPORT$NOFU = SEGMENTS$FUNO + SEGMENTS$FUND + SEGMENTS$FUNI + SEGMENTS$FUNL
EXPORT$BULS = SEGMENTS$SHNZ 
EXPORT$FFSH = SEGMENTS$SHFF 
EXPORT$PFSH = SEGMENTS$SHPF 
EXPORT$SOSH = SEGMENTS$SHSO 
EXPORT$STRS = SEGMENTS$SHST 
EXPORT$BVSH = SEGMENTS$SHBV 
EXPORT$SRTS = SEGMENTS$SHSB 
EXPORT$COPE = SEGMENTS$PECO 
EXPORT$MOPE = SEGMENTS$PEMO 
EXPORT$STPE = SEGMENTS$PEST 
EXPORT$HAPE = SEGMENTS$PEHA 
EXPORT$MUPE = SEGMENTS$PEMU 
EXPORT$RBTR = SEGMENTS$TBRB 
EXPORT$RTTR = SEGMENTS$TBRT 
EXPORT$BRPE = SEGMENTS$PEBR 
EXPORT$BRBO = SEGMENTS$BOBR 
EXPORT$DCCO = SEGMENTS$CODC 
EXPORT$BRAC = SEGMENTS$COBR 
EXPORT$PECO = SEGMENTS$COPE 
EXPORT$SPSK = SEGMENTS$SKSP 
EXPORT$POJA = SEGMENTS$JAPO 
EXPORT$PAJA = SEGMENTS$JAPA 
EXPORT$LTJA = SEGMENTS$JALT 
EXPORT$HEEG = SEGMENTS$GUHE 
EXPORT$RBGU = SEGMENTS$GURB 
EXPORT$MEGU = SEGMENTS$GUME 
EXPORT$HERG = SEGMENTS$GUHR 
EXPORT$THGU = SEGMENTS$GUTH 
EXPORT$CAGU = SEGMENTS$GUCA 
EXPORT$WEGU = SEGMENTS$GUWE 
EXPORT$GWGU = SEGMENTS$GUGW 
EXPORT$WGWH = SEGMENTS$GUWG 
EXPORT$LAGU = SEGMENTS$GULA 
EXPORT$BOGU = SEGMENTS$GUBO 
EXPORT$BLKI = SEGMENTS$KIBL 
EXPORT$SAGU = SEGMENTS$GUSA 
EXPORT$CATE = SEGMENTS$TECA 
EXPORT$COTE = SEGMENTS$TECO 
EXPORT$ARTE = SEGMENTS$TEAR 
EXPORT$FOTE = SEGMENTS$TEFO 
EXPORT$LETE = SEGMENTS$TELE 
EXPORT$ROYT = SEGMENTS$TERO 
EXPORT$ELTE = SEGMENTS$TEEL 
EXPORT$COMU = SEGMENTS$MUCO 
EXPORT$PIGU = SEGMENTS$GUPI 
EXPORT$MAMU = SEGMENTS$MUMA 
EXPORT$KIMU = SEGMENTS$MUKI 
EXPORT$CRMU = SEGMENTS$MUCR 
EXPORT$ANMU = SEGMENTS$MUAN 
EXPORT$RHAU = SEGMENTS$AURH 
EXPORT$TUPU = SEGMENTS$PUTU
EXPORT$MUXA = SEGMENTS$MUXA 

# Assign AOU species codes to small seabirds

# paste(paste("EXPORT$",SEABIRD_CODES[SEABIRD_CODES$SPP_CODE %in% SPP_CODES_SM_temp,"AOU_CODE"]," = SEGMENTS$",SPP_CODES_SM_temp,sep = ""),collapse = " ")

EXPORT$WRSP = SEGMENTS$SPGA 
EXPORT$BSTP = SEGMENTS$SPHA 
EXPORT$LESP = SEGMENTS$SPLE 
EXPORT$ASSP = SEGMENTS$SPAS 
EXPORT$RISP = SEGMENTS$SPHO 
EXPORT$FTSP = SEGMENTS$SPFT 
EXPORT$BLSP = SEGMENTS$SPBL 
EXPORT$LSTP = SEGMENTS$SPLS 
EXPORT$LESP = SEGMENTS$SPLW + SEGMENTS$SPLD + SEGMENTS$SPLI
EXPORT$REPH = SEGMENTS$PHRE 
EXPORT$RNPH = SEGMENTS$PHNO 
EXPORT$CAAU = SEGMENTS$AUCA

#Find SPP_CODES which do not have a unique AOU_CODE (taxonomically ambiguous codes, out-dated taxonomic groupings (e.g., PEDR, MUXA))

# paste(paste("EXPORT$",SPP_CODES_AMBIG_temp," = SEGMENTS$",SPP_CODES_AMBIG_temp,sep = ""),collapse = " ")

EXPORT$LOON = SEGMENTS$LOON
EXPORT$GREB = SEGMENTS$GREB
EXPORT$CORM = SEGMENTS$CORM
EXPORT$SPWR = SEGMENTS$SPWR
EXPORT$SPDR = SEGMENTS$SPDR
EXPORT$COOK = SEGMENTS$COOK
EXPORT$PHAL = SEGMENTS$PHAL
EXPORT$JAEG = SEGMENTS$JAEG

EXPORT$SHSS = SEGMENTS$SHSS
EXPORT$PEDR = SEGMENTS$PEDR
EXPORT$SPLH = SEGMENTS$SPLH
EXPORT$MUXC = SEGMENTS$MUXC
EXPORT$TEAC = SEGMENTS$TEAC
EXPORT$JAPL = SEGMENTS$JAPL 

# EXPORT$SHSP = SEGMENTS$SHSP 
# EXPORT$PESM = SEGMENTS$PESM 
# EXPORT$PESC = SEGMENTS$PESC 
# EXPORT$PTSP = SEGMENTS$PTSP 
# EXPORT$ALCI = SEGMENTS$ALCI 
# EXPORT$SPSP = SEGMENTS$SPSP 
# EXPORT$TROP = SEGMENTS$TROP 
# EXPORT$DUCK = SEGMENTS$DUCK 
# EXPORT$GULL = SEGMENTS$GULL 
# EXPORT$GUAN = SEGMENTS$GUAN 
# EXPORT$TERN = SEGMENTS$TERN 
# EXPORT$ALCD = SEGMENTS$ALCD 
# EXPORT$NPSS = SEGMENTS$NPSS 
# EXPORT$UNID = SEGMENTS$UNID 
# EXPORT$SHOR = SEGMENTS$SHOR 
# EXPORT$RAPT = SEGMENTS$RAPT 
# EXPORT$PASS = SEGMENTS$PASS


# #### EXPORT_PaCSEA ####
# 
# #Import formatted and segmented PaCSEA dataset from USGS (Josh Adams and Jonathan Felis)
# EXPORT_PaCSEA=read.csv(paste(DATA_DIR,"PaCSEA/","PaCSEA_4000mCentroids.csv",sep = ""))
# 
# #Assign segWidthKM accross segment width for large and small seabird (segWidth_Sm_KM ,segWidth_Lg_KM) columns
# #(this distinction does not exist in aerial data)
# EXPORT_PaCSEA[,c("segWidth_Sm_KM","segWidth_Lg_KM")]=EXPORT_PaCSEA[,"segWidthKM"]
# 
# #Remove segWidthKM columns
# EXPORT_PaCSEA=subset(EXPORT_PaCSEA,select= -c(segWidthKM))
# 
# #Add to EXPORT columns of 0's for any columns (i.e., species names) that exist in EXPORT_PaCSEA or EXPORT_SoCA but not in EXPORT
# EXPORT[,colnames(EXPORT_PaCSEA)[!colnames(EXPORT_PaCSEA)%in%colnames(EXPORT)]]=0
# 
# #Add columns (i.e., species names) missing from EXPORT_PaCSEA or EXPORT_SoCA header that are found in EXPORT
# EXPORT_PaCSEA[,colnames(EXPORT)[!colnames(EXPORT)%in%colnames(EXPORT_PaCSEA)]]=0
# 
# #Append EXPORT_PaCSEA to EXPORT
# EXPORT=rbind(EXPORT,EXPORT_PaCSEA)
# 
# #### EXPORT_SoCA ####
# 
# #Import formatted and segmented SoCA dataset from USGS (Josh Adams and Jonathan Felis)
# EXPORT_SoCA=read.csv(paste(DATA_DIR,"SoCA/","SoCA_4000mCentroids.csv",sep = ""))
# 
# #Assign segWidthKM accross segment width for large and small seabird (segWidth_Sm_KM ,segWidth_Lg_KM) columns
# #(this distinction does not exist in aerial data)
# EXPORT_SoCA[,c("segWidth_Sm_KM","segWidth_Lg_KM")]=EXPORT_SoCA[,"segWidthKM"]
# 
# #Remove segWidthKM columns
# EXPORT_SoCA=subset(EXPORT_SoCA,select= -c(segWidthKM))
# 
# #Add to EXPORT columns of 0's for any columns (i.e., species names) that exist in EXPORT_PaCSEA or EXPORT_SoCA but not in EXPORT
# EXPORT[,colnames(EXPORT_SoCA)[!colnames(EXPORT_SoCA)%in%colnames(EXPORT)]]=0
# 
# #Add columns (i.e., species names) missing from EXPORT_PaCSEA or EXPORT_SoCA header that are found in EXPORT
# EXPORT_SoCA[,colnames(EXPORT)[!colnames(EXPORT)%in%colnames(EXPORT_SoCA)]]=0
# 
# #Append EXPORT_SoCA to EXPORT
# EXPORT=rbind(EXPORT,EXPORT_SoCA)
# 
# #Merge CMTE column (synonymous with Arctic/Common Tern) into TEAC and then remove CMTE column
# EXPORT$TEAC = EXPORT$TEAC + EXPORT$CMTE
# EXPORT = subset(EXPORT,select = -c(CMTE))


#### SPP_CODES_SYDEMAN ####
SPP_CODES_SYDEMAN <- read.csv(paste(DATA_DIR,"CalCOFI/Sydeman_Species_Codes.csv",sep = ""),
                              head=T,stringsAsFactors=F)

#### obs_SYDEMAN ####
obs_SYDEMAN = data.frame()

for(i in 1:3)
{PROGRAM_temp = c("CalCOFI","CPR","NMFS")
SURVEY_temp = c("CalCOFI", "NOAA CPR", "NOAA Rockfish")  
obs_SYDEMAN_temp = read.csv(paste(DATA_DIR,SURVEY_temp[i],"/Bird and Mammal Census_",PROGRAM_temp[i],"_obs.csv",sep = ""), stringsAsFactors = F)
obs_SYDEMAN = rbind(obs_SYDEMAN,obs_SYDEMAN_temp)}
remove(obs_SYDEMAN_temp,PROGRAM_temp,SURVEY_temp )

#### TRANSECTS_SYDEMAN ####
TRANSECTS_SYDEMAN = data.frame()

for(i in 1:3)
{PROGRAM_temp = c("CalCOFI","CPR","NMFS")
SURVEY_temp = c("CalCOFI", "NOAA CPR", "NOAA Rockfish")  
TRANSECTS_SYDEMAN_temp = read.csv(paste(DATA_DIR,SURVEY_temp[i],"/Bird and Mammal Census_",PROGRAM_temp[i],"_Transect Log.csv",sep = ""), stringsAsFactors = F)
TRANSECTS_SYDEMAN = rbind(TRANSECTS_SYDEMAN,TRANSECTS_SYDEMAN_temp)}
remove(TRANSECTS_SYDEMAN_temp,PROGRAM_temp,SURVEY_temp )

TRANSECTS_SYDEMAN$GMT_LMT = "LMT"
TRANSECTS_SYDEMAN[TRANSECTS_SYDEMAN$Cruise %in% unique(TRANSECTS_SYDEMAN[TRANSECTS_SYDEMAN$Time..sec. < 400 | TRANSECTS_SYDEMAN$Time..sec. > 2200 ,"Cruise"]),"GMT_LMT"] = "GMT"

TRANSECTS_SYDEMAN$LM_YEAR = as.numeric(substr(TRANSECTS_SYDEMAN$Date,1,4))
TRANSECTS_SYDEMAN$LM_MONTH= as.numeric(substr(TRANSECTS_SYDEMAN$Date,6,7))
TRANSECTS_SYDEMAN$LM_DAY= as.numeric(substr(TRANSECTS_SYDEMAN$Date,9,10))

TRANSECTS_SYDEMAN$LM_HOUR = as.numeric(stringr::str_sub(floor(TRANSECTS_SYDEMAN$Time..sec.),-4,-3))
#in cases where there are no -4,-3 digits (implying mid-night to 1am e.g.,00:10:10) assign LM_HOUR to 0
TRANSECTS_SYDEMAN[is.na(TRANSECTS_SYDEMAN$LM_HOUR),"LM_HOUR"] = 0 
TRANSECTS_SYDEMAN$LM_MIN= as.numeric(stringr::str_sub(floor(TRANSECTS_SYDEMAN$Time..sec.),-2,-1))
TRANSECTS_SYDEMAN$LM_SEC= (TRANSECTS_SYDEMAN$Time..sec.- floor(TRANSECTS_SYDEMAN$Time..sec.))*60

for(i in unique(TRANSECTS_SYDEMAN[!is.na(TRANSECTS_SYDEMAN$LM_YEAR),"LM_YEAR"]))
{TRANSECTS_SYDEMAN[!is.na(TRANSECTS_SYDEMAN$LM_YEAR) & TRANSECTS_SYDEMAN$LM_YEAR == i,"TRANSECT_ID"] = seq(1,nrow(TRANSECTS_SYDEMAN[!is.na(TRANSECTS_SYDEMAN$LM_YEAR) & TRANSECTS_SYDEMAN$LM_YEAR == i,]))}

TRANSECTS_SYDEMAN$SEG_ID = 1

#Create surveyID column with names of surveys by year
TRANSECTS_SYDEMAN$surveyID = paste(TRANSECTS_SYDEMAN$SVY,TRANSECTS_SYDEMAN$LM_YEAR,sep = "")

#Create transectID column by pasting together surveyID column with unique TRANSECT_ID column
TRANSECTS_SYDEMAN$transectID=paste(TRANSECTS_SYDEMAN$surveyID,sprintf("%04d",TRANSECTS_SYDEMAN$TRANSECT_ID),sep="_")

#Create segmentID column by pasting together transectID column with unique SEG_ID column
library(stringr)
TRANSECTS_SYDEMAN$segmentID=paste(TRANSECTS_SYDEMAN$transectID,sprintf("%02d",as.numeric(TRANSECTS_SYDEMAN$SEG_ID)),sep="_")  


#Generate year	month	day	hours	minutes	seconds from the SEG_DATE_TIME_MID
library(chron)
TRANSECTS_SYDEMAN$year=TRANSECTS_SYDEMAN$LM_YEAR
TRANSECTS_SYDEMAN$month=TRANSECTS_SYDEMAN$LM_MONTH
TRANSECTS_SYDEMAN$day=TRANSECTS_SYDEMAN$LM_DAY
TRANSECTS_SYDEMAN$hours=TRANSECTS_SYDEMAN$LM_HOUR
TRANSECTS_SYDEMAN$minutes=TRANSECTS_SYDEMAN$LM_MIN
TRANSECTS_SYDEMAN$seconds=TRANSECTS_SYDEMAN$LM_SEC

#Add a platform column to distinguish from aerial surveys
TRANSECTS_SYDEMAN$platform="boat"

#Relabel the BEAUFORT to seaState
TRANSECTS_SYDEMAN$seaState=NA

#Convert to SEG_LENGTH in meters to segLengthKM in kilometers
TRANSECTS_SYDEMAN$segLengthKM=TRANSECTS_SYDEMAN$Length..m./1000

#Convert to WIDTH_SM in meters to segWidth_Sm_KM in kilometers
TRANSECTS_SYDEMAN$segWidth_Sm_KM=TRANSECTS_SYDEMAN$Width..m./1000

#Convert to WIDTH_LG in meters to segWidth_Lg_KM in kilometers
TRANSECTS_SYDEMAN$segWidth_Lg_KM=TRANSECTS_SYDEMAN$Width..m./1000

#Calculate longitude	latitude midpoints of each segment from end coordinates (LONG_5,LAT_5,LONG_6,LAT_6)
TRANSECTS_SYDEMAN$longitude=TRANSECTS_SYDEMAN$Longitude.Mid..º.
TRANSECTS_SYDEMAN$latitude=TRANSECTS_SYDEMAN$Latitude.Mid..º.

for(i in unique(TRANSECTS_SYDEMAN$GIS.key))
{#Compile text variable separated by commas of all SPP_CODE, SPP_NUMBER, and DISTANCE falling within each GIS.key
  TRANSECTS_SYDEMAN[TRANSECTS_SYDEMAN$GIS.key==i,
                    "SPP_CODE_ALL"]=paste(obs_SYDEMAN[obs_SYDEMAN$GIS.key==i
                                                               & !is.na(obs_SYDEMAN$Count),"Species"],collapse=",")
  TRANSECTS_SYDEMAN[TRANSECTS_SYDEMAN$GIS.key==i,
                    "SPP_NUMBER_ALL"]=paste(obs_SYDEMAN[obs_SYDEMAN$GIS.key==i
                                                                 & !is.na(obs_SYDEMAN$Count),"Count"],collapse=",")}


#Create a list object TRANSECTS_SYDEMAN_ALL composed of dataframes corresponding to each strip transect observation 
#in a given TRANSECT with associated LAT,LONG, and distance information. This list can be efficiently queried using lapply.
TRANSECTS_SYDEMAN_ALL=mapply(cbind, 
                             SPP_CODE=strsplit(TRANSECTS_SYDEMAN$SPP_CODE_ALL,","),
                             SPP_NUMBER=strsplit(TRANSECTS_SYDEMAN$SPP_NUMBER_ALL,","),
                             LONG_MID=TRANSECTS_SYDEMAN$Longitude.Mid..º.,
                             LAT_MID=TRANSECTS_SYDEMAN$Latitude.Mid..º.)

TRANSECTS_SYDEMAN_ALL=lapply(TRANSECTS_SYDEMAN_ALL,function(x) as.data.frame(x,stringsAsFactors=F))

SPP_CODES_SYDEMAN_temp = unique(SPP_CODES_SYDEMAN[SPP_CODES_SYDEMAN$Seabird. == 1,"Sydeman.Codes"])[unique(SPP_CODES_SYDEMAN[SPP_CODES_SYDEMAN$Seabird. == 1,"Sydeman.Codes"])%in%obs_SYDEMAN$Species]
SPP_CODES_SYDEMAN_temp = SPP_CODES_SYDEMAN_temp[SPP_CODES_SYDEMAN_temp%in%AOU_CODES$SPEC]

SPP_CODES_SYDEMAN_AMBIG_temp = unique(SPP_CODES_SYDEMAN[SPP_CODES_SYDEMAN$Seabird. == 1,"Sydeman.Codes"])[unique(SPP_CODES_SYDEMAN[SPP_CODES_SYDEMAN$Seabird. == 1,"Sydeman.Codes"])%in%obs_SYDEMAN$Species]
SPP_CODES_SYDEMAN_AMBIG_temp = SPP_CODES_SYDEMAN_AMBIG_temp[!SPP_CODES_SYDEMAN_AMBIG_temp%in%AOU_CODES$SPEC]

#Sum SPP_NUMBERs for different species within each TRANSECT_ID
for(i in SPP_CODES_SYDEMAN_temp)
{TRANSECTS_SYDEMAN[,i]=do.call(rbind, lapply(TRANSECTS_SYDEMAN_ALL,function(x) 
  sum(as.numeric(x[x$SPP_CODE%in%c(i),"SPP_NUMBER"]),na.rm=T)))}

#Sum SPP_NUMBERs for different species within each TRANSECT_ID
for(i in SPP_CODES_SYDEMAN_AMBIG_temp)
{TRANSECTS_SYDEMAN[,i]=do.call(rbind, lapply(TRANSECTS_SYDEMAN_ALL,function(x) 
  sum(as.numeric(x[x$SPP_CODE%in%c(i),"SPP_NUMBER"]),na.rm=T)))}

# # Data checks: Check whether the total number of seabirds in the TRANSECTS_SYDEMAN$PFSH_COUNT
# # column equals the sum of "PFSH" in obs_SYDEMAN
# sum(obs_SYDEMAN[obs_SYDEMAN$Species == "PFSH","Count"])
# # [1] 13882
# sum(TRANSECTS_SYDEMAN$PFSH)
# # [1] 13807
# sum(obs_SYDEMAN[obs_SYDEMAN$Species == "SOSH","Count"])
# # [1] 195016
# sum(TRANSECTS_SYDEMAN$SOSH)
# # [1] 194907
# 
# # These counts don't perfectly align because there are GIS.key codes in obs_SYDEMAN
# # that don't have corresponding GIS.key codes in TRANSECTS_SYDEMAN. That's ok, we can only work
# # with the data where we have full effort information (i.e., TRANSECTS_SYDEMAN)
# unique(obs_SYDEMAN$GIS.key)[which(!unique(obs_SYDEMAN$GIS.key)%in%unique(TRANSECTS_SYDEMAN$GIS.key))]

###### EXPORT_SYDEMAN ######

EXPORT_SYDEMAN = TRANSECTS_SYDEMAN[,c("surveyID","transectID","segmentID","year","month","day","hours","minutes","seconds",
                                      "platform","seaState","segLengthKM","segWidth_Sm_KM","segWidth_Lg_KM","longitude","latitude")]


# paste(paste("EXPORT_SYDEMAN$",SPP_CODES_SYDEMAN_temp," = TRANSECTS_SYDEMAN$",SPP_CODES_SYDEMAN_temp,sep = ""),collapse = " ")

EXPORT_SYDEMAN$ANMU = TRANSECTS_SYDEMAN$ANMU 
EXPORT_SYDEMAN$ARLO = TRANSECTS_SYDEMAN$ARLO 
EXPORT_SYDEMAN$ARTE = TRANSECTS_SYDEMAN$ARTE 
EXPORT_SYDEMAN$ASSP = TRANSECTS_SYDEMAN$ASSP 
EXPORT_SYDEMAN$BFAL = TRANSECTS_SYDEMAN$BFAL 
EXPORT_SYDEMAN$BLKI = TRANSECTS_SYDEMAN$BLKI 
EXPORT_SYDEMAN$BLSC = TRANSECTS_SYDEMAN$BLSC 
EXPORT_SYDEMAN$BLSK = TRANSECTS_SYDEMAN$BLSK 
EXPORT_SYDEMAN$BLSP = TRANSECTS_SYDEMAN$BLSP 
EXPORT_SYDEMAN$BLTE = TRANSECTS_SYDEMAN$BLTE 
EXPORT_SYDEMAN$BOGU = TRANSECTS_SYDEMAN$BOGU 
EXPORT_SYDEMAN$BRAC = TRANSECTS_SYDEMAN$BRAC 
EXPORT_SYDEMAN$BRBO = TRANSECTS_SYDEMAN$BRBO 
EXPORT_SYDEMAN$BRNO = TRANSECTS_SYDEMAN$BRNO 
EXPORT_SYDEMAN$BRPE = TRANSECTS_SYDEMAN$BRPE 
EXPORT_SYDEMAN$BULS = TRANSECTS_SYDEMAN$BULS 
EXPORT_SYDEMAN$BVSH = TRANSECTS_SYDEMAN$BVSH 
EXPORT_SYDEMAN$CAAU = TRANSECTS_SYDEMAN$CAAU 
EXPORT_SYDEMAN$CAGU = TRANSECTS_SYDEMAN$CAGU 
EXPORT_SYDEMAN$CATE = TRANSECTS_SYDEMAN$CATE 
EXPORT_SYDEMAN$CLGR = TRANSECTS_SYDEMAN$CLGR 
EXPORT_SYDEMAN$COLO = TRANSECTS_SYDEMAN$COLO 
EXPORT_SYDEMAN$COMU = TRANSECTS_SYDEMAN$COMU 
EXPORT_SYDEMAN$COPE = TRANSECTS_SYDEMAN$COPE 
EXPORT_SYDEMAN$COTE = TRANSECTS_SYDEMAN$COTE 
EXPORT_SYDEMAN$CRMU = TRANSECTS_SYDEMAN$CRMU 
EXPORT_SYDEMAN$DCCO = TRANSECTS_SYDEMAN$DCCO 
EXPORT_SYDEMAN$EAGR = TRANSECTS_SYDEMAN$EAGR 
EXPORT_SYDEMAN$ELTE = TRANSECTS_SYDEMAN$ELTE 
EXPORT_SYDEMAN$FFSH = TRANSECTS_SYDEMAN$FFSH 
EXPORT_SYDEMAN$FOTE = TRANSECTS_SYDEMAN$FOTE 
EXPORT_SYDEMAN$FRGU = TRANSECTS_SYDEMAN$FRGU 
EXPORT_SYDEMAN$FTSP = TRANSECTS_SYDEMAN$FTSP 
EXPORT_SYDEMAN$GLGU = TRANSECTS_SYDEMAN$GLGU 
EXPORT_SYDEMAN$GWGU = TRANSECTS_SYDEMAN$GWGU 
EXPORT_SYDEMAN$HAPE = TRANSECTS_SYDEMAN$HAPE 
EXPORT_SYDEMAN$HEEG = TRANSECTS_SYDEMAN$HEEG 
EXPORT_SYDEMAN$HERG = TRANSECTS_SYDEMAN$HERG 
EXPORT_SYDEMAN$HOPU = TRANSECTS_SYDEMAN$HOPU 
EXPORT_SYDEMAN$JFPE = TRANSECTS_SYDEMAN$JFPE 
EXPORT_SYDEMAN$KEGU = TRANSECTS_SYDEMAN$KEGU 
EXPORT_SYDEMAN$LAAL = TRANSECTS_SYDEMAN$LAAL 
EXPORT_SYDEMAN$LETE = TRANSECTS_SYDEMAN$LETE 
EXPORT_SYDEMAN$LTJA = TRANSECTS_SYDEMAN$LTJA 
EXPORT_SYDEMAN$MAMU = TRANSECTS_SYDEMAN$MAMU 
EXPORT_SYDEMAN$MEGU = TRANSECTS_SYDEMAN$MEGU 
EXPORT_SYDEMAN$MOPE = TRANSECTS_SYDEMAN$MOPE 
EXPORT_SYDEMAN$MUPE = TRANSECTS_SYDEMAN$MUPE 
EXPORT_SYDEMAN$NOFU = TRANSECTS_SYDEMAN$NOFU 
EXPORT_SYDEMAN$PAAU = TRANSECTS_SYDEMAN$PAAU 
EXPORT_SYDEMAN$PAJA = TRANSECTS_SYDEMAN$PAJA 
EXPORT_SYDEMAN$PALO = TRANSECTS_SYDEMAN$PALO 
EXPORT_SYDEMAN$PAPE = TRANSECTS_SYDEMAN$PAPE 
EXPORT_SYDEMAN$PECO = TRANSECTS_SYDEMAN$PECO 
EXPORT_SYDEMAN$PFSH = TRANSECTS_SYDEMAN$PFSH 
EXPORT_SYDEMAN$PIGU = TRANSECTS_SYDEMAN$PIGU 
EXPORT_SYDEMAN$POJA = TRANSECTS_SYDEMAN$POJA 
EXPORT_SYDEMAN$RBGU = TRANSECTS_SYDEMAN$RBGU 
EXPORT_SYDEMAN$RBME = TRANSECTS_SYDEMAN$RBME 
EXPORT_SYDEMAN$RBTR = TRANSECTS_SYDEMAN$RBTR 
EXPORT_SYDEMAN$REPH = TRANSECTS_SYDEMAN$REPH 
EXPORT_SYDEMAN$RFBO = TRANSECTS_SYDEMAN$RFBO 
EXPORT_SYDEMAN$RHAU = TRANSECTS_SYDEMAN$RHAU 
EXPORT_SYDEMAN$RNGR = TRANSECTS_SYDEMAN$RNGR 
EXPORT_SYDEMAN$RNPH = TRANSECTS_SYDEMAN$RNPH 
EXPORT_SYDEMAN$ROYT = TRANSECTS_SYDEMAN$ROYT 
EXPORT_SYDEMAN$RTLO = TRANSECTS_SYDEMAN$RTLO 
EXPORT_SYDEMAN$RTTR = TRANSECTS_SYDEMAN$RTTR 
EXPORT_SYDEMAN$SAGU = TRANSECTS_SYDEMAN$SAGU 
EXPORT_SYDEMAN$SCMU = TRANSECTS_SYDEMAN$SCMU 
EXPORT_SYDEMAN$SOSH = TRANSECTS_SYDEMAN$SOSH 
EXPORT_SYDEMAN$SPSK = TRANSECTS_SYDEMAN$SPSK 
EXPORT_SYDEMAN$STAL = TRANSECTS_SYDEMAN$STAL 
EXPORT_SYDEMAN$STPE = TRANSECTS_SYDEMAN$STPE 
EXPORT_SYDEMAN$SUSC = TRANSECTS_SYDEMAN$SUSC 
EXPORT_SYDEMAN$THGU = TRANSECTS_SYDEMAN$THGU 
EXPORT_SYDEMAN$TUPU = TRANSECTS_SYDEMAN$TUPU 
EXPORT_SYDEMAN$WEGR = TRANSECTS_SYDEMAN$WEGR 
EXPORT_SYDEMAN$WEGU = TRANSECTS_SYDEMAN$WEGU 
EXPORT_SYDEMAN$WISP = TRANSECTS_SYDEMAN$WISP 
EXPORT_SYDEMAN$WRSP = TRANSECTS_SYDEMAN$WRSP 
EXPORT_SYDEMAN$WTSH = TRANSECTS_SYDEMAN$WTSH 
EXPORT_SYDEMAN$WWSC = TRANSECTS_SYDEMAN$WWSC

# paste(paste("EXPORT_SYDEMAN$",SPP_CODES_SYDEMAN_AMBIG_temp," = TRANSECTS_SYDEMAN$",SPP_CODES_SYDEMAN_AMBIG_temp,sep = ""),collapse = " ")


EXPORT_SYDEMAN$SHSS = TRANSECTS_SYDEMAN$DKSH
EXPORT_SYDEMAN$PEDR = TRANSECTS_SYDEMAN$DRPE
EXPORT_SYDEMAN$KEPE = TRANSECTS_SYDEMAN$KRPE
EXPORT_SYDEMAN$LESP = TRANSECTS_SYDEMAN$LHSP
EXPORT_SYDEMAN$LSTP = TRANSECTS_SYDEMAN$LTSP
EXPORT_SYDEMAN$SRTS = TRANSECTS_SYDEMAN$SHOS
EXPORT_SYDEMAN$PESO = TRANSECTS_SYDEMAN$SOPE
EXPORT_SYDEMAN$CORM = TRANSECTS_SYDEMAN$UNCO
EXPORT_SYDEMAN$GREB = TRANSECTS_SYDEMAN$UNGR
EXPORT_SYDEMAN$LOON = TRANSECTS_SYDEMAN$UNLO
EXPORT_SYDEMAN$UNMU = TRANSECTS_SYDEMAN$UNMU
EXPORT_SYDEMAN$PHAL = TRANSECTS_SYDEMAN$UNPH
EXPORT_SYDEMAN$MUXC = TRANSECTS_SYDEMAN$XCMU
EXPORT_SYDEMAN$JAEG = TRANSECTS_SYDEMAN$UNJA 

# EXPORT_SYDEMAN$HYGU = TRANSECTS_SYDEMAN$HYGU 
# EXPORT_SYDEMAN$UNAL = TRANSECTS_SYDEMAN$UNAL 
# EXPORT_SYDEMAN$UNAU = TRANSECTS_SYDEMAN$UNAU 
# EXPORT_SYDEMAN$UNGU = TRANSECTS_SYDEMAN$UNGU 
# EXPORT_SYDEMAN$UNLA = TRANSECTS_SYDEMAN$UNLA 
# EXPORT_SYDEMAN$UNPC = TRANSECTS_SYDEMAN$UNPC 
# EXPORT_SYDEMAN$UNPE = TRANSECTS_SYDEMAN$UNPE 
# EXPORT_SYDEMAN$UNSA = TRANSECTS_SYDEMAN$UNSA 
# EXPORT_SYDEMAN$UNSH = TRANSECTS_SYDEMAN$UNSH 
# EXPORT_SYDEMAN$UNSP = TRANSECTS_SYDEMAN$UNSP 
# EXPORT_SYDEMAN$UNTE = TRANSECTS_SYDEMAN$UNTE 
# EXPORT_SYDEMAN$UNTR = TRANSECTS_SYDEMAN$UNTR 


#Add to EXPORT columns of 0's for any columns (i.e., species names) that exist in EXPORT_SYDEMAN but not in EXPORT
EXPORT[,colnames(EXPORT_SYDEMAN)[!colnames(EXPORT_SYDEMAN)%in%colnames(EXPORT)]]=0

#Add columns (i.e., species names) missing from EXPORT_SYDEMAN header that are found in EXPORT
EXPORT_SYDEMAN[,colnames(EXPORT)[!colnames(EXPORT)%in%colnames(EXPORT_SYDEMAN)]]=0

#Append EXPORT_SYDEMAN to EXPORT
EXPORT=rbind(EXPORT,EXPORT_SYDEMAN)



######### QA/QC CHECKS ########

#Check for NAs in final table
summary(EXPORT[,1:16])
summary(EXPORT[,17:168])

#Duplication of SEABIRD_CODES
View(SEABIRD_CODES[order(SEABIRD_CODES$SEQ_ID),])
duplicated(SEABIRD_CODES[order(SEABIRD_CODES$SEQ_ID),"MasterCode"])

#Any codes in EXPORT not defined in SEABIRD_CODES?
colnames(EXPORT[,17:168])%in%SEABIRD_CODES$MasterCode

# # Remove non-seabird SPP_CODE columns from EXPORT
# 
# # paste(SEABIRD_CODES[SEABIRD_CODES$MasterCode %in%c(c("BLOY","BLTU","CAEG","GBHE","GREG","MAGO","OSPR",
# #                                                      "PEFA","RODO","SAND","SNEG","UNCU","UNEG","UNLW",
# #                                                      "UNMW","UNSW","WHIM","WILL")), "COMMON_NAME"], collapse = ", ")
# # Black Oystercatcher, Black Turnstone, Cattle Egret, Great Blue Heron, Great Egret, 
# # Marbled Godwit, Osprey, Peregrine Falcon, Rock Dove, Sanderling, Snowy Egret, 
# # Unidentified curlew spp. (e.g. whimbrel or long-billed), Unidentified egret, 
# # Unidentified large wader, Unidentified medium wader, Unidentified small wader, 
# # Whimbrel, Willet
# 
# EXPORT <- subset(EXPORT,select = -c(BLOY,BLTU,CAEG,GBHE,GREG,MAGO,OSPR,PEFA,RODO,
#                                     SAND,SNEG,UNCU,UNEG,UNLW,UNMW,UNSW,WHIM,WILL))
# 
# 
# # Remove overly general SPP_CODE columns from EXPORT
# 
# # paste(SEABIRD_CODES[SEABIRD_CODES$MasterCode %in%c("SHSP","PTSP","SPSP","GULL","TERN","ALCD",
# #                                                    "UNSG","UNPR","UDIV","UNLG","UNLT","UNLA",
# #                                                    "UNMG","UNMT","UNSB","USDV","UNST"), "COMMON_NAME"], collapse = ", ")
# # unid. shearwater, unid. Pterodroma, unid. storm-petrel, 
# # unid. gull, unid. tern, unid. alcid, unid. Procellarid, 
# # Unidentified diver , Unidentified large gull, 
# # Unidentified large tern (e.g., Caspian Tern, Elegant Tern) , 
# # Unidentified Lariidae includes gulls and terns , Unidentified medium gull, 
# # Unidentified medium tern , Unidentified seabird, Unidentified small diver, Unidentified small gull, 
# # Unidentified small tern (e.g., Least Tern, Forster\x90s Tern) 
# 
# EXPORT <- subset(EXPORT,select = -c(SHSP,PTSP,SPSP,GULL,TERN,ALCD,UNSG,UNPR,UDIV,UNLG,
#                                     UNLT,UNLA,UNMG,UNMT,UNSB,USDV,UNST))


########## GRP_CODES ####### 

#GroupID_Pacific_Red_throated_loons
#SEABIRD_CODES[SEABIRD_CODES$GRP_CODE%in%c("GRP_RTLO_PALO"),c("MasterCode","COMMON_NAME")]
#MasterCode       COMMON_NAME
#RTLO             Red-throated Loon
#PALO             Pacific Loon
EXPORT$GRP_RTLO_PALO = EXPORT$RTLO + EXPORT$PALO


#GroupID_Shearwaters_Dark
#SEABIRD_CODES[SEABIRD_CODES$GRP_CODE%in%c("GRP_SOSH_SRTS_FFSH"),c("MasterCode","COMMON_NAME")]
#MasterCode       COMMON_NAME
#FFSH                  Flesh-footed Shearwater
#SOSH                         Sooty Shearwater
#SRTS Slender-billed (Short-tailed) Shearwater
#SHSS          Sooty/Slender-billed Shearwater
EXPORT$GRP_SOSH_SRTS_FFSH = EXPORT$SOSH + EXPORT$SRTS + EXPORT$FFSH + EXPORT$SHSS


#GroupID_Leachs_Ashy_Storm_Petrel (plus Least Storm Petrel, White-rumped storm-petrel, Dark-rumped storm-petrel, Leach's/Harcourt's Storm-Petrel)
#SEABIRD_CODES[SEABIRD_CODES$GRP_CODE%in%c("GRP_LESP_ASSP_LSTP"),c("MasterCode","COMMON_NAME")]
#MasterCode       COMMON_NAME
#LESP              Leach's Storm-Petrel
#ASSP                 Ashy Storm-Petrel
#LSTP                Least Storm-Petrel
#LESP white-rumped Leach's Storm-Petrel
#LESP  dark-rumped Leach's Storm-Petrel
#SPLH   Leach's/Harcourt's Storm-Petrel
#SPWR         White-rumped storm-petrel
#SPDR          Dark-rumped storm-petrel
EXPORT$GRP_LESP_ASSP_LSTP = EXPORT$LESP + EXPORT$ASSP + EXPORT$LSTP + EXPORT$SPWR + EXPORT$SPDR + EXPORT$SPLH

#GroupID_Cormorants
#SEABIRD_CODES[SEABIRD_CODES$GRP_CODE%in%c("GRP_CORM"),c("MasterCode","COMMON_NAME")]
#MasterCode       COMMON_NAME
#CORM          unid. cormorant
#DCCO Double-crested Cormorant
#BRAC       Brandt's Cormorant
#PECO        Pelagic Cormorant
EXPORT$GRP_CORM = EXPORT$DCCO + EXPORT$BRAC + EXPORT$PECO + EXPORT$CORM

#GroupID_Phalaropes
#SEABIRD_CODES[SEABIRD_CODES$GRP_CODE%in%c("GRP_PHAL"),c("MasterCode","COMMON_NAME")]
#MasterCode       COMMON_NAME
#PHAL                 unid. phalarope
#REPH                   Red Phalarope
#RNPH Red-necked (Northern) Phalarope
EXPORT$GRP_PHAL = EXPORT$REPH + EXPORT$RNPH + EXPORT$PHAL

#GroupID_Parasitic_Long_tailed_jaegers (plus Pomarine Jaeger)
#SEABIRD_CODES[SEABIRD_CODES$GRP_CODE%in%c("GRP_JAEG"),c("MasterCode","COMMON_NAME")]
#MasterCode       COMMON_NAME
#JAEG                 unid. jaeger
#POJA              Pomarine Jaeger
#PAJA             Parasitic Jaeger
#LTJA           Long-tailed Jaeger
#JAPL Parasitic/Long-tailed Jaeger
EXPORT$GRP_JAEG = EXPORT$POJA + EXPORT$PAJA + EXPORT$LTJA + EXPORT$JAPL + EXPORT$JAEG

#GroupID_Herring_Thayers_Gulls
#SEABIRD_CODES[SEABIRD_CODES$GRP_CODE%in%c("GRP_HERG_THGU"),c("MasterCode","COMMON_NAME")]
#MasterCode   COMMON_NAME
#HERG  Herring Gull
#THGU Thayer's Gull
#MasterCode       COMMON_NAME
EXPORT$GRP_HERG_THGU = EXPORT$HERG + EXPORT$THGU

#GroupID_Western_Glaucous_winged_Gulls
#SEABIRD_CODES[SEABIRD_CODES$GRP_CODE%in%c("GRP_WEGU_GWGU_WGWH"),c("MasterCode","COMMON_NAME")]
#MasterCode       COMMON_NAME
#WEGU                          Western Gull
#GWGU                  Glaucous-winged Gull
#WGWH Western x Glaucous-winged Gull hybrid
EXPORT$GRP_WEGU_GWGU_WGWH = EXPORT$WEGU + EXPORT$GWGU + EXPORT$WGWH

#GroupID_Common_Arctic_Terns
#SEABIRD_CODES[SEABIRD_CODES$GRP_CODE%in%c("GRP_COTE_ARTE_TEAC"),c("MasterCode","COMMON_NAME")]
#MasterCode       COMMON_NAME
#COTE        Common Tern
#ARTE        Arctic Tern
#TEAC Arctic/Common Tern
EXPORT$GRP_COTE_ARTE_TEAC = EXPORT$COTE + EXPORT$ARTE + EXPORT$TEAC

#GroupID_Elegant_Royal_Terns (plus Caspian Tern and Forster's Tern)
#SEABIRD_CODES[SEABIRD_CODES$GRP_CODE%in%c("GRP_ROYT_ELTE_CATE_FOTE"),c("MasterCode","COMMON_NAME")]
#MasterCode       COMMON_NAME
#CATE   Caspian Tern
#FOTE Forster's Tern
#ROYT     Royal Tern
#ELTE   Elegant Tern
#ERTE
EXPORT$GRP_ROYT_ELTE_CATE_FOTE = EXPORT$ROYT + EXPORT$ELTE  + EXPORT$CATE + EXPORT$FOTE # + EXPORT$ERTE #No occurrence of ERTE code except in USGS data not included in this dataset

#GroupID_Scoters
#SEABIRD_CODES[SEABIRD_CODES$GRP_CODE%in%c("GRP_SUSC_WWSC_BLSC"),c("MasterCode","COMMON_NAME")]
#MasterCode       COMMON_NAME
#BLSC        Black Scoter
#SUSC         Surf Scoter
#WWSC White winged Scoter
#USCR Unidentified scoter 

EXPORT$GRP_SUSC_WWSC_BLSC = EXPORT$SUSC + EXPORT$WWSC + EXPORT$BLSC + EXPORT$USCR

#GroupID_Western_Clarks_Grebes
#SEABIRD_CODES[SEABIRD_CODES$GRP_CODE%in%c("GRP_WEGR_WCGR_CLGR"),c("MasterCode","COMMON_NAME")]
#MasterCode       COMMON_NAME
#CLGR   Clark's Grebe
#WEGR   Western Grebe
#WCGR Western/Clark's
EXPORT$GRP_WEGR_WCGR_CLGR = EXPORT$WEGR + EXPORT$WCGR + EXPORT$CLGR

#Group Cookalaria Petrels
#SEABIRD_CODES[SEABIRD_CODES$GRP_CODE%in%c("GRP_COOK"),c("MasterCode","COMMON_NAME")]
#MasterCode       COMMON_NAME
#PESC Stejnegers/Cook's Petrel
#COPE            Cook's Petrel
#STPE       Stejneger's Petrel
#COOK         unid. Cookilaria
EXPORT$GRP_COOK = EXPORT$COPE + EXPORT$STPE + EXPORT$COOK #+ EXPORT$PESC #no instances of PESC in data

#Group Solander's and Murphy's Petrels
#SEABIRD_CODES[SEABIRD_CODES$GRP_CODE%in%c("GRP_MUPE_PESM"),c("MasterCode","COMMON_NAME")]
#MasterCode       COMMON_NAME
#PESM Solander's/Murphy's Petrel
#MUPE            Murphy's Petrel
EXPORT$GRP_MUPE_PESM = EXPORT$MUPE #+ EXPORT$PESM #no instances of PESM in data

#Group Hawaiian and Dark-Rumped Petrels
#SEABIRD_CODES[SEABIRD_CODES$GRP_CODE%in%c("GRP_HAPE_PEDR"),c("MasterCode","COMMON_NAME")]
#MasterCode       COMMON_NAME
#PEDR Dark-rumped Petrel
#HAPE    Hawaiian Petrel
EXPORT$GRP_HAPE_PEDR = EXPORT$HAPE + EXPORT$PEDR

#Group Xantus's (now Scripps'/Guadalupe) and Craveri's Murrelets
#SEABIRD_CODES[SEABIRD_CODES$GRP_CODE%in%c("GRP_MUXA_CRMU_MUXC"),c("MasterCode","COMMON_NAME")]
#MasterCode       COMMON_NAME
#MUXA          Xantus's Murrelet
#CRMU         Craveri's Murrelet
#MUXC Xantus'/Craveri's Murrelet
#SCMU          Scripps's murrelet
#SGMU Scripps'/Guadalupe Murrelet
EXPORT$GRP_MUXA_CRMU_MUXC = EXPORT$MUXA + EXPORT$CRMU + EXPORT$MUXC + EXPORT$SCMU #+ EXPORT$SGMU #no instances of SGMU in data

#Group Marbled and Kittlitz's Murrelets
#SEABIRD_CODES[SEABIRD_CODES$GRP_CODE%in%c("GRP_MAMU_KIMU"),c("MasterCode","COMMON_NAME")]
#MasterCode       COMMON_NAME
#MAMU    Marbled Murrelet
#KIMU Kittlitz's Murrelet
EXPORT$GRP_MAMU_KIMU = EXPORT$MAMU + EXPORT$KIMU

#Group Horned, Eared and Pied Billed Grebes
#SEABIRD_CODES[SEABIRD_CODES$GRP_CODE%in%c("GRP_HOGR_PBGR_USGR"),c("MasterCode","COMMON_NAME")]
#MasterCode       COMMON_NAME
#HOGR             Horned Grebe
#PBGR             Pied-billed Grebe
#USGR             Unidentified small grebe
#EXPORT$GRP_HOGR_PBGR_USGR = EXPORT$HOGR + EXPORT$PBGR + EXPORT$USGR #No occurrence of HOGR, PBGR, and USGR code except in USGS data not included in this version dataset


#Write .csv datafile combining segmented SWFSC, PaCSEA or SoCA datasets
write.csv(EXPORT,"/Users/trevorjoyce/Grad School/Research/2_Pacific Marine Bird Modeling/CCE_SEABIRDS_2.4KM_SEGMENTS_GROUPED_v0.9.csv")


# #Write .csv file with SEABIRD_CODES actually used
# write.csv(SEABIRD_CODES[SEABIRD_CODES$MasterCode%in%colnames(EXPORT),c("SEQ_ID","MasterCode","MasterCode_Flag","Size","COMMON_NAME","AOU_CODE",
#                                                                        "SPP_CODE","SoCA.Codes","Ainley.Code","Ainley.Code.Secondary","SYDEMAN_SPP_CODE",
#                                                                        "POINT_BLUE_SPP_CODE","ZAMON_SPP_CODE","OCNMS_SPP_CODE","W2W_SPP_CODE","GRP_CODE","GroupID")],
#           "/Users/trevorjoyce/Grad School/Research/2_Pacific Marine Bird Modeling/SEABIRD_CODES_SIMPLE.csv")

