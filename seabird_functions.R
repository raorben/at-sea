#####  FLOCK_COMBINE  #####
#Concatenates daily flock files in a specified directory (DIR) into a single file defined by the OUTPUT
FLOCK_COMBINE=function(DIR,OUTPUT)
{FLOCK_FILES=data.frame(FILE_NAMES=list.files(DIR),DUMMY=1)
FLOCK_FILES=FLOCK_FILES[substr(FLOCK_FILES$FILE_NAMES,1,1)=="F",]
file.create(OUTPUT)
for(i in 1:nrow(FLOCK_FILES))
{file.append(OUTPUT,paste(DIR,FLOCK_FILES$FILE_NAMES[i],sep="/"))}
}   
#Example: FLOCK_COMBINE(DIR="~/Grad School/Research/1_2013_Seabird and Flock Analyses/Raw Data/Flock Data/Yearly_Archives/2002/dsj/Leg 1 flock files",
#                       OUTPUT="~/Desktop/Test.txt")

#Create 2014_1647_OST_FLCK data file from daily files (some manual editing required to deal with differences in end row (0-2 blank rows)
#FLOCK_COMBINE(DIR="~/Grad School/Research/1_2013_Seabird and Flock Analyses/Raw Data/Flock Data/Yearly_Archives/2014/Leg 1/flock files edited",
#              OUTPUT="~/Grad School/Research/1_2013_Seabird and Flock Analyses/Raw Data/Flock Data/Yearly_Archives/2014/2014_1647_OST_FLCK_Leg1.txt")
#FLOCK_COMBINE(DIR="~/Grad School/Research/1_2013_Seabird and Flock Analyses/Raw Data/Flock Data/Yearly_Archives/2014/Leg 2/flock files edited",
#              OUTPUT="~/Grad School/Research/1_2013_Seabird and Flock Analyses/Raw Data/Flock Data/Yearly_Archives/2014/2014_1647_OST_FLCK_Leg2.txt")
#FLOCK_COMBINE(DIR="~/Grad School/Research/1_2013_Seabird and Flock Analyses/Raw Data/Flock Data/Yearly_Archives/2014/Leg 3/flock files edited",
#              OUTPUT="~/Grad School/Research/1_2013_Seabird and Flock Analyses/Raw Data/Flock Data/Yearly_Archives/2014/2014_1647_OST_FLCK_Leg3.txt")
#FLOCK_COMBINE(DIR="~/Grad School/Research/1_2013_Seabird and Flock Analyses/Raw Data/Flock Data/Yearly_Archives/2014/Leg 4/flock files edited",
#              OUTPUT="~/Grad School/Research/1_2013_Seabird and Flock Analyses/Raw Data/Flock Data/Yearly_Archives/2014/2014_1647_OST_FLCK_Leg4.txt")
#FLOCK_COMBINE(DIR="~/Grad School/Research/1_2013_Seabird and Flock Analyses/Raw Data/Flock Data/Yearly_Archives/2014/Leg 5/flock files edited",
#              OUTPUT="~/Grad School/Research/1_2013_Seabird and Flock Analyses/Raw Data/Flock Data/Yearly_Archives/2014/2014_1647_OST_FLCK_Leg5.txt")
#file.create("~/Grad School/Research/1_2013_Seabird and Flock Analyses/Raw Data/Flock Data/2014_1647_OST_FLCK.txt")
#file.append("~/Grad School/Research/1_2013_Seabird and Flock Analyses/Raw Data/Flock Data/2014_1647_OST_FLCK.txt",
#            paste("~/Grad School/Research/1_2013_Seabird and Flock Analyses/Raw Data/Flock Data/Yearly_Archives/2014/2014_1647_OST_FLCK_Leg",c(1:5),".txt",sep=""))



##### SEABIRD_COMBINE  #####
#Concatenates daily flock files in a specified directory (DIR) into a single file defined by the OUTPUT
SEABIRD_COMBINE=function(DIR,OUTPUT)
{SEABIRD_FILES=data.frame(FILE_NAMES=list.files(DIR),DUMMY=1)
SEABIRD_FILES=SEABIRD_FILES[substr(SEABIRD_FILES$FILE_NAMES,1,1)=="S",]
file.create(OUTPUT)
for(i in 1:nrow(SEABIRD_FILES))
{file.append(OUTPUT,paste(DIR,SEABIRD_FILES$FILE_NAMES[i],sep="/"))}
}   
#Example: SEABIRD_COMBINE(DIR="~/Grad School/Research/1_2013_Seabird and Flock Analyses/Raw Data/Flock Data/Yearly_Archives/2002/dsj/Leg 1 flock files",
#                       OUTPUT="~/Desktop/Test.txt")

#Create 2014_1647_OST_FLCK data file from daily files (some manual editing required to deal with differences in end row (0-2 blank rows)
#SEABIRD_COMBINE(DIR="~/Grad School/Research/1_2013_Seabird and Flock Analyses/Raw Data/Strip Transect Data/Yearly_Archives/2014/Leg 1/seabird files edited",
#              OUTPUT="~/Grad School/Research/1_2013_Seabird and Flock Analyses/Raw Data/Strip Transect Data/Yearly_Archives/2014/1647_CalCurCEAS_2014_OST_Leg1.txt")
#SEABIRD_COMBINE(DIR="~/Grad School/Research/1_2013_Seabird and Flock Analyses/Raw Data/Strip Transect Data/Yearly_Archives/2014/Leg 2/seabird files edited",
#                OUTPUT="~/Grad School/Research/1_2013_Seabird and Flock Analyses/Raw Data/Strip Transect Data/Yearly_Archives/2014/1647_CalCurCEAS_2014_OST_Leg2.txt")
#SEABIRD_COMBINE(DIR="~/Grad School/Research/1_2013_Seabird and Flock Analyses/Raw Data/Strip Transect Data/Yearly_Archives/2014/Leg 3/seabird files edited",
#                OUTPUT="~/Grad School/Research/1_2013_Seabird and Flock Analyses/Raw Data/Strip Transect Data/Yearly_Archives/2014/1647_CalCurCEAS_2014_OST_Leg3.txt")
#SEABIRD_COMBINE(DIR="~/Grad School/Research/1_2013_Seabird and Flock Analyses/Raw Data/Strip Transect Data/Yearly_Archives/2014/Leg 4/seabird files edited",
#                OUTPUT="~/Grad School/Research/1_2013_Seabird and Flock Analyses/Raw Data/Strip Transect Data/Yearly_Archives/2014/1647_CalCurCEAS_2014_OST_Leg4.txt")
#SEABIRD_COMBINE(DIR="~/Grad School/Research/1_2013_Seabird and Flock Analyses/Raw Data/Strip Transect Data/Yearly_Archives/2014/Leg 5/seabird files edited",
#                OUTPUT="~/Grad School/Research/1_2013_Seabird and Flock Analyses/Raw Data/Strip Transect Data/Yearly_Archives/2014/1647_CalCurCEAS_2014_OST_Leg5.txt")
#file.create("~/Grad School/Research/1_2013_Seabird and Flock Analyses/Raw Data/Strip Transect Data/1647_CalCurCEAS_2014_OST.txt")
#file.append("~/Grad School/Research/1_2013_Seabird and Flock Analyses/Raw Data/Strip Transect Data/1647_CalCurCEAS_2014_OST.txt",
#            paste("~/Grad School/Research/1_2013_Seabird and Flock Analyses/Raw Data/Strip Transect Data/Yearly_Archives/2014/1647_CalCurCEAS_2014_OST_Leg",c(1:5),".txt",sep=""))



#####  MARMAM_HORIZ  #####
#processing function to merge information stored in individual rows to associated rows 
#within days (B), transects (R,E), and sightings (S,A)
MARMAM_HORIZ=function(INPUT,OUTPUT=NA)
{require(chron)
  MAM_DATA=as.data.frame(INPUT)
  MAM_DATA=subset(MAM_DATA,select=-c(v1,v3,v4))
  #Sequential ID to reestablish the original order after merges
  MAM_DATA$SEQ_ID=seq(1:nrow(MAM_DATA))
  #Convert the "." to a binomial on or off effort 
  MAM_DATA$EFFORT=ifelse(MAM_DATA$EFFORT==".",1,0) 
  
  #convert numeric 1-2 digit year into 4 digit year
  MAM_DATA$LM_YEAR=as.numeric(ifelse(MAM_DATA$LM_YEAR>70,
                                     paste("19",sprintf("%02d",as.integer(MAM_DATA$LM_YEAR)),sep=""),
                                     paste("20",sprintf("%02d",as.integer(MAM_DATA$LM_YEAR)),sep=""))) 
  #copy down the date,time, and position values from above where missing i.e. EVENT_CODE==1,2,3,4,5,?)
  for(i in 2:nrow(MAM_DATA))
  {if(is.na(MAM_DATA[i,"LM_YEAR"])|is.na(MAM_DATA[i,"LM_HOUR"])) 
  {MAM_DATA[i,c("LM_YEAR","LM_MONTH","LM_DAY","LM_HOUR","LM_MIN","LM_SEC",
                "LAT_HEMI","LAT_DEG","LAT_MIN","LONG_HEMI","LONG_DEG","LONG_MIN")]=
    MAM_DATA[i-1,c("LM_YEAR","LM_MONTH","LM_DAY","LM_HOUR","LM_MIN","LM_SEC",
                   "LAT_HEMI","LAT_DEG","LAT_MIN","LONG_HEMI","LONG_DEG","LONG_MIN")]}} 
  #convert NAs in the seconds (where time collected only to min precision) to zero
  MAM_DATA[is.na(MAM_DATA$LM_SEC),"LM_SEC"]=0 
  #calculate Julian date/time in format of MS Excel (v.Windows 2010)
  MAM_DATA$LOCAL_DATE_TIME=julian(MAM_DATA$LM_MONTH,MAM_DATA$LM_DAY,MAM_DATA$LM_YEAR,
                                  origin=c(month = 12, day = 30, year = 1899),dec=5)+
    MAM_DATA$LM_HOUR/24+
    MAM_DATA$LM_MIN/(60*24)+
    MAM_DATA$LM_SEC/(60*60*24) 
  
  #Calculate local date in Julian format
  MAM_DATA$LOCAL_DAY=floor(MAM_DATA$LOCAL_DATE_TIME) 
  #Assign the Cruise number from the file name
  MAM_DATA$CRUISE=as.numeric(MAM_DATA[MAM_DATA$EVENT_CODE=="B","DATA1"][1])
  #create CRUISE_DAY code
  MAM_DATA$CRUISE_DAY=paste(MAM_DATA$CRUISE,"_",MAM_DATA$LOCAL_DAY,sep="") 
  
  #convert from degrees decimal minutes and N-S to -90-90 decimal degrees
  MAM_DATA$LAT=ifelse(MAM_DATA$LAT_HEMI==" N",MAM_DATA$LAT_DEG +(MAM_DATA$LAT_MIN/60),
                      -1*(MAM_DATA$LAT_DEG +(MAM_DATA$LAT_MIN/60))) 
  #convert from degrees decimal minutes and E-W to -180-180 decimal degrees
  MAM_DATA$LONG=ifelse(MAM_DATA$LONG_HEMI==" E",MAM_DATA$LONG_DEG +(MAM_DATA$LONG_MIN/60),
                       -1*(MAM_DATA$LONG_DEG +(MAM_DATA$LONG_MIN/60))) 
  #convert from -180:180 to 0:360 reference system (better for displaying Pacific)
  MAM_DATA$LONG=ifelse(MAM_DATA$LONG<0,360+MAM_DATA$LONG,MAM_DATA$LONG)
  
  MAM_DATA$TRANSECT_ID=NA
  MAM_DATA[MAM_DATA$EVENT_CODE=="R","TRANSECT_ID"]=seq(1,nrow(MAM_DATA[MAM_DATA$EVENT_CODE=="R",]))
  for(i in 2:nrow(MAM_DATA))
  {if(is.na(MAM_DATA$TRANSECT_ID[i]) & MAM_DATA$EVENT_CODE[i-1]!="E" & MAM_DATA[i,"LOCAL_DAY"]==MAM_DATA[i-1,"LOCAL_DAY"])
  {MAM_DATA$TRANSECT_ID[i]=MAM_DATA$TRANSECT_ID[i-1]}}
  
  #assign a unique number to each transect (starting with EVENT_CODE=R) within a CRUISE_DAY 
  MAM_DATA$T_CODE_NUM=1
  for(i in 2:nrow(MAM_DATA))
  {if(MAM_DATA[i,"LOCAL_DAY"]!=MAM_DATA[i-1,"LOCAL_DAY"])
    MAM_DATA[i,"T_CODE_NUM"]=1
  else  
    if(MAM_DATA[i,"EVENT_CODE"]=="R") 
      MAM_DATA[i,"T_CODE_NUM"]=MAM_DATA[i-1,"T_CODE_NUM"]+1 
    else
      MAM_DATA[i,"T_CODE_NUM"]=MAM_DATA[i-1,"T_CODE_NUM"]} 
  
  #Move T_CODE_NUM back by 1 to include EVENT_CODE=B row in the first transect of the day 
  MAM_DATA$T_CODE_NUM=ifelse(MAM_DATA$T_CODE_NUM==1,MAM_DATA$T_CODE_NUM,MAM_DATA$T_CODE_NUM-1)
  #create TRANSECT_CODE identifier from CRUISE_DAY and T_CODE_NUM
  MAM_DATA$TRANSECT_CODE=paste(MAM_DATA$CRUISE_DAY,"_",sprintf("%02d",MAM_DATA$T_CODE_NUM),sep="") 
  
  
  MAM_DATA$T_CODE_NUM=1
  for(i in 2:nrow(MAM_DATA))
  {if(MAM_DATA[i,"LOCAL_DAY"]!=MAM_DATA[i-1,"LOCAL_DAY"])
    MAM_DATA[i,"T_CODE_NUM"]=1
  else  
    if(MAM_DATA[i,"EVENT_CODE"]=="R") 
      MAM_DATA[i,"T_CODE_NUM"]=MAM_DATA[i-1,"T_CODE_NUM"]+1 
    else
      MAM_DATA[i,"T_CODE_NUM"]=MAM_DATA[i-1,"T_CODE_NUM"]} 
  
  #Create a table of the EVENT_CODE="B" information and merge to all records within a unique CRUISE_DAY
  MAM_DATA_B=MAM_DATA[MAM_DATA$EVENT_CODE=="B",c("CRUISE_DAY","DATA3")]
  colnames(MAM_DATA_B)=c("CRUISE_DAY","GM_OFFSET")
  MAM_DATA_B$GM_OFFSET=as.numeric(MAM_DATA_B$GM_OFFSET)
  MAM_DATA_B=MAM_DATA_B[!duplicated(MAM_DATA_B$CRUISE_DAY),] 
  MAM_DATA=merge(MAM_DATA, MAM_DATA_B,by="CRUISE_DAY",all.x=TRUE)
  MAM_DATA=MAM_DATA[order(MAM_DATA$SEQ_ID),]
  
  #Create a table of the EVENT_CODE="P" (personnel) information at the start of each transect and
  #merge to all records within a uniquely identified TRANSECT_ID
  MAM_DATA_P=MAM_DATA[MAM_DATA$EVENT_CODE=="P" & !is.na(MAM_DATA$TRANSECT_ID),c("TRANSECT_ID","DATA1","DATA2","DATA3")]
  colnames(MAM_DATA_P)=c("TRANSECT_ID","LEFT_OBS","RECORDER","RIGHT_OBS")
  MAM_DATA_P$LEFT_OBS=as.numeric(MAM_DATA_P$LEFT_OBS) 
  MAM_DATA_P$RECORDER=as.numeric(MAM_DATA_P$RECORDER) 
  MAM_DATA_P$RIGHT_OBS=as.numeric(MAM_DATA_P$RIGHT_OBS)
  MAM_DATA_P=MAM_DATA_P[!duplicated(MAM_DATA_P$TRANSECT_ID),] 
  MAM_DATA=merge(MAM_DATA, MAM_DATA_P,by="TRANSECT_ID",all.x=TRUE)
  MAM_DATA=MAM_DATA[order(MAM_DATA$SEQ_ID),]
  
  #Create a table of the EVENT_CODE="N" (navigation) information at the start of each transect and
  #merge to all records within a uniquely identified TRANSECT_ID
  MAM_DATA_N=MAM_DATA[MAM_DATA$EVENT_CODE=="N" & !is.na(MAM_DATA$TRANSECT_ID),c("TRANSECT_ID","DATA1","DATA2")] 
  colnames(MAM_DATA_N)=c("TRANSECT_ID","COURSE","SPEED")
  MAM_DATA_N$COURSE=as.numeric(MAM_DATA_N$COURSE)
  MAM_DATA_N$SPEED=as.numeric(MAM_DATA_N$SPEED)
  MAM_DATA_N=MAM_DATA_N[!duplicated(MAM_DATA_N$TRANSECT_ID),] 
  MAM_DATA=merge(MAM_DATA, MAM_DATA_N,by="TRANSECT_ID",all.x=TRUE)
  MAM_DATA=MAM_DATA[order(MAM_DATA$SEQ_ID),]
  
  #Create a table of the EVENT_CODE="V" (visibility) information at the start of each transect and
  #merge to all records within a uniquely identified TRANSECT_ID
  MAM_DATA_V=MAM_DATA[MAM_DATA$EVENT_CODE=="V" & !is.na(MAM_DATA$TRANSECT_ID),c("TRANSECT_ID","DATA1","DATA2","DATA3","DATA4")]
  colnames(MAM_DATA_V)=c("TRANSECT_ID","BEAUFORT","SW_HGT","SW_DIR","WIND_VEL")
  MAM_DATA_V$BEAUFORT=as.numeric(MAM_DATA_V$BEAUFORT) 
  MAM_DATA_V$SW_HGT=as.numeric(MAM_DATA_V$SW_HGT) 
  MAM_DATA_V$SW_DIR=as.numeric(MAM_DATA_V$SW_DIR) 
  MAM_DATA_V$WIND_VEL=as.numeric(MAM_DATA_V$WIND_VEL) 
  MAM_DATA_V=MAM_DATA_V[!duplicated(MAM_DATA_V$TRANSECT_ID),] 
  MAM_DATA=merge(MAM_DATA, MAM_DATA_V,by="TRANSECT_ID",all.x=TRUE)
  MAM_DATA=MAM_DATA[order(MAM_DATA$SEQ_ID),]
  
  #Create a table of the EVENT_CODE="W" (weather) information at the start of each transect and
  #merge to all records within a uniquely identified TRANSECT_ID
  MAM_DATA_W=MAM_DATA[MAM_DATA$EVENT_CODE=="W" & !is.na(MAM_DATA$TRANSECT_ID),c("TRANSECT_ID","DATA1","DATA2","DATA3","DATA4","DATA5")]
  colnames(MAM_DATA_W)=c("TRANSECT_ID","RAIN_FOG","HORIZ_SUN","VERT_SUN","WIND_DIR","VIS")
  MAM_DATA_W$RAIN_FOG=as.numeric(MAM_DATA_W$RAIN_FOG) 
  MAM_DATA_W$HORIZ_SUN=as.numeric(MAM_DATA_W$HORIZ_SUN) 
  MAM_DATA_W$VERT_SUN=as.numeric(MAM_DATA_W$VERT_SUN) 
  MAM_DATA_W$WIND_DIR=as.numeric(MAM_DATA_W$WIND_DIR) 
  MAM_DATA_W$VIS=as.numeric(MAM_DATA_W$VIS)
  MAM_DATA_W=MAM_DATA_W[!duplicated(MAM_DATA_W$TRANSECT_ID),]
  MAM_DATA=merge(MAM_DATA, MAM_DATA_W,by="TRANSECT_ID",all.x=TRUE)
  MAM_DATA=MAM_DATA[order(MAM_DATA$SEQ_ID),]
  
  #Create a table of the EVENT_CODE="R" (resume effort) information at the start of each transect and
  #merge to all records within a uniquely identified TRANSECT_ID
  MAM_DATA_R=MAM_DATA[MAM_DATA$EVENT_CODE=="R" & !is.na(MAM_DATA$TRANSECT_ID),c("TRANSECT_ID","LAT","LONG","LOCAL_DATE_TIME")]
  colnames(MAM_DATA_R)=c("TRANSECT_ID","LAT_1","LONG_1","LOCAL_DATE_TIME_1")
  MAM_DATA=merge(MAM_DATA, MAM_DATA_R,by="TRANSECT_ID",all.x=TRUE)
  MAM_DATA=MAM_DATA[order(MAM_DATA$SEQ_ID),]
  #Create a table of the EVENT_CODE="E" (end effort) information at the start of each transect and
  #merge to all records within a uniquely identified TRANSECT_ID
  MAM_DATA_E=MAM_DATA[MAM_DATA$EVENT_CODE=="E" & !is.na(MAM_DATA$TRANSECT_ID),c("TRANSECT_ID","LAT","LONG","LOCAL_DATE_TIME")]
  colnames(MAM_DATA_E)=c("TRANSECT_ID","LAT_2","LONG_2","LOCAL_DATE_TIME_2")
  MAM_DATA=merge(MAM_DATA, MAM_DATA_E,by="TRANSECT_ID",all.x=TRUE)
  MAM_DATA=MAM_DATA[order(MAM_DATA$SEQ_ID),]
  
  
  #Copy down EVENT_CODE="P" (personnel) information if updated in the middle of a transect
  for(i in 2:nrow(MAM_DATA))
  {if(MAM_DATA[i,"EVENT_CODE"]=="P") 
    MAM_DATA[i,c("LEFT_OBS","RECORDER","RIGHT_OBS")]=as.numeric(MAM_DATA[i,c("DATA1","DATA2","DATA3")])
  else
    if(MAM_DATA[i,"EVENT_CODE"]=="R")
      MAM_DATA[i,c("LEFT_OBS","RECORDER","RIGHT_OBS")]=MAM_DATA[i,c("LEFT_OBS","RECORDER","RIGHT_OBS")]
    else
      if(MAM_DATA[i,"LOCAL_DAY"]==MAM_DATA[i-1,"LOCAL_DAY"])
        MAM_DATA[i,c("LEFT_OBS","RECORDER","RIGHT_OBS")]=MAM_DATA[i-1,c("LEFT_OBS","RECORDER","RIGHT_OBS")] 
  }
  
  #Copy down EVENT_CODE="V" (visibility) information if updated in the middle of a transect
  for(i in 2:nrow(MAM_DATA))
  {if(MAM_DATA[i,"EVENT_CODE"]=="V") 
    MAM_DATA[i,c("BEAUFORT","SW_HGT","SW_DIR","WIND_VEL")]=as.numeric(MAM_DATA[i,c("DATA1","DATA2","DATA3","DATA4")])
  else
    if(MAM_DATA[i,"EVENT_CODE"]=="R")
      MAM_DATA[i,c("BEAUFORT","SW_HGT","SW_DIR","WIND_VEL")]=MAM_DATA[i,c("BEAUFORT","SW_HGT","SW_DIR","WIND_VEL")]
    else
      if(MAM_DATA[i,"LOCAL_DAY"]==MAM_DATA[i-1,"LOCAL_DAY"])
        MAM_DATA[i,c("BEAUFORT","SW_HGT","SW_DIR","WIND_VEL")]=MAM_DATA[i-1,c("BEAUFORT","SW_HGT","SW_DIR","WIND_VEL")]
  } 
  
  #Copy down EVENT_CODE="N" (navigation) information if updated in the middle of a transect
  for(i in 2:nrow(MAM_DATA))
  {if(MAM_DATA[i,"EVENT_CODE"]=="N") 
    MAM_DATA[i,c("COURSE","SPEED")]=as.numeric(MAM_DATA[i,c("DATA1","DATA2")])
  else
    if(MAM_DATA[i,"EVENT_CODE"]=="R")
      MAM_DATA[i,c("COURSE","SPEED")]=MAM_DATA[i,c("COURSE","SPEED")]
    else
      if(MAM_DATA[i,"LOCAL_DAY"]==MAM_DATA[i-1,"LOCAL_DAY"])
        MAM_DATA[i,c("COURSE","SPEED")]=MAM_DATA[i-1,c("COURSE","SPEED")]
  } 
  
  #Copy down EVENT_CODE="W" (weather) information if updated in the middle of a transect
  for(i in 2:nrow(MAM_DATA))
  {if(MAM_DATA[i,"EVENT_CODE"]=="W") 
    MAM_DATA[i,c("RAIN_FOG","HORIZ_SUN","VERT_SUN","WIND_DIR","VIS")]=as.numeric(MAM_DATA[i,c("DATA1","DATA2","DATA3","DATA4","DATA5")])
  else
    if(MAM_DATA[i,"EVENT_CODE"]=="R")
      MAM_DATA[i,c("RAIN_FOG","HORIZ_SUN","VERT_SUN","WIND_DIR","VIS")]=MAM_DATA[i,c("RAIN_FOG","HORIZ_SUN","VERT_SUN","WIND_DIR","VIS")]
    else
      if(MAM_DATA[i,"LOCAL_DAY"]==MAM_DATA[i-1,"LOCAL_DAY"])
        MAM_DATA[i,c("RAIN_FOG","HORIZ_SUN","VERT_SUN","WIND_DIR","VIS")]=MAM_DATA[i-1,c("RAIN_FOG","HORIZ_SUN","VERT_SUN","WIND_DIR","VIS")]
  } 
  
  
  #Make an SIGHTING_ID field that correspond to the sighting number or zero
  require(stringr)
  MAM_DATA$SIGHTING_ID=ifelse(MAM_DATA$EVENT_CODE=="S",str_trim(MAM_DATA$DATA1),NA)
  #Copy this SIGHTING_ID field down to the associated fields
  for(i in 1:nrow(MAM_DATA))
  {if(MAM_DATA$EVENT_CODE[i]%in%c("A","1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","?"))
    MAM_DATA$SIGHTING_ID[i]=MAM_DATA$SIGHTING_ID[i-1]}
  
  
  #Assemble OBS_CODE from TRANSECT_CODE and sightning number (ifelse accomodates non-numeric sighting numbers)
  MAM_DATA$SIGHTING_ID=ifelse(!is.na(as.numeric(MAM_DATA$SIGHTING_ID)),
                              sprintf("%04d",as.integer(MAM_DATA$SIGHTING_ID)),
                              MAM_DATA$SIGHTING_ID)
  
  MAM_DATA[!is.na(MAM_DATA$SIGHTING_ID),"SIGHTING_ID"]=paste(MAM_DATA[!is.na(MAM_DATA$SIGHTING_ID),"CRUISE_DAY"],
                                                             MAM_DATA[!is.na(MAM_DATA$SIGHTING_ID),"SIGHTING_ID"],
                                                             sep="_")
  
  print(paste("Test for uniqueness of SIGHTING_IDs (",MAM_DATA$CRUISE[1],"):",length(which(MAM_DATA$EVENT_CODE=="S")),":",length(unique(MAM_DATA[!is.na(MAM_DATA$SIGHTING_ID),"SIGHTING_ID"]))))
  
  
  #Create a table containing marine mammal sightings and associated information
  MAM_DATA_S=MAM_DATA[MAM_DATA$EVENT_CODE%in%c("S","A","1","2","3","4","5","6","7","8","9","10",
                                               "11","12","13","14","15","?") 
                      & !is.na(MAM_DATA$SIGHTING_ID),
                      c("SIGHTING_ID","EVENT_CODE","DATA1","DATA2","DATA3","DATA4","DATA5","DATA6","DATA7","DATA8")]
  
  #add fields to be populated from generic data columns
  MAM_DATA_S[,c("OBSERVER_ID","CUE","S_CODE","BEARING","RETICLE","DISTANCE","INITIAL_SPP",
                "DATA","PHOTO","BIRDS","SPP_CODE_1","SPP_CODE_2","SPP_CODE_3")]=NA
  
  #horizontalize and label data from EVENT_CODE="S" and "A" generic data columns
  for(i in 1:nrow(MAM_DATA_S))
  {
    if(MAM_DATA_S$EVENT_CODE[i]=="S")
    {MAM_DATA_S[i,c("OBSERVER_ID","CUE","S_CODE","BEARING","RETICLE","DISTANCE","INITIAL_SPP")]=
      MAM_DATA_S[i,c("DATA2","DATA3","DATA4","DATA5","DATA6","DATA7","DATA8")]
    MAM_DATA_S[i,c("DATA","PHOTO","BIRDS","SPP_CODE_1","SPP_CODE_2","SPP_CODE_3")]=
      MAM_DATA_S[i+1,c("DATA2","DATA3","DATA4","DATA5","DATA6","DATA7")]}
  }
  
  #convert format of data columns in MAM_DATA_S data frame to numeric
  MAM_DATA_S$OBSERVER_ID=as.numeric(MAM_DATA_S$OBSERVER_ID); 
  MAM_DATA_S$CUE=as.numeric(MAM_DATA_S$CUE); MAM_DATA_S$S_CODE=as.numeric(MAM_DATA_S$S_CODE); 
  MAM_DATA_S$BEARING=as.numeric(MAM_DATA_S$BEARING); MAM_DATA_S$RETICLE=as.numeric(MAM_DATA_S$RETICLE); 
  MAM_DATA_S$DISTANCE=as.numeric(MAM_DATA_S$DISTANCE);MAM_DATA_S$DATA=as.numeric(MAM_DATA_S$DATA)
  #remove blank spaces in SPP codes while maintaining them as character strings (to avoid losing non-numeric species codes)
  require(stringr)
  MAM_DATA_S$INITIAL_SPP=str_trim(MAM_DATA_S$INITIAL_SPP); MAM_DATA_S$SPP_CODE_1=str_trim(MAM_DATA_S$SPP_CODE_1); 
  MAM_DATA_S$SPP_CODE_2=str_trim(MAM_DATA_S$SPP_CODE_2); MAM_DATA_S$SPP_CODE_3=str_trim(MAM_DATA_S$SPP_CODE_3)
  MAM_DATA_S$DATA=str_trim(MAM_DATA_S$DATA);MAM_DATA_S$BIRDS=str_trim(MAM_DATA_S$BIRDS);
  MAM_DATA_S$PHOTO=str_trim(MAM_DATA_S$PHOTO);
  
  #For each marine mammal sighting number string together and take the mean of the 
  #different observer estimates of marine mammal group size and percent composition
  
  for(i in unique(MAM_DATA_S$SIGHTING_ID))
  { MAM_DATA_S_temp=MAM_DATA_S[MAM_DATA_S$SIGHTING_ID==i & MAM_DATA_S$EVENT_CODE%in%as.character(1:15),]
  MAM_DATA_S[MAM_DATA_S$SIGHTING_ID==i,"OBS_ID_ALL"]=paste(as.numeric(MAM_DATA_S_temp$DATA1),collapse=",")
  MAM_DATA_S[MAM_DATA_S$SIGHTING_ID==i,"BEST_ALL"]=paste(as.numeric(MAM_DATA_S_temp$DATA2),collapse=",")
  MAM_DATA_S[MAM_DATA_S$SIGHTING_ID==i,"HIGH_ALL"]=paste(as.numeric(MAM_DATA_S_temp$DATA3),collapse=",")
  MAM_DATA_S[MAM_DATA_S$SIGHTING_ID==i,"LOW_ALL"]=paste(as.numeric(MAM_DATA_S_temp$DATA4),collapse=",")
  MAM_DATA_S[MAM_DATA_S$SIGHTING_ID==i,"PERCENT_SPP1_ALL"]=paste(as.numeric(MAM_DATA_S_temp$DATA5),collapse=",")
  MAM_DATA_S[MAM_DATA_S$SIGHTING_ID==i,"PERCENT_SPP2_ALL"]=paste(as.numeric(MAM_DATA_S_temp$DATA6),collapse=",")
  MAM_DATA_S[MAM_DATA_S$SIGHTING_ID==i,"PERCENT_SPP3_ALL"]=paste(as.numeric(MAM_DATA_S_temp$DATA7),collapse=",")}
  
  #Remove the EVENT_CODE= A,1,2,3,4,5,6 etc. rows and reorganize the column order removing generic data columns
  MAM_DATA_S=MAM_DATA_S[MAM_DATA_S$EVENT_CODE=="S",]
  MAM_DATA_S=subset(MAM_DATA_S,select=c("SIGHTING_ID","OBSERVER_ID","CUE","S_CODE","BEARING","RETICLE",
                                        "DISTANCE","INITIAL_SPP","DATA","PHOTO","BIRDS","SPP_CODE_1",
                                        "SPP_CODE_2","SPP_CODE_3","OBS_ID_ALL","BEST_ALL","HIGH_ALL",
                                        "LOW_ALL","PERCENT_SPP1_ALL","PERCENT_SPP2_ALL","PERCENT_SPP3_ALL"))
  
  #Merge MAM_DATA_S rows with the overall MAM_DATA table 
  MAM_DATA=merge(MAM_DATA, MAM_DATA_S,by="SIGHTING_ID",all.x=TRUE)
  MAM_DATA=MAM_DATA[order(MAM_DATA$SEQ_ID),]
  
  #Reorganize columns
  MAM_DATA=subset(MAM_DATA,select=c(SEQ_ID,CRUISE,CRUISE_DAY,TRANSECT_CODE,LAT,LONG,LOCAL_DATE_TIME,
                                    LM_MONTH,LM_DAY,LM_YEAR,LM_HOUR,LM_MIN,LM_SEC,EVENT_CODE,EFFORT,
                                    DATA1,DATA2,DATA3,DATA4,DATA5,DATA6,DATA7,DATA8,
                                    LEFT_OBS,RECORDER,RIGHT_OBS,COURSE,SPEED, BEAUFORT,SW_HGT,SW_DIR,
                                    WIND_VEL,RAIN_FOG,HORIZ_SUN,VERT_SUN,WIND_DIR,VIS,LAT_1,LONG_1,LAT_2,LONG_2,
                                    LOCAL_DATE_TIME_1,LOCAL_DATE_TIME_2,SIGHTING_ID,OBSERVER_ID,CUE,S_CODE,BEARING,
                                    RETICLE,DISTANCE,INITIAL_SPP,DATA,PHOTO,BIRDS,SPP_CODE_1,SPP_CODE_2,
                                    SPP_CODE_3,OBS_ID_ALL,BEST_ALL,HIGH_ALL,LOW_ALL,PERCENT_SPP1_ALL,
                                    PERCENT_SPP2_ALL,PERCENT_SPP3_ALL))
  
  #GM_OFFSET, removed
  
  #Reorder all the records back to the original configuration after merges
  MAM_DATA=MAM_DATA[order(MAM_DATA$SEQ_ID),]
  
  if(!is.na(OUTPUT)){write.csv(MAM_DATA,file=OUTPUT)}
  return(MAM_DATA)
}

#### SEABIRD_IMPORT ####

SEABIRD_IMPORT=function(FILE,WIDTHS,SEABIRD_DATA=NULL,COL_CLASSES,COL_NAMES)
{require(chron)
  
  if(is.null(SEABIRD_DATA)){
    #Read the various data fields from fixed width character colunms in each data file
    SEABIRD_DATA=read.fwf(FILE,widths=WIDTHS,colClasses=COL_CLASSES,col.names=COL_NAMES,header=FALSE)
    SEABIRD_DATA=as.data.frame(SEABIRD_DATA)
  }
  
  #Reshape date time colunms into MS Excel date-time serial (Julian date with origin of 12/30/1899))
  SEABIRD_DATA$YEAR=as.numeric(ifelse(SEABIRD_DATA$YEAR>70,paste("19",sprintf("%02d",SEABIRD_DATA$YEAR),sep=""),paste("20",sprintf("%02d",SEABIRD_DATA$YEAR),sep="")))
  SEABIRD_DATA$LM_YEAR=as.numeric(ifelse(SEABIRD_DATA$LM_YEAR>70,paste("19",sprintf("%02d",SEABIRD_DATA$LM_YEAR),sep=""),paste("20",sprintf("%02d",SEABIRD_DATA$LM_YEAR),sep="")))
  if(!"LM_SEC"%in%colnames(SEABIRD_DATA)){SEABIRD_DATA$LM_SEC=0}
  SEABIRD_DATA$LM_SEC=ifelse(is.na(SEABIRD_DATA$LM_SEC),0,SEABIRD_DATA$LM_SEC)
  if(!"SEC"%in%colnames(SEABIRD_DATA)){SEABIRD_DATA$SEC=0}
  SEABIRD_DATA$SEC=ifelse(is.na(SEABIRD_DATA$SEC),0,SEABIRD_DATA$SEC)
  SEABIRD_DATA$GM_DATE_TIME=julian(SEABIRD_DATA$MONTH,SEABIRD_DATA$DAY,SEABIRD_DATA$YEAR,
                                   origin=c(month = 12, day = 30, year = 1899)) +
    SEABIRD_DATA$HOUR/24+SEABIRD_DATA$MIN/(60*24)+SEABIRD_DATA$SEC/(60*60*24)
  SEABIRD_DATA$LOCAL_DATE_TIME=julian(SEABIRD_DATA$LM_MONTH,SEABIRD_DATA$LM_DAY,SEABIRD_DATA$LM_YEAR,
                                      origin=c(month = 12, day = 30, year = 1899),dec=5)+
    SEABIRD_DATA$LM_HOUR/24+SEABIRD_DATA$LM_MIN/(60*24)+SEABIRD_DATA$LM_SEC/(60*60*24)
  SEABIRD_DATA$LOCAL_DAY=julian(SEABIRD_DATA$LM_MONTH,SEABIRD_DATA$LM_DAY,SEABIRD_DATA$LM_YEAR,
                                origin=c(month = 12, day = 30, year = 1899))
  
  #Remove off-effort EVENT CODES as well as position updates within on effort transects (only available in later surveys)
  #SEABIRD_DATA=SEABIRD_DATA[SEABIRD_DATA$EVENT_CODE!=0 & SEABIRD_DATA$EVENT_CODE<=3,] #Code edited 12/19/2014: removed exclusion of EVENT_CODE==0
  SEABIRD_DATA=SEABIRD_DATA[SEABIRD_DATA$EVENT_CODE<=3,]
  SEABIRD_DATA$CRUISE_DAY=paste(SEABIRD_DATA$CRUISE,"_",SEABIRD_DATA$LOCAL_DAY,sep="")
  
  #Assign unique number for each OBSERVATION within a TRANSECT
  SEABIRD_DATA$OBS_CODE_NUM=1
  for(i in 1:nrow(SEABIRD_DATA))
  {if(SEABIRD_DATA[i,"EVENT_CODE"]>1) 
    SEABIRD_DATA[i,"OBS_CODE_NUM"]=SEABIRD_DATA[i-1,"OBS_CODE_NUM"]+1 
  else 
    SEABIRD_DATA[i,"OBS_CODE_NUM"]=SEABIRD_DATA[i,"OBS_CODE_NUM"]}
  
  #Assign a unique number for each TRANSECT within a CRUISE_DAY
  SEABIRD_DATA$T_CODE_NUM=1
  for(i in 2:nrow(SEABIRD_DATA))
  {if(SEABIRD_DATA[i,"LOCAL_DAY"]!=SEABIRD_DATA[i-1,"LOCAL_DAY"])
    SEABIRD_DATA[i,"T_CODE_NUM"]=1
  else  
    if(SEABIRD_DATA[i,"EVENT_CODE"]==1) 
      SEABIRD_DATA[i,"T_CODE_NUM"]=SEABIRD_DATA[i-1,"T_CODE_NUM"]+1 
    else
      SEABIRD_DATA[i,"T_CODE_NUM"]=SEABIRD_DATA[i-1,"T_CODE_NUM"]}
  
  #unique identifier for each TRANSECTS and SEABIRD_DATA 
  SEABIRD_DATA$TRANSECT_CODE=paste(SEABIRD_DATA$CRUISE_DAY,"_",SEABIRD_DATA$T_CODE_NUM,sep="")
  SEABIRD_DATA$OBS_CODE=paste(SEABIRD_DATA$TRANSECT_CODE,"_",SEABIRD_DATA$OBS_CODE_NUM,sep="")
  
  #Add colunms for WIND_DIR and WIND_VEL missing in earlier years of surveys
  if(!"WIND_DIR"%in%colnames(SEABIRD_DATA)){SEABIRD_DATA$WIND_DIR=NA;SEABIRD_DATA$WIND_VEL=NA}
  
  #convert from -180:180 to 0:360 reference system (better for displaying Pacific)
  if("LONG"%in%colnames(SEABIRD_DATA))
  {SEABIRD_DATA$LONG=ifelse(SEABIRD_DATA$LONG<0,360+SEABIRD_DATA$LONG,SEABIRD_DATA$LONG)}
  
  
  colnames(SEABIRD_DATA)[colnames(SEABIRD_DATA)%in%"YEAR"] = "GM_YEAR"
  colnames(SEABIRD_DATA)[colnames(SEABIRD_DATA)%in%"MONTH"] = "GM_MONTH"
  colnames(SEABIRD_DATA)[colnames(SEABIRD_DATA)%in%"DAY"] = "GM_DAY"
  colnames(SEABIRD_DATA)[colnames(SEABIRD_DATA)%in%"HOUR"] = "GM_HOUR"
  colnames(SEABIRD_DATA)[colnames(SEABIRD_DATA)%in%"MIN"] = "GM_MIN"
  colnames(SEABIRD_DATA)[colnames(SEABIRD_DATA)%in%"SEC"] = "GM_SEC"
  
  #Select a standardized subset of columns for output
  SEABIRD_DATA=subset(SEABIRD_DATA,select=c(CRUISE,CRUISE_DAY,TRANSECT_CODE,OBS_CODE,BEAUFORT,SHIP_COURSE,OBS_COND,
                                            OBS_SIDE,OBS_CODE_1,EVENT_CODE,SPP_CODE,SPP_NUMBER,DISTANCE,ASSOC,BEHAV,
                                            FLIGHT_DIR,AGE,SEX,COMMENTS,LAT,LONG,GM_DATE_TIME,LOCAL_DATE_TIME,LOCAL_DAY,
                                            LM_YEAR,LM_MONTH,LM_DAY,LM_HOUR,LM_MIN,LM_SEC,
                                            GM_YEAR,GM_MONTH,GM_DAY,GM_HOUR,GM_MIN,GM_SEC,
                                            WIND_DIR,WIND_VEL))
  
  return(SEABIRD_DATA)}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#Calculate Geodesic proximity (WGS1984 ellipsoid) by taking the minimum distance from the vertices of a line or polygon  
NEAR=function(DATA,NEAR_OBJ)
{require(rgdal);require(raster);require(rgeos);require(Imap)
  if(class(NEAR_OBJ)[[1]]=="SpatialLinesDataFrame"|class(NEAR_OBJ)[[1]]=="SpatialLines")
  {NEAR_OBJ=as.data.frame(coordinates(NEAR_OBJ)[[1]][[1]])}
  if(class(NEAR_OBJ)[[1]]=="SpatialPolygonsDataFrame"|class(NEAR_OBJ)[[1]]=="SpatialPolygons")
  {NEAR_OBJ=as.data.frame(coordinates(as(NEAR_OBJ,"SpatialLines"))[[1]][[1]])}
  colnames(NEAR_OBJ)=c("x","y")
  NEAR_LONG=matrix(rep(NEAR_OBJ$x,nrow(DATA)),ncol=nrow(NEAR_OBJ),nrow=nrow(DATA),byrow=T)
  NEAR_LAT=matrix(rep(NEAR_OBJ$y,nrow(DATA)),ncol=nrow(NEAR_OBJ),nrow=nrow(DATA),byrow=T)
  DATA_LONG=matrix(rep(DATA[,"LONG"],nrow(NEAR_OBJ)),ncol=nrow(NEAR_OBJ),nrow=nrow(DATA),byrow=F)
  DATA_LAT=matrix(rep(DATA[,"LAT"],nrow(NEAR_OBJ)),ncol=nrow(NEAR_OBJ),nrow=nrow(DATA),byrow=F)
  NEAR_DIST=gdist(NEAR_LONG,NEAR_LAT,DATA_LONG,DATA_LAT,units="km")
  return(data.frame(DATA,
                    NEAR_DIST=sapply(seq(1,nrow(NEAR_DIST)), function(i) {NEAR_DIST[i,which.min(NEAR_DIST[i,])]}),
                    NEAR_LONG=sapply(seq(1,nrow(NEAR_DIST)), function(i) {NEAR_LONG[i,which.min(NEAR_DIST[i,])]}),
                    NEAR_LAT=sapply(seq(1,nrow(NEAR_DIST)), function(i) {NEAR_LAT[i,which.min(NEAR_DIST[i,])]})))}


#Function to transform the coordinates of any objects of the classes SpatialPolygonsDataFrame, SpatialLinesDataFrame, or 
#SpatialPointsDataFrame from a -180:180 to 0:360 reference system or vice versa (emulates the rotate() function from the raster package)
SP_ROTATE=function(SP_OBJ)
{
  if(class(SP_OBJ)[1]=="SpatialPolygonsDataFrame")
  {
    if(SP_OBJ@bbox[1]<0) #polygons with negative longitudes (reference frame: -180:180)
    {SP_OBJ@bbox[1]=SP_OBJ@bbox[1]+360 #shift bounding box minimum by + 360 (i.e. -180 -> 180)
    SP_OBJ@bbox[3]=ifelse(SP_OBJ@bbox[3]<0,SP_OBJ@bbox[3]+360,SP_OBJ@bbox[3]) #shift bounding box maximum by + 360 (i.e. 180 -> 360)
    for(i in 1:nrow(summary(SP_OBJ@polygons)))
    {for(j in 1:nrow(summary(SP_OBJ@polygons[[i]]@Polygons)))
    {SP_OBJ@polygons[[i]]@Polygons[[j]]@coords[,1]=ifelse(SP_OBJ@polygons[[i]]@Polygons[[j]]@coords[,1]<0,
                                                          360+SP_OBJ@polygons[[i]]@Polygons[[j]]@coords[,1],
                                                          SP_OBJ@polygons[[i]]@Polygons[[j]]@coords[,1]) #change coordinate longitudes < 0 to >180 by adding 360
    }
    }
    }
    else{
      if(SP_OBJ@bbox[3]>180) #polygons with longitudes > 180 (i.e. reference frame: 0:360)
      {SP_OBJ@bbox[1]=ifelse(SP_OBJ@bbox[1]>180,SP_OBJ@bbox[1]-360,SP_OBJ@bbox[1]) #shift bounding box minimum by - 180 (i.e. 0 -> -180)
      SP_OBJ@bbox[3]=SP_OBJ@bbox[3]-360 #shift bounding box maximum by - 180 (i.e. 360 -> 180)
      for(i in 1:nrow(summary(SP_OBJ@polygons)))
      {for(j in 1:nrow(summary(SP_OBJ@polygons[[i]]@Polygons)))
      {SP_OBJ@polygons[[i]]@Polygons[[j]]@coords[,1]=ifelse(SP_OBJ@polygons[[i]]@Polygons[[j]]@coords[,1]>180,
                                                            SP_OBJ@polygons[[i]]@Polygons[[j]]@coords[,1]-360,
                                                            SP_OBJ@polygons[[i]]@Polygons[[j]]@coords[,1]) #change coordinate longitudes >180 to <0 by subtracting 360
      }
      }
      }
    }
  }
  
  if(class(SP_OBJ)[1]=="SpatialLinesDataFrame")
  {
    if(SP_OBJ@bbox[1]<0)
    {SP_OBJ@bbox[1]=SP_OBJ@bbox[1]+360 #shift bounding box minimum by + 360 (i.e. -180 -> 180)
    SP_OBJ@bbox[3]=ifelse(SP_OBJ@bbox[3]<0,SP_OBJ@bbox[3]+360,SP_OBJ@bbox[3]) #shift bounding box maximum by + 360 (i.e. 180 -> 360)
    for(i in 1:nrow(summary(SP_OBJ@lines)))
    {for(j in 1:nrow(summary(SP_OBJ@lines[[i]]@Lines)))
    {SP_OBJ@lines[[i]]@Lines[[j]]@coords[,1]=ifelse(SP_OBJ@lines[[i]]@Lines[[j]]@coords[,1]<0,
                                                    360+SP_OBJ@lines[[i]]@Lines[[j]]@coords[,1],
                                                    SP_OBJ@lines[[i]]@Lines[[j]]@coords[,1])
    }
    }
    }
    else{
      if(SP_OBJ@bbox[3]>180)
      {SP_OBJ@bbox[1]=ifelse(SP_OBJ@bbox[1]>180,SP_OBJ@bbox[1]-360,SP_OBJ@bbox[1]) #shift bounding box minimum by - 180 (i.e. 0 -> -180)
      SP_OBJ@bbox[3]=SP_OBJ@bbox[3]-360 #shift bounding box maximum by - 180 (i.e. 360 -> 180)
      for(i in 1:nrow(summary(SP_OBJ@lines)))
      {for(j in 1:nrow(summary(SP_OBJ@lines[[i]]@Lines)))
      {SP_OBJ@lines[[i]]@Lines[[j]]@coords[,1]=ifelse(SP_OBJ@lines[[i]]@Lines[[j]]@coords[,1]>180,
                                                      SP_OBJ@lines[[i]]@Lines[[j]]@coords[,1]-360,
                                                      SP_OBJ@lines[[i]]@Lines[[j]]@coords[,1])
      }
      }
      }
    }
  }
  
  
  if(class(SP_OBJ)[1]=="SpatialPointsDataFrame")
  {if(SP_OBJ@bbox[1]<0)
  {SP_OBJ@bbox[1]=SP_OBJ@bbox[1]+360 #shift bounding box minimum by + 360 (i.e. -180 -> 180)
  SP_OBJ@bbox[3]=ifelse(SP_OBJ@bbox[3]<0,SP_OBJ@bbox[3]+360,SP_OBJ@bbox[3]) #shift bounding box maximum by + 360 (i.e. 180 -> 360)
  SP_OBJ@coords[,1]=ifelse(SP_OBJ@coords[,1]<0,360+SP_OBJ@coords[,1],SP_OBJ@coords[,1])}
    else{
      if(SP_OBJ@bbox[3]>180)
      {SP_OBJ@bbox[1]=ifelse(SP_OBJ@bbox[1]>180,SP_OBJ@bbox[1]-360,SP_OBJ@bbox[1]) #shift bounding box minimum by - 180 (i.e. 0 -> -180)
      SP_OBJ@bbox[3]=SP_OBJ@bbox[3]-360 #shift bounding box maximum by - 180 (i.e. 360 -> 180)
      SP_OBJ@coords[,1]=ifelse(SP_OBJ@coords[,1]>180,SP_OBJ@coords[,1]-360,SP_OBJ@coords[,1])}
    }
  }
  return(SP_OBJ)
}



##### Create a North Arrow for plotting on any map
NORTH_ARROW = function(TIP_COORD)
{RANGE_X=par()$usr[1:2]
RANGE_Y=par()$usr[3:4]
lines(x=c(TIP_COORD[1],TIP_COORD[1]),
      y=c(TIP_COORD[2],TIP_COORD[2]-0.10*(RANGE_Y[2]-RANGE_Y[1])),lwd=2)
lines(x=c(TIP_COORD[1],TIP_COORD[1]-0.010*(RANGE_X[2]-RANGE_X[1])),
      y=c(TIP_COORD[2],TIP_COORD[2]-0.025*(RANGE_X[2]-RANGE_X[1])),lwd=2)
text(x=TIP_COORD[1],y=TIP_COORD[2]-0.10*(RANGE_Y[2]-RANGE_Y[1]),
     label="N",cex=1.35,pos=1)}

##### Create a Length bar for plotting on any map
LENGTH_BAR = function(LEFT_COORD,PRECISION=-2,PROPORTION=0.25)
{require(Imap)
  RANGE_X=par()$usr[1:2]
  RANGE_Y=par()$usr[3:4]
  DEG_KM=gdist(RANGE_X[1],RANGE_Y[1],RANGE_X[1]+1,RANGE_Y[1],units="km")
  LENGTH_KM=round(PROPORTION*DEG_KM*(RANGE_X[2]-RANGE_X[1]),PRECISION)
  LENGTH_DEG=(RANGE_X[2]-RANGE_X[1])*LENGTH_KM/(DEG_KM*(RANGE_X[2]-RANGE_X[1]))
  lines(x=c(LEFT_COORD[1],LEFT_COORD[1]+LENGTH_DEG),y=c(LEFT_COORD[2],LEFT_COORD[2]),lwd=2)
  lines(x=c(LEFT_COORD[1]+LENGTH_DEG,LEFT_COORD[1]+LENGTH_DEG),
        y=c(LEFT_COORD[2],LEFT_COORD[2]-0.025*(RANGE_Y[2]-RANGE_Y[1])),lwd=2)
  lines(x=c(LEFT_COORD[1]+LENGTH_DEG/2,LEFT_COORD[1]+LENGTH_DEG/2),
        y=c(LEFT_COORD[2],LEFT_COORD[2]-0.025*(RANGE_Y[2]-RANGE_Y[1])),lwd=2)
  lines(x=c(LEFT_COORD[1]+LENGTH_DEG/4,LEFT_COORD[1]+LENGTH_DEG/4),
        y=c(LEFT_COORD[2],LEFT_COORD[2]-0.025*(RANGE_Y[2]-RANGE_Y[1])),lwd=2)
  lines(x=c(LEFT_COORD[1],LEFT_COORD[1]),
        y=c(LEFT_COORD[2],LEFT_COORD[2]-0.025*(RANGE_Y[2]-RANGE_Y[1])),lwd=2)
  text(x=c(LEFT_COORD[1],LEFT_COORD[1]+LENGTH_DEG/4,LEFT_COORD[1]+LENGTH_DEG/2,LEFT_COORD[1]+LENGTH_DEG),
       y=rep(LEFT_COORD[2]-0.025*(RANGE_Y[2]-RANGE_Y[1]),4),
       labels=c(round(c(0,LENGTH_KM/4,LENGTH_KM/2),0),paste(round(LENGTH_KM,0),"km")),pos=1)}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#Function to create a user defined color-bar on a plot
COLOR_BAR=function(XLEFT,YBOTTOM,XRIGHT,YTOP,COLORS,TICKS,ROUND,CEX)
{require(Imap)
  RANGE_Y=abs(YTOP-YBOTTOM)
  RANGE_X=abs(XRIGHT-XLEFT)
  TICKS=round(TICKS,ROUND)
  if(ROUND<0){ROUND=0}
  for(i in 0:(length(COLORS)-1)){rect(XLEFT,YBOTTOM+i*RANGE_Y/length(COLORS),
                                      XRIGHT,YBOTTOM+(i+1)*RANGE_Y/length(COLORS),col=COLORS[i],lty=0)}
  rect(XLEFT,YBOTTOM,XRIGHT,YTOP,col=NA,lty=1,lwd=CEX)
  segments(x0=XRIGHT, y0=seq(YBOTTOM,YTOP,length.out=length(TICKS)), 
           x1 = XRIGHT+0.20*RANGE_X, y1 = seq(YBOTTOM,YTOP,length.out=length(TICKS)),lwd=CEX)
  text(x=rep(XRIGHT+0.20*RANGE_X,length(TICKS)), y=seq(YBOTTOM,YTOP,length.out=length(TICKS)),
       labels=sprintf(paste("%.",ROUND,"f",sep=""),TICKS),pos=4,cex=CEX)}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Create a grids of different cell sizes to link to CTD summaries
PREDICT_POLYS=function(LAT_MIN, LAT_MAX, LONG_MIN, LONG_MAX, CELL_SIZE)
{#Create grid of prediction cells
  PREDICT_GRID <- GridTopology(cellcentre.offset=c(LONG_MIN+0.5*CELL_SIZE,LAT_MIN+0.5*CELL_SIZE), 
                               cellsize=c(CELL_SIZE,CELL_SIZE), 
                               cells.dim=c(abs(LONG_MAX-LONG_MIN)*1/CELL_SIZE,abs(LAT_MAX-LAT_MIN)*1/CELL_SIZE))
  #Reformat grid as Spatial Polygons
  PREDICT_POLYS <- as.SpatialPolygons.GridTopology(PREDICT_GRID,proj4string=CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
  
  #Extract coordinates and create a dataframe
  COORD_POLYS=coordinates(PREDICT_POLYS)
  colnames(COORD_POLYS)=c("LONG_MID","LAT_MID")
  COORD_POLYS=as.data.frame(COORD_POLYS)
  
  #Create a SpatialPolygonsDataFrame
  PREDICT_POLYS=SpatialPolygonsDataFrame(PREDICT_POLYS, COORD_POLYS)
  #projection(CRUISE_DAY_SP)=CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
  return(PREDICT_POLYS)
}

#Example: CTD_GRID_1.0=PREDICT_POLYS(23.0, 28.0, -80.0, -74.0, 1.0)
#projection(CTD_GRID_1.0)=CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


#function to transform these coordinates into an sp SpatialPolygonDataFrame which is R equivalent of a Shapefile
GRID=function(LAT_1, LONG_1, LAT_2, LONG_2, LAT_3, LONG_3, LAT_4, LONG_4)
{GRID=as.data.frame(cbind(LAT_1, LONG_1, LAT_2, LONG_2, LAT_3, LONG_3, LAT_4, LONG_4))
GRID_SPoly <- list()
for (i in 1:nrow(GRID)) 
{GRID_SPoly[[i]] <- Polygons(list(Polygon(rbind(as.numeric(GRID[i, c("LONG_1","LAT_1")]), 
                                                as.numeric(GRID[i, c("LONG_2","LAT_2")]),
                                                as.numeric(GRID[i, c("LONG_3","LAT_3")]),
                                                as.numeric(GRID[i, c("LONG_4","LAT_4")]),
                                                as.numeric(GRID[i, c("LONG_1","LAT_1")])))),
                             ID = as.character(i))}

GRID_POLYGONS <- SpatialPolygonsDataFrame(SpatialPolygons(GRID_SPoly), as.data.frame(GRID), 
                                          match.ID = FALSE)
#set the projection to GCS_WGS84 (default projection for ArcGIS and GPS coordinates)
projection(GRID_POLYGONS)=CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
return(GRID_POLYGONS)}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Function to scale cex to some numeric vector for mapping 
#function takes a numeric vector and outputs a vector of the same length with numeric values between CEX_MIN and CEX_MAX
#that have been scaled using an exponential or other algorithm provided by classInterval, N controls the number and spacing 
#of categories
CEX_SEQ=function(DATA,CEX_MIN,CEX_MAX,STYLE,N,BREAKS=NULL)
{if(STYLE=="exponential")
{DATA=CEX_MIN+(CEX_MAX-CEX_MIN)*as.numeric(cut(DATA,breaks=exp(1)^(seq(logb(min(DATA),exp(1)),
                                                                       logb(max(DATA),exp(1)),
                                                                       length.out=N))))/N
return(DATA)}
  
  # Take DATA cut by BREAKS (output: integers from 1:n) and scale CEX values from CEX_MIN to CEX_MAX
  if(STYLE=="fixed")
  {N = length(BREAKS) - 1
  DATA=seq(CEX_MIN,CEX_MAX,length.out = N)[cut(DATA,breaks=BREAKS,include.lowest = T)]
  return(DATA)}
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Function to calculate mixed layer depth from a raster brick (DATA) of SODA temp layers either as a specific threshold of
#temperature change relative to the surface (dT_THRESH) or as the depth of a specific isotherm (T_THRESH)
SODA_MLD=function(DATA,dT_THRESH=NA,T_THRESH=NA)
{if(c("raster") %in% as.character(installed.packages()[,1]))
  require(raster) else {install.packages("raster");require(raster)}
  
  if(c("rgdal") %in% as.character(installed.packages()[,1]))
    require(rgdal) else {install.packages("rgdal");require(rgdal)}
  
  if(c("ncdf") %in% as.character(installed.packages()[,1]))
    require(ncdf) else {install.packages("ncdf");require(ncdf)}
  
  SODA=DATA 
  #Calculate a raster brick of temperature change relative to the 5.01m reading
  SODA_dT=SODA
  for (i in 1:length(names(SODA))) {
    SODA_dT[[i]] <- calc(SODA, function(x){x[[1]]-x[[i]]} )
  }
  #Extract depth of layers from the names slot in SODA raster
  SODA_Z=as.numeric(substr(names(SODA),2,nchar(names(SODA))))
  
  #Calculate the change in depth between layers
  SODA_dZ=SODA_Z
  SODA_dZ=NA
  for(i in 2:length(SODA_Z))
  {SODA_dZ[i]=SODA_Z[i]-SODA_Z[i-1]}
  SODA_dZ[1]=0
  
  #Calculate the Mixed layer depth at a threshold temperature change (dT_THRESH) relative to the 5.01m 
  #temperature reading using a linear approximation to the real curve between SODA depth Rasterlayers
  if(!is.na(dT_THRESH)) 
  {SODA_MLD=calc(SODA_dT,function(x){SODA_Z[which(x>dT_THRESH)[1]-1]+SODA_dZ[which(x>dT_THRESH)[1]]*
      (dT_THRESH-x[which(x>dT_THRESH)[1]-1])/
      (x[which(x>dT_THRESH)[1]]-x[which(x>dT_THRESH)[1]-1])})}
  
  if(!is.na(T_THRESH)) 
  {SODA_MLD=calc(SODA,function(x){ifelse(x[[1]]<=T_THRESH,
                                         SODA_Z[1],
                                         SODA_Z[which(x<=T_THRESH)[1]-1]+SODA_dZ[which(x<=T_THRESH)[1]]*
                                           (x[which(x<=T_THRESH)[1]-1]-T_THRESH)/
                                           (x[which(x<=T_THRESH)[1]-1]-x[which(x<=T_THRESH)[1]]))})}
  
  return(SODA_MLD)
}
