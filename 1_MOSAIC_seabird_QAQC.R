#Pulls in all survey data
#Adds species common names and scientific names from protocol file
#Selects new survey to process and makes
#  mammal csv
#  bird ON csv
#  bird OFF csv
#  bird model (>30 sightings)

# sightings missing counts to correct
# sightings with species codes that have errors

# compiled csv from cruise
# compiled rds from cruise
# compiles all rds files from each cruise to date

library(ggplot2)
library(dplyr)
library(data.table)
library(lubridate)

if(Sys.info()[7]=="rachaelorben") {usr<-"/Users/rachaelorben";
  dir<-"/Library/CloudStorage/Box-Box/Seabird Oceanography Lab/Current_Research/MOSAIC_Seabird At-Sea Observations/"}
#if(Sys.info()[7]=="will") dir<-"/Volumes/GoogleDrive/My Drive/Seabird_Oceanography_Lab/At-SeaSurveys/HALO/Raw Dat"

sp<-read.csv(paste0(usr,dir,"data/SeaLog-Species_CodeList.csv"))
names(sp)<-c("Species_Name","Code","Sci_name","Animal")

Files<-list.files(paste0(usr,dir,"data/Corrected"),pattern = ".csv",full.names = T,recursive = T)

survey_dat<-NULL
for (j in 1:length(Files)){
  dat<-read.csv(file=Files[j],stringsAsFactors=FALSE,na.strings = "null") 
  dat$TripID<-as.character(dat$TripID)
  dat$PortCondition<-as.character(dat$PortCondition)
  dat$Beaufort<-as.character(dat$Beaufort)
  dat$Record<-as.character(dat$Record)
  dat$oid<-1:nrow(dat)
  dat$file<-Files[j]
  
  dat$Cruise_ID<-sapply(strsplit(Files[j], split='/', fixed=TRUE), function(x) (x[12])) #RAO specifications
  
  survey_dat<-bind_rows(survey_dat,dat)
}
unique(survey_dat$Vessel)

which(survey_dat$Vessel!="PacificStorm")
#survey_dat[32658,] #search & fix in corrected data

survey_dat[survey_dat=="null"]<-NA

survey_dat$datetime<-ymd_hms(survey_dat$GPSTime)

which(is.na(survey_dat$datetime))
#survey_dat[30765,] #search & fix in corrected data

survey_dat$month<-month(survey_dat$datetime)
survey_dat$date<-date(survey_dat$datetime)
survey_dat$year<-year(survey_dat$datetime)
#str(survey_dat)
#str(sp)
survey_dat<-left_join(survey_dat, sp, by=c("Species"="Code"))


# searches for sightings without number of birds --------------------------
unique(survey_dat$Cruise_ID)
C_ID<-unique(survey_dat$Cruise_ID)[3] #select new cruise ID here

missing_number<-survey_dat%>%filter(Cruise_ID==C_ID) %>%
  filter(is.na(Species)==FALSE)%>%
  filter(is.na(Count)==TRUE)
missing_number #should say: <0 rows> (or 0-length row.names)

#ADD YES TO THIS FILE NAME ONCE YOU HAVE FIXED THESE SIGHTINGS so it is not written over!!!!
write.csv(missing_number, 
          paste0(usr,dir,"Analysis/processed_data/",C_ID,"_sightings_missing_count_correctedYES.csv"))

# species summary for new cruise ---------------------------------------------------------
species_sum<-survey_dat%>%filter(Cruise_ID==C_ID)%>% 
  filter(Species!="null")%>%
  group_by(Species, Species_Name, Animal, On.OffTx)%>%
  summarise(Total_Birds=sum(Count, na.rm=TRUE),Sightings=n())%>%
  arrange(-Total_Birds)

species_sum_mammals<-species_sum%>%filter(Animal=="mammal")

species_sum_birds<-species_sum%>%filter(Animal=="bird")%>%
  arrange(Species)

write.csv(species_sum_birds%>%filter(On.OffTx=="ON")%>%
            ungroup()%>%
            select(-Animal,-On.OffTx), 
          paste0(usr,dir,"Analysis/processed_data/",C_ID,"_SpeciesSummaryTable_allbirdsON.csv")) #birds ON

write.csv(species_sum_birds%>%filter(On.OffTx=="OFF")%>%
            ungroup()%>%
            select(-Animal,-On.OffTx), 
          paste0(usr,dir,"Analysis/processed_data/",C_ID,"_SpeciesSummaryTable_allbirdsOFF.csv")) #birds OFF


# searches for four-letter codes not in reference list --------------------
species_sum_UNK<-species_sum%>%filter(is.na(Animal)==TRUE) #codes to fix in data
info<-survey_dat %>%
  filter(Species %in% species_sum_UNK$Species)

species_sum_UNK_dt<-full_join(species_sum_UNK,info%>%select(Species,datetime,StarboardObs,PortObs), by="Species")%>%
  filter(is.na(Species)==FALSE)%>%ungroup()%>%
  select(-Animal,-On.OffTx, -Total_Birds,-Sightings) 

species_sum_UNK_dt
# summary table of sightings to fix, added FIXED once these are made
#write.csv(species_sum_UNK_dt,paste0(dir,"Analysis/processed_data/",C_ID,"_SpeciesSummaryTable_BIRD_tofix.csv"))

#  makes Obs column & Obs side column -------------------------------------
names(survey_dat)

survey_dat$Obs<-survey_dat$StarboardObs
survey_dat$s<-1
survey_dat$p<-1
survey_dat$s[is.na(survey_dat$StarboardObs)]<-0
survey_dat$p[is.na(survey_dat$PortObs)]<-0

survey_dat$Obs2<-survey_dat$s+survey_dat$p
unique(survey_dat$Obs2)
which(survey_dat$Obs2==2)
(twoObs<-survey_dat%>%filter(Obs2==2)) #checks for instances when OBS were entered on both port & starboard

survey_dat$Obs[is.na(survey_dat$StarboardObs)==TRUE]<-survey_dat$PortObs[is.na(survey_dat$StarboardObs)==TRUE]
unique(survey_dat$Obs) 

#observer fields sometimes are half filled if the GPS logs when someone is changing the field. 
survey_dat%>%filter(Obs=="LDB") #records to clean up "LDB","WAP" "APD"
survey_dat%>%filter(is.na(Obs)==TRUE)%>%filter(On.OffTx=="ON") #records without obs. to fix

#adds info to side observed column
survey_dat$ObsSidePS<-NA
survey_dat$ObsSidePS[survey_dat$s==1]<-"Starboard"
survey_dat$ObsSidePS[survey_dat$p==1]<-"Port"

# searches for flight heights for non-flying birds ------------------------
names(survey_dat)

flight_heights<-survey_dat%>%filter(is.na(UserSelect1)==FALSE)

sp_SUM_fh<-flight_heights%>%group_by(Species,PrimaryBehavior,SecondaryBehavior,UserSelect1)%>%
  summarise(n=n())

sp_SUM_fh%>%ungroup()%>%filter(PrimaryBehavior=="Sitting")

unique(sp_SUM_fh$PrimaryBehavior)    


# removes columns that aren't collected & Obs columns-----------------------------------
nrow(survey_dat)
names(survey_dat)
summary(survey_dat)
unique(survey_dat$CloudCover)
unique(survey_dat$Plumage)
unique(survey_dat$Sex)
unique(survey_dat$ObsSideIO)
survey_dat<-survey_dat%>%select(-TxWidth, -Transect, -StarboardCondition, 
                                -CloudCover, -FogConc,-IceType, -IceConc, -Sex, -Plumage, -ObsSideIO,
                                -Distance, -Angle, -UserSelect2, -UserText, -UserNumeric,-s,-p,-Obs2,-PortObs,
                                -StarboardObs)

survey_dat<-survey_dat%>%rename(Condition = PortCondition, FlightHt = UserSelect1, DayID = TripID)
survey_dat<-survey_dat%>%select(Vessel,Cruise_ID,DayID,Record, oid, Condition, Beaufort, Weather, On.OffTx,
                                Type, Obs, ObsSidePS,Species, Count, PrimaryBehavior,SecondaryBehavior,Age,
                                Bin,FlightHt,Latitude,Longitude,datetime,Species_Name,Sci_name,Animal,Comments, file)
unique(survey_dat$Vessel)
survey_dat%>%filter(is.na(Vessel)==TRUE) #checks for vessel not entered
survey_dat%>%filter(is.na(DayID)==TRUE) #checks DayID not entered 

# pulls out comments from new cruise and makes list for manual review---------------------------------------
comments<-survey_dat%>%filter(Cruise_ID==C_ID)%>%
  filter(is.na(Comments)==FALSE)

write.csv(comments, 
          paste0(usr,dir,"Analysis/processed_data/",C_ID,"_survey_data_RecordsWithComments.csv")) 


# saves compiled cruise data ----------------------------------------------
mammal_sightings<-survey_dat%>%filter(Animal=="mammal")%>%filter(Cruise_ID==C_ID)
write.csv(mammal_sightings, 
          paste0(usr,dir,"Analysis/processed_data/",C_ID,"_mammals.csv")) #mammals for cruise

write.csv(survey_dat%>%filter(Cruise_ID==C_ID), 
          paste0(usr,dir,"Analysis/processed_data/",C_ID,"_survey_data.csv")) 

saveRDS(survey_dat%>%filter(Cruise_ID==C_ID), 
        paste0(usr,dir,"Analysis/processed_data/",C_ID,"_survey_data.rds")) 

# compiles all cruise data together and saves ----------------------------------------------

Files<-list.files(paste0(usr,dir,"Analysis/processed_data/"),pattern = "_survey_data.rds",full.names = T,recursive = T)

all_survey<-NULL
for (j in 1:length(Files)){
  dat<-readRDS(Files[j])
  all_survey<-rbind(all_survey,dat)
}

saveRDS(all_survey, 
        paste0(usr,dir,"Analysis/processed_data/Survey_data_MOSAIC_ALL.rda")) 
