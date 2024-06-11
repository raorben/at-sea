library(sf)
library(ggplot2)
library(lubridate)
#library(units)
#library(tmap)
library(trajr) #turning angles
library(tidyr) #replace_na
#library(plyr) # used but specified with ::
library(dplyr)
library(gridExtra)
library(trakR) #for makeTrip (for finding turning angle bouts >10)

if(Sys.info()[7]=="rachaelorben") {usr<-"/Users/rachaelorben";
gitdir<-"/git_repos/at-sea/";

if(Sys.info()[7]=="rachaelorben") dir<-"/Library/CloudStorage/Box-Box/Seabird Oceanography Lab/Current_Research/MOSAIC_Seabird At-Sea Observations/"}
if(Sys.info()[7]=="kennerlw") dir<-"/Volumes/GoogleDrive/My Drive/Seabird_Oceanography_Lab/At-SeaSurveys/HALO/Raw Dat"

#source(paste0(usr,gitdir,"seabird_functions.R"))

#### bring in data ####

sp<-read.csv(paste0(usr,dir,"data/SeaLog-Species_CodeList.csv"), na.strings = "NA")
names(sp)<-c("Species_Name","Species","Sci_name","Animal","Unidentified_YN","Size")
head(sp)

(Files<-list.files(paste0(usr,dir,"Analysis/processed_data"),pattern = "_survey_data.rds",full.names = T,recursive = T))

obs<-NULL
for (j in 1:length(Files)){
  dat<-readRDS(Files[j])
  obs<-rbind(obs,dat)
}

names(obs)
obs<-rename(obs,lon=Longitude)
obs<-rename(obs,lat=Latitude)

#reorder by date and time
obs<-obs%>%group_by(Cruise_ID,DayID) %>% 
  arrange(datetime,.by_group = TRUE)


# add bird size column to obs --------------------------------------------------
obs<-left_join(obs,sp%>%select(Species,Size),by="Species")

# standardize missing value codes (probably not present)
obs[is.na(obs$lat) | obs$lat==99.999,"lat"]<-NA
obs[is.na(obs$lon) | obs$lon==999.999,"lon"]<-NA

obs%>%filter(is.na(lat))
obs%>%filter(is.na(lon))

obs<-obs%>%filter(is.na(lat)==FALSE)
#summary(obs)

#Eliminate "9999" codes in Count (signifying NA or not counted - primarily used for fish/dolphins)
obs[!is.na(obs$Count) & obs$Count ==9999,"Count"]<-NA

# Convert observation Bin into dist (number categories) -------------------------------------------
# 1=0-100m from the ship 
# 2=100-200m " " " 
# 3=200-300m " " "
# 4=outside of this area

unique(obs$Bin)

obs$dist<-obs$Bin
obs$dist[obs$Bin=="0-100"]<-1
obs$dist[obs$Bin=="100-200"]<-2
obs$dist[obs$Bin=="200-300"]<-3
obs$dist[obs$Bin=="outside of area"]<-4
obs$dist<-as.numeric(obs$dist)
unique(obs$dist)

# Adjust Condition to only flag true changes (4=5) ------------------------------
unique(obs$Condition) #"5" "4" "3" "1" "2" NA 
unique(obs$ObsSidePS)

obs$date<-date(obs$datetime)

#1 	Conditions extremely bad
#--Storm-Petrels, Phalaropes, and small auklets cannot reliably be detected to 100 m
#--All individuals of all other species cannot reliably be detected to 200 m 
#2 	Conditions poor
#--All Storm-Petrels, Phalaropes, and small auklets visible to 100 m
#--All individuals of all other species visible to 200 m 
#3 	Conditions fair
#--All Storm-Petrels, Phalaropes, and small auklets visible to 200 m 
#--All individuals of all other species visible to 300 m
#4 	Conditions good
#--All individuals of all species visible out to 300 m
#5 	Conditions excellent
#--All individuals of all species visible out to 300 m +

str(obs$Condition)
obs$Condition[obs$Condition==5]<-4
unique(obs$Condition)

# Condition Check --------------------------------------------------------
# Identify Observations Outside of the Area 

(toFix<-obs%>%filter(is.na(Species)==FALSE)%>% #should be nothing
  filter(is.na(Condition)==TRUE))
write.csv(toFix,paste0(usr,dir,"Analysis/Segmentation/RecordstoFix_noCondition.csv"))

outside<-obs%>%filter(dist==4) #finds observations outside strips
outside$Notes_Ex<-"Recorded outside of 300m in field"

#what do we do about observations that are outside the area - this code gives them dist==4
toosmall.1<-obs%>%filter(Condition==1)%>%
  filter(is.na(Species)==FALSE)%>%
  filter(Size=="Sm")
toosmall.1$Notes_Ex<-"Small bird observed during Condition 1"
obs$dist[obs$Condition==1 & is.na(obs$Species)==FALSE & obs$Size=="Sm"]<-4

outside.1<-obs%>%filter(Condition==1)%>%
  filter(is.na(Species)==FALSE)%>%
  filter(Size=="Lg")%>%
  filter(dist>1)
outside.1$Notes_Ex<-"Large bird observed during Condition 1 farther than 100m from ship"
obs$dist[obs$Condition==1 & is.na(obs$Species)==FALSE & obs$Size=="Lg" & obs$dist>1]<-4

small.2<-obs%>%filter(Condition==2)%>%
  filter(is.na(Species)==FALSE)%>%
  filter(dist==3 | dist==2)%>%
  filter(Size=="Sm")
small.2$Notes_Ex<-"Small bird observed during Condition 2 farther than 100m from ship"
obs$dist[obs$Condition==2 & is.na(obs$Species)==FALSE & obs$Size=="Sm" & obs$dist>1]<-4

outside.2<-obs%>%filter(Condition==2)%>%
  filter(is.na(Species)==FALSE)%>%
  filter(dist==3)%>%
  filter(Size=="Lg")
outside.2$Notes_Ex<-"Large bird observed during Condition 2 farther than 200m from ship"
obs$dist[obs$Condition==2 & is.na(obs$Species)==FALSE & obs$Size=="Lg" & obs$dist==3]<-4

small.3<-obs%>%filter(Condition==3)%>%
  filter(is.na(Species)==FALSE)%>%
  filter(dist==3)%>%
  filter(Size=="Sm")
small.3$Notes_Ex<-"Small bird observed during Condition 3 farther than 200m from ship"
obs$dist[obs$Condition==3 & is.na(obs$Species)==FALSE & obs$Size=="Sm" & obs$dist==3]<-4

Obs_outside_Conditions<-rbind(outside, toosmall.1, outside.1,small.2,outside.2,small.3)
write.csv(Obs_outside_Conditions,paste0(usr,dir,"Analysis/Segmentation/Obs_outside_Conditions_flagged.csv"))

print(Obs_outside_Conditions%>%group_by(Species_Name)%>%filter(Animal=="bird")%>%
  summarise(n=sum(Count, na.rm=TRUE)), n=57)

# Flags: Condition, On/Off, ObsSide -------------------------------------------------------------------
#amazing code that creates a "flag" column for changes: 
#"Condition", "On.OffTx", "ObsSidePS"

obs<-obs %>% ungroup()%>%
  mutate(across(Condition:ObsSidePS, ~ +(lag(.x, default = first(.x)) != .x), .names = "flag_{col}"))

#run twice to get Date (some typos in DayID)
obs<-obs %>% 
  mutate(across(date, ~ +(lag(.x, default = first(.x)) != .x), .names = "flag_{col}"))
obs<-obs %>% 
  mutate(across(Cruise_ID, ~ +(lag(.x, default = first(.x)) != .x), .names = "flag_{col}"))

#NOT: "Beaufort", "Weather", "Type"
#removed meaningless flags since it is easier than adjusting code above
obs<-obs%>%dplyr::select(-flag_Type,-flag_Beaufort,-flag_Weather)

# Flag Turning Angle: calculate turning angles and makes a flag for angles greater than 10 (3 consecutive pts)----------------
obs<-obs%>%filter(is.na(lon)==FALSE) #shouldn't filter anything out, but code retained to double check
  obs%>%filter(is.na(lat))
  obs%>%filter(is.na(datetime))

coords <- data.frame(x=obs$lon,y=obs$lat, times=obs$datetime)
trj <- TrajFromCoords(coords)
plot(trj)

#Calculates the step angles (in radians) of each segment, relative to the previous segment 
turnA<-trajr::TrajAngles(trj, lag = 1, compass.direction = NULL)

obs$turnA<-c(NA,turnA*180/pi,NA) #convert radians to degrees
obs$turnA<-abs(round(obs$turnA,2)) #absolute value & round to 2 digits

obs%>%filter(abs(turnA)>10)%>%
  summarise(n=n())

#finds bouts of turning angles >10 degrees and sequentially numbers them. 
#TripNum=0 is for turning angles <10
obs<-MakeTrip(
  obs,
  ID = "Cruise_ID",
  DistCutOff = 10,
  Dist2Colony = "turnA",
  NumLocCut = 1
)

obs$TripNum[obs$TripNum==0]<-NA
obs$CruiseTN<-paste0(obs$Cruise_ID,"_",obs$TripNum)
obs<-obs %>% 
  group_by(CruiseTN) %>% 
  mutate(flag_Turns = ifelse(turnA == max(turnA), 1,0))
obs$flag_Turns[is.na(obs$TripNum)==TRUE]<-0

obs %>% group_by(CruiseTN) %>% slice(which.max(turnA))%>%select(turnA,flag_Turns)

head(obs)
ggplot()+
  geom_path(data=obs, 
            aes(x=lon, y=lat, group=DayID, color=DayID))+
  geom_point(data=obs%>%filter(flag_Turns==1), 
             aes(x=lon, y=lat))+
  theme(legend.position = "none")+
  NULL+
  facet_wrap(~Cruise_ID, nrow=2)


# Flag GPS break: removed off effort -& flags breaks in GPS crumbs greater than 4min  --------
obs<-obs%>%filter(On.OffTx=="ON") #removes off-effort points here

obs<-obs%>%group_by(Cruise_ID,DayID)%>%
  arrange(datetime,.by_group = TRUE)%>%
  mutate(tdiff_lag=datetime-lag(datetime),
         tdiff_lead=datetime-lead(datetime))
obs$tdiff.s=as.numeric(obs$tdiff_lag)+as.numeric(obs$tdiff_lead*-1)

obs$flag_GPScrumb_break<-0
obs$flag_GPScrumb_break[obs$tdiff.s>60*4]<-1
obs$flag_GPScrumb_break[as.numeric(obs$tdiff_lag)<240]<-0 #reverts flag if observation is close on one side
obs$flag_GPScrumb_break[as.numeric(obs$tdiff_lead)>(-240)]<-0 #reverts flag if observation is close on other side

track_breaks<-obs%>%filter(flag_GPScrumb_break==1)

# Master Flag -------------------------------------------------------------
obs<-obs%>%filter((dist!=4) %>% replace_na(TRUE)) #removes observations outside of the survey area here
obs<-obs%>%filter(On.OffTx=="ON") #removes off-effort points here (already done, but to confirm)

#compile flags into Flag_Event Change (master flag) & sequentially number segments (leg_ID)
head(obs%>%select(starts_with("flag_"))) #prints rows included in Flag_EventChange

obs<-obs %>% 
  mutate(Flag_EventChange = as.numeric(if_any(starts_with("flag_"), ~.x == 1)))

# get the indices of the flags 
s.sel<-which(obs$Flag_EventChange==1)
e.sel<-s.sel-1

sel<-data.frame(s.sel=s.sel,e.sel=c(e.sel[2:length(e.sel)],nrow(obs)))

head(sel); tail(sel) #check which ones don't work
sel$length<-sel$e.sel-sel$s.sel
sel$st<-obs$datetime[sel$s.sel]
sel$ed<-obs$datetime[sel$e.sel]
sel$dur<-sel$ed-sel$st
sel$dur<-round(as.numeric(sel$dur, units="mins"),2)

#looks for short (1pt segments w/ birds since these can't be lines!)
short_segs<-sel%>%filter(length==0) 
short_segsII<-sel%>%filter(dur<5)%>%filter(length!=0) # duration of segments (nothing else done w/ info yet..)

obs_short<-obs[short_segs$s.sel,]
obs_short<-obs_short%>%filter(is.na(Species)==FALSE)

(obs_short_sum<-obs_short%>%filter(is.na(Species)==FALSE)%>%
    group_by(Cruise_ID,date)%>%
    summarise(nflag_date=sum(flag_date),
              nflag_Condition=sum(flag_Condition),
              nflag_On.OffTx=sum(flag_On.OffTx),
              flag_ObsSidePS=sum(flag_ObsSidePS),
              flag_GPScrumb_break=sum(flag_GPScrumb_break)))
write.csv(obs_short,paste0(usr,dir,"Analysis/Segmentation/ShortSegments_Obsflagged&removed.csv"))
write.csv(obs_short_sum,paste0(usr,dir,"Analysis/Segmentation//ShortSegmentsSUM_flagged.csv"))

print(obs_short%>%group_by(Species_Name)%>%filter(Animal=="bird")%>%
        summarise(n=sum(Count, na.rm=TRUE)), n=16)


# Number  legs -------------------------------------------------------
sel<-sel%>%filter(length!=0) #remove legs with only one point within a set of conditions
#sel<-sel%>%filter(dur>5) #removes legs with a duration of <5min

obs$leg_ID<-NA
for (i in 1:(nrow(sel))){
  obs$leg_ID[sel$s.sel[i]:sel$e.sel[i]]<-i
}


# Map spatial lines legs
# convert to spatial lines
leg_sf <- obs %>% filter(is.na(leg_ID)==FALSE) %>%
  st_as_sf(coords = c("lon", "lat"), crs = 4326) %>%
  group_by(Cruise_ID, leg_ID) %>%
  dplyr::summarise(do_union = F) %>%
  st_cast(to = 'LINESTRING')

# length in kilometers
leg_sf$dist_km <- as.numeric(st_length(leg_sf)/1000)

# put all these lengths into a dataframe
leg_length <- data.frame(leg_sf[c("Cruise_ID", "leg_ID", "dist_km")])
hist(leg_sf$dist_km, main = "Length of legs (km)")

plyr::d_ply(leg_sf%>%filter(is.na(leg_ID)==FALSE), ~Cruise_ID, function(d){
  g <- ggplot(d) +
    geom_sf(aes(col = as.factor(leg_ID), group=leg_ID, geometry = geometry)) +
    scale_colour_discrete(name = "#Leg") +
    theme_classic()
  ggsave(g, file = paste0(usr,dir,"Analysis/Segmentation/Leg_Plots/All_transects_leg_", d$Cruise_ID[1], ".png", sep=""), 
         dpi = 300, width = 10, height = 8)
  grid.arrange(g)
})

# Segmentation into 5 km sections ------------------------------------------
ref = 5
obs$day<-as.character(obs$date)

#d<-obs%>%filter(leg_ID==1) #for de-bugs
obs<-obs%>%filter(is.na(leg_ID)==FALSE) #removes legs w/ length 1

seg_df <- plyr::ddply(obs, ~Cruise_ID, .fun = function(day){
  county <- 1
  dday <- plyr::ddply(day, ~leg_ID, function(d){
    d$seg <- county
    d$duration <- c(NA, as.numeric(difftime(d$datetime[2:nrow(d)], d$datetime[1], units = "secs")))
    x <- leg_length[leg_length$leg_ID == d$leg_ID[1] &
                      leg_length$Cruise_ID == d$Cruise_ID[1], "dist_km"] # length of leg d
    if (x <= ref){ # si le leg est plus court que la longueur max du segment alors on ne le divise pas et on laisse seg = count
      county <<- county + 1
      return(d)
    } else { # sinon on va diviser le leg en segments
      nb_segments <- x %/% ref # division euclidienne
      time_tot <- as.numeric(difftime(d$datetime[nrow(d)], d$datetime[1], units = "secs"))
      time_segments <- time_tot %/% nb_segments # number of seconds in the leg divided by the number of segments
      # splits that we should use to get the right duration in each seg
      splits <- seq(1, (time_tot-time_segments + 1), time_segments)
      # equivalent with closest positions in time
      splits_positions <- plyr::aaply(splits, .margins = 1, function(s){
        x <- abs(d$duration - s)
        return(which(x == min(x, na.rm = T))[1]) # take first element in case there are two equally distant points
      })
      # replace NA because first split should always be 1
      splits_positions[1] <- 1
      # add the last position + 1 as the final split
      splits_positions <- c(splits_positions, (nrow(d)+1))
      for (i in 1:(length(splits_positions)-1)){ # I split the dataframe in segments and aa the right seg to each
        d$seg[c(splits_positions[i] : (splits_positions[i+1]-1))] <- county # if there was a rest to the division of the leg time, then the last seconds go with the last segment code
        county <<- county + 1
      }
      return(d)
      # doesn't work, meant to find longer segment and potentially resplit
      # seg_sf <- d %>% 
      #   st_as_sf(coords = c("lon", "lat"), crs = 4326) %>%
      #   group_by(Cruise_ID, seg) %>%
      #   dplyr::summarise(do_union = F) %>%
      #   st_cast(to = 'LINESTRING')
      # # length in kilometers
      # seg_sf$dist_km <- as.numeric(st_length(seg_sf)/1000)
      # if(length(unique(seg_sf$dist_km>8))==2){return(d); break}
    }
  })
  return(dday)
})


# add a unique identifier per segment
seg_df$code_seg <- paste(seg_df$leg_ID, seg_df$seg, sep = "_")


seg_sf <- seg_df %>%
  st_as_sf(coords = c("lon", "lat"), crs = 4326) %>%
  group_by(Cruise_ID, leg_ID, seg) %>%
  dplyr::summarise(do_union = F) %>%
  st_cast(to = 'LINESTRING')

plyr::d_ply(seg_sf, ~Cruise_ID, function(d){
  g <- ggplot(d) +
    geom_sf(aes(geometry = geometry, color = as.character(seg))) +
    scale_colour_discrete(name = "#Segment", guide = "none") +
    theme_classic()
  ggsave(g, file = paste0(usr,dir,"Analysis/Leg_Plots/All_transects_seg_5km_", d$Cruise_ID[1], ".png", sep=""), dpi = 300, width = 6, height = 8)
})

seg_sf <- seg_df %>%
  st_as_sf(coords = c("lon", "lat"), crs = 4326) %>%
  group_by(Cruise_ID, code_seg) %>%
  dplyr::summarise(do_union = F) %>%
  st_cast(to = 'LINESTRING')
seg_sf$dist_km <- as.numeric(st_length(seg_sf)/1000)
seg_df<-left_join(seg_df,seg_sf%>%select(code_seg,dist_km), by="code_seg")


# Condition and Bird Size -------------------------------------------------
#1 	Conditions extremely bad
#--Storm-Petrels, Phalaropes, and small auklets cannot reliably be detected to 100 m
#--All individuals of all other species cannot reliably be detected to 200 m 
#2 	Conditions poor
#--All Storm-Petrels, Phalaropes, and small auklets visible to 100 m
#--All individuals of all other species visible to 200 m 
#3 	Conditions fair
#--All Storm-Petrels, Phalaropes, and small auklets visible to 200 m 
#--All individuals of all other species visible to 300 m
#4 	Conditions good
#--All individuals of all species visible out to 300 m
#5 	Conditions excellent
#--All individuals of all species visible out to 300 m +

seg_df$width_obs<-NA
for (i in 1:nrow(seg_df)){
  if(is.na(seg_df$Size[i])==TRUE) {next}
  if(seg_df$Size[i]=="Lg"){
    seg_df$width_obs[i][seg_df$Condition[i]==1 | seg_df$Condition[i]==2]<-200;
    seg_df$width_obs[i][seg_df$Condition[i]==3 | seg_df$Condition[i]==4]<-300
  }
  
  if(seg_df$Size[i]=="Sm"){
    seg_df$width_obs[i][seg_df$Condition[i]==2]<-100;
    seg_df$width_obs[i][seg_df$Condition[i]==3]<-200;
    seg_df$width_obs[i][seg_df$Condition[i]==4]<-300
  }
}

#to check logic
seg_df%>%filter(is.na(Size)==FALSE)%>%filter(Condition!=4)%>%
  select(Species, Size, Condition, width_obs)

# save
saveRDS(seg_df, file = paste0(usr,dir,"Analysis/Segmentation/All_Obs_5kmSegs_df.rds"))
saveRDS(seg_sf, file = paste0(usr,dir,"Analysis/Segmentation/All_Obs_5kmSegs_sf.rds"))
seg_sf<-readRDS(file = paste0(usr,dir,"Analysis/Segmentation/All_Obs_5kmSegs_sf.rds"))

# summarise segment mean, min, max and variance of lengths
print("Variance and distribution of segment lengths based on 5 km")
print(summary(seg_sf$dist_km))
print(var(seg_sf$dist_km))
hist(seg_sf$dist_km, main = "Distribution of segment lengths", xlab = "distance (km)")


seg_sf_cut<-seg_df%>%filter(dist_km>1)
#calculate survey effort dropped by cruise relative to cutoff selected above
seg_sf_cut%>%group_by(Cruise_ID)%>%
  summarise(LengthDropped=sum(dist_km))
print("Variance and distribution of segment lengths based on 5 km")
print(summary(seg_sf_cut$dist_km))
print(var(seg_sf_cut$dist_km))
hist(seg_sf_cut$dist_km, main = "Distribution of segment lengths", xlab = "distance (km)")


seg_df%>%filter(dist_km>1)%>%filter()
