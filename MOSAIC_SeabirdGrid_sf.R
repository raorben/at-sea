library(sf)
library(ggplot2)
library(dplyr)
library(lubridate)
library(units)
library(terra)
library(tmap)

if(Sys.info()[7]=="rachaelorben") {usr<-"/Users/rachaelorben";
gitdir<-"/git_repos/at-sea/";

dir<-"/Library/CloudStorage/Box-Box/Seabird Oceanography Lab/Current_Research/MOSAIC_Seabird At-Sea Observations/"}

if(Sys.info()[7]=="kennerlw") dir<-"/Volumes/GoogleDrive/My Drive/Seabird_Oceanography_Lab/At-SeaSurveys/HALO/Raw Dat"

source(paste0(usr,gitdir,"seabird_functions.R"))

#### SEABIRD_CODES ####

sp<-read.csv(paste0(usr,dir,"data/SeaLog-Species_CodeList.csv"), na.strings = "NA")
names(sp)<-c("Species_Name","Species","Sci_name","Animal","Unidentified_YN","Size")
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
obs<-rename(obs,lon=Longitude)
obs<-rename(obs,lat=Latitude)

#reorder by date and time
obs<-obs%>%group_by(Cruise_ID,DayID) %>% 
  arrange(datetime,.by_group = TRUE)


# add size column to obs --------------------------------------------------

obs<-left_join(obs,sp%>%select(Species,Size),by="Species")

#standardize missing  value codes
obs[is.na(obs$lat) | obs$lat==99.999,"lat"]<-NA
obs[is.na(obs$lon) | obs$lon==999.999,"lon"]<-NA

summary(obs)

#Eliminate "9999" codes in Count (signifying NA or not counted - primarily used for fish/dolphins)
obs[!is.na(obs$Count) & obs$Count ==9999,"Count"]<-NA

# make DISTANCE column from Bin -------------------------------------------
# 1=0-100m from the ship 
# 2=100-200m " " " 
# 3=200-300m " " "
# 4=outside of this area

names(obs)
unique(obs$Bin)

obs$dist<-obs$Bin
obs$dist[obs$Bin=="0-100"]<-1
obs$dist[obs$Bin=="100-200"]<-2
obs$dist[obs$Bin=="200-300"]<-3
obs$dist[obs$Bin=="outside of area"]<-4
obs$dist<-as.numeric(obs$dist)
unique(obs$dist)

outside<-obs%>%filter(dist==4)

# Checks for LAT/LON = NAs ----------------------------------------------
# IF missing locations are identified see Section 2. in Pacific Marine Bird Modeling_v0.9_Abbreviated_for_Tammy
# 2. Fill in OBSERVATION coordinates (LAT and LONG) containing NAs with information from MARMAM table
# Section deleted here since no on-effort sightings were identified without locations

obs%>%filter(is.na(lat))
obs%>%filter(is.na(lon))

# 3. Add TRANSECT ID information to OBSERVATION dataframe based on ON/OFF Tx and changes in "Condition"
unique(obs$Condition) #"5" "4" "3" "1" "2" NA 
unique(obs$ObsSidePS)

obs$date<-date(obs$datetime)


# Adjust Condition to only flag true changes (4=5) ------------------------------
#Calculate variable strip WIDTHS and AREAS based on Condition and bird size class (LG - shearwaters, boobies, etc. 
#SM - phalaropes, storm-petrels, small auklets, shorebirds)
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

#amazing code that creates a "flag" column for changes: 
#"Condition", "On.OffTx", "ObsSidePS"
names(obs)
obs<-obs %>% ungroup()%>%
  mutate(across(Condition:ObsSidePS, ~ +(lag(.x, default = first(.x)) != .x), .names = "flag_{col}"))
#run twice to get Date (some typos in DayID)
obs<-obs %>% 
  mutate(across(date, ~ +(lag(.x, default = first(.x)) != .x), .names = "flag_{col}"))
obs<-obs %>% 
  mutate(across(Cruise_ID, ~ +(lag(.x, default = first(.x)) != .x), .names = "flag_{col}"))

names(obs)
#NOT: "Beaufort", "Weather", "Type"
#removed meaningless flags
obs<-obs%>%dplyr::select(-flag_Type,-flag_Beaufort,-flag_Obs,-flag_Weather)

#removed off Tx locations
obs<-obs%>%filter(On.OffTx=="ON") 

#flags breaks in GPS crumbs greater than 3 min
obs<-obs%>%group_by(Cruise_ID,DayID) %>% 
  arrange(datetime,.by_group = TRUE)%>%
  mutate(tdiff_lag=datetime-lag(datetime),
         tdiff_lead=datetime-lead(datetime))
obs$tdiff.s=as.numeric(obs$tdiff_lag)+as.numeric(obs$tdiff_lead*-1)

obs$flag_GPScrumb_break<-0
obs$flag_GPScrumb_break[obs$tdiff.s>60*4]<-1
obs$flag_GPScrumb_break[as.numeric(obs$tdiff_lag)<200]<-0 #reverts flag if observation is close on one side
obs$flag_GPScrumb_break[as.numeric(obs$tdiff_lead)>(-200)]<-0 #reverts flag if observation is close on other side

#calculate turning angles and flag angles greater than 10 over a X min period. 
#TODO!!! - maybe not needed with sf implimentation

#compile flags into Flag_EventChange (master flag) & sequentially number segments (TRANSECT_ID)
head(obs%>%select(starts_with("flag_"))) #prints rows included in Flag_EventChange

obs<-obs %>% 
  mutate(Flag_EventChange = as.numeric(if_any(starts_with("flag_"), ~.x == 1)))

# get the indices of the flags 
s.sel<-which(obs$Flag_EventChange==1)
e.sel<-s.sel-1

sel<-data.frame(s.sel=s.sel,e.sel=c(e.sel[2:length(e.sel)],nrow(obs)))
head(sel); tail(sel) #check which ones don't work
sel$length<-sel$e.sel-sel$s.sel

#looks for short (1pt segments w/ birds since these can't be lines!)
short_segs<-sel%>%filter(length==0)
obs_short<-obs[short_segs$s.sel,]
obs_short<-obs_short%>%filter(is.na(Species)==FALSE)
names(obs_short)
(obs_short_sum<-obs_short%>%filter(is.na(Species)==FALSE)%>%
    group_by(Cruise_ID,date)%>%
  summarise(nflag_date=sum(flag_date),
            nflag_Condition=sum(flag_Condition),
            nflag_On.OffTx=sum(flag_On.OffTx),
            flag_ObsSidePS=sum(flag_ObsSidePS),
            flag_GPScrumb_break=sum(flag_GPScrumb_break)))
#write.csv(obs_short,paste0(usr,dir,"Analysis/processed_data/ShortSegments_flagged.csv"))
#write.csv(obs_short_sum,paste0(usr,dir,"Analysis/processed_data/ShortSegmentsSUM_flagged.csv"))


# Numbers transects -------------------------------------------------------
sel<-sel%>%filter(length!=0) #remove transects with only one point

obs$Transect_ID<-NA
for (i in 1:(nrow(sel))){
  obs$Transect_ID[sel$s.sel[i]:sel$e.sel[i]]<-i
}

head(obs)


#observations that are not assigned to a Transet_ID
lost_obs<-obs%>%filter(is.na(Transect_ID)==TRUE)%>%filter(is.na(Species)==FALSE)

# calculate inter point distances ------------------------------------------
#check for duplicate GPS segments (e.g. transect 508)
obs.mp<-obs %>% filter(is.na(Transect_ID)==FALSE)%>%
  st_as_sf(coords = c("lon", "lat"), na.fail = FALSE, crs = "epsg:4326") %>%
  ungroup() %>%
  group_by(datetime)%>%
  #summarize() %>%
  #filter(st_geometry_type(.) == "MULTIPOINT") %>%
  st_cast("POINT", keep=TRUE)
str(obs.mp)

dist<-obs.mp%>%st_distance(by_element = TRUE)
dist<-st_distance(obs.mp$geometry,lead(obs.mp$geometry), by_element = T) 
obs.mp$dist<-dist

obs.mp.dist<-obs.mp%>%filter(dist==unit(0, "cm"))%>%filter(tdiff_lead<(-5))
names(obs.mp.dist)
obs.mp.dist.SUM<-obs.mp.dist%>%group_by(Cruise_ID,DayID,Transect_ID)%>%
  summarise(mDt=min(datetime),
            mxDt=max(datetime))
#write.csv(obs.mp.dist.SUM,paste0(usr,dir,"Analysis/processed_data/DuplicateGPS_SUM_flagged.csv"))

# sf - transects to line segments -----------------------------------------
length(unique(obs$Transect_ID))

#makes a move2 object, but maybe not best approach
#obs[is.na(obs$Transect_ID)==TRUE,] #double check for NA Trnsect_IDs
#obs.m2<-mt_as_move2(x=obs%>% dplyr::select(-Vessel,-Record,-oid,-Beaufort,-Weather,-Type,-file),
#                track_id_column = "Transect_ID",
#                time_column = "datetime",
#                coords = c("lon", "lat"))|> sf::st_set_crs(4326L)

#plot(obs.m2[c("Cruise_ID","Condition","ObsSidePS")])
#str(obs.m2)

obs_pt<-obs %>% filter(is.na(Transect_ID)==FALSE)%>%
  st_as_sf(coords = c("lon", "lat"), na.fail = FALSE, crs = "epsg:4326") %>%
  group_by(Transect_ID) %>%
  summarize() %>%
  filter(st_geometry_type(.) == "MULTIPOINT") 
obs_ls<-obs_pt %>% 
  st_cast("LINESTRING", keep=TRUE)

obs_ls %>% group_by(Transect_ID) %>% summarize(n=n())%>%
  filter(n>1)
unique(obs_ls$Transect_ID)

obs_info<-obs%>%ungroup()%>%
  filter(is.na(Transect_ID)==FALSE)%>%
  filter(is.na(Condition)==FALSE)%>%
  group_by(Cruise_ID,date,Transect_ID,Condition,ObsSidePS)%>%
  select(Cruise_ID,date,Transect_ID,Condition,ObsSidePS)%>%
  slice(1)

#finds wonky transects
obs_info %>% group_by(Transect_ID) %>% summarize(n=n())%>%
  filter(n>1)
length(unique(as.numeric(obs_info$Transect_ID)))
summary(obs_info)

#missing 508 since all GPS points are the same
obs_ls<-left_join(obs_ls,obs_info,by="Transect_ID")

str(obs_ls)
plot(obs_ls)

# Condition Check --------------------------------------------------------
# Identify Observations Outside of the Area 

obs%>%filter(is.na(Species)==FALSE)%>%
  filter(is.na(Condition)==TRUE)

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

#dist
# 1=0-100m from the ship 
# 2=100-200m " " " 
# 3=200-300m " " "
# 4=outside of this area

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

Obs_outside_Conditions<-rbind(toosmall.1, outside.1,small.2,outside.2,small.3)
write.csv(Obs_outside_Conditions,paste0(usr,dir,"Analysis/processed_data/Obs_outside_Conditions_flagged.csv"))


# Buffer Transects --------------------------------------------------------
# https://spencerschien.info/post/spatial_buffer/
# need to come back and adjust Port/Starboard by heading of each transect (YIKES!)
plot(obs_ls%>%filter(Condition==1))
plot(obs_ls%>%filter(Condition==2))
plot(obs_ls%>%filter(Condition==3))
plot(obs_ls%>%filter(Condition==4))

plot(obs_ls%>%filter(ObsSidePS=="Port"))
plot(obs_ls%>%filter(ObsSidePS=="Starboard"))

ls<-st_linestring(rbind(c(0,0), c(10,0)))
plot(st_buffer(st_linestring(rbind(c(0,0), c(10,0))), -2, singleSide=T), axes=T)

sf_use_s2(TRUE)
sf_use_s2(FALSE)
obs_ls_planer<-obs_ls%>%st_transform("EPSG:32609") #Zone 10= most of study area

obs_ls_P1<-obs_ls_planer%>%
  filter(Condition==1)
obs_ls_P1.b <- st_buffer(obs_ls_P1, dist = 100, endCapStyle = "FLAT",singleSide = TRUE)
#st_distance(obs_ls_P1%>%filter(Transect_ID==192)) #double check units are m

obs_ls_P2<-obs_ls_planer%>%
  filter(Condition==2)
obs_ls_P2.b <- st_buffer(obs_ls_P2, dist = 200, endCapStyle = "FLAT",singleSide = TRUE)

obs_ls_P3<-obs_ls_planer%>%
  filter(Condition==3)
obs_ls_P3.b300 <- st_buffer(obs_ls_P3, dist = 300, endCapStyle = "FLAT",singleSide = TRUE)

obs_ls_P3<-obs_ls_planer%>%
  filter(Condition==3)
obs_ls_P3.b200 <- st_buffer(obs_ls_P3, dist = 200, endCapStyle = "FLAT",singleSide = TRUE)

obs_ls_P4<-obs_ls_planer%>%
  filter(Condition==4)
obs_ls_P4.b <- st_buffer(obs_ls_P4, dist = 300, endCapStyle = "FLAT",singleSide = TRUE)


##terra implimentation looks the same
vsp <- terra::vect(obs_ls_P3)
buffer_terra <- terra::buffer(vsp, width = 300, capstyle = "FLAT", singlesided = TRUE)
buffer_terra.sf <- sf::st_as_sf(buffer_terra)

#plot to check
#ggplot() +
#  geom_sf(data = obs_ls_P1.b%>%filter(Transect_ID==192), fill = "yellow", color = "gray")+
#  geom_sf(data = obs_ls_P1%>%filter(Transect_ID==192), color = "blue", linewidth = 1)+
#  geom_sf(data = obs_ls_P1.b%>%filter(Transect_ID==192)%>%st_cast("POINT"), color = "orange")

tm_shape(obs_ls_P1.b) +
  tm_borders() +
  tm_facets(by = "Transect_ID")

library(tmap)
tm_shape(obs_ls_P2.b) +
  tm_borders() +
  tm_facets(by = "Transect_ID")

unique(obs_ls_P2.b$Transect_ID)
ggplot() +
  geom_sf(data = obs_ls_P2.b%>%filter(Transect_ID=="507"), fill = "yellow", color = "gray")+
  geom_sf(data = obs_ls_P2%>%filter(Transect_ID=="507"), color = "blue", linewidth = 1)+
  geom_sf(data = obs_ls_P2.b%>%filter(Transect_ID=="507")%>%st_cast("POINT"), color = "orange")+
  geom_sf(data = obs_pt%>%filter(Transect_ID=="507")%>%st_transform("EPSG:32609"), color = "pink")
  

ggplot() +
  geom_sf(data = obs_ls_P2.b%>%filter(Transect_ID=="521"), fill = "yellow", color = "gray")+
  geom_sf(data = obs_ls_P2%>%filter(Transect_ID=="521"), color = "blue", linewidth = 1)+
  geom_sf(data = obs_ls_P2.b%>%filter(Transect_ID=="521")%>%st_cast("POINT"), color = "orange")+
  geom_sf(data = obs_pt%>%filter(Transect_ID=="521")%>%st_transform("EPSG:32609"), color = "pink")

tm_shape(obs_ls_P3.b300) +
  tm_borders() +
  tm_facets(by = "Transect_ID")

tm_shape(obs_ls_P3.b200) +
  tm_borders() +
  tm_facets(by = "Transect_ID")

tm_shape(obs_ls_P4.b) +
  tm_borders() +
  tm_facets(by = "Transect_ID",)


ggplot() +
  geom_sf(data = buffer_terra.sf%>%filter(Transect_ID=="70"), fill = "yellow", color = "gray")+
  geom_sf(data = obs_ls_P3%>%filter(Transect_ID=="70")%>%st_cast("POINT"), color = "blue", linewidth = 1)

A<-obs_ls_P3%>%filter(Transect_ID=="70")%>%st_cast("POINT")
B<-obs%>%filter(Transect_ID=="70")
ggplot()+
  geom_point(data=obs%>%filter(Transect_ID=="70"),aes(x=lon,y=lat,color=datetime))



# perpendicular points ----------------------------------------------------

#calculate HEADING from each OBSERVATION to the starting coordinates of transect (LONG_1,LAT_1)
obs[!is.na(obs$lon_1) 
    & !is.na(obs$Longitude),"HEADING_ORIGIN"] <- geosphere::bearingRhumb(obs[!is.na(obs$lon_1) 
                                                                             & !is.na(obs$Longitude),c("lon_1","lat_1")],
first_obs<-obs%>%group_by(Transect_ID)%>%
  slice_min(datetime)

obs$Head<-NA
for (i in 1:nrow(obs)){
  tID<-obs$Transect_ID[i]
  if(is.na(tID)==TRUE) next
  obs$Head[i]<-geosphere::bearingRhumb(obs[!is.na(obs$lon_1) & !is.na(obs$Longitude),c("lon_1","lat_1")],
  
} 
                                                                                                                                                 obs[!is.na(obs$lon_1) 
                                                                             
# calculate the perpendicular distance (DIST_PERP) of each OBSERVATION from the TRANSECT line,
# where DIST_ORIGIN represents the hypotenuse and HEADING_DIFF 
# represents the interior angle (THETA) in a right triangle
obs$DIST_PERP <- obs$DIST_ORIGIN * abs(sin(obs$HEADING_DIFF * pi/180))



