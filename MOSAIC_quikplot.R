#for error: fatal: could not read Username for 'https://github.com': Device not configured
#run the following line in R Terminal replace user name with yours
#git remote set-url origin git@github.com:raorben/at-sea.git

library(ggplot2)
library(dplyr)
library(data.table)
library(lubridate)
library(trakR)

#for RHAU fancyplot
library(sf)
library(tidyverse)
library(MetBrewer)

if(Sys.info()[7]=="rachaelorben") {usr<-"/Users/rachaelorben";
dir<-"/Library/CloudStorage/Box-Box/Seabird Oceanography Lab/Current_Research/MOSAIC_Seabird At-Sea Observations/"}

if(Sys.info()[7]=="kennerlw") dir<-"/Volumes/GoogleDrive/My Drive/Seabird_Oceanography_Lab/At-SeaSurveys/HALO/Raw Dat"

sp<-read.csv(paste0(usr,dir,"data/SeaLog-Species_CodeList.csv"))
names(sp)<-c("Species_Name","Code","Sci_name","Animal","Unidentified_YN")

# compiles all cruise data together and saves ----------------------------------------------

Files<-list.files(paste0(usr,dir,"Analysis/processed_data/"),pattern = "_survey_data.rds",full.names = T,recursive = T)

all_survey<-NULL
for (j in 1:length(Files)){
  dat<-readRDS(Files[j])
  all_survey<-rbind(all_survey,dat)
}

str(sp$Code)
str(unique(all_survey$Species))
all_survey<-left_join(all_survey,sp%>%select(Code,Unidentified_YN), by=c("Species"="Code"))

saveRDS(all_survey, 
        paste0(usr,dir,"Analysis/processed_data/Survey_data_MOSAIC_ALL.rda")) 

survey_dat<-readRDS(paste0(usr,dir,"Analysis/processed_data/Survey_data_MOSAIC_ALL.rda")) 
unique(survey_dat$Cruise_ID)


# unique IDs for on effort segments ---------------------------------------
# helpful for plotting
# add in filter for "outside of zone" ON effort observations
names(survey_dat)

survey_dat_ON<-survey_dat%>%filter(On.OffTx=="ON")%>%
  distinct(Cruise_ID,datetime,.keep_all = T) %>%
  arrange(Cruise_ID,datetime) %>% 
  mutate(tdiff=as.numeric(datetime)-as.numeric(lag(datetime)),
         gap_hour=tdiff>120, # find times when there is a gap > 2 minutes
         gap_hour=ifelse(is.na(gap_hour),0,gap_hour), #fill NAs
         gapID=(cumsum(gap_hour)), # gap_hour is T/F so cumsum is adding 1 for each T aka giving a index number to each gap
         # gapID=ifelse(gap_hour==T,gapID,""), # Make any gapIDs for which gaps_hours are F an empty string
         # gapID=ifelse(!is.na(gapID),gapID,""),# Make any gapIDs that are NAs an empty string
         Segment_ODid=paste0(as.character(Cruise_ID),"_",gapID)) 
unique(survey_dat_ON$Segment_ODid)
survey_dat_ON$Cruise_Type<-"MOSAIC"
survey_dat_ON$Cruise_Type[survey_dat_ON$Cruise_ID=="202307HL3" | survey_dat_ON$Cruise_ID=="202308HL4"]<-"HAKE"
survey_dat_ON$Cruise_Type[survey_dat_ON$Cruise_ID=="202301HALO" | survey_dat_ON$Cruise_ID=="202307HALO" | survey_dat_ON$Cruise_ID=="202404HALO"]<-"HALO"


# quick summary of off-transect sightings
survey_dat_OFF<-survey_dat%>%filter(On.OffTx=="OFF")%>%filter(Species!="null")
(survey_dat_OFF%>%select(Cruise_ID,Species,Species_Name,datetime, PrimaryBehavior)%>%
    arrange(Cruise_ID))

# quick error checking plot of survey on-effort segments
ggplot()+
  geom_path(data=survey_dat_ON,
            aes(x=Longitude,y=Latitude, color=Segment_ODid,group=Segment_ODid))+
  facet_wrap(~Cruise_ID)+
  theme(legend.position = "none")



# all cruise plots ---------------------------------------------------------
unique(survey_dat_ON$Cruise_ID)
C_ID<-unique(survey_dat_ON$Cruise_ID)[7]

newp<-data.frame(name="Newport",lat=44.620416,lon=-124.056905)
w2hr<-map_data('world')
w2hr_sub<-w2hr[w2hr$region%in%c("USA","Canada"),]

ggplot()+
  geom_polygon(data=w2hr_sub,aes((long),lat,group=group),fill="gray60",color="grey10",linewidth=0.1)+
  geom_path(data=survey_dat_ON,aes(x=Longitude,y=Latitude, group=Segment_ODid))+
  coord_fixed(ratio=1.7,xlim = c(-126.5,-122.9),ylim=c(40.1,46.2))+
  xlab(expression(paste("Longitude (",degree,"W)")))+
  ylab(expression(paste("Latitude (",degree,"N)")))+
  theme_bw()+
  facet_wrap(~Cruise_Type+Cruise_ID, nrow=2)
ggsave(paste0(usr,dir,"/Analysis/maps/On_Effort.jpeg"))


survey_dat_ON$date<-as.factor(date(survey_dat_ON$datetime))
ggplot()+
  geom_polygon(data=w2hr_sub,aes((long),lat,group=group),fill="gray60",color="grey10",linewidth=0.1)+
  geom_path(data=survey_dat_ON%>%filter(Cruise_ID==C_ID),
             aes(x=Longitude,y=Latitude, group=Segment_ODid, color=date))+
  coord_fixed(ratio=1.7,xlim = c(-126.5,-122.9),ylim=c(40.1,46.2))+
  xlab(expression(paste("Longitude (",degree,"W)")))+
  ylab(expression(paste("Latitude (",degree,"N)")))+
  theme_bw()+
  #theme(legend.position = "none")+
  facet_wrap(~Cruise_ID, nrow=1)
ggsave(paste0(usr,dir,"/Analysis/maps/On_Effort_",C_ID,".jpeg"))

ggplot()+
  geom_polygon(data=w2hr_sub,aes((long),lat,group=group),fill="gray60",color="grey10",linewidth=0.1)+
  geom_path(data=survey_dat_ON%>%filter(Cruise_Type=="HAKE"),
            aes(x=Longitude,y=Latitude, group=Segment_ODid, color=date))+
  coord_fixed(ratio=1.7,xlim = c(-126.5,-123),ylim=c(40.1,48.5))+
  xlab(expression(paste("Longitude (",degree,"W)")))+
  ylab(expression(paste("Latitude (",degree,"N)")))+
  theme_bw()
  #theme(legend.position = "none")
ggsave(paste0(usr,dir,"/Analysis/maps/On_Effort_HAKE.jpeg"))

ggplot()+
  geom_polygon(data=w2hr_sub,aes((long),lat,group=group),fill="gray60",color="grey10",linewidth=0.1)+
  geom_path(data=survey_dat_ON%>%filter(Cruise_Type=="HALO"),
            aes(x=Longitude,y=Latitude, group=Segment_ODid, color=date))+
  coord_fixed(ratio=1.7,xlim = c(-126.5,-123),ylim=c(44,46))+
  xlab(expression(paste("Longitude (",degree,"W)")))+
  ylab(expression(paste("Latitude (",degree,"N)")))+
  theme_bw()
#theme(legend.position = "none")
ggsave(paste0(usr,dir,"/Analysis/maps/On_Effort_HALO.jpeg"))


quartz(height=7,width=12)
ggplot()+
  geom_polygon(data=w2hr_sub,aes((long),lat,group=group),fill="gray60",color="grey10",size=0.1)+
  geom_path(data=survey_dat_ON,aes(x=Longitude,y=Latitude, group=Segment_ODid))+
  geom_point(data=survey_dat_ON%>%filter(Species!="null")%>%
               filter(Animal=="bird"),
             aes(x=Longitude,y=Latitude, color=Species, size=Count))+
  coord_fixed(ratio=1.7,xlim = c(-126.5,-122.9),ylim=c(40.1,46.2))+
  xlab(expression(paste("Longitude (",degree,"W)")))+
  ylab(expression(paste("Latitude (",degree,"N)")))+
  theme_bw()+
  facet_wrap(~Cruise_ID, nrow=2)
ggsave(paste0(usr,dir,"/Analysis/maps/SeabirdSightings.jpeg"))

quartz(height=8,width=12)
ggplot()+
  geom_polygon(data=w2hr_sub,aes((long),lat,group=group),fill="gray60",color="grey10",size=0.1)+
  geom_path(data=survey_dat_ON,aes(x=Longitude,y=Latitude, group=Segment_ODid))+
  geom_point(data=survey_dat_ON%>%filter(Species!="null")%>%filter(Animal=="bird"),
             aes(x=Longitude,y=Latitude, color=Species_Name, size=Count))+
  coord_fixed(ratio=1.7,xlim = c(-126.5,-122.9),ylim=c(40.1,46.2))+
  xlab(expression(paste("Longitude (",degree,"W)")))+
  ylab(expression(paste("Latitude (",degree,"N)")))+
  theme_bw()+
  #facet_wrap(~Cruise_ID)+
  NULL
ggsave(width=10,paste0(usr,dir,"/Analysis/maps/SeabirdSightings_FullName.jpeg"))

ggplot()+
  geom_polygon(data=w2hr_sub,aes((long),lat,group=group),fill="gray60",color="grey10",size=0.1)+
  geom_path(data=survey_dat_ON,aes(x=Longitude,y=Latitude, group=Segment_ODid))+
  geom_point(data=survey_dat_ON%>%filter(Species!="null")%>%filter(Animal=="mammal"),
             aes(x=Longitude,y=Latitude, color=Species_Name,size=Count))+
  coord_fixed(ratio=1.7,xlim = c(-126.5,-122.9),ylim=c(40.1,46.2))+
  xlab(expression(paste("Longitude (",degree,"W)")))+
  ylab(expression(paste("Latitude (",degree,"N)")))+
  theme_bw()+
  facet_wrap(~Cruise_ID, nrow=2)
ggsave(paste0(usr,dir,"/Analysis/maps/MammalSightings.jpeg"))


# selects out species with >30 sightings for individual species map -------
species_sum<-survey_dat_ON%>%
  filter(Species!="null")%>%
  filter(Unidentified_YN=="N")%>%
  group_by(Species, Species_Name, Animal)%>%
  summarise(Total_Birds=sum(Count, na.rm=TRUE),Sightings=n())%>%
  arrange(-Sightings)

birds<-species_sum%>%filter(Animal=="bird")%>%filter(Sightings>30)
bird_IDs<-unique(birds$Species)

birds_to_model<-survey_dat_ON%>%filter(Species %in% bird_IDs)
unique(birds_to_model$Species_Name)


birds_to_model$Species<-as.factor(birds_to_model$Species)


C_ID<-unique(survey_dat_ON$Cruise_ID)[7]
quartz(height=7, width=8)
ggplot()+
  geom_polygon(data=w2hr_sub,aes((long),lat,group=group),fill="gray60",color="grey10",size=0.1)+
  geom_path(data=survey_dat_ON%>%select(-Species)%>%filter(Cruise_ID==C_ID),aes(x=Longitude,y=Latitude, group=Segment_ODid))+
  geom_point(data=birds_to_model%>%filter(Cruise_ID==C_ID),
             aes(x=Longitude,y=Latitude, color=Species_Name, size=Count))+
  coord_fixed(ratio=1.7,xlim = c(-126.5,-122.9),ylim=c(40.1,46.2))+
  xlab(expression(paste("Longitude (",degree,"W)")))+
  ylab(expression(paste("Latitude (",degree,"N)")))+
  theme_bw()+
  theme(axis.text.x = element_text(angle = -60, hjust=-.1))
ggsave(paste0(usr,dir,"/Analysis/maps/SeabirdSightings_topSP_sightings30_Cruise",C_ID,"noUNI.jpeg"))

quartz(height=7, width=8)
ggplot()+
  geom_polygon(data=w2hr_sub,aes((long),lat,group=group),fill="gray60",color="grey10",size=0.1)+
  geom_path(data=survey_dat_ON%>%select(-Species)%>%filter(Cruise_Type=="HAKE"),aes(x=Longitude,y=Latitude, group=Segment_ODid))+
  geom_point(data=birds_to_model%>%filter(Cruise_Type=="HAKE"),
             aes(x=Longitude,y=Latitude, color=Species_Name, size=Count))+
  coord_fixed(ratio=1.7,xlim = c(-126.5,-122.9),ylim=c(40.1,48))+
  xlab(expression(paste("Longitude (",degree,"W)")))+
  ylab(expression(paste("Latitude (",degree,"N)")))+
  theme_bw()+
  theme(axis.text.x = element_text(angle = -60, hjust=-.1))
ggsave(paste0(usr,dir,"/Analysis/maps/SeabirdSightings_topSP_sightings30_Cruise_HAKE_noUNI.jpeg"))


# finds high fliers -------------------------------------------------------


unique(survey_dat_ON$FlightHt)
high_birds<-survey_dat_ON%>%filter(FlightHt==">20m")

high_birds_sum<-high_birds%>%group_by(Cruise_ID,Species, Species_Name)%>%
  summarise(n=sum(Count))


ggplot()+
  geom_bar(data=high_birds_sum,aes(x=Species_Name, y=n, fill=Species),stat = "identity")+
  theme(axis.text.x = element_text(angle = -90, hjust=.1))+
  ylab("Count")+
  xlab("")+
  theme(legend.position = "none")+
  #facet_wrap(~Cruise_ID)+
  NULL
ggsave(paste0(usr,dir,"/Analysis/maps/SeabirdSightings_FlightHeight>20m.jpeg"))  


# COMU behavior teser for Brian & Brian -----------------------------------


ggplot()+
  geom_bar(data=survey_dat%>%filter(Species=="COMU")%>%
             filter(is.na(PrimaryBehavior)==FALSE),
           aes(x=Species_Name, fill=PrimaryBehavior),
           position = position_dodge())+
  theme(axis.text.x = element_text(angle = -90, hjust=.1))+
  ylab("Count")+
  xlab("")+
  facet_wrap(~Cruise_ID)
ggsave(paste0(usr,dir,"/Analysis/maps/COMUbehavior.jpeg"))  


ggplot()+
  geom_bar(data=survey_dat%>%filter(Species=="SOSH" | Species=="STSH"| Species=="SSSH")%>%
             filter(is.na(PrimaryBehavior)==FALSE),
           aes(x=Species_Name, fill=PrimaryBehavior),
           position = position_dodge())+
  theme(axis.text.x = element_text(angle = -90, hjust=.1))+
  ylab("Count")+
  xlab("")+
  facet_wrap(~Cruise_ID)
ggsave(paste0(usr,dir,"/Analysis/maps/Shearwater_behavior.jpeg"))


# MAMU sigtings -----------------------------------------------------------
quartz(height=7,width=8)
ggplot()+
  geom_polygon(data=w2hr_sub,aes((long),lat,group=group),fill="gray60",color="grey10",size=0.1)+
  geom_path(data=survey_dat_ON,aes(x=Longitude,y=Latitude, group=Segment_ODid))+
  geom_point(data=survey_dat_ON%>%filter(Species!="null")%>%
               filter(Animal=="bird")%>%filter(Species=="MAMU"),
             aes(x=Longitude,y=Latitude, color=Species, size=Count))+
  coord_fixed(ratio=1.7,xlim = c(-126.5,-122.9),ylim=c(40.1,46.2))+
  xlab(expression(paste("Longitude (",degree,"W)")))+
  ylab(expression(paste("Latitude (",degree,"N)")))+
  theme_bw()+
  facet_wrap(~Cruise_ID, nrow=2)
ggsave(paste0(usr,dir,"/Analysis/maps/MAMUSightings.jpeg"))

survey_dat_ON%>%filter(Species!="null")%>%
  filter(Animal=="bird")%>%filter(Species=="MAMU")%>%
  group_by(Cruise_ID)%>%
  summarise(n_birds=sum(Count),
            n_sightings=n())



# BRAC --------------------------------------------------------------------
quartz(height=7,width=8)
ggplot()+
  geom_polygon(data=w2hr_sub,aes((long),lat,group=group),fill="gray60",color="grey10",linewidth=0.1)+
  geom_path(data=survey_dat_ON,aes(x=Longitude,y=Latitude, group=Segment_ODid))+
  geom_point(data=survey_dat_ON%>%filter(Species!="null")%>%
               filter(Animal=="bird")%>%filter(Species_Name=="Brandt's Cormorant"),
             aes(x=Longitude,y=Latitude, color=Species, size=Count))+
  coord_fixed(ratio=1.7,xlim = c(-126.5,-122.9),ylim=c(40.1,46.2))+
  xlab(expression(paste("Longitude (",degree,"W)")))+
  ylab(expression(paste("Latitude (",degree,"N)")))+
  theme_bw()+
  facet_wrap(~Cruise_ID, nrow=2)
ggsave(paste0(usr,dir,"/Analysis/maps/BRACSightings.jpeg"))

survey_dat_ON%>%filter(Species!="null")%>%
  filter(Animal=="bird")%>%filter(Species_Name=="Brandt's Cormorant")%>%
  group_by(Cruise_ID)%>%
  summarise(n_birds=sum(Count),
            n_sightings=n())

# SHSHs -------------------------------------------------------------------
quartz(height=7,width=8)
ggplot()+
  geom_polygon(data=w2hr_sub,aes((long),lat,group=group),fill="gray60",color="grey10",size=0.1)+
  geom_path(data=survey_dat_ON,aes(x=Longitude,y=Latitude, group=Segment_ODid))+
  geom_point(data=survey_dat_ON%>%filter(Species!="null")%>%
               filter(Animal=="bird")%>%filter(Species=="SOSH"),
             aes(x=Longitude,y=Latitude, color=Species, size=Count))+
  coord_fixed(ratio=1.7,xlim = c(-126.5,-122.9),ylim=c(40.1,46.2))+
  xlab(expression(paste("Longitude (",degree,"W)")))+
  ylab(expression(paste("Latitude (",degree,"N)")))+
  theme_bw()+
  facet_wrap(~Cruise_ID, nrow=2)
ggsave(paste0(usr,dir,"/Analysis/maps/SOSHSightings.jpeg"))

survey_dat_ON%>%filter(Species!="null")%>%
  filter(Animal=="bird")%>%filter(Species=="SOSH")%>%
  group_by(Cruise_ID)%>%
  summarise(n_birds=sum(Count),
            n_sightings=n())

# CAAUs -------------------------------------------------------------------

quartz(height=7,width=8)
ggplot()+
  geom_polygon(data=w2hr_sub,aes((long),lat,group=group),fill="gray60",color="grey10",size=0.1)+
  geom_path(data=survey_dat_ON,aes(x=Longitude,y=Latitude, group=Segment_ODid))+
  geom_point(data=survey_dat_ON%>%filter(Species!="null")%>%
               filter(Animal=="bird")%>%filter(Species=="CAAU"),
             aes(x=Longitude,y=Latitude, color=Species, size=Count))+
  coord_fixed(ratio=1.7,xlim = c(-126.5,-122.9),ylim=c(40.1,46.2))+
  xlab(expression(paste("Longitude (",degree,"W)")))+
  ylab(expression(paste("Latitude (",degree,"N)")))+
  theme_bw()+
  facet_wrap(~Cruise_ID, nrow=2)
ggsave(paste0(usr,dir,"/Analysis/maps/CAAUSightings.jpeg"))

survey_dat_ON%>%filter(Species!="null")%>%
  filter(Animal=="bird")%>%filter(Species=="CAAU")%>%
  group_by(Cruise_ID)%>%
  summarise(n_birds=sum(Count),
            n_sightings=n())

# RHAU & Wind for ARF Proposal--------------------------------------------------------------------
quartz(height=7,width=8)
ggplot()+
  geom_polygon(data=w2hr_sub,aes((long),lat,group=group),fill="gray60",color="grey10",linewidth=0.1)+
  geom_path(data=survey_dat_ON,aes(x=Longitude,y=Latitude, group=Segment_ODid))+
  geom_point(data=survey_dat_ON%>%filter(Species!="null")%>%
               filter(Animal=="bird")%>%filter(Species=="RHAU"),
             aes(x=Longitude,y=Latitude, color=Species, size=Count))+
  coord_fixed(ratio=1.7,xlim = c(-126.5,-122.9),ylim=c(40.1,46.2))+
  xlab(expression(paste("Longitude (",degree,"W)")))+
  ylab(expression(paste("Latitude (",degree,"N)")))+
  theme_bw()+
  facet_wrap(~Cruise_ID, nrow=2)
ggsave(paste0(usr,dir,"/Analysis/maps/RHAUSightings_byCruise.jpeg"))

quartz(height=7,width=8)
#Wind Energy Lease Araes

path.OWF<- paste0(usr,dir,"data/BOEM_Renewable_Energy_Shapefiles_6/BOEM_Wind_Planning_Area_Outlines_10_19_2023.shp")
OWF.lr <- st_read(path.OWF)
OWF.lr <-sf::st_as_sf(OWF.lr )

OWF.lr$ADDITIONAL
#hack to avoid converting other data to sf
OWF.B<-OWF.lr%>%filter(CATEGORY1=="Oregon Call Area")%>%filter(ADDITIONAL=="Oregon Call Area - Brookings")
brook<-OWF.B
lonlat = unlist(map(brook$geometry,1))
brook2<-data.frame(lat=lonlat[176:350],lon=lonlat[1:175])
OWF.C<-OWF.lr%>%filter(CATEGORY1=="Oregon Call Area")%>%filter(ADDITIONAL=="Oregon Call Area - Coos Bay")
coos<-OWF.C
lonlat = unlist(map(coos$geometry,1))
coos2<-data.frame(lat=lonlat[256:510],lon=lonlat[1:255])

states <- map_data("state")
survey_dat_ON$month<-(month(survey_dat_ON$datetime,label=TRUE))
colonies<-data.frame(colony=c("Hunters Island"),
                     lat=c(42.313556),
                     lon=c(-124.425298))

ggplot()+
  #geom_polygon(data=w2hr_sub,aes((long),lat,group=group),fill="gray60",color="grey10",linewidth=0.1)+
  geom_polygon(data=states,aes((long),lat,group=group),fill="gray10",color="grey95",linewidth=0.1)+
  geom_polygon(data=coos2,aes(y=lat,x=lon),fill="gray80")+
  geom_polygon(data=brook2,aes(y=lat,x=lon),fill="gray80")+
  geom_path(data=survey_dat_ON,aes(x=Longitude,y=Latitude, group=Segment_ODid))+
  geom_point(data=survey_dat_ON%>%filter(Species!="null")%>%
               filter(Animal=="bird")%>%filter(Species=="RHAU"),
             aes(x=Longitude,y=Latitude, color=as.factor(month), size=Count))+
  geom_point(data=colonies,
             aes(x=lon,y=lat), color="turquoise", size=3, pch=17)+
  annotate("text", label = "Hunters Island", x = -123.8, y = 42.313556, size = 3, colour = "white")+
  annotate("text", label = "Oregon Call Area", x = -126.35, y = 43.8, size = 3, hjust = 0, colour = "black")+
  annotate("text", label = "Coos Bay", x = -126.35, y = 43.7, size = 3, hjust = 0, colour = "black")+
  annotate("text", label = "Oregon Call Area", x = -126.35, y = 42.2, size = 3, hjust = 0, colour = "black")+
  annotate("text", label = "Brookings", x = -126.35, y = 42.1, size = 3, hjust = 0, colour = "black")+
  scale_color_manual(values=met.brewer("Tam", 4))+
  coord_fixed(ratio=1.7,xlim = c(-126.5,-122.9),ylim=c(41,44.5))+
  xlab(expression(paste("Longitude (",degree,"W)")))+
  ylab(expression(paste("Latitude (",degree,"N)")))+
  theme_bw()+
  theme(legend.title = element_blank())
ggsave(paste0(usr,dir,"/Analysis/maps/RHAUSightings_All_colorMonth.jpeg"))


survey_dat_ON%>%filter(Species!="null")%>%
  filter(Animal=="bird")%>%filter(Species=="RHAU")%>%
  group_by(Cruise_ID)%>%
  summarise(n_birds=sum(Count),
            n_sightings=n())


# BFAL & LAALs -------------------------------------------------------------------

quartz(height=7,width=8)
ggplot()+
  geom_polygon(data=w2hr_sub,aes((long),lat,group=group),fill="gray60",color="grey10",size=0.1)+
  geom_path(data=survey_dat_ON,aes(x=Longitude,y=Latitude, group=Segment_ODid))+
  geom_point(data=survey_dat_ON%>%filter(Species!="null")%>%
               filter(Animal=="bird")%>%filter(Species=="BFAL" | Species=="LAAL"),
             aes(x=Longitude,y=Latitude, color=Species, size=Count))+
  coord_fixed(ratio=1.7,xlim = c(-126.5,-122.9),ylim=c(40.1,46.2))+
  xlab(expression(paste("Longitude (",degree,"W)")))+
  ylab(expression(paste("Latitude (",degree,"N)")))+
  theme_bw()+
  facet_wrap(~Cruise_ID, nrow=2)
ggsave(paste0(usr,dir,"/Analysis/maps/BFAL&LAALSightings.jpeg"))

survey_dat_ON%>%filter(Species!="null")%>%
  filter(Animal=="bird")%>%filter(Species=="BFAL"| Species=="LAAL")%>%
  group_by(Cruise_ID)%>%
  summarise(n_birds=sum(Count),
            n_sightings=n())


# Cruise Summary: Unidentified vs. Identified ----------------------------------------------------------
library(tidyr)
survey_dat_ON%>%filter(Animal=="bird")%>%filter(Bin!="outside of area")%>%
                         group_by(Unidentified_YN,Cruise_Type,Cruise_ID)%>%
  summarise(n=n(),
            totalcount=sum(Count))

survey_dat_ON%>%filter(Animal=="bird")%>%filter(Bin!="outside of area")%>%
  group_by(Unidentified_YN,Cruise_Type)%>%
  summarise(n=n(),
            totalcount=sum(Count))


Unk_long<-survey_dat_ON%>%filter(Animal=="bird")%>%filter(Bin!="outside of area")%>%
  filter(Unidentified_YN=="Y")%>%
  group_by(Unidentified_YN,Cruise_ID,Species_Name)%>%
  summarise(n=n(),
            totalcount=sum(Count))

Unk_Survey<-survey_dat_ON%>%filter(Animal=="bird")%>%filter(Bin!="outside of area")%>%
  filter(Unidentified_YN=="Y")%>%
  group_by(Unidentified_YN,Cruise_ID)%>%
  summarise(n=n(),
            totalcount=sum(Count))

(UNK_wide_sightings<-Unk_long %>% select(-totalcount)%>%
  pivot_wider(names_from = c(Cruise_ID), values_from = c(n), values_fill = 0)%>% 
  ungroup()%>%select(-Unidentified_YN))

(UNK_wide_total<-Unk_long %>%select(-n)%>%
    pivot_wider(names_from = c(Cruise_ID), values_from = c(totalcount), values_fill = 0)%>% 
    ungroup()%>%select(-Unidentified_YN))


survey_dat_ON%>%filter(Animal=="bird")%>%filter(Bin!="outside of area")%>%
  group_by(Unidentified_YN,Cruise_Type,Cruise_ID)%>%
  summarise(n=n(),
            totalcount=sum(Count))



C<-survey_dat_ON%>%filter(Animal=="bird")%>%filter(Bin!="outside of area")%>%
  filter(Cruise_ID=="202310")%>%
  #filter(Cruise_Type=="HAKE")%>%
  filter(Unidentified_YN=="N")
nrow(C)
sum(C$Count)
length(unique(C$Species))
unique(C$Species_Name)

Cu<-survey_dat_ON%>%filter(Animal=="bird")%>%filter(Bin!="outside of area")%>%
  filter(Cruise_ID=="202310")%>%
 #filter(Cruise_Type=="HAKE")%>%
  filter(Unidentified_YN=="Y")
nrow(Cu)
sum(Cu$Count)
length(unique(Cu$Species))
unique(Cu$Species_Name)


# sightings sorted by known & unknown -------------------------------------
(sighting_sum<-survey_dat_ON%>%filter(Animal=="bird")%>%
    select(-Animal)%>%
    filter(Bin!="outside of area")%>%
  filter(Cruise_ID=="202310")%>%
  #filter(Cruise_Type=="HAKE")%>%
  group_by(Species_Name,Unidentified_YN)%>%
  summarise(Sightings=n(),Individuals=sum(Count))%>%
  arrange(Unidentified_YN))
write.csv(sighting_sum, 
          paste0(usr,dir,"Analysis/processed_data/sightingsSum_202310.csv")) 

# HAKE top Species --------------------------------------------------------

quartz(height=7, width=8)
ggplot()+
  geom_polygon(data=w2hr_sub,aes((long),lat,group=group),fill="gray60",color="grey10",size=0.1)+
  geom_path(data=survey_dat_ON%>%select(-Species)%>%filter(Cruise_Type=="HAKE"),aes(x=Longitude,y=Latitude, group=Segment_ODid))+
  geom_point(data=birds_to_model%>%filter(Cruise_Type=="HAKE")%>%filter(Species=="COMU"),
             aes(x=Longitude,y=Latitude, color=Species_Name, size=Count))+
  coord_fixed(ratio=1.7,xlim = c(-126.5,-122.9),ylim=c(40.1,48))+
  xlab(expression(paste("Longitude (",degree,"W)")))+
  ylab(expression(paste("Latitude (",degree,"N)")))+
  theme_bw()+
  theme(axis.text.x = element_text(angle = -60, hjust=-.1))
ggsave(paste0(usr,dir,"/Analysis/maps/SeabirdSightings_COMU_Cruise_HAKE_noUNI.jpeg"))


quartz(height=7, width=8)
ggplot()+
  geom_polygon(data=w2hr_sub,aes((long),lat,group=group),fill="gray60",color="grey10",size=0.1)+
  geom_path(data=survey_dat_ON%>%select(-Species)%>%filter(Cruise_Type=="HAKE"),aes(x=Longitude,y=Latitude, group=Segment_ODid))+
  geom_point(data=birds_to_model%>%filter(Cruise_Type=="HAKE")%>%
               filter(Species=="STSH"|Species=="SOSH" |Species=="BULS"  |Species=="PFSH"),
             aes(x=Longitude,y=Latitude, color=Species_Name, size=Count))+
  coord_fixed(ratio=1.7,xlim = c(-126.5,-122.9),ylim=c(40.1,48))+
  xlab(expression(paste("Longitude (",degree,"W)")))+
  ylab(expression(paste("Latitude (",degree,"N)")))+
  theme_bw()+
  theme(axis.text.x = element_text(angle = -60, hjust=-.1))+facet_wrap(~Species, nrow=1)
ggsave(paste0(usr,dir,"/Analysis/maps/SeabirdSightings_Shearwaters_Cruise_HAKE_noUNI.jpeg"))


quartz(height=7, width=8)
ggplot()+
  geom_polygon(data=w2hr_sub,aes((long),lat,group=group),fill="gray60",color="grey10",size=0.1)+
  geom_path(data=survey_dat_ON%>%select(-Species)%>%filter(Cruise_Type=="HAKE"),aes(x=Longitude,y=Latitude, group=Segment_ODid))+
  geom_point(data=birds_to_model%>%filter(Cruise_Type=="HAKE")%>%
               filter(Species=="BFAL" |Species=="NOFU"),
             aes(x=Longitude,y=Latitude, color=Species_Name, size=Count))+
  coord_fixed(ratio=1.7,xlim = c(-126.5,-122.9),ylim=c(40.1,48))+
  xlab(expression(paste("Longitude (",degree,"W)")))+
  ylab(expression(paste("Latitude (",degree,"N)")))+
  theme_bw()+
  theme(axis.text.x = element_text(angle = -60, hjust=-.1))+facet_wrap(~Species, nrow=1)
ggsave(paste0(usr,dir,"/Analysis/maps/SeabirdSightings_BFAL_NOFU_Cruise_HAKE_noUNI.jpeg"))


# Phalaropes & Wind for ARF Proposal--------------------------------------------------------------------
quartz(height=7,width=8)
ggplot()+
  geom_polygon(data=w2hr_sub,aes((long),lat,group=group),fill="gray60",color="grey10",linewidth=0.1)+
  geom_path(data=survey_dat_ON,aes(x=Longitude,y=Latitude, group=Segment_ODid))+
  geom_point(data=survey_dat_ON%>%filter(Species!="null")%>%
               filter(Animal=="bird")%>%filter(Species=="RNPH" | Species=="REPH" | Species=="UNPH"),
             aes(x=Longitude,y=Latitude, color=Species, size=Count))+
  coord_fixed(ratio=1.7,xlim = c(-126.5,-122.9),ylim=c(40.1,46.2))+
  xlab(expression(paste("Longitude (",degree,"W)")))+
  ylab(expression(paste("Latitude (",degree,"N)")))+
  theme_bw()+
  facet_wrap(~Cruise_ID, nrow=2)
ggsave(paste0(usr,dir,"/Analysis/maps/Phalarope_Sightings_byCruise.jpeg"))


data=survey_dat_ON%>%filter(Species!="null")%>%
  filter(Animal=="bird")%>%filter(Species=="RNPH" | Species=="REPH" | Species=="UNPH")


data%>%group_by(Cruise_ID, Species_Name)%>%
  summarise(n_sightings=n_distinct(datetime),
            n_count=sum(Count))

survey_dat_ON$month<-month(survey_dat_ON$datetime)
data%>%group_by(month,Species_Name)%>%
  summarise(n_sightings=n_distinct(datetime),
            n_count=sum(Count))
data%>%group_by(Species_Name,FlightHt)%>%filter(is.na(FlightHt)==FALSE)%>%
  summarise(n_count=sum(Count))


ggplot()+
  geom_polygon(data=w2hr_sub,aes((long),lat,group=group),fill="gray60",color="grey10",linewidth=0.1)+
  geom_path(data=survey_dat_ON,aes(x=Longitude,y=Latitude, group=Segment_ODid))+
  geom_point(data=survey_dat_ON%>%filter(Species!="null")%>%
               filter(Animal=="bird")%>%filter(Species=="RNPH" | Species=="REPH" | Species=="UNPH"),
             aes(x=Longitude,y=Latitude, color=Species, size=Count))+
  coord_fixed(ratio=1.7,xlim = c(-126.5,-122.9),ylim=c(40.1,46.2))+
  xlab(expression(paste("Longitude (",degree,"W)")))+
  ylab(expression(paste("Latitude (",degree,"N)")))+
  theme_bw()+
  facet_wrap(~month, nrow=1)
ggsave(paste0(usr,dir,"/Analysis/maps/Phalarope_Sightings_byMonth.jpeg"))


ggplot()+
  #geom_polygon(data=w2hr_sub,aes((long),lat,group=group),fill="gray60",color="grey10",linewidth=0.1)+
  geom_polygon(data=states,aes((long),lat,group=group),fill="gray10",color="grey95",linewidth=0.1)+
  geom_polygon(data=coos2,aes(y=lat,x=lon),fill="gray80")+
  geom_polygon(data=brook2,aes(y=lat,x=lon),fill="gray80")+
  geom_path(data=survey_dat_ON,aes(x=Longitude,y=Latitude, group=Segment_ODid))+
  geom_point(data=survey_dat_ON%>%filter(Species!="null")%>%
               filter(Animal=="bird")%>%filter(Species=="RNPH" | Species=="REPH" | Species=="UNPH"),
             aes(x=Longitude,y=Latitude, color=as.factor(month), size=Count))+
  #geom_point(data=colonies,
  #           aes(x=lon,y=lat), color="turquoise", size=3, pch=17)+
  #annotate("text", label = "Hunters Island", x = -123.8, y = 42.313556, size = 3, colour = "white")+
  annotate("text", label = "Oregon Call Area", x = -126.35, y = 43.8, size = 3, hjust = 0, colour = "black")+
  annotate("text", label = "Coos Bay", x = -126.35, y = 43.7, size = 3, hjust = 0, colour = "black")+
  annotate("text", label = "Oregon Call Area", x = -126.35, y = 42.2, size = 3, hjust = 0, colour = "black")+
  annotate("text", label = "Brookings", x = -126.35, y = 42.1, size = 3, hjust = 0, colour = "black")+
  scale_color_manual(values=met.brewer("Tam", 4))+
  coord_fixed(ratio=1.7,xlim = c(-126.5,-122.9),ylim=c(41,44.5))+
  xlab(expression(paste("Longitude (",degree,"W)")))+
  ylab(expression(paste("Latitude (",degree,"N)")))+
  theme_bw()+
  theme(legend.title = element_blank())
ggsave(paste0(usr,dir,"/Analysis/maps/Phalarope_Sightings_All_colorMonth_windares.jpeg"))

ggplot()+
  #geom_polygon(data=w2hr_sub,aes((long),lat,group=group),fill="gray60",color="grey10",linewidth=0.1)+
  geom_polygon(data=states,aes((long),lat,group=group),fill="gray10",color="grey95",linewidth=0.1)+
  geom_polygon(data=coos2,aes(y=lat,x=lon),fill="gray80")+
  geom_polygon(data=brook2,aes(y=lat,x=lon),fill="gray80")+
  geom_path(data=survey_dat_ON,aes(x=Longitude,y=Latitude, group=Segment_ODid))+
  geom_point(data=survey_dat_ON%>%filter(Species!="null")%>%
               filter(Animal=="bird")%>%filter(Species=="RNPH" | Species=="REPH" | Species=="UNPH"),
             aes(x=Longitude,y=Latitude, color=as.factor(Species), size=Count))+
  #geom_point(data=colonies,
  #           aes(x=lon,y=lat), color="turquoise", size=3, pch=17)+
  #annotate("text", label = "Hunters Island", x = -123.8, y = 42.313556, size = 3, colour = "white")+
  annotate("text", label = "Oregon Call Area", x = -126.35, y = 43.8, size = 3, hjust = 0, colour = "black")+
  annotate("text", label = "Coos Bay", x = -126.35, y = 43.7, size = 3, hjust = 0, colour = "black")+
  annotate("text", label = "Oregon Call Area", x = -126.35, y = 42.2, size = 3, hjust = 0, colour = "black")+
  annotate("text", label = "Brookings", x = -126.35, y = 42.1, size = 3, hjust = 0, colour = "black")+
  scale_color_manual(values=met.brewer("Tam", 4))+
  coord_fixed(ratio=1.7,xlim = c(-126.5,-122.9),ylim=c(41,44.5))+
  xlab(expression(paste("Longitude (",degree,"W)")))+
  ylab(expression(paste("Latitude (",degree,"N)")))+
  theme_bw()+
  theme(legend.title = element_blank())
ggsave(paste0(usr,dir,"/Analysis/maps/Phalarope_Sightings_All_colorSpecies_windares.jpeg"))
