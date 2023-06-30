 library(ggplot2)
library(dplyr)
library(data.table)
library(lubridate)
library(trakR)

if(Sys.info()[7]=="rachaelorben") dir<-"/Users/rachaelorben/Box/Seabird Oceanography Lab/Current_Research/MOSAIC_Seabird At-Sea Observations/"
#if(Sys.info()[7]=="will") dir<-"/Volumes/GoogleDrive/My Drive/Seabird_Oceanography_Lab/At-SeaSurveys/HALO/Raw Dat"

survey_dat<-readRDS(paste0(dir,"Analysis/processed_data/Survey_data_MOSAIC_ALL.rda")) 


# unique IDs for on effort segments ---------------------------------------
# helpful for plotting
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
newp<-data.frame(name="Newport",lat=44.620416,lon=-124.056905)
w2hr<-map_data('world')
w2hr_sub<-w2hr[w2hr$region%in%c("USA","Canada"),]

ggplot()+
  geom_polygon(data=w2hr_sub,aes((long),lat,group=group),fill="gray60",color="grey10",size=0.1)+
  geom_path(data=survey_dat_ON,aes(x=Longitude,y=Latitude, group=Segment_ODid))+
  coord_fixed(ratio=1.7,xlim = c(-126.5,-122.9),ylim=c(40.1,46.2))+
  xlab(expression(paste("Longitude (",degree,"W)")))+
  ylab(expression(paste("Latitude (",degree,"N)")))+
  theme_bw()+
  facet_wrap(~Cruise_ID)
ggsave(paste0(dir,"/Analysis/maps/On_Effort.jpeg"))

quartz(height=7,width=8)
ggplot()+
  geom_polygon(data=w2hr_sub,aes((long),lat,group=group),fill="gray60",color="grey10",size=0.1)+
  geom_path(data=survey_dat_ON,aes(x=Longitude,y=Latitude, group=Segment_ODid))+
  geom_point(data=survey_dat_ON%>%filter(Species!="null")%>%filter(Animal=="bird"),
             aes(x=Longitude,y=Latitude, color=Species, size=Count))+
  coord_fixed(ratio=1.7,xlim = c(-126.5,-122.9),ylim=c(40.1,46.2))+
  xlab(expression(paste("Longitude (",degree,"W)")))+
  ylab(expression(paste("Latitude (",degree,"N)")))+
  theme_bw()+
  facet_wrap(~Cruise_ID)
ggsave(paste0(dir,"/Analysis/maps/SeabirdSightings.jpeg"))

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
  facet_wrap(~Cruise_ID)
ggsave(width=10,paste0(dir,"/Analysis/maps/SeabirdSightings_FullName.jpeg"))

ggplot()+
  geom_polygon(data=w2hr_sub,aes((long),lat,group=group),fill="gray60",color="grey10",size=0.1)+
  geom_path(data=survey_dat_ON,aes(x=Longitude,y=Latitude, group=Segment_ODid))+
  geom_point(data=survey_dat_ON%>%filter(Species!="null")%>%filter(Animal=="mammal"),
             aes(x=Longitude,y=Latitude, color=Species_Name,size=Count))+
  coord_fixed(ratio=1.7,xlim = c(-126.5,-122.9),ylim=c(40.1,46.2))+
  xlab(expression(paste("Longitude (",degree,"W)")))+
  ylab(expression(paste("Latitude (",degree,"N)")))+
  theme_bw()+
  facet_wrap(~Cruise_ID)
ggsave(paste0(dir,"/Analysis/maps/MammalSightings.jpeg"))


# selects out species with >30 sightings for individual species map -------
species_sum<-survey_dat_ON%>%
  filter(Species!="null")%>%
  group_by(Species, Species_Name, Animal)%>%
  summarise(Total_Birds=sum(Count, na.rm=TRUE),Sightings=n())%>%
  arrange(-Sightings)

birds<-species_sum%>%filter(Animal=="bird")%>%filter(Sightings>30)
bird_IDs<-unique(birds$Species)

birds_to_model<-survey_dat_ON%>%filter(Species %in% bird_IDs)
C_ID<-unique(birds_to_model$Cruise_ID)[2]

birds_to_model$Species<-as.factor(birds_to_model$Species)

quartz(height=7, width=6)
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
ggsave(paste0(dir,"/Analysis/maps/SeabirdSightings_topSP_sightings30_Cruise",C_ID,".jpeg"))


unique(survey_dat_ON$FlightHt)
high_birds<-survey_dat_ON%>%filter(FlightHt==">20m")

high_birds_sum<-high_birds%>%group_by(Cruise_ID,Species, Species_Name)%>%
  summarise(n=sum(Count))


ggplot()+
  geom_bar(data=high_birds_sum,aes(x=Species_Name, y=n, fill=Species),stat = "identity")+
  theme(axis.text.x = element_text(angle = -90, hjust=.1))+
  ylab("Count")+
  xlab("")+
  theme(legend.position = "none")+facet_wrap(~Cruise_ID)
ggsave(paste0(dir,"/Analysis/maps/SeabirdSightings_FlightHeight>20m.jpeg"))  


# COMU behavior teser for Brian & Brian
ggplot()+
  geom_bar(data=survey_dat%>%filter(Species=="COMU")%>%
             filter(is.na(PrimaryBehavior)==FALSE),
           aes(x=Species_Name, fill=PrimaryBehavior),
           position = position_dodge())+
  theme(axis.text.x = element_text(angle = -90, hjust=.1))+
  ylab("Count")+
  xlab("")+
  facet_wrap(~Cruise_ID)
ggsave(paste0(dir,"/Analysis/maps/COMUbehavior_AugOct2022_Orben.jpeg"))  

# COMU behavior teser for Brian & Brian
ggplot()+
  geom_bar(data=survey_dat%>%filter(Species=="SOSH" | Species=="STSH"| Species=="SSSH")%>%
             filter(is.na(PrimaryBehavior)==FALSE),
           aes(x=Species_Name, fill=PrimaryBehavior),
           position = position_dodge())+
  theme(axis.text.x = element_text(angle = -90, hjust=.1))+
  ylab("Count")+
  xlab("")+
  facet_wrap(~Cruise_ID)
ggsave(paste0(dir,"/Analysis/maps/Shearwater_behavior_AugOct2022_Orben.jpeg"))  

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
  facet_wrap(~Cruise_ID)
ggsave(paste0(dir,"/Analysis/maps/MAMUSightings.jpeg"))

survey_dat_ON%>%filter(Species!="null")%>%
  filter(Animal=="bird")%>%filter(Species=="MAMU")%>%
  group_by(Cruise_ID)%>%
  summarise(n_birds=sum(Count),
            n_sightings=n())
