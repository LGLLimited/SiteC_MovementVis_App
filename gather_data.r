

d_opr <- readRDS("data/data_operational_10Feb22.rds")

d <- d_opr %>% 
  filter(Type %in% c("Release", "Station", "Mobile","Haul"),
         ! Species %in% c("TBD","Unknown")) %>% 
  filter((!is.na(Latitude) & !is.na(Longitude))) %>% 
  mutate(Life_Stage=if_else(Life_Stage=="adult","Adult",Life_Stage))


det_sites <- d %>% 
  filter(Type %in% c("Station","Release","Haul")) %>% 
  distinct(Detect_Site,Type, Latitude, Longitude) %>%
  st_as_sf(coords=c("Longitude","Latitude"),crs=4326) %>% 
  filter(!str_detect(Detect_Site,"HIST"))

peace_network <- st_read(dsn = './data',layer="peace_line") %>% 
  st_transform(4326) %>% 
  st_zm() %>% 
  filter(StreamName %in% c("Chowade River","Cypress Creek","Farrell Creek","Sukunka River","Burnt River","Graham River","Turnoff Creek","Fiddes Creek","Beatton River","Wolverine River","Kiskatinaw River","Murray River","Roberston Creek","Peace River","Pine River","Halfway River","Moberly River","Maurice Creek","Cameron River")) %>% 
  mutate(lwd=if_else(StreamName=="Peace River","Peace","Trib"))

location_pts <- st_read(dsn='./data',layer="shp_locations-point") %>% 
  st_zm() %>% 
  filter(StreamName =="Site C Project")#%in% c("Site C Project","Peace Canyon Dam","Many Islands"))

zone_coord_lut <- read_csv("data/mobile_zone_midpoints.csv") %>% 
  rename(ZoneLat=Latitude, 
         ZoneLong=Longitude)

ind_d <- d  %>% #filter(Tag_ID==898)
   # present data
  # left_join(zone_coord_lut,by="Zone_No") %>% # snap mobile dets to zone river segment centriods in zone_coord_lut
  # mutate(Latitude=if_else(Type=="Mobile",ZoneLat,Latitude),
  #        Longitude=if_else(Type=="Mobile",ZoneLong,Longitude)) %>% 
  select(Tag_ID,Ch,Code,Life_Stage,Species,Detect_Site,First_Datetime,Last_Datetime,Latitude,Longitude) %>% 
  arrange(First_Datetime) %>% 
  pivot_longer(cols = c(First_Datetime,Last_Datetime), names_to = "FirstLast", values_to = "Datetime") %>% 
  mutate(Present=TRUE, 
         yr=year(Datetime)) %>% 
  filter(between(yr,2019,2021)) %>%
  distinct(Tag_ID, Ch,Code,Species, Life_Stage, Detect_Site, Latitude,Longitude,Datetime,Present,yr) %>% 
  arrange(Tag_ID, Datetime) %>% 
  nest(data=-Tag_ID) %>% 
  mutate(data=map(data, 
          ~.x %>% 
            mutate(haul=if_else(lag(Detect_Site, 1)=="HAUL" | lead(Detect_Site, 1)=="HAUL", "Hauled", "Movement") %>% 
                     replace_na("Movement")))) %>% 
  unnest(data) %>% 
  filter(Latitude!=0&Longitude!=0)

#ind_d %>% filter(Tag_ID==1018)

most_tags_by_sp <- ind_d  %>% group_by(Tag_ID,Life_Stage,Species) %>% count() %>% group_by(Species) %>% filter(n==max(n)) %>% pull(Tag_ID) %>% c("1018")
billy <- "898"


ind_d <- ind_d %>% 
  filter(Tag_ID %in% c(billy,most_tags_by_sp)) %>% 
  mutate(tagcode=case_when(Ch=="3" ~ paste0("149.360 ", Code),
                           #Ch=="4" ~ paste0("149.440 ", Code),
                           Ch=="5" ~ paste0("149.400 ", Code)),
         fish_name=paste0(Species," ",tagcode)) #Billy the Bull Trout", paste0(Species," #",Tag_ID)))


# Tag sites but no lat-longs for Haul records
# d_opr %>% filter(Type=="Haul") %>% 
#   distinct(Tag_Site)
  
d2 <- d %>%
  left_join(zone_coord_lut,by="Zone_No") %>%
  # snap mobile dets to zone river segment centriods in zone_coord_lut
  mutate(Latitude=if_else(Type=="Mobile",ZoneLat, Latitude),
         Longitude=if_else(Type=="Mobile",ZoneLong, Longitude),
         month=month(Last_Datetime,label = TRUE,abbr = FALSE),
         week=week(Last_Datetime)) %>%
    filter(Detect_Year %in% c("2019","2020","2021"), month %in% month.name[4:10]) %>% 
  distinct(Tag_ID, Type, Species, Life_Stage, Detect_Site, month, Last_Datetime, Detect_Year, Latitude, Longitude) %>%
  mutate(#month=factor(month,levels=month.name[4:10],ordered = TRUE),
         weekstart=floor_date(Last_Datetime,"week"),
         monthstart=floor_date(Last_Datetime,"month")) %>%
    mutate(MoYr=format(monthstart, "%B %Y"),
           WkYr=format(weekstart, "%b %d %Y")) %>% 
  filter(Type!="Haul")

n_month <- d2 %>% 
  distinct(Tag_ID, Species, Life_Stage, MoYr) %>% 
  group_by(Species, Life_Stage, MoYr) %>% 
  count()%>% 
  rename(Time=MoYr)

d_month <- d2 %>% 
  distinct(Tag_ID,Species,Detect_Site,Life_Stage,Type,monthstart,MoYr,Latitude,Longitude) %>% #
  #filter(MoYr=="September 2019") %>% filter(Species=="Bull Trout",Life_Stage=="Juvenile") %>% 
    group_by(Species,Type,monthstart,Life_Stage,Detect_Site, MoYr,Latitude, Longitude) %>% 
    count() %>%
    ungroup() %>% 
    arrange(monthstart) %>% 
  rename(Time=MoYr,
         Timestart=monthstart)

n_week <- d2 %>% 
  distinct(Tag_ID, Species, Life_Stage, WkYr) %>% 
  group_by(Species, Life_Stage, WkYr) %>% 
  count() %>% 
  rename(Time=WkYr)

d_week <- d2 %>%
  distinct(Tag_ID,Species,Detect_Site,Life_Stage,Type,weekstart,WkYr,Latitude,Longitude) %>%
  group_by(Species,Type,weekstart,Life_Stage,Detect_Site, WkYr,Latitude, Longitude) %>% 
  count() %>%
  ungroup() %>% 
  arrange(weekstart) %>% 
  rename(Time=WkYr,
         Timestart=weekstart)

d_seas <- lst(d_month, d_week)
n_seas <- lst(n_month, n_week)
# d2 %>% mutate(weekstart=floor_date(Last_Datetime,unit="week")) %>% View()
#   group_by(Species, Life_Stage, Detect_Site, week, Detect_Year, MoYr,Latitude, Longitude) %>% 
#   count() %>%
#   ungroup() %>% 
#   arrange(Detect_Year,week) %>% filter(Species=="Arctic Grayling") #%>% View()
# d2 <- d %>%
#   left_join(zone_coord_lut,by="Zone_No") %>% 
#   # snap mobile dets to zone river segment centriods in zone_coord_lut
#   mutate(Latitude=if_else(Type=="Mobile",ZoneLat, Latitude),
#          Longitude=if_else(Type=="Mobile",ZoneLong, Longitude)) %>% 
#   distinct(Tag_ID, Type, Species, Life_Stage, Detect_Site, Last_Datetime, Detect_Year, Latitude, Longitude) %>% 
#   group_by(Species,Life_Stage,Detect_Site, month=month(Last_Datetime,label = TRUE,abbr = FALSE), Detect_Year, Latitude, Longitude) %>% 
#   count() %>% 
#   ungroup() %>% 
#   mutate(month=factor(month,levels=c("April","May","June","July","August","September","October"))) %>%  
#   filter(Detect_Year %in% c("2019","2020","2021"), month %in% month.name[4:10]) %>% 
#   mutate(date=mdy(paste0(month,"-1-",Detect_Year))) %>% 
#   arrange(date) %>% 
#   mutate(MoYr=paste0(month," ",Detect_Year)) %>% 
#   rename(count_n=n)

