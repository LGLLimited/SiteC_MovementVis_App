library(tidyverse)
library(lubridate)
library(sf)

# Operational data ####
d_opr <-  readRDS("data/OperationalData_Cleaned_13June2024.rds") %>% 
  as_tibble() %>% 
  # records deemed impossible per Nich and Dave
  filter(!R_ID %in% 3819:3824)

# Fixing Species ===========================

d_opr  <- d_opr %>%
 mutate(Species = 
    case_when(
        Species=="BT" ~ "Bull Trout",
        Species=="RB" ~ "Rainbow Trout",
        Species=="AG" ~ "Arctic Grayling",
        Species=="WP" ~ "Walleye",
        Species=="BB" ~ "Burbot",
        Species=="MW" ~ "Mountain Whitefish",
        TRUE ~ Species
    ))


#all(colnames(readRDS('data/data_operational_20230208.rds') %>% as_tibble()) %in% colnames(d_opr))

min_date <- ymd("2019-04-01")
max_date <- ymd("2023-12-31")

#d_opr 
#d_opr <- readRDS("data/data_operational_10Feb22.rds")
d <- d_opr %>% 
  filter(Type %in% c("Release", "Station", "Mobile","Haul", "PIT"),
         ! Species %in% c("TBD","Unknown","Place Holder", "Mainstreams 2008 fish")) %>% 
  filter((!is.na(Latitude) & !is.na(Longitude))) %>% 
  filter(Dataset=="Present")  %>% 
 mutate(Life_Stage=if_else(Life_Stage=="adult"|Life_Stage==0,"Adult",Life_Stage)) %>%
  filter(between(date(First_Datetime), min_date, max_date))

# Date for ui text
max_data_date <- format(max_date,"%B %d, %Y")

# Map data ####
det_sites <- d %>% 
  filter(Type %in% c("Station","Release","Haul", "PIT")) %>% 
  distinct(Detect_Site,Type, Latitude, Longitude) %>%
  # Converts to a spatial dataframe
  st_as_sf(coords=c("Longitude","Latitude"),crs=4326) %>% 
  filter(!str_detect(Detect_Site,"HIST"))

peace_network <- st_read(dsn = './data',layer="peace_line",quiet=TRUE) %>% 
  st_transform(4326) %>% 
  st_zm() %>% 
  filter(StreamName %in% c("Unnamed Creek","Needham Creek","Pouce Coupe River","Chowade River","Cypress Creek","Farrell Creek","Sukunka River","Burnt River","Graham River","Turnoff Creek","Fiddes Creek","Beatton River","Wolverine River","Kiskatinaw River","Murray River","Roberston Creek","Peace River","Pine River","Halfway River","Moberly River","Maurice Creek","Cameron River")) %>% 
  mutate(lwd=if_else(StreamName=="Peace River","Peace","Trib"))


receivers <- st_read(dsn='./data',layer="fixed_stations",quiet=TRUE) %>% 
  mutate(lon=st_coordinates(.)[,1],
         lat=st_coordinates(.)[,2]) %>% 
  st_set_geometry(NULL)

# Reference points, like Site C dam
location_pts <- st_read(dsn='./data',layer="shp_locations-point", quiet=TRUE) %>% 
  st_zm() %>% 
  filter(StreamName =="Site C Project")#%in% c("Site C Project","Peace Canyon Dam","Many Islands"))

# zone_coord_lut <- read_csv("data/mobile_zone_midpoints_new.csv",show_col_types = FALSE) %>% 
#   rename(ZoneLat=Latitude, 
#          ZoneLong=Longitude)

# This csv is generated in GIS
# some zones need centroids eyeballed manually when zone has multiple river segments within.
zone_coord_lut <- read_csv("data/moblie_zone_centroids_2023.csv",show_col_types = FALSE) %>%
  rename(ZoneLat=Latitude,
         ZoneLong=Longitude)

# Individual data ####
ind_d <- d  %>% #filter(Species=="Rainbow Trout") %>% #filter(Tag_ID==898) 
  select(Tag_ID,Ch,Code,Life_Stage,Species,Detect_Site,First_Datetime,Last_Datetime,Latitude,Longitude) %>% 
  arrange(First_Datetime) %>% 
  pivot_longer(cols = c(First_Datetime,Last_Datetime), names_to = "FirstLast", values_to = "Datetime") %>% 
  mutate(Present=TRUE, 
         yr=year(Datetime)) %>% 
  distinct(Tag_ID, Ch,Code,Species, Life_Stage, Detect_Site, Latitude,Longitude,Datetime,Present,yr) %>% 
  arrange(Tag_ID, Datetime) %>% 
  nest(data=-Tag_ID) %>% 
  mutate(data=map(data, 
          ~.x %>% 
            mutate(haul=if_else(lag(Detect_Site, 1)=="HAUL" | lead(Detect_Site, 1)=="HAUL", "Hauled", "Movement") %>% 
                     replace_na("Movement")))) %>% 
  unnest(data) %>% 
  filter(Latitude!=0&Longitude!=0) %>% 
  #Force MST to UTC
  mutate(Datetime=floor_date(force_tz(Datetime,"UTC"),unit = 'second')) # convert data from MST with milliseconds to UTC with seconds


# Look at number of sites a fish was detected at and the duration in years from  
# from first to last detection to pick interesting fish for individual plots- manual process
# trying to strike a balance between detections at more sites and over a longer duration

# ind_d %>% 
#   distinct(Tag_ID,Life_Stage,Species,Detect_Site) %>%
#   group_by(Tag_ID,Life_Stage,Species) %>% 
#   count(name = "n_sites") %>% #filter(Species=="Rainbow Trout") %>% arrange(desc(n_sites))
# inner_join(
#   ind_d %>% 
#   group_by(Tag_ID) %>% 
#   summarize(YearsFirstLast=time_length(max(Datetime)-min(Datetime),'years')), #%>% #filter(Tag_ID==988), #%>% arrange(desc(DateRange)),
#   by="Tag_ID") %>% 
#   arrange(Species,Life_Stage, desc(n_sites), desc(YearsFirstLast)) %>% 
#   group_by(Species,Life_Stage) %>% 
#   mutate(id=row_number()) %>%
#   filter(id<11) %>%  # top 10 per species/lifestage
#   View() 

# Can also look at tags with the most detections by species/lifestage
most_dets_by_sp <- ind_d  %>% 
  group_by(Tag_ID,Life_Stage,Species) %>% 
  count() %>% 
  group_by(Species,Life_Stage) %>% 
  filter(n==max(n))

#Fish with the most movement and years detected
most_dist_year <- d %>% 
  group_by(Tag_ID, Life_Stage, Species) %>% 
  summarise(
    DiffYears = n_distinct(Detect_Year), 
    min = min(Detect_River_RKM), 
    max=max(Detect_River_RKM),
    .groups = "keep"
  ) %>% 
  mutate(distance = max-min) %>% 
  group_by(Life_Stage, Species) %>% 
  arrange(desc(DiffYears), desc(distance)) %>% 
  slice_head(n=5) 

#Fish with the most movement and years detected
most_dist <- d %>% 
  group_by(Tag_ID, Life_Stage, Species) %>% 
  summarise(
    min = min(Detect_River_RKM), 
    max=max(Detect_River_RKM),
    .groups = "keep"
  ) %>% 
  mutate(distance = max-min) %>% 
  group_by(Life_Stage, Species) %>% 
  arrange(desc(distance)) %>% 
  slice_head(n=5) 

# Fish with the most number of unique detection zones
most_zones <- d %>% 
  group_by(Tag_ID, Life_Stage, Species) %>% 
  summarise(DiffZones = n_distinct(Zone_No),
            .groups = "keep") %>% 
  group_by(Life_Stage, Species) %>% 
  slice_max(DiffZones, n=5) 


filter(most_zones, Tag_ID %in% most_dist$Tag_ID) %>% print(n=Inf)

# Interesting Individuals #########################################################

## Original picks ==============================================================
grayling <- c("511", "434") #Codes: 341, 314 (both fine, both go down moberly, 341 better)
billy <- "898"
bull_trout <- c(billy,"540","544")  #Codes: 496, 208 (really good), 337 (great)

burbot <- c("822","745")  #Codes: 626 (fine), 627 (not good)
rainbow <- c("607","563")  #Codes: 275 (okay), 342 (pretty good)
whitefish <- c("1018", "943")  #Codes: 120 (okay, hauled), 674 (good)
walleye <- c("521", "480")  #Codes: 160 (real good), 162 (great)

## Interesting fish ========================================================
new_bulltrout_dist <- c("809", "637")  #Codes:647 (all the way up halfway twice), 148 (goes up halfway)
new_bulltrout_zone <- c("1107", "612") #Codes: 209 (fantastic example), 269 (fine)
new_bulltrout_dist_year  <- c("829") #Codes: 661 (just back and forth over many years, trouble with dam?)
newgralying <- c("964", "571") #Codes: 695 (pretty good), 249 (better)
newburbot  <- c("485", "632") #Codes: 247 (not good), 236 (bad)
newrainbow <- c("573", "655") #Codes: 238 (good but not special), 215 (good)
newwhite <- c("1015") #Codes: 117 (not interesting)
newwalleye <- c("480", "877", "472", "519") #Codes: 162 (great), 501 (great), 116 (fine), 158 (nope)

## New Interesting fish (20241119) =====================================================================================
NovBull <- c("1139", "476", "941", "613", "568")#(241 #pretty good),  (308 #good), (672 #pretty okay), (233 #good), (331 #not good)
NovBull2 <- c("544", "1101", "613", "829", "960", "540") 
  #(337 & 203 #good if we don't mind it getting stuck), (233 #good), 
                    #(661 #another stuck at dam), (691 #great), (208 #mid)

NovChoosen  <- c("960", "613") #691, 233
## Final picks =================================================================
#bulltroutFin <- c(billy, "540", "544", "1107", "829") #I'd consider 647 as well   #Codes: 496, 208, 337, 209, 661
bullTroutFin  <- c(billy, "540", "613", "829", "960", "1518") #496, 208, 233, 661, 691, 163
graylingFin  <- c("511", "571") #Codes: 341, 249
burbotFin <- c("822","745")  #Codes: 626, 627
rainbowFin <- c("607","563", "655") #Codes 275, 342, 215
whiteFin <- c("1018", "943")  #Codes: 120, 674 
walleyeFin  <- c("521", "480", "877") #Codes: 160, 162, 501

selected_individuals <- 
  c(NovChoosen, bulltroutFin, graylingFin, burbotFin, rainbowFin, whiteFin, walleyeFin)

ind_d <- ind_d %>% 
  filter(Tag_ID %in% selected_individuals) %>% 
  mutate(tagcode=case_when(Ch=="3" ~ paste0("149.360 ", Code),
                          #Ch=="4" ~ paste0("149.440 ", Code),
                           Ch=="5" ~ paste0("149.400 ", Code)),
         fish_name=paste0(Species," ",tagcode))


# Tag sites but no lat-longs for Haul records
# d_opr %>% filter(Type=="Haul") %>% 
#   distinct(Tag_Site)

# Seasonal data ####  
d2 <- d %>% #distinct(Zone_No)
  left_join(zone_coord_lut,by="Zone_No") %>%
  # snap mobile dets to zone river segment centriods in zone_coord_lut
  mutate(Latitude=if_else(Type=="Mobile",ZoneLat, Latitude),
         Longitude=if_else(Type=="Mobile",ZoneLong, Longitude),
         month=month(Last_Datetime,label = TRUE,abbr = FALSE),
         week=week(Last_Datetime)) %>%
    filter(Detect_Year %in% c("2019","2020","2021","2022", "2023", "2024"), month %in% month.name[4:10]) %>% 
  distinct(Tag_ID, Type, Species, Life_Stage, Detect_Site, month, Last_Datetime, Detect_Year, Latitude, Longitude) %>%
  mutate(weekstart=floor_date(Last_Datetime,"week"),
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

d_seas <- lst(Monthly=d_month, Weekly=d_week)
n_seas <- lst(Monthly=n_month, Weekly=n_week)

# Filtering out detections outside of peace_network ############################
newbox <- st_as_sfc(st_bbox(peace_network)) #Creates box containing all of peace network

ind_d_sf <- 
  st_as_sf(ind_d, coords=c("Longitude", "Latitude"), crs=st_crs(peace_network))

d_seas_sf <- 
  map(d_seas, ~.x %>% 
       st_as_sf(coords=c("Longitude", "Latitude"), crs=st_crs(peace_network)))

ind_d_filtered <- st_filter(ind_d_sf, newbox) %>% 
  as_tibble() %>% 
  mutate(
    Latitude = (st_coordinates(geometry)[, 2]), 
    Longitude=  (st_coordinates(geometry)[, 1])
  ) %>% select(colnames(ind_d), -geometry)

d_seas_filtered <- map(d_seas_sf, 
  ~st_filter(.x, newbox) %>% 
  as_tibble() %>% 
  mutate(
    Latitude = (st_coordinates(geometry)[, 2]), 
    Longitude=  (st_coordinates(geometry)[, 1])
  ) %>% select(colnames(d_seas[[1]]), -geometry)
)

## Comparing filtered to non-filtered =========================================
anti_join(ind_d, ind_d_filtered)
map2(d_seas, d_seas_filtered, ~anti_join(.x, .y))

## Setting the values as the old names =========================================
ind_d <- ind_d_filtered
d_seas <- d_seas_filtered

# Saving final app_data.rda ####################################################
save(list = c("location_pts","peace_network","receivers","max_data_date","ind_d","d_seas","n_seas"),file = "data/app_data.rda")

