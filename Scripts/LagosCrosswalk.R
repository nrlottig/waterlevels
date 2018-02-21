library(LAGOSNE)
library(tidyverse)
library(sf)
library(mapview)
NAD83 = '+init=epsg:4269'

#You only need to run the lagos_get once...after that lagos data is saved on your computer in the lagos package
lagos_get()
############
############
#Load LAGOS data and project
dt <- lagos_load('1.087.1')
lakes <- dt$locus %>%
    st_as_sf(coords = c("nhd_long", "nhd_lat")) %>%
    st_set_crs(NAD83) %>%
    left_join(dt$state, by = "state_zoneid")

#good to filter because mapview complains if there are too many points
WI_LAGOS.lakes= lakes %>% filter(state == "WI")

#Load Water Level Crosswalk Table
lagoscrosswalk <- read_csv("Documents/GitHub/waterlevels/lagoscrosswalk.csv")
WILakes = lagoscrosswalk %>% filter(is.na(lagoslakeid)) %>% st_as_sf(coords = c("long", "lat")) %>%
    st_set_crs(NAD83)

#Plot the Data
mapview(WILakes,color="red",col.regions="red") +mapview(WI_LAGOS.lakes,cex=2,color=NULL)                                     


####Attempt Joins on names & Legacy IDs
#name/county info
lakes <- dt$locus %>% left_join(dt$state, by = "state_zoneid") %>% filter(state=="WI") %>% 
    left_join(dt$county, by="county_zoneid") %>% mutate(county_name = as.character(county_name)) %>% 
    mutate(gnis_name=as.character(gnis_name)) %>% select(lagoslakeid,county_name, gnis_name) %>% 
    drop_na(gnis_name)
lagoswibic = dt$lakes_limno %>% select(lagoslakeid,legacyid) %>% left_join(dt$locus,by="lagoslakeid") %>% 
    left_join(dt$state, by = "state_zoneid") %>% filter(state=="WI") %>% select(lagoslakeid,legacyid) %>% 
    mutate(legacyid = as.character(legacyid)) %>% mutate(legacyid = as.numeric(legacyid)) %>% 
    drop_na(legacyid)

#####Get datafile from Wu
lakestable <- read_csv("Desktop/lakestable.csv")
levellakes = lakestable %>% select(-NHDID) %>% mutate(LakeID=as.numeric(floor(LakeID))) %>% 
    left_join(lagoswibic,by=c("LakeID" = "legacyid"))

#Now match up the files that didn't join based on WIBIC
levellakes2 = levellakes %>% filter(is.na(lagoslakeid)) %>% filter(!duplicated(LakeID)) %>% 
    mutate(County=paste(County," County",sep="")) %>% select(LakeID,Name,County)

#Filter out any lake names that occur more than once in any county.
lakes = lakes %>% filter(gnis_name %in% levellakes2$Name) %>% filter(county_name %in% levellakes2$County)
duplakes = lakes %>% group_by(county_name,gnis_name) %>% filter(n()>1) %>% arrange(gnis_name,county_name)
lakes = lakes %>% filter(!(lagoslakeid %in% duplakes$lagoslakeid))


WILakesJoin = lakes %>% right_join(levellakes2,by=c("gnis_name" = "Name","county_name"="County"))
WILakesJoin = WILakesJoin %>% select(LakeID,lagoslakeid) %>% drop_na(lagoslakeid)
length(unique(WILakesJoin$LakeID))
length(unique(WILakesJoin$lagoslakeid))
WILakesJoin = WILakesJoin[!duplicated(WILakesJoin),]

WILakes3 = merge(levellakes,WILakesJoin,by="LakeID",all.x=TRUE)
WILakes4 = WILakes3 %>%  mutate(lagoslakeid.x = ifelse(is.na(lagoslakeid.x), lagoslakeid.y, lagoslakeid.x)) %>%
    select(LakeID,WiscID,lagoslakeid=lagoslakeid.x,lat,long)
write.table(WILakes4,"Desktop/WILakes.csv",sep=",",row.names=FALSE)

library(geosphere)

checklakes = lagoscrosswalk %>% left_join(dt$locus, by="lagoslakeid") %>% rowwise() %>% 
    mutate(dist=distm(c(long,lat),c(nhd_long,nhd_lat),fun = distGeo)) %>% arrange(desc(dist))
hist(log10(checklakes$dist))


out = distm(cbind(checklakes$long,checklakes$lat),cbind(checklakes$nhd_long,checklakes$nhd_lat),fun = distGeo)
