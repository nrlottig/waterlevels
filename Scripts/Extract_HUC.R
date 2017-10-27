library(readr)
hucids <- read.csv("~/Documents/GitHub/waterlevels/data/hucids.csv")
HU4 <- read_csv("data/HU4.txt", col_types = cols(AreaAcres = col_skip(), 
                                                 AreaSqKm = col_skip(), FID = col_skip(), 
                                                 GNIS_ID = col_skip(), Ha = col_skip(), 
                                                 Ha_In_NWI = col_skip(), LAT = col_skip(), 
                                                 Lon = col_skip(), Pct_In_NWI = col_skip(), 
                                                 States = col_skip(), TNMID = col_skip()))
HU8 <- read_csv("data/HU8.txt", col_types = cols(AreaAcres = col_skip(), 
                                                 AreaSqKm = col_skip(), FID = col_skip(), 
                                                 GNIS_ID = col_skip(), 
                                                 Ha = col_skip(), Ha_In_NWI = col_skip(), 
                                                 LAT = col_skip(), Lon = col_skip(), Pct_In_NWI = col_skip(), 
                                                 States = col_skip(), TNMID = col_skip()))
HU12 <- read_csv("data/HU12.txt", col_types = cols(Country = col_skip(), 
                                                   FID = col_skip(), Ha = col_skip(), Ha_In_NWI = col_skip(), 
                                                   Ha_In_USA = col_skip(), LAT = col_skip(), 
                                                   Lon = col_skip(), Name = col_skip(), 
                                                   Pct_In_NWI = col_skip(), Pct_In_USA = col_skip(), 
                                                   TNMID = col_skip()))

HU4$HUC4 = as.numeric(HU4$HUC4)
HU4 = HU4[which(as.numeric(HU4$HUC4) %in% hucids$HUC4),]
hucids = merge(hucids,HU4,by.x="HUC4",by.y="HUC4")
names(hucids)[18] = "hu4_zoneid"
rm(HU4)
HU8$HUC8 = as.numeric(HU8$HUC8)
HU8 = HU8[which(HU8$HUC8 %in% hucids$HUC8),]
hucids = merge(hucids,HU8,by.x="HUC8",by.y="HUC8")
names(hucids)[20] = "hu8_zoneid"
rm(HU8)
HU12$HUC12 = as.numeric(HU12$HUC12)
HU12 = HU12[which(HU12$HUC12 %in% hucids$HUC12),]
hucids = merge(hucids,HU12)
names(hucids)[21] = "hu12_zoneid"
rm(HU12)
hucids = hucids[,c(4,7,8,9,18,20,21)]
