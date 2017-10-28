Hu12_lulc <- read_delim("~/Dropbox/CSI_LIMNO/CSI_LAGOS-exports/LAGOS-NE-EDI/LAGOS-NE-GEO-EXPORT/original_uploads/LAGOSNE_hu12_lulc105.txt", 
                        "\t", escape_double = FALSE, trim_ws = TRUE)
hu4_lulc <- read.delim("~/Dropbox/CSI_LIMNO/CSI_LAGOS-exports/LAGOS-NE-EDI/LAGOS-NE-GEO-EXPORT/original_uploads/LAGOSNE_hu4_lulc105.txt")
hu8_lulc <- read.delim("~/Dropbox/CSI_LIMNO/CSI_LAGOS-exports/LAGOS-NE-EDI/LAGOS-NE-GEO-EXPORT/original_uploads/LAGOSNE_hu8_lulc105.txt")
hu8_chag <- read.delim("~/Dropbox/CSI_LIMNO/CSI_LAGOS-exports/LAGOS-NE-EDI/LAGOS-NE-GEO-EXPORT/original_uploads/LAGOSNE_hu8_chag105.txt")

Hu12_lulc =Hu12_lulc[,c(1,4,81:112,149,153)]
Hu12_lulc = Hu12_lulc[,c(1,2,4,6,8,10,12,14,16,18,20,22,24,26,28,30,32,34:36)]
Hu12_lulc = Hu12_lulc[which(Hu12_lulc$hu12_zoneid %in% hucids$hu12_zoneid),]
hu12_chag = hu12_chag[which(hu12_chag$hu12_zoneid %in% hucids$hu12_zoneid),]
hu12_chag = hu12_chag[,c(1:5,78:85,98:143)]
hu12_chag = hu12_chag[,c(1:17,19,seq(from=21,to=59,by=2))]
huc12 = merge(Hu12_lulc,hu12_chag)
rm(hu12_chag)
rm(Hu12_lulc)   
hu4_lulc = hu4_lulc[,c(1,4,82:112,149,153)]
hu4_lulc = hu4_lulc[,c(1,2,seq(from=3,to=33,by=2),34,35)]
hu4_lulc = hu4_lulc[which(hu4_lulc$hu4_zoneid %in% hucids$hu4_zoneid),]
hu4_chag = hu4_chag[which(hu4_chag$hu4_zoneid %in% hucids$hu4_zoneid),]
hu4_chag = hu4_chag[,c(1:5,78:85,98:143)]
hu4_chag = hu4_chag[,c(1:17,19,seq(from=21,to=59,by=2))]
hu4 = merge(hu4_chag,hu4_lulc)
rm(hu4_chag)
rm(hu4_lulc)

hu8_lulc = hu8_lulc[,c(1,4,81:112,149,153)]
hu8_lulc = hu8_lulc[,c(1,2,4,6,8,10,12,14,16,18,20,22,24,26,28,30,32,34:36)]
hu8_lulc = hu8_lulc[which(hu8_lulc$hu8_zoneid %in% hucids$hu8_zoneid),]
hu8_chag = hu8_chag[,c(1:5,78:85,98:143)]
hu8_chag = hu8_chag[,c(1:17,19,seq(from=21,to=59,by=2))]
hu8_chag = hu8_chag[which(hu8_chag$hu8_zoneid %in% hucids$hu8_zoneid),]
hu8 = merge(hu8_chag,hu8_lulc)
rm(hu8_chag)
rm(hu8_lulc)
