library(tidyverse)
library(readxl)
library(LAGOSNE)
library(randomForest)
library(bestNormalize)

#FUNCTIONS
remove_zero_cols <- function(df) {
rem_vec <- NULL
df <- as.data.frame(df)
for(i in 1:ncol(df)){
  if(is.numeric(df[,i])==TRUE){
    this_sum <- summary(df[,i])
    # zero_test <- length(which(this_sum == 0))
    if(this_sum[[3]] == 0) {                #removes column if 50% of data are zeros
      rem_vec[i] <- names(df)[i]
      }
    }
  }  
features_to_remove <- rem_vec[!is.na(rem_vec)]
rem_ind <- which(names(df) %in% features_to_remove)
df <- df[,-rem_ind]
return(df)
}

#rawdata
ecocontext_riparian <- read_excel("data/HYDRO_WATERBODY_seepage_24oct2018.xlsx",sheet = "primary_variables")

dat <- ecocontext_riparian %>% select(-FID,
                                      -ID,
                                      -OBJECTID,
                                      -WATERBODY_,
                                      -HYDROID,
                                      -HYDROCODE,
                                      -HYDROTYPE,
                                      -LANDLOCK_C,
                                      -WiscID,
                                      -County,
                                      -hydro24k,
                                      -centroid_x,
                                      -centroid_y,
                                      -problem,
                                      -landscapep,
                                      -NATURAL_CO,
                                      -HYDROLOGY,
                                      -Lake_type,
                                      -Katie_clas,
                                      -Katie_note,
                                      -R_LU06_MISSING) %>% 
  mutate(MeanDepth = replace(MeanDepth,MeanDepth==0,NA)) %>% 
  select(everything(),-contains("LU11")) %>% 
  mutate(w_develop= W_LU06_21+W_LU06_22+W_LU06_23+W_LU06_24) %>% 
  mutate(w_forest = W_LU06_41+W_LU06_42+W_LU06_43) %>% 
  mutate(w_ag = W_LU06_81+W_LU06_82) %>%
  mutate(w_wetland = W_LU06_90+W_LU06_95) %>% 
  mutate(r_develop= R_LU06_21+R_LU06_22+R_LU06_23+R_LU06_24) %>% 
  mutate(r_forest = R_LU06_41+R_LU06_42+R_LU06_43) %>% 
  mutate(r_ag = R_LU06_81+R_LU06_82) %>%
  mutate(r_wetland = R_LU06_90+R_LU06_95) %>% 
  filter(WBIC != 783730) %>% 
  filter(WBIC != 2014700) %>% 
  filter(WBIC != 2017500)

#get Conductivity data
Conductivity_final <- read_csv("data/Conductivity_final.csv")
dat_cond <- Conductivity_final %>% select(WBIC,final_value) %>% rename(cond=final_value)
dat <- dat %>% left_join(dat_cond)
#get Ca and Mg Data
CaMg <- read_csv("data/CaMg_20181109.csv")
dat_cations <- CaMg %>% select(WBIC,mg_average,ca_average) %>% 
  distinct(WBIC,.keep_all = TRUE)
dat <- dat %>% left_join(dat_cations)
#get elevation data
lake_elevation <- read_excel("data/seepage_lake_shed_elev.xlsx")
dat_elevation <- lake_elevation %>% select(WBIC,lake_MEAN,watershed_MIN,elevation_difference)
dat <- dat %>% left_join(dat_elevation)
#get ecoregion data
regression_coefs_20180328_eco_land <- read_excel("data/regression_coefs_20180328_eco_land.xlsx")
regression_coefs_20180328_eco_land <- regression_coefs_20180328_eco_land[!duplicated(regression_coefs_20180328_eco_land$WBIC),] %>% 
  select(-WiscID)
dat <- dat %>% left_join(regression_coefs_20180328_eco_land)
#get watershed slope
seepage_lake_slope_data <- read_csv("data/seepage_lake_slope_data.csv") %>% 
  select(-X1,-WiscID,-LakeName) %>% 
  mutate(mean_ann_wdrl_gals = replace(mean_ann_wdrl_gals,is.na(mean_ann_wdrl_gals),0)) %>%
  distinct(WBIC,.keep_all = TRUE)
dat <- dat %>% left_join(seepage_lake_slope_data)

#reduce data to seepage lakes in cdev data
regressionstats <- read_csv("BayHModels/regressionstats.csv")
seepage_20180414 <- read.csv("big_data/seepage_20180414.csv",stringsAsFactors = F) %>% select(WBIC,WiscID) %>% distinct()
dat.lakes <- regressionstats %>% left_join(seepage_20180414) %>% select(-slope)

dat <- dat %>% right_join(dat.lakes)

#cleanup data 
dat <- remove_zero_cols(as.data.frame(dat))
summary(dat)
dat <- dat %>%   
  mutate(SDI = SHAPE_LEN/(2*sqrt(pi*SHAPE_AREA))) %>% 
  mutate(area_depth_ratio = SHAPE_AREA/(MaxDepth*0.3048))
rm(list=setdiff(ls(), "dat"))

# dat.gnet <- read_csv("data/Gnet_slopes.csv")

# dat <- dat %>% right_join(dat.gnet)
refcols <- c("WBIC",
             "LakeName",
             "US_L3NAME",
             "ECO_LANDSC",
             "HUC12_CODE")
# # respcols <- c("mean",
#               "ll",
#               "ul",
#               "slope_group")
non_transformed <- c("lat",
                     "long")
dat <- dat[,c(refcols,non_transformed,setdiff(names(dat),c(refcols,non_transformed)))]

# dat1 <- dat[,c(refcols,non_transformed)]
# dat2 <- dat[,setdiff(names(dat),c(refcols,non_transformed))]

# trans.data <- function(x) {
  # x.out <- bestNormalize(x,allow_orderNorm = FALSE,standardize = FALSE)$x.t
  # return(x.out)
# }

# dat_transform <- dat2 %>% 
  # mutate_all(funs(trans.data))
# dat_final <- cbind(dat1[,c(refcols,non_transformed)],dat_transform)
# write_csv(dat_final,"data/ecocontext_transformed_riparian.csv")
write_csv(dat,"data/ecocontext_full.csv")
