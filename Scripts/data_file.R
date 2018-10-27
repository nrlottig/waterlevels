library(tidyverse)
library(readxl)
library(LAGOSNE)
library(party)
library(randomForest)
library(bestNormalize)


EcoContext <- read_excel("RFModels/EcoContext.xlsx")

dat = EcoContext %>% 
  dplyr::select(-ID,-OBJECTID,-WATERBODY_NAME,-HYDROID,-HYDROCODE,
         -HYDROTYPE,-LANDLOCK_C,-Area,-WatershedA,
         -County,-MeanDepth,-problem,-hydro24k,-centroid_x,
         -centroid_y,-NATURAL_COMMUNITY,-Lake_type,-HYDROLOGY,
         -`Katie classification`,-`Katie notes`,-W_LAT)

#remove parameters that have significant number of zeros
dat <- dat %>% 
  mutate(develop= W_LU06_21+W_LU06_22+W_LU06_23+W_LU06_24) %>% 
  mutate(forest = W_LU06_41+W_LU06_42+W_LU06_43) %>% 
  mutate(ag = W_LU06_81+W_LU06_82) %>% 
  mutate(SDI = SHAPE_LEN/(2*sqrt(pi*SHAPE_AREA))) %>% 
  mutate(area_depth_ratio = SHAPE_AREA/(MaxDepth*0.3048)) %>% 
  mutate(forest = W_LU06_41+W_LU06_42+W_LU06_43) %>% 
  mutate(wetland = W_LU06_90+W_LU06_95)
dat = dat %>% dplyr::select(-W_BD_201,-W_BD_204,-W_BD_205,-W_BD_206,-W_BD_207,-W_BD_208,-W_BD_209,-W_BD_210,-W_BD_MISSI,
                     -W_BR_2,-W_BR_3,-W_BR_MISSI,-W_QG_3,-W_QG_4,-W_QG_6,-W_QG_7,-W_QG_8,-W_QG_9,-W_QG_10,
                     -W_QG_11,-W_QG_12,-W_QG_13,-W_QG_14,-W_QG_15,-W_QG_16,-W_QG_17,-W_QG_18,-W_QG_20,
                     -W_QG_21,-W_QG_22,-W_QG_24,-W_QG_29,-W_QG_99,-W_QG_MISSI,-W_LU06_23,-W_LU06_24,-W_LU06_31,
                     -W_LU06_MIS,-W_TEMP_ANN,-W_TEMP_GS,-W_TEMP_JUL,-W_PRCP_ANN)

#remove 2011 landcover (keeping 2006)
dat = dat %>% select(everything(),-contains("LU11"))
dat = dat %>% mutate(W_LA_Ratio = W_AREA/SHAPE_AREA)
#remove data that is 50% zero values
dat <- dat %>% select(-W_BD_202,-W_BR_1,-W_BR_41,-W_BR_42,-W_QG_2,-W_QG_5)
summary(dat)

#get Ca and Mg Data
CaMg <- read_csv("data/CaMg.csv")
dat_cations <- CaMg %>% select(WBIC,CaConcentration,MgConcentration) %>% 
  mutate(cation_ratio = CaConcentration/MgConcentration)
dat <- dat %>% left_join(dat_cations)

#get Conductivity data
Conductivity_final <- read_csv("data/Conductivity_final.csv")
dat_cond <- Conductivity_final %>% select(WBIC,final_value) %>% rename(cond=final_value)
dat <- dat %>% left_join(dat_cond)
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

#cleanup data 
rm(list=setdiff(ls(), "dat"))
refcols <- c("WBIC",
             "WiscID",
             "LakeName",
             "US_L3NAME",
             "ECO_LANDSC",
             "HUC12_CODE")
dat <- dat[,c(refcols,setdiff(names(dat),refcols))]
dat <- dat %>% select(-W_BD_203,-W_LU06_52,-W_LU06_71,-W_QG_1)
write_csv(dat,"data/ecocontext.csv")

#Long_conversion
dat.long <- dat %>% gather(key = variable,value = value,-refcols)
ggplot(data = dat.long) + geom_histogram(aes(x=value)) + facet_wrap(vars(variable),scales = "free")

dat_transform <- dat %>% 
  mutate(ag = bestNormalize(ag/100)$x.t) %>% 
  mutate(area_depth_ratio = log10(area_depth_ratio+1)) %>% 
  mutate(MaxDepth = bestNormalize(MaxDepth)$x.t) %>% 
  mutate(cond = bestNormalize(cond)$x.t) %>% 
  mutate(develop = bestNormalize(develop/100)$x.t) %>%
  mutate(elevation_difference = bestNormalize(elevation_difference)$x.t) %>% 
  mutate(forest = bestNormalize(forest/100)$x.t) %>%
  mutate(lake_MEAN = bestNormalize(lake_MEAN)$x.t) %>% 
  mutate(SDI = bestNormalize(SDI)$x.t) %>% 
  mutate(mean_ann_wdrl_gals = sqrt(mean_ann_wdrl_gals+0.001)) %>% 
  mutate(SHAPE_AREA = bestNormalize(SHAPE_AREA)$x.t) %>% 
  mutate(SHAPE_LEN = bestNormalize(SHAPE_LEN)$x.t) %>% 
  mutate(slope2 = bestNormalize(slope2)$x.t) %>% 
  mutate(slope3 = bestNormalize(slope3)$x.t) %>% 
  mutate(slope4 = bestNormalize(slope4)$x.t) %>% 
  mutate(slope5 = bestNormalize(slope5)$x.t) %>% 
  mutate(slopenearest = bestNormalize(slopenearest)$x.t) %>% 
  mutate(TRW_AREA = bestNormalize(TRW_AREA)$x.t) %>% 
  mutate(W_AREA = bestNormalize(W_AREA)$x.t) %>%
  mutate(W_DARCY = bestNormalize(W_DARCY)$x.t) %>%
  mutate(W_LA_Ratio = bestNormalize(W_LA_Ratio)$x.t) %>%
  mutate(W_LU06_11 = bestNormalize(W_LU06_11/100)$x.t) %>%
  mutate(W_LU06_21 = bestNormalize(W_LU06_21/100)$x.t) %>%
  mutate(W_LU06_22 = bestNormalize(W_LU06_22/100)$x.t) %>%
  mutate(W_LU06_41 = bestNormalize(W_LU06_41/100)$x.t) %>%
  mutate(W_LU06_42 = bestNormalize(W_LU06_42/100)$x.t) %>%
  mutate(W_LU06_43 = bestNormalize(W_LU06_43/100)$x.t) %>%
  mutate(W_LU06_81 = bestNormalize(W_LU06_81/100)$x.t) %>%
  mutate(W_LU06_82 = bestNormalize(W_LU06_82/100)$x.t) %>%
  mutate(W_LU06_90 = bestNormalize(W_LU06_90/100)$x.t) %>%
  mutate(W_LU06_95 = bestNormalize(W_LU06_95/100)$x.t) %>%
  mutate(W_PERM = bestNormalize(W_PERM)$x.t) %>%
  mutate(W_SLOPE = bestNormalize(W_SLOPE)$x.t) %>%
  mutate(watershed_MIN = bestNormalize(watershed_MIN)$x.t) %>%
  mutate(wetland = bestNormalize(wetland/100)$x.t) %>% 
  gather(key = variable,value = value,-refcols)

BNobject <- bestNormalize(dat$W_LA_Ratio,allow_orderNorm = FALSE)
BNobject


ggplot(data = dat_transform) + geom_histogram(aes(x=value)) + facet_wrap(vars(variable),scales = "free")

dat2 <- dat_transform %>% spread(key = variable,value = value)
write_csv(dat2,"data/ecocontext_transformed.csv")



  mutate(SHAPE_AREA = log10(SHAPE_AREA)) %>% 
  mutate(TRW_AREA = log10(TRW_AREA)) %>% 
  mutate(W_LA_Ratio = sqrt(W_LA_Ratio)) %>% 
  mutate(W_AREA = log10(W_AREA)) %>% 
  mutate(W_DARCY = 1/W_DARCY)

#gather data
dat.long <- dat_transform %>% gather(key = variable,value = value,-refcols,-respcols)
dat.gnet <- dat.long %>% filter(ECO_LANDSC!="Western Prairie") %>% 
  filter(variable!="bedrock") %>% 
  filter(variable!="landscapeposition") %>% 
  filter(variable!="cation_ratio") %>% 
  filter(variable!="MgConcentration") %>% 
  filter(variable!="CaConcentration") %>% 
  filter(variable!="TRW_AREA") %>% 
  filter(variable!="watershed_MAX") %>% 
  filter(variable!="watershed_MEAN")
d.gnet.w <- dat.gnet %>% spread(key = variable,value = value)
# ggplot(data = dat.long, aes(x=value)) + geom_histogram() + facet_wrap(vars(variable),scales="free")

p.out <- ggplot(dat.gnet %>% drop_na(Gnet),aes(x=value,y=Gnet)) + geom_point() +
  geom_smooth(method=lm, se=FALSE) + 
  facet_wrap(vars(ECO_LANDSC,variable),scales="free")

ggsave(filename = "graphics/ECO_Gnet.png",plot = p.out,width = 38,height = 38,units="in",dpi = 300,device = "png")

######Random Forest Data
response.vars <- names(dat)[c(8,12,13,11)]
driver.vars <- names(d.gnet.w)[c(6,7,14:44)]

model_data <- d.gnet.w %>% select(c(Gnet,driver.vars)) %>% 
  mutate(US_L3NAME = as.factor(as.character(US_L3NAME))) %>% 
  mutate(ECO_LANDSC = as.factor(as.character(ECO_LANDSC))) %>% 
  drop_na()

str(model_data)
forest.cf <- cforest(Gnet ~ ., data = model_data,
                   control = cforest_unbiased(ntree = 10001,mtry=3))
oob.pred<-predict(forest.cf, type="response", OOB=TRUE)
residual<-model_data[[1]]-oob.pred
mse<-sum(residual^2)/length(model_data[[1]])
(pseudo.R2<-1-mse/var(model_data[[1]]))

vi <- varimp(forest.cf, conditional = TRUE)
dotchart(vi[order(vi)])


(rf.data = randomForest(Gnet ~ ., data = model_data,keep.inbag=TRUE,importance=TRUE,ntree=10001))
