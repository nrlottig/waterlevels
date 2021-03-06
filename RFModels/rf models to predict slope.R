library(tidyverse)
library(randomForest)
library(caret)
library(e1071)
require(VSURF)
library(readxl)
library(LAGOSNE)
library(intervals)

EcoContext <- read_excel("RFModels/EcoContext.xlsx")
sub_ecocontext = EcoContext %>% select(-ID,-OBJECTID,-WATERBODY_NAME,-HYDROID,-HYDROCODE,
                                                           -HYDROTYPE,-LANDLOCK_C,WBIC,-WiscID,-SHAPE_AREA,-SHAPE_LEN,
                                                           -County,-MeanDepth,-problem,-hydro24k,-centroid_x,
                                                           -centroid_y,-NATURAL_COMMUNITY,-Lake_type,-HYDROLOGY,
                                                           -`Katie classification`,-`Katie notes`) %>% drop_na()
sub_ecocontext = sub_ecocontext %>% select(-W_BD_201,-W_BD_204,-W_BD_205,-W_BD_206,-W_BD_207,-W_BD_208,-W_BD_209,-W_BD_210,-W_BD_MISSI,
                     -W_BR_2,-W_BR_3,-W_BR_MISSI,-W_QG_3,-W_QG_4,-W_QG_6,-W_QG_7,-W_QG_8,-W_QG_9,-W_QG_10,
                     -W_QG_11,-W_QG_12,-W_QG_13,-W_QG_14,-W_QG_15,-W_QG_16,-W_QG_17,-W_QG_18,-W_QG_20,
                     -W_QG_21,-W_QG_22,-W_QG_24,-W_QG_29,-W_QG_99,-W_QG_MISSI,-W_LU06_23,-W_LU06_24,-W_LU06_31)
regressionstats <- read_csv("GW_Models/HLM_out.csv")
lagoscrosswalk <- read_csv("RFModels/lagoscrosswalk.csv")
dt = read_csv("GW_Models/lake_climate_20180414_openWaterSeason.csv")
dt <- dt %>% select(WiscID,WBIC) %>% filter(!duplicated(WiscID))
# lagoscrosswalk = lagoscrosswalk %>% select(WiscID,lagoslakeid)
# dt <- lagosne_load(version = "1.087.1")
# dt.lakes= dt$lakes_limno
cond <- read_csv("data/Conductivity_final.csv") %>% select(WBIC,final_value)
CaMg <- read_csv("data/CaMg.csv") %>% 
  select(WiscID,CaConcentration,MgConcentration) %>% 
  mutate(camg_ratio = CaConcentration/MgConcentration)

dat <- regressionstats %>% 
  left_join(dt) %>% 
  left_join(cond) %>% 
  left_join(CaMg) %>% 
  left_join(sub_ecocontext) %>% 
  select(-CaConcentration,-MgConcentration,-camg_ratio) %>% 
  drop_na()

ggplot(data = dat, aes(x=final_value,y=Gnet)) + geom_point() + geom_smooth(method="lm")

dat = regressionstats %>% left_join(lagoscrosswalk) %>% drop_na(lagoslakeid) %>% left_join(select(dt$lakes_limno,lagoslakeid,maxdepth)) %>% 
  left_join(dt$buffer500m.lulc) %>% select(everything(),-contains("nlcd2001"),-contains("nlcd1992"),-contains("nlcd2006")) %>% 
  left_join(select(dt$locus,lagoslakeid,lake_area_ha,lake_perim_meters,elevation_m)) %>% drop_na(buffer500m_nhdid) %>% 
  left_join(select(dt$iws,lagoslakeid,iws_ha)) %>% select(everything(),-contains("nlcd2011_ha"),-buffer500m_nhdid) %>% 
  mutate(walaratio = iws_ha/lake_area_ha)  %>% left_join(select(EcoContext,WiscID,MaxDepth)) %>% mutate(MaxDepth = MaxDepth*0.3048)

dat$maxdepth[is.na(dat$maxdepth)] = dat$MaxDepth[is.na(dat$maxdepth)]
dat = dat %>% drop_na(maxdepth) %>% filter(maxdepth>0) %>% select(-MaxDepth)






global.slope = as.numeric(quantile(out$BUGSoutput$sims.list$mu.alpha,c(0.025,0.975)))

sapply(out$BUGSoutput$sims.list$BB[1:1500,1:51,1],function(x) quantile(x,c(0.025,0.975)))

sims.out = as.data.frame(out$BUGSoutput$sims.list$BB[1:1500,1:51,1])
lakeinterval = as.data.frame(t(sapply(sims.out,function(x) quantile(x,c(0.025,0.975)))))
lake.interval = Intervals(t(sapply(sims.out,function(x) quantile(x,c(0.025,0.975)))))
over.lap = interval_overlap(from = lake.interval,to = global.slope)
over.lap = as.numeric(sapply(over.lap,'[',1))


slope.class = rep(NA,51)
for(i in 1:nrow(lakeinterval)){
  if (lakeinterval[i,2] < global.slope[1]) slope.class[i] = 0 else if(lakeinterval[i,1] > global.slope[2]) slope.class[i]=2 else slope.class[i] =1
  
}

regressionstats$gnet.class = slope.class



dat = EcoContext %>% left_join(regressionstats) %>% select(-ID,-OBJECTID,-WATERBODY_NAME,-HYDROID,-HYDROCODE,
                                                           -HYDROTYPE,-LANDLOCK_C,-WBIC,-SHAPE_AREA,-SHAPE_LEN,
                                                           -County,-MeanDepth,-problem,-hydro24k,-centroid_x,
                                                           -centroid_y,-NATURAL_COMMUNITY,-Lake_type,-HYDROLOGY,
                                                           -`Katie classification`,-`Katie notes`) %>% drop_na()
summary(dat)

dat = dat %>% select(-W_BD_201,-W_BD_204,-W_BD_205,-W_BD_206,-W_BD_207,-W_BD_208,-W_BD_209,-W_BD_210,-W_BD_MISSI,
                     -W_BR_2,-W_BR_3,-W_BR_MISSI,-W_QG_3,-W_QG_4,-W_QG_6,-W_QG_7,-W_QG_8,-W_QG_9,-W_QG_10,
                     -W_QG_11,-W_QG_12,-W_QG_13,-W_QG_14,-W_QG_15,-W_QG_16,-W_QG_17,-W_QG_18,-W_QG_20,
                     -W_QG_21,-W_QG_22,-W_QG_24,-W_QG_29,-W_QG_99,-W_QG_MISSI,-W_LU06_23,-W_LU06_24,-W_LU06_31)

dat = dat %>% select(everything(),-contains("LU11"))
summary(dat)
dat = dat %>% mutate(W_LA_Ratio = WatershedA/Area)
model.data = dat #choose which dataset to use so code works without editing further down
names(model.data)
factor.cols = c(7:8)

#assigning factor to correct variables
model.data = as.data.frame(model.data)
for(i in 1:length(factor.cols)){
  model.data[,factor.cols[i]] = as.factor(model.data[,factor.cols[i]]) 
}


##########Random Forest Modeling

#set response variable
Y.col = 3
Y = model.data[[Y.col]]
names(model.data)[Y.col]
#check counts for balancing RF model
table(Y)


X = model.data[,c(5:58)]
names(X)
#,sampsize=rep(min(table(Y)),nlevels(Y)),strata=Y
(rf.data = randomForest(y = Y,x = X,keep.inbag=TRUE,importance=TRUE,ntree=10001))
pred = predict(rf.data)
plot(Y,pred)
abline(a=0,b=1)
(conf.out = confusionMatrix(pred,Y))

med.vsurf = VSURF(x = X,y = Y,parallel = TRUE,ncores = 7,clusterType = "FORK")
