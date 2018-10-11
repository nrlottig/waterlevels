library(tidyverse)
library(readxl)
library(LAGOSNE)
library(party)
library(randomForest)


EcoContext <- read_excel("RFModels/EcoContext.xlsx")
regressionstats <- read_csv("BayHModels/regressionstats.csv")

dat = EcoContext %>% left_join(regressionstats) %>% 
  select(-ID,-OBJECTID,-WATERBODY_NAME,-HYDROID,-HYDROCODE,
         -HYDROTYPE,-LANDLOCK_C,-Area,-WatershedA,
         -County,-MeanDepth,-problem,-hydro24k,-centroid_x,
         -centroid_y,-NATURAL_COMMUNITY,-Lake_type,-HYDROLOGY,
         -`Katie classification`,-`Katie notes`,-W_LAT,-lat,-long) %>% 
  drop_na()

#remove parameters that have significant number of zeros
dat <- dat %>% mutate(bedrock = W_BD_202+W_BD_203+W_BD_204+W_BD_205) %>% 
  mutate(develop= W_LU06_21+W_LU06_22+W_LU06_23+W_LU06_24) %>% 
  mutate(forest = W_LU06_41+W_LU06_42+W_LU06_43) %>% 
  mutate(ag = W_LU06_81+W_LU06_82) %>% 
  mutate(SDI = SHAPE_LEN/(2*sqrt(pi*SHAPE_AREA))) %>% 
  mutate(forest = W_LU06_41+W_LU06_42+W_LU06_43) %>% 
  mutate(wetland = W_LU06_90+W_LU06_95)
dat = dat %>% select(-W_BD_201,-W_BD_204,-W_BD_205,-W_BD_206,-W_BD_207,-W_BD_208,-W_BD_209,-W_BD_210,-W_BD_MISSI,
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
dat_elevation <- lake_elevation %>% select(WBIC,lake_MEAN,watershed_MIN,watershed_MAX,watershed_MEAN,elevation_difference)
dat <- dat %>% left_join(dat_elevation)
#get process data
HLM_out <- read_csv("GW_Models/HLM_out.csv") %>% rename(process_slope=slope)
dat <- dat %>% left_join(HLM_out)

#get HUC data
lagoscrosswalk <- read_csv("lagoscrosswalk.csv") %>% select(WIBIC,lagoslakeid,County) %>% 
  rename(WBIC=WIBIC)
lagoscrosswalk <- lagoscrosswalk[!duplicated(lagoscrosswalk$WBIC),]
dat <- dat %>% left_join(lagoscrosswalk)

#get ecoregion data
regression_coefs_20180328_eco_land <- read_excel("data/regression_coefs_20180328_eco_land.xlsx")
regression_coefs_20180328_eco_land <- regression_coefs_20180328_eco_land[!duplicated(regression_coefs_20180328_eco_land$WBIC),] %>% 
  select(-WiscID)
dat <- dat %>% left_join(regression_coefs_20180328_eco_land)


#cleanup data 
rm(list=setdiff(ls(), "dat"))
refcols <- c("WBIC",
             "WiscID",
             "lagoslakeid",
             "County",
             "LakeName",
             "US_L3NAME",
             "ECO_LANDSC")
respcols <- c("slope",
              "n.points",
              "sd.slope",
              "mape",
              "Gnet",
              "process_slope")
dat <- dat[,c(refcols,respcols,setdiff(names(dat),c(refcols,respcols)))]

dat_transform <- dat %>% mutate(SHAPE_AREA = log10(SHAPE_AREA)) %>% 
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
