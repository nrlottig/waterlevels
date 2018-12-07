rm(list=ls())
lapply(paste('package:',names(sessionInfo()$otherPkgs),sep=""),detach,character.only=TRUE,unload=TRUE)
library(tidyverse)
library(cowplot)
library(ggmap)
library(maps)
library(mapdata)
library(bestNormalize)
library(randomForest)

####Original Data
dat.gnet <- read_csv("data/Gnet_slopes.csv")
eco <- read_csv("data/ecocontext_transformed_riparian.csv")
BugsOut <- read.csv("GW_Models/BUGSutputSummary.csv")
J <- length(unique(dat.gnet$WBIC))
# dat.gnet <- dat.gnet %>% left_join(eco)
dat.gnet$WBIC_name <- factor(dat.gnet$WBIC,levels=dat.gnet$WBIC[order(dat.gnet$mean)]) #Code to order gnet variables for plotting
dat.gnet <- dat.gnet %>% mutate(slope_group = as.numeric(slope_group)) %>% 
  mutate(slope_group = replace(slope_group,which(mean < BugsOut[J*2+1,2] & slope_group==0),2))


#Ordered plot of Gnet
ggplot(data = dat.gnet,aes(x = (WBIC_name),y=mean,color=as.factor(slope_group))) + 
  #Turn this into a grey polygon band
  geom_point() +
  geom_errorbar(aes(ymin=ll,ymax=ul)) +
  labs(x="WiscID",y="Groundwater Recharge (mm/d)", color="Lake Group") +
  theme(text=element_text(size=10,  family="sans"),
        axis.text.x = element_text(angle = 90, hjust = 1,size=6)) +
  scale_color_manual(name="Lake Group",
                      labels=c("Low Recharge","Regional Average","High Recharge"),
                      values=c(rgb(27,158,119,255,maxColorValue = 255),
                               rgb(217,95,2,255,maxColorValue = 255),
                               rgb(117,112,179,255,maxColorValue = 255))) + 
  annotate("rect", ymin = BugsOut[J*2+1,4], ymax = BugsOut[J*2+1,8], xmin=-Inf,xmax=Inf,
           alpha = .3,fill="grey") +
  annotate("text",x = 30,y=1,label = "Grey bar denotes 95% confidence interval of regional groundwater recharge for seepage lakes",size=2)
ggsave(filename = "graphics/gnet_by_gnet.png",units = "in",dpi = 300,width=8,height=5)

#########Tasks for WU################
# 1) There are two lakes for which we don't have eco context variables (WiscID: 123 and 248)
# 2) Create similar plots except order by
#     a. lattitude
#     b. wetland % in watershed
#     c. conductivity
#     d. any other eco context variables that seem to show some pattern

WI <- map_data(map = "state",region = "wisconsin")
WI.cty <- map_data(map = "county",region = "wisconsin")
ggplot() + 
  geom_polygon(data = WI, aes(x=long, y = lat, group = group), fill = NA, color = "black") + 
  geom_polygon(data = WI.cty, aes(x=long, y = lat, group = group), fill = NA, color = "darkgrey") +
  coord_fixed(1.3) + 
  geom_point(data = dat.gnet,aes(x =long,y=lat,color=as.factor(slope_group)),size=3) +
  scale_color_manual(name="Lake Group",
                     labels=c("Low Recharge","Regional Average","High Recharge"),
                     values=c(rgb(27,158,119,200,maxColorValue = 255),
                              rgb(217,95,2,200,maxColorValue = 255),
                              rgb(117,112,179,200,maxColorValue = 255)))
ggsave(filename = "graphics/gnet_map.png",units="in",dpi=300)

#BoxPlots
dat.box <- dat.gnet[,c(5,10:41)]
dat.box.long <- dat.box %>% gather(key = "key",value = "value",-slope_group)
ggplot(data = dat.box.long) + 
  geom_boxplot(aes(x=as.factor(slope_group),y=value,color=as.factor(slope_group))) + 
  facet_wrap(vars(key),scales = "free") +
  theme(text=element_text(size=10,  family="sans")) +
  scale_color_manual(name="Lake Group",
                     labels=c("Low Recharge","Regional Average","High Recharge"),
                     values=c(rgb(27,158,119,255,maxColorValue = 255),
                              rgb(217,95,2,255,maxColorValue = 255),
                              rgb(117,112,179,255,maxColorValue = 255))) +
  labs(y="Driver Value",x="Lake Group")
ggsave(filename = "graphics/gnet_boxplots.png",units = "in",dpi = 300)

refcols <- c("WBIC",
             "LakeName",
             "US_L3NAME",
             "ECO_LANDSC",
             "HUC12_CODE",
             "WBIC_name")
respcols <- c("mean",
              "ll",
              "ul",
              "slope_group")

dat.long <- dat.gnet %>% gather(key = variable,value = value,-refcols,-respcols)


p.out <- ggplot(dat.long,aes(x=value,y=mean)) + geom_point(aes(color=as.factor(slope_group))) +
  geom_smooth(method=lm, se=FALSE) + 
  facet_wrap(vars(variable),scales="free") +
  scale_color_manual(name="Lake Group",
                     labels=c("Low Recharge","Regional Average","High Recharge"),
                     values=c(rgb(27,158,119,255,maxColorValue = 255),
                              rgb(217,95,2,255,maxColorValue = 255),
                              rgb(117,112,179,255,maxColorValue = 255))) +
  labs(y="Groundwater Recharge (mmd)",x="Driver Value")
p.out
ggsave(filename = "graphics/GNET.png",plot = p.out,width = 12,height = 12,units="in",dpi = 300,device = "png")

##########Random Forest Modeling

#set response variable
model.data <- dat.gnet
Y.col = 5
Y = as.factor(model.data[[Y.col]])
names(model.data)[Y.col]
#check counts for balancing RF model
table(Y)


X = model.data[,c(8,10:37)]
names(X)
X$ECO_LANDSC = as.factor(X$ECO_LANDSC)
#,sampsize=rep(min(table(Y)),nlevels(Y)),strata=Y
(rf.data = randomForest(y = Y,x = X,keep.inbag=TRUE,importance=TRUE,ntree=10001,sampsize = c(6,6,6),strata=Y))
pred = predict(rf.data)
plot(Y,pred)
abline(a=0,b=1)
(conf.out = confusionMatrix(pred,Y))
                                