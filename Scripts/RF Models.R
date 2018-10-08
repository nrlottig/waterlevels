library(tidyverse)
library(randomForest)
library(caret)
library(e1071)
require(VSURF)
library(readxl)


EcoContext <- read_excel("RFModels/EcoContext.xlsx")
regressionstats <- read_csv("BayHModels/regressionstats.csv")

dat = EcoContext %>% left_join(regressionstats) %>% 
  select(-ID,-OBJECTID,-WATERBODY_NAME,-HYDROID,-HYDROCODE,
                                                           -HYDROTYPE,-LANDLOCK_C,-SHAPE_AREA,-SHAPE_LEN,
                                                           -County,-MeanDepth,-problem,-hydro24k,-centroid_x,
                                                           -centroid_y,-NATURAL_COMMUNITY,-Lake_type,-HYDROLOGY,
                                                           -`Katie classification`,-`Katie notes`,-W_LAT) %>% 
  drop_na()
#remove parameters that have significant number of zeros
dat = dat %>% select(-W_BD_201,-W_BD_204,-W_BD_205,-W_BD_206,-W_BD_207,-W_BD_208,-W_BD_209,-W_BD_210,-W_BD_MISSI,
                     -W_BR_2,-W_BR_3,-W_BR_MISSI,-W_QG_3,-W_QG_4,-W_QG_6,-W_QG_7,-W_QG_8,-W_QG_9,-W_QG_10,
                     -W_QG_11,-W_QG_12,-W_QG_13,-W_QG_14,-W_QG_15,-W_QG_16,-W_QG_17,-W_QG_18,-W_QG_20,
                     -W_QG_21,-W_QG_22,-W_QG_24,-W_QG_29,-W_QG_99,-W_QG_MISSI,-W_LU06_23,-W_LU06_24,-W_LU06_31,
                     -W_LU06_MIS)
#remove 2011 landcover (keeping 2006)
dat = dat %>% select(everything(),-contains("LU11"))
dat = dat %>% mutate(W_LA_Ratio = WatershedA/Area)
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

dat <- dat %>% left_join(new.data)
#gather data
dat.long <- dat %>% gather(key = variable,value = value,-slope,-sd.slope,-mape,
                           -n.points,-Gnet,-process_slope,-WBIC,-WiscID)

p.out <- ggplot(dat.long,aes(x=value,y=slope)) + geom_point() +
  geom_smooth(method=lm) + facet_wrap(~variable,scales="free")

ggsave(filename = "graphics/cum_slope.png",plot = p.out,width = 14,height = 8,units="in",dpi = 300,device = "png")

model.data = dat[,c(47,3:31,40:45,48)] %>% drop_na() %>% mutate(landscapeposition = as.factor(landscapeposition))#choose which dataset to use so code works without editing further down
# new.data <- dat[,c(2,3:30,40:45)] %>% drop_na()


##########Random Forest Modeling

#set response variable
Y.col = 1
Y = model.data[[Y.col]]
names(model.data)[Y.col]
#check counts for balancing RF model
# table(Y)


X = model.data[,c(2:37)]
names(X)
(rf.data = randomForest(y = Y,x = X,keep.inbag=TRUE,importance=TRUE,ntree=10001))

new.data <- new.data %>% mutate(pred_ratio = pred) %>% select(WiscID,pred_ratio)
pred = predict(rf.data,newdata = new.data[,c(2:35)])


plot(Y,pred)
# (conf.out = confusionMatrix(pred,Y))

med.vsurf = VSURF(x = X,y = Y,parallel = TRUE,ncores = 7,ntree = 5001,nfor.thres = 5001,nfor.interp = 5001,nfor.pred = 5001,clusterType = "FORK")
med.vsurf = VSURF(x = X,y = Y,parallel = TRUE,ncores = 7,sampsize=rep(min(table(Y)),nlevels(Y)),strata=Y,clusterType = "FORK",keep.inbag=TRUE)
names(X[med.vsurf$varselect.interp])
names(X[med.vsurf$varselect.pred])
# med.vsurf.dtw13.dmv13 = med.vsurf #Save output for future use 4.5 run time.
X = X[,med.vsurf$varselect.pred]
(rf.data2 = randomForest(y = Y,x = X,keep.inbag=TRUE,importance=TRUE,ntree=10001,sampsize=rep(min(table(Y)),nlevels(Y)),strata=Y))
pred = predict(rf.data2)
(conf.out = confusionMatrix(pred,Y))

(imp.out = importance(rf.data2,type=1,scale=FALSE))
X = X[,c(1,2)]#X[order(imp.out[,1],decreasing = TRUE)]
options(device="quartz")
dev.new(width=6,height=4)
par(oma=c(1.2,.8,.2,.2),mar=c(0,0,0,0),family="Times",ps=10)
good.anmes = c("Precipitation Cluster","Mean Annual Precip (30yr avg)")
attributes(imp.out)$dimnames[[1]] = good.anmes
dotplot(imp.out[order(imp.out[,1],decreasing = FALSE),1]*100,main="",xlab="Mean Decrease Accuracy (%)")

names(X)
plot.names = names(X)#c("Precipitation Cluster","Mean Annual Precip (30yr avg)")
dev.new(width=6,height=6)
par(mfrow=c(2,3),oma=c(0,0,0,0),mar=c(3,3,.2,.2),family="Times",ps=10,cex.axis=1,cex.lab=2,cex.main=1.5)
for(i in 1:ncol(X)){
  out1 = partialPlot(x = rf.data2,pred.data = X,x.var = names(X)[i],which.class = 1,plot=FALSE)
  out2 = partialPlot(x = rf.data2,pred.data = X,x.var = names(X)[i],which.class = 2,plot=FALSE)
  # out3 = partialPlot(x = rf.data2,pred.data = X,x.var = names(X)[i],which.class = 3,plot=FALSE)
  yrange = range(out1$y,out2$y,out3$y)
  plot(out1,col=1,type="l",ylim=yrange,xlab="",ylab="",lwd=2)
  mtext(side=1,plot.names[i],line=1.8)
  if(i==1 | i==5) mtext(side=2,"Impact on Class Probability",line=1.8)
  lines(out2,col=2,type="l",lwd=2)
  # lines(out3,col=3,type="l",lwd=2)
}


##############
##############
##############
# Run models without cluster 1 (southern WI)
Y2 = waterlevelclusterID[[3]][which(waterlevelclusterID$clusterid_dtw1_3!=1)]
Y2 = as.factor(as.numeric(Y))
X2 = waterlevelclusterID[which(waterlevelclusterID$clusterid_dtw1_3!=1),c(7,13:24,29:80,85:155,160:180,182:183,185:188)]
(rf.data = randomForest(y = Y2,x = X2,keep.inbag=TRUE,importance=TRUE,ntree=10001,sampsize=c(46,46),strata=Y))
pred = predict(rf.data)
(conf.out = confusionMatrix(pred,Y))
varImpPlot(rf.data,scale=FALSE,type=1)

med.vsurf = VSURF(x = X2,y = Y2,parallel = TRUE,ncores = 7,sampsize=c(46,46),strata=Y,clusterType = "FORK",keep.inbag=TRUE)
names(X2[med.vsurf$varselect.interp])
names(X2[med.vsurf$varselect.pred])
X2 = X2[,med.vsurf$varselect.pred]
(rf.data2 = randomForest(y = Y2,x = X,keep.inbag=TRUE,importance=TRUE,ntree=10001,sampsize=c(46,46),strata=Y))
pred = predict(rf.data2)
(conf.out = confusionMatrix(pred,Y2))
varImpPlot(rf.data2,scale=FALSE,type=1)

names(X2)

plot.names = names(X2)#c("Evergreen Forest (HU8,%)","Precipitation Cluster","Mean Annual Precip (30yr avg)")
dev.new(width=6,height=4)
par(mfrow=c(2,3),oma=c(0,0,0,0),mar=c(3,3,.2,.2),family="Times",ps=10,cex.axis=1,cex.lab=2,cex.main=1.5)
for(i in 1:ncol(X2)){
  out1 = partialPlot(x = rf.data2,pred.data = X2,x.var = names(X2)[i],which.class = 1,plot=FALSE)
  out2 = partialPlot(x = rf.data2,pred.data = X2,x.var = names(X2)[i],which.class = 2,plot=FALSE)
  # out3 = partialPlot(x = rf.data2,pred.data = X2,x.var = names(X2)[i],which.class = 3,plot=FALSE)
  yrange = range(out1$y,out2$y)
  plot(out1,col=1,type="l",ylim=yrange,xlab="",ylab="",lwd=2)
  mtext(side=1,plot.names[i],line=1.8)
  if(i==1 | i==5) mtext(side=2,"Impact on Class Probability",line=1.8)
  lines(out2,col=2,type="l",lwd=2)
  # lines(out3,col=3,type="l",lwd=2)
}


#########RF Models without LTER Wells
#set response variable

factor.cols = c(3:7,10:12,184,185)

#assigning factor to correct variables
dataset = as.data.frame(dataset)
for(i in 1:length(factor.cols)){
  dataset[,factor.cols[i]] = as.factor(dataset[,factor.cols[i]]) 
}


Y = dataset[[189]]
names(dataset)[189]
#check counts for balancing RF model
table(Y)


X = dataset[,c(7,13:24,29:80,85:155,160:180,182:183,185:188)] #remove runnoff
names(X)
(rf.data = randomForest(y = Y,x = X,keep.inbag=TRUE,importance=TRUE,ntree=10001,sampsize=rep(min(table(Y)),nlevels(Y)),strata=Y))
pred = predict(rf.data)
(conf.out = confusionMatrix(pred,Y))

med.vsurf = VSURF(x = X,y = Y,parallel = TRUE,ncores = 7,ntree = 5001,nfor.thres = 5001,nfor.interp = 5001,nfor.pred = 5001,clusterType = "FORK",sampsize=c(46,46,46),strata=Y,keep.inbag=TRUE)
med.vsurf = VSURF(x = X,y = Y,parallel = TRUE,ncores = 7,sampsize=rep(min(table(Y)),nlevels(Y)),strata=Y,clusterType = "FORK",keep.inbag=TRUE)
names(X[med.vsurf$varselect.interp])
names(X[med.vsurf$varselect.pred])
# med.vsurf.dtw13.dmv13 = med.vsurf #Save output for future use 4.5 run time.
X = X[,med.vsurf$varselect.pred]
(rf.data2 = randomForest(y = Y,x = X,keep.inbag=TRUE,importance=TRUE,ntree=10001,sampsize=rep(min(table(Y)),nlevels(Y)),strata=Y))
pred = predict(rf.data2)
(conf.out = confusionMatrix(pred,Y))

(imp.out = importance(rf.data2,type=1,scale=FALSE))
X = X[,c(2,1,3)]#X[order(imp.out[,1],decreasing = TRUE)]
options(device="quartz")
dev.new(width=6,height=4)
par(oma=c(1.2,.8,.2,.2),mar=c(0,0,0,0),family="Times",ps=10)
good.anmes = names(X)#c("Evergreen Forest (HU8,%)","Precipitation Cluster","Mean Annual Precip (30yr avg)")
attributes(imp.out)$dimnames[[1]] = good.anmes
dotplot(imp.out[order(imp.out[,1],decreasing = FALSE),1]*100,main="",xlab="Mean Decrease Accuracy (%)")

names(X)
plot.names = names(X)#c("Evergreen Forest (HU8,%)","Precipitation Cluster","Mean Annual Precip (30yr avg)")
dev.new(width=8,height=4)
par(mfrow=c(1,2),oma=c(0,0,0,0),mar=c(3,3,.2,.2),family="Times",ps=10,cex.axis=1,cex.lab=2,cex.main=1.5)
for(i in 1:ncol(X)){
  out1 = partialPlot(x = rf.data2,pred.data = X,x.var = names(X)[i],which.class = 1,plot=FALSE)
  out2 = partialPlot(x = rf.data2,pred.data = X,x.var = names(X)[i],which.class = 2,plot=FALSE)
  # out3 = partialPlot(x = rf.data2,pred.data = X,x.var = names(X)[i],which.class = 3,plot=FALSE)
  yrange = range(out1$y,out2$y,out3$y)
  plot(out1,col=1,type="l",ylim=yrange,xlab="",ylab="",lwd=2)
  mtext(side=1,plot.names[i],line=1.8)
  if(i==1 | i==5) mtext(side=2,"Impact on Class Probability",line=1.8)
  lines(out2,col=2,type="l",lwd=2)
  # lines(out3,col=3,type="l",lwd=2)
}
