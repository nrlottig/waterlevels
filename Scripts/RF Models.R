library(randomForest)
library(caret)
library(e1071)
require(VSURF)

easy.formula<-function(response,predictors){
  easy <- as.formula(paste(paste(response," ~"),
                           paste(predictors, collapse= "+"))); return(easy)}

waterlevelclusterID <- read.csv("~/Documents/GitHub/waterlevels/data/waterlevelclusterID.csv") #Data with all LTER wells
dataset = merge(waterlevelclusterID,dataset) #Data with representative LTER well


model.data = dataset #choose which dataset to use so code works without editing further down

factor.cols = c(3:7,10:12,184,185,189)

#assigning factor to correct variables
model.data = as.data.frame(model.data)
for(i in 1:length(factor.cols)){
  model.data[,factor.cols[i]] = as.factor(model.data[,factor.cols[i]]) 
}


##########Random Forest Modeling

#set response variable
Y = model.data[[189]]
names(model.data)[189]
#check counts for balancing RF model
table(Y)


X = model.data[,c(7,13:180,182:183,185:188)]
X = model.data[,c(7,13:24,29:80,85:155,160:180,182:183,186:188)] #remove runnoff
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
