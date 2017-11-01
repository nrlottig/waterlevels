library(randomForest)
library(caret)
library(e1071)
require(VSURF)
library(forestFloor)

easy.formula<-function(response,predictors){
  easy <- as.formula(paste(paste(response," ~"),
                           paste(predictors, collapse= "+"))); return(easy)}

waterlevelclusterID <- read.csv("~/Documents/GitHub/waterlevels/data/waterlevelclusterID.csv")

#Getting the data frame in the correct format for random forest model
waterlevelclusterID = waterlevelclusterID[,c(1,3,7,2,8,4,5,6)]
hucids = merge(hucids,hu4)
hucids = merge(hucids,hu8)
hucids = merge(hucids,huc12)
waterlevelclusterID = merge(waterlevelclusterID,hucids)
names(waterlevelclusterID)

factor.cols = c(3:7,10:12,184,185)

#assigning factor to correct variables
waterlevelclusterID = as.data.frame(waterlevelclusterID)
for(i in 1:length(factor.cols)){
  waterlevelclusterID[,factor.cols[i]] = as.factor(waterlevelclusterID[,factor.cols[i]]) 
}


##########Random Forest Modeling

#set response variable
Y = waterlevelclusterID[[3]]
names(waterlevelclusterID)[3]
#check counts for balancing RF model
table(waterlevelclusterID[,3])


X = waterlevelclusterID[,c(7,13:180,182:183,185)]
X = waterlevelclusterID[,c(7,13:24,29:80,85:155,160:180,182:183,185)] #remove runnoff
names(X)
X[,160] = log10(X[,160])
(rf.data = randomForest(y = Y,x = X,keep.inbag=TRUE,importance=TRUE,ntree=10001,sampsize=46))
pred = predict(rf.data)
(conf.out = confusionMatrix(pred,Y))

med.vsurf = VSURF(x = X,y = Y,parallel = TRUE,ncores = 7)
names(X[med.vsurf$varselect.interp])
names(X[med.vsurf$varselect.pred])
X = X[,med.vsurf$varselect.pred]
(rf.data2 = randomForest(y = Y,x = X,keep.inbag=TRUE,importance=TRUE,ntree=10001,sampsize=46))
pred = predict(rf.data2)
(conf.out = confusionMatrix(pred,Y))

imp.out = importance(rf.data2)
X = X[order(imp.out[,1],decreasing = TRUE)]
options(device="quartz")
dev.new(width=6,height=4)
par(oma=c(1.2,.8,.2,.2),mar=c(0,0,0,0),family="Times",ps=10)
dotplot(imp.out[order(imp.out[,1],decreasing = FALSE),1],main="",xlab="Increase Mean Square Error (%)")

ff = forestFloor(rf.fit=rf.data2,X=X,bootstrapFC = TRUE)
ffcol=fcol(ff,cols=2)
dev.new(width=9,height=4)
par(oma=c(1.2,.8,.2,.2),mar=c(0,0,0,0),family="Times",ps=10,cex.axis=1.5,cex.lab=2,cex.main=1.5)
plot(ff,orderByImportance=FALSE)

names(X)
plot.names = c("pct evergreen (HU8)","30yr avg precip (HU12)","Precip Cluster","30yr max precip (HU8)"
               ,"pct evergreen (HU12)","min baseflow (HU12)","TRI (HU12)","Slope (HU12)")
dev.new(width=8,height=4)
par(mfrow=c(2,5),oma=c(0,0,0,0),mar=c(3,3,.2,.2),family="Times",ps=10,cex.axis=1,cex.lab=2,cex.main=1.5)
for(i in 1:ncol(X)){
  out1 = partialPlot(x = rf.data2,pred.data = X,x.var = names(X)[i],which.class = 1,plot=FALSE)
  out2 = partialPlot(x = rf.data2,pred.data = X,x.var = names(X)[i],which.class = 2,plot=FALSE)
  out3 = partialPlot(x = rf.data2,pred.data = X,x.var = names(X)[i],which.class = 3,plot=FALSE)
  yrange = range(out1$y,out2$y,out3$y)
  plot(out1,col=1,type="l",ylim=yrange,xlab="",ylab="")
  mtext(side=1,plot.names[i],line=1.8)
  if(i==1 | i==5) mtext(side=2,"Relative logit contribution",line=1.8)
  lines(out2,col=2,type="l")
  lines(out3,col=3,type="l")
  }
out1 = partialPlot(x = rf.data2,pred.data = X,x.var = 1,which.class = 3)

plot(waterlevelclusterID$Latitude,waterlevelclusterID$hu12_runoff_mean)
