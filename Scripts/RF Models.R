library(randomForest)
library(caret)
library(e1071)
require(VSURF)
library(forestFloor)

easy.formula<-function(response,predictors){
  easy <- as.formula(paste(paste(response," ~"),
                           paste(predictors, collapse= "+"))); return(easy)}

waterlevelclusterID <- read.csv("~/Documents/GitHub/waterlevels/data/waterlevelclusterID.csv")
waterlevelclusterID = waterlevelclusterID[,c(1,3,7,2,8,4,5,6)]
hucids = merge(hucids,hu4)
hucids = merge(hucids,hu8)
hucids = merge(hucids,huc12)
waterlevelclusterID = merge(waterlevelclusterID,hucids)
names(waterlevelclusterID)
factor.cols = c(3,4,5,9:11)

for(i in 1:length(factor.cols)){
  waterlevelclusterID[,factor.cols[i]] = as.factor(waterlevelclusterID[,factor.cols[i]]) 
}

(response = names(waterlevelclusterID)[5])
table(waterlevelclusterID[,5])

(predictors = names(waterlevelclusterID)[c(6:8,10:179)])
# predictors = predictors[c(5,6,7,8,9,11,12,13)]
big.formula = easy.formula(response, predictors)



rf.data = randomForest(big.formula,data=waterlevelclusterID,keep.inbag=TRUE,importance=TRUE,ntree=10001,sampsize=39)
rf.data
pred = predict(rf.data)
(conf.out = confusionMatrix(pred,waterlevelclusterID$clusterid_dtw1_3))

Y = waterlevelclusterID[[5]]
X = waterlevelclusterID[,c(6:8,10:179)]
(rf.data = randomForest(y = Y,x = X,keep.inbag=TRUE,importance=TRUE,ntree=10001,sampsize=34))
med.vsurf = VSURF(x = X,y = Y)
length(med.vsurf$varselect.pred)
names(X[med.vsurf$varselect.interp])
X = X[,med.vsurf$varselect.interp]
(rf.data2 = randomForest(y = Y,x = X,keep.inbag=TRUE,importance=TRUE,ntree=10001,sampsize=34))
pred = predict(rf.data2)
(conf.out = confusionMatrix(pred,Y))

imp.out = importance(rf.data2)
X = X[order(imp.out[,1],decreasing = TRUE)]
options(device="quartz")
dev.new(width=6,height=4)
par(oma=c(1.2,.8,.2,.2),mar=c(0,0,0,0),family="Times",ps=10)

dotplot(imp.out[order(imp.out[,1],decreasing = FALSE),1],main="",xlab="Increase Mean Square Error (%)")

ff = forestFloor(rf.fit=rf.data2,X=X,bootstrapFC = TRUE)
Col=fcol(ff,cols=2)
dev.new(width=9,height=4)
par(oma=c(1.2,.8,.2,.2),mar=c(0,0,0,0),family="Times",ps=10,cex.axis=1.5,cex.lab=2,cex.main=1.5)
plot(ff)
