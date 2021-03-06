library(maps)
library(maptools)
library(scales)


map.regions = c('wisconsin')
dev.new(width=5,height=5)
map('state',region=map.regions,col=grey(.98),fill=TRUE,resolution = 0,mar=c(0,0,0,0),border=grey(.5),lty=5, main="Water Level Clusters")
points(model.data$Longitude,model.data$Latitude,col=alpha(as.numeric(model.data$DTW1_2_ClusterID),.7),pch=16)
legend("topright",legend=c(1:2),pch=16,col=c(1:2),title = "Cluster ID",bty="n")       

map.regions = c('wisconsin')
dev.new(width=5,height=5)
map('state',region=map.regions,col=grey(.98),fill=TRUE,resolution = 0,mar=c(0,0,0,0),border=grey(.5),lty=5, main="Water Level Clusters")
points(dataset$Longitude,dataset$Latitude,col=alpha(as.numeric(dataset$DTW1_2_ClusterID),.7),pch=16)
legend("topright",legend=c(1:2),pch=16,col=c(1:2),title = "Cluster ID",bty="n") 
 

rf.drivers = data.frame(wiscwaterlevelid=waterlevelclusterID$wiscwaterlevelid,clusterid = waterlevelclusterID$clusterid_dtw2_4,clusterpred=pred,meanrunnoff=waterlevelclusterID$hu12_runoff_mean,maxrunnoff=waterlevelclusterID$hu12_runoff_max,maxprecip=waterlevelclusterID$hu12_prism_ppt_30yr_normal_800mm2_annual_max)

dev.new(width=4,height=4)
par(oma=c(3,3,.2,.2),mar=c(0,0,0,0),family="Times",ps=10)
boxplot(model.data$hu12_prism_ppt_30yr_normal_800mm2_annual_mean~model.data$DTW1_2_ClusterID,xlab="",ylab="")
mtext(side=1,line=2,"Cluster ID",cex=1.25)
mtext(side=2,line=2,"Annual Precip (30yr avg,mm)",cex=1.25)

dev.new(width=4,height=4)
par(oma=c(3,3,.2,.2),mar=c(0,0,0,0),family="Times",ps=10)
boxplot(waterlevelclusterID$hu12_prism_ppt_30yr_normal_800mm2_annual_mean~waterlevelclusterID$clusterid_dtw1_3,xlab="",ylab="")
mtext(side=1,line=2,"Cluster ID",cex=1.25)
mtext(side=2,line=2,"Annual Precip (30yr avg,mm)",cex=1.25)

dev.new(width=4,height=4)
par(oma=c(3,3,.2,.2),mar=c(0,0,0,0),family="Times",ps=10)
boxplot(waterlevelclusterID$Dist_hicap~waterlevelclusterID$clusterid_dtw2_4,xlab="",ylab="")
mtext(side=1,line=2,"Cluster ID",cex=1.25)
mtext(side=2,line=2,"Max Precip (mm/yr)",cex=1.25)


dev.new(width=4,height=4)
par(oma=c(3,3,.2,.2),mar=c(0,0,0,0),family="Times",ps=10)
plot(dtw2_4_validation$num_clust,dtw2_4_validation$Rand_DTW,ylim=range(dtw2_4_validation[,c(2:3)]),type="b",col="blue",pch=16,lwd=2,xlab="",ylab="")
lines(dtw2_4_validation$num_clust,dtw2_4_validation$Cluster_DTW,type="b",col="red",pch=16,lwd=2)
lines(dtw2_4_validation$num_clust,dtw2_4_validation$DTW_DIFF,type="b",col="green",pch=16,lwd=2)
mtext(side=1,line=2,"Number of Clusters",cex=1.25)
mtext(side=2,line=2,"DTW Distance",cex=1.25)
legend("right",legend=c("Random Clustering","Kernal Clustering","Difference"),pch=16,lwd=2,col=c("blue","red","green"),bty="n")
