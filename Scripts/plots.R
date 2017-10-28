library(maps)
library(maptools)
library(scales)
map.regions = c('wisconsin')
dev.new(width=5,height=5)
map('state',region=map.regions,col=grey(.98),fill=TRUE,resolution = 0,mar=c(0,0,0,0),border=grey(.5),lty=5)
points(waterlevelclusterID$Longitude,waterlevelclusterID$Latitude,col=alpha(as.numeric(waterlevelclusterID$clusterid_dtw2_4),.7),pch=16)
legend("topright",legend=c(1:4),pch=16,col=c(1:4),title = "Cluster ID",bty="n")       


rf.drivers = data.frame(wiscwaterlevelid=waterlevelclusterID$wiscwaterlevelid,clusterid = waterlevelclusterID$clusterid_dtw2_4,clusterpred=pred,meanrunnoff=waterlevelclusterID$hu12_runoff_mean,maxrunnoff=waterlevelclusterID$hu12_runoff_max,maxprecip=waterlevelclusterID$hu12_prism_ppt_30yr_normal_800mm2_annual_max)

dev.new(width=4,height=4)
par(oma=c(3,3,.2,.2),mar=c(0,0,0,0),family="Times",ps=10)
boxplot(rf.drivers$meanrunnoff~rf.drivers$clusterid,xlab="",ylab="R")
mtext(side=1,line=2,"Cluster ID",cex=1.25)
mtext(side=2,line=2,"Mean Runnoff (in/yr)",cex=1.25)

dev.new(width=4,height=4)
par(oma=c(3,3,.2,.2),mar=c(0,0,0,0),family="Times",ps=10)
boxplot(rf.drivers$maxprecip~rf.drivers$clusterid,xlab="",ylab="")
mtext(side=1,line=2,"Cluster ID",cex=1.25)
mtext(side=2,line=2,"Max Precip (mm/yr)",cex=1.25)
