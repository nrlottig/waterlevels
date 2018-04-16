library(tidyverse)
library(readxl)
library(maps)
library(maptools)
library(scales)
dt = read_csv("GW_Models/HLM_out.csv")
EcoContext <- read_excel("RFModels/EcoContext.xlsx")

dat = dt %>% left_join(select(EcoContext,WiscID,lat,long)) %>% drop_na()

rbPal <- colorRampPalette(c('red','blue'))
dat$Col <- rbPal(10)[as.numeric(cut(dat$Gnet,breaks = 10))]
map.regions = c('wisconsin')
options(device="quartz")
dev.new(width=7,height=6)
par(mar=c(5.1,4.1,4.1,2.1))
map('state',region=map.regions,col=grey(.98),fill=TRUE,resolution = 0,mar=c(0,0,0,0),border=grey(.5),lty=5, main="Water Level Clusters")
points(dat$long,dat$lat,col=alpha(dat$Col,1),pch=16)
# legend("topright",legend=c("Gnet -2.70 mm/d","Gnet 0.150 mm/d"),pch=16,col=c("#FF0000","#0000FF"),title = "Net Groundwater Discharge",bty="n")       
legend.col(col = dat$Col[order(dat$Gnet)], lev = dat$Gnet)


legend.col <- function(col, lev){
  
  opar <- par
  
  n <- length(col)
  
  bx <- par("usr")
  
  box.cx <- c(bx[2] + (bx[2] - bx[1]) / 1000,
              bx[2] + (bx[2] - bx[1]) / 1000 + (bx[2] - bx[1]) / 50)
  box.cy <- c(bx[3], bx[3])
  box.sy <- (bx[4] - bx[3]) / n
  
  xx <- rep(box.cx, each = 2)
  
  par(xpd = TRUE)
  for(i in 1:n){
    
    yy <- c(box.cy[1] + (box.sy * (i - 1)),
            box.cy[1] + (box.sy * (i)),
            box.cy[1] + (box.sy * (i)),
            box.cy[1] + (box.sy * (i - 1)))
    polygon(xx, yy, col = col[i], border = col[i])
    
  }
  par(new = TRUE)
  plot(0, 0, type = "n",
       ylim = c(min(lev), max(lev)),
       yaxt = "n", ylab = "",
       xaxt = "n", xlab = "",
       frame.plot = FALSE)
  axis(side = 4, las = 2, tick = FALSE, line = .25)
  par <- opar
}

