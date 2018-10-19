library(tidyverse)
library(cowplot)
library(ggmap)
library(maps)
library(mapdata)

####Original Data
dat.gnet <- read_csv("data/Gnet_slopes.csv")
eco <- read_csv("data/ecocontext.csv")
dat.gnet <- dat.gnet %>% left_join(eco)
dat.gnet$WiscID <- factor(dat.gnet$WiscID,levels=dat.gnet$WiscID[order(dat.gnet$cond)]) #Code to order gnet variables for plotting
dat.gnet <- dat.gnet %>% mutate(slope_group = as.numeric(slope_group)) %>% 
  mutate(slope_group = replace(slope_group,which(mean < -1.626692 & slope_group==0),2))


#Ordered plot of Gnet
ggplot(data = dat.gnet,aes(x = WiscID,y=mean,color=as.factor(slope_group))) + 
  geom_hline(yintercept = -1.626692,col="blue") + 
  geom_hline(yintercept = -1.24984,col="blue")+
  geom_point() +
  geom_errorbar(aes(ymin=ll,ymax=ul)) +
  labs(x="WiscID",y="Groundwater Recharge (mm/d)", color="Lake Group") +
  theme(text=element_text(size=10,  family="sans"),
        axis.text.x = element_text(angle = 90, hjust = 1,size=6)) +
  scale_color_manual(name="Lake Group",
                      labels=c("Low Recharge","Regional Average","High Recharge"),
                      values=c(rgb(27,158,119,255,maxColorValue = 255),
                               rgb(217,95,2,255,maxColorValue = 255),
                               rgb(117,112,179,255,maxColorValue = 255)))
ggsave(filename = "graphics/gnet_by_cond.png",units = "in",dpi = 300,width=8,height=5)

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
dat.box <- dat.gnet[,c(5,18:58)] %>% mutate(SHAPE_AREA = log10(SHAPE_AREA)) %>% 
  mutate(W_LA_Ratio = sqrt(W_LA_Ratio))
dat.box.long <- dat.box %>% gather(key = "key",value = "value",-slope_group)
ggplot(data = dat.box.long) + 
  geom_boxplot(aes(x=as.factor(slope_group),y=value,color=as.factor(slope_group))) + 
  facet_wrap(vars(key),scales = "free") +
  theme(text=element_text(size=10,  family="sans")) +
  scale_color_manual(name="Lake Group",
                     labels=c("Low Recharge","Regional Average","High Recharge"),
                     values=c(rgb(27,158,119,255,maxColorValue = 255),
                              rgb(217,95,2,255,maxColorValue = 255),
                              rgb(117,112,179,255,maxColorValue = 255)))
ggsave(filename = "graphics/gnet_boxplots.png",units = "in",dpi = 300)
