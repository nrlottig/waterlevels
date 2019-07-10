library(tidyverse)
library(lubridate)
library(gridExtra)

waterlevel <- read.csv("big_data/seepage_20180414.csv",sep=",",header=TRUE) %>% 
  mutate(Date = as.Date(as.character(Date),format= "%m/%d/%Y")) %>% 
  mutate(Value = as.numeric(as.character(Value))*304.8)  %>% 
  select(WBIC,WiscID,Date,Value) %>% 
  mutate(Year=year(Date)) %>% 
  drop_na()
crosswalk <- waterlevel %>% select(WBIC,WiscID) %>% distinct()

process_extrapolations <- read_csv("data/process_extrapolations.csv") %>% 
  rename(level_process = level) %>% 
  group_by(WBIC) %>% 
  mutate(level_process = scale(level_process,center = T,scale = F)) %>% 
  ungroup()

cdev_lakelevel_predictions <- read_csv("data/cdev_lakelevel_predictions.csv") %>% 
  mutate(Year = year(Date)) %>% 
  select(WiscID,Year,yhat) %>% 
  rename(level_precip = yhat) %>% 
  mutate(level_precip = level_precip/10) %>% 
  group_by(WiscID,Year) %>% 
  summarize(level_precip = mean(level_precip)) %>% 
  mutate(level_precip = scale(level_precip,center = T,scale = F)) %>% 
  left_join(crosswalk) %>% 
  ungroup() %>% 
  select(-WiscID)



#identify longest record for comparision purposes
best.lakes <- waterlevel %>% 
  group_by(WBIC,WiscID) %>% 
  summarize(total_n=n(),dat_yr = length(unique(Year))) %>% 
  ungroup() %>% 
  group_by(WBIC) %>% 
  filter(dat_yr == max(dat_yr)) %>% 
  filter(total_n == max(total_n)) %>% 
  ungroup() %>% 
  filter(dat_yr >=10)
#filter water level records
waterlevel <- waterlevel %>% filter(WiscID %in% best.lakes$WiscID)
waterlevel <- waterlevel %>% 
  group_by(WBIC,Year) %>% 
  summarize(Value = mean(Value,na.rm=TRUE)/10) %>% 
  mutate(level_obs = scale(Value,center = T,scale = F)) %>% 
  select(-Value)

dat <- process_extrapolations %>% 
  full_join(cdev_lakelevel_predictions) %>% 
  full_join(waterlevel) %>% 
  gather(key = method,value = value,-Year,-WBIC) %>% 
  arrange(WBIC)

lakes <- unique(dat$WBIC)
lake_info <- read.csv("big_data/seepage_20180414.csv",sep=",",header=TRUE) %>% 
  select(WBIC,SiteName) %>% distinct() %>%
  group_by(WBIC) %>% 
  slice(1) %>% 
  ungroup()
z=1
plot = list()

pdf("graphics/water_level_summary.pdf",width=8,height=10.5,onefile = TRUE)
for(i in 1:length(lakes)) {
  temp.dat <- dat %>% filter(WBIC==lakes[i])
  lake.info <- lake_info %>% filter(WBIC==lakes[i])
  plot[[z]] <- ggplot(data = temp.dat, aes(x=Year,y=value,color=method)) + geom_line() + geom_point() +
    labs(title = paste(lake.info[[1]],": ",lake.info[[2]],sep=""))
  if (z %% 3 == 0) { ## print 8 plots on a page
    do.call(grid.arrange,  c(plot,ncol=1))
    plot = list() # reset plot 
    z = 0 # reset index
  }
    z = z + 1
}    
if (length(plot) != 0) { do.call(grid.arrange,  c(plot,ncol=1)) }
dev.off()
