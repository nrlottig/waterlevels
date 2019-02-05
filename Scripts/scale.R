rm(list=ls())
library(tidyverse)
library(stringr)
dat = read_tsv("BayHModels/BHM_input_20180410.csv")

dat2 <- dat %>% group_by(WiscID) %>% 
  mutate(Value=scale(Value,center = TRUE,scale = FALSE)) %>% 
  mutate(precipCMDV = scale(precipCMDV,center = TRUE,scale = FALSE)) %>% 
  ungroup() %>% 
  mutate(Year = as.numeric(str_extract(Date,"\\d\\d\\d\\d"))) %>% 
  group_by(WiscID,Year) %>% group_by(WiscID) %>% mutate(count=length(unique(Year))) %>% 
  mutate(precip_range = (max(precipCMDV)-min(precipCMDV))) %>% 
  mutate(level_range  = (max(Value)-min(Value))) %>% 
  ungroup()

dat3 <- dat2 %>% group_by(WiscID) %>% 
  summarise(n=mean(count),level_range=mean(level_range),precip_range=mean(precip_range))

ggplot(data = dat3,aes(x=n)) + geom_histogram(bins=50) + geom_vline(xintercept = 5)
ggplot(data = dat3,aes(x=level_range)) + geom_histogram(bins=50)
ggplot(data = dat3,aes(x=precip_range)) + geom_histogram(bins=50)

dat4 <- dat2 %>% filter(count>=10)
ggplot(data = dat4,aes(x=precipCMDV,y=Value)) + geom_point() + facet_wrap(vars(WiscID),scales="free")

write_csv(dat4,"data/cmdv_level.csv")
