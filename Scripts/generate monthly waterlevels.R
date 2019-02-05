library(lubridate)
seepage_20180414 <- read.csv("big_data/seepage_20180414.csv",header=TRUE)
dat <- seepage_20180414 %>% select(WBIC,WiscID,Value,Date) %>% 
  mutate(Date = as.Date(as.character(Date),format="%m/%d/%Y")) %>% 
  mutate(Year = year(Date)) %>% 
  mutate(Month = month(Date)) %>% 
  mutate(Value = as.numeric(as.character(Value))*304.8) %>% 
  drop_na() %>% 
  group_by(WiscID,Year,Month) %>% 
  summarize(Value = mean(Value)) %>% 
  mutate(Date = as.Date(paste(Year,"-",Month,"-01",sep="")))
summary(dat)
write_csv(x = dat,path = "data/monthly_waterlevels.csv")
