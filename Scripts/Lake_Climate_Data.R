library(tidyverse)
library(lubridate)

precip <- read_csv("big_data/precip.csv") %>% rename(Precip=Rain) %>% 
  rename(Date = time) %>% 
  select(WBIC,Date,Precip) %>% 
  mutate(Date = as.Date(Date))
evap <- read_csv("big_data/evap_daily.csv") %>% select(WBIC,Date,Evap)

#merge met data and remove other files
dat <- evap %>% left_join(precip)
dat$Year = year(dat$Date)
rm(evap)
rm(precip)

#get waterlevel data
waterlevels <- read.csv("big_data/seepage_20180414.csv")

#Filter data to lakes with observations
waterlevels <- waterlevels[which(waterlevels$WBIC %in% dat$WBIC),]
dat <- dat[which(dat$WBIC %in% waterlevels$WBIC),]
waterlevels <- waterlevels %>% select(WiscID,WBIC,Value,Date) %>% 
  mutate(Value = as.numeric(as.character(Value))) %>% 
  mutate(Date = as.Date(as.character(Date),"%m/%d/%Y")) %>% 
  mutate(Value = Value*304.8) %>% 
  mutate(Year = year(Date)) %>% 
  mutate(DoY = yday(Date)) %>% 
  filter(DoY >= 106 & DoY <=320)  %>% #April 15 to Nov 15
  group_by(WiscID,Year) %>% 
  mutate(count = n()) %>% filter(count >2) %>% ungroup() %>% arrange(WiscID,Date)

waterlevel_deltas <- data.frame(WiscID=numeric(),
                                WBIC=numeric(),
                                Date1=character(),
                                Value1=numeric(),
                                Date2 = character(),
                                Value2 = numeric(),
                                stringsAsFactors = FALSE)

lakes <- unique(waterlevels$WiscID)
for(i in 1:length(lakes)){
  temp.lake <- waterlevels %>% filter(WiscID==lakes[i])
  years <- unique(temp.lake$Year)
  for(j in 1:length(years)){
    temp.year <- temp.lake %>% filter(Year==years[j])
    if(nrow(temp.year)<2) next else {
      max.t = nrow(temp.year)
      for(t in 1:(max.t-1)){
        which.date <- min(which((temp.year$DoY[t:max.t] - temp.year$DoY[t])>9))-1
        if(is.infinite(which.date)) break() else {
          waterlevel_deltas[nrow(waterlevel_deltas) + 1,1:6] = c(temp.year$WiscID[t],
                                                                 temp.year$WBIC[t],
                                                                 as.character(temp.year$Date[t]),
                                                                 temp.year$Value[t],
                                                                 as.character(temp.year$Date[t+which.date]),
                                                                 temp.year$Value[t+which.date])
        }
      }
    }
  }
}

waterlevel_deltas <- waterlevel_deltas %>% 
  mutate(Date1 = as.Date(Date1)) %>% 
  mutate(Date2 = as.Date(Date2)) %>%
  mutate(Year = year(Date1)) %>% 
  group_by(WBIC) %>% 
  mutate(count = n()) %>% filter(count >4) %>% ungroup() %>% arrange(WBIC,Date1) %>% 
  mutate(id = seq(from=1,to=n(),by=1)) %>% 
  mutate(totalP = NA) %>% 
  mutate(totalE = NA)

lakes <- unique(waterlevel_deltas$WBIC)
for(i in 1:length(lakes)){
  temp.lake <- waterlevel_deltas %>% filter(WBIC==lakes[i])
  years <- unique(temp.lake$Year)
  for(j in 1:length(years)){
    temp.year <- temp.lake %>% filter(Year==years[j])
    temp.dat <- dat %>% filter(WBIC==lakes[i] & Year==years[j])
    date_matrix = tibble(Date = seq(from=as.Date(paste(years[j],"-01-01",sep="")),to=as.Date(paste(years[j],"-12-31",sep="")),by="day"))
    temp.dat <- date_matrix %>% left_join(temp.dat)
    for(t in 1:nrow(temp.year)){
      waterlevel_deltas$totalE[temp.year$id[t]] <- sum(temp.dat$Evap[which(temp.dat$Date>=temp.year$Date1[t] & temp.dat$Date<=temp.year$Date2[t])])
      waterlevel_deltas$totalP[temp.year$id[t]] <- sum(temp.dat$Precip[which(temp.dat$Date>=temp.year$Date1[t] & temp.dat$Date<=temp.year$Date2[t])])
    }
  }
}

waterlevel_deltas <- waterlevel_deltas %>% drop_na()
write_csv(x = waterlevel_deltas,path = "data/waterlevel_delta.csv")
waterlevel_deltas <- read.csv("data/waterlevel_delta.csv") %>% mutate(Date1 = as.Date(Date1)) %>% 
  mutate(Date2 = as.Date(Date2))
summary(waterlevel_deltas)
# waterlevel_deltas <- waterlevel_deltas %>% rename(totalP = deltaP) %>% rename(totalE = deltaE)
waterlevel_deltas <- waterlevel_deltas %>% drop_na() %>% 
  mutate(days = as.numeric(Date2-Date1)) %>% 
  mutate(delta_level = (as.numeric(Value2)-as.numeric(Value1))/days) %>% 
  mutate(deltaPE = (totalP+totalE)/days) %>% 
  arrange(WBIC,Date1) %>% 
  group_by(WBIC,Year) %>% 
  mutate(obs_lag = yday(Date1)) %>%
  mutate(obs_lag = obs_lag - lag(obs_lag,n=1)) %>% 
  mutate(level_lag = as.numeric(Value1) - lag(as.numeric(Value1),n=1)) %>% 
  mutate(deltal_lag =delta_level - lag(delta_level,n=1)) %>% 
  mutate(code = paste(obs_lag,level_lag,deltal_lag,sep="")) %>% 
  ungroup() %>% 
  filter(code != "100") %>% 
  mutate(missing = delta_level - deltaPE)

p1 <- ggplot(data = waterlevel_deltas,aes(x=deltaPE,y=delta_level)) + geom_point()
p1
ggplot(data = waterlevel_deltas, aes(x=1,y=missing)) + 
  geom_point(alpha=0.2,position='jitter') +
  geom_boxplot(outlier.size=4, outlier.colour='blue', alpha=0.1,coef=5.5)

(outlier.stats <- boxplot.stats(waterlevel_deltas$missing,coef = 5.5))
waterlevel_deltas <- waterlevel_deltas %>% filter(missing >outlier.stats$stats[1] & missing < outlier.stats$stats[5])
p1 + geom_point(data=waterlevel_deltas,aes(x=deltaPE,y=delta_level),color="red")
hist(waterlevel_deltas$missing)
summary(waterlevel_deltas$missing)
write_csv(waterlevel_deltas %>% select(-missing,-code,-deltal_lag,-level_lag,-obs_lag),"data/cleaned_waterlevel_deltas.csv")
