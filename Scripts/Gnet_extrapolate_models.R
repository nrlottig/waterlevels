library(tidyverse)
library(lubridate)
library(foreach)
library(doParallel)
library(iterators)

registerDoParallel(cores = 3)

# ####The Data
# Gnet_slopes <- read_csv("data/Gnet_slopes.csv") %>% select(WBIC)
# 
# #limit the evap and precip data to the 50 lakes we have GW recharge estimates 
# evap_daily <- read_csv("big_data/evap_daily.csv", 
#                        col_types = cols(X1 = col_skip())) %>% 
#   distinct(WBIC, Date,.keep_all = TRUE) %>% right_join(Gnet_slopes)
# precip_daily <- read_csv("big_data/precip_daily.csv", 
#                          col_types = cols(X1 = col_skip())) %>% 
#   distinct(WBIC, Date,.keep_all = TRUE) %>% right_join(Gnet_slopes)
# 
# dat <- evap_daily %>% full_join(precip_daily)
# write_tsv(dat,"big_data/gnet_met_data.csv")
gnet_met_data <- read_delim("big_data/gnet_met_data.csv", 
                            "\t", escape_double = FALSE, trim_ws = TRUE)
gnet_mu_sims <- read_csv("data/gnet_mu_sims.csv")
waterlevel <- read.csv("big_data/seepage_20180414.csv",sep=",",header=TRUE) %>% 
  mutate(Date = as.Date(as.character(Date),format= "%m/%d/%Y")) %>% 
  mutate(Value = as.numeric(as.character(Value))*304.8) %>% 
  select(WBIC,Date,Value)

#simmulations

lakes = unique(gnet_met_data$WBIC)
for(i in 1:length(lakes)) {
  dat <- gnet_met_data %>% filter(WBIC==1842400)
  dat_level <- waterlevel %>% filter(WBIC==1842400) %>% 
    filter(Value>50000)
  date_matrix = tibble(Date = seq(from=min(dat$Date),to=max(dat$Date),by="day"))
  dat <- date_matrix %>% left_join(dat)
  dat <- dat %>% mutate(Evap = replace_na(Evap, median(Evap,na.rm=TRUE) )) %>% 
    mutate(Precip = replace_na(Precip, median(Precip,na.rm=TRUE) ))
  met_summary <- dat %>% mutate(year=year(Date)) %>% 
    dplyr:::select(year,Evap,Precip) %>% 
    group_by(year) %>% 
    summarise_all(funs(sum))
  ggplot(data= met_summary) + geom_line(aes(x=year,y=Precip)) + geom_line(aes(x=year,y=Evap))
  
  trials = 100
  out_loop <- foreach(icount(trials), .combine=cbind) %dopar% {
    S = matrix(nrow = nrow(dat),ncol = 1)
    S[1,] = 100*1000
    for(z in 2:nrow(S)){
      sims <- sample_n(gnet_mu_sims,1)
      S[z,1] <- S[(z-1),1] + sims[[1,2]]*(dat$Precip[z] + dat$Evap[z]) - (.000024*(S[(z-1),1]) + sims[[1,1]])
    } #end of time series loop
    # S
    extrap_level <- data.frame(Date= dat$Date, level = S)
    dat.compare = dat_level %>% left_join(extrap_level) %>% 
      mutate(offset = level-Value)
    offset <- mean(dat.compare$offset)
    ggplot(data = extrap_level,aes(x=Date,y=level)) + 
      geom_line(data=dat_level,aes(x=Date,y=(Value + offset)),color="red") + 
      geom_line()
      
} #end 1000 simulation loop
  out <- as.data.frame(t(apply(out_loop, 1, quantile, c(0.025,0.5,0.975))))
  out <- cbind(dat$WBIC,dat$Date,out)
  names(out) <- c("WBIC","Date","ll_val","median_val","ul_val")
  } #end lake loop

ggplot(data = out,aes(x=Date,y=median_val)) + geom_line() +
  geom_line(aes(x=Date,y=ll_val),color="lightblue") + geom_line(aes(x=Date,y=ul_val),color="lightblue")
