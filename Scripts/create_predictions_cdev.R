library(tidyverse)
ecocontext_full <- read_csv("/Users/noahlottig/Documents/GitHub/waterlevels/data/ecocontext_full.csv")
regressionstats <- read_csv("/Users/noahlottig/Documents/GitHub/waterlevels/BayHModels/regressionstats.csv")
dat <- merge(regressionstats, ecocontext_full, by.x = "WiscID", by.y = "WiscID")
dat_input <- cbind(dat$slope,dat$MaxDepth, dat$W_PERM, dat$W_DARCY, dat$cond,
                   dat$elevation_difference,dat$Area,dat$r_forest)
name_list <- c("slope","MaxDepth", "W_PERM", "W_DARCY", "cond", "elevation_difference", "Area", "r_forest")
dat_input <- as.data.frame(dat_input)
colnames(dat_input) <- name_list
fit2 <- lm(slope ~ elevation_difference + r_forest*W_DARCY,data = dat_input)
summary(fit2)

dat_all <- read_csv("/Users/noahlottig/Documents/GitHub/waterlevels/data/ecocontext_full_all_lakes.csv") %>% 
    select(WBIC,elevation_difference,r_forest,W_DARCY) %>% drop_na()
summary(dat_input)

dat_all <- dat_all %>% 
    filter(elevation_difference >=5) %>% 
    filter(elevation_difference <= 62) %>% 
    filter(W_DARCY >= -503) %>%
    filter(W_DARCY <= 6) %>% 
    filter(r_forest >= 2) %>% 
    filter(r_forest <= 88)

slopes <- predict(object = fit2,newdata = dat_all)

dat_all <- dat_all %>% 
    mutate(slope=slopes)

write_csv(dat_all,path = "/Users/noahlottig/Documents/GitHub/waterlevels/data/cdev_slope_pred")
