library(tidyverse)
library(randomForest)
library(VSURF)
ecocontext_full <- read_csv("/Users/noahlottig/Documents/GitHub/waterlevels/data/ecocontext_full_all_lakes.csv")
lake_gnet_coef <- read_csv("data/lake_gnet_coef.csv")
dat <- lake_gnet_coef %>% left_join(ecocontext_full) %>% drop_na(LakeName)
dat <- dat %>%  select(-W_LAT,
                                                                 -MeanDepth,
                                                                 -SHAPE_AREA,
                                                                 -SHAPE_LEN,
                                                                 -W_AREA,
                                                                 -W_PRCP_ANN,
                                                                 -W_TEMP_ANN,
                                                                 -W_TEMP_GS,
                                                                 -W_TEMP_JUL,
                       -LakeName,
                       -US_L3NAME,
                       -ECO_LANDSC,
                       -HUC12_CODE) %>% 
  select_if(~ !any(is.na(.)))
dat.ld <- dat[,c(5:ncol(dat))]
names(dat.ld)
out <- VSURF(x=dat.ld,y=dat$coef,ntree=10000,nfor.thres = 1000,nfor.interp = 500,nfor.pred = 500,parallel = T)
names(dat.ld)[out$varselect.thres]
names(dat.ld)[out$varselect.interp]
names(dat.ld)[out$varselect.pred]

rf.fit <- randomForest(x=dat[,names(dat.ld)[out$varselect.pred]],y=dat$coef,ntree = 10000)
rf.fit

lm.fit <- lm(coef ~ lake_MEAN+MaxDepth+cond,data = dat)
summary(lm.fit)
plot(dat$coef,predict(lm.fit))
abline(a=0,b=1)
library(glmulti)
library(doParallel)
cl <- parallel::makeForkCluster(7)
# cl <- parallel::makeCluster(7)
doParallel::registerDoParallel(cl)
getDoParWorkers()

system.time(glmulti.lm.out2 <- 
              glmulti("coef", c("MaxDepth", "W_PERM", "W_DARCY", "cond", "elevation_difference", "Area", "r_forest"), data = dat2,
                      level = 1,               # No interaction considered
                      method = "h",            # Exhaustive approach
                      marginality = TRUE,      # forces inclusion of main effect parameters in interactions
                      crit = "aicc",            # AIC as criteria
                      confsetsize = 10,         # Keep 5 best modelS
                      fitfunction = "lm",     # lm function
                      maxsize = 3,
                      minsize = -1,
                      chunks=1,chunk = 1,
                      plotty = F, report = T))     



system.time({
  glmulti.lm.out <- foreach(i=1:7) %dopar% {
    z<-c(1:7)
    t<-z[i]
    glmulti::glmulti("coef", c("MaxDepth", "W_PERM", "W_DARCY", "cond", "elevation_difference", "SDI", "slopenearest", "Area", "r_forest"),
            data = dat,
            level = 2,               # No interaction considered
            method = "g",            # Exhaustive approach
            marginality = TRUE,      # forces inclusion of main effect parameters in interactions
            crit = "aicc",            # AIC as criteria
            confsetsize = 10,         # Keep 5 best modelS
            fitfunction = "lm",     # lm function
            maxsize = 3,
            minsize = -1,
            chunks=7,
            chunk = t,
            plotty = F,
            report = F)
    }

  })


dat2 <- ecocontext_full %>% 
  select(WBIC,MaxDepth,lake_MEAN,cond) %>% 
  drop_na() %>% 
  filter(MaxDepth>=12 & MaxDepth <=67) %>% 
  filter(lake_MEAN >=256 & lake_MEAN <= 517) %>% 
  filter(cond >= 16 & cond <=412)

new.preds <- predict(object = lm.fit,newdata = dat2)
new.preds <- data.frame(WBIC=dat2$WBIC,gnet=new.preds)

write_csv(x = new.preds,path = "data/gnet_pred_coefs.csv")
