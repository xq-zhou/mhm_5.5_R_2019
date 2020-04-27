# lateral loading


library(tidyverse)
library(lubridate)
library(xts)
library(ncdf4)
library(raster)
library(rasterVis)
library(RColorBrewer)

# mhm info
mhm_nc <- nc_open('./data/mHM_01_18.nc')
print(mhm_nc)
nc_close(mhm_nc)

# common part
# create spatial points
bode_qstation <- readxl::read_xls('./data/Bode_QWQ.xls')
bode_qstation_Sel <-
  bode_qstation %>% filter(ID_LHW %in% c('579610', '579620','579049','579745','579070','579085')) %>%
  dplyr::select(ID_LHW,DHDN_X,DHDN_Y)

coordinates(bode_qstation_Sel) =c('DHDN_X','DHDN_Y')

# group by seasons
# https://stackoverflow.com/questions/58198027/r-how-to-calculate-the-seasonal-average-value-for-each-year-using-stackapply
dates <-
  seq(as.Date("2001-01-01"), as.Date("2018-12-31"), by = "month")

months <- format(dates, "%Y-%m")

groups <- function(x) {
  d <- as.POSIXlt(x)

  ans <- character(length(x))
  ans[d$mon %in%  c(11, 0:1)] <- "DJF"
  ans[d$mon %in%  2:4] <- "MAM"
  ans[d$mon %in%  5:7] <- "JJA"
  ans[d$mon %in% 8:10] <- "SON"
  ans
}


## fastRunoff  [mm month-1]
mhm_Qfast <- raster::stack('./data/mHM_01_18.nc', varname = 'QIf')

## slowRunoff
mhm_Qslow <- raster::stack('./data/mHM_01_18.nc', varname = 'QIs')

## baseRunoff
mhm_Qbase <- raster::stack('./data/mHM_01_18.nc', varname = 'QB')

## total Runoff
mhm_Q_total <- raster::stack('./data/mHM_01_18.nc', varname = 'Q')


# cfastrunoff
wqm_cfast <- raster::stack('./data/WQM_01_18.nc', varname = 'cfastrunoff')

## cslowrunoff
wqm_cslow <- raster::stack('./data/WQM_01_18.nc', varname = 'cslowrunoff')

## cbaseflow
wqm_cbasefow <- raster::stack('./data/WQM_01_18.nc', varname = 'cbasefow')

## nitrate in total Runoff
wqm_ctotalrunoff <- raster::stack('./data/WQM_01_18.nc', varname = 'ctotalrunoff')


#  fastrunoff load
# mm/month* mg/L = 10^-6 kg/m2/month= 10^-2 kg/ha/month
load_fast <- mhm_Qfast*wqm_cfast
plot(load_fast[[1]])

# slow runoff load
load_slow <- mhm_Qslow*wqm_cslow
plot(load_slow[[1]])

# baseflow load
load_base <- mhm_Qbase*wqm_cbasefow
plot(load_base[[1]])

# total runoff load
load_total <-  mhm_Q_total* wqm_ctotalrunoff
plot(load_total[[1]])



# fast runoff load-----
# fast runoff load in 2001-2008
load_fast <- setZ(load_fast,dates)

load_fast_2001_08 <- load_fast[[which(date(getZ(load_fast))  >= as.Date("2001-01-01") &
                               date(getZ(load_fast)) <= as.Date("2008-12-31"))]]
# median seasonal load in 2001-2008
load_fast_2001_08_md <- stackApply(load_fast_2001_08,
           indices = groups(dates[1:96]),
           median,
           na.rm = T)
hist(load_fast_2001_08_md)

# fast runoff load in 2011-2018
load_fast_2011_18 <- load_fast[[which(date(getZ(load_fast))  >= as.Date("2011-01-01") &
          date(getZ(load_fast)) <= as.Date("2018-12-31"))]]
# median seasonal load in 2011-2018
load_fast_2011_18_md <- stackApply(load_fast_2011_18,
                                   indices = groups(dates[121:216]),
                                   median,
                                   na.rm = T)
hist(load_fast_2011_18_md)
# kg/ha/month
levelplot(load_fast_2011_18_md)

# fast runoff median seasonal load differences in two periods-----
#  10^-2 kg/ha/month = 30/100 kg/ha/d
load_fast_diff <- (load_fast_2011_18_md-load_fast_2001_08_md)/100*30

hist(load_fast_diff)
minValue(load_fast_diff)
maxValue(load_fast_diff)
#
fast_rcl <- matrix(c(-13, -10, -10.5),
               byrow = TRUE, ncol = 3)

load_fast_diff_rcl <- reclassify(load_fast_diff,fast_rcl)
#
fast.at= c(-10.5, -10, -5, 0, 2.6)
fast.colkey= list(space='bottom', at=fast.at, labels=list(at=c(-10,-5,0,2.5), labels=c("\u2264 -10", -5, 0,  "\u2265 2.5")))

fast.theme <- rasterTheme(region = brewer.pal(4, "RdYlBu"))

levelplot(load_fast_diff_rcl,
          at=fast.at,
          par.setting=fast.theme,
          colorkey=fast.colkey,
          main='The  difference of median seasonal fast runoff load [kg/ha/d] \n in 2011-2018 compared to 2001-2008' ,
          names.attr = c('Winter', 'Spring', 'Summer', 'Autumn'),
          scales = list(draw = FALSE))+
  layer(sp.points(bode_qstation_Sel,pch = 21,col='black'))


# slow runoff load-----
load_slow <- setZ(load_slow,dates)
# slow runoff load in 2001-2008
load_slow_2001_08 <- load_slow[[which(date(getZ(load_slow))  >= as.Date("2001-01-01") &
                                        date(getZ(load_slow)) <= as.Date("2008-12-31"))]]
# median seasonal load in 2001-2008
load_slow_2001_08_md <- stackApply(load_slow_2001_08,
                                   indices = groups(dates[1:96]),
                                   median,
                                   na.rm = T)
hist(load_slow_2001_08_md)

# slow runoff load in 2011-2018
load_slow_2011_18 <- load_slow[[which(date(getZ(load_slow))  >= as.Date("2011-01-01") &
                                        date(getZ(load_slow)) <= as.Date("2018-12-31"))]]
# median seasonal load in 2011-2018
load_slow_2011_18_md <- stackApply(load_slow_2011_18,
                                   indices = groups(dates[121:216]),
                                   median,
                                   na.rm = T)
hist(load_slow_2011_18_md)

# slow runoff load differences-----
# 10^-2 kg/ha/month = 30/100 kg/ha/d
load_slow_diff <- (load_slow_2011_18_md-load_slow_2001_08_md)/100*30

hist(load_slow_diff)
minValue(load_slow_diff)
maxValue(load_slow_diff)
# reclassify
slow_rcl <- matrix(c(-35, -10, -10.5,
                  5, 10, 5.5),
                byrow = TRUE, ncol = 3)

load_slow_diff_rcl <- reclassify(load_slow_diff,slow_rcl)
hist(load_slow_diff_rcl)
#
slow.at= c(-10.5, -10, -5, 0, 5, 5.5)
col.at = seq(-10,5,by=5)
slow.colkey= list(space='bottom', at=slow.at, labels=list(at=col.at, labels=c("\u2264 -10", -5, 0,  "\u2265 5")))

slow.theme <- rasterTheme(region = brewer.pal(5, "RdYlBu"))

levelplot(load_slow_diff_rcl,
          at=slow.at,
          par.setting=slow.theme,
          colorkey=slow.colkey,
          main='The difference of median seasonal slow interflow load [kg/ha/d] \n in 2011-2018 compared to 2001-2008' ,
          names.attr = c('Winter', 'Spring', 'Summer', 'Autumn'),
          scales = list(draw = FALSE))+
  layer(sp.points(bode_qstation_Sel,pch = 21,col='black'))

# baseflow load-----
load_base <- setZ(load_base,dates)
# base runoff load in 2001-2008
load_base_2001_08 <- load_base[[which(date(getZ(load_base))  >= as.Date("2001-01-01") &
                                        date(getZ(load_base)) <= as.Date("2008-12-31"))]]
# median seasonal load in 2001-2008
load_base_2001_08_md <- stackApply(load_base_2001_08,
                                   indices = groups(dates[1:96]),
                                   median,
                                   na.rm = T)
hist(load_base_2001_08_md)

# base flow load in 2011-2018
load_base_2011_18 <- load_base[[which(date(getZ(load_base))  >= as.Date("2011-01-01") &
                                        date(getZ(load_base)) <= as.Date("2018-12-31"))]]
# median seasonal load in 2011-2018
load_base_2011_18_md <- stackApply(load_base_2011_18,
                                   indices = groups(dates[121:216]),
                                   median,
                                   na.rm = T)
hist(load_base_2011_18_md)
# base flow  load differences-----
# 10^-2 kg/ha/month = 30/100 kg/ha/d
load_base_diff <- (load_base_2011_18_md-load_base_2001_08_md)/100*30

hist(load_base_diff)
minValue(load_base_diff)
maxValue(load_base_diff)

# reclassify
base_rcl <- matrix(c(-47, -10, -10.5,
                     5, 10, 5.5),
                   byrow = TRUE, ncol = 3)

load_base_diff_rcl <- reclassify(load_base_diff,base_rcl)

hist(load_base_diff_rcl)

#
base.at= c(-10.5, -10, -5, 0, 5, 5.5)

base.colkey= list(space='bottom', at=base.at, labels=list(at=seq(-10,5,by=5), labels=c("\u2264 -10", -5, 0,  "\u2265 5")))

base.theme <- rasterTheme(region = brewer.pal(5, "RdYlBu"))

levelplot(load_base_diff_rcl,
          at=base.at,
          par.setting=base.theme,
          colorkey=base.colkey,
          main='The  difference of median seasonal baseflow load [kg/ha/d] \n in 2011-2018 compared to 2001-2008' ,
          names.attr = c('Winter', 'Spring', 'Summer', 'Autumn'),
          scales = list(draw = FALSE))+
  layer(sp.points(bode_qstation_Sel,pch = 21,col='black'))

# total runoff load----
load_total <- setZ(load_total,dates)
# total runoff load in 2001-2008
load_total_2001_08 <- load_total[[which(date(getZ(load_total))  >= as.Date("2001-01-01") &
                                        date(getZ(load_total)) <= as.Date("2008-12-31"))]]
# median seasonal load in 2001-2008
load_total_2001_08_md <- stackApply(load_total_2001_08,
                                   indices = groups(dates[1:96]),
                                   median,
                                   na.rm = T)
hist(load_total_2001_08_md)

# total runoff load in 2011-2018
load_total_2011_18 <- load_total[[which(date(getZ(load_total))  >= as.Date("2011-01-01") &
                                        date(getZ(load_total)) <= as.Date("2018-12-31"))]]
# median seasonal load in 2011-2018
load_total_2011_18_md <- stackApply(load_total_2011_18,
                                   indices = groups(dates[121:216]),
                                   median,
                                   na.rm = T)
hist(load_total_2011_18_md)
#  total runoff load difference in two periods-----
load_total_diff <- (load_total_2011_18_md-load_total_2001_08_md)/100*30

hist(load_total_diff)
minValue(load_total_diff)
maxValue(load_total_diff)
#
total_rcl <- matrix(c(-79, -20, -20.5,
                      10, 18, 10.5),
                    byrow = TRUE, ncol = 3)

load_total_diff_rcl <- reclassify(load_total_diff,total_rcl)
hist(load_total_diff_rcl)
#
total.at = c(-20.5, -20, -15, -10, -5, 0,  5, 10, 10.5)

colkey = list(space = 'bottom',
              at = total.at,
              labels = list(
                at = seq(-20, 10, by = 5),
                labels = c("\u2264 -20",-15,-10,-5, 0,  5, "\u2265 10")
              ))

total.theme <- rasterTheme(region = brewer.pal(8, "RdYlBu"))

levelplot(load_total_diff_rcl,
          at=total.at,
          par.setting=total.theme,
          colorkey=colkey,
          main='The  difference of median seasonal total runoff load [kg/ha/d]\n in 2011-2018 compared to 2001-2008' ,
          names.attr = c('Winter', 'Spring', 'Summer', 'Autumn'),
          scales = list(draw = FALSE))+
  layer(sp.points(bode_qstation_Sel,pch = 21,col='black'))
