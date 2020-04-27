# lateral runoff component and percentage


library(tidyverse)
library(lubridate)
library(xts)
library(ncdf4)
library(raster)
library(rasterVis)
library(RColorBrewer)

# IDs between silb and meis station
# library(foreign)
# IDs_silb_meis <- read.dbf('./data/Between_silb_meis_ID.dbf')
# IDs_meis_haus <- read.dbf('./data/Between_meis_haus_ID.dbf')
# points(IDs_silb_meis$E,IDs_silb_meis$N,cex=0.5)
# points(IDs_meis_haus$E,IDs_meis_haus$N,cex=0.5,col=2)

# raster info
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

#


# QIs:slowRunoff   QIf:(fastRunoff)  QB:baseflow

## fastRunoff----
mhm_Qfast <- raster::stack('./data/mHM_01_18.nc', varname = 'QIf')
mhm_Qfast
plot(mhm_Qfast[[1]])

## select 2001-2008 periods
mhm_Qfast_2001_08 <-
  mhm_Qfast[[which(date(getZ(mhm_Qfast))  >= as.Date("2001-01-01") &
                     date(getZ(mhm_Qfast)) <= as.Date("2008-12-31"))]]
mhm_Qfast_2001_08
# Apply a function on subsets of a RasterStack
mhm_Qfast_2001_08_md <-
  stackApply(mhm_Qfast_2001_08,
             indices = groups(dates[1:96]),
             median,
             na.rm = T)

levelplot(mhm_Qfast_2001_08_md)
## select 2011-2018 periods

mhm_Qfast_2011_18 <-
  mhm_Qfast[[which(date(getZ(mhm_Qfast))  >= as.Date("2011-01-01") &
                     date(getZ(mhm_Qfast)) <= as.Date("2018-12-31"))]]
mhm_Qfast_2011_18
# Apply a function on subsets of a RasterStack
mhm_Qfast_2011_18_md <-
  stackApply(mhm_Qfast_2011_18,
             indices = groups(dates[121:216]),
             median,
             na.rm = T)

levelplot(mhm_Qfast_2011_18_md)
hist(mhm_Qfast_2011_18_md)
# fast runoff in recent period compared to historical period-----
mhm_Qfast_diff <- mhm_Qfast_2011_18_md - mhm_Qfast_2001_08_md

hist(mhm_Qfast_diff)

qfast.colkey=list(space='bottom',at=c(-5,-1,0,1),labels=list(at=c(-1,0,1),lalels=c(-1,0,1) ))
qfast.theme <- rasterTheme(region = brewer.pal(4, "RdYlBu"))


levelplot(mhm_Qfast_diff,
          par.setting=qfast.theme,
          colorkey=qfast.colkey,
          main='The seasonal median fast interflow (mm) in 2011-2018 compared to 2001-2008' ,
          names.attr = c('Winter', 'Spring', 'Summer', 'Autumn'),
          scales = list(draw = FALSE)
          )+
  layer(sp.points(bode_qstation_Sel,pch = 21,col='black'))


## slowRunoff----
mhm_Qslow <- raster::stack('./data/mHM_01_18.nc', varname = 'QIs')
mhm_Qslow
plot(mhm_Qslow[[1]])

## select 2001-2008 periods
mhm_Qslow_2001_08 <-
  mhm_Qslow[[which(date(getZ(mhm_Qslow))  >= as.Date("2001-01-01") &
                     date(getZ(mhm_Qslow)) <= as.Date("2008-12-31"))]]
mhm_Qslow_2001_08
# Apply a function on subsets of a RasterStack
mhm_Qslow_2001_08_md <-
  stackApply(mhm_Qslow_2001_08,
             indices = groups(dates[1:96]),
             median,
             na.rm = T)
hist(mhm_Qslow_2001_08_md)
levelplot(mhm_Qslow_2001_08_md)

## select 2011-2018 periods
mhm_Qslow_2011_18 <-
  mhm_Qslow[[which(date(getZ(mhm_Qslow))  >= as.Date("2011-01-01") &
                     date(getZ(mhm_Qslow)) <= as.Date("2018-12-31"))]]
mhm_Qslow_2011_18
# Apply a function on subsets of a RasterStack
mhm_Qslow_2011_18_md <-
  stackApply(mhm_Qslow_2011_18,
             indices = groups(dates[121:216]),
             median,
             na.rm = T)

hist(mhm_Qslow_2011_18_md)
levelplot(mhm_Qslow_2011_18_md)

# slow runoff in recent period  compared to historical period-------
mhm_Qslow_diff <- mhm_Qslow_2011_18_md - mhm_Qslow_2001_08_md

hist(mhm_Qslow_diff)
maxValue(mhm_Qslow_diff)
minValue(mhm_Qslow_diff)
#
 slow_rcl <- matrix(c(-21, -10, -10.5,
                   5, 10, 5.5),
                 byrow = TRUE, ncol = 3)

 mhm_Qslow_diff_rcl <- reclassify(mhm_Qslow_diff,slow_rcl)

#
 qslow.at=c(-10.5, -10, -5, 0, 5, 5.5)
 qslow.colkey=list(space='bottom',at=qslow.at ,lables=list(at=seq(-10,5,by=5), lables=c("\u2264 -10", -5,  0, "\u2265 5"  )))
 qslow.coltheme = rasterTheme(region = brewer.pal(5, "RdYlBu"))

levelplot(
  mhm_Qslow_diff_rcl,
  par.settings = qslow.coltheme,
  at=qslow.at,
  colorkey=qslow.colkey,
  main = 'The seasonal median slow interflow (mm) in 2011-2018 compared to 2001-2008' ,
  names.attr = c('Winter', 'Spring', 'Summer', 'Autumn'),
  scales = list(draw = FALSE)
)+
  layer(sp.points(bode_qstation_Sel,pch = 21,col='black'))


## baseRunoff----
mhm_Qbase <- raster::stack('./data/mHM_01_18.nc', varname = 'QB')
mhm_Qbase
plot(mhm_Qbase[[1]])

## select 2001-2008 periods
mhm_Qbase_2001_08 <-
  mhm_Qbase[[which(date(getZ(mhm_Qbase)) >= as.Date("2001-01-01") &
                     date(getZ(mhm_Qbase)) <= as.Date("2008-12-31"))]]
mhm_Qbase_2001_08
# Apply a function on subsets of a RasterStack
mhm_Qbase_2001_08_md <-
  stackApply(mhm_Qbase_2001_08,
             indices = groups(dates[1:96]),
             median,
             na.rm = T)

hist(mhm_Qbase_2001_08_md)

levelplot(mhm_Qbase_2001_08_md)

## select 2011-2018 periods
mhm_Qbase_2011_18 <-
  mhm_Qbase[[which(date(getZ(mhm_Qbase))  >= as.Date("2011-01-01") &
                     date(getZ(mhm_Qbase)) <= as.Date("2018-12-31"))]]
mhm_Qbase_2011_18
# Apply a function on subsets of a RasterStack
mhm_Qbase_2011_18_md <-
  stackApply(mhm_Qbase_2011_18,
             indices = groups(dates[121:216]),
             median,
             na.rm = T)

hist(mhm_Qbase_2011_18_md)
levelplot(mhm_Qbase_2011_18_md)

# baseflow in recent period  compared to historical period------
mhm_Qbase_diff <- mhm_Qbase_2011_18_md - mhm_Qbase_2001_08_md

hist(mhm_Qbase_diff)
minValue(mhm_Qbase_diff)
maxValue(mhm_Qbase_diff)

Qbase_rcl <- matrix(c(-23, -5, -5.5,
                 5, 16, 5.5),
               byrow = TRUE, ncol = 3)

mhm_Qbase_diff_rcl <- reclassify(mhm_Qbase_diff,Qbase_rcl)
hist(mhm_Qbase_diff_rcl)
#
qbase.at = c(-5.5, -5, 0, 5, 5.5)
qbase.colbrk= seq(-5, 5, by = 5)
qbase.colkey= list(space='bottom',at=qbase.at, lables=list(at=qbase.colbrk, labels=c("\u2264 -5", 0, "\u2265 5" )))
qbase.theme <- rasterTheme(region = brewer.pal(4, "RdYlBu"))

levelplot(
  mhm_Qbase_diff_rcl,
  par.settings = qbase.theme,
  at = qbase.at,
  colorkey=qbase.colkey,
  main = 'The seasonal median baseflow (mm) in 2011-2018 compared to 2001-2008',
  names.attr = c('Winter', 'Spring', 'Summer', 'Autumn'),
  scales = list(draw = FALSE)
) +
  layer(sp.points(bode_qstation_Sel,pch = 21,col='black'))


#############
# runoff component percentage
#############
## total Runoff----
mhm_Q_total <- raster::stack('./data/mHM_01_18.nc', varname = 'Q')
mhm_Q_total
plot(mhm_Q_total[[1]])

# fast runoff percentage-----------
mhm_qfast_perct <- mhm_Qfast / mhm_Q_total

 maxValue(mhm_qfast_perct)

mhm_qfast_perct <- setZ(mhm_qfast_perct, dates)
## select 2001-2008 periods
mhm_qfast_perct_2001_08 <-
  mhm_qfast_perct[[which(date(getZ(mhm_qfast_perct)) >= as.Date("2001-01-01") &
                           date(getZ(mhm_qfast_perct)) <= as.Date("2008-12-31"))]]
mhm_qfast_perct_2001_08
# Apply a function on subsets of a RasterStack
mhm_qfast_perct_2001_08_md <-
  stackApply(mhm_qfast_perct_2001_08,
             indices = groups(dates[1:96]),
             median,
             na.rm = T)

## select 2011-2018 periods
mhm_qfast_perct_2011_08 <-
  mhm_qfast_perct[[which(date(getZ(mhm_qfast_perct))  >= as.Date("2011-01-01") &
                           date(getZ(mhm_qfast_perct)) <= as.Date("2018-12-31"))]]
#
mhm_qfast_perct_2011_08_md <-
  stackApply(mhm_qfast_perct_2011_08,
             indices = groups(dates[121:216]),
             median,
             na.rm = T)


hist(mhm_qfast_perct_2011_08_md)
levelplot(mhm_qfast_perct_2011_08_md*100)

# fast runoff percentage difference in two periods----------
mhm_qfast_perct_diff <-
  (mhm_qfast_perct_2011_08_md - mhm_qfast_perct_2001_08_md)*100

hist(mhm_qfast_perct_diff)
maxValue(mhm_qfast_perct_diff)
minValue(mhm_qfast_perct_diff)
#qfast_perct reclassify
qfast_perct_rcl <- matrix(c(-14, -5, -5.5,
         4, 14, 4.5),
       byrow = TRUE, ncol = 3)

mhm_qfast_perct_diff_rcl <- reclassify(mhm_qfast_perct_diff,qfast_perct_rcl)
#qfast_perct plot
qfast_perct.at=c( -5.5,-5,0,4,4.5 )
qfast_perct.brk=c(-5,0,4)
qfast_perct.colkey=list(space='bottom',at=qfast_perct.at,labels=list(at=qfast_perct.brk,labels=c("\u2264 -5", 0, "\u2265 4")))
qfast_perct.theme <- rasterTheme(region = brewer.pal(4, "RdYlBu"))

levelplot(mhm_qfast_perct_diff_rcl,
          par.setting=qfast_perct.theme,
          at=qfast_perct.at,
          colorkey=qfast_perct.colkey,
          main='The proportion of fast interflow component contribute to total discharge (%) in 2011-2018 compared to 2001-2008' ,
          names.attr = c('Winter', 'Spring', 'Summer', 'Autumn'),
          scales = list(draw = FALSE) # remove axes labels & ticks
          )


## slow runoff percentage-----
mhm_qslow_perct <- mhm_Qslow / mhm_Q_total
mhm_qslow_perct
#
mhm_qslow_perct <- setZ(mhm_qslow_perct, dates)
### select 2001-2008 periods

mhm_qslow_perct_2001_08 <-
  mhm_qslow_perct[[which(date(getZ(mhm_qslow_perct)) >= as.Date("2001-01-01") &
                           date(getZ(mhm_qslow_perct)) <= as.Date("2008-12-31"))]]
mhm_qslow_perct_2001_08
# Apply a function on subsets of a RasterStack
mhm_qslow_perct_2001_08_md <-
  stackApply(mhm_qslow_perct_2001_08,
             indices = groups(dates[1:96]),
             median,
             na.rm = T)

### select 2011-2018 periods
mhm_qslow_perct_2011_08 <-
  mhm_qslow_perct[[which(date(getZ(mhm_qslow_perct))  >= as.Date("2011-01-01") &
                           date(getZ(mhm_qslow_perct)) <= as.Date("2018-12-31"))]]
#
mhm_qslow_perct_2011_08_md <-
  stackApply(mhm_qslow_perct_2011_08,
             indices = groups(dates[121:216]),
             median,
             na.rm = T)
hist(mhm_qslow_perct_2011_08_md)
levelplot(mhm_qslow_perct_2011_08_md)

# slow runoff percentage difference in two periods plot------
mhm_qslow_perct_diff <-
  (mhm_qslow_perct_2011_08_md - mhm_qslow_perct_2001_08_md)*100

hist(mhm_qslow_perct_diff)
min(minValue(mhm_qslow_perct_diff))
max(maxValue(mhm_qslow_perct_diff))
# set col at
qslow_perct.at = c(min(minValue(mhm_qslow_perct_diff)),-30,-20,-10,0,10,20,max(maxValue(mhm_qslow_perct_diff)))
# set colkey
qslow_perct.colbrk=c(-30,-20,-10,0,10,20)
qslow_perct.colkey=list(space='bottom',at=qslow_perct.at,labels=list(at=qslow_perct.colbrk,labels=c('\u2264 -30',-20,-10,0,10,'\u2265 20' )))
# set col theme
qslow_perct.theme <- rasterTheme(region = brewer.pal(7, "RdYlBu"))

levelplot(mhm_qslow_perct_diff,
          par.setting = qslow_perct.theme,
          at = qslow_perct.at,
          colorkey = qslow_perct.colkey,
          main = 'The proportion of slow interflow component contribute to total discharge (%) in 2011-2018 compared to 2001-2008' ,
          names.attr = c('Winter', 'Spring', 'Summer', 'Autumn'),
          scales = list(draw = FALSE) # remove axes labels & ticks
          ) +
  layer(sp.points(bode_qstation_Sel,pch = 21,col='black'))


# ## base flow percentage----

mhm_Qbase_perct <- mhm_Qbase / mhm_Q_total
#
mhm_Qbase_perct <- setZ(mhm_Qbase_perct, dates)
### select 2001-2008 periods
mhm_Qbase_perct_2001_08 <-
  mhm_Qbase_perct[[which(date(getZ(mhm_Qbase_perct)) >= as.Date("2001-01-01") &
                           date(getZ(mhm_Qbase_perct)) <= as.Date("2008-12-31"))]]
mhm_Qbase_perct_2001_08
# Apply a function on subsets of a RasterStack
mhm_Qbase_perct_2001_08_md <-
  stackApply(mhm_Qbase_perct_2001_08,
             indices = groups(dates[1:96]),
             median,
             na.rm = T)

### select 2011-2018 periods
mhm_Qbase_perct_2011_08 <-
  mhm_Qbase_perct[[which(date(getZ(mhm_Qbase_perct))  >= as.Date("2011-01-01") &
                           date(getZ(mhm_Qbase_perct)) <= as.Date("2018-12-31"))]]
#
mhm_Qbase_perct_2011_08_md <-
  stackApply(mhm_Qbase_perct_2011_08,
             indices = groups(dates[121:216]),
             median,
             na.rm = T)

hist(mhm_Qbase_perct_2011_08_md)

levelplot(mhm_Qbase_perct_2011_08_md*100)

# base flow percentage difference in two periods plot---------
mhm_Qbase_perct_diff <-
  (mhm_Qbase_perct_2011_08_md - mhm_Qbase_perct_2001_08_md)*100

hist(mhm_Qbase_perct_diff)

# https://stackoverflow.com/questions/34835669/use-a-single-colour-for-range-of-values-and-include-inequality-symbol-on-colorke
## reclassify data to  set all values smaller than -20 (or larger than 20) e.g. to -20.5 (or 20.5)
#  not interested in smaller or larger values
m_rcl <- matrix(c(-100, -20, -20.5,
                  30, 100, 30.5),
                byrow = TRUE, ncol = 3)

Qbase_perct_rcl <- reclassify(mhm_Qbase_perct_diff, m_rcl)
 hist(Qbase_perct_rcl)
# set col at
Qbase_perct.at = c(-21,-20,-10,0,10,20,30,31)
# set colkey
Qbase_perct.brk=seq(-20,30,by=10 )
Qbase_perct.colkey=list(space = "bottom",at=Qbase_perct.at,labels=list(at=Qbase_perct.brk,labels=c("\u2264 -20", -10, 0, 10, 20, "\u2265 30")))
# set col theme
Qbase_perct.theme <- rasterTheme(region = brewer.pal(7, "RdYlBu"))

hist(Qbase_perct_rcl)
levelplot(Qbase_perct_rcl,
          par.setting = Qbase_perct.theme,
          at = Qbase_perct.at,
          colorkey = Qbase_perct.colkey,
          main = 'The proportion of baseflow component contribute to total discharge (%) in 2011-2018 compared to 2001-2008' ,
          names.attr = c('Winter', 'Spring', 'Summer', 'Autumn'),
          scales = list(draw = FALSE) # remove axes labels & ticks
) +
  layer(sp.points(bode_qstation_Sel,pch = 21,col='black'))

