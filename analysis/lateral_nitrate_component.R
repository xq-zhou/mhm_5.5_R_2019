# lateral nitrate concentration and percentage of different runoff component nitrate concentration
# &  instream uptake and denitri seasonal charge in two periods

library(tidyverse)
library(lubridate)
library(xts)
library(ncdf4)
library(raster)
library(rasterVis)
library(RColorBrewer)


# raster info
wqm_nc <- nc_open('./data/WQM_01_18.nc')
print(wqm_nc)
nc_close(wqm_nc)

# in the sequence of cfastrunoff,cslowrunoff,cbasefow,ctotalrunoff

# commom code for group raster stack to season
# create spatial points
bode_qstation <- readxl::read_xls('./data/Bode_QWQ.xls')
bode_qstation_Sel <-
  bode_qstation %>% filter(ID_LHW %in% c('579610', '579620','579049','579745','579070','579085')) %>%
  dplyr::select(ID_LHW,DHDN_X,DHDN_Y)

coordinates(bode_qstation_Sel) =c('DHDN_X','DHDN_Y')

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


# cfastrunoff
wqm_cfast <- raster::stack('./data/WQM_01_18.nc', varname = 'cfastrunoff')
plot(wqm_cfast[[1]])
wqm_cfast <- setMinMax(wqm_cfast)
minValue(wqm_cfast)
maxValue(wqm_cfast)


## subset cfast in 2001-2008 periods

wqm_cfast_2001_08 <-
  wqm_cfast[[which(date(getZ(wqm_cfast))  >= as.Date("2001-01-01") &
                     date(getZ(wqm_cfast)) <= as.Date("2008-12-31"))]]

hist(wqm_cfast_2001_08)
# Apply a median on each season of a cfast
wqm_cfast_2001_08_md <-
  stackApply(wqm_cfast_2001_08,
             indices = groups(dates[1:96]),
             median,
             na.rm = T)

hist(wqm_cfast_2001_08_md)
#
levelplot(wqm_cfast_2001_08_md)+
  layer(sp.points(bode_qstation_Sel,pch = 21,col='white'))
# # subset cfast in 2011-2018 periods
wqm_cfast_2011_18 <-
  wqm_cfast[[which(date(getZ(wqm_cfast))  >= as.Date("2011-01-01") &
                     date(getZ(wqm_cfast)) <= as.Date("2018-12-31"))]]


# Apply a function on subsets of a RasterStack
wqm_cfast_2011_18_md <-
  stackApply(wqm_cfast_2011_18,
             indices = groups(dates[121:216]),
             median,
             na.rm = T)

hist(wqm_cfast_2011_18_md)

hist(wqm_cfast_2011_18_md-wqm_cfast_2001_08_md)

# fast runoff different in two periods------
wqm_cfast_diff <- wqm_cfast_2011_18_md-wqm_cfast_2001_08_md
hist(wqm_cfast_diff)

maxValue(wqm_cfast_diff)
minValue(wqm_cfast_diff)

# reclassify
fast_rcl <- matrix(c(-30, -20, -20.5,
                 20, 400, 20.5),
               byrow = TRUE, ncol = 3)

wqm_cfast_diff_rcl <- reclassify(wqm_cfast_diff, fast_rcl)
hist(wqm_cfast_diff_rcl)

#
cfast.at = c(-21,-20,-15,-10, -5,  0,  5, 10, 15, 20,21)
cfast.colbrk = seq(-20,20,by=5)
cfast.colkey  = list(space = "bottom",
            at = cfast.at,
            lables = list(at=cfast.colbrk, lables=c("\u2264 -20",-15, -10, -5, 0, 5, 10, 15, "\u2265 20")) )
cfast.coltheme <- rasterTheme(region = rev(brewer.pal(10, "RdYlBu")) )

levelplot(wqm_cfast_diff_rcl,
          at = cfast.at,
          par.setting=cfast.coltheme,
          colorkey=cfast.colkey,
          main='The difference of median seasonal fast runoff nitrate concentration [mg/L] \n in 2011-2018 compared to 2001-2008',
          names.attr = c('Winter', 'Spring', 'Summer', 'Autumn'),
          scales = list(draw = FALSE)
          )+
  layer(sp.points(bode_qstation_Sel,pch = 21,col='black'))



## nitrate in slow Runoff----
wqm_cslow <- raster::stack('./data/WQM_01_18.nc', varname = 'cslowrunoff')

wqm_cslow <- setMinMax(wqm_cslow)
minValue(wqm_cslow)
maxValue(wqm_cslow)

## subset cslow in 2001-2008 periods
wqm_cslow_2001_08 <-
  wqm_cslow[[which(date(getZ(wqm_cslow))  >= as.Date("2001-01-01") &
                     date(getZ(wqm_cslow)) <= as.Date("2008-12-31"))]]

hist(wqm_cslow_2001_08)
# Apply a median on each season of a cslow
wqm_cslow_2001_08_md <-
  stackApply(wqm_cslow_2001_08,
             indices = groups(dates[1:96]),
             median,
             na.rm = T)

hist(wqm_cslow_2001_08_md)
levelplot(wqm_cslow_2001_08_md,
          main='The nitrate concentration of slow runoff [mg/L] in 2001-2008',
          names.attr = c('Winter', 'Spring', 'Summer', 'Autumn'),
          scales = list(draw = FALSE))

# # subset cslow in 2011-2018 periods
wqm_cslow_2011_18 <-
  wqm_cslow[[which(date(getZ(wqm_cslow))  >= as.Date("2011-01-01") &
                     date(getZ(wqm_cslow)) <= as.Date("2018-12-31"))]]


# Apply a function on subsets of a RasterStack
wqm_cslow_2011_18_md <-
  stackApply(wqm_cslow_2011_18,
             indices = groups(dates[121:216]),
             median,
             na.rm = T)

hist(wqm_cslow_2011_18_md)
levelplot(wqm_cslow_2011_18_md,
          main='The nitrate concentration of slow runoff [mg/L] in 2001-2008',
          names.attr = c('Winter', 'Spring', 'Summer', 'Autumn'),
          scales = list(draw = FALSE))

# slow runoff concentration diff-----

wqm_cslow_diff <- wqm_cslow_2011_18_md-wqm_cslow_2001_08_md
hist(wqm_cslow_diff)

maxValue(wqm_cslow_diff)

#
cslow_rcl <- matrix(c(-10, -5, -5.5,
                  5, 51, 5.5),
                byrow = TRUE, ncol = 3)

wqm_cslow_diff_rcl <- reclassify(wqm_cslow_diff,cslow_rcl)
#
cslow.at=c(-3.5,-3,-2,-1,0,1,2,3,4,5,6)
cslow.colbrk=seq(-3,5,by=1)
cslow.colkey=list(space = "bottom",
            at=cslow.at,
            lables=list(at=cslow.colbrk, lables=c("\u2264 -3", -2, -1, 0, 1, 2,3,4, "\u2265 5")) )
# col scheme
coltheme <- rasterTheme(region =rev(brewer.pal(10, "RdYlBu")) )


 levelplot(wqm_cslow_diff_rcl,
           par.setting=coltheme,
           at = cslow.at,
           colorkey = cslow.colkey,
           main='The difference of median seasonal slow runoff nitrate concentration [mg/L] \n in 2011-2018 compared to 2001-2008',
           names.attr = c('Winter', 'Spring', 'Summer', 'Autumn'),
           scales = list(draw = FALSE))+
   layer(sp.points(bode_qstation_Sel,pch = 21,col='black'))


## nitrate in base Runoff----
wqm_cbasefow <- raster::stack('./data/WQM_01_18.nc', varname = 'cbasefow')

wqm_cbasefow <- setMinMax(wqm_cbasefow)
minValue(wqm_cbasefow)
maxValue(wqm_cbasefow)

#
## subset cbasefow in 2001-2008 periods
wqm_cbasefow_2001_08 <-
  wqm_cbasefow[[which(date(getZ(wqm_cbasefow))  >= as.Date("2001-01-01") &
                     date(getZ(wqm_cbasefow)) <= as.Date("2008-12-31"))]]

hist(wqm_cbasefow_2001_08)
# Apply a median on each season of a cbasefow
wqm_cbasefow_2001_08_md <-
  stackApply(wqm_cbasefow_2001_08,
             indices = groups(dates[1:96]),
             median,
             na.rm = T)

hist(wqm_cbasefow_2001_08_md)
levelplot(wqm_cbasefow_2001_08_md,
          main='The nitrate concentration of slow runoff [mg/L] in 2001-2008',
          names.attr = c('Winter', 'Spring', 'Summer', 'Autumn'),
          scales = list(draw = FALSE))

# # subset cbasefow in 2011-2018 periods
wqm_cbasefow_2011_18 <-
  wqm_cbasefow[[which(date(getZ(wqm_cbasefow))  >= as.Date("2011-01-01") &
                     date(getZ(wqm_cbasefow)) <= as.Date("2018-12-31"))]]


# Apply a function on subsets of a RasterStack
wqm_cbasefow_2011_18_md <-
  stackApply(wqm_cbasefow_2011_18,
             indices = groups(dates[121:216]),
             median,
             na.rm = T)

hist(wqm_cbasefow_2011_18_md)
levelplot(wqm_cbasefow_2011_18_md,
          main='The nitrate concentration of base flow [mg/L] in 2001-2008',
          names.attr = c('Winter', 'Spring', 'Summer', 'Autumn'),
          scales = list(draw = FALSE))
# cbase flow difference-----
wqm_cbasefow_diff <- wqm_cbasefow_2011_18_md-wqm_cbasefow_2001_08_md

hist(wqm_cbasefow_diff)
maxValue(wqm_cbasefow_diff)
minValue(wqm_cbasefow_diff)

#
cbase <- matrix(c(-7, -3, -3.5,
         2, 8, 2.5),
       byrow = TRUE, ncol = 3)

wqm_cbasefow_diff_rcl <- reclassify(wqm_cbasefow_diff,cbase)
#
cbase.at= c(-3.5, -3, -2, -1, 0, 1, 2, 2.5)
cbase.colkey=list(space = "bottom",
            at=cbase.at,
            lables=list(at=seq(-3,2,by=1), lables=c("\u2264 -3", -2, -1, 0, 1, "\u2265 2")) )
# col scheme
cbase.coltheme <- rasterTheme(region =rev(brewer.pal(7, "RdYlBu")) )


levelplot(wqm_cbasefow_diff_rcl,
          par.setting=cbase.coltheme,
          at = cbase.at,
          colorkey = cbase.colkey,
          main='The difference of median seasonal base flow nitrate concentration [mg/L] \n in 2011-2018 compared to 2001-2008',
          names.attr = c('Winter', 'Spring', 'Summer', 'Autumn'),
          scales = list(draw = FALSE))+
  layer(sp.points(bode_qstation_Sel,pch = 21,col='black'))

## nitrate in total Runoff----
wqm_ctotalrunoff <- raster::stack('./data/WQM_01_18.nc', varname = 'ctotalrunoff')

wqm_ctotalrunoff <- setMinMax(wqm_ctotalrunoff)
minValue(wqm_ctotalrunoff)
maxValue(wqm_ctotalrunoff)

## subset ctotalrunoff in 2001-2008 periods
wqm_ctotalrunoff_2001_08 <-
  wqm_ctotalrunoff[[which(date(getZ(wqm_ctotalrunoff))  >= as.Date("2001-01-01") &
                        date(getZ(wqm_ctotalrunoff)) <= as.Date("2008-12-31"))]]

hist(wqm_ctotalrunoff_2001_08)
# Apply a median on each season of a ctotalrunoff
wqm_ctotalrunoff_2001_08_md <-
  stackApply(wqm_ctotalrunoff_2001_08,
             indices = groups(dates[1:96]),
             median,
             na.rm = T)

hist(wqm_ctotalrunoff_2001_08_md)
levelplot(wqm_ctotalrunoff_2001_08_md,
          main='The nitrate concentration of total runoff [mg/L] in 2001-2008',
          names.attr = c('Winter', 'Spring', 'Summer', 'Autumn'),
          scales = list(draw = FALSE))

# # subset ctotalrunoff in 2011-2018 periods
wqm_ctotalrunoff_2011_18 <-
  wqm_ctotalrunoff[[which(date(getZ(wqm_ctotalrunoff))  >= as.Date("2011-01-01") &
                        date(getZ(wqm_ctotalrunoff)) <= as.Date("2018-12-31"))]]


# Apply a function on subsets of a RasterStack
wqm_ctotalrunoff_2011_18_md <-
  stackApply(wqm_ctotalrunoff_2011_18,
             indices = groups(dates[121:216]),
             median,
             na.rm = T)

hist(wqm_ctotalrunoff_2011_18_md)
levelplot(wqm_ctotalrunoff_2011_18_md,
          main='The nitrate concentration of base flow [mg/L] in 2001-2008',
          names.attr = c('Winter', 'Spring', 'Summer', 'Autumn'),
          scales = list(draw = FALSE))

# total runoff nitrate concentration different between two periods---------
wqm_ctotalrunoff_diff <- wqm_ctotalrunoff_2011_18_md-wqm_ctotalrunoff_2001_08_md

hist(wqm_ctotalrunoff_diff)
# https://stackoverflow.com/questions/34835669/use-a-single-colour-for-range-of-values-and-include-inequality-symbol-on-colorke
## reclassify data to  set all values smaller than -20 (or larger than 20) e.g. to -20.5 (or 20.5)
#  not interested in smaller or larger values
ctotal_rcl <- matrix(c(-10, -3, -3.5,
                  3, 10, 3.5),
                byrow = TRUE, ncol = 3)

wqm_ctotalrunoff_diff_rcl <- reclassify(wqm_ctotalrunoff_diff, ctotal_rcl)
hist(wqm_ctotalrunoff_diff_rcl)

#
ctotal.at=c(-3.5,-3,-2,-1,0,1,2,3,3.5)
ctotal.colbrk=seq(-3,3,by=1)
ctotal.colkey=list(space = "bottom",
            at=ctotal.at,
            title=
            lables=list(at=ctotal.colbrk, lables=c("\u2264 -3", -2, -1, 0, 1, 2, "\u2265 3")) )
# col scheme
ctotal.coltheme <- rasterTheme(region =rev(brewer.pal(10, "RdYlBu")) )

levelplot(wqm_ctotalrunoff_diff_rcl,
          par.setting=ctotal.coltheme,
          at = ctotal.at,
          colorkey = ctotal.colkey,
          main='The difference of median seasonal total runoff nitrate concentration [mg/L] \n in 2011-2018 compared to 2001-2008',
          names.attr = c('Winter', 'Spring', 'Summer', 'Autumn'),
          scales = list(draw = FALSE))+
    layer(sp.points(bode_qstation_Sel,pch = 21,col='black'))


# instream uptake----
#
waterassimi<- raster::stack('./data/WQM_01_18.nc', varname = 'waterassimi')

waterassimi <- setMinMax(waterassimi)
# getZ(waterassimi)
# select 2001-2008 period
waterassimi_2001_08 <- waterassimi[[which(date(getZ(waterassimi))  >= as.Date("2001-01-01") &
          date(getZ(waterassimi)) <= as.Date("2008-12-31"))]]

levelplot(waterassimi_2001_08[[12]],margin=F)

#
waterassimi_2001_08_md <- stackApply(waterassimi_2001_08,
           indices = groups(dates[1:96]),
           median,
           na.rm = T)

hist(waterassimi_2001_08_md)
# select 2011-2018 period
waterassimi_2011_18 <- waterassimi[[which(date(getZ(waterassimi))  >= as.Date("2011-01-01") &
          date(getZ(waterassimi)) <= as.Date("2018-12-31"))]]

#
waterassimi_2011_18_md <- stackApply(waterassimi_2011_18,
           indices = groups(dates[121:216]),
           median,
           na.rm = T)

hist(waterassimi_2011_18_md)

# in stream uptake different in two periods-----
waterassimi_diff <- waterassimi_2011_18_md-waterassimi_2001_08_md
hist(waterassimi_diff)
minValue(waterassimi_diff)
maxValue(waterassimi_diff)
#
wassi_rcl <- matrix(c(-30, -5, -5.5,
                      5, 48, 5.5),
                    byrow = TRUE, ncol = 3)
waterassimi_diff_rcl <- reclassify(waterassimi_diff,wassi_rcl)
#
wassi.at = c(-5.5, -5, -4, -3, -2, -1, 0, 1,  2,  3,  4,  5, 5.5)
wassi.colbrk = seq(-5, 5, by = 1)
wassi.colkey = list(space='bottom',
                    at=wassi.at,
                    labels=list(at=wassi.colbrk,
                                labels=c("\u2264 -5",-4, -3, -2, -1, 0, 1, 2, 3, 4, "\u2265 5" )))

wassi.theme <- rasterTheme(region =rev(brewer.pal(12, "RdYlBu")) )

levelplot(
  waterassimi_diff_rcl,
  at = wassi.at,
  par.setting = wassi.theme,
  colorkey = wassi.colkey,
  main = 'The difference of median seasonal instream uptake [mg/m2/d] \n in 2011-2018 compared to 2001-2008',
  names.attr = c('Winter', 'Spring', 'Summer', 'Autumn'),
  scales = list(draw = FALSE)
) +
  layer(sp.points(bode_qstation_Sel, pch = 21, col = 'black'))

# instream denitri-------
waterdenitri <- raster::stack('./data/WQM_01_18.nc', varname = 'waterdenitri')

waterdenitri <- setMinMax(waterdenitri)

# select 2001-2008 period
waterdenitri_2001_08 <- waterdenitri[[which(date(getZ(waterdenitri))  >= as.Date("2001-01-01") &
                                            date(getZ(waterdenitri)) <= as.Date("2008-12-31"))]]
#
waterdenitri_2001_08_md <- stackApply(waterdenitri_2001_08,
                                     indices = groups(dates[1:96]),
                                     median,
                                     na.rm = T)
hist(waterdenitri_2001_08_md)
# select 2011-2018 period
waterdenitri_2011_18 <- waterdenitri[[which(date(getZ(waterdenitri))  >= as.Date("2011-01-01") &
                                            date(getZ(waterdenitri)) <= as.Date("2018-12-31"))]]

#
waterdenitri_2011_18_md <- stackApply(waterdenitri_2011_18,
                                     indices = groups(dates[121:216]),
                                     median,
                                     na.rm = T)
hist(waterdenitri_2011_18_md)

# instream denitri different in two periods----
waterdenitri_diff <- waterdenitri_2011_18_md-waterdenitri_2001_08_md
hist(waterdenitri_diff)

minValue(waterdenitri_diff)
maxValue(waterdenitri_diff)

#
wdentri_rcl = matrix(c(-21, -3, -3.5,
                       3, 8, 3.5),
                     byrow = TRUE, ncol = 3)

waterdenitri_diff_rcl <- reclassify(waterdenitri_diff,wdentri_rcl)

#
#
wdenitri.at = c(-3.5, -3, -2, -1, 0, 1,  2,  3, 3.5)
wdenitri.colbrk = seq(-3, 3, by = 1)
wdenitri.colkey = list(space='bottom',
                    at=wdenitri.at,
                    labels=list(at=wdenitri.colbrk,
                                labels=c("\u2264 -3",-2, -1, 0, 1, 2,  "\u2265 3" )))

wdenitri.theme <- rasterTheme(region =rev(brewer.pal(8, "RdYlBu")) )

levelplot(waterdenitri_diff_rcl,
          at = wdenitri.at,
          par.setting = wdenitri.theme,
          colorkey = wdenitri.colkey,
          main = 'The difference of median seasonal instream denitrification [mg/m2/d] \n in 2011-2018 compared to 2001-2008',
          names.attr = c('Winter', 'Spring', 'Summer', 'Autumn'),
          scales = list(draw = FALSE)
) +
  layer(sp.points(bode_qstation_Sel, pch = 21, col = 'black'))
