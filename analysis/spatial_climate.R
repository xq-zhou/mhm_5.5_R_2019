# precipitation and temperature in 2015-2018 compared to 2001-2014

library(tidyverse)
library(ncdf4)
library(raster)
library(rasterVis)

# rm(list = ls())
#
pre <- nc_open('./data/pre_Y_own_2000_2019.nc')
print(pre)
precip <- ncvar_get(pre,'pre')
nc_close(pre)
#
dates <-
  seq(as.Date("2000-01-01"), as.Date("2019-12-31"), by = "day")


# seasonal
months <- format(dates,'%Y-%m')

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
pre <- raster::stack('./data/pre_Y_own_2000_2019.nc',varname ='pre')

plot(pre[[1]])
# invert y axis
pre <- flip(pre,'y')
pre <- setZ(pre,dates)
t <- getZ(pre)
# subsest period 2001-2014
pre_2001_14 <- pre[[which( getZ(pre)  >= as.Date("2001-01-01") &
                 (getZ(pre) <= as.Date("2014-12-31") ) )]]


# seasonal precipiation of the whole period

pre_2001_14_sum <- stackApply(pre_2001_14,
           indices = groups(dates[367:5479]),
           sum,
           na.rm = T)
# yearly mean seasonal precipiation
pre_2001_14_yseas <- pre_2001_14_sum/14

levelplot(pre_2001_14_yseas)
hist(pre_2001_14_yseas)


# subsest period 2015-2018
pre_2015_18 <- pre[[which( getZ(pre)  >= as.Date("2015-01-01") &
                             (getZ(pre) <= as.Date("2018-12-31") ) )]]

#
pre_2015_18_sum <- stackApply(pre_2015_18,
                   indices = groups(dates[5480:6940]),
                   sum,
                   na.rm = T)
#
pre_2015_18_yseas <- pre_2015_18_sum/4

levelplot(pre_2015_18_yseas)

# yearly seasonal precipitation difference in 2015-2018 compared to 2001-2014
pre_diff <- pre_2015_18_yseas-pre_2001_14_yseas


hist(pre_diff)
#

col.at=c()
levelplot(pre_diff)
