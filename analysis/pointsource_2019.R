# point source data 2019

library(tidyverse)
library(lubridate)
library(purrr)

#
fpath <- list.files('D:/BODE_data/Water quality data/pointsource_bode/Conc_correct',full.name=T)

# Concentration -----------------------------------------------------------


dat <- fpath %>%  map(~read_table2(.,skip = 4) )
names(dat) <- list.files('D:/BODE_data/Water quality data/pointsource_bode/Conc_correct',full.name=F)

dat_p <- map(dat,~ return(.x[1:5479,]))
# crop data from 2010-2013 to 2016-2019

sel <- function(x){
    x %>% filter(YYYY==2010 | YYYY==2011 | YYYY==2012 | YYYY==2013 | YYYY==2014) %>%
    select(TN) %>%
    mutate(Date=seq(as.Date('2015-01-01'),as.Date('2019-12-31'),by='day'),
           YYYY=year(Date),
           MM=month(Date),
           DD=day(Date),
           HH=hour(Date),
           NN=minute(Date)
           ) %>%
    select(-Date,-TN,TN)
}


dat_sel <- dat_p %>%  map( ~sel(.))

# append list of select data to old list

dat_l <- mapply(rbind,dat_p,dat_sel,SIMPLIFY=FALSE)


#### create function to write out data to seperate text files
wt<- function(dataset,col_name ){

  #   # add lines to a text file
  lines<- 'nodata	-9999
n	1	measurements	per	day	[1,	1440]
start	2000	1	1	0	0	(YYYY	MM	DD	HH	MM)
end	2019	12	31	0	0	(YYYY	MM	DD	HH	MM)
YYYY	MM	DD	HH	NN	TN'
  #
  write(lines, col_name )

  #
  # #write out the filled time series
  write.table(dataset, col_name ,sep="   ",row.names = F,col.names = F,append = T)
}

for (nm in names(dat_l) ){
  wt(dat_l[[nm]],nm)
}

#

# Discharge ---------------------------------------------------------------

fpath <- list.files('D:/BODE_data/Water quality data/pointsource_bode/Discharge',full.name=T)

dat <- fpath %>%  map(~read_table2(.,skip = 4) )
names(dat) <- list.files('D:/BODE_data/Water quality data/pointsource_bode/Discharge',full.name=F)
#
dat_p <- map(dat,~ return(.x[1:5479,1:6])) %>% map(set_names,c('YYYY','MM','DD','HH','NN','Q'))

# crop data from 2010-2013 to 2016-2019

sel <- function(x){
  x %>% filter(YYYY==2010 | YYYY==2011 | YYYY==2012 | YYYY==2013 | YYYY==2014) %>%
    select('Q') %>%
    mutate(Date=seq(as.Date('2015-01-01'),as.Date('2019-12-31'),by='day'),
           YYYY=year(Date),
           MM=month(Date),
           DD=day(Date),
           HH=hour(Date),
           NN=minute(Date)
    ) %>%
    select(-Date,-Q,Q)
}


dat_sel <- dat_p %>%  map( ~sel(.))

# append list of select data to old list

dat_l <- mapply(rbind,dat_p,dat_sel,SIMPLIFY=FALSE)


#### create function to write out data to seperate text files
wt<- function(dataset,col_name ){

  #   # add lines to a text file
  lines<- 'nodata	-9999
n	1	measurements	per	day	[1,	1440]
start	2000	1	1	0	0	(YYYY	MM	DD	HH	MM)
end	2019	12	31	0	0	(YYYY	MM	DD	HH	MM)'
  #
  write(lines, col_name )

  #
  # #write out the filled time series
  write.table(dataset, col_name ,sep="   ",row.names = F,col.names = F,append = T)
}

for (nm in names(dat_l) ){
  wt(dat_l[[nm]],nm)
}

