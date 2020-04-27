# meis haus runoff component

library(tidyverse)
library(lubridate)
library(xts)

# station nitrate in runoff component
Nsim <- read_table2('./data/Nsimulted_meis_haus.txt')
Nsim <- Nsim %>% select(-X25)

# station discharge in runoff component
Qsim <- read_table2('./data/Qsimulted_meis_haus.txt')
Qsim <- Qsim %>% select(-X25)

Date <- seq(as.Date('2004-01-03'),as.Date('2018-12-31'),by='day')
#
Qsim_daily <-  Qsim %>% mutate(Date=Date,Year=year(Date),mon=month(Date))


Qsim_month <- Qsim_daily %>% group_by(Year,mon) %>% summarise_all(median)


#
Q_base_sim_month <- Qsim_month %>% select(contains('baseflow'))
