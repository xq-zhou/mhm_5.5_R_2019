# station time series trend analysis


library(hydroTSM)
library(modifiedmk)
library(formattable)



# concentration trend  ----------------------------------------------------

Conc_out <-  read_table("./data/daily_concentration_cali2018.out", col_names = TRUE)
# combine mutiple columns to one date
Conc_cali <-
  Conc_out %>% unite('dates', c('Year', 'Mon', 'Day')) %>% mutate(dates =
                                                                    ymd(dates))

Conc_cali[Conc_cali == -9999] <- NA

# convert to xts
Conc_xts <- xts(Conc_cali[, 3:16], order.by = Conc_cali$dates)


#convert daily time series to monthly
Conc_xts_mon <- daily2monthly(Conc_xts,mean,na.rm=T)

plot(Conc_xts_mon )
# all 12 monthly data trend (Seasonal Kendall trend test)-----

#
meis_mon <- ts( coredata(Conc_xts_mon$INConcob0579610), start = c(2010,1),frequency=12)
month_mk_meis <- SeasonalMannKendall(meis_mon)
summary(month_mk_meis)


#
haus_mon <- ts( coredata(Conc_xts_mon$INConcob0579620), start = c(2010,1),frequency=12)
month_mk_haus <- SeasonalMannKendall(haus_mon)
summary(month_mk_haus)

#
wege_mon <- ts( coredata(Conc_xts_mon$INConcob0579049), start = c(2010,1),frequency=12)
month_mk_wege <- SeasonalMannKendall(wege_mon)
summary(month_mk_wege)

#
Hadmers_mon <- ts( coredata(Conc_xts_mon$INConcob0579070), start = c(2010,1),frequency=12)
month_mk_Hadmers <- SeasonalMannKendall(Hadmers_mon)
summary(month_mk_Hadmers)


#
stassfurt_mon <- ts( coredata(Conc_xts_mon$INConcob0579085), start = c(2010,1),frequency=12)
month_mk_stassfurt <- SeasonalMannKendall(stassfurt_mon)
summary(month_mk_stassfurt)

# create table

mk_table <- data.frame(tau= data.frame(month_mk_meis$tau,
                                       month_mk_haus$tau,
                                       month_mk_wege$tau,
                                       month_mk_Hadmers$tau,
                                       month_mk_stassfurt$tau),
                  sl= data.frame(month_mk_meis$sl,
                                 month_mk_haus$sl,
                                 month_mk_wege$sl,
                                 month_mk_Hadmers$sl,
                                 month_mk_stassfurt$sl) )
formattable(mk_table)

# seasonal trend-----
# winter seasonal
Conc_xts_winter <- dm2seasonal(Conc_xts, FUN=mean, season="DJF",na.rm=T)
plot(Conc_xts_winter )

Conc_xts_winter_ts<- ts( coredata(Conc_xts_winter),start = c(2010,1),frequency=1)
print(Conc_xts_winter_ts)
meis_winter <-  MannKendall(Conc_xts_winter_ts[,1])
haus_winter <- MannKendall(Conc_xts_winter_ts[,3])
wege_winter <- MannKendall(Conc_xts_winter_ts[,5])
had_winter <- MannKendall(Conc_xts_winter_ts[,9])
stass_winter <- MannKendall(Conc_xts_winter_ts[,11])

# spring
Conc_xts_spring <- dm2seasonal(Conc_xts, FUN=mean, season="MAM",na.rm=T)
plot(Conc_xts_spring )

Conc_xts_spring_ts<- ts( coredata(Conc_xts_spring),start = c(2010,1),frequency=1)
print(Conc_xts_spring_ts)
meis_spring <-  MannKendall(Conc_xts_spring_ts[,1])
haus_spring <- MannKendall(Conc_xts_spring_ts[,3])
# wege_spring <- MannKendall(Conc_xts_spring_ts[,5])
had_spring <- MannKendall(Conc_xts_spring_ts[,9])
stass_spring <- MannKendall(Conc_xts_spring_ts[,11])

# summer
Conc_xts_summer <- dm2seasonal(Conc_xts, FUN=mean, season="JJA",na.rm=T)
plot(Conc_xts_summer )

Conc_xts_summer_ts<- ts( coredata(Conc_xts_summer),start = c(2010,1),frequency=1)
print(Conc_xts_summer_ts)
meis_summer <-  MannKendall(Conc_xts_summer_ts[,1])
haus_summer <- MannKendall(Conc_xts_summer_ts[,3])
# wege_summer <- MannKendall(Conc_xts_summer_ts[,5])
had_summer <- MannKendall(Conc_xts_summer_ts[,9])
stass_summer <- MannKendall(Conc_xts_summer_ts[,11])

# autumn
Conc_xts_autumn <- dm2seasonal(Conc_xts, FUN=mean, season="SON",na.rm=T)
plot(Conc_xts_autumn )

Conc_xts_autumn_ts<- ts( coredata(Conc_xts_autumn),start = c(2010,1),frequency=1)
print(Conc_xts_autumn_ts)
meis_autumn <-  MannKendall(Conc_xts_autumn_ts[,1])
haus_autumn <- MannKendall(Conc_xts_autumn_ts[,3])
# wege_autumn <- MannKendall(Conc_xts_autumn_ts[,5])
had_autumn <- MannKendall(Conc_xts_autumn_ts[,9])
stass_autumn <- MannKendall(Conc_xts_autumn_ts[,11])



# Discharge ---------------------------------------------------------------

# Discharge

Q_meis <- read_table2('D:/BODE_data/Alex/579610.txt',skip = 5)
colnames(Q_meis) <- c('Year','Mon', 'Day','HH','MM','Q')
Q_meis <-  Q_meis %>%
  unite('dates', c('Year', 'Mon', 'Day')) %>% mutate(dates = ymd(dates))
Q_meis[Q_meis == -9999] <- NA

# create xts time series
Q_meis_xts <- xts(Q_meis$Q, order.by = Q_meis$dates)

#convert daily time series to monthly
Q_meis_xts_mon <- daily2monthly(Q_meis_xts,median,na.rm=T)

plot(Q_meis_xts_mon )
# monthly data trend -----

#
Q_mon  <- data.frame( Q_mon=coredata(Q_meis_xts_mon), date=index(Q_meis_xts_mon))

t <- Q_mon %>% filter(between(date,as.Date('2001-01-01'),as.Date('2008-12-31'))) %>%
  group_by(month(date)) %>% group_modify(~ts(start =c(2010,1),frequency=1))
