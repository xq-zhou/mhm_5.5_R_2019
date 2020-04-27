# calibration and validation results

library(tidyverse)
library(lubridate)
library(hydroGOF)
library(xts)
 # # adding the equation of a fitted regression line on a scatter plot
library(ggpmisc)

rm(list = ls())



# calibration results -----------------------------------------------------

# Discharge

Q <- read_table2('./data/Qsimulted_station_01_18.txt')
Q_cali <-
  unite(Q, 'dates', c('Year', 'Mon', 'Day')) %>%
  mutate(dates = ymd(dates)) %>%
  filter(between(dates,as.Date('2010-01-01'),as.Date('2015-12-31') )  )
Q_cali[Q_cali == -9999] <- NA
# create xts time series
Q_xts <- xts(Q_cali[, 3:16], order.by = Q_cali$dates)


Q_mon <- apply.monthly(Q_xts,mean)

# Nash-Sutcliffe efficiency between sim and obs
NSE_Q_Cali <- data.frame(NSE(Q_xts$Qsim_0000579610, Q_xts$Qobs_0000579610),
                         NSE(Q_xts$Qsim_0000579620, Q_xts$Qobs_0000579620),
                         NSE(Q_xts$Qsim_0000579049, Q_xts$Qobs_0000579049),
                         NSE(Q_xts$Qsim_0000579745, Q_xts$Qobs_0000579745),
                         NSE(Q_xts$Qsim_0000579810, Q_xts$Qobs_0000579810),
                         NSE(Q_xts$Qsim_0000579070, Q_xts$Qobs_0000579070),
                         NSE(Q_xts$Qsim_0000579085, Q_xts$Qobs_0000579085)  )
# KGE
KGE(Q_xts$Qsim_0000579610, Q_xts$Qobs_0000579610)
KGE(Q_xts$Qsim_0000579620, Q_xts$Qobs_0000579620)
KGE(Q_xts$Qsim_0000579049, Q_xts$Qobs_0000579049)
KGE(Q_xts$Qsim_0000579745, Q_xts$Qobs_0000579745)
KGE(Q_xts$Qsim_0000579810, Q_xts$Qobs_0000579810)
KGE(Q_xts$Qsim_0000579070, Q_xts$Qobs_0000579070)
KGE(Q_xts$Qsim_0000579085, Q_xts$Qobs_0000579085)




# pBias

pbias(Q_xts$Qsim_0000579610, Q_xts$Qobs_0000579610)
pbias(Q_xts$Qsim_0000579620, Q_xts$Qobs_0000579620)
pbias(Q_xts$Qsim_0000579049, Q_xts$Qobs_0000579049)
pbias(Q_xts$Qsim_0000579745, Q_xts$Qobs_0000579745)
pbias(Q_xts$Qsim_0000579810, Q_xts$Qobs_0000579810)
pbias(Q_xts$Qsim_0000579070, Q_xts$Qobs_0000579070)
pbias(Q_xts$Qsim_0000579085, Q_xts$Qobs_0000579085)

# plot
Q_plot <- function(data, xvar, yvar1, yvar2) {
  xvar <- enquo(xvar)
  yvar1 <- enquo(yvar1)
  yvar2 <- enquo(yvar2)
  ggfun <- ggplot(data, aes(x = !!xvar)) +
    geom_point(aes(y = !!yvar1, color = 'blue')) +
    geom_line(aes(y = !!yvar2, color = 'red')) +
    geom_vline(
      xintercept = as.Date('2010-01-01'),
      color = 'black',
      linetype = "dashed"
    ) +
    scale_x_date(
      date_labels = '%b-%Y',
      date_breaks = '1 year',
      expand = c(0, 0)
    ) +
    scale_y_continuous(expand = c(0, 0.1)) +
    ylab(expression(" Q (m3/s)")) +   #
    scale_color_manual(
      name = "",
      values = c('blue', 'red'),
      labels = c('Measured', 'Simulated'),
      guide = guide_legend(override.aes = list(
        linetype = c('blank', 'solid'),
        shape = c(16, NA)
      ))
    ) +
    theme_bw() +
    theme(
      axis.text.x = element_text(
        size = 10,
        hjust = 0.5,
        colour = 'black'
      ),
      axis.text.y = element_text(size = 20, colour = 'black'),
      axis.title.y = element_text(size = 20, colour = 'black'),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      legend.position = c(0.95, 0.95),
      legend.direction = "horizontal",
      legend.justification = c('right', 'top'),
      legend.text = element_text(size = 20, colour = 'black')
    )

  return(ggfun)
}

Q_plot(Q_cali, dates, Qobs_0000579610, Qsim_0000579610)
ggsave('./images/Qcali_NewPre_579610.jpeg', dpi = 600)

Q_plot(Q_cali, dates, Qobs_0000579620, Qsim_0000579620)
ggsave('./images/Qcali_NewPre_579620.jpeg', dpi = 600)

Q_plot(Q_cali, dates, Qobs_0000579049, Qsim_0000579049)
ggsave('./images/Qcali_NewPre_579049.jpeg', dpi = 600)

Q_plot(Q_cali, dates, Qobs_0000579745, Qsim_0000579745)
ggsave('./images/Qcali_NewPre_579745.jpeg', dpi = 600)

Q_plot(Q_cali, dates, Qobs_0000579810, Qsim_0000579810)
ggsave('./images/Qcali_NewPre_579810.jpeg', dpi = 600)

Q_plot(Q_cali, dates, Qobs_0000579070, Qsim_0000579070)
ggsave('./images/Qcali_NewPre_579070.jpeg', dpi = 600)

Q_plot(Q_cali, dates, Qobs_0000579085, Qsim_0000579085)
ggsave('./images/Qcali_NewPre_579085.jpeg', dpi = 600)


# Concentration -----
Conc_out <-  read_table("./data/daily_concentration_cali2018.out", col_names = TRUE)
# combine mutiple columns to one date
Conc_cali <-
  Conc_out %>% unite('dates', c('Year', 'Mon', 'Day')) %>%
  mutate(dates = ymd(dates)) %>%
  filter(between(dates,as.Date('2010-01-01'),as.Date('2015-12-31') )  )

Conc_cali[Conc_cali == -9999] <- NA

#
Conc_xts <- xts(Conc_cali[, 3:16], order.by = Conc_cali$dates)

# NSE
NSE(Conc_xts$INConcsi0579610, Conc_xts$INConcob0579610)
NSE(Conc_xts$INConcsi0579620, Conc_xts$INConcob0579620)
NSE(Conc_xts$INConcsi0579049, Conc_xts$INConcob0579049)
NSE(Conc_xts$INConcsi0579745, Conc_xts$INConcob0579745)
NSE(Conc_xts$INConcsi0579810, Conc_xts$INConcob0579810)
NSE(Conc_xts$INConcsi0579070, Conc_xts$INConcob0579070)
NSE(Conc_xts$INConcsi0579085, Conc_xts$INConcob0579085)


# KGE
KGE(Conc_xts$INConcsi0579610, Conc_xts$INConcob0579610)
KGE(Conc_xts$INConcsi0579620, Conc_xts$INConcob0579620)
KGE(Conc_xts$INConcsi0579049, Conc_xts$INConcob0579049)
KGE(Conc_xts$INConcsi0579745, Conc_xts$INConcob0579745)
KGE(Conc_xts$INConcsi0579810, Conc_xts$INConcob0579810)
KGE(Conc_xts$INConcsi0579070, Conc_xts$INConcob0579070)
KGE(Conc_xts$INConcsi0579085, Conc_xts$INConcob0579085)

#
pbias(Conc_xts$INConcsi0579610, Conc_xts$INConcob0579610)
pbias(Conc_xts$INConcsi0579620, Conc_xts$INConcob0579620)
pbias(Conc_xts$INConcsi0579049, Conc_xts$INConcob0579049)
pbias(Conc_xts$INConcsi0579745, Conc_xts$INConcob0579745)
pbias(Conc_xts$INConcsi0579810, Conc_xts$INConcob0579810)
pbias(Conc_xts$INConcsi0579070, Conc_xts$INConcob0579070)
pbias(Conc_xts$INConcsi0579085, Conc_xts$INConcob0579085)


## adding the equation of a fitted regression line on a scatter plot

Conc_plot <- function(data, xvar, yvar1, yvar2) {
  xvar <- enquo(xvar)
  yvar1 <- enquo(yvar1)  #obs
  yvar2 <- enquo(yvar2)  #sim
  ggfun <- ggplot(data, aes(x = !!xvar)) +
    geom_point(aes(y = !!yvar1, color = 'blue')) +
    geom_line(aes(y = !!yvar2, color = 'red')) +
    # regression equation
    geom_smooth(aes(x=!!xvar,y=!!yvar1,color ='blue'),method = "lm", formula = y~x,se=F) +
    geom_smooth(aes(x=!!xvar,y=!!yvar2,color = 'red'),method = "lm", formula = y~x,se=F) +
    stat_poly_eq(aes(x=!!xvar,y=!!yvar1,color ='blue',
                     label=paste(stat(eq.label), stat(adj.rr.label), sep = "~~~~")),
                 formula = y~x, label.x.npc = "left", label.y.npc = 0.85,
                 coef.digits=3,rr.digits=3,parse=T)+
    stat_poly_eq(aes(x=!!xvar,y=!!yvar2,color = 'red',
                     label=paste(stat(eq.label), stat(adj.rr.label), sep = "~~~~")),
                 formula = y~x,  label.x.npc = "left", label.y.npc = 0.8,
                 coef.digits=3,rr.digits=3, parse=T)+
    geom_vline(
      xintercept = as.Date('2010-01-01'),
      color = 'black',
      linetype = "dashed"
    ) +
    scale_x_date(
      date_labels = '%Y',
      date_breaks = '1 year',
      expand = c(0, 0)
    ) +
    scale_y_continuous(expand = c(0, 0.1),limits = c(0,10)) +
    ylab(expression("N-NO"[3] ^ -{
    } * " [mg/L]")) +   # expression(' Q,  m'^"3"*' s'^"-1")
    scale_color_manual(
      name = "",
      values = c('blue', 'red'),
      labels = c('Measured', 'Simulated'),
      guide = guide_legend(override.aes = list(
        linetype = c('blank', 'solid'),
        shape = c(16, NA)
      ))
    ) +
    theme_bw() +
    theme(
      axis.text.x = element_text(
        size = 12,
        hjust = 0.5,
        colour = 'black'
      ),
      axis.text.y = element_text(size = 12, colour = 'black'),
      axis.title.y = element_text(size = 15, colour = 'black'),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      legend.position = c(0.98, 0.9),
      legend.direction = "horizontal",
      legend.justification = c('right', 'top'),
      legend.text = element_text(size = 15, colour = 'black')
    )+
    expand_limits(y=0)

  return(ggfun)
}




Conc_plot(Conc_cali, dates, INConcob0579610, INConcsi0579610)

ggsave('./images/Conc_cali_newPre_579610.jpeg')

Conc_plot(Conc_cali, dates, INConcob0579620, INConcsi0579620)
ggsave('./images/Conc_cali_newPre_579620.jpeg')

Conc_plot(Conc_cali, dates, INConcob0579049, INConcsi0579049)
ggsave('./images/Conc_cali_newPre_579049.jpeg')

Conc_plot(Conc_cali, dates, INConcob0579745, INConcsi0579745)
ggsave('./images/Conc_cali_newPre_579745.jpeg')

Conc_plot(Conc_cali, dates, INConcob0579810, INConcsi0579810)
ggsave('./images/Conc_cali_newPre_579810.jpeg')

Conc_plot(Conc_cali, dates, INConcob0579070, INConcsi0579070)
ggsave('./images/Conc_cali_newPre_579070.jpeg')

Conc_plot(Conc_cali, dates, INConcob0579085, INConcsi0579085)
ggsave('./images/Conc_cali_newPre_579085.jpeg')


# Load in calibration period-----
# m3/s *mg/L *86400 s/d =86.4 kg/d
Load_cali <- Q_cali[, -c(1, 2)] * Conc_cali[, -c(1, 2)] *86.4

# write_csv(Load_cali, './data/Load_cali.csv')
#
Load_cali_xts <-
  xts(Q_cali[, -c(1, 2)] * Conc_cali[, -c(1, 2)], order.by = Conc_cali$dates)

# NSE
NSE(Load_cali_xts$Qsim_0000579610,
    Load_cali_xts$Qobs_0000579610)
NSE(Load_cali_xts$Qsim_0000579620,
    Load_cali_xts$Qobs_0000579620)
NSE(Load_cali_xts$Qsim_0000579049,
    Load_cali_xts$Qobs_0000579049)
NSE(Load_cali_xts$Qsim_0000579745,
    Load_cali_xts$Qobs_0000579745)
NSE(Load_cali_xts$Qsim_0000579810,
    Load_cali_xts$Qobs_0000579810)
NSE(Load_cali_xts$Qsim_0000579070,
    Load_cali_xts$Qobs_0000579070)
NSE(Load_cali_xts$Qsim_0000579085,
    Load_cali_xts$Qobs_0000579085)

# KGE
KGE(Load_cali_xts$Qsim_0000579610,
    Load_cali_xts$Qobs_0000579610)
KGE(Load_cali_xts$Qsim_0000579620,
    Load_cali_xts$Qobs_0000579620)
KGE(Load_cali_xts$Qsim_0000579049,
    Load_cali_xts$Qobs_0000579049)
KGE(Load_cali_xts$Qsim_0000579745,
    Load_cali_xts$Qobs_0000579745)
KGE(Load_cali_xts$Qsim_0000579810,
    Load_cali_xts$Qobs_0000579810)
KGE(Load_cali_xts$Qsim_0000579070,
    Load_cali_xts$Qobs_0000579070)
KGE(Load_cali_xts$Qsim_0000579085,
    Load_cali_xts$Qobs_0000579085)

# pbias
pbias(Load_cali_xts$Qsim_0000579610,
      Load_cali_xts$Qobs_0000579610)
pbias(Load_cali_xts$Qsim_0000579620,
      Load_cali_xts$Qobs_0000579620)
pbias(Load_cali_xts$Qsim_0000579049,
      Load_cali_xts$Qobs_0000579049)
pbias(Load_cali_xts$Qsim_0000579745,
      Load_cali_xts$Qobs_0000579745)
pbias(Load_cali_xts$Qsim_0000579810,
      Load_cali_xts$Qobs_0000579810)
pbias(Load_cali_xts$Qsim_0000579070,
      Load_cali_xts$Qobs_0000579070)
pbias(Load_cali_xts$Qsim_0000579085,
      Load_cali_xts$Qobs_0000579085)



# validation --------------------------------------------------------------

# Discharge in validation 2004-2009------
Q_out <-
  read_table(  './data/daily_discharge_vali2018.out'  )
Q_vali <-
  unite(Q_out, 'dates', c('Year', 'Mon', 'Day')) %>% mutate(dates = ymd(dates))


# create xts time series
Q_xts_vali <- xts(Q_vali[, 3:16], order.by = Q_vali$dates)

#

NSE(Q_xts_vali$Qsim_0000579610, Q_xts_vali$Qobs_0000579610)
NSE(Q_xts_vali$Qsim_0000579620, Q_xts_vali$Qobs_0000579620)
NSE(Q_xts_vali$Qsim_0000579049, Q_xts_vali$Qobs_0000579049)
NSE(Q_xts_vali$Qsim_0000579745, Q_xts_vali$Qobs_0000579745)
NSE(Q_xts_vali$Qsim_0000579810, Q_xts_vali$Qobs_0000579810)
NSE(Q_xts_vali$Qsim_0000579070, Q_xts_vali$Qobs_0000579070)
NSE(Q_xts_vali$Qsim_0000579085, Q_xts_vali$Qobs_0000579085)

#
KGE(Q_xts_vali$Qsim_0000579610, Q_xts_vali$Qobs_0000579610)
KGE(Q_xts_vali$Qsim_0000579620, Q_xts_vali$Qobs_0000579620)
KGE(Q_xts_vali$Qsim_0000579049, Q_xts_vali$Qobs_0000579049)
KGE(Q_xts_vali$Qsim_0000579745, Q_xts_vali$Qobs_0000579745)
KGE(Q_xts_vali$Qsim_0000579810, Q_xts_vali$Qobs_0000579810)
KGE(Q_xts_vali$Qsim_0000579070, Q_xts_vali$Qobs_0000579070)
KGE(Q_xts_vali$Qsim_0000579085, Q_xts_vali$Qobs_0000579085)


# pbias
pbias(Q_xts_vali$Qsim_0000579610, Q_xts_vali$Qobs_0000579610)
pbias(Q_xts_vali$Qsim_0000579620, Q_xts_vali$Qobs_0000579620)
pbias(Q_xts_vali$Qsim_0000579049, Q_xts_vali$Qobs_0000579049)
pbias(Q_xts_vali$Qsim_0000579745, Q_xts_vali$Qobs_0000579745)
pbias(Q_xts_vali$Qsim_0000579810, Q_xts_vali$Qobs_0000579810)
pbias(Q_xts_vali$Qsim_0000579070, Q_xts_vali$Qobs_0000579070)
pbias(Q_xts_vali$Qsim_0000579085, Q_xts_vali$Qobs_0000579085)


# absolute value
# m3/s *24*3600 *1000 /10^6 km2= mm/d
Q_abs_vali <- Q_vali %>% mutate(Qobs_meis=Qobs_0000579610*24*3600*10^3/(210*10^6),
                                Qsim_meis=Qsim_0000579610*24*3600*10^3/(210*10^6),
                                Qobs_haus=Qobs_0000579620*24*3600*10^3/(456*10^6),
                                Qsim_haus=Qsim_0000579620*24*3600*10^3/(456*10^6),
                                Qobs_wege=Qobs_0000579049*24*3600*10^3/(1215*10^6) ,
                                Qsim_wege=Qsim_0000579049*24*3600*10^3/(1215*10^6) ,
                                Qobs_nien=Qobs_0000579745*24*3600*10^3/(270*10^6) ,
                                Qsim_nien=Qsim_0000579745*24*3600*10^3/(270*10^6),
                                Qobs_hads=Qobs_0000579070*24*3600*10^3/(2758*10^6),
                                Qsim_hads=Qsim_0000579070*24*3600*10^3/(2758*10^6),
                                Qobs_stass=Qobs_0000579085*24*3600*10^3/(3200*10^6),
                                Qsim_stass=Qsim_0000579085*24*3600*10^3/(3200*10^6),
                                )
# absolute difference
Q_abs_vali_diff <-
  Q_abs_vali %>% mutate(
    meis_diff = Qsim_meis - Qobs_meis,
    haus_diff = Qsim_haus - Qobs_haus,
    wege_diff = Qsim_wege - Qobs_wege,
    nien_diff = Qsim_nien - Qobs_nien,
    hads_diff = Qsim_hads - Qobs_hads,
    stass_diff = Qsim_stass - Qobs_stass
  ) %>%
  group_by(year(dates)) %>%
  summarise(
    meis_diff_year = sum(meis_diff),
    haus_diff_year = sum(haus_diff),
    wege_diff_year = sum(wege_diff),
    nien_diff_year = sum(nien_diff),
    hads_diff_year = sum(hads_diff),
    stass_diff_year = sum(stass_diff)
  ) %>%
  summarise(
    meis_diff_year_mean = mean(meis_diff_year),
    haus_diff_year_mean = mean(haus_diff_year),
    wege_diff_year_mean = mean(wege_diff_year),
    nien_diff_year_mean = mean(nien_diff_year),
    hads_diff_year_mean = mean(hads_diff_year),
    stass_diff_year_mean = mean(stass_diff_year)
  )

#
Q_plot <- function(data, xvar, yvar1, yvar2) {
  xvar <- enquo(xvar)
  yvar1 <- enquo(yvar1)
  yvar2 <- enquo(yvar2)
  ggfun <- ggplot(data, aes(x = !!xvar)) +
    geom_point(aes(y = !!yvar1, color = 'blue')) +
    geom_line(aes(y = !!yvar2, color = 'red')) +
    # geom_vline(
    #   xintercept = as.Date('2010-01-01'),
    #   color = 'black',
    #   linetype = "dashed"
    # ) +
    scale_x_date(
      date_labels = '%b-%Y',
      date_breaks = '1 year',
      expand = c(0, 0)
    ) +
    scale_y_continuous(expand = c(0, 0.1)) +
    ylab(expression(" Q (mm)")) +   #
    scale_color_manual(
      name = "",
      values = c('blue', 'red'),
      labels = c('Measured', 'Simulated'),
      guide = guide_legend(override.aes = list(
        linetype = c('blank', 'solid'),
        shape = c(16, NA)
      ))
    ) +
    theme_bw() +
    theme(
      axis.text.x = element_text(
        size = 10,
        hjust = 0.5,
        colour = 'black'
      ),
      axis.text.y = element_text(size = 20, colour = 'black'),
      axis.title.y = element_text(size = 20, colour = 'black'),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      legend.position = c(0.95, 0.95),
      legend.direction = "horizontal",
      legend.justification = c('right', 'top'),
      legend.text = element_text(size = 20, colour = 'black')
    )

  return(ggfun)
}

Q_plot(Q_abs_vali,dates,Qobs_meis,Qsim_meis)



# discharge in 2016-2018 --------------------------------------------------

Q <- read_table('./data/daily_discharge_cali2018.out')
Q_vali <-
  unite(Q, 'dates', c('Year', 'Mon', 'Day')) %>%
  mutate(dates = ymd(dates)) %>%
  filter(between(dates,as.Date('2016-01-01'),as.Date('2018-12-31') )  )
Q_vali[Q_vali == -9999] <- NA
# create xts time series
Q_xts_vali <- xts(Q_vali[, 3:16], order.by = Q_vali$dates)

#

NSE(Q_xts_vali$Qsim_0000579610, Q_xts_vali$Qobs_0000579610)
NSE(Q_xts_vali$Qsim_0000579620, Q_xts_vali$Qobs_0000579620)
NSE(Q_xts_vali$Qsim_0000579049, Q_xts_vali$Qobs_0000579049)
NSE(Q_xts_vali$Qsim_0000579745, Q_xts_vali$Qobs_0000579745)
NSE(Q_xts_vali$Qsim_0000579810, Q_xts_vali$Qobs_0000579810)
NSE(Q_xts_vali$Qsim_0000579070, Q_xts_vali$Qobs_0000579070)
NSE(Q_xts_vali$Qsim_0000579085, Q_xts_vali$Qobs_0000579085)

#
KGE(Q_xts_vali$Qsim_0000579610, Q_xts_vali$Qobs_0000579610)
KGE(Q_xts_vali$Qsim_0000579620, Q_xts_vali$Qobs_0000579620)
KGE(Q_xts_vali$Qsim_0000579049, Q_xts_vali$Qobs_0000579049)
KGE(Q_xts_vali$Qsim_0000579745, Q_xts_vali$Qobs_0000579745)
KGE(Q_xts_vali$Qsim_0000579810, Q_xts_vali$Qobs_0000579810)
KGE(Q_xts_vali$Qsim_0000579070, Q_xts_vali$Qobs_0000579070)
KGE(Q_xts_vali$Qsim_0000579085, Q_xts_vali$Qobs_0000579085)


# pbias
pbias(Q_xts_vali$Qsim_0000579610, Q_xts_vali$Qobs_0000579610)
pbias(Q_xts_vali$Qsim_0000579620, Q_xts_vali$Qobs_0000579620)
pbias(Q_xts_vali$Qsim_0000579049, Q_xts_vali$Qobs_0000579049)
pbias(Q_xts_vali$Qsim_0000579745, Q_xts_vali$Qobs_0000579745)
pbias(Q_xts_vali$Qsim_0000579810, Q_xts_vali$Qobs_0000579810)
pbias(Q_xts_vali$Qsim_0000579070, Q_xts_vali$Qobs_0000579070)
pbias(Q_xts_vali$Qsim_0000579085, Q_xts_vali$Qobs_0000579085)


# absolute difference
Q_abs_vali <-
  Q_vali %>% mutate(
    Qobs_meis = Qobs_0000579610 * 24 * 3600 * 10 ^ 3 / (210 * 10 ^ 6),
    Qsim_meis = Qsim_0000579610 * 24 * 3600 * 10 ^ 3 / (210 * 10 ^ 6),
    Qobs_haus = Qobs_0000579620 * 24 * 3600 * 10 ^ 3 / (456 * 10 ^ 6),
    Qsim_haus = Qsim_0000579620 * 24 * 3600 * 10 ^ 3 / (456 * 10 ^ 6),
    Qobs_wege = Qobs_0000579049 * 24 * 3600 * 10 ^ 3 / (1215 * 10 ^ 6) ,
    Qsim_wege = Qsim_0000579049 * 24 * 3600 * 10 ^ 3 / (1215 * 10 ^ 6) ,
    Qobs_nien = Qobs_0000579745 * 24 * 3600 * 10 ^ 3 / (270 * 10 ^ 6) ,
    Qsim_nien = Qsim_0000579745 * 24 * 3600 * 10 ^ 3 / (270 * 10 ^ 6),
    Qobs_hads = Qobs_0000579070 * 24 * 3600 * 10 ^ 3 / (2758 * 10 ^ 6),
    Qsim_hads = Qsim_0000579070 * 24 * 3600 * 10 ^ 3 / (2758 * 10 ^ 6),
    Qobs_stass = Qobs_0000579085 * 24 * 3600 * 10 ^ 3 / (3200 * 10 ^ 6),
    Qsim_stass = Qsim_0000579085 * 24 * 3600 * 10 ^ 3 / (3200 * 10 ^ 6),
  )
# absolute difference
Q_abs_vali_diff <-
  Q_abs_vali %>% mutate(
    meis_diff = Qsim_meis - Qobs_meis,
    haus_diff = Qsim_haus - Qobs_haus,
    wege_diff = Qsim_wege - Qobs_wege,
    nien_diff = Qsim_nien - Qobs_nien,
    hads_diff = Qsim_hads - Qobs_hads,
    stass_diff = Qsim_stass - Qobs_stass
  ) %>%
  group_by(year(dates)) %>%
  summarise(
    meis_diff_year = sum(meis_diff),
    haus_diff_year = sum(haus_diff),
    wege_diff_year = sum(wege_diff),
    nien_diff_year = sum(nien_diff),
    hads_diff_year = sum(hads_diff),
    stass_diff_year = sum(stass_diff)
  ) %>%
  summarise(
    meis_diff_year_mean = mean(meis_diff_year),
    haus_diff_year_mean = mean(haus_diff_year),
    wege_diff_year_mean = mean(wege_diff_year),
    nien_diff_year_mean = mean(nien_diff_year,na.rm = T),
    hads_diff_year_mean = mean(hads_diff_year),
    stass_diff_year_mean = mean(stass_diff_year)
  )


# concentration in validation 2004-2009--------

Conc_out_vali <-
  read_table("./data/daily_concentration_vali2018.out",
    col_names = TRUE
  )
# combine mutiple columns to one date
Conc_vali <-
  Conc_out_vali %>% unite('dates', c('Year', 'Mon', 'Day')) %>% mutate(dates =
                                                                         ymd(dates))

Conc_vali[Conc_vali == -9999] <- NA

#
Conc_vali_xts <- xts(Conc_vali[, 3:16], order.by = Conc_vali$dates)

#

Conc_plot <- function(data, xvar, yvar1, yvar2) {
  xvar <- enquo(xvar)
  yvar1 <- enquo(yvar1)
  yvar2 <- enquo(yvar2)
  ggfun <- ggplot(data, aes(x = !!xvar)) +
    geom_point(aes(y = !!yvar1, color = 'blue')) +
    geom_line(aes(y = !!yvar2, color = 'red')) +
    # geom_vline(
    #   xintercept = as.Date('2010-01-01'),
    #   color = 'black',
    #   linetype = "dashed"
    # ) +
    scale_x_date(
      date_labels = '%b-%Y',
      date_breaks = '1 year',
      expand = c(0, 0)
    ) +
    scale_y_continuous(expand = c(0, 0.1)) +
    ylab(expression("N-NO"[3] ^ -{
    } * " [mg/L]")) +   # expression(' Q,  m'^"3"*' s'^"-1")
    scale_color_manual(
      name = "",
      values = c('blue', 'red'),
      labels = c('Measured', 'Simulated'),
      guide = guide_legend(override.aes = list(
        linetype = c('blank', 'solid'),
        shape = c(16, NA)
      ))
    ) +
    theme_bw() +
    theme(
      axis.text.x = element_text(
        size = 10,
        hjust = 0.5,
        colour = 'black'
      ),
      axis.text.y = element_text(size = 10, colour = 'black'),
      axis.title.y = element_text(size = 20, colour = 'black'),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      legend.position = c(0.98, 0.99),
      legend.direction = "horizontal",
      legend.justification = c('right', 'top'),
      legend.text = element_text(size = 15, colour = 'black')
    )+
    expand_limits(y=0)

  return(ggfun)
}


# plot
Conc_plot(Conc_vali,dates,INConcob0579610,INConcsi0579610)

Conc_plot(Conc_vali,dates,INConcob0579620,INConcsi0579620)

Conc_plot(Conc_vali,dates,INConcob0579049,INConcsi0579049)

Conc_plot(Conc_vali,dates,INConcob0579745,INConcsi0579745)

Conc_plot(Conc_vali,dates,INConcob0579810,INConcsi0579810)

Conc_plot(Conc_vali,dates,INConcob0579070,INConcsi0579070)

Conc_plot(Conc_vali,dates,INConcob0579085,INConcsi0579085)

#

# NSE
NSE(Conc_vali_xts$INConcsi0579610,
    Conc_vali_xts$INConcob0579610)
NSE(Conc_vali_xts$INConcsi0579620,
    Conc_vali_xts$INConcob0579620)
NSE(Conc_vali_xts$INConcsi0579049,
    Conc_vali_xts$INConcob0579049)
NSE(Conc_vali_xts$INConcsi0579745,
    Conc_vali_xts$INConcob0579745)
NSE(Conc_vali_xts$INConcsi0579810,
    Conc_vali_xts$INConcob0579810)
NSE(Conc_vali_xts$INConcsi0579070,
    Conc_vali_xts$INConcob0579070)
NSE(Conc_vali_xts$INConcsi0579085,
    Conc_vali_xts$INConcob0579085)

# KGE
KGE(Conc_vali_xts$INConcsi0579610,
    Conc_vali_xts$INConcob0579610)
KGE(Conc_vali_xts$INConcsi0579620,
    Conc_vali_xts$INConcob0579620)
KGE(Conc_vali_xts$INConcsi0579049,
    Conc_vali_xts$INConcob0579049)
KGE(Conc_vali_xts$INConcsi0579745,
    Conc_vali_xts$INConcob0579745)
KGE(Conc_vali_xts$INConcsi0579810,
    Conc_vali_xts$INConcob0579810)
KGE(Conc_vali_xts$INConcsi0579070,
    Conc_vali_xts$INConcob0579070)
KGE(Conc_vali_xts$INConcsi0579085,
    Conc_vali_xts$INConcob0579085)
#
pbias(Conc_vali_xts$INConcsi0579610,
      Conc_vali_xts$INConcob0579610)
pbias(Conc_vali_xts$INConcsi0579620,
      Conc_vali_xts$INConcob0579620)
pbias(Conc_vali_xts$INConcsi0579049,
      Conc_vali_xts$INConcob0579049)
pbias(Conc_vali_xts$INConcsi0579745,
      Conc_vali_xts$INConcob0579745)
pbias(Conc_vali_xts$INConcsi0579810,
      Conc_vali_xts$INConcob0579810)
pbias(Conc_vali_xts$INConcsi0579070,
      Conc_vali_xts$INConcob0579070)
pbias(Conc_vali_xts$INConcsi0579085,
      Conc_vali_xts$INConcob0579085)

# concentration in 2016-2018 ----------------------------------------------
Conc_out <-  read_table("./data/daily_concentration_cali2018.out", col_names = TRUE)
# combine mutiple columns to one date
Conc_vali <-
  Conc_out %>% unite('dates', c('Year', 'Mon', 'Day')) %>%
  mutate(dates = ymd(dates)) %>%
  filter(between(dates,as.Date('2016-01-01'),as.Date('2018-12-31') )  )

Conc_vali[Conc_vali == -9999] <- NA
#
Conc_vali_xts <- xts(Conc_vali[, 3:16], order.by = Conc_vali$dates)

# NSE
NSE(Conc_vali_xts$INConcsi0579610,
    Conc_vali_xts$INConcob0579610)
NSE(Conc_vali_xts$INConcsi0579620,
    Conc_vali_xts$INConcob0579620)
NSE(Conc_vali_xts$INConcsi0579049,
    Conc_vali_xts$INConcob0579049)
NSE(Conc_vali_xts$INConcsi0579745,
    Conc_vali_xts$INConcob0579745)
NSE(Conc_vali_xts$INConcsi0579810,
    Conc_vali_xts$INConcob0579810)
NSE(Conc_vali_xts$INConcsi0579070,
    Conc_vali_xts$INConcob0579070)
NSE(Conc_vali_xts$INConcsi0579085,
    Conc_vali_xts$INConcob0579085)

# KGE
KGE(Conc_vali_xts$INConcsi0579610,
    Conc_vali_xts$INConcob0579610)
KGE(Conc_vali_xts$INConcsi0579620,
    Conc_vali_xts$INConcob0579620)
KGE(Conc_vali_xts$INConcsi0579049,
    Conc_vali_xts$INConcob0579049)
KGE(Conc_vali_xts$INConcsi0579745,
    Conc_vali_xts$INConcob0579745)
KGE(Conc_vali_xts$INConcsi0579810,
    Conc_vali_xts$INConcob0579810)
KGE(Conc_vali_xts$INConcsi0579070,
    Conc_vali_xts$INConcob0579070)
KGE(Conc_vali_xts$INConcsi0579085,
    Conc_vali_xts$INConcob0579085)
#
pbias(Conc_vali_xts$INConcsi0579610,
      Conc_vali_xts$INConcob0579610)
pbias(Conc_vali_xts$INConcsi0579620,
      Conc_vali_xts$INConcob0579620)
pbias(Conc_vali_xts$INConcsi0579049,
      Conc_vali_xts$INConcob0579049)
pbias(Conc_vali_xts$INConcsi0579745,
      Conc_vali_xts$INConcob0579745)
pbias(Conc_vali_xts$INConcsi0579810,
      Conc_vali_xts$INConcob0579810)
pbias(Conc_vali_xts$INConcsi0579070,
      Conc_vali_xts$INConcob0579070)
pbias(Conc_vali_xts$INConcsi0579085,
      Conc_vali_xts$INConcob0579085)






# Load in valibration period----------------------------------------
# m3/s *mg/L *86400 s/d =86.4 kg/d

Load_vali <- Q_vali[, -c(1, 2)] * Conc_vali[, -c(1, 2)]*86.4


#
Load_vali_xts <-
  xts(Q_vali[, -c(1, 2)] * Conc_vali[, -c(1, 2)], order.by = Conc_vali$dates)

# NSE
NSE(Load_vali_xts$Qsim_0000579610,
    Load_vali_xts$Qobs_0000579610)
NSE(Load_vali_xts$Qsim_0000579620,
    Load_vali_xts$Qobs_0000579620)
NSE(Load_vali_xts$Qsim_0000579049,
    Load_vali_xts$Qobs_0000579049)
NSE(Load_vali_xts$Qsim_0000579745,
    Load_vali_xts$Qobs_0000579745)
NSE(Load_vali_xts$Qsim_0000579810,
    Load_vali_xts$Qobs_0000579810)
NSE(Load_vali_xts$Qsim_0000579070,
    Load_vali_xts$Qobs_0000579070)
NSE(Load_vali_xts$Qsim_0000579085,
    Load_vali_xts$Qobs_0000579085)

# KGE
KGE(Load_vali_xts$Qsim_0000579610,
      Load_vali_xts$Qobs_0000579610)
KGE(Load_vali_xts$Qsim_0000579620,
      Load_vali_xts$Qobs_0000579620)
KGE(Load_vali_xts$Qsim_0000579049,
      Load_vali_xts$Qobs_0000579049)
KGE(Load_vali_xts$Qsim_0000579745,
      Load_vali_xts$Qobs_0000579745)
KGE(Load_vali_xts$Qsim_0000579810,
      Load_vali_xts$Qobs_0000579810)
KGE(Load_vali_xts$Qsim_0000579070,
      Load_vali_xts$Qobs_0000579070)
KGE(Load_vali_xts$Qsim_0000579085,
      Load_vali_xts$Qobs_0000579085)

# pbias
pbias(Load_vali_xts$Qsim_0000579610,
      Load_vali_xts$Qobs_0000579610)
pbias(Load_vali_xts$Qsim_0000579620,
      Load_vali_xts$Qobs_0000579620)
pbias(Load_vali_xts$Qsim_0000579049,
      Load_vali_xts$Qobs_0000579049)
pbias(Load_vali_xts$Qsim_0000579745,
      Load_vali_xts$Qobs_0000579745)
pbias(Load_vali_xts$Qsim_0000579810,
      Load_vali_xts$Qobs_0000579810)
pbias(Load_vali_xts$Qsim_0000579070,
      Load_vali_xts$Qobs_0000579070)
pbias(Load_vali_xts$Qsim_0000579085,
      Load_vali_xts$Qobs_0000579085)

# absolute differences in 2004-2009----
# m3/s *mg/L *86400 s/d =86.4 kg/d

Load_vali <- Q_vali[, -c(1, 2)] * Conc_vali[, -c(1, 2)]*86.4

# kg/d / 100ha =  /10^2 kg/ha/d
Load_vali_diff <- Load_vali %>% mutate(
  Load_obs_meis = Qobs_0000579610    / (210 * 10 ^2),
  Load_sim_meis = Qsim_0000579610    / (210 * 10 ^2),
  Load_obs_haus = Qobs_0000579620    / (456 * 10 ^2),
  Load_sim_haus = Qsim_0000579620    / (456 * 10 ^2),
  Load_obs_wege = Qobs_0000579049    / (1215 * 10 ^2) ,
  Load_sim_wege = Qsim_0000579049    / (1215 * 10 ^2) ,
  Load_obs_nien = Qobs_0000579745    / (270 * 10 ^2) ,
  Load_sim_nien = Qsim_0000579745    / (270 * 10 ^2),
  Load_obs_hads = Qobs_0000579070    / (2758 * 10 ^2),
  Load_sim_hads = Qsim_0000579070    / (2758 * 10 ^2),
  Load_obs_stass = Qobs_0000579085    / (3200 * 10 ^2),
  Load_sim_stass = Qsim_0000579085    / (3200 * 10 ^2),
)


# plot
  #
 Load_vali_diff %>% mutate(dates = seq(as.Date('2004-01-01'),as.Date('2009-12-31'),by='days') ) %>%
   ggplot(aes(x=dates))+
   geom_point(aes(y=Load_obs_haus),color='black')+
   geom_line(aes(y=Load_sim_haus),color='red')+
   theme_bw()+
   labs(x='Date', y= expression('Nitrate Load (kg/ha/d)'))+
   theme(
     legend.position = c(.95, .95),
     legend.justification = c("right", "top"),
     legend.box.just = "right",
     legend.margin = margin(6, 6, 6, 6)
   )



# absolute difference
Load_abs_vali_diff <-
  Load_vali_diff %>% mutate(
    dates = seq(as.Date('2004-01-01'),as.Date('2009-12-31'),by='days'),
    meis_diff = Load_sim_meis - Load_obs_meis,
    haus_diff = Load_sim_haus - Load_obs_haus,
    wege_diff = Load_sim_wege - Load_obs_wege,
    nien_diff = Load_sim_nien - Load_obs_nien,
    hads_diff = Load_sim_hads - Load_obs_hads,
    stass_diff = Load_sim_stass - Load_obs_stass
  ) %>%
  group_by(year(dates)) %>%
  summarise(
    meis_diff_year = sum(meis_diff,na.rm = T),
    haus_diff_year = sum(haus_diff,na.rm = T),
    wege_diff_year = sum(wege_diff,na.rm = T),
    nien_diff_year = sum(nien_diff,na.rm = T),
    hads_diff_year = sum(hads_diff,na.rm = T),
    stass_diff_year = sum(stass_diff,na.rm = T)
  )  %>%
  summarise(
    meis_diff_year_mean = mean(meis_diff_year,na.rm = T),
    haus_diff_year_mean = mean(haus_diff_year,na.rm = T),
    wege_diff_year_mean = mean(wege_diff_year,na.rm = T),
    nien_diff_year_mean = mean(nien_diff_year,na.rm = T),
    hads_diff_year_mean = mean(hads_diff_year,na.rm = T),
    stass_diff_year_mean = mean(stass_diff_year,na.rm = T)
  )

#absolute didffereces in 2016-2018----
# m3/s *mg/L *86400 s/d =86.4 kg/d

Load_vali <- Q_vali[, -c(1, 2)] * Conc_vali[, -c(1, 2)]*86.4


# glimpse data distribution
Load_vali %>% mutate(dates = seq(as.Date('2016-01-01'),as.Date('2018-12-31'),by='days') ) %>%
  ggplot(aes(x=factor(year(dates)), y=Qobs_0000579610,group=factor(year(dates)))) +
  geom_boxplot(outlier.shape = NA)+
  coord_cartesian(ylim = c(0,500))+
  theme_bw()

Load_vali_diff <- Load_vali %>% mutate(
  Load_obs_meis = Qobs_0000579610    / (210 * 10 ^2),
  Load_sim_meis = Qsim_0000579610    / (210 * 10 ^2),
  Load_obs_haus = Qobs_0000579620    / (456 * 10 ^2),
  Load_sim_haus = Qsim_0000579620    / (456 * 10 ^2),
  Load_obs_wege = Qobs_0000579049    / (1215 * 10 ^2) ,
  Load_sim_wege = Qsim_0000579049    / (1215 * 10 ^2) ,
  Load_obs_nien = Qobs_0000579745    / (270 * 10 ^2) ,
  Load_sim_nien = Qsim_0000579745    / (270 * 10 ^2),
  Load_obs_hads = Qobs_0000579070    / (2758 * 10 ^2),
  Load_sim_hads = Qsim_0000579070    / (2758 * 10 ^2),
  Load_obs_stass = Qobs_0000579085    / (3200 * 10 ^2),
  Load_sim_stass = Qsim_0000579085    / (3200 * 10 ^2),
)
#
Load_vali_diff %>% mutate(dates = seq(as.Date('2016-01-01'),as.Date('2018-12-31'),by='days') ) %>%
  ggplot(aes(x=dates))+
  geom_point(aes(y=Load_obs_haus),color='black')+
  geom_line(aes(y=Load_sim_haus),color='red')+
  theme_bw()+
  labs(x='Date', y= expression('Nitrate Load (kg/ha/d)'))+
  theme(
    legend.position = c(.95, .95),
    legend.justification = c("right", "top"),
    legend.box.just = "right",
    legend.margin = margin(6, 6, 6, 6)
  )



# absolute difference in 2016-2018
Load_abs_vali_diff <-
  Load_vali_diff %>% mutate(
    dates = seq(as.Date('2016-01-01'),as.Date('2018-12-31'),by='days'),
    meis_diff = Load_sim_meis - Load_obs_meis,
    haus_diff = Load_sim_haus - Load_obs_haus,
    wege_diff = Load_sim_wege - Load_obs_wege,
    nien_diff = Load_sim_nien - Load_obs_nien,
    hads_diff = Load_sim_hads - Load_obs_hads,
    stass_diff = Load_sim_stass - Load_obs_stass
  ) %>%
  group_by(year(dates)) %>%
  summarise(
    meis_diff_year = sum(meis_diff,na.rm = T),
    haus_diff_year = sum(haus_diff,na.rm = T),
    wege_diff_year = sum(wege_diff,na.rm = T),
    nien_diff_year = sum(nien_diff,na.rm = T),
    hads_diff_year = sum(hads_diff,na.rm = T),
    stass_diff_year = sum(stass_diff,na.rm = T)
  )  %>%
  summarise(
    meis_diff_year_mean = mean(meis_diff_year,na.rm = T),
    haus_diff_year_mean = mean(haus_diff_year,na.rm = T),
    wege_diff_year_mean = mean(wege_diff_year,na.rm = T),
    nien_diff_year_mean = mean(nien_diff_year,na.rm = T),
    hads_diff_year_mean = mean(hads_diff_year,na.rm = T),
    stass_diff_year_mean = mean(stass_diff_year,na.rm = T)
  )
