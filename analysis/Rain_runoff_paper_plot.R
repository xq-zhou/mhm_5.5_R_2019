# plot precipitation and discharge in one plot

library(tidyverse)
library(ncdf4)
library(lubridate)
library(gridExtra)
library(ggpmisc)
# select grid pre ---------------------------------------------------------

ncpath <- "D:/BODE_data/Climate data/netcdf_prepare/data/"
ncname <- "pre_Y_own_2000_2019"
ncfname <- paste(ncpath, ncname, ".nc", sep = "")
ncin <- nc_open(ncfname)
print(ncin)
pre <- ncvar_get(ncin, "pre")
nc_close(ncin)
# lon <- ncvar_get(ncin, "lon")
# lat <- ncvar_get(ncin, "lat")
# find index of 2018-12-31
  # as.Date(1462,origin='2000-01-01')

# select validation and calibration period precipitation-----------
pre_Meisdorf_2004_2018 <- pre[50, 61, 1462:6940]
#
pre_Hausneidorf_2004_2018 <- pre[49, 45, 1462:6940]
#
pre_Wegeleben_2004_2018 <- pre[44, 39, 1462:6940]
#
pre_Nienhagen_2004_2018 <- pre[40, 34, 1462:6940]
#
pre_Oschers_2004_2018 <- pre[48, 24, 1462:6940]
#
pre_Hadmers_2004_2018 <- pre[53, 26, 1462:6940]
#
pre_stassfurt_2004_2018 <- pre[71, 43, 1462:6940]


# #
sim_pre_2004_2018 <- data.frame(
  pre_Meisdorf_2004_2018,
  pre_Hausneidorf_2004_2018,
  pre_Wegeleben_2004_2018,
  pre_Nienhagen_2004_2018,
  pre_Oschers_2004_2018,
  pre_Hadmers_2004_2018,
  pre_stassfurt_2004_2018,
  Date=seq(as.Date('2004-01-01'),as.Date('2018-12-31'),by='day')
)

write_csv(sim_pre_2004_2018,'./data/Pre_cali_stations_2004_2018.csv')
# calibration  discharge -----------
Q <- read_table('./data/daily_discharge_cali2018.out')
Q_cali <- unite(Q, 'Date', c('Year', 'Mon', 'Day')) %>% mutate(Date = ymd(Date))

# validation  discharge -----------
Q_v <- read_table('D:/Project progress/Improved_pre_results/Newpre_cali/data/validation/daily_discharge.out')
Q_vali <- unite(Q_v, 'Date', c('Year', 'Mon', 'Day')) %>% mutate(Date = ymd(Date))

#
Q <- rbind(Q_vali,Q_cali)

write_csv(Q,'./data/Q_2004_2018.csv')
# combine Pre and Discharge----
df <- sim_pre_2004_2018 %>% full_join(Q,by='Date')



# plot discharge and precipitation in one plot with base R graphic-----------

# Calculate the range needed to avoid having your hyetograph and hydrograph overlap
# https://rpubs.com/cxiao/hydrograph-ggplot2-plot
#
# plot.new()
#
# par(mfrow = c(2, 1))
# par(mar=c(1,3,0.5,3))
# maxRange <-  max(c(df$Qobs_0000579610,df$Qsim_0000579610),na.rm = T)
#
# maxP <- max(df$pre_Meisdorf_2004_2018,na.rm = T)
#
# plot(df$Date, df$Qsim_0000579610,
#      type = 'l', col = "red",lwd=2,
#      ylim = c(0, 1.3 * maxRange),
#       # specified limits to lie at the edges of the plot window,
#      #
#      xaxs = "i", yaxs = "i",
#
#      axes=FALSE )
#
# lines(df$Date, df$Qobs_0000579610, col = "black")
# box()
# axis(side = 1,tcl = -0.2,labels=FALSE)
# axis(side = 2,tcl = -0.2,mgp = c(3, 0.3, 0), cex.axis = 0.8)
# mtext( expression( "Discharge (" ~ m^3* s^-1* ')'  ),side = 2,line = 1.2,cex.lab=0.8)
#
# # #
# par(new = TRUE)
# plot(x = df$Date, y = rep(0, nrow(df)),
#      type = "n", ylim = c(2 * maxP, 0),
#      #  specified limits to lie at the edges of the plot wind
#      xaxs = "i", yaxs = "i",
#      axes=FALSE  )
# segments(x0 = df$Date , y0 = rep(0, nrow(df)),
#          x1 = df$Date, y1 = df$pre_Meisdorf_2004_2018,
#          lend = 2, lwd =1,col = "blue")
# #
# yrAxis <- seq(0,150,by = 50)
# axis(4,at=yrAxis,labels = yrAxis,tcl = -0.2, mgp = c(3, 0.3, 0), cex.axis = 0.8)
#
# mtext('Precipitation (mm/d)',side = 4, line = 1.2,cex.lab=0.8)
# # legend
# legend(x=grconvertX(0.7, from="npc", to="user"),
#        y = grconvertY(0.7, from = "npc", to="user"),
#        c("Observed", "Simulated"),
#        col = c("black", "red"),lwd = c(1,2),cex = 0.8,
#        # Remove legend border using box.lty = 0
#        box.lty = 0)
# abline(v=as.Date("01/01/2010","%d/%m/%Y"), col='black',lty='dashed')
# abline(v=as.Date("01/01/2016","%d/%m/%Y"), col='black',lty='dashed')
#
#
# # plot concentration
#
# plot(df_all$Date,df_all$INConcsi0579610,
#      type = 'l', col = "red",lwd=2,
#      xaxs = "i",  yaxs = "i" ,
#      tcl = -0.2,mgp = c(3, 0.3, 0), cex.axis = 0.8,
#      ylim = c(0,8))
# points(df_all$Date,df_all$INConcob0579610,pch=19)
#
# axis(side = 2, tcl = -0.2,mgp = c(3, 0.3, 0), cex.axis = 0.8, ylim=c(0,8))
# mtext( expression( "N-NO"[3] ^-{} * " [mg/L]") ,side = 2,line = 1.2)
# legend(x=grconvertX(0.5, from="npc", to="user"),
#        y = grconvertY(0.95, from = "npc", to="user"),
#        c("Observed", "Simulated"),
#        col = c("black", "red"),
#        lty = c(NA,1),pch =c(19,NA),
#        lwd = c(NA,2),
#        cex = 0.8,
#         ncol = 2,
#        # Remove legend border using box.lty = 0
#        box.lty = 0)
# abline(v=as.Date("01/01/2010","%d/%m/%Y"), col='black',lty='dashed')
# abline(v=as.Date("01/01/2016","%d/%m/%Y"), col='black',lty='dashed')




# NO3 vali and calibration plot -------------------------------------------

# calibration
Conc_out <-  read_table("./data/daily_concentration_cali2018.out", col_names = TRUE)
# combine mutiple columns to one date
Conc_cali <-
  Conc_out %>% unite('dates', c('Year', 'Mon', 'Day')) %>% mutate(dates = ymd(dates))
Conc_cali[Conc_cali == -9999] <- NA

# validation
Conc_out_vali <-
  read_table("./data/daily_concentration_vali2018.out",
             col_names = TRUE  )
# combine mutiple columns to one date
Conc_vali <-
  Conc_out_vali %>% unite('dates', c('Year', 'Mon', 'Day')) %>% mutate(dates = ymd(dates))

Conc_vali[Conc_vali == -9999] <- NA

#
conc_vali_cali <- rbind(Conc_vali,Conc_cali)

# write_csv(conc_vali_cali, './data/conc_vali_cali.csv')

# combine pre,discharge and concentration----
df_all <- full_join(df,conc_vali_cali,by=c('Date'='dates'))


plot(df_all$INConcob0579610~df_all$Date)
# hydrograph and water quality plot ----
hydro_wq_plot <- function(data,Date, obs, sim, pre,wqobs,wqsim) {

  par(mfrow = c(2, 1))
  par(mar=c(1.5,3,0.5,3))
  # hydrograph plot
  maxRange <- max( data[[obs]], data[[sim]], na.rm = T )
  maxP <- max(data[[pre]],na.rm = T)
  # simulation discharge
  plot(data[[Date]], data[[sim]],
       type = 'l', col = "red",lwd=2,
       ylim = c(0, 1.5 * maxRange),
       # specified limits to lie at the edges of the plot window
       xaxs = "i", yaxs = "i",
       # # turn off  axis ticks
       axes=FALSE)
  # observation discharge
  lines(data[[Date]], data[[obs]], col = "black")
  #
  box()
  axis.Date(side = 1,at=seq( min(df$Date),max(df$Date),by='years'),tcl = -0.2,labels=FALSE)
  axis(side = 2,tcl = -0.2,mgp = c(3, 0.3, 0), cex.axis = 0.8)
  mtext( expression( "Discharge (" ~ m^3* s^-1* ')'  ),side = 2,line = 1.2,cex.lab=0.8)


  # precipitation
  par(new = TRUE)
  plot(x = data[[Date]], y = rep(0, nrow(data)),
       type = "n", ylim = c(2 * maxP, 0),
       #  specified limits to lie at the edges of the plot wind
       xaxs = "i", yaxs = "i",
       axes=FALSE  )
  segments(x0 = data[[Date]], y0 = rep(0, nrow(data)),
           x1 = data[[Date]], y1 = data[[pre]],
           lend = 2, lwd =1,col = "blue")
  # secondary y axis which is difficulty to set using ggplot

  axis(4,ylim=c(0,150),col.axis="blue",tcl = -0.2, mgp = c(3, 0.3, 0), cex.axis = 0.8)
  mtext('Precipitation (mm/d)',side = 4, col = "blue",line = 1.2,cex.lab=0.8)

  # legend
  legend(x=grconvertX(0.5, from="npc", to="user"),
         y = grconvertY(0.7, from = "npc", to="user"),
         c("Observed", "Simulated"),
         col = c("black", "red"),lwd = c(1,2),cex = 0.8,
         # Remove legend border using box.lty = 0
         box.lty = 0,
         ncol = 2)
  abline(v=as.Date("01/01/2010","%d/%m/%Y"), col='black',lty='dashed')
  abline(v=as.Date("01/01/2016","%d/%m/%Y"), col='black',lty='dashed')


  # plot concentration
  # simulation concentration
  plot(data[[Date]],data[[wqsim]],
       type = 'l', col = "red",lwd=2,
       xaxs = "i",  yaxs = "i" ,
       tcl = -0.2,mgp = c(3, 0.3, 0), cex.axis = 0.8,
       ylim = c(0,10))
  # observation concentration
  points(data[[Date]],data[[wqobs]],pch=19)

  axis(side = 2, tcl = -0.2,mgp = c(3, 0.3, 0), cex.axis = 0.8, ylim=c(0,8))
  mtext( expression( "N-NO"[3] ^-{} * " [mg/L]") ,side = 2,line = 1.2)
  legend(x=grconvertX(0.5, from="npc", to="user"),
         y = grconvertY(0.95, from = "npc", to="user"),
         c("Observed", "Simulated"),
         col = c("black", "red"),
         lty = c(NA,1),pch =c(19,NA),
         lwd = c(NA,2),
         cex = 0.8,
         ncol = 2,
         # Remove legend border using box.lty = 0
         box.lty = 0)
  abline(v=as.Date("01/01/2010","%d/%m/%Y"), col='black',lty='dashed')
  abline(v=as.Date("01/01/2016","%d/%m/%Y"), col='black',lty='dashed')
}

#
hydro_wq_plot(data=df_all,Date = 'Date',obs='Qobs_0000579610',
              sim='Qsim_0000579610',pre='pre_Meisdorf_2004_2018',
              wqobs='INConcob0579610',wqsim='INConcsi0579610')
#
hydro_wq_plot(data=df_all,Date = 'Date',obs='Qobs_0000579620',
              sim='Qsim_0000579620',pre='pre_Hausneidorf_2004_2018',
              wqobs='INConcob0579620',wqsim='INConcsi0579620')
#
hydro_wq_plot(data=df_all,Date = 'Date',obs='Qobs_0000579049',
              sim='Qsim_0000579049',pre='pre_Wegeleben_2004_2018',
              wqobs='INConcob0579049',wqsim='INConcsi0579049')
#
hydro_wq_plot(data=df_all,Date = 'Date',obs='Qobs_0000579745',
              sim='Qsim_0000579745',pre='pre_Wegeleben_2004_2018',
              wqobs='INConcob0579745',wqsim='INConcsi0579745')
#
hydro_wq_plot(data=df_all,Date = 'Date',obs='Qobs_0000579070',
              sim='Qsim_0000579070',pre='pre_Hadmers_2004_2018',
              wqobs='INConcob0579070',wqsim='INConcsi0579070')
#
hydro_wq_plot(data=df_all,Date = 'Date',obs='Qobs_0000579085',
              sim='Qsim_0000579085',pre='pre_stassfurt_2004_2018',
              wqobs='INConcob0579085',wqsim='INConcsi0579085')



# # ggplot rainfall runoff
# !!!!!!!!!!!!!!!!
# problem in secondary axis scale need to solved later
# !!!!!!!!!!!!!!!!
# # Calculate the range needed to avoid having your hyetograph and hydrograph overlap
#
# maxRange <- 1.1*(max(df$pre_Meisdorf_2004_2018) + max(df$Qobs_0000579610,df$Qsim_0000579610 ))
#
#    df %>%
#   ggplot(aes(x=Date))+
#     geom_line( aes(y=Qobs_0000579610, color='black') ) +
#     geom_line( aes(y=Qsim_0000579610, color='red') )    +
#      # Use geom_tile to create the inverted hyetograph. geom_tile has a bug that displays a warning message for height and width, you can ignore it.
#      geom_col(aes(y = -1*pre_Meisdorf_2004_2018 ) )+ # y = the center point of each bar
#      scale_y_continuous(
#                         sec.axis = sec_axis( trans= ~ -1*.,
#                                            name = 'Precipitation (mm/d)',
#
#                                            #breaks = c(0,50,100),
#                                            #labels = c('0','50','100')
#                                            ))
#
#    +
#     scale_x_date(date_breaks = "2 years",
#                    date_minor_breaks = "1 year",
#                    date_labels = "%m-%Y") +
#     theme_bw()
#
#
#    +
#     theme(
#       axis.text.x = element_text(
#         size = 10,
#         hjust = 0.5,
#         colour = 'black'
#       ),
#       axis.text.y = element_text(size = 10, colour = 'black'),
#       axis.title.y = element_text(size = 15, colour = 'black'),
#       panel.grid.major = element_blank(),
#       panel.grid.minor = element_blank(),
#       legend.position = c(0.98, 0.99),
#       legend.direction = "horizontal",
#       legend.justification = c('right', 'top'),
#       legend.text = element_text(size = 15, colour = 'black')
#     )


