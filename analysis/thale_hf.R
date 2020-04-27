# thale wq high frequency data correction

library(tidyverse)
install.packages('TTR')
library("TTR")
library(xts)

thale_hf <- read_csv('./data/Rapbode_smoothdata.csv')

colnames(thale_hf) <- c('Date','Value')

p <- thale_hf %>% filter(Value>0.7 & Value<2.5) %>% na.omit()
p %>%
  ggplot(aes(Date,Value)) +
  geom_line()
# moving avarage time series to remove high frequency noise
p_sm <- SMA( xts(p$Value, order.by = p$Date), n= 24*60/10)
#
plot(p_sm)

thale_daily <- apply.daily(p_sm,median,na.rm=T)
plot(thale_daily)
thale_daily <- na.omit(thale_daily)

thale_daily_df <- data.frame(Date=index(thale_daily),No3=coredata(thale_daily))

colnames(thale_daily_df) <- c('Date','No3')
thale_daily_df$Date <- as.Date(thale_daily_df$Date)

write_csv(thale_daily_df,'./data/thale_daily_df.csv')

View(thale_daily_df)
#

# writetext ---------------------------------------------------------------

#creat new time series
t_tofill<- seq(as.Date("2011-06-18"), as.Date("2014-11-17"), by="day")
#convert time series to datafame with the same colname 'Date'
t<-data.frame(list(Date=t_tofill))

#merge two time series by colname

No3_df<- merge(t, thale_daily_df,by='Date',all=T)

#fill NA
No3_df <- No3_df %>% fill(No3, .direction = 'up')

# anyNA(No3_df)
# write out to text file
lines<- 'nodata	-9999
n	1	measurements	per	day	[1,	1440]
start	2011	1	1	0	0	(YYYY	MM	DD	HH	MM)
end	2014	12	31	0	0	(YYYY	MM	DD	HH	MM)
YYYY	MM	DD	HH	NN	IN'
write(lines,"wq_579020_2011_2014.txt")

# create new time series
df<- data.frame(date=as.Date(t_tofill),year = as.numeric(format(t_tofill, format = "%Y")),
                month = as.numeric(format(t_tofill, format = "%m")),
                day = as.numeric(format(t_tofill, format = "%d")),
                hour=rep(0,length(t_tofill)),min=rep(0,length(t_tofill)))
# combine two dataframe
data_out<- data.frame(df[,-1],No3_df[,2])

#write out the filled time series
write.table(data_out,"wq_579020_2011_2014.txt",sep="\t",row.names = F,col.names = F,append = T)



# compred to interpolation method -----------------------------------------


con_thale <- read_csv('./data/thale_daily_df.csv')
colnames(con_thale) <- c('Date','No3')
con_thale$Date <- as.Date(con_thale$Date)

 var(con_thale$No3)

con_thale %>%
  ggplot(aes(Date,No3))+
  geom_point()+
  geom_smooth()

# compared to interpolated using Q

wq_thale <- read_table2('D:/BODE_data/Water quality data/LHW/wq_579020.txt',skip = 4)
colnames(wq_thale) <- c('Year',"Mon",'Day','HH','NN','No3')
wq_thale_dt <- wq_thale%>% dplyr:: select('Year',"Mon",'Day','No3') %>%
  unite('date','Year',"Mon",'Day',sep = '-') %>%
  mutate(date=lubridate::ymd(date) )
wq_thale_dt <- wq_thale_dt %>% filter(date %in% con_thale$Date)

plot(con_thale$No3,wq_thale_dt$No3)
abline(a=0,b=1)


#
Q_thale <- read_table2('D:/BODE_data/Water quality data/LHW/Q579020.txt',skip = 5)
colnames(Q_thale) <- c('Year',"Mon",'Day','HH','MM',"Q")

Q_thale_dt <- Q_thale %>% dplyr:: select('Year',"Mon",'Day',"Q") %>%
  unite('date','Year',"Mon",'Day',sep = '-') %>%
  mutate(date=lubridate::ymd(date) )

var(Q_thale_dt$Q)
summary(Q_thale_dt)
# select discharge in period 2007-2015
Q_thale_dt_sel <- Q_thale_dt %>% filter(date %in% con_thale$Date)
# to analysis of the relationship between discharge and concentration
c_Q <- data.frame(q=Q_thale_dt_sel$Q,conc=con_thale$No3)

# C=aQ^b
# to append the regression line equation
power_eqn = function(df, start = list(a =2/2.2,b=1)){
  m = nls(conc ~ a*q^b, start = start, data = df);
  eq <- substitute(italic(y) == a  ~italic(x)^b,
                   list(a = format(coef(m)[1], digits = 2),
                        b = format(coef(m)[2], digits = 2)))
  as.character(as.expression(eq));
}


ggplot(c_Q,aes(x=q,y=conc))+
  geom_point()+
  stat_smooth(method = 'nls',
              method.args = list(formula = 'y~a*x^b',start=c(a = 2/2.2,b=1)),
              se=FALSE)+
  geom_text(x=20,y=2,label = power_eqn(c_Q), parse = TRUE)


