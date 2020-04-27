# station concentration boxplot
# https://www.r-bloggers.com/add-p-values-and-significance-levels-to-ggplots/

library(tidyverse)
library(lubridate)
library(ggpubr)

# Silb----

wq_silb <- read_table2('./data/wq_579605.txt',skip = 5)
colnames(wq_silb) <- c('Year','Mon', 'Day','HH','MM','IN')
wq_silb <- unite(wq_silb, 'dates', c('Year', 'Mon', 'Day')) %>% mutate(dates = ymd(dates)) %>% select(-c('HH','MM'))
wq_silb[wq_silb == -9999] <- NA

plot(wq_silb$IN)

# meis 2001-2014
wq_silb_2001_14 <-
  wq_silb %>% filter(between(dates, as.Date('2001-01-01'), as.Date('2014-12-31')))

# meis 2015-2018
wq_silb_2015_18 <-
  wq_silb %>% filter(between(dates, as.Date('2015-01-01'), as.Date('2018-12-31')))


# meis----
wq_meis <- read_table2('./data/wq_579610.txt',skip = 5)
colnames(wq_meis) <- c('Year','Mon', 'Day','HH','MM','IN')
wq_meis <- unite(wq_meis, 'dates', c('Year', 'Mon', 'Day')) %>% mutate(dates = ymd(dates)) %>% select(-c('HH','MM'))
wq_meis[wq_meis == -9999] <- NA

wq_meis <-wq_meis %>% filter(between(dates,as.Date('2000-01-01'),as.Date('2018-12-31')))
# meis 2001-2008
wq_meis_2001_08 <- wq_meis %>% filter(between(dates,as.Date('2001-01-01'),as.Date('2008-12-31')))

# meis 2011-2018
wq_meis_2011_18 <- wq_meis %>% filter(between(dates,as.Date('2011-01-01'),as.Date('2018-12-31')))

# combine three periods

wq_meis_df <-  bind_rows(wq_meis_2001_08,wq_meis_2011_18)  %>% mutate(Period=c(    rep('Period_2001_2008',2922),
                                                                                   rep('Period_2011_2018',2922)))
wq_meis_df %>% ggplot(aes( x=factor(month(dates),levels=c("10","11","12","1","2","3","4","5","6","7","8","9")),
                           y=IN,
                           fill= Period )) +
  geom_boxplot(outlier.shape = NA) +
  scale_fill_discrete(name = 'Period', labels = c('2001-2008', '2011-2018')) +
  labs(x = "Month", y =' NO3 (mg/L)')+
  theme_bw()+
  ggtitle('Meisdorf')+
  stat_compare_means(aes(group=Period),
                     hide.ns=T,
                     label = 'p.signif') # *: p <= 0.05,**: p <= 0.01,***: p <= 0.001,****: p <= 0.0001

# haus----
wq_haus <- read_table2('./data/wq_579620.txt',skip = 5)
colnames(wq_haus) <- c('Year','Mon', 'Day','HH','MM','IN')
wq_haus <- unite(wq_haus, 'dates', c('Year', 'Mon', 'Day')) %>% mutate(dates = ymd(dates)) %>% select(-c('HH','MM'))
wq_haus[wq_haus == -9999] <- NA

wq_haus <-wq_haus %>% filter(between(dates,as.Date('2000-01-01'),as.Date('2018-12-31')))
# haus 2001_2008
wq_haus_2001_08 <- wq_haus %>% filter(between(dates,as.Date('2001-01-01'),as.Date('2008-12-31')))

# haus 2011-2018
wq_haus_2011_18 <- wq_haus %>% filter(between(dates,as.Date('2011-01-01'),as.Date('2018-12-31')))

# combine three periods

wq_haus_df <-  bind_rows(wq_haus_2001_08,wq_haus_2011_18)  %>% mutate(Period=c(   rep('Period_2001_2008',2922),
                                                                                  rep('Period_2011_2018',2922)))
wq_haus_df %>% ggplot(aes( x=factor(month(dates),levels=c("10","11","12","1","2","3","4","5","6","7","8","9")),
                           y=IN,
                           fill= Period )) +
  geom_boxplot(outlier.shape = NA) +
  scale_fill_discrete(name = 'Period', labels = c('2001-2008', '2011-2018')) +
  labs(x = "Month", y =' NO3 (mg/L)')+
  theme_bw()+
  ggtitle('Hausneidorf')+
  stat_compare_means(aes(group=Period),
                     hide.ns=T,
                     label = 'p.signif')

# wege----
wq_wege <- read_table2('./data/wq_579049.txt',skip = 5)

colnames(wq_wege) <- c('Year','Mon', 'Day','HH','MM','IN')
wq_wege <- unite(wq_wege, 'dates', c('Year', 'Mon', 'Day')) %>% mutate(dates = ymd(dates)) %>% select(-c('HH','MM'))
wq_wege[wq_wege == -9999] <- NA
wq_wege <-wq_wege %>% filter(between(dates,as.Date('2000-01-01'),as.Date('2018-12-31')))

# 2001-2008
wq_wege_2001_08 <- wq_wege %>% filter(between(dates,as.Date('2001-01-01'),as.Date('2008-12-31')))
# 2011-2018
wq_wege_2011_18 <- wq_wege %>% filter(between(dates,as.Date('2011-01-01'),as.Date('2018-12-31')))
#
wq_wege_df <-  bind_rows(wq_wege_2001_08,wq_wege_2011_18)  %>% mutate(Period=c(    rep('Period_2001_2008',2922),
                                                                                      rep('Period_2011_2018',2922)))
wq_wege_df %>% ggplot(aes( x=factor(month(dates),levels=c("10","11","12","1","2","3","4","5","6","7","8","9")),
                            y=IN,
                            fill= Period )) +
  geom_boxplot(outlier.shape = NA) +
  scale_fill_discrete(name = 'Period', labels = c('2001-2008', '2011-2018')) +
  labs(x = "Month", y =' NO3 (mg/L)')+
  theme_bw()+
  ggtitle('Wegeleben')+
  stat_compare_means(aes(group=Period),
                     hide.ns=T,
                     label = 'p.signif')

# neinhage----
wq_nien <- read_table2('./data/wq_579745.txt',skip = 5)
colnames(wq_nien) <- c('Year','Mon', 'Day','HH','MM','IN')
wq_nien <- unite(wq_nien, 'dates', c('Year', 'Mon', 'Day')) %>% mutate(dates = ymd(dates)) %>% select(-c('HH','MM'))
wq_nien[wq_nien == -9999] <- NA

wq_nien <- wq_nien %>% filter(between(dates,as.Date('2000-01-01'),as.Date('2018-12-31'))) %>%
  distinct() %>% slice(-2314,-2427,-2483,-2542) #remove duplicated rows


# 2001-2008
wq_nien_2001_08 <- wq_nien %>% filter(between(dates,as.Date('2001-01-01'),as.Date('2008-12-31'))) %>% distinct()

# 2011-2018
wq_nien_2011_18 <- wq_nien %>% filter(between(dates,as.Date('2011-01-01'),as.Date('2018-12-31')))
#
wq_nien_df <-  bind_rows(wq_nien_2001_08, wq_nien_2011_18)  %>%
  mutate(Period = c(rep('Period_2001_2008', 2922),
                    rep('Period_2011_2018', 2922)))
wq_nien_df %>% ggplot(aes( x=factor(month(dates),levels=c("10","11","12","1","2","3","4","5","6","7","8","9")),
                            y=IN,
                            fill= Period )) +
  geom_boxplot(outlier.shape = NA) +
  scale_fill_discrete(name = 'Period', labels = c('2001-2008', '2011-2018')) +
  labs(x = "Month", y =' NO3 (mg/L)')+
  theme_bw()+
  ggtitle('Nienhagen')+
  stat_compare_means(aes(group=Period),
                     hide.ns=T,
                     label = 'p.signif')


# hadmers----
wq_had <- read_table2('./data/wq_579070.txt',skip = 5)
colnames(wq_had) <- c('Year','Mon', 'Day','HH','MM','IN')
wq_had <- unite(wq_had, 'dates', c('Year', 'Mon', 'Day')) %>% mutate(dates = ymd(dates)) %>% select(-c('HH','MM'))
wq_had[wq_had == -9999] <- NA

wq_had <-wq_had %>% filter(between(dates,as.Date('2000-01-01'),as.Date('2018-12-31')))
#
# 2001-2008
wq_had_2001_08 <- wq_had %>% filter(between(dates,as.Date('2001-01-01'),as.Date('2008-12-31')))
# 2011-2018
wq_had_2011_18 <- wq_had %>% filter(between(dates,as.Date('2011-01-01'),as.Date('2018-12-31')))
#
wq_had_df <-  bind_rows(wq_had_2001_08,wq_had_2011_18)  %>% mutate(Period=c(    rep('Period_2001_2008',2922),
                                                                                      rep('Period_2011_2018',2922)))
wq_had_df %>% ggplot(aes( x=factor(month(dates),levels=c("10","11","12","1","2","3","4","5","6","7","8","9")),
                            y=IN,
                            fill= Period )) +
  geom_boxplot(outlier.shape = NA) +
  scale_fill_discrete(name = 'Period', labels = c('2001-2008', '2011-2018')) +
  labs(x = "Month", y =' NO3 (mg/L)')+
  theme_bw()+
  ggtitle('Hadmersleben')+
  stat_compare_means(aes(group=Period),
                     hide.ns=T,
                     label = 'p.signif')



# stass-----
wq_stass <- read_table2('./data/wq_579085.txt',skip = 5)
colnames(wq_stass) <- c('Year','Mon', 'Day','HH','MM','IN')
wq_stass <- unite(wq_stass, 'dates', c('Year', 'Mon', 'Day')) %>% mutate(dates = ymd(dates)) %>% select(-c('HH','MM'))
wq_stass[wq_stass == -9999] <- NA

wq_stass <-wq_stass %>% filter(between(dates,as.Date('2000-01-01'),as.Date('2018-12-31')))
# 2001-2008
wq_stass_2001_08 <- wq_stass %>% filter(between(dates,as.Date('2001-01-01'),as.Date('2008-12-31')))
# 2011-2018
wq_stass_2011_18 <- wq_stass %>% filter(between(dates,as.Date('2011-01-01'),as.Date('2018-12-31')))
#
wq_stass_df <-  bind_rows(wq_stass_2001_08,wq_stass_2011_18)  %>% mutate(Period=c(    rep('Period_2001_2008',2922),
                                                                                   rep('Period_2011_2018',2922)))
wq_stass_df %>% ggplot(aes( x=factor(month(dates),levels=c("10","11","12","1","2","3","4","5","6","7","8","9")),
                           y=IN,
                           fill= Period )) +
  geom_boxplot(outlier.shape = NA) +
  scale_fill_discrete(name = 'Period', labels = c('2001-2008', '2011-2018')) +
  labs(x = "Month", y =' NO3 (mg/L)')+
  theme_bw()+
  ggtitle('Stassfurt')+
  stat_compare_means(aes(group=Period),
                     hide.ns=T,
                     label = 'p.signif')


# change the reference period ---------------------------------------------



# meis----
wq_meis <- read_table2('./data/wq_579610.txt', skip = 5)
colnames(wq_meis) <- c('Year', 'Mon', 'Day', 'HH', 'MM', 'IN')
wq_meis <-
  unite(wq_meis, 'dates', c('Year', 'Mon', 'Day')) %>% mutate(dates = ymd(dates)) %>% select(-c('HH', 'MM'))
wq_meis[wq_meis == -9999] <- NA

wq_meis <-
  wq_meis %>% filter(between(dates, as.Date('2000-01-01'), as.Date('2018-12-31')))
# meis 2001-2014
wq_meis_2001_14 <-
  wq_meis %>% filter(between(dates, as.Date('2001-01-01'), as.Date('2014-12-31')))

# meis 2015-2018
wq_meis_2015_18 <-
  wq_meis %>% filter(between(dates, as.Date('2015-01-01'), as.Date('2018-12-31')))

# combine three periods

wq_meis_df <-
  bind_rows(wq_meis_2001_14, wq_meis_2015_18)  %>% mutate(Period = c(rep('Period_2001_2014', 5113),
                                                                     rep('Period_2015_2018', 1461)))
wq_meis_df %>% ggplot(aes(
  x = factor(
    month(dates),
    levels = c("10", "11", "12", "1", "2", "3", "4", "5", "6", "7", "8", "9")
  ),
  y = IN,
  fill = Period
)) +
  geom_boxplot(outlier.shape = NA) +
  scale_fill_discrete(name = 'Period', labels = c('2001-2014', '2015-2018')) +
  labs(x = "Month", y = ' NO3 (mg/L)') +
  theme_bw() +
  ggtitle('Meisdorf') +
  stat_compare_means(aes(group = Period),
                     hide.ns = T,
                     label = 'p.signif') # *: p <= 0.05,**: p <= 0.01,***: p <= 0.001,****: p <= 0.0001

# haus----
wq_haus <- read_table2('./data/wq_579620.txt', skip = 5)
colnames(wq_haus) <- c('Year', 'Mon', 'Day', 'HH', 'MM', 'IN')
wq_haus <-
  unite(wq_haus, 'dates', c('Year', 'Mon', 'Day')) %>% mutate(dates = ymd(dates)) %>% select(-c('HH', 'MM'))
wq_haus[wq_haus == -9999] <- NA

wq_haus <-
  wq_haus %>% filter(between(dates, as.Date('2000-01-01'), as.Date('2018-12-31')))
# haus 2001_2014
wq_haus_2001_14 <-
  wq_haus %>% filter(between(dates, as.Date('2001-01-01'), as.Date('2014-12-31')))

# haus 2015-2018
wq_haus_2015_18 <-
  wq_haus %>% filter(between(dates, as.Date('2015-01-01'), as.Date('2018-12-31')))

# combine three periods

wq_haus_df <-
  bind_rows(wq_haus_2001_14, wq_haus_2015_18)  %>% mutate(Period = c(rep('Period_2001_2014', 5113), rep('Period_2015_2018', 1461)))
wq_haus_df %>% ggplot(aes(
  x = factor(
    month(dates),
    levels = c("10", "11", "12", "1", "2", "3", "4", "5", "6", "7", "8", "9")
  ),
  y = IN,
  fill = Period
)) +
  geom_boxplot(outlier.shape = NA) +
  scale_fill_discrete(name = 'Period', labels = c('2001-2014', '2015-2018')) +
  labs(x = "Month", y = ' NO3 (mg/L)') +
  theme_bw() +
  ggtitle('Hausneidorf') +
  stat_compare_means(aes(group = Period),
                     hide.ns = T,
                     label = 'p.signif')

# wege----
wq_wege <- read_table2('./data/wq_579049.txt', skip = 5)

colnames(wq_wege) <- c('Year', 'Mon', 'Day', 'HH', 'MM', 'IN')
wq_wege <-
  unite(wq_wege, 'dates', c('Year', 'Mon', 'Day')) %>% mutate(dates = ymd(dates)) %>% select(-c('HH', 'MM'))
wq_wege[wq_wege == -9999] <- NA
wq_wege <-
  wq_wege %>% filter(between(dates, as.Date('2000-01-01'), as.Date('2018-12-31')))

# 2001-2014
wq_wege_2001_14 <-
  wq_wege %>% filter(between(dates, as.Date('2001-01-01'), as.Date('2014-12-31')))
# 2015-2018
wq_wege_2015_18 <-
  wq_wege %>% filter(between(dates, as.Date('2015-01-01'), as.Date('2018-12-31')))
#
wq_wege_df <-
  bind_rows(wq_wege_2001_14, wq_wege_2015_18)  %>% mutate(Period = c(rep('Period_2001_2014', 5113), rep('Period_2015_2018', 1461)))
wq_wege_df %>% ggplot(aes(
  x = factor(
    month(dates),
    levels = c("10", "11", "12", "1", "2", "3", "4", "5", "6", "7", "8", "9")
  ),
  y = IN,
  fill = Period
)) +
  geom_boxplot(outlier.shape = NA) +
  scale_fill_discrete(name = 'Period', labels = c('2001-2014', '2015-2018')) +
  labs(x = "Month", y = ' NO3 (mg/L)') +
  theme_bw() +
  ggtitle('Wegeleben') +
  stat_compare_means(aes(group = Period),
                     hide.ns = T,
                     label = 'p.signif')

# neinhage----
wq_nien <- read_table2('./data/wq_579745.txt', skip = 5)
colnames(wq_nien) <- c('Year', 'Mon', 'Day', 'HH', 'MM', 'IN')
wq_nien <-
  unite(wq_nien, 'dates', c('Year', 'Mon', 'Day')) %>% mutate(dates = ymd(dates)) %>% select(-c('HH', 'MM'))
wq_nien[wq_nien == -9999] <- NA

wq_nien <-
  wq_nien %>% filter(between(dates, as.Date('2000-01-01'), as.Date('2018-12-31'))) %>%
  distinct() %>% slice(-2314, -2427, -2483, -2542) #remove duplicated rows


# 2001-2014
wq_nien_2001_14 <-
  wq_nien %>% filter(between(dates, as.Date('2001-01-01'), as.Date('2014-12-31'))) %>% distinct()

# 2015-2018
wq_nien_2015_18 <-
  wq_nien %>% filter(between(dates, as.Date('2015-01-01'), as.Date('2018-12-31')))
#
wq_nien_df <-  bind_rows(wq_nien_2001_14, wq_nien_2015_18)  %>%
  mutate(Period = c(rep('Period_2001_2014', 5113), rep('Period_2015_2018', 1461)))

wq_nien_df %>% ggplot(aes(
  x = factor(
    month(dates),
    levels = c("10", "11", "12", "1", "2", "3", "4", "5", "6", "7", "8", "9")
  ),
  y = IN,
  fill = Period
)) +
  geom_boxplot(outlier.shape = NA) +
  scale_fill_discrete(name = 'Period', labels = c('2001-2014', '2015-2018')) +
  labs(x = "Month", y = ' NO3 (mg/L)') +
  theme_bw() +
  ggtitle('Nienhagen') +
  stat_compare_means(aes(group = Period),
                     hide.ns = T,
                     label = 'p.signif')

# wq_nien_df %>% mutate(mon=factor(
#   month(dates),
#   levels = c("10", "11", "12", "1", "2", "3", "4", "5", "6", "7", "8", "9")
# )) %>%
#   ggboxplot( x = "mon", y = "IN",
#           color = "Period", palette = "jco",
#           add = "jitter")

# hadmers----
wq_had <- read_table2('./data/wq_579070.txt', skip = 5)
colnames(wq_had) <- c('Year', 'Mon', 'Day', 'HH', 'MM', 'IN')
wq_had <-
  unite(wq_had, 'dates', c('Year', 'Mon', 'Day')) %>% mutate(dates = ymd(dates)) %>% select(-c('HH', 'MM'))
wq_had[wq_had == -9999] <- NA

wq_had <-
  wq_had %>% filter(between(dates, as.Date('2000-01-01'), as.Date('2018-12-31')))
#
# 2001-2014
wq_had_2001_14 <-
  wq_had %>% filter(between(dates, as.Date('2001-01-01'), as.Date('2014-12-31')))
# 2015-2018
wq_had_2015_18 <-
  wq_had %>% filter(between(dates, as.Date('2015-01-01'), as.Date('2018-12-31')))
#
wq_had_df <-
  bind_rows(wq_had_2001_14, wq_had_2015_18)  %>% mutate(Period = c(rep('Period_2001_2014', 5113), rep('Period_2015_2018', 1461)))
wq_had_df %>% ggplot(aes(
  x = factor(
    month(dates),
    levels = c("10", "11", "12", "1", "2", "3", "4", "5", "6", "7", "8", "9")
  ),
  y = IN,
  fill = Period
)) +
  geom_boxplot(outlier.shape = NA) +
  scale_fill_discrete(name = 'Period', labels = c('2001-2014', '2015-2018')) +
  labs(x = "Month", y = ' NO3 (mg/L)') +
  theme_bw() +
  ggtitle('Hadmersleben') +
  stat_compare_means(aes(group = Period),
                     hide.ns = T,
                     label = 'p.signif')



# stass-----
wq_stass <- read_table2('./data/wq_579085.txt', skip = 5)
colnames(wq_stass) <- c('Year', 'Mon', 'Day', 'HH', 'MM', 'IN')
wq_stass <-
  unite(wq_stass, 'dates', c('Year', 'Mon', 'Day')) %>% mutate(dates = ymd(dates)) %>% select(-c('HH', 'MM'))
wq_stass[wq_stass == -9999] <- NA

wq_stass <-
  wq_stass %>% filter(between(dates, as.Date('2000-01-01'), as.Date('2018-12-31')))
# 2001-2014
wq_stass_2001_14 <-
  wq_stass %>% filter(between(dates, as.Date('2001-01-01'), as.Date('2014-12-31')))
# 2015-2018
wq_stass_2015_18 <-
  wq_stass %>% filter(between(dates, as.Date('2015-01-01'), as.Date('2018-12-31')))
#
wq_stass_df <-
  bind_rows(wq_stass_2001_14, wq_stass_2015_18)  %>% mutate(Period = c(rep('Period_2001_2014', 5113), rep('Period_2015_2018', 1461)))
wq_stass_df %>% ggplot(aes(
  x = factor(
    month(dates),
    levels = c("10", "11", "12", "1", "2", "3", "4", "5", "6", "7", "8", "9")
  ),
  y = IN,
  fill = Period
)) +
  geom_boxplot(outlier.shape = NA) +
  scale_fill_discrete(name = 'Period', labels = c('2001-2014', '2015-2018')) +
  labs(x = "Month", y = ' NO3 (mg/L)') +
  theme_bw() +
  ggtitle('Stassfurt') +
  stat_compare_means(aes(group = Period),
                     hide.ns = T,
                     label = 'p.signif')

