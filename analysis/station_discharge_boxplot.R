# station discharge boxplot with significant level

library(tidyverse)
library(lubridate)
library(ggpubr)
library(naniar)


# MEis----
Q_meis <- read_table2('./data/579610.txt', skip = 5)
colnames(Q_meis) <- c('Year', 'Mon', 'Day', 'HH', 'MM', 'Q')

# 2000-2018
Q_meis <- Q_meis %>% filter(Year >= 2000 & Year <= 2018)

# 2001-2008
q_meis_2001_08 <- Q_meis %>% filter(Year >= 2001 & Year <= 2008)
q_meis_2011_18 <- Q_meis %>% filter(Year >= 2011 & Year <= 2018)

q_meis <-
  bind_rows(q_meis_2001_08, q_meis_2011_18) %>% mutate(Period = c(rep('Period_2001_2008', 2922),
                                                                  rep('Period_2011_2018', 2922)))

q_meis %>%
  ggplot(aes(
    x = factor(
      Mon,
      levels = c("10", "11", "12", "1", "2", "3", "4", "5", "6", "7", "8", "9")
    ),
    y = Q,
    fill = Period
  )) +
  geom_boxplot(outlier.shape = NA) +
  labs(x = "Month", y = "Discharge (m3/s)") +
  theme_bw() +
  scale_fill_discrete(  labels = c('2001-2008', '2011-2018')) +
  ggtitle('Meisdorf') +
  coord_cartesian(ylim = c(0,10)) +
  stat_compare_means(aes(group = Period),
                     hide.ns = T,
                     label = 'p.signif',
                     label.x = seq(1:12),
                     label.y = 10) # *: p <= 0.05,**: p <= 0.01,***: p <= 0.001,****: p <= 0.0001


# Haus----

Q_haus <- read_table2('./data/579620.txt', skip = 5)
colnames(Q_haus) <- c('Year', 'Mon', 'Day', 'HH', 'MM', 'Q')

Q_haus <- Q_haus %>% filter(Year >= 2000 & Year <= 2018)

# 2001-2008
q_haus_2001_08 <- Q_haus %>% filter(Year >= 2001 & Year <= 2008)
# 2011-2018
q_haus_2011_18 <- Q_haus %>% filter(Year >= 2011 & Year <= 2018)

q_haus <-
  bind_rows(q_haus_2001_08, q_haus_2011_18) %>% mutate(Period = c(rep('Period_2001_2008', 2922),
                                                                  rep('Period_2011_2018', 2922)))


q_haus %>%
  ggplot(aes(
    x = factor(
      Mon,
      levels = c("10", "11", "12", "1", "2", "3", "4", "5", "6", "7", "8", "9")
    ),
    y = Q,
    fill = Period
  )) +
  geom_boxplot(outlier.shape = NA) +
  labs(x = "Month", y = "Discharge (m3/s)") +
  theme_bw() +
  scale_fill_discrete(  labels = c('2001-2008', '2011-2018')) +
  ggtitle('Hausneidorf') +
  coord_cartesian(ylim = c(0,10))+
  stat_compare_means(aes(group = Period),
                     hide.ns = T,
                     label = 'p.signif',
                     label.x = seq(1:12),
                     label.y = 10) # *: p <= 0.05,**: p <= 0.01,***: p <= 0.001,****: p <= 0.0001

# wegeleben----
Q_wege <- read_table2('./data/579049.txt', skip = 5)
colnames(Q_wege) <- c('Year', 'Mon', 'Day', 'HH', 'MM', 'Q')

Q_wege <- Q_wege %>% filter(Year >= 2000 & Year <= 2018)

# 2001-2008
q_wege_2001_08 <- Q_wege %>% filter(Year >= 2001 & Year <= 2008)
# 2011-2018
q_wege_2011_18 <- Q_wege %>% filter(Year >= 2011 & Year <= 2018)

q_wege <-
  bind_rows(q_wege_2001_08, q_wege_2011_18) %>% mutate(Period = c(rep('Period_2001_2008', 2922),
                                                                  rep('Period_2011_2018', 2922)))


q_wege %>%
  ggplot(aes(
    x = factor(
      Mon,
      levels = c("10", "11", "12", "1", "2", "3", "4", "5", "6", "7", "8", "9")
    ),
    y = Q,
    fill = Period
  )) +
  geom_boxplot(outlier.shape = NA) +
  labs(x = "Month", y = "Discharge (m3/s)") +
  theme_bw() +
  scale_fill_discrete(  labels = c('2001-2008', '2011-2018')) +
  ggtitle('Wegeleben') +
  coord_cartesian(ylim = c(0,30))+
  stat_compare_means(aes(group = Period),
                     hide.ns = T,
                     label = 'p.signif',
                     label.x = seq(1:12),
                     label.y = 30) # *: p <= 0.05,**: p <= 0.01,***: p <= 0.001,****: p <= 0.0001

# nienhagen----

Q_nien <- read_table2('./data/579745.txt', skip = 5)
colnames(Q_nien) <- c('Year', 'Mon', 'Day', 'HH', 'MM', 'Q')

Q_nien <- Q_nien %>% filter(Year >= 2000 & Year <= 2018)

# 2001-2008
q_nien_2001_08 <- Q_nien %>% filter(Year >= 2001 & Year <= 2008)
# 2011-2018
q_nien_2011_18 <- Q_nien %>% filter(Year >= 2011 & Year <= 2018) %>%
                  naniar::replace_with_na(replace = list(Q = -9999))

q_nien <-
  bind_rows(q_nien_2001_08, q_nien_2011_18) %>% mutate(Period = c(rep('Period_2001_2008', 2922),
                                                                  rep('Period_2011_2018', 2922)))


q_nien %>%
  ggplot(aes(
    x = factor(
      Mon,
      levels = c("10", "11", "12", "1", "2", "3", "4", "5", "6", "7", "8", "9")
    ),
    y = Q,
    fill = Period
  )) +
  geom_boxplot(outlier.shape = NA) +
  labs(x = "Month", y = "Discharge (m3/s)") +
  theme_bw() +
  scale_fill_discrete(  labels = c('2001-2008', '2011-2018')) +
  ggtitle('Nienhagen') +
  coord_cartesian(ylim = c(0,10))+
  stat_compare_means(aes(group = Period),
                     hide.ns = T,
                     label = 'p.signif',
                     label.x = seq(1:12),
                     label.y = 10) # *: p <= 0.05,**: p <= 0.01,***: p <= 0.001,****: p <= 0.0001

# Hadmersleben----
Q_had <- read_table2('./data/579070.txt', skip = 5)
colnames(Q_had) <- c('Year', 'Mon', 'Day', 'HH', 'MM', 'Q')

Q_had <- Q_had %>% filter(Year >= 2000 & Year <= 2018)

# 2001-2008
q_had_2001_08 <- Q_had %>% filter(Year >= 2001 & Year <= 2008)
# 2011-2018
q_had_2011_18 <- Q_had %>% filter(Year >= 2011 & Year <= 2018)

q_had <-
  bind_rows(q_had_2001_08, q_had_2011_18) %>% mutate(Period = c(rep('Period_2001_2008', 2922),
                                                                  rep('Period_2011_2018', 2922)))


q_had %>%
  ggplot(aes(
    x = factor(
      Mon,
      levels = c("10", "11", "12", "1", "2", "3", "4", "5", "6", "7", "8", "9")
    ),
    y = Q,
    fill = Period
  )) +
  geom_boxplot(outlier.shape = NA) +

  labs(x = "Month", y = "Discharge (m3/s)") +
  theme_bw() +
  scale_fill_discrete(  labels = c('2001-2008', '2011-2018')) +
  ggtitle('Hadmersleben') +
  coord_cartesian(ylim = c(0,50))+
  stat_compare_means(aes(group = Period),
                     hide.ns = T,
                     label = 'p.signif',
                     label.x = seq(1:12),
                     label.y = 50) # *: p <= 0.05,**: p <= 0.01,***: p <= 0.001,****: p <= 0.0001

# stassfurt----

Q_stass <- read_table2('./data/579085.txt', skip = 5)
colnames(Q_stass) <- c('Year', 'Mon', 'Day', 'HH', 'MM', 'Q')

Q_stass <- Q_stass %>% filter(Year >= 2000 & Year <= 2018)

# 2001-2008
q_stass_2001_08 <- Q_stass %>% filter(Year >= 2001 & Year <= 2008)
# 2011-2018
q_stass_2011_18 <- Q_stass %>% filter(Year >= 2011 & Year <= 2018)

q_stass <-
  bind_rows(q_stass_2001_08, q_stass_2011_18) %>% mutate(Period = c(rep('Period_2001_2008', 2922),
                                                                  rep('Period_2011_2018', 2922)))


q_stass %>%
  ggplot(aes(
    x = factor(
      Mon,
      levels = c("10", "11", "12", "1", "2", "3", "4", "5", "6", "7", "8", "9")
    ),
    y = Q,
    fill = Period
  )) +
  geom_boxplot(outlier.shape = NA) +

  labs(x = "Month", y = "Discharge (m3/s)") +
  theme_bw() +
  scale_fill_discrete(  labels = c('2001-2008', '2011-2018')) +
  ggtitle('Stassfurt') +
  coord_cartesian(ylim = c(0,50))+
  stat_compare_means(aes(group = Period),
                     hide.ns = T,
                     label = 'p.signif',
                     label.x = seq(1:12),
                     label.y = 50) # *: p <= 0.05,**: p <= 0.01,***: p <= 0.001,****: p <= 0.0001


# reference period 2001-2014 ----------------------------------------------


# meis----
Q_meis <- read_table2('./data/579610.txt', skip = 5)
colnames(Q_meis) <- c('Year', 'Mon', 'Day', 'HH', 'MM', 'Q')

# 2000-2018
Q_meis <- Q_meis %>% filter(Year >= 2000 & Year <= 2018)

# 2001-2008
q_meis_2001_14 <- Q_meis %>% filter(Year >= 2001 & Year <= 2014)
q_meis_2015_18 <- Q_meis %>% filter(Year >= 2015 & Year <= 2018)

q_meis <-
  bind_rows(q_meis_2001_14, q_meis_2015_18) %>% mutate(Period = c(rep('Period_2001_2014', 5113),
                                                                  rep('Period_2015_2018', 1461)))

q_meis %>%
  ggplot(aes(
    x = factor(
      Mon,
      levels = c("10", "11", "12", "1", "2", "3", "4", "5", "6", "7", "8", "9")
    ),
    y = Q,
    fill = Period
  )) +
  geom_boxplot(outlier.shape = NA) +
  labs(x = "Month", y = "Discharge (m3/s)") +
  theme_bw() +
  scale_fill_discrete(  labels = c('2001-2014', '2015-2018')) +
  ggtitle('Meisdorf') +
  coord_cartesian(ylim = c(0,10)) +
  stat_compare_means(aes(group = Period),
                     hide.ns = T,
                     label = 'p.signif',
                     label.x = seq(1:12),
                     label.y = 10) # *: p <= 0.05,**: p <= 0.01,***: p <= 0.001,****: p <= 0.0001


# Haus----

Q_haus <- read_table2('./data/579620.txt', skip = 5)
colnames(Q_haus) <- c('Year', 'Mon', 'Day', 'HH', 'MM', 'Q')

Q_haus <- Q_haus %>% filter(Year >= 2000 & Year <= 2018)

# 2001-2014
q_haus_2001_14 <- Q_haus %>% filter(Year >= 2001 & Year <= 2014)
# 2015-2018
q_haus_2015_18 <- Q_haus %>% filter(Year >= 2015 & Year <= 2018)

q_haus <-
  bind_rows(q_haus_2001_14, q_haus_2015_18) %>% mutate(Period = c(rep('Period_2001_2014', 5113),
                                                                  rep('Period_2015_2018', 1461)))


q_haus %>%
  ggplot(aes(
    x = factor(
      Mon,
      levels = c("10", "11", "12", "1", "2", "3", "4", "5", "6", "7", "8", "9")
    ),
    y = Q,
    fill = Period
  )) +
  geom_boxplot(outlier.shape = NA) +
  labs(x = "Month", y = "Discharge (m3/s)") +
  theme_bw() +
  scale_fill_discrete(  labels = c('2001-2014', '2015-2018')) +
  ggtitle('Hausneidorf') +
  coord_cartesian(ylim = c(0,10))+
  stat_compare_means(aes(group = Period),
                     hide.ns = T,
                     label = 'p.signif',
                     label.x = seq(1:12),
                     label.y = 10) # *: p <= 0.05,**: p <= 0.01,***: p <= 0.001,****: p <= 0.0001

# wegeleben----
Q_wege <- read_table2('./data/579049.txt', skip = 5)
colnames(Q_wege) <- c('Year', 'Mon', 'Day', 'HH', 'MM', 'Q')

Q_wege <- Q_wege %>% filter(Year >= 2000 & Year <= 2018)

# 2001-2014
q_wege_2001_14 <- Q_wege %>% filter(Year >= 2001 & Year <= 2014)
# 2015-2018
q_wege_2015_18 <- Q_wege %>% filter(Year >= 2015 & Year <= 2018)

q_wege <-
  bind_rows(q_wege_2001_14, q_wege_2015_18) %>% mutate(Period = c(rep('Period_2001_2014', 5113),
                                                                  rep('Period_2015_2018', 1461)))


q_wege %>%
  ggplot(aes(
    x = factor(
      Mon,
      levels = c("10", "11", "12", "1", "2", "3", "4", "5", "6", "7", "8", "9")
    ),
    y = Q,
    fill = Period
  )) +
  geom_boxplot(outlier.shape = NA) +
  labs(x = "Month", y = "Discharge (m3/s)") +
  theme_bw() +
  scale_fill_discrete(  labels = c('2001-2014', '2015-2018')) +
  ggtitle('Wegeleben') +
  coord_cartesian(ylim = c(0,40))+
  stat_compare_means(aes(group = Period),
                     hide.ns = T,
                     label = 'p.signif',
                     label.x = seq(1:12),
                     label.y = 40) # *: p <= 0.05,**: p <= 0.01,***: p <= 0.001,****: p <= 0.0001

# nienhagen----

Q_nien <- read_table2('./data/579745.txt', skip = 5)
colnames(Q_nien) <- c('Year', 'Mon', 'Day', 'HH', 'MM', 'Q')

Q_nien <- Q_nien %>% filter(Year >= 2000 & Year <= 2018)

# 2001-2014
q_nien_2001_14 <- Q_nien %>% filter(Year >= 2001 & Year <= 2014)
# 2015-2018
q_nien_2015_18 <- Q_nien %>% filter(Year >= 2015 & Year <= 2018) %>%
  naniar::replace_with_na(replace = list(Q = -9999))

q_nien <-
  bind_rows(q_nien_2001_14, q_nien_2015_18) %>% mutate(Period = c(rep('Period_2001_2014', 5113),
                                                                  rep('Period_2015_2018', 1461)))


q_nien %>%
  ggplot(aes(
    x = factor(
      Mon,
      levels = c("10", "11", "12", "1", "2", "3", "4", "5", "6", "7", "8", "9")
    ),
    y = Q,
    fill = Period
  )) +
  geom_boxplot(outlier.shape = NA) +
  labs(x = "Month", y = "Discharge (m3/s)") +
  theme_bw() +
  scale_fill_discrete(  labels = c('2001-2014', '2015-2018')) +
  ggtitle('Nienhagen') +
  coord_cartesian(ylim = c(0,10))+
  stat_compare_means(aes(group = Period),
                     hide.ns = T,
                     label = 'p.signif',
                     label.x = seq(1:12),
                     label.y = 10) # *: p <= 0.05,**: p <= 0.01,***: p <= 0.001,****: p <= 0.0001

# Hadmersleben----
Q_had <- read_table2('./data/579070.txt', skip = 5)
colnames(Q_had) <- c('Year', 'Mon', 'Day', 'HH', 'MM', 'Q')

Q_had <- Q_had %>% filter(Year >= 2000 & Year <= 2018)

# 2001-2014
q_had_2001_14 <- Q_had %>% filter(Year >= 2001 & Year <= 2014)
# 2015-2018
q_had_2015_18 <- Q_had %>% filter(Year >= 2015 & Year <= 2018)

q_had <-
  bind_rows(q_had_2001_14, q_had_2015_18) %>% mutate(Period = c(rep('Period_2001_2014', 5113),
                                                                rep('Period_2015_2018', 1461)))


q_had %>%
  ggplot(aes(
    x = factor(
      Mon,
      levels = c("10", "11", "12", "1", "2", "3", "4", "5", "6", "7", "8", "9")
    ),
    y = Q,
    fill = Period
  )) +
  geom_boxplot(outlier.shape = NA) +

  labs(x = "Month", y = "Discharge (m3/s)") +
  theme_bw() +
  scale_fill_discrete(  labels = c('2001-2014', '2015-2018')) +
  ggtitle('Hadmersleben') +
  coord_cartesian(ylim = c(0,60))+
  stat_compare_means(aes(group = Period),
                     hide.ns = T,
                     label = 'p.signif',
                     label.x = seq(1:12),
                     label.y = 60) # *: p <= 0.05,**: p <= 0.01,***: p <= 0.001,****: p <= 0.0001

# stassfurt----

Q_stass <- read_table2('./data/579085.txt', skip = 5)
colnames(Q_stass) <- c('Year', 'Mon', 'Day', 'HH', 'MM', 'Q')

Q_stass <- Q_stass %>% filter(Year >= 2000 & Year <= 2018)

# 2001-2014
q_stass_2001_14 <- Q_stass %>% filter(Year >= 2001 & Year <= 2014)
# 2015-2018
q_stass_2015_18 <- Q_stass %>% filter(Year >= 2015 & Year <= 2018)

q_stass <-
  bind_rows(q_stass_2001_14, q_stass_2015_18) %>% mutate(Period = c(rep('Period_2001_2014', 5113),
                                                                    rep('Period_2015_2018', 1461)))


q_stass %>%
  ggplot(aes(
    x = factor(
      Mon,
      levels = c("10", "11", "12", "1", "2", "3", "4", "5", "6", "7", "8", "9")
    ),
    y = Q,
    fill = Period
  )) +
  geom_boxplot(outlier.shape = NA) +

  labs(x = "Month", y = "Discharge (m3/s)") +
  theme_bw() +
  scale_fill_discrete(  labels = c('(2001-2014)', '(2015-2018)')) +
  ggtitle('Stassfurt') +
  coord_cartesian(ylim = c(0,50))+
  stat_compare_means(aes(group = Period),
                     hide.ns = T,
                     label = 'p.signif',
                     label.x = seq(1:12),
                     label.y = 50) # *: p <= 0.05,**: p <= 0.01,***: p <= 0.001,****: p <= 0.0001


