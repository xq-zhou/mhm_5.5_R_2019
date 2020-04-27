# boxplot with -test p value


library(tidyverse)
library(lubridate)
library(xts)
library(hydroGOF)
library(ncdf4)

# https://www.r-bloggers.com/add-p-values-and-significance-levels-to-ggplots/


# simulated runoff component boxplot -----------------------------------------------------------

ncpath <- "./data/"
ncname <- "mHM_01_18"
ncfname <- paste(ncpath, ncname, ".nc", sep = "")
ncin <- nc_open(ncfname)
print(ncin)
Qfast <- ncvar_get(ncin, "QIf")
Qslow <- ncvar_get(ncin, "QIs")
Qbase <- ncvar_get(ncin, "QB")
Qtotal <- ncvar_get(ncin, "Q")
nc_close(ncin)


# runoff component in in 2004-2018
Qfast_Meisdorf_2004_2018 <- Qfast[50, 61, 1:216]

Qslow_Meisdorf_2004_2018 <- Qslow[50, 61, 1462:6940]
Qbase_Meisdorf_2004_2018 <- Qbase[50, 61, 1462:6940]
Qtotal_Meisdorf_2004_2018 <- Qtotal[50, 61, 1462:6940]
#
Q_Hausneidorf_2004_2018 <- Q[49, 45, 1462:6940]
#
Q_Wegeleben_2004_2018 <- Q[44, 39, 1462:6940]
#
Q_Nienhagen_2004_2018 <- Q[40, 34, 1462:6940]
#
Q_Oschers_2004_2018 <- Q[48, 24, 1462:6940]
#
Q_Hadmers_2004_2018 <- Q[53, 26, 1462:6940]
#
Q_stassfurt_2004_2018 <- Q[71, 43, 1462:6940]
