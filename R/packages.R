#############
##
# source("https://github.com/rjwatt42/BrawStats/raw/main/packages.R")
# suppressPackageStartupMessages

list.of.packages<-c("ggplot2","grDevices",
                    "mnormt","lme4",
                    "readxl","writexl","stringr","clipr",
                    "car","pracma","abind"
)
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)>0) install.packages(new.packages,repos="http://cran.fhcrc.org")

library(ggplot2)
library(grDevices)

library(mnormt)      # pmnorm for logistic
library(lme4)        # lmer (mixed models)
library(readxl)      # excel
library(writexl)     # x excel
library(stringr)     # for str_* functions
library(clipr)       # for clipboard functions
library(car)         # Anova type 3 correct
library(pracma)      # for meshgrid & fmincon
library(abind)       # for abind
