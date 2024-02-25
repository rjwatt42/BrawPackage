#############
##
# source("https://github.com/rjwatt42/BrawStats/raw/main/packages.R")
# suppressPackageStartupMessages

list.of.packages<-c("ggplot2","tidyr",
                    "mnormt","lme4","MuMIn",
                    "readxl","writexl","car","stringi","stringr","clipr",
                    "SuppDists","e1071","pracma",
                    "NlcOptim"
)
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)>0) install.packages(new.packages,repos="http://cran.fhcrc.org")

library(ggplot2)
library(tidyr)

# library(gridExtra)   # not sure any longer
library(mnormt)      # pmnorm for logistic
library(lme4)        # lmer (mixed models)
library(MuMIn)       # r-squared for mixed models
library(readxl)      # excel
library(writexl)     # x excel
library(car)         # Anova type 3 correct
library(stringr)     # for str_* functions
library(clipr)       # for clipboard functions
# library(SuppDists)   # for Johnson distributions
library(e1071)       # for skewness and kurtosis
library(pracma)      # for meshgrid & fmincon
library(NlcOptim)    # for fmincon
