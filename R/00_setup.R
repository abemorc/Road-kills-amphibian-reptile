
# Road kills
# Instalacion/carga librerias  --------------------------------------------


# Instalacion librerias faltantes -----------------------------------------

oldw <- getOption("warn")
options(warn = -1)

packages <- c("readxl", "tidyverse", "lubridate", "tidytable", "psych",
              "here", "visreg", "Hmisc", "broom", "MuMIn",
              "PerformanceAnalytics", "modeest", "caret", "randomForest")

installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages])
}

options(warn = oldw)

options(scipen = 999)
# options(scipen=0, digits=7)

# carga de librerias usadas en proyecto road kills -------------------------

###
library(here)
library(readxl)
library(tidyverse)
library(lubridate)
library(modeest)
library(PerformanceAnalytics)
library(psych)
library(caret)
library(MuMIn)
library(visreg)
library(tidytable)
library(broom)
library(Hmisc)
library(randomForest)

load(here("02.2022.RData"))


