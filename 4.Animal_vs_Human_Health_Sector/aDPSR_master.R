######################################################################################
###########       ANIMAL Driver, Pressure, State & Response Figures

### Packages
library(reshape2);library(tidyverse); library(plyr); library(gridExtra); library(grid); 
library(tmap); library(tmaptools); library(here)


### Import data
load(here("0.data", "Animal_Data.RData"))
### Data preparation & Plotting

## DRIVERS
#setwd(script.wd)
source(here("4.Animal_vs_Human_Health_Sector", "aDriver_dataprep.R"), encoding = "latin1")
#source("aDriver_plots.R")

## PRESSURE
source(here("4.Animal_vs_Human_Health_Sector", "aPRESSURE_dataprep.R"))
#source("aPRESSURE_plots.R")

## STATE
source(here("4.Animal_vs_Human_Health_Sector", "aSTATE_dataprep.R"))
#source("aSTATE_plots.R")


## D, R & S combined 
#setwd(script.wd)
source(here("4.Animal_vs_Human_Health_Sector", "aFigures_merge.R"))
#source("aFigures_plot.R")

## RESPONSE
#setwd(script.wd)
source(here("4.Animal_vs_Human_Health_Sector", "aRESPONSE_dataprep.R"))

## FIG5 script
#plot.bin <- "yes"
source(here("4.Animal_vs_Human_Health_Sector", "aProduction_vs_response_1.r"))



