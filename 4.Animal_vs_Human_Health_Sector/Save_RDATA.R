# SAVE RDATA
######################################################################################
###########       ANIMAL Driver, Pressure, State & Response Figures


### Working directories

parent.wd<-"C:/Users/egepeh12/OneDrive - Kungl. Vetenskapsakademien/Desktop/AMRTrends_code/"
script.wd <- paste( parent.wd, "//4.Animal vs Human Health Sector/", sep = "" )
data.wd <- paste( parent.wd, "//0.data", sep = "")
data1.wd <- paste( parent.wd, "//0.data//4.4.Animal_vs_Human_Health_Sector_data", sep = "")
plot.wd <- paste( parent.wd, "//plots/", sep = "" )

### Import data
setwd(data.wd)
region.df<-read.csv2("region.csv",header=T,stringsAsFactors = FALSE) # region key
# ISO Codes
key.df <- read.csv("Key_Incl_FAOandOIE.csv", header = T, stringsAsFactors = FALSE)

setwd(data1.wd)
# Historical animal disease data
ahs.df <- read.csv2("AHS.csv", stringsAsFactors = FALSE, na.strings = c(""))
rp.df <- read.csv2("Rinderpest.csv", stringsAsFactors = FALSE, na.strings = c(""))
ppr.df <- read.csv2("PPR.csv", stringsAsFactors = FALSE)
fmd.df <- read.csv2("FMD.csv", stringsAsFactors = FALSE)
csf.df <- read.csv2("CSF.csv", stringsAsFactors = FALSE)
cbpp.df <- read.csv2("CBPP.csv", stringsAsFactors = FALSE)
bse.df <- read.csv2("BSE.csv", stringsAsFactors = FALSE)


# Raw data and driver data
raw.df <- read.csv2('RawNumbers_v09.csv', stringsAsFactors = FALSE)
aDRI.df <- read.csv2("Animal_Drivers.csv", stringsAsFactors = FALSE)

# Biomass data
prod.df <- read.csv2( "FAO_Animal_production.csv", stringsAsFactors = F)
bm_ISO.df <- read.csv2("Biomass.csv", stringsAsFactors = F)
fish.df <- read.csv2("Fish quantities.csv", stringsAsFactors = F, na.strings = c("","-"))


# Human population data
pop.df <- read.csv2("Population_New.csv", stringsAsFactors = F)


save.image(file =here:::here("0.data/", "Animal_Data.RData"))
