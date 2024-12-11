
################################################# animal disease prevalence


# SUBSET - In boxplots and barplots only use those countries, which report at least one other
# category: pressure, state or impact
keep.df <- raw.df[ which(raw.df$SPECIES == "ANIMAL" & raw.df$DPSIR == "PRESSURE" |
                           raw.df$SPECIES == "ANIMAL" & raw.df$DPSIR == "STATE" |
                           raw.df$SPECIES == "HUMAN/ANIMAL" & raw.df$DPSIR == "PRESSURE" |
                           raw.df$SPECIES == "HUMAN/ANIMAL" & raw.df$DPSIR == "STATE" ),]
keep.vec <- unique(keep.df$ISO3)



# Format DFs into long form with proper names
names(bse.df)[names(bse.df) == "BSE.risk.status"] <- "Country"
names(bse.df)[names(bse.df) == "X"] <- "2010"
names(bse.df)[names(bse.df) == "X.1"] <- "2015"
names(bse.df)[names(bse.df) == "X.2"] <- "2017"
bse.df <- bse.df[-1,]
bse.melt <- melt(bse.df, id.vars = "Country")
names(bse.melt)[names(bse.melt) == "value"] <- "BSE"
bse.melt$BSE.free <- NA

names(cbpp.df)[names(cbpp.df) == "CBPP.free.status"] <- "Country"
names(cbpp.df)[names(cbpp.df) == "X"] <- "2005"
names(cbpp.df)[names(cbpp.df) == "X.1"] <- "2010"
names(cbpp.df)[names(cbpp.df) == "X.2"] <- "2015"
names(cbpp.df)[names(cbpp.df) == "X.3"] <- "2017"
cbpp.df <- cbpp.df[-1,]
cbpp.melt <- melt(cbpp.df, id.vars = "Country")
names(cbpp.melt)[names(cbpp.melt) == "value"] <- "CBPP"
cbpp.melt$CBPP.free <- NA

names(csf.df)[names(csf.df) == "CSF.free.status"] <- "Country"
names(csf.df)[names(csf.df) == "AHS.free.status"] <- "2015"
names(csf.df)[names(csf.df) == "X"] <- "2017"
csf.df <- csf.df[-1,]
csf.melt <- melt(csf.df, id.vars = "Country")
names(csf.melt)[names(csf.melt) == "value"] <- "CSF"
csf.melt$CSF.free <- NA

fmd.df <- fmd.df[-c(1,92:96),]
names(fmd.df)[names(fmd.df) == "Countries"] <- "Country"
fmd.df$stat <- NA
for(i in 1:72){
  fmd.df$stat[[i]] <- "FMD.free"
}
for(i in 76:90){
  fmd.df$stat[[i]] <- "At.least.one.FMD.free.zone"
}
fmd.df <- fmd.df[complete.cases(fmd.df$stat),]
fmd.melt <- melt(fmd.df, id.vars = c("Country", "stat"))
names(fmd.melt)[names(fmd.melt) == "value"] <- "FMD"
fmd.melt$FMD.free <- NA
fmd.melt$variable <- gsub("X", "", fmd.melt$variable)
fmd.melt$variable <- gsub("May.", "", fmd.melt$variable)


names(ppr.df)[names(ppr.df) == "PPR.free.status"] <- "Country"
names(ppr.df)[names(ppr.df) == "X"] <- "2015"
names(ppr.df)[names(ppr.df) == "X.1"] <- "2017"
ppr.df <- ppr.df[-1,]
ppr.melt <- melt(ppr.df, id.vars = "Country")
names(ppr.melt)[names(ppr.melt) == "value"] <- "PPR"
ppr.melt$PPR.free <- NA

names(rp.df)[names(rp.df) == "free.countries.by.Resolutions"] <- "Country"
names(rp.df)[names(rp.df) == "X"] <- "2000"
names(rp.df)[names(rp.df) == "X.1"] <- "2005"
names(rp.df)[names(rp.df) == "X.2"] <- "2010"
names(rp.df)[names(rp.df) == "X.3"] <- "2011"
rp.df <- rp.df[-1,-6]
rp.melt <- melt(rp.df, id.vars = "Country")
names(rp.melt)[names(rp.melt) == "value"] <- "RP"
rp.melt$RP.free <- NA

names(ahs.df)[names(ahs.df) == "X"] <- "Country"
names(ahs.df)[names(ahs.df) == "X.1"] <- "2015"
names(ahs.df)[names(ahs.df) == "X.2"] <- "2017"
ahs.df <- ahs.df[-1,]
ahs.melt <- melt(ahs.df, id.vars = "Country")
names(ahs.melt)[names(ahs.melt) == "value"] <- "AHS"
ahs.melt$AHS.free <- NA


# Merge them all into one big DF
histdis.df <- merge(ahs.melt, rp.melt, by = c("Country", "variable"), all = TRUE)
histdis.df <- histdis.df[complete.cases(histdis.df$Country),]
histdis.df <- merge(histdis.df, ppr.melt, by = c("Country", "variable"), all = TRUE)
histdis.df <- merge(histdis.df, fmd.melt, by = c("Country", "variable"), all = TRUE)
histdis.df <- merge(histdis.df, cbpp.melt, by = c("Country", "variable"), all = TRUE)
histdis.df <- merge(histdis.df, bse.melt, by = c("Country", "variable"), all = TRUE)
histdis.df <- merge(histdis.df, csf.melt, by = c("Country", "variable"), all = TRUE)


hist.dis1.df <- histdis.df

### Code countries' status into numbers

for(i in 1:nrow(hist.dis1.df)){
  
  #AHS
  if(is.na(hist.dis1.df$AHS[[i]])){
    hist.dis1.df$AHS[[i]] <- "NA"
  }
  if(hist.dis1.df$AHS[[i]] == "free"){
    hist.dis1.df$AHS.free[[i]] <- 1
  }
  if(hist.dis1.df$AHS[[i]] == "free "){
    hist.dis1.df$AHS.free[[i]] <- 1
  }
  if(hist.dis1.df$AHS[[i]] == "NA"){
    hist.dis1.df$AHS.free[[i]] <- 5
  }
  
  
  #RP
  if(is.na(hist.dis1.df$RP[[i]])){
    hist.dis1.df$RP[[i]] <- "NA"
  }
  if(hist.dis1.df$RP[[i]] == "free"){
    hist.dis1.df$RP.free[[i]] <- 1
  }
  if(hist.dis1.df$RP[[i]] == "disease free"){
    hist.dis1.df$RP.free[[i]] <- 2
  }
  if(hist.dis1.df$RP[[i]] == "NA" | histdis.df$RP[[i]] == "disease(Zone)"){
    hist.dis1.df$RP.free[[i]] <- 5
  }
  
  #PPR
  if(is.na(hist.dis1.df$PPR[[i]])){
    hist.dis1.df$PPR[[i]] <- "NA"
  }
  if(hist.dis1.df$PPR[[i]] == "free"){
    hist.dis1.df$PPR.free[[i]] <- 1
  }
  if(hist.dis1.df$PPR[[i]] == "NA" | hist.dis1.df$PPR[[i]] == ""){
    hist.dis1.df$PPR.free[[i]] <- 5
  }
  
  # FDM
  if(is.na(hist.dis1.df$FMD[[i]])){
    hist.dis1.df$FMD[[i]] <- "NA"
  }
  if(hist.dis1.df$Country[[i]] == "Argentina" | hist.dis1.df$Country[[i]] == "Philippines"){
    hist.dis1.df$FMD[[i]] <- 0
  }
  if(hist.dis1.df$FMD[[i]] == "NA" | hist.dis1.df$FMD[[i]] == ""){
    hist.dis1.df$FMD.free[[i]] <- 5
  }
  if(hist.dis1.df$FMD[[i]] == "with vacc" & hist.dis1.df$stat[[i]] == "At.least.one.FMD.free.zone"){
    hist.dis1.df$FMD.free[[i]] <- 4
  }
  if(hist.dis1.df$FMD[[i]] == "without vacc" & hist.dis1.df$stat[[i]] == "At.least.one.FMD.free.zone"){
    hist.dis1.df$FMD.free[[i]] <- 3
  }
  if(hist.dis1.df$FMD[[i]] == "with vacc" & hist.dis1.df$stat[[i]] == "FMD.free"){
    hist.dis1.df$FMD.free[[i]] <- 2
  }
  if(hist.dis1.df$FMD[[i]] == "without vacc" & hist.dis1.df$stat[[i]] == "FMD.free"){
    hist.dis1.df$FMD.free[[i]] <- 1
  }
  if(hist.dis1.df$FMD[[i]] == "free"){
    hist.dis1.df$FMD.free[[i]] <- 1  
  }
  if(hist.dis1.df$FMD[[i]] == "with & without vacc"){
    hist.dis1.df$FMD.free[[i]] <- 4
  }
  if(hist.dis1.df$FMD[[i]] == 0){
    hist.dis1.df$FMD.free[[i]] <- 0
  }
  
  # CSF
  if(is.na(hist.dis1.df$CSF[[i]])){
    hist.dis1.df$CSF[[i]] <- "NA"
  }
  if(hist.dis1.df$CSF[[i]] == "free"){
    hist.dis1.df$CSF.free[[i]] <- 1
  }
  if(hist.dis1.df$CSF[[i]] == "free zone"){
    hist.dis1.df$CSF.free[[i]] <- 2
  }
  if(hist.dis1.df$CSF[[i]] == "" | hist.dis1.df$CSF[[i]] == "NA"){
    hist.dis1.df$CSF.free[[i]] <- 5
  }
  
  
  # CBPP
  if(is.na(hist.dis1.df$CBPP[[i]])){
    hist.dis1.df$CBPP[[i]] <- "NA"
  }
  if(hist.dis1.df$CBPP[[i]] == "free"){
    hist.dis1.df$CBPP.free[[i]] <- 1
  }
  if(hist.dis1.df$CBPP[[i]] == "NA"){
    hist.dis1.df$CBPP.free[[i]] <- 5
  }
  if(hist.dis1.df$CBPP[[i]] == ""){
    hist.dis1.df$CBPP.free[[i]] <- 5
  }
  if(hist.dis1.df$Country[[i]] == "Namibia"){
    hist.dis1.df$CBPP.free[[i]] <- 2
  }
  
  # BSE
  if(is.na(hist.dis1.df$BSE[[i]])){
    hist.dis1.df$BSE[[i]] <- "NA"
  }
  if(hist.dis1.df$BSE[[i]] == "Negligible" | hist.dis1.df$BSE[[i]] == "negligible" ){
    hist.dis1.df$BSE.free[[i]] <- 1
  }
  if(hist.dis1.df$BSE[[i]] == "Controlled" | hist.dis1.df$BSE[[i]] == "controlled" ){
    hist.dis1.df$BSE.free[[i]] <- 2
  }
  if(hist.dis1.df$BSE[[i]] == "" | hist.dis1.df$BSE[[i]] == "NA"){
    hist.dis1.df$BSE.free[[i]] <- 5
  }
}

# Drop the character columns    
histdis1.num <- subset(hist.dis1.df, select=-c(AHS, RP, PPR, FMD, CBPP, BSE, CSF, stat))

# Merge with ISO3 code
histdis1.ISO <- merge(histdis1.num,key.df[,c("EORA_a3","OIE_Country")], by.x="Country", by.y="OIE_Country")


histdis2.ISO <- histdis1.ISO

for(i in 1:nrow(histdis2.ISO)){
  if( histdis2.ISO$variable [[i]] == 1996){
    histdis2.ISO$AHS.free[[i]] <- NA
    histdis2.ISO$BSE.free[[i]] <- NA
    histdis2.ISO$CBPP.free[[i]] <- NA
    histdis2.ISO$CSF.free[[i]] <- NA
    histdis2.ISO$PPR.free[[i]] <- NA
    histdis2.ISO$RP.free[[i]] <- NA
  }
  if( histdis2.ISO$variable [[i]] == 2000){
    histdis2.ISO$AHS.free[[i]] <- NA
    histdis2.ISO$BSE.free[[i]] <- NA
    histdis2.ISO$CBPP.free[[i]] <- NA
    histdis2.ISO$CSF.free[[i]] <- NA
    histdis2.ISO$PPR.free[[i]] <- NA
  }
  if( histdis2.ISO$variable [[i]] == 2005){
    histdis2.ISO$AHS.free[[i]] <- NA
    histdis2.ISO$BSE.free[[i]] <- NA
    histdis2.ISO$CSF.free[[i]] <- NA
    histdis2.ISO$PPR.free[[i]] <- NA
  }
  if( histdis2.ISO$variable [[i]] == 2010){
    histdis2.ISO$AHS.free[[i]] <- NA
    histdis2.ISO$CSF.free[[i]] <- NA
    histdis2.ISO$PPR.free[[i]] <- NA
  }
  if( histdis2.ISO$variable [[i]] == 2011){
    histdis2.ISO$AHS.free[[i]] <- NA
    histdis2.ISO$BSE.free[[i]] <- NA
    histdis2.ISO$CBPP.free[[i]] <- NA
    histdis2.ISO$CSF.free[[i]] <- NA
    histdis2.ISO$PPR.free[[i]] <- NA
    histdis2.ISO$FMD.free[[i]] <- NA
  }
  if( histdis2.ISO$variable [[i]] == 2015 | histdis2.ISO$variable [[i]] == 2017 ){
    histdis2.ISO$RP.free[[i]] <- NA
  }
}

# Calculate the index
histdis.final2 <- histdis2.ISO

histdis.final2$sum <- rowSums(histdis.final2[,c("AHS.free", "BSE.free","CBPP.free","CSF.free",
                                                "FMD.free", "PPR.free", "RP.free")], na.rm=TRUE)
histdis.final2$total <- NA
for(i in 1:nrow(histdis.final2)){
  if(histdis.final2$variable[[i]] == 1996){
    histdis.final2$total[[i]] <- 5
  }
  if(histdis.final2$variable[[i]] == 2000){
    histdis.final2$total[[i]] <- 10
  }
  if(histdis.final2$variable[[i]] == 2005){
    histdis.final2$total[[i]] <- 15
  }
  if(histdis.final2$variable[[i]] == 2010){
    histdis.final2$total[[i]] <- 20
  }
  if(histdis.final2$variable[[i]] == 2011){
    histdis.final2$total[[i]] <- 5
  }
  if(histdis.final2$variable[[i]] == 2015 | histdis.final2$variable[[i]] == 2017){
    histdis.final2$total[[i]] <- 30
  }
}

histdis.final2$ADCI <- histdis.final2$sum/histdis.final2$total

histdis.plot <- histdis.final2
names(histdis.plot)[names(histdis.plot) == "variable"] <- "Year"



### Changes in ADCI for each country - compare mean from 2000-2010 with mean from 2010-2017
adci.mean <- aggregate( data = histdis.plot[which(histdis.plot$Year == 2000 |
                                                    histdis.plot$Year == 2005 |
                                                    histdis.plot$Year == 2010 ),], 
                        ADCI~Country+EORA_a3, function(x) mean(x))
names(adci.mean)[names(adci.mean) == "ADCI"] <- "0010"
adci.mean2 <- aggregate( data = histdis.plot[which(histdis.plot$Year == 2011 |
                                                     histdis.plot$Year == 2015 |
                                                     histdis.plot$Year == 2017 |
                                                     histdis.plot$Year == 2010 ),], 
                         ADCI~Country+EORA_a3, function(x) mean(x))
names(adci.mean2)[names(adci.mean2) == "ADCI"] <- "1017"

adci.mean.final <- merge(adci.mean, adci.mean2, by = c("Country", "EORA_a3"), all = T)
adci.mean.melt <- melt(adci.mean.final, id.vars = c("Country", "EORA_a3"))

adci.mean.meltSUB <- adci.mean.melt[ which(adci.mean.melt$EORA_a3 %in% keep.vec),]

# Scale values
adci.scaled <- adci.mean.meltSUB 
adci.scaled$scaled <- scale(adci.scaled$value, center = T, scale = T)

# Calculate difference in means
adci.cast <- aggregate( data = adci.scaled, scaled~EORA_a3, 
                        function(x) diff(x))
names(adci.cast)[names(adci.cast) == "V1"] <- "Infections"
adci.merge <- adci.cast

# Non-scaled DF
adci.castNS <- aggregate( data = adci.scaled, value~EORA_a3, 
                          function(x) diff(x))
names(adci.castNS)[names(adci.castNS) == "value"] <- "Infections"
adci.mergeNS <- adci.castNS


# Early mean for RESPONSE plots
histdis.plot$Year <- as.numeric( as.character( histdis.plot$Year ) )
adci.earlyM <- aggregate( data = histdis.plot[which ( histdis.plot$Year <= 2005 &
                                                        histdis.plot$Year > 1999),], 
                        ADCI~Country+EORA_a3, function(x) mean(x))
names(adci.earlyM)[names(adci.earlyM) == "ADCI"] <- "0005ADCI"
adci.earlyM$`0005ADCI.scaled` <- scale( adci.earlyM$`0005ADCI`, center = T, scale = T)




### Modified Veterinarians per ton 

# Make separate DF the drivers
MVets_Ton.df <- aDRI.df[which(aDRI.df$SHORTNAME == "MoVetsPerTon"),]
MVets_Ton.df <- MVets_Ton.df[,-c(1, 4, 6)]
MVets_Ton.df <- unique(MVets_Ton.df)
MVets_Ton.df$Value <- as.numeric(as.character(MVets_Ton.df$Value))

## Drop China and Congo for now. Reporting double values for most years
MVets_Ton.df <- MVets_Ton.df[!(MVets_Ton.df$EORA_a3 == "CHN" | MVets_Ton.df$EORA_a3 == "COG"),]

# Compare mean 2005-2009 with mean 2010-2014
MVets_Ton.Mean <- aggregate( data = MVets_Ton.df[which(MVets_Ton.df$Year < 2010 ), ], 
                             Value~EORA_a3, function(x) mean(x))
names(MVets_Ton.Mean)[names(MVets_Ton.Mean) == "Value"] <- "0509"
MVets_Ton.Mean1 <- aggregate( data = MVets_Ton.df[which(MVets_Ton.df$Year >= 2010 ), ], 
                              Value~EORA_a3, function(x) mean(x))
names(MVets_Ton.Mean1)[names(MVets_Ton.Mean1) == "Value"] <- "1014"
MVets_Ton.Mean <- merge(MVets_Ton.Mean, MVets_Ton.Mean1, by = "EORA_a3", all = TRUE)

# Keep only countries that have a mean for both periods. Drop MDV, because value is Infinity
MVets_Ton.Mean <- MVets_Ton.Mean[complete.cases(MVets_Ton.Mean),]
MVets_Ton.Mean <- MVets_Ton.Mean[!(MVets_Ton.Mean$EORA_a3 == "MDV"),]

MVets_Ton.MeanSUB <- MVets_Ton.Mean[ which(MVets_Ton.Mean$EORA_a3 %in% keep.vec), ]

# Scale values
MVets_Ton.melt <- melt( MVets_Ton.MeanSUB, id.vars = "EORA_a3")
MVets_Ton.melt$scaled <- scale(MVets_Ton.melt$value, center = T, scale = T)

# Calculate difference in means
MVets_Ton.final <- aggregate( data = MVets_Ton.melt, scaled~EORA_a3, 
                              function(x) diff(x))
names( MVets_Ton.final )[ names( MVets_Ton.final ) == "V1" ] <- "Workforce"
MVets_Ton.merge <- MVets_Ton.final

# Non-scaled DF
MVets_Ton.finalNS <- aggregate( data = MVets_Ton.melt, value~EORA_a3, 
                                function(x) diff(x))
names(MVets_Ton.finalNS)[names(MVets_Ton.finalNS) == "value"] <- "Workforce"
MVets_Ton.mergeNS <- MVets_Ton.finalNS


# Early mean for RESPONSE plots
MVets.earlyM <- aggregate( data = MVets_Ton.df[which ( MVets_Ton.df$Year <= 2005 ),], 
                          Value~EORA_a3, function(x) mean(x))
names(MVets.earlyM)[names(MVets.earlyM) == "Value"] <- "05MVets"
MVets.earlyM$`05MVets.scaled` <- scale( MVets.earlyM$`05MVets`, center = T, scale = T)




### TRADE: IMPORTS PER DOMESTIC PRODUCTION
imPROD.df <- aDRI.df [ which ( aDRI.df$SHORTNAME == "IM.Prod" ) , ]
imPROD.df <- unique ( imPROD.df )
imPROD.df <- imPROD.df[!(imPROD.df$EORA_a3 == "CHN" | imPROD.df$EORA_a3 == "SDN" | 
                           imPROD.df$EORA_a3 == "COG" | 
                           imPROD.df$EORA_a3 == "ETH"), ]

# Calculate means, merge and add the change in means
imPROD.df$Value <- as.numeric( as.character ( imPROD.df$Value ) )
imPROD.Mean0509 <- aggregate( data = imPROD.df[ which( imPROD.df$Year >= 2006 & imPROD.df$Year < 2010 ), ], 
                              Value~EORA_a3, function(x) mean(x))
imPROD.Mean1014 <- aggregate( data = imPROD.df[ which( imPROD.df$Year >= 2010 ), ], 
                              Value~EORA_a3, function(x) mean(x))
names( imPROD.Mean0509 )[ names( imPROD.Mean0509 ) == "Value"] <- "0509"
names( imPROD.Mean1014 )[ names( imPROD.Mean1014 ) == "Value"] <- "1014"

imPROD.Mean <- merge( imPROD.Mean0509, imPROD.Mean1014, by = "EORA_a3", all = T)
imPROD.melt <- melt( imPROD.Mean, id.vars = "EORA_a3" )
imPROD.melt <- imPROD.melt[! (imPROD.melt$value  == "Inf"), ] 
imPROD.meltSUB <- imPROD.melt[ which(imPROD.melt$EORA_a3 %in% keep.vec), ]

imPROD.meltSUB$scaled <- scale( imPROD.meltSUB$value, center = T, scale = T )

imPROD.final <- aggregate( data = imPROD.meltSUB, scaled~EORA_a3, 
                           function(x) diff(x))
names(imPROD.final)[ names(imPROD.final) == "V1" ] <- "Trade"

imPROD.merge <- imPROD.final

# Non-scaled DF
imPROD.finalNS <- aggregate( data = imPROD.melt, value~EORA_a3, 
                             function(x) diff(x))
names(imPROD.finalNS)[names(imPROD.finalNS) == "value"] <- "Trade"
imPROD.mergeNS <- imPROD.finalNS


# Early mean for RESPONSE plots
imPROD.earlyM <- aggregate( data = imPROD.df[which ( imPROD.df$Year <= 2005 &
                                                       imPROD.df$Year > 1999),], 
                           Value~EORA_a3, function(x) mean(x))
names(imPROD.earlyM)[names(imPROD.earlyM) == "Value"] <- "0005imPROD"
# Drop Maledives (Value = Inf)
imPROD.earlyM <- imPROD.earlyM[ !(imPROD.earlyM$EORA_a3 == "MDV"),]

imPROD.earlyM$`0005imPROD.scaled` <- scale( imPROD.earlyM$`0005imPROD`, center = T, scale = T)






## Total meat and fish production

#prod.df <- read.csv2( "FAO_Animal_production.csv", stringsAsFactors = F)
prod.df <- prod.df[ which ( prod.df$Unit == "tonnes" ),]
prod.cast <- dcast( data = prod.df, Area+Year~Item, value.var = "Value")

# Merge with ISO code
#bm_ISO.df <- read.csv2("Biomass.csv", stringsAsFactors = F)
prod.castISO <- merge(prod.cast, bm_ISO.df[, c( "EORA_a3", "Country" ) ], by.x = "Area",
                      by.y = "Country", all = T)
#key.df <- read.csv2("Key_Incl_FAOandOIE.csv", stringsAsFactors = F)
prod.castISO <- merge(prod.castISO, key.df[, c("FAO.Area", "EORA_a3") ], by.x = "Area",
                      by.y = "FAO.Area", all = T)
prod.castISO <- prod.castISO[! (is.na(prod.castISO$Area)),]
prod.castISO$ISO3 <- prod.castISO$EORA_a3.x
for(i in 1:nrow(prod.castISO)){
  if(is.na(prod.castISO$EORA_a3.x[[i]])){
    prod.castISO$ISO3[[i]] <- prod.castISO$EORA_a3.y[[i]]
  }
}
# Drop countries that do not have an ISO code
prod.castISO <- prod.castISO [! (prod.castISO$Area == "Czechia" | prod.castISO$Area == "Czechoslovakia" | 
                                   prod.castISO$Area == "Channel Islands" | 
                                   prod.castISO$Area == "Occupied Palestinian Territory" |
                                   prod.castISO$Area == "Palestine; Occupied Tr." | 
                                   prod.castISO$Area == "Serbia and Montenegro" |
                                   prod.castISO$Area == "Un. Sov. Soc. Rep." |
                                   prod.castISO$Area == "Yugoslavia SFR" |
                                   prod.castISO$Area == "Belgium-Luxembourg"),]

# Add Lao and Maldives manually
for(i in 1:nrow(prod.castISO)){
  if(prod.castISO$Area [[i]] == "Lao People's Democratic Republic"){
    prod.castISO$ISO3 [[i]] <- "LAO"
  }
  if(prod.castISO$Area [[i]] == "Maldives"){
    prod.castISO$ISO3 [[i]] <- "MDV"
  }
}

prod.castISO <- prod.castISO[ , !names(prod.castISO) %in% c("EORA_a3.x","EORA_a3.y")]




### FISH DATA


#fish.df <- read.csv2("Fish quantities.csv", stringsAsFactors = F, na.strings = c("","-"))
fish.df <- fish.df[, -c(3:4)]
fish.melt <- melt( data = fish.df, id.vars = c("Country..Country.", "Species..Main.grouping.") )
fish.melt <- fish.melt[ complete.cases ( fish.melt ), ]
fish.melt$value <- as.character(fish.melt$value)
for(i in 1:nrow(fish.melt)){
  if(fish.melt$value[[i]] == "0 0"){
    fish.melt$value[[i]] <- "0"
  }
}
fish.melt$variable <- gsub( "X", "", fish.melt$variable)

fish.melt$value <- as.numeric( as.character( fish.melt$value ) )
fish.sum <- aggregate( data = fish.melt, value~Country..Country.+variable, function(x) sum(x))
names(fish.sum)[names(fish.sum) == "variable"] <- "Year"
names(fish.sum)[names(fish.sum) == "value"] <- "Fish"
names(fish.sum)[names(fish.sum) == "Country..Country."] <- "Area"

## Add ISO code to fish data
fishISO.df <- merge(fish.sum, key.df[, c("FAO.Area", "EORA_a3") ], by.x = "Area",
                    by.y = "FAO.Area", all = T)
## Throw out "non-existent" countries & add missing codes manually
fishISO.df <- fishISO.df [! (fishISO.df$Area == "Czechia" | fishISO.df$Area == "Czechoslovakia" | 
                               fishISO.df$Area == "Channel Islands" | 
                               fishISO.df$Area == "Occupied Palestinian Territory" |
                               fishISO.df$Area == "Palestine; Occupied Tr." | 
                               fishISO.df$Area == "Serbia and Montenegro" |
                               fishISO.df$Area == "Un. Sov. Soc. Rep." |
                               fishISO.df$Area == "Yugoslavia SFR" |
                               fishISO.df$Area == "Belgium-Luxembourg" |
                               fishISO.df$Area == "Bonaire/S.Eustatius/Saba"),]
fishISO.df <- fishISO.df[! (is.na(fishISO.df$Fish) ), ]

for(i in 1:nrow(fishISO.df)){
  if(fishISO.df$Area [[i]] == "Bolivia (Plurinat.State)"){
    fishISO.df$EORA_a3 [[i]] <- "BOL"
  }
  if(fishISO.df$Area [[i]] == "Iran (Islamic Rep. of)"){
    fishISO.df$EORA_a3 [[i]] <- "IRN"
  }
  if(fishISO.df$Area [[i]] == "Congo; Dem. Rep. of the"){
    fishISO.df$EORA_a3 [[i]] <- "COD"
  }
  if(fishISO.df$Area [[i]] == "Czech Republic"){
    fishISO.df$EORA_a3 [[i]] <- "CZE"
  }
  if(fishISO.df$Area [[i]] == "Fiji; Republic of"){
    fishISO.df$EORA_a3 [[i]] <- "FJI"
  }
  if(fishISO.df$Area [[i]] == "Korea; Dem. People's Rep"){
    fishISO.df$EORA_a3 [[i]] <- "PRK"
  }
  if(fishISO.df$Area [[i]] == "Korea; Republic of"){
    fishISO.df$EORA_a3 [[i]] <- "KOR"
  }
  if(fishISO.df$Area [[i]] == "Lao People's Dem. Rep."){
    fishISO.df$EORA_a3 [[i]] <- "LAO"
  }
  if(fishISO.df$Area [[i]] == "Lao People's Democratic Republic"){
    fishISO.df$EORA_a3 [[i]] <- "LAO"
  }
  if(fishISO.df$Area [[i]] == "Macedonia; Fmr Yug Rp of"){
    fishISO.df$EORA_a3 [[i]] <- "MKD"
  }
  if(fishISO.df$Area [[i]] == "Mayotte"){
    fishISO.df$EORA_a3 [[i]] <- "MYT"
  }
  if(fishISO.df$Area [[i]] == "Micronesia; Fed.States of"){
    fishISO.df$EORA_a3 [[i]] <- "FSM"
  }
  if(fishISO.df$Area [[i]] == "Northern Mariana Is."){
    fishISO.df$EORA_a3 [[i]] <- "MNP"
  }
  if(fishISO.df$Area [[i]] == "R?union"){
    fishISO.df$EORA_a3 [[i]] <- "REU"
  }
  if(fishISO.df$Area [[i]] == "Taiwan Province of China"){
    fishISO.df$EORA_a3 [[i]] <- "TWN"
  }
  if(fishISO.df$Area [[i]] == "Tanzania; United Rep. of"){
    fishISO.df$EORA_a3 [[i]] <- "TZA"
  }
  if(fishISO.df$Area [[i]] == "Turks and Caicos Is."){
    fishISO.df$EORA_a3 [[i]] <- "TCA"
  }
  if(fishISO.df$Area [[i]] == "US Virgin Islands"){
    fishISO.df$EORA_a3 [[i]] <- "VIR"
  }
  if(fishISO.df$Area [[i]] == "Venezuela; Boliv Rep of"){
    fishISO.df$EORA_a3 [[i]] <- "VEN"
  }
  if(fishISO.df$Area [[i]] == "Zanzibar"){
    fishISO.df$EORA_a3 [[i]] <- "TZA"
  }
}

fishISO.df <- merge(fishISO.df, key.df[, c("OIE_Country" , "EORA_a3") ], by.x = "Area",
                    by.y = "OIE_Country", all = T)
fishISO.df <- fishISO.df[! (is.na(fishISO.df$Fish) ), ]

fishISO.df$ISO3 <- fishISO.df$EORA_a3.x
for(i in 1:nrow(fishISO.df)){
  if(is.na(fishISO.df$EORA_a3.x[[i]] ) ){
    fishISO.df$ISO3[[i]] <- fishISO.df$EORA_a3.y[[i]]
  }
  if(fishISO.df$Area[[i]] == "Moldova; Republic of"){
    fishISO.df$ISO3[[i]] <- "MDA"
  }
  if(fishISO.df$Area[[i]] == "Moldova; Republic of"){
    fishISO.df$ISO3[[i]] <- "MDA"
  }
}

fishISO.df <- fishISO.df[ , !names(fishISO.df) %in% c("EORA_a3.x","EORA_a3.y")]



aPROD.df <- merge(prod.castISO, fishISO.df[, c("ISO3", "Year", "Fish")], 
                  by = c("ISO3", "Year"), all = TRUE)
# Prepare for summing
aPROD.df <- aPROD.df[ !is.na(aPROD.df$Year), ]

aPROD.df <- aPROD.df[ !(is.na(aPROD.df$`Beef and Buffalo Meat`) &
                          is.na(aPROD.df$`Meat; horse`) &
                          is.na(aPROD.df$`Meat; pig`) &
                          is.na(aPROD.df$`Meat; Poultry`) &
                          is.na(aPROD.df$`Meat; turkey`) &
                          is.na(aPROD.df$`Sheep and Goat Meat`) &
                          is.na(aPROD.df$Fish)),] 
aPROD.df <- aPROD.df[, !names(aPROD.df) %in% c("Meat; Total")]


# SUM UP
aPROD.df$ALL <- rowSums(aPROD.df[, c(4:10)], na.rm = TRUE)

# Remove duplicates
aPROD.ALL <- aPROD.df[, !names(aPROD.df) %in% c("Beef and Buffalo Meat", "Meat; horse",
                                                "Meat; pig", "Meat; Poultry", "Meat; turkey",
                                                "Sheep and Goat Meat", "Fish")]

### for use in the model comparison analysis of responses
aPROD.All.Mean0713 <- aggregate( data = aPROD.ALL[ which(aPROD.ALL$Year > 2006 & aPROD.ALL$Year < 2014 ), ], 
                             ALL~ISO3, function(x) mean(x) )

### comment out when no need to produce new data file
#setwd(data1.wd)
#write.csv(file = "animalproduction.csv",aPROD.All.Mean0713,row.names = FALSE)


# Add population to get per capita meat & fish production
#pop.df <- read.csv2("C:\\Users\\franziska\\Dropbox\\DPSIR\\DPSIR_Data\\ALL\\Population_New.csv",
#                    stringsAsFactors = F)

aPROD.pop <- merge(aPROD.df [, c("ISO3", "Year", "Area", "ALL")], 
                   pop.df[, c("Country.Code", "Year", "Population")], 
                   by.x = c("ISO3", "Year"), by.y = c("Country.Code", "Year"), all = T )
aPROD.pop <- aPROD.pop[ complete.cases(aPROD.pop$ALL),]
aPROD.pop <- aPROD.pop[ complete.cases(aPROD.pop$Population),]

# Meat & Fish production per capita
aPROD.pop$ALL <- as.numeric(as.character(aPROD.pop$ALL))
aPROD.pop$Population <- as.numeric(as.character(aPROD.pop$Population))
aPROD.pop$Prod.Per.Capita <- aPROD.pop$ALL/aPROD.pop$Population

# Some countries still have multiple reporting. Take them out
aPROD.pop <- aPROD.pop[ !(aPROD.pop$Area == "China" | aPROD.pop$Area == "Sudan" | 
                            aPROD.pop$Area == "South Sudan" | aPROD.pop$Area == "Sudan (former)"),]
aPROD.pop <- unique(aPROD.pop)

# Calculate means 2005-2009 & 2010-2014 
aPROD.Mean0509 <- aggregate( data = aPROD.pop[ which(aPROD.pop$Year >= 2005 & aPROD.pop$Year < 2010 ), ], 
                             Prod.Per.Capita~ISO3, function(x) mean(x) )
aPROD.Mean1014 <- aggregate( data = aPROD.pop[ which(aPROD.pop$Year >= 2010 ), ], 
                             Prod.Per.Capita~ISO3, function(x) mean(x) )
names(aPROD.Mean0509)[ names( aPROD.Mean0509 ) == "Prod.Per.Capita"] <- "0509"
names(aPROD.Mean1014)[ names( aPROD.Mean1014 ) == "Prod.Per.Capita"] <- "1014"

aPROD.Mean <- merge(aPROD.Mean0509, aPROD.Mean1014, by = "ISO3", all = T)
aPROD.melt <- melt(aPROD.Mean, id.vars = "ISO3")
aPROD.meltSUB <- aPROD.melt[ which(aPROD.melt$ISO3 %in% keep.vec), ]

aPROD.meltSUB$scaled <- scale( aPROD.meltSUB$value, center = T, scale = T ) 
aPROD.final <- aggregate( data = aPROD.meltSUB, scaled~ISO3, function(x) diff(x) )
names(aPROD.final)[names(aPROD.final) == "V1"] <- "Production"

aPROD.merge <- aPROD.final

# Non-scaled DF
aPROD.finalNS <- aggregate( data = aPROD.melt, value~ISO3, 
                            function(x) diff(x))
names(aPROD.finalNS)[names(aPROD.finalNS) == "value"] <- "Production"
aPROD.mergeNS <- aPROD.finalNS


# Early mean for RESPONSE plots
aPROD.earlyM <- aggregate( data = aPROD.pop[which ( aPROD.pop$Year <= 2005 &
                                                      aPROD.pop$Year > 1999),], 
                            Prod.Per.Capita~ISO3, function(x) mean(x))
names(aPROD.earlyM)[names(aPROD.earlyM) == "Prod.Per.Capita"] <- "0005aPROD"
aPROD.earlyM$`0005aPROD.scaled` <- scale( aPROD.earlyM$`0005aPROD`, center = T, scale = T)

# Create a common DF for the EARLY MEANS
early.mean <- merge( adci.earlyM, MVets.earlyM, by = "EORA_a3", all = T )
early.mean <- merge( early.mean, imPROD.earlyM, by = "EORA_a3", all = T )
early.mean <- merge( early.mean, aPROD.earlyM, by.x = "EORA_a3",
                     by.y = "ISO3", all = T )
early.mean <- early.mean[, names(early.mean) %in% c("EORA_a3", "Country") |
                           grepl(".scaled", names(early.mean)) ]
early.melt <- melt( early.mean, id.vars = c("EORA_a3", "Country") )
early.melt <- early.melt[ complete.cases( early.melt$value ), ]
early.meanDRI <- aggregate( data = early.melt, value~Country+EORA_a3,
                            function(x) mean(x) )


### Put all mean changes into one data frame
aDRI.Mean_Change <- merge(MVets_Ton.merge, adci.merge, by = "EORA_a3", all = T)
aDRI.Mean_Change <- merge(aDRI.Mean_Change, imPROD.merge, by = "EORA_a3", all = T)
aDRI.Mean_Change <- merge(aDRI.Mean_Change, aPROD.merge [, c("ISO3", "Production") ], 
                          by.x = "EORA_a3", by.y = "ISO3", all = T)

### Reverse Workforce indicator
aDRI.Mean_Change$WorkforceREV <- aDRI.Mean_Change$Workforce * (-1)
aDRI.Mean_Change <- aDRI.Mean_Change[, !(names( aDRI.Mean_Change) == "Workforce")]
names(aDRI.Mean_Change)[ names( aDRI.Mean_Change ) == "WorkforceREV"] <- "Workforce"

### Create the mean of workforce, infections & production indicators ("mean change")
aDRI.final <- aDRI.Mean_Change
aDRI.final$MEAN <- rowMeans(aDRI.final[, c("Infections", "Production", "Workforce")], na.rm = T)

aDRI.plot <- melt( aDRI.final, id.vars = "EORA_a3" )
aDRI.plot <- aDRI.plot[ complete.cases ( aDRI.plot ), ]




### Add an info column to final DF
aDRI.plot$info <- NA
aDRI.plot$alpha <- NA

for(i in 1:nrow(aDRI.plot)){
  if(aDRI.plot$variable[[i]] == "Infections"){
    aDRI.plot$info[[i]] <- "Disease Control Indicator. Mean change from 00-10 to 10-17"
  }
  if(aDRI.plot$variable[[i]] == "Trade"){
    aDRI.plot$info[[i]] <- "Import-Production-Ratio. Mean change from 05-09 to 10-14"
  }
  if(aDRI.plot$variable[[i]] == "Production"){
    aDRI.plot$info[[i]] <- "Domestic meat & fish production. Mean change from 05-09 to 10-14"
  }
  if(aDRI.plot$variable[[i]] == "Workforce"){
    aDRI.plot$info[[i]] <- "Veterinarians per ton of biomass (Reversed). Mean change from 05-09 to 10-14"
  }
  if(aDRI.plot$variable[[i]] == "MEAN"){
    aDRI.plot$info[[i]] <- "Mean of the change of the other indicators."
  }
  if(aDRI.plot$variable[[i]] == "MEAN"){
    aDRI.plot$alpha[[i]] <- 1
  }
  if(aDRI.plot$variable[[i]] != "MEAN"){
    aDRI.plot$alpha[[i]] <- 0.6
  }
  
}


### Data frame for p values - USE THE NON-SCALED VALUES !!!

# Keep only those countries, which report P, S or I in addition to the drivers 
MVets_Ton.mergeNS <- MVets_Ton.mergeNS[ which(MVets_Ton.mergeNS$EORA_a3 %in% keep.vec), ]
adci.mergeNS <- adci.mergeNS[ which(adci.mergeNS$EORA_a3 %in% keep.vec), ]
imPROD.mergeNS <- imPROD.mergeNS[ which(imPROD.mergeNS$EORA_a3 %in% keep.vec), ]
aPROD.mergeNS <- aPROD.mergeNS[ which(aPROD.mergeNS$ISO3 %in% keep.vec), ]


Mean_Change.NS <- merge(MVets_Ton.mergeNS, adci.mergeNS, by = "EORA_a3", all = T)
Mean_Change.NS <- merge(Mean_Change.NS, imPROD.mergeNS, by = "EORA_a3", all = T)
Mean_Change.NS <- merge(Mean_Change.NS, aPROD.mergeNS [, c("ISO3", "Production") ], 
                        by.x = "EORA_a3", by.y = "ISO3", all = T)

Mean_Change.NS$MEAN <- rowMeans( Mean_Change.NS[, c(2:3,5)], na.rm = T)
Mean_ChangeNS.melt <- melt( Mean_Change.NS, id.vars = "EORA_a3" )
Mean_ChangeNS.melt <- Mean_ChangeNS.melt[ complete.cases( Mean_ChangeNS.melt ), ]


ADp.df <- plyr:::ddply(Mean_ChangeNS.melt,.(variable), summarise,
              pval = summary (glm (value ~ 1 ) )$coefficients[1,4],
              N=length(is.na(value)==FALSE))
ADp.df$alpha <- NA
ADp.df [ which(ADp.df [, "variable"]%in%c("MEAN")), "alpha"] <- 1
ADp.df [ which(!ADp.df [, "variable"]%in%c("MEAN")), "alpha"] <- 0.6




### Preparing  the barplots
barplot.df <- aDRI.plot

# For each indicator/ variable. Calculate proportion of countries with increase/decrease
barplot.df$sign <- NA
barplot.df[ which(barplot.df$value > 0), "sign"] <- "pos"
prop.pos.df <- aggregate( data = barplot.df, sign~variable,
                          function(x) length(x))
names(prop.pos.df)[names(prop.pos.df) == "sign"] <- "Nb.pos"

barplot.df$sign <- NA
barplot.df[ which(barplot.df$value < 0), "sign"] <- "neg"
prop.neg.df <- aggregate( data = barplot.df, sign~variable,
                          function(x) length(x))
names(prop.neg.df)[names(prop.neg.df) == "sign"] <- "Nb.neg"

prop.final <- merge(prop.pos.df, prop.neg.df, by = "variable")
prop.final$prop.pos <- prop.final$Nb.pos/(prop.final$Nb.pos + prop.final$Nb.neg)
prop.final$prop.neg <- prop.final$Nb.neg/(prop.final$Nb.pos + prop.final$Nb.neg)

barplot.df <- merge(barplot.df, prop.final,
                    by = "variable", all = T)
barplot.df[ which(barplot.df$value > 0), "sign"] <- "pos"

barplot.df$prop <- NA
barplot.df$N <- NA
for(i in 1:nrow(barplot.df)){
  if(barplot.df$sign[[i]] == "pos"){
    barplot.df$prop[[i]] <- barplot.df$prop.pos[[i]]
  }
  if(barplot.df$sign[[i]] == "neg"){
    barplot.df$prop[[i]] <- barplot.df$prop.neg[[i]]
  }
  if(barplot.df$sign[[i]] == "pos"){
    barplot.df$N[[i]] <- barplot.df$Nb.pos[[i]]
  }
  if(barplot.df$sign[[i]] == "neg"){
    barplot.df$N[[i]] <- barplot.df$Nb.neg[[i]]
  }
}

barplot.df <- barplot.df [ !names(barplot.df) %in% c("prop.pos", "prop.neg", "Nb.pos", "Nb.neg")]




barplot.final <- barplot.df[!duplicated( barplot.df[c("variable","sign")] ), ]
barplot.final <- barplot.final[!names(barplot.final) %in% c("EORA_a3", "value", "info")]


barplot.final$variable <- ordered( barplot.final$variable,
                                   levels = c("MEAN", "Production", "Trade", "Workforce", "Infections"))

# Chi-Square test for every indicator
p.df2 <- plyr:::ddply( barplot.df, .(variable), summarise,
                pval = chisq.test(N)$p.value)


barplot.final <- merge(barplot.final, p.df2, by = "variable", all = T)



# For WOLRDMAP: Use all countries, even if they don't report P, S or I

# Scale each data frame
adci.mean.meltFULL <- adci.mean.melt
adci.mean.meltFULL$scaled <- scale( adci.mean.meltFULL$value, center = T, scale = T)

MVets_Ton.MeanFULL <- melt(MVets_Ton.Mean, id.vars = "EORA_a3")
MVets_Ton.MeanFULL$scaled <- scale( MVets_Ton.MeanFULL$value, center = T, scale = T)

imPROD.meltFULL <- imPROD.melt
imPROD.meltFULL$scaled <- scale( imPROD.meltFULL$value, center = T, scale = T)

aPROD.meltFULL <- aPROD.melt
aPROD.meltFULL$scaled <- scale( aPROD.meltFULL$value, center = T, scale = T)

# Calculate change
adci.castFULL <- dcast( data = adci.mean.meltFULL, EORA_a3~variable, value.var = "scaled") 
adci.castFULL$ADCI <- adci.castFULL$`1017` - adci.castFULL$`0010`

MVets_Ton.castFULL <- dcast( data = MVets_Ton.MeanFULL, EORA_a3~variable, value.var = "scaled") 
MVets_Ton.castFULL$Workforce <- MVets_Ton.castFULL$`1014` - MVets_Ton.castFULL$`0509`

imPROD.castFULL <- dcast( data = imPROD.meltFULL, EORA_a3~variable, value.var = "scaled") 
imPROD.castFULL$Trade <- imPROD.castFULL$`1014` - imPROD.castFULL$`0509`

aPROD.castFULL <- dcast( data = aPROD.meltFULL, ISO3~variable, value.var = "scaled") 
aPROD.castFULL$Production <- aPROD.castFULL$`1014` - aPROD.castFULL$`0509`


# Merge the different indicators
Wmap.data <- merge(adci.castFULL [, c("EORA_a3", "ADCI")], 
                   MVets_Ton.castFULL[, c("EORA_a3", "Workforce")], by = "EORA_a3", all = T)
Wmap.data <- merge( Wmap.data, imPROD.castFULL[, c("EORA_a3", "Trade")], 
                    by = "EORA_a3", all = T)
Wmap.data <- merge( Wmap.data, aPROD.castFULL[, c("ISO3", "Production")], 
                    by.x = "EORA_a3", by.y = "ISO3", all = T)


# Reverse workforce indicator
Wmap.data$WorkforceREV <- Wmap.data$Workforce * (-1)
Wmap.data <- Wmap.data [, !(names(Wmap.data) == "Workforce")]
names(Wmap.data)[ names(Wmap.data) == "WorkforceREV"] <- "Workforce"

# Calculate the mean change
Wmap.data$MEAN <- rowMeans( Wmap.data[, c("ADCI", "Workforce", "Production")], na.rm = T)

Wmap.melt <- melt( Wmap.data, id.vars = "EORA_a3" )
Wmap.final <- Wmap.melt[ complete.cases( Wmap.melt ), ]
