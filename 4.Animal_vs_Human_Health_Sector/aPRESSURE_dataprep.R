#############################################################################################
#########     DATA PREPARATION FOR ANIMAL PRESSURE - ABX USE INDICATORS

#library(reshape2); library(ggplot2); library(plyr)

#raw.df <- read.csv2('RawNumbers_v09.csv', stringsAsFactors = FALSE)

#key.df <- read.csv2("CountryKey_New.csv", stringsAsFactors = F)

# Use total, cephalosporins, polymyxins & tetracycline
aniP.df <- raw.df[ which(raw.df$SHORTNAME == "Tetracyclines.ABXUseKgPerTonMeat&Fish" |
                           raw.df$SHORTNAME == "Total.ABXUseKgPerTonMeat&Fish" |
                           raw.df$SHORTNAME == "Polymyxins.ABXUseKgPerTonMeat&Fish" |
                           raw.df$SHORTNAME == "Cephalosporins.ABXUseKgPerTonMeat&Fish" |
                           raw.df$SHORTNAME == "ceph12.ABXUseKgPerTonMeat&Fish" |
                           raw.df$SHORTNAME == "ceph34.ABXUseKgPerTonMeat&Fish" |
                           raw.df$SHORTNAME == "Fluoroquinolones.ABXUseKgPerTonMeat&Fish" ), ]

aniP.cast <- dcast( aniP.df, Country+Year~SHORTNAME, value.var = "Value" )
aniP.cast$CEPH <- aniP.cast$`Cephalosporins.ABXUseKgPerTonMeat&Fish`
aniP.cast$`ceph12.ABXUseKgPerTonMeat&Fish` <- as.numeric( as.character( 
  aniP.cast$`ceph12.ABXUseKgPerTonMeat&Fish` ) )
aniP.cast$`ceph34.ABXUseKgPerTonMeat&Fish` <- as.numeric( as.character( 
  aniP.cast$`ceph34.ABXUseKgPerTonMeat&Fish` ) )
for(i in 1:nrow(aniP.cast)){
  if( is.na( aniP.cast$`Cephalosporins.ABXUseKgPerTonMeat&Fish`[[i]])){
    aniP.cast$CEPH [[i]] <- aniP.cast$`ceph12.ABXUseKgPerTonMeat&Fish`[[i]] + 
                            aniP.cast$`ceph34.ABXUseKgPerTonMeat&Fish`[[i]]
  }
}

aniP.cast <- aniP.cast[, !(names(aniP.cast) %in% c("ceph12.ABXUseKgPerTonMeat&Fish", 
                                                   "ceph34.ABXUseKgPerTonMeat&Fish",
                                                   "Cephalosporins.ABXUseKgPerTonMeat&Fish"))]
names(aniP.cast)[ names(aniP.cast) == "CEPH"] <- "Cephalosporins.ABXUseKgPerTonMeat&Fish"

aniP.melt <- melt( aniP.cast, id.vars = c("Country", "Year") )
aniP.melt <- aniP.melt[ complete.cases(aniP.melt), ]

# Calculate mean before and after 2010 for each country (including 2010 in both means!)
aniP.melt$value <- as.numeric( as.character( aniP.melt$value ) )
aniP.mean0010 <- aggregate( data = aniP.melt[ which(aniP.melt$Year <= 2010),], 
                         value~Country+variable, function(x) mean(x))
names(aniP.mean0010)[names(aniP.mean0010) == "value"] <- "0010"

aniP.mean1014 <- aggregate( data = aniP.melt[ which(aniP.melt$Year >= 2010 ) , ], 
                            value~Country+variable, function(x) mean(x))
names(aniP.mean1014)[names(aniP.mean1014) == "value"] <- "1014"

aniP.mean <- merge(aniP.mean0010, aniP.mean1014, by = c("Country", "variable"), all = T)

# Calculate mean change between the two periods for each country
aniP.mean$`0010.1014` <- aniP.mean$`1014` - aniP.mean$`0010`

aniP.final <- aniP.mean[, names(aniP.mean) %in% c("Country", "variable", "0010.1014")]
aniP.final <- aniP.final [ complete.cases(aniP.final), ]

aniP.final <- merge(aniP.final, key.df[, c("EORA_a3", "EORA_name")], by.x = "Country",
                    by.y = "EORA_name", all = T)
aniP.final <- aniP.final[ complete.cases( aniP.final$variable ), ]

for(i in 1:nrow(aniP.final)){
  if(aniP.final$Country [[i]] == "United States"){
    aniP.final$EORA_a3 [[i]] <- "USA"
  }
  if(aniP.final$Country [[i]] == "United Kingdom"){
    aniP.final$EORA_a3 [[i]] <- "GBR"
  }
}

aniP.final$alpha <- NA
aniP.final [ which(aniP.final [, "variable"]%in%c("Total.ABXUseKgPerTonMeat&Fish")), "alpha"] <- 1
aniP.final [ which(!aniP.final [, "variable"]%in%c("Total.ABXUseKgPerTonMeat&Fish")), "alpha"] <- 0.6


# P-Values
APp.df <- ddply(aniP.final,.(variable), summarise,
              pval = summary (glm (`0010.1014` ~ 1 ) )$coefficients[1,4],
              N=length(is.na(`0010.1014`) == FALSE) )
APp.df$alpha <- NA
APp.df [ which(APp.df [, "variable"]%in%c("Total.ABXUseKgPerTonMeat&Fish")), "alpha"] <- 1
APp.df [ which(!APp.df [, "variable"]%in%c("Total.ABXUseKgPerTonMeat&Fish")), "alpha"] <- 0.6


# For the plot, scale the data 
aniP.scaled <- aniP.cast

aniP.scaled$Total.scaled <- scale( as.numeric( as.character( aniP.scaled$`Total.ABXUseKgPerTonMeat&Fish` )),
                                   center = T, scale = T)
aniP.scaled$Ceph.scaled <- scale( as.numeric( as.character( aniP.scaled$`Cephalosporins.ABXUseKgPerTonMeat&Fish` )),
                                   center = T, scale = T)
aniP.scaled$Polymyxin.scaled <- scale( as.numeric( as.character( aniP.scaled$`Polymyxins.ABXUseKgPerTonMeat&Fish` )),
                                   center = T, scale = T)
aniP.scaled$Tetracycline.scaled <- scale( as.numeric( as.character( aniP.scaled$`Tetracyclines.ABXUseKgPerTonMeat&Fish` )),
                                   center = T, scale = T)
aniP.scaled$Fluoroquinolones.scaled <- scale( as.numeric( as.character( aniP.scaled$`Fluoroquinolones.ABXUseKgPerTonMeat&Fish` )),
                                          center = T, scale = T)

# Calculate means and mean change
aniP.scaled <- aniP.scaled[, names(aniP.scaled) %in% c("Country", "Year", "Total.scaled", "Ceph.scaled",
                                                     "Polymyxin.scaled", "Tetracycline.scaled",
                                                     "Fluoroquinolones.scaled")]
aniP.scaled <- melt( aniP.scaled, id.vars = c("Country", "Year") )
aniP.mean0010.scale <- aggregate( data = aniP.scaled[ which( aniP.scaled$Year <= 2010), ], 
                            value~Country+variable, function(x) mean(x) )
names(aniP.mean0010.scale)[names(aniP.mean0010.scale) == "value"] <- "0010"
aniP.mean1014.scale <- aggregate( data = aniP.scaled[ which( aniP.scaled$Year >= 2010), ], 
                            value~Country+variable, function(x) mean(x) )
names(aniP.mean1014.scale)[names(aniP.mean1014.scale) == "value"] <- "1014"

aniP.plot <- merge(aniP.mean0010.scale, aniP.mean1014.scale, by = c("Country", "variable"), all = T)
aniP.plot$`0010.1014` <- aniP.plot$`1014` - aniP.plot$`0010`
aniP.plot <- aniP.plot[ complete.cases( aniP.plot$`0010.1014`), !(names(aniP.plot) %in% c("0010", "1014"))]

aniP.plot$alpha <- NA
aniP.plot [ which( aniP.plot [, "variable"]%in%c("Total.scaled")), "alpha"] <- 1
aniP.plot [ which( !aniP.plot [, "variable"]%in%c("Total.scaled")), "alpha"] <- 0.6




### Create EARLY MEAN DF
early.mean <- aggregate( data = aniP.melt[ which(aniP.melt$Year <= 2005),], 
                            value~Country+variable, function(x) mean(x))
early.cast <- dcast( data = early.mean, Country~variable, value.var = "value" )

early.cast$`Fluoroquinolones.scaled` <- scale( early.cast$`Fluoroquinolones.ABXUseKgPerTonMeat&Fish`,
                                               center = T, scale = T )
early.cast$`Polymyxins.scaled` <- scale( early.cast$`Polymyxins.ABXUseKgPerTonMeat&Fish`,
                                               center = T, scale = T )
early.cast$`Tetracyclines.scaled` <- scale( early.cast$`Tetracyclines.ABXUseKgPerTonMeat&Fish`,
                                               center = T, scale = T )
early.cast$`Total.scaled` <- scale( early.cast$`Total.ABXUseKgPerTonMeat&Fish`,
                                               center = T, scale = T )
early.cast$`Cephalosporins.scaled` <- scale( early.cast$`Cephalosporins.ABXUseKgPerTonMeat&Fish`,
                                    center = T, scale = T )
early.melt <- melt( early.cast, id.vars = "Country")
early.melt$cat <- NA
early.melt [which( grepl(".scaled", early.melt$variable) ), "cat" ] <- "scaled"
early.melt [ !( grepl(".scaled", early.melt$variable) ), "cat" ] <- "non.scaled"

early.meanPRE <- early.melt [ complete.cases( early.melt$value), ]



################################################################################################
#############       PREPARING DF FOR THE BARPLOT

### Preparing  the barplots
aniP.bar <- aniP.plot

# For each indicator/ variable. Calculate proportion of countries with increase/decrease
aniP.bar$sign <- NA
aniP.bar[ which(aniP.bar$`0010.1014` > 0), "sign"] <- "pos"
prop.pos.df <- aggregate( data = aniP.bar, sign~variable,
                          function(x) length(x))
names(prop.pos.df)[names(prop.pos.df) == "sign"] <- "Nb.pos"

aniP.bar$sign <- NA
aniP.bar[ which(aniP.bar$`0010.1014` < 0), "sign"] <- "neg"
prop.neg.df <- aggregate( data = aniP.bar, sign~variable,
                          function(x) length(x))
names(prop.neg.df)[names(prop.neg.df) == "sign"] <- "Nb.neg"

prop.final <- merge(prop.pos.df, prop.neg.df, by = "variable")
prop.final$prop.pos <- prop.final$Nb.pos/(prop.final$Nb.pos + prop.final$Nb.neg)
prop.final$prop.neg <- prop.final$Nb.neg/(prop.final$Nb.pos + prop.final$Nb.neg)

aniPbar.df <- merge(aniP.bar, prop.final,
                    by = "variable", all = T)
aniPbar.df[ which(aniPbar.df$`0010.1014` > 0), "sign"] <- "pos"

aniPbar.df$prop <- NA
aniPbar.df$N <- NA
for(i in 1:nrow(aniPbar.df)){
  if(aniPbar.df$sign[[i]] == "pos"){
    aniPbar.df$prop[[i]] <- aniPbar.df$prop.pos[[i]]
  }
  if(aniPbar.df$sign[[i]] == "neg"){
    aniPbar.df$prop[[i]] <- aniPbar.df$prop.neg[[i]]
  }
  if(aniPbar.df$sign[[i]] == "pos"){
    aniPbar.df$N[[i]] <- aniPbar.df$Nb.pos[[i]]
  }
  if(aniPbar.df$sign[[i]] == "neg"){
    aniPbar.df$N[[i]] <- aniPbar.df$Nb.neg[[i]]
  }
}

aniPbar.df <- aniPbar.df [ !names(aniPbar.df) %in% c("prop.pos", "prop.neg", "Nb.pos", "Nb.neg")]




aniPbar.final <- aniPbar.df[!duplicated( aniPbar.df[c("variable","sign")] ), ]
#aniPbar.final <- aniPbar.final[!names(aniPbar.final) %in% c("EORA_a3", "value", "info")]


aniPbar.final$variable <- ordered( aniPbar.final$variable,
                                   levels = c("Total.scaled", "Ceph.scaled", "Polymyxin.scaled", 
                                              "Tetracycline.scaled", "Fluoroquinolones.scaled") )

# Chi-Square test for every indicator
p.df2 <- ddply( aniPbar.df, .(variable), summarise,
                pval = chisq.test(N)$p.value)


aniPbar.final <- merge(aniPbar.final, p.df2, by = "variable", all = T)

aniPbar.final$SHORTNAME <- NA
aniPbar.final[ which(aniPbar.final$variable == "Ceph.scaled"), "SHORTNAME" ] <- "Cephalosporins"
aniPbar.final[ which(aniPbar.final$variable == "Fluoroquinolones.scaled"), 
               "SHORTNAME" ] <- "Fluoroquinolones"
aniPbar.final[ which(aniPbar.final$variable == "Polymyxin.scaled"), "SHORTNAME" ] <- "Polymyxins"
aniPbar.final[ which(aniPbar.final$variable == "Tetracycline.scaled"), "SHORTNAME" ] <- "Tetracycline"
aniPbar.final[ which(aniPbar.final$variable == "Total.scaled"), "SHORTNAME" ] <- "Total"

aniPbar.final <- aniPbar.final[ , !(names(aniPbar.final) %in% c("variable")) ]
names(aniPbar.final)[ names(aniPbar.final) == "SHORTNAME"] <- "variable"