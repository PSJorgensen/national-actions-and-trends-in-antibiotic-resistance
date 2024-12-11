#############################################################################################
#########     DATA PREPARATION FOR ANIMAL STATE - ABX RESISTANCE INDICATORS


aniS.df <- raw.df[ which( raw.df$SPECIES == "ANIMAL" & raw.df$DPSIR == "STATE"), ]

# Rename some of the indicators (adjust to same spelling)
for(i in 1:nrow(aniS.df)){
  if(aniS.df$SHORTNAME[[i]] == "ChickenResistanceEColiNalidixic acid"){
    aniS.df$SHORTNAME[[i]] <- "ChickenResistanceEColiNalidixicAcid"
  }
  if(aniS.df$SHORTNAME[[i]] == "ChickenResistanceEColiSulphonamide"){
    aniS.df$SHORTNAME[[i]] <- "ChickenResistanceEColiSulfonamides"
  }
  if(aniS.df$SHORTNAME[[i]] == "ChickenResistanceEColiTetracyclin" |
     aniS.df$SHORTNAME[[i]] == "ChickenResistanceEColiTetracyclines"){
    aniS.df$SHORTNAME[[i]] <- "ChickenResistanceEColiTetracycline"
  }
  if(aniS.df$SHORTNAME[[i]] == "ChickenResistanceSalmonellaSppSulfonamide"){
    aniS.df$SHORTNAME[[i]] <- "ChickenResistanceSalmonellaSppSulphonamides"
  }
  if(aniS.df$SHORTNAME[[i]] == "ChickenResistanceSalmonellaSppTetracyclines"){
    aniS.df$SHORTNAME[[i]] <- "ChickenResistanceSalmonellaSppTetracycline"
  }
  if(aniS.df$SHORTNAME[[i]] == "RetailChickenResistanceNon-typhoidSalmonelllaCefoxitin"){
    aniS.df$SHORTNAME[[i]] <- "RetailChickenResistanceNon-typhoidSalmonellaCefoxitin"
  }
  if(aniS.df$SHORTNAME[[i]] == "RetailChickenResistanceNon-typhoidSalmonelllaCeftiaxone"){
    aniS.df$SHORTNAME[[i]] <- "RetailChickenResistanceNon-typhoidSalmonellaCeftiaxone"
  }
  if(aniS.df$SHORTNAME[[i]] == "RetailChickenResistanceNon-typhoidSalmonelllaCeftiofur"){
    aniS.df$SHORTNAME[[i]] <- "RetailChickenResistanceNon-typhoidSalmonellaCeftiofur"
  }
  if(aniS.df$SHORTNAME[[i]] == "RetailChickenResistanceNon-typhoidSalmonelllaTetracyclin"){
    aniS.df$SHORTNAME[[i]] <- "RetailChickenResistanceNon-typhoidSalmonellaTetracyclin"
  }
  if(aniS.df$SHORTNAME[[i]] == "RetailChickenResistanceNon-typhoidSalmonelllaAmpicillin"){
    aniS.df$SHORTNAME[[i]] <- "RetailChickenResistanceNon-typhoidSalmonellaAmpicillin"
  }
  
}

aniS.df <- aniS.df[ which( aniS.df$SHORTNAME == "ChickenResistanceEColiCefotaxime" |
                             aniS.df$SHORTNAME == "ChickenResistanceSalmonellaSppCefotaxime" |
                             aniS.df$SHORTNAME == "ChickenResistanceEColiCeftiofur" |
                             aniS.df$SHORTNAME == "ChickenResistanceSalmonellaSppCeftiofur" |
                             aniS.df$SHORTNAME == "ChickenResistanceEColiAmpicillin" |
                             aniS.df$SHORTNAME == "ChickenResistanceSalmonellaSppAmpicillin" |
                             aniS.df$SHORTNAME == "ChickenResistanceEColiTetracycline" |
                             aniS.df$SHORTNAME == "ChickenResistanceSalmonellaSppTetracycline" |
                             # Only UK
                             aniS.df$SHORTNAME == "ChickenResistanceEColiCeftazidime" |  
                             aniS.df$SHORTNAME == "ChickenResistanceEColiColistin" |
                             # US Data 
                             aniS.df$SHORTNAME == "RetailChickenResistanceEColiCefoxitin" |
                             aniS.df$SHORTNAME == "RetailChickenResistanceNon-typhoidSalmonellaCefoxitin" |
                             aniS.df$SHORTNAME == "RetailChickenResistanceEColiCeftiaxone" |
                             aniS.df$SHORTNAME == "RetailChickenResistanceNon-typhoidSalmonellaCeftiaxone" |
                             aniS.df$SHORTNAME == "RetailChickenResistanceEColiCeftiofur" |
                             aniS.df$SHORTNAME == "RetailChickenResistanceNon-typhoidSalmonellaCeftiofur" |
                             aniS.df$SHORTNAME == "RetailChickenResistanceEColiAmpicillin" |
                             aniS.df$SHORTNAME == "RetailChickenResistanceNon-typhoidSalmonellaAmpicillin" |
                             aniS.df$SHORTNAME == "RetailChickenResistanceEColiTetracyclin" |
                             aniS.df$SHORTNAME == "RetailChickenResistanceNon-typhoidSalmonellaTetracyclin"), ]

# Group the different types of ABX and sum within groups
aniS.group <- aniS.df

aniS.group$ABX.GROUP <- NA
aniS.group$path <- NA
aniS.group[ which(aniS.group$SHORTNAME %in% c("ChickenResistanceEColiCefotaxime",
                                              "ChickenResistanceEColiCeftiofur",
                                              "ChickenResistanceEColiCeftazidime",
                                              "RetailChickenResistanceEColiCefoxitin",
                                              "RetailChickenResistanceEColiCeftiaxone",
                                              "RetailChickenResistanceEColiCeftiofur",
                                              "ChickenResistanceSalmonellaSppCefotaxime",
                                              "ChickenResistanceSalmonellaSppCeftiofur",
                                              "RetailChickenResistanceNon-typhoidSalmonellaCefoxitin",
                                              "RetailChickenResistanceNon-typhoidSalmonellaCeftiaxone",
                                              "RetailChickenResistanceNon-typhoidSalmonellaCeftiofur")), "ABX.GROUP"] <- "ceph"

aniS.group[ which(aniS.group$SHORTNAME %in% c("ChickenResistanceEColiColistin")), "ABX.GROUP"] <- "colistin"

aniS.group[ which(aniS.group$SHORTNAME %in% c("ChickenResistanceEColiAmpicillin",
                                              "RetailChickenResistanceEColiAmpicillin",
                                              "ChickenResistanceSalmonellaSppAmpicillin",
                                              "RetailChickenResistanceNon-typhoidSalmonellaAmpicillin")), "ABX.GROUP"] <- "ampicillin"

aniS.group[ which(aniS.group$SHORTNAME %in% c("ChickenResistanceEColiTetracycline",
                                              "ChickenResistanceSalmonellaSppTetracycline",
                                              "RetailChickenResistanceEColiTetracyclin",
                                              "RetailChickenResistanceNon-typhoidSalmonellaTetracyclin")), "ABX.GROUP"] <- "tetra"

aniS.group[ which( grepl("EColi", aniS.group$SHORTNAME)), "path"] <- "EColi"
aniS.group[ which( grepl("Salmonella", aniS.group$SHORTNAME)), "path"] <- "Salmonella"

# Sum up abx for each year and country
aniS.group$Value <- as.numeric( as.character( aniS.group$Value ) )
aniS.sum <- aggregate( data = aniS.group, Value~Country+ISO3+Year+ABX.GROUP+path, 
                       function(x) sum(x) )


# For each group and country, calculate mean before & after 2010 & difference in means
aniS.mean0309 <- aggregate( data = aniS.sum [ which( aniS.sum$Year < 2010 ),], 
                            Value~Country+ISO3+ABX.GROUP+path,
                            function(x) mean(x))
names(aniS.mean0309)[ names(aniS.mean0309) == "Value"] <- "0309"

aniS.mean1016 <- aggregate( data = aniS.sum [ which( aniS.sum$Year >= 2010 ),], 
                            Value~Country+ISO3+ABX.GROUP+path,
                            function(x) mean(x))
names(aniS.mean1016)[ names(aniS.mean1016) == "Value"] <- "1016"

aniS.mean <- merge( aniS.mean0309, aniS.mean1016, by = c("Country", "ISO3", "ABX.GROUP", "path"),
                    all = T)

aniS.mean$`0309.1016` <- aniS.mean$`1016` - aniS.mean$`0309`


# Calculate mean change in Ecoli resistance and Salmonella resistance for each country
aniS.meanTOTAL <- aggregate( data = aniS.mean, `0309.1016`~Country+ISO3+path,
                             function(x) mean(x))
aniS.meanTOTAL$ABX.GROUP <- NA
aniS.meanTOTAL[ which( aniS.meanTOTAL$path == "EColi"), "ABX.GROUP"] <- "MEAN.EColi"
aniS.meanTOTAL[ which( aniS.meanTOTAL$path == "Salmonella"), "ABX.GROUP"] <- "MEAN.Salmonella"


# Scale & center change in mean for each abx group & pathogen
aniS.mean2 <- aniS.mean [, !names(aniS.mean) %in% c("0309", "1016")]
aniS.mean2 <- aniS.mean2[ complete.cases(aniS.mean2), ]

# Merge with the means before scaling
aniS.mean2 <- merge(aniS.mean2, aniS.meanTOTAL, all = T)

aniS.scale <- aniS.mean2

aniS.scale.cast <-  dcast(aniS.scale, Country+ISO3~ABX.GROUP+path, value.var = "0309.1016")
aniS.scale.cast$cephE.scaled <- scale( aniS.scale.cast$ceph_EColi, center = T, scale = T)
aniS.scale.cast$cephS.scaled <- scale( aniS.scale.cast$ceph_Salmonella, center = T, scale = T)
aniS.scale.cast$ampiE.scaled <- scale( aniS.scale.cast$ampicillin_EColi, center = T, scale = T)
aniS.scale.cast$ampiS.scaled <- scale( aniS.scale.cast$ampicillin_Salmonella, center = T, scale = T)
aniS.scale.cast$tetraE.scaled <- scale( aniS.scale.cast$tetra_EColi, center = T, scale = T)
aniS.scale.cast$tetraS.scaled <- scale( aniS.scale.cast$tetra_Salmonella, center = T, scale = T)
aniS.scale.cast$MEANS.scaled <- scale( aniS.scale.cast$MEAN.Salmonella_Salmonella, center = T, scale = T)
aniS.scale.cast$MEANE.scaled <- scale( aniS.scale.cast$MEAN.EColi_EColi, center = T, scale = T)

aniS.scale.cast <- aniS.scale.cast[ , names(aniS.scale.cast) %in% c("Country", "ISO3") |
                                      grepl(".scaled", names(aniS.scale.cast) ) ]


aniS.scale.melt <- melt( aniS.scale.cast, id.vars = c("Country", "ISO3") )
aniS.scale.melt <- aniS.scale.melt [complete.cases( aniS.scale.melt ), ]


######            Prepare for plotting (boxplots & violine plots)
aniS.scale.final <-  aniS.scale.melt

aniS.scale.final$variable <- as.character( aniS.scale.final$variable )
aniS.scale.final$variable <- factor( aniS.scale.final$variable )


# For the p-values use non-scaled DF
aniS.pVAL <- aniS.mean2 [, ! names(aniS.mean2) %in% c("0309", "1016")]
aniS.pVAL.cast <- dcast( aniS.pVAL, Country+ISO3~ABX.GROUP+path, value.var = "0309.1016")
aniS.pVAL.melt <- melt( aniS.pVAL.cast, id.vars = c("Country", "ISO3") )
aniS.pVAL.final <- aniS.pVAL.melt[ complete.cases( aniS.pVAL.melt ), ]

ASp.df <- ddply(aniS.pVAL.final,.(variable), summarise,
                pval = summary (glm (value ~ 1 ) )$coefficients[1,4],
                N=length(is.na(value)==FALSE))





######            BARPLOTS

### Preparing  the barplots (also using non-scaled values)
aniS.bar <- aniS.pVAL.final

# For each indicator/ variable. Calculate proportion of countries with increase/decrease
aniS.bar$sign <- NA
aniS.bar[ which(aniS.bar$value > 0), "sign"] <- "pos"
prop.pos.df <- aggregate( data = aniS.bar, sign~variable,
                          function(x) length(x))
names(prop.pos.df)[names(prop.pos.df) == "sign"] <- "Nb.pos"

aniS.bar$sign <- NA
aniS.bar[ which(aniS.bar$value < 0), "sign"] <- "neg"
prop.neg.df <- aggregate( data = aniS.bar, sign~variable,
                          function(x) length(x))
names(prop.neg.df)[names(prop.neg.df) == "sign"] <- "Nb.neg"

prop.final <- merge(prop.pos.df, prop.neg.df, by = "variable")
prop.final$prop.pos <- prop.final$Nb.pos/(prop.final$Nb.pos + prop.final$Nb.neg)
prop.final$prop.neg <- prop.final$Nb.neg/(prop.final$Nb.pos + prop.final$Nb.neg)

aniSbar.df <- merge(aniS.bar, prop.final,
                    by = "variable", all = T)
aniSbar.df[ which(aniSbar.df$value > 0), "sign"] <- "pos"

aniSbar.df$prop <- NA
aniSbar.df$N <- NA

# Drop countries with zero mean change
aniSbar.df <- aniSbar.df[ complete.cases( aniSbar.df$sign ), ]

for(i in 1:nrow(aniSbar.df)){
  if(aniSbar.df$sign[[i]] == "pos"){
    aniSbar.df$prop[[i]] <- aniSbar.df$prop.pos[[i]]
  }
  if(aniSbar.df$sign[[i]] == "neg"){
    aniSbar.df$prop[[i]] <- aniSbar.df$prop.neg[[i]]
  }
  if(aniSbar.df$sign[[i]] == "pos"){
    aniSbar.df$N[[i]] <- aniSbar.df$Nb.pos[[i]]
  }
  if(aniSbar.df$sign[[i]] == "neg"){
    aniSbar.df$N[[i]] <- aniSbar.df$Nb.neg[[i]]
  }
}

aniSbar.df <- aniSbar.df [ !names(aniSbar.df) %in% c("prop.pos", "prop.neg", "Nb.pos", "Nb.neg")]


aniSbar.final <- aniSbar.df[!duplicated( aniSbar.df[c("variable","sign")] ), ]
aniSbar.final <- aniSbar.final[!names(aniSbar.final) %in% c("EORA_a3", "value")]



# Chi-Square test for every indicator
p.df2 <- ddply( aniSbar.df, .(variable), summarise,
                pval = chisq.test(N)$p.value)


aniSbar.final <- merge(aniSbar.final, p.df2, by = "variable", all = T)




### Create data frame for EARLY MEANS
early.mean <- aggregate( data = aniS.sum [ which( aniS.sum$Year <= 2005 ),], 
                            Value~Country+ISO3+ABX.GROUP+path,
                            function(x) mean(x))
early.meanCOMB <- aggregate( data = early.mean, Value~Country+ISO3+path,
                             function(x) mean(x) )
early.cast <- dcast( data = early.meanCOMB, Country+ISO3~path, value.var = "Value")
early.cast$EColi.scaled <- scale( early.cast$EColi, center = T, scale = T)
early.cast$Salmonella.scaled <- scale( early.cast$Salmonella, center = T, scale = T)

early.melt <- melt( early.cast, id.vars = c("Country", "ISO3") )
early.melt$cat <- NA
early.melt [ which( grepl( ".scaled", early.melt$variable)), "cat"] <- "scaled"
early.melt [ ! (grepl( ".scaled", early.melt$variable)), "cat"] <- "non.scaled"

early.meanSTA <- early.melt[ complete.cases( early.melt$value ), ]
