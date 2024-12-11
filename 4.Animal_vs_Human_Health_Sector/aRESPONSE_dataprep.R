#########################################################################################
#############       RESPONSES TO ANIMAL D, P, S

# D, P, S data is already prepared, take from existing DF
dps.df <- violin.df[ which(violin.df$SHORTNAME == "MEAN" |
                             violin.df$SHORTNAME == "Total"),]
dps.df <- dps.df [, !(names(dps.df) %in% c("variable", "alpha"))]


# Add response data from raw.df (mean response across all indicators - except NAP)
aniR.df <- raw.df[ which( raw.df$DPSIR == "RESPONSE" & raw.df$SHORTNAME != "NAP published"),]

aniR.df$Value <- as.numeric( as.character( aniR.df$Value ) )
res_shortnames <- c("AMR Training (Human Health)", 
                                     "AMR Surveillance System (Humans)" ,
                                     "Monitoring System for AMU (Human Health)", 
                                     "AMR Awareness (Human Health)", 
                                     "AMS and Regulation (Human)",
                                      "AMR Training (Animal Health and Food Prod)",
                                      "Monitoring System for AMU (Animals and Crop)",
                                      "AMR Awareness (Animal Health and Food Prod)",
                                      "AMS and Regulation (Ani&Crop)",
                                      "AMR Surveillance System (Animals and Foods)")
  
aniR.df2 <- aniR.df[aniR.df$SHORTNAME %in% res_shortnames, ]

aniR.mean <- aggregate( data = aniR.df2, Value~ISO3+Country, function(x) mean(x) )

aniR.final <- merge(dps.df, aniR.mean, by = c("Country", "ISO3"), all.x = T)
names(aniR.final)[ names(aniR.final) == "Value" ] <- "CODE"




#####      Add the EARLY MEAN values ("early.meanDRI", "early.meanPRE", "early.meanSTA")
# Note: PRE and STA have both scaled & unscaled means
names(early.meanDRI)[ names(early.meanDRI) == "value" ] <- "MEAN.DRI0005"
for(i in 1:nrow(early.meanDRI)){
  if(early.meanDRI$EORA_a3[[i]] == "CZE"){
    early.meanDRI$Country[[i]] <- "Czech Republic"
  }
  if(early.meanDRI$EORA_a3[[i]] == "NLD"){
    early.meanDRI$Country[[i]] <- "The Netherlands"
  }
  if(early.meanDRI$EORA_a3[[i]] == "USA"){
    early.meanDRI$Country[[i]] <- "United States"
  }
}

# For the plot use only the scaled means
early.meanPRE_Sc <- aggregate( data = early.meanPRE[ which( early.meanPRE$cat == "scaled" ), ],
                               value~Country, function(x) mean(x) )
names(early.meanPRE_Sc)[ names(early.meanPRE_Sc) == "value" ] <- "MEAN.PRE0005"

early.meanSTA_Sc <- aggregate( data = early.meanSTA[ which( early.meanSTA$cat == "scaled" ), ],
                               value~Country+ISO3+variable, function(x) mean(x) )
names(early.meanSTA_Sc)[ names(early.meanSTA_Sc) == "value" ] <- "MEAN.STA0005"
early.meanSTA_Sc <- dcast( early.meanSTA_Sc, Country+ISO3~variable, value.var = "MEAN.STA0005")

# Merge D, P and S together
early.df <- merge( early.meanDRI, early.meanPRE_Sc, by = "Country", all = T)
early.df <- merge( early.df, early.meanSTA_Sc, by.x = c("EORA_a3", "Country"), 
                   by.y = c("ISO3", "Country"), all = T)
names(early.df)[ names(early.df) == "EColi.scaled"] <- "MEAN.STA.E0005"
names(early.df)[ names(early.df) == "Salmonella.scaled"] <- "MEAN.STA.S0005"

early.melt <- melt( early.df, id.vars = c("EORA_a3", "Country") )


# Merge with mean response data
early.final <- merge( early.melt, aniR.final[, c("Country", "ISO3", "CODE")], 
                      by.x = c("Country", "EORA_a3"), by.y = c("Country", "ISO3"),
                      all.x = TRUE)
early.final <- early.final[ complete.cases( early.final ), ]

# Add a DPSIR column
early.final$DPSIR <- NA
early.final [ which( early.final$variable == "MEAN.DRI0005"), "DPSIR" ] <- "DRIVER"
early.final [ which( early.final$variable == "MEAN.PRE0005"), "DPSIR" ] <- "PRESSURE"
early.final [ which( early.final$variable == "MEAN.STA.E0005"), "DPSIR" ] <- "STATE.EColi"
early.final [ which( early.final$variable == "MEAN.STA.S0005"), "DPSIR" ] <- "STATE.Salmonella"




# Build a DF for the third row (sign of change)
sign.df <- aniR.final
sign.df$sign <- NA
sign.df [ which( sign.df$value < 0 ), "sign" ] <- -1
sign.df [ which( sign.df$value > 0 ), "sign" ] <- 1

