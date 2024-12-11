#############################################################################################
############        COLLECT DRIVERS, PRESSURES & STATE IN ONE DF FOR facet_wrap()



# Merge the DFs needed for the box/violinplots
aDRI.plotNEW <- aDRI.plot
aDRI.plotNEW$DPSIR <- "DRIVER"
aniP.plot$DPSIR <- "PRESSURE"
aniS.scale.final$DPSIR <- NA

aniS.scale.final[ which( grepl("E", aniS.scale.final$variable)), "DPSIR"] <- "STATE.EColi" 
aniS.scale.final[ which( grepl("S", aniS.scale.final$variable)), "DPSIR"] <- "STATE.Salmonella" 


names(aniP.plot)[ names(aniP.plot) == "0010.1014" ] <- "value"
names(aDRI.plotNEW)[ names(aDRI.plotNEW) == "EORA_a3" ] <- "ISO3"

aDRI.ISO <- merge(aDRI.plotNEW, key.df[, c("EORA_a3", "EORA_name")], by.x = "ISO3",
                  by.y = "EORA_a3", all.x = T)
for(i in 1:nrow(aDRI.ISO)){
  if(aDRI.ISO$ISO3[[i]] == "CZE"){
    aDRI.ISO$EORA_name[[i]] <- "Czech Republic"
  }
}
names(aDRI.ISO)[ names(aDRI.ISO) == "EORA_name"] <- "Country"

violin.df <- merge(aDRI.ISO[, !( names(aDRI.ISO) %in% c("info"))], aniS.scale.final,
                    all = T)
violin.df <- violin.df[ complete.cases(violin.df$variable),]

aniP.ISO <- merge(aniP.plot, key.df[, c("EORA_a3", "EORA_name")], by.x = "Country",
                  by.y = "EORA_name", all.x = T)
for(i in 1:nrow(aniP.ISO)){
  if(aniP.ISO$Country[[i]] == "United Kingdom" ){
    aniP.ISO$EORA_a3[[i]] <- "GBR"
  }
  if(aniP.ISO$Country[[i]] == "United States" ){
    aniP.ISO$EORA_a3[[i]] <- "USA"
  }
}
names(aniP.ISO)[ names(aniP.ISO) == "EORA_a3"] <- "ISO3"

violin.df <- merge( violin.df, aniP.ISO, all = T)

# Add missing values for alpha
for(i in 1:nrow(violin.df)){
  if( is.na(violin.df$alpha[[i]] ) ){
    violin.df$alpha[[i]] <- 0.6
  }
}

# Prepare DF for plotting
violin.df$value <- as.numeric( as.character( violin.df$value ) )

### Add fake numbers for ciprofloxacin  to have the plots in line
violin.df$variable <- as.character( violin.df$variable )

violin.df[nrow(violin.df) + 1,] = c( "XXX", "cipro_EColi", 1000, 0.6, "STATE.EColi", "Fake")
violin.df[nrow(violin.df) + 1,] = c( "XXX1", "cipro_Salmonella", 1000, 0.6, "STATE.Salmonella", "Fake1")

for(i in 1:nrow( violin.df )){
  if(violin.df$variable[[i]] == "MEAN\n EColi"){
    violin.df$alpha[[i]] <- 1
  }
  if(violin.df$variable[[i]] == "MEAN\n Salm"){
    violin.df$alpha[[i]] <- 1
  }
}

violin.df$variable <- factor( violin.df$variable )

# Change variable names (not possible to label identically afterwards)
violin.df$variable <- as.character( violin.df$variable )
violin.df$SHORTNAME <- violin.df$variable
for(i in 1:nrow(violin.df)){
  if(violin.df$variable[[i]] == "MEAN\n EColi"){
    violin.df$SHORTNAME[[i]] <- "MEAN"
  }
  if(violin.df$variable[[i]] == "MEAN\n Salm"){
    violin.df$SHORTNAME[[i]] <- "MEAN"
  }
  if(violin.df$variable[[i]] == "Tetracycline.scaled"){
    violin.df$SHORTNAME[[i]] <- "Tetracyclines"
  }
  if(violin.df$variable[[i]] == "Tetracycline"){
    violin.df$SHORTNAME[[i]] <- "Tetracyclines"
  }
  if(violin.df$variable[[i]] == "tetra\n EColi"){
    violin.df$SHORTNAME[[i]] <- "Tetracyclines"
  }
  if(violin.df$variable[[i]] == "tetra\n Salm"){
    violin.df$SHORTNAME[[i]] <- "Tetracyclines"
  }
  if(violin.df$variable[[i]] == "Cephalosporins"){
    violin.df$SHORTNAME[[i]] <- "Cephalosporins"
  } 
  if(violin.df$variable[[i]] == "ceph\n EColi"){
    violin.df$SHORTNAME[[i]] <- "Cephalosporins"
  } 
  if(violin.df$variable[[i]] == "ceph\n Salm"){
    violin.df$SHORTNAME[[i]] <- "Cephalosporins"
  } 
  if(violin.df$variable[[i]] == "cipro_EColi"){
    violin.df$SHORTNAME[[i]] <- "Ciprofloxacin"
  } 
  if(violin.df$variable[[i]] == "cipro_Salmonella"){
    violin.df$SHORTNAME[[i]] <- "Ciprofloxacin"
  } 
  if(violin.df$variable[[i]] == "ampi\n EColi"){
    violin.df$SHORTNAME[[i]] <- "Ampicillin"
  } 
  if(violin.df$variable[[i]] == "ampi\n Salm"){
    violin.df$SHORTNAME[[i]] <- "Ampicillin"
  } 
  if(violin.df$variable[[i]] == "Fluoroquinolones"){
    violin.df$SHORTNAME[[i]] <- "Fluoroquinolones"
  } 
  if(violin.df$variable[[i]] == "Polymyxins"){
    violin.df$SHORTNAME[[i]] <- "Polymyxin"
  } 
  #if(violin.df$variable[[i]] == "Polymyxins"){
   # violin.df$SHORTNAME[[i]] <- "Polymyxin"
  #} 
  if(violin.df$variable[[i]] == "Total"){
    violin.df$SHORTNAME[[i]] <- "Total"
  } 
}

violin.df$SHORTNAME <- factor(violin.df$SHORTNAME)
violin.df$value <- as.numeric( as.character( violin.df$value ) )

violin.df$SHORTNAME <- ordered ( violin.df$SHORTNAME,
                              levels = c("MEAN", "Production","Workforce", "Infections", "Trade",
                                         "Total", "Tetracyclines", "Cephalosporins",
                                         "Fluoroquinolones", "Polymyxin",
                                         "Ciprofloxacin", "Ampicillin") ) 
   






# Merge DFs for the p-values
p.df <- merge(ADp.df, APp.df, all = T)
p.df <- merge(p.df, ASp.df, all = T)
p.df$DPSIR <- NA
p.df[ which( p.df$variable == "Workforce" |
               p.df$variable == "Infections" |
               p.df$variable == "Trade" |
               p.df$variable == "Production" |
               p.df$variable == "MEAN"), "DPSIR"] <- "DRIVER"
p.df[ which( p.df$variable == "Fluoroquinolones"|
               p.df$variable == "Polymyxins" |
               p.df$variable == "Tetracycline" |
               p.df$variable == "Total" |
               p.df$variable == "Cephalosporins" ), "DPSIR"] <- "PRESSURE"
p.df[ which( p.df$variable == "ampi\n EColi" |
               p.df$variable == "ceph\n EColi" |
               p.df$variable == "tetra\n EColi"|
               #p.df$variable == "cephE.scaled" |
               p.df$variable == "MEAN\n EColi"), #|
               #p.df$variable == "tetraE.scaled" |
               #p.df$variable == "ampiE.scaled"), 
             "DPSIR"] <- "STATE.EColi"
p.df[ which(  p.df$variable == "ampi\n Salm" |
                p.df$variable == "ceph\n Salm" |
                p.df$variable == "tetra\n Salm"|
               # p.df$variable == "cephS.scaled" |
                p.df$variable == "MEAN\n Salm"), #|
               # p.df$variable == "tetraS.scaled"|
               # p.df$variable == "ampiS.scaled"), 
      "DPSIR"] <- "STATE.Salmonella"

# Adjust variable names to violin.df names
p.df$variable <- as.character( p.df$variable )

for(i in 1:nrow(p.df)){
  if(p.df$variable[[i]] == "Fluoroquinolones"){
    p.df$variable[[i]] <- "Fluoroquinolones.scaled" 
  }
  if(p.df$variable[[i]] == "Polymyxins"){
    p.df$variable[[i]] <- "Polymyxin.scaled" 
  }
  if(p.df$variable[[i]] == "Tetracycline"){
    p.df$variable[[i]] <- "Tetracycline.scaled" 
  }
  if(p.df$variable[[i]] == "Total"){
    p.df$variable[[i]] <- "Total.scaled" 
  }
  if(p.df$variable[[i]] == "Cephalosporins"){
    p.df$variable[[i]] <- "Ceph.scaled" 
  }
  if(p.df$variable[[i]] == "ampi\n EColi"){
    p.df$variable[[i]] <- "ampiE.scaled" 
  }
  if(p.df$variable[[i]] == "ampi\n Salm"){
    p.df$variable[[i]] <- "ampiS.scaled" 
  }
  if(p.df$variable[[i]] == "ceph\n EColi"){
    p.df$variable[[i]] <- "cephE.scaled" 
  }
  if(p.df$variable[[i]] == "ceph_Salmonella"){
    p.df$variable[[i]] <- "cephS.scaled" 
  }
  if(p.df$variable[[i]] == "tetra\n EColi"){
    p.df$variable[[i]] <- "tetraE.scaled" 
  }
  if(p.df$variable[[i]] == "tetra\n Salm"){
    p.df$variable[[i]] <- "tetraS.scaled" 
  }
  if(p.df$variable[[i]] == "MEAN\n EColi"){
    p.df$variable[[i]] <- "MEANE.scaled"
    p.df$DPSIR[[i]] <- "STATE.EColi"
  }
  if(p.df$variable[[i]] == "MEAN\n Salm"){
    p.df$variable[[i]] <- "MEANS.scaled"
    p.df$DPSIR[[i]] <- "STATE.Salmonella"
  }
}

p.df[nrow(p.df) + 1,] = c( "cipro_EColi", NA, NA, NA, "STATE.EColi")
p.df[nrow(p.df) + 1,] = c( "cipro_Salmonella", NA, NA, NA, "STATE.Salmonella")

# Rename for labels
p.df$variable <- as.character( p.df$variable )
p.df$SHORTNAME <- p.df$variable
for(i in 1:nrow(p.df)){
  if(p.df$variable[[i]] == "MEANE.scaled"){
    p.df$SHORTNAME[[i]] <- "MEAN"
  }
  if(p.df$variable[[i]] == "MEANS.scaled"){
    p.df$SHORTNAME[[i]] <- "MEAN"
  }
  if(p.df$variable[[i]] == "Tetracycline.scaled"){
    p.df$SHORTNAME[[i]] <- "Tetracyclines"
  }
  if(p.df$variable[[i]] == "tetraE.scaled"){
    p.df$SHORTNAME[[i]] <- "Tetracyclines"
  }
  if(p.df$variable[[i]] == "tetraS.scaled"){
    p.df$SHORTNAME[[i]] <- "Tetracyclines"
  }
  if(p.df$variable[[i]] == "Ceph.scaled"){
    p.df$SHORTNAME[[i]] <- "Cephalosporins"
  } 
  if(p.df$variable[[i]] == "cephE.scaled"){
    p.df$SHORTNAME[[i]] <- "Cephalosporins"
  } 
  if(p.df$variable[[i]] == "ceph\n Salm"){
    p.df$SHORTNAME[[i]] <- "Cephalosporins"
  } 
  if(p.df$variable[[i]] == "cipro_EColi"){
    p.df$SHORTNAME[[i]] <- "Ciprofloxacin"
  } 
  if(p.df$variable[[i]] == "cipro_Salmonella"){
    p.df$SHORTNAME[[i]] <- "Ciprofloxacin"
  } 
  if(p.df$variable[[i]] == "ampiE.scaled"){
    p.df$SHORTNAME[[i]] <- "Ampicillin"
  } 
  if(p.df$variable[[i]] == "ampiS.scaled"){
    p.df$SHORTNAME[[i]] <- "Ampicillin"
  } 
  if(p.df$variable[[i]] == "Fluoroquinolones.scaled"){
    p.df$SHORTNAME[[i]] <- "Fluoroquinolones"
  } 
  if(p.df$variable[[i]] == "Polymyxin.scaled"){
    p.df$SHORTNAME[[i]] <- "Polymyxin"
  } 
  if(p.df$variable[[i]] == "Total.scaled"){
    p.df$SHORTNAME[[i]] <- "Total"
  } 
}



p.df$SHORTNAME <- factor( p.df$SHORTNAME )

p.df$SHORTNAME <- ordered ( p.df$SHORTNAME,
                                levels = c("MEAN", "Production","Workforce", "Infections", "Trade",
                                           "Total", "Tetracyclines", "Cephalosporins",
                                           "Fluoroquinolones", "Polymyxin", "Ciprofloxacin", "Ampicillin") )




### Merge data frames for the barplots
barplot.final$DPSIR <- "DRIVER"
aniSbar.final$DPSIR <- NA
aniSbar.final[ grepl( "EColi", aniSbar.final$variable), "DPSIR"] <- "STATE.EColi"
aniSbar.final[ grepl( "Salmonella", aniSbar.final$variable), "DPSIR"] <- "STATE.Salmonella"
aniSbar.final$alpha <- 0.6
aniSbar.final <- aniSbar.final[ , c("variable", "alpha", "sign", "prop", "N", "pval", "DPSIR")]

aniPbar.final$DPSIR <- "PRESSURE"
aniPbar.final <- aniPbar.final[ , c("variable", "alpha", "sign", "prop", "N", "pval", "DPSIR")]

bar.plot <- merge(barplot.final, aniSbar.final, all = T)
bar.plot <- merge(bar.plot, aniPbar.final, all = T )

for(i in 1:nrow(bar.plot)){
  if(bar.plot$variable[[i]] == "ampi\n Salm"){
    bar.plot$DPSIR[[i]] <- "STATE.Salmonella"
  }
  if(bar.plot$variable[[i]] == "ceph\n Salm"){
    bar.plot$DPSIR[[i]] <- "STATE.Salmonella"
  }
  if(bar.plot$variable[[i]] == "tetra\n Salm"){
    bar.plot$DPSIR[[i]] <- "STATE.Salmonella"
  }
  if(bar.plot$variable[[i]] == "MEAN\n Salm"){
    bar.plot$DPSIR[[i]] <- "STATE.Salmonella"
  }
}

# Add fake rows to plot empty spaces
bar.plot$variable <- as.character( bar.plot$variable )

bar.plot[nrow(bar.plot) + 1,] = c( "cipro_EColi", 0.6, "neg", 0, NA, NA, "STATE.EColi")
bar.plot[nrow(bar.plot) + 1,] = c( "cipro_EColi", 0.6, "pos", 0, NA, NA, "STATE.EColi")
bar.plot[nrow(bar.plot) + 1,] = c( "cipro_Salmonella", 0.6, "neg", 0, NA, NA, "STATE.Salmonella")
bar.plot[nrow(bar.plot) + 1,] = c( "cipro_Salmonella", 0.6, "pos", 0, NA, NA, "STATE.Salmonella")

bar.plot$variable <- factor( bar.plot$variable )
bar.plot$prop <- as.numeric(as.character(bar.plot$prop))


# Rename for labels
bar.plot$variable <- as.character( bar.plot$variable )
bar.plot$SHORTNAME <- bar.plot$variable
for(i in 1:nrow(bar.plot)){
  if(bar.plot$variable[[i]] == "MEAN\n EColi"){
    bar.plot$SHORTNAME[[i]] <- "MEAN"
  }
  if(bar.plot$variable[[i]] == "MEAN\n Salm"){
    bar.plot$SHORTNAME[[i]] <- "MEAN"
  }
  #if(bar.plot$variable[[i]] == "Tetracycline.scaled"){
   # bar.plot$SHORTNAME[[i]] <- "Tetracyclines"
  #}
  if(bar.plot$variable[[i]] == "Tetracycline"){
    bar.plot$SHORTNAME[[i]] <- "Tetracyclines"
  }
  if(bar.plot$variable[[i]] == "tetra\n EColi"){
    bar.plot$SHORTNAME[[i]] <- "Tetracyclines"
  }
  if(bar.plot$variable[[i]] == "tetra\n Salm"){
    bar.plot$SHORTNAME[[i]] <- "Tetracyclines"
  }
  #if(bar.plot$variable[[i]] == "Ceph.scaled"){
   # bar.plot$SHORTNAME[[i]] <- "Cephalosporins"
  #} 
  if(bar.plot$variable[[i]] == "ceph\n EColi"){
    bar.plot$SHORTNAME[[i]] <- "Cephalosporins"
  } 
  if(bar.plot$variable[[i]] == "ceph\n Salm"){
    bar.plot$SHORTNAME[[i]] <- "Cephalosporins"
  } 
  if(bar.plot$variable[[i]] == "cipro_EColi"){
    bar.plot$SHORTNAME[[i]] <- "Ciprofloxacin"
  } 
  if(bar.plot$variable[[i]] == "cipro_Salmonella"){
    bar.plot$SHORTNAME[[i]] <- "Ciprofloxacin"
  } 
  if(bar.plot$variable[[i]] == "ampi\n EColi"){
    bar.plot$SHORTNAME[[i]] <- "Ampicillin"
  } 
  if(bar.plot$variable[[i]] == "ampi\n Salm"){
    bar.plot$SHORTNAME[[i]] <- "Ampicillin"
  } 
  #if(bar.plot$variable[[i]] == "Fluoroquinolones.scaled"){
   # bar.plot$SHORTNAME[[i]] <- "Fluoroquinolones"
  #} 
  #if(bar.plot$variable[[i]] == "Polymyxin.scaled"){
  #  bar.plot$SHORTNAME[[i]] <- "Polymyxin"
  #} 
  if(bar.plot$variable[[i]] == "Polymyxins"){
    bar.plot$SHORTNAME[[i]] <- "Polymyxin"
  } 
  if(bar.plot$variable[[i]] == "Total"){
    bar.plot$SHORTNAME[[i]] <- "Total"
  } 
}


# Order indicators
bar.plot$SHORTNAME <- ordered( bar.plot$SHORTNAME,
                              level = c("MEAN", "Production","Workforce", "Infections", "Trade",
                                        "Total", "Tetracyclines", "Cephalosporins",
                                        "Fluoroquinolones", "Polymyxin", "Ciprofloxacin",
                                        "Ampicillin") )

bar.plot[ which(bar.plot$SHORTNAME == "MEAN" & bar.plot$DPSIR == "STATE.EColi"), "alpha"] <- 1
bar.plot[ which(bar.plot$SHORTNAME == "MEAN" & bar.plot$DPSIR == "STATE.Salmonella"), "alpha"] <- 1




