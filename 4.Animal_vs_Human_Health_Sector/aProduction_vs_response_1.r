

# Add response data from raw.df (mean response across all indicators - except NAP)
aniR.df <- raw.df[ which( raw.df$DPSIR == "RESPONSE" & raw.df$SHORTNAME != "NAP published"),]

aniR.df$Value <- as.numeric( as.character( aniR.df$Value ) )
aniR.mean <- aggregate( data = aniR.df, Value~ISO3+Country, function(x) mean(x) )

aniR.Amean <- aggregate( data = aniR.df[which(aniR.df$SHORTNAME%in%unique(aniR.df$SHORTNAME)[c(1,3,5,7,12)]),], 
  Value~ISO3+Country, function(x) mean(x) )
aniR.Hmean <- aggregate( data = aniR.df[which(aniR.df$SHORTNAME%in%unique(aniR.df$SHORTNAME)[c(2,4,6,8,13)]),], 
                         Value~ISO3+Country, function(x) mean(x) )
names(aniR.mean)[3]<-"Gmean"
names(aniR.Amean)[3]<-"Amean"
names(aniR.Hmean)[3]<-"Hmean"

#unique(aniR.df$SHORTNAME)[c(1,3,5,7,12)] animal variables
#unique(aniR.df$SHORTNAME)[c(2,4,6,8,13)] human variables


aPROD.df[,!names(aPROD.df)%in%c("ISO3","Area")]<-
  apply(aPROD.df[,!names(aPROD.df)%in%c("ISO3","Area")],2,as.numeric)


aPROD.df[,!names(aPROD.df)%in%c("ISO3","Area","Year")][is.na(aPROD.df[,!names(aPROD.df)%in%c("ISO3","Area","Year")])==TRUE]<-0


aPROD.0509 <- aggregate( data = aPROD.df[ which(aPROD.df$Year %in% c(2005:2010)), 
                                          !names(aPROD.df)%in%c("Year")], 
                             .~ISO3+Area, function(x) mean(x,na.rm = FALSE) )


aPRODmg.0509<-aPROD.0509[,c("ISO3","Area","Beef and Buffalo Meat","Meat; pig","Meat; Poultry")]
aPRODmg.0509[,"Beef and Buffalo Meat"]<-aPRODmg.0509[,"Beef and Buffalo Meat"]*0.1636464
aPRODmg.0509[,"Meat; pig"]<-aPRODmg.0509[,"Meat; pig"]*1.298113
aPRODmg.0509[,"Meat; Poultry"]<-aPRODmg.0509[,"Meat; Poultry"]*148
# amu in grams
aPRODmg.0509[,"AMUsum"]<-apply(aPRODmg.0509[,c("Beef and Buffalo Meat","Meat; pig","Meat; Poultry")],1,sum)
# amu in tonnes
aPRODmg.0509[,"AMUsum"]<-aPRODmg.0509[,"AMUsum"]/1000000

#aPROD.0509[,c("ISO3","Area","AMUsum")]
# cattle 45 mg/PCU 0.275 T = 0.1636364 g/T
# chicken 148 mg/PCU 0.001 T = 148.000 g / T
# pig 172 mg/PCU 0.1325 T = 1.298113 g / T

# CATTLE 140-425; mid-point = 275 kg
# PIGS 25-240; mid-point = 132.5 kg
# CHICKEN 1; mid-point = 1 kg
# TURKEY

# mid-point estimates from https://assets.publishing.service.gov.uk/government/uploads/system/uploads/attachment_data/file/580710/1101060-v1-Understanding_the_PCU_-_gov_uk_guidance.pdf 

############# PREPARING DATA FRAMES FOR PLOTTING

aniRm.df<-merge(aPROD.0509,aniR.mean,by.x=c("ISO3","Area"),by.y=c("ISO3","Country"))

#

aniRmm.df<-reshape2::melt(aniRm.df,id.vars = c("ISO3","Area","Gmean"))
names(aniRmm.df)[names(aniRmm.df)=="variable"]<-"prodType"
names(aniRmm.df)[names(aniRmm.df)=="value"]<-"prodValue"

aniRmm.df<-merge(aniRmm.df,aniR.Amean,by.x = c("ISO3","Area"), by.y = c("ISO3","Country"))
aniRmm.df<-merge(aniRmm.df,aniR.Hmean,by.x = c("ISO3","Area"), by.y = c("ISO3","Country"))

aniRmm.df[,"AHrdiff"]<-aniRmm.df[,"Amean"]-aniRmm.df[,"Hmean"]

#

aniRmmm.df<-reshape2::melt(aniRmm.df,id.vars = c("ISO3","Area","prodType","prodValue","AHrdiff"))
names(aniRmmm.df)[names(aniRmmm.df)=="variable"]<-"responseType"
names(aniRmmm.df)[names(aniRmmm.df)=="value"]<-"responseValue"

aniRmmmALL.df<-aniRmmm.df[which(aniRmmm.df$prodType%in%c("ALL")),]
aniRmmmALL.df[,"prodGroup"]<-cut_interval(log10(aniRmmmALL.df$prodValue),5)

# add per capita production

aniRmmmALL.df<-merge(aniRmmmALL.df,aPROD.Mean0509,by="ISO3")
names(aniRmmmALL.df)[which(names(aniRmmmALL.df)=="0509")]<-"perCapAll"
aniRmmmALL.df[,"capprodGroup"]<-cut_interval(log10(aniRmmmALL.df$perCapAll),5)

aniRmmmALL.df[,"capprodGroup"]<-as.character(aniRmmmALL.df[,"capprodGroup"])
aniRmmmALL.df[,"prodGroup"]<-as.character(aniRmmmALL.df[,"prodGroup"])

aniRmmmALL.df<-merge(aniRmmmALL.df,aPRODmg.0509[,c("ISO3","Area","AMUsum")],by=c("ISO3","Area"))

aniRmmmALL.df[,"AMUGroup"]<-cut_interval(log10(aniRmmmALL.df$AMUsum),5)
aniRmmmALL.df[,"AMUGroup"]<-as.character(aniRmmmALL.df[,"AMUGroup"])


aniRmmmmALL.df<-reshape2::melt(aniRmmmALL.df[,!names(aniRmmmALL.df)%in%c("prodType","Area","prodGroup","capprodGroup","AMUGroup")],
     id.vars = c("ISO3","AHrdiff","responseType","responseValue"),
     measure.vars = c("perCapAll","prodValue","AMUsum"))

aniRmmmgALL.df<-reshape2::melt(aniRmmmALL.df[,!names(aniRmmmALL.df)%in%c("prodType","Area","perCapAll","prodValue","AMUsum")],
                     id.vars = c("ISO3","AHrdiff","responseType","responseValue"),
                     measure.vars = c("prodGroup","capprodGroup","AMUGroup"))

aniRmmmgALL.df[,"value"]<-factor(aniRmmmgALL.df[,"value"],levels=c(
  "[-3.16,-1.9]","[-2.34,-1.89]","(-1.9,-0.635]",
    "(-1.89,-1.44]","(-1.44,-0.988]","(-0.988,-0.537]", "(-0.537,-0.0851]",
    "(-0.635,0.63]","(0.63,1.89]","(1.89,3.16]","[2.26,3.38]",
  "(3.38,4.5]","(4.5,5.62]","(5.62,6.73]", "(6.73,7.85]"))


##### ADDING COUNTRY INCOME GROUP TO CONTINUOUS DATA FRAME
names(aniRmmmmALL.df)[names(aniRmmmmALL.df)%in%c("variable","value")]<-
  c("cvar","cval")
names(aniRmmmgALL.df)[names(aniRmmmgALL.df)%in%c("variable","value")]<-
  c("gvar","gval")

aniRmmmmALLig.df<-merge(aniRmmmmALL.df,aniRmmmgALL.df,by=c("ISO3","AHrdiff",
                  "responseType","responseValue"))

aniRmmmmALLig.df<-merge(aniRmmmmALLig.df,
                region.df[which(region.df$region_type == "GDP Regions"),
                  c("a3","region_name")],by.x = "ISO3", by.y = "a3")

#""      aniRmmmALL.df
      
########################### PLOT
aniRmmmmALLig.df[,"region_name"]<-factor(aniRmmmmALLig.df[,"region_name"],
                                  levels=c("Low Income","Lower middle income",
                                           "Upper middle income","High Income"))#,
 newlevels.vec<-c("LMIC","LMIC","LMIC","HIC")
 aniRmmmmALLig.df[,"region2"]<-factor(newlevels.vec[ aniRmmmmALLig.df[,"region_name"]]) 


########
### continuous loess - current figure

 gg.prod_anihumact <- ggplot(aniRmmmmALLig.df[which( !aniRmmmmALLig.df$responseType%in%c("Gmean") &
                                                       aniRmmmmALLig.df$cvar == c("prodValue")),],
                             aes(y = responseValue, x = log10(cval), colour = responseType)) +
   geom_point(alpha = 0.3) + 
   theme_bw() +
   geom_smooth(data = aniRmmmmALLig.df[which( !aniRmmmmALLig.df$responseType%in%c("Gmean") &
                                                aniRmmmmALLig.df$cvar == c("prodValue") & 
                                                log10(aniRmmmmALLig.df$cval) < 7 & 
                                                log10(aniRmmmmALLig.df$cval) > 3),],
               size = 1, level = 0.5, alpha = 0.3, method = "lm") +
   ylab("Action") + 
   xlab("Animal Production (log10[tonnes])") +
   theme(legend.position = c(0.10, 0.8)) +
   scale_colour_manual(name = "Sector", labels = c("Animal", "Human"), values = c("#66c2a5", "#fc8d62")) +
   facet_grid(cols = vars(region2)) + 
   theme(legend.background = element_blank()) +
   # Adding "A" and "B" to each facet
   geom_text(data = data.frame(region2 = c("HIC", "LMIC"), label = c("A", "B")),
             aes(x = -Inf, y = Inf, label = label),
             hjust = -0.1, vjust = 1.5, size = 5, fontface = "bold", inherit.aes = FALSE)
 
 # Save the plot

pdf("plots/Fig5.pdf", width = 8, height = 4)
print(gg.prod_anihumact)  
dev.off()

 


### STATISTICAL MODELLING
library(StepReg)


## columns production values ## animal human health response values
anicRALLig.df<-
  dcast(aniRmmmmALLig.df,region2+ISO3+responseType+responseValue~cvar,fun.aggregate = mean,value.var = "cval")


### ANIMAL RESPONSE
### NO INCOME REGION MODELS
model0<-glm(responseValue~region2*log10(prodValue),data=anicRALLig.df[which(
  anicRALLig.df$responseType=="Amean"),])
model1<-glm(responseValue~log10(prodValue)+region2,data=anicRALLig.df[which(
  anicRALLig.df$responseType=="Amean"),])
model2<-glm(responseValue~log10(prodValue),data=anicRALLig.df[which(
  anicRALLig.df$responseType=="Amean"),])

anova(model0,model1,test="F")
summary(model2) ##

### ANIMAL VS HUMAN RESPONSE
### NO INCOME REGION MODELS
model0<-glm(responseValue~responseType*log10(prodValue),data=anicRALLig.df[which(
  anicRALLig.df$responseType!="Gmean"),])
model1<-glm(responseValue~responseType+log10(prodValue),data=anicRALLig.df[which(
  anicRALLig.df$responseType!="Gmean"),])
model2<-glm(responseValue~log10(prodValue),data=anicRALLig.df[which(
  anicRALLig.df$responseType!="Gmean"),])
model3<-glm(responseValue~1,data=anicRALLig.df[which(
  anicRALLig.df$responseType!="Gmean"),])

anova(model0,model1,test="F")
anova(model1,model2,test="F")
anova(model2,model3,test="F")

# stepwise anova - prodValue - model0


### ANIMAL VS HUMAN RESPONSE
### HIC REGION MODELS
model0<-glm(responseValue~responseType*log10(prodValue),data=anicRALLig.df[which(
  anicRALLig.df$responseType!="Gmean"& anicRALLig.df$region2=="HIC"),])
model1<-glm(responseValue~responseType+log10(prodValue),data=anicRALLig.df[which(
  anicRALLig.df$responseType!="Gmean"& anicRALLig.df$region2=="HIC"),])
model2<-glm(responseValue~log10(prodValue),data=anicRALLig.df[which(
  anicRALLig.df$responseType!="Gmean"& anicRALLig.df$region2=="HIC"),])
model3<-glm(responseValue~1,data=anicRALLig.df[which(
  anicRALLig.df$responseType!="Gmean"& anicRALLig.df$region2=="HIC"),])

anova(model0,model1,test="F")
anova(model1,model2,test="F")
anova(model2,model3,test="F")

# stepwise anova - prodValue - model0


### LMIC REGION MODELS
model0<-glm(responseValue~responseType*log10(prodValue),data=anicRALLig.df[which(
  anicRALLig.df$responseType!="Gmean"& anicRALLig.df$region2=="LMIC"),])
model1<-glm(responseValue~responseType+log10(prodValue),data=anicRALLig.df[which(
  anicRALLig.df$responseType!="Gmean"& anicRALLig.df$region2=="LMIC"),])
model2<-glm(responseValue~log10(prodValue),data=anicRALLig.df[which(
  anicRALLig.df$responseType!="Gmean"& anicRALLig.df$region2=="LMIC"),])
model3<-glm(responseValue~1,data=anicRALLig.df[which(
  anicRALLig.df$responseType!="Gmean"& anicRALLig.df$region2=="LMIC"),])

anova(model0,model1,test="F")
anova(model1,model2,test="F")
anova(model2,model3,test="F")

# stepwise anova - prodValue - model0


### INCOME REGION INTERACTION MODELS
model0<-glm(responseValue~responseType*log10(perCapAll)*region2,data=anicRALLig.df[which(
                anicRALLig.df$responseType!="Gmean"),])

  model1<-glm(responseValue~responseType*log10(perCapAll)+
                log10(perCapAll)*region2,data=anicRALLig.df[which(
  anicRALLig.df$responseType!="Gmean"),])
  model2a<-glm(responseValue~responseType+
                log10(perCapAll)*region2,data=anicRALLig.df[which(
                  anicRALLig.df$responseType!="Gmean"),])
  model2b<-glm(responseValue~responseType*log10(perCapAll)+region2,data=anicRALLig.df[which(
                   anicRALLig.df$responseType!="Gmean"),])
  model3<-glm(responseValue~
                 log10(perCapAll)*region2,data=anicRALLig.df[which(
                   anicRALLig.df$responseType!="Gmean"),])
  model4<-glm(responseValue~
                log10(perCapAll)+region2,data=anicRALLig.df[which(
                  anicRALLig.df$responseType!="Gmean"),])
  model5a<-glm(responseValue~
                log10(perCapAll),data=anicRALLig.df[which(
                  anicRALLig.df$responseType!="Gmean"),])
  model5b<-glm(responseValue~
                 region2,data=anicRALLig.df[which(
                   anicRALLig.df$responseType!="Gmean"),])
  
  anova(model0,model1,test="F")
  anova(model1,model2a,test="F")
  anova(model1,model2b,test="F")
  anova(model2a,model3,test="F")
  anova(model3,model4,test="F")
  anova(model4,model5a,test="F")
  anova(model4,model5b,test="F")
  
summary(model1)
step(model1)

### model 4 - perCapAll; model1 - prodValue; model3 - AMUsum;
