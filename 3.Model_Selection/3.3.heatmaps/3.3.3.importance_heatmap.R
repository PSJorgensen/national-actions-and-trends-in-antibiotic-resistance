#heatmap for importance scores
library(readr)
library(dplyr)
library(ggplot2)
# GLOBMOD -----------------

##All
#read the results
ALL_globmod_importance_results <- read_csv("3.Model_Selection/3.1.CHANGE/3.1.1.ALL_CHANGE/ALL_globmod_importance.csv",show_col_types = FALSE)
#annotate the results by adding mod_inc (model includes ALL countries)
ALL_globmod_importance_results$mod_inc <- c("ALL")
# annotate the results df by adding model output (mod_type)-- explanatory variable in the model is change
ALL_globmod_importance_results$mod_type <- c("CHANGE")

## HIC
HIC_globmod_importance_results <- read_csv("3.Model_Selection/3.1.CHANGE/3.1.2.HIC_CHANGE/HIC_globmod_importance.csv",show_col_types = FALSE)
HIC_globmod_importance_results$mod_inc <- c("HIC")
HIC_globmod_importance_results$mod_type <- c("CHANGE")

## LMIC
LMIC_globmod_importance_results <- read_csv("3.Model_Selection/3.1.CHANGE/3.1.3.LMIC_CHANGE/LMIC_globmod_importance.csv",show_col_types = FALSE)
LMIC_globmod_importance_results$mod_inc <- c("LMIC")
LMIC_globmod_importance_results$mod_type <- c("CHANGE") 
LMIC_globmod_importance_results <- LMIC_globmod_importance_results %>% filter(!mod_names=="S")

# # # BINOMIAL -----------------
# # no need to back transform the results since we are looking at the importance
# 
# # ##All
ALL_bin_importance_results <- read_csv("3.Model_Selection/3.2.BINOMIAL/3.2.1.ALL_bin/ALLbin_globmod_importance.csv",show_col_types = FALSE)
ALL_bin_importance_results$mod_inc <- c("ALL")
ALL_bin_importance_results$mod_type <- c("BINOMIAL")
ALL_bin_importance_results <- ALL_bin_importance_results %>% filter(!mod_names=="E")

# bind all the data put it together
importance_df <- rbind(ALL_globmod_importance_results, 
                       HIC_globmod_importance_results, 
                       LMIC_globmod_importance_results,
                        ALL_bin_importance_results)

importance_df$mod_inc <- as.factor(importance_df$mod_inc)
importance_df$mod_names <- as.factor(importance_df$mod_names)
importance_df$mod_type <- as.factor(importance_df$mod_type)
importance_df$variable_names <- as.factor(importance_df$variable_names)

########### TIDYING-----

# Named vector with new labels
new_labels_all <- c(
  "income:meantmpAreaPop"="income:Mean Temperature",
  "GDPcap_mean:income"="income:GDP",
"gini:income"="income:Gini",

"income:PopDensity"="income:Population Density", 
"income:prod.per.area"= "income:Animal Production",

"income:vacTotal"="income:Vaccination",
"income:workTotal"="income:Workforce",
"income:infTotal"="income:Infection",
"income:sanTotal"="income:Sanitation",

"AwarenessandEducation:income"="income:Awareness and Education",
"General:income"="income:General",
"income:MonitoringandSurveillance"="income:Monitoring and Surveillance",
"income:RESPONSE"="income:Action",
"income:x0008"="income:Baseline",

"DPSIR:meantmpAreaPop"="DPSE:Mean Temperature",
"DPSIR:GDPcap_mean" ="DPSE:GDP", 
"DPSIR:gini"="DPSE:Gini",
"DPSIR:PopDensity"="DPSE:Population Density",
"DPSIR:prod.per.area"= "DPSE:Animal Production",

"DPSIR:vacTotal"="DPSE:Vaccination",
"DPSIR:workTotal"="DPSE:Workforce",
"DPSIR:infTotal"="DPSE:Infection",
"DPSIR:sanTotal" ="DPSE:Sanitation",


"AwarenessandEducation:DPSIR"="DPSE:Awareness and Education",
"DPSIR:General"="DPSE:General",
"DPSIR:MonitoringandSurveillance"="DPSE:Monitoring and Surveillance",
"DPSIR:RESPONSE"= "DPSE:Action",
"DPSIR:x0008" ="DPSE:Baseline",
"meantmpAreaPop" = "Mean Temperature",
"prod.per.area" = "Animal Production",
"gini" = "Gini",
"PopDensity" ="Population Density",
"GDPcap_mean" = "GDP",
"vacTotal"= "Vaccination", 
"workTotal" ="Workforce", 
"infTotal" ="Infection", 
"sanTotal"= "Sanitation", 
"income" ="income",
"DPSIR"= "DPSE",
"AwarenessandEducation"="Awareness and Education", 
"General"= "General", 
"MonitoringandSurveillance"= "Monitoring and Surveillance", 
"RESPONSE"= "Action",
"x0008"= "Baseline"
)

# Update coefficient names in importance_df
importance_df$variable_names <- factor(importance_df$variable_names,
                                 levels = names(new_labels_all),
                                 labels = new_labels_all)



importance_df$mod_names <-
  factor(importance_df$mod_names, levels = c( "DPSEA",
                                           "DPSEA.noDr",
                                           "aP.noDr",
                                           "aS.noDr",
                                           "aE.noDr",
                                           "aP",
                                           "aS",
                                           "aE",
                                           "Dr",
                                           "P",
                                           "S",
                                           "E",
                                           "DPS",
                                           "PSE",
                                           "DP",
                                           "PS",
                                           "SE"))

quantile(importance_df$`importance score`) # breaks are decided according to quantile
breaks <- c(0, 0.02, 0.2, 0.4, 0.6, 1 )
x_axis_levels <- c("DPSEA", "DPSEA.noDr", "aP.noDr", "aS.noDr", "aE.noDr", 
                   "aP", "aS", "aE", "Dr", "P", "S", "E", "DPS", "PSE", 
                   "DP", "PS", "SE")
y_axis_levels <- c( "Animal Production",
                    "Mean Temperature",
                    "Population Density",
                    "income",
                    "GDP",
                    "Gini",
                    "Vaccination",
                    "Workforce",
                    "Infection",
                    "Sanitation",
                    "DPSE",
                    "Awareness and Education",
                    "General",
                    "Monitoring and Surveillance",
                    "Action",
                    "Baseline")

importance_df <- importance_df %>% select(c("variable_names",
"importance score",
"mod_names",
"mod_inc",
"mod_type"))

# add the excluded vars----
library(readxl)
missing_vars <- read_excel(here("3.Model_Selection/missing variables for each model.xlsx")) %>% select(-c(rank))
missing_vars$"importance score" <- NA
missing_vars <- missing_vars %>% select(colnames(importance_df))
importance_df <- rbind(missing_vars, importance_df) %>% group_by(mod_names, mod_type, mod_inc)
#---
# change into Linear and Categorical Trend
rep_str = c("CHANGE"="Linear Trend",
            "BINOMIAL" = "Categorical Trend")

importance_df$mod_type <- str_replace_all(importance_df$mod_type, rep_str)
mod_type_level2 <- c("Linear Trend", "Categorical Trend")
importance_df$mod_type <-
  factor(importance_df$mod_type, levels = mod_type_level2)

# # Plot the heatmap with the all reordered variables for all change binomial
# #This plot will go to SM
# # save if you want to
pdf("new plots 241210/Fig S5.pdf",width=12,height=8)
ggplot() +
  geom_tile(data=importance_df %>% filter(mod_inc== "HIC" | mod_inc== "LMIC"), aes(x=mod_names, y=variable_names, fill=cut(`importance score`, breaks=breaks))) +
  scale_fill_brewer(palette = "OrRd", na.value = "gray", drop = FALSE)+
  theme_light() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        strip.background = element_rect(fill="darkgray"),
        strip.text = element_text(colour = 'black')) +
       # panel.grid.major = element_blank()) + # Removes major grid lines +
       # panel.grid.minor = element_blank())  + # Removes minor grid lines  +
  facet_grid(mod_type~mod_inc,scales="free_x") +
  geom_hline(yintercept = c(11.5, 15.5, 23.5, 27.5, 39.5, 43.5)) + ylab("Variable Names") + xlab("Model Names") +
  ggtitle("Importance Scores of Variables in Averaged Models HIC, LMIC")+
  labs(fill = "Importance Score Breaks")  +
  scale_y_discrete(limit = y_axis_levels, drop=FALSE)+
  scale_x_discrete(limit = x_axis_levels)
dev.off()


# Create the main plot----
importance_df_main_plot <- importance_df %>% filter(mod_inc=="ALL") %>%
  filter(
        variable_names== "income"|
          variable_names=="DPSE"|
          variable_names== "Mean Temperature"|
          variable_names== "Animal Production"|
          variable_names== "Gini"|
          variable_names== "Population Density"|
          variable_names== "GDP"|
          variable_names== "Vaccination"|
          variable_names==  "Workforce"|
          variable_names==  "Infection"|
          variable_names==  "Sanitation"|
          variable_names== "Awareness and Education"|
          variable_names== "General"|
          variable_names==  "Monitoring and Surveillance"|
          variable_names==  "Action"|
          variable_names==   "Baseline"
  )

#quantile(importance_df_main_plot$`importance score`) # breaks are decided according to quantile
main_plot_breaks <- c(0, 0.05, 0.1, 0.2, 0.4, 1 )

# Plot the heatmap with only ALL and main variables.
#This plot will go to main text ---- NOT ANYMORE
# save if you want to
pdf("new plots 241210/Fig S1.pdf",width=8,height=8)
ggplot() +
  geom_tile(data=importance_df %>% filter(mod_inc== c("ALL")), aes(x=mod_names, y=variable_names, fill=cut(`importance score`, breaks=breaks))) +
  scale_fill_brewer(palette = "OrRd", na.value = "gray", drop = FALSE)+
  theme_light() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        strip.background = element_rect(fill="darkgray"),
        strip.text = element_text(colour = 'black')) +
  # panel.grid.major = element_blank()) + # Removes major grid lines +
  # panel.grid.minor = element_blank())  + # Removes minor grid lines  +
  facet_grid(mod_type~mod_inc,scales="free_x") +
  geom_hline(yintercept = c(11.5, 15.5, 23.5, 27.5, 39.5, 43.5)) + ylab("Variable Names") + xlab("Model Names") +
  ggtitle("Importance Scores of Variables in Averaged Models ALL")+
  labs(fill = "Importance Score Breaks")  +
  scale_y_discrete(limit = y_axis_levels, drop=FALSE)+
  scale_x_discrete(limit = x_axis_levels)
dev.off()
