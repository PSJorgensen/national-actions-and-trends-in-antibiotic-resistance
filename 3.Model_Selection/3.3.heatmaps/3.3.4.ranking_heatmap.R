## ranking ----
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
rank_df <- rbind(ALL_globmod_importance_results, HIC_globmod_importance_results, LMIC_globmod_importance_results,
                   ALL_bin_importance_results) #, HIC_bin_importance_results, LMIC_bin_importance_results)
                    # ALL_mean_importance_results, HIC_mean_importance_results, LMIC_mean_importance_results)

rank_df$mod_inc <- as.factor(rank_df$mod_inc)
rank_df$mod_names <- as.factor(rank_df$mod_names)
rank_df$mod_type <- as.factor(rank_df$mod_type)
rank_df$variable_names <- as.factor(rank_df$variable_names)
rank_df <- rank_df %>% select(-c("importance score", "mod_number"))

########### TIDYING-----
# Define the desired order of the coefficients
# Named vector with new labels
variable_names_order <- c(
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

# Update coefficient names in heatmap_df
rank_df$variable_names <- factor(rank_df$variable_names,
                                       levels = names(variable_names_order),
                                       labels = variable_names_order)

# # Define the desired order of the models
mod_type_level <- c("CHANGE", "BINOMIAL")
rank_df$mod_type <- factor(rank_df$mod_type, levels = mod_type_level)
# exclude all the interaction terms
rank_df <- rank_df %>% filter(!grepl(":", variable_names))
rank_df <-rank_df %>% group_by(mod_names, mod_inc, mod_type)
# data ranks isn't sequential so rerank them

rank_df <- rank_df %>%
  group_by(mod_names, mod_inc, mod_type) %>%
  mutate(new_rank = row_number()) %>%
  ungroup()  %>%
  select(-c(rank)) %>% 
  rename(rank = new_rank)

rank_df_top5 <- rank_df %>% filter(rank < 6) #filter the df for top 5 ranked variables
rank_df_top5$rank <- as.factor(rank_df_top5$rank)

rank_df$mod_names <-
  factor(rank_df$mod_names, levels = c( "DPSEA",
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

library(readxl)
missing_vars <- read_excel(here("3.Model_Selection/missing variables for each model.xlsx")) %>% select(colnames(rank_df_top5))

rank_df_top5 <- rbind(missing_vars, rank_df_top5) %>% group_by(mod_names, mod_type, mod_inc)

rank_df_top5$variable_names <- as.factor(rank_df_top5$variable_names)
variable_nms_ord<- c( "income",
                      "DPSE",
                      "Mean Temperature",
                      "Animal Production",
                      "Gini",
                      "Population Density",
                      "GDP",
                      "Vaccination",
                      "Workforce",
                      "Infection",
                      "Sanitation",
                      "Awareness and Education",
                      "General",
                      "Monitoring and Surveillance",
                      "Action",
                      "Baseline")
# Update coefficient names in heatmap_df
rank_df_top5$variable_names <- factor(rank_df_top5$variable_names,
                                      levels = variable_nms_ord)
# change into Linear and Categorical Trend
rep_str = c("CHANGE"="Linear Trend",
            "BINOMIAL" = "Categorical Trend")
rank_df_top5$mod_type <- str_replace_all(rank_df_top5$mod_type, rep_str)
mod_type_level2 <- c("Linear Trend", "Categorical Trend")
rank_df_top5$mod_type <-
  factor(rank_df_top5$mod_type, levels = mod_type_level2)

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
rank_df_top5_supp <- rank_df_top5 %>%  filter(mod_inc=="HIC"|
                                                mod_inc=="LMIC")
# Plot the heatmap with the reordered variables 
# save if you want to
pdf("new plots 241210/Fig S4.pdf",width=12,height=8)
ggplot() +
  geom_tile(data=rank_df_top5_supp, aes(x=mod_names, y=variable_names,  fill=rank)) +
  scale_fill_brewer(palette = "OrRd" , na.value="gray", drop = FALSE, direction = -1)+
  scale_y_discrete(limit = y_axis_levels, drop=FALSE)+
  scale_x_discrete(limit = x_axis_levels) +
  theme_light() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        strip.background = element_rect(fill="darkgray"),
        strip.text = element_text(colour = 'black'),
      #  panel.grid.major = element_blank(),  # Removes major grid lines
        panel.grid.minor = element_blank())  + # Removes minor grid lines  +
  facet_grid(mod_type~mod_inc,scales="free_x") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  geom_hline(yintercept = c(11.5, 15.5))  + 
  ylab("Variable Names") + xlab("Model Names") + ggtitle("Variable Ranks in Each Model")+
  labs(fill = "Rank")  
dev.off()

  
#filter the df for top 5 ranked variables 
 
rank_df_main<- rank_df_top5 %>%  filter(mod_inc=="ALL") %>% 
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
      variable_names==   "Baseline" )

rank_df_main$mod_inc <- as.character(rank_df_main$mod_inc)
rank_df_main <- rank_df_main %>% mutate(mod_inc=factor(mod_inc,
                                                levels = "ALL"))

pdf("new plots 241210/Fig 4.pdf",width=6,height=6)
ggplot() +
  geom_tile(data=rank_df_main, aes(x=mod_names, y=variable_names,  fill=rank)) +
  scale_fill_brewer(palette = "OrRd", na.value="gray", drop = FALSE, direction = -1)+
  theme_light() +
  scale_y_discrete(limit = y_axis_levels, drop=FALSE)+
  scale_x_discrete(limit = x_axis_levels) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        strip.background = element_rect(fill="darkgray"),
        strip.text = element_text(colour = 'black'),
        #  panel.grid.major = element_blank(),  # Removes major grid lines
        panel.grid.minor = element_blank())  + # Removes minor grid lines  +
  facet_grid(mod_type~mod_inc,scales="free_x", drop = FALSE) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  geom_hline(yintercept = c(11.5, 15.5)) +
  ylab("Variable Names") + xlab("Model Names") + ggtitle("Variable Ranks in Each Model")+
  labs(fill = "Rank")  
dev.off()
