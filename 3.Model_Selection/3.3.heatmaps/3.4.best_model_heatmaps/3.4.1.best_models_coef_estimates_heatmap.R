library(MuMIn)
library(data.table)
library(tidyverse)
library(lme4)
library(RColorBrewer)

# CHANGE-----
#Import data
load(here("3.Model_Selection/3.1.CHANGE/3.1.1.ALL_CHANGE/ALL_global_workspace_output.RData"))
dredged_list_all <- c(dredged_list5, dredged_list3)

# create a best model list
best_mod <- list()
mod_sum <- list()
df_coeffs <- list()
df_coeffs_dt <- list()
for (i in 1:length(dredged_list_all)) {
  # Apply get.models to each element of dredged_list_all
  best_mod[i] <- get.models(dredged_list_all[[i]], subset = 1)
  # Extract summary and convert coefficients to a data frame
  mod_sum[[i]] <- summary(best_mod[[i]])
  df_coeffs[[i]] <- as.data.frame(mod_sum[[i]]$coefficients)
  # Convert to data.table with coefficient names as a new column
  df_coeffs_dt[[i]] <- setDT(df_coeffs[[i]], keep.rownames = "coefficient")
  df_coeffs_dt[[i]]$coefficient <- factor(df_coeffs_dt[[i]]$coefficient)
  # Exclude the intercept row
  df_coeffs_dt[[i]] <- df_coeffs_dt[[i]] %>% filter(coefficient != "(Intercept)")
  df_coeffs_dt[[i]]$mod_number <- i+1
  df_coeffs_dt[[i]]$mod_type <- c("CHANGE")
  df_coeffs_dt[[i]]$mod_inc <- c("ALL")
}

# Combine the data frames generated in each model into a single data frame
#best_model_coeffs <- do.call(rbind, df_coeffs_dt)
best_model_coeffs <- rbindlist(df_coeffs_dt, fill = TRUE) 
best_model_coeffs$mod_type <- as.factor(best_model_coeffs$mod_type)

# add mod_names according to mod_numbers 
best_model_coeffs <- best_model_coeffs %>%
  mutate(mod_names = case_when(
    mod_number == 2 ~ "DPSEA",
    mod_number == 3 ~ "DPSEA.noDr",
    mod_number == 4 ~ "aP.noDr",
    mod_number == 5 ~ "aS.noDr",
    mod_number == 6 ~ "aE.noDr",
    mod_number == 7 ~ "aP",
    mod_number == 8 ~ "aS",
    mod_number == 9 ~ "aE",
    mod_number == 10 ~ "Dr",
    mod_number == 11 ~ "P",
    mod_number == 12 ~ "S",
    mod_number == 13 ~ "E",
    mod_number == 14 ~ "DPS",
    mod_number == 15 ~ "PSE",
    mod_number == 16 ~ "DP",
    mod_number == 17 ~ "PS",
    mod_number == 18 ~ "SE",
    TRUE ~ NA_character_
  )) %>%
  mutate(mod_names = 
           factor(mod_names, 
                  levels = c("DPSEA", "DPSEA.noDr", "aP.noDr", "aS.noDr", "aE.noDr", 
                                                  "aP", "aS", "aE", "Dr", "P", "S", "E", "DPS", "PSE", 
                                                  "DP", "PS", "SE")))

coeffs_order <- c( "DPSIRDRI", 
                   "DPSIRDRI:workTotal", 
                   "DPSIRRESISTANCE:workTotal",
                   "DPSIRRESISTANCE",
                   "DPSIRUSE:workTotal",
                   "DPSIRUSE",
                   "incomeHIC:sanTotal",
                   "incomeHIC:x0008",    
                   "incomeHIC",
                   "meantmpAreaPop",
                   "prod.per.area",
                   "gini",
                   "PopDensity",
                   "GDPcap_mean",
                   "vacTotal",
                   "workTotal",
                   "infTotal",
                   "sanTotal",
                   "x0008",
                   "AwarenessandEducation",
                   "General",
                   "MonitoringandSurveillance",
                   "RESPONSE")

best_model_coeffs$coefficient <- factor(best_model_coeffs$coefficient, levels=coeffs_order) 
#best_model_coeffs <- best_model_coeffs %>% na.omit()
best_model_coeffs$mod_type <- as.factor(best_model_coeffs$mod_type)
best_model_coeffs$mod_inc <- as.factor(best_model_coeffs$mod_inc)

# save best model coefficients as a file 
#write_csv(best_model_coeffs, file = "change_best_model_coeffs.csv")

# # Update the dataframe to handle both positions of incomeHIC an DPSIR in interactions
 
best_model_coeffs <- best_model_coeffs %>%
  mutate(interaction = str_detect(coefficient, "incomeHIC:|:incomeHIC|DPSIRUSE:|DPSIRRESISTANCE:|DPSIRDRI:")) %>%
  # Extract the variable involved in the interaction with incomeHIC or DPSIR
  mutate(related_var = if_else(interaction,
                               str_replace(coefficient, "incomeHIC:|:incomeHIC|DPSIRUSE:|DPSIRRESISTANCE:|DPSIRDRI:", ""),
                               NA_character_)) %>%
  # Determine interaction type
  mutate(int_style = case_when(
    str_detect(coefficient, "DPSIRUSE:|DPSIRRESISTANCE:|DPSIRDRI:") ~ "DPSE interaction",
    str_detect(coefficient, "incomeHIC:|:incomeHIC") ~ "income interaction",
    TRUE ~ NA_character_
  )) %>%
  group_by(mod_names) %>%
  # Mark rows with the appropriate interaction type
  mutate(crossed = case_when(
    any(interaction & str_detect(coefficient, "DPSIRUSE:|DPSIRRESISTANCE:|DPSIRDRI:")) & coefficient %in% unique(related_var) ~ "DPSE interaction",
    any(interaction & str_detect(coefficient, "incomeHIC:|:incomeHIC")) & coefficient %in% unique(related_var) ~ "income interaction",
    TRUE ~ NA_character_
  )) %>%
  ungroup()
best_model_coeffs$crossed <- as.factor(best_model_coeffs$crossed)

# best_model_coeffs <- best_model_coeffs %>%
#   mutate(Estimate = if_else(crossed, NA_real_, Estimate))

#add new labs
# Named vector with new labels
new_labels <- c("incomeHIC" ="income",
                "meantmpAreaPop" = "Mean Temperature",
                "prod.per.area" = "Animal Production",
                "gini" = "Gini",
                "PopDensity" ="Population Density",
                "GDPcap_mean" = "GDP",
                "vacTotal"= "Vaccination",
                "workTotal" ="Workforce",
                "infTotal" ="Infection",
                "sanTotal"= "Sanitation",
                "AwarenessandEducation"="Awareness and Education",
                "General"= "General",
                "MonitoringandSurveillance"= "Monitoring and Surveillance",
                "RESPONSE"= "Action",
                "x0008" ="Baseline")


# # # Update coefficient names in heatmap_df
best_model_coeffs$coefficient <- factor(best_model_coeffs$coefficient,
                                 levels = names(new_labels),
                                 labels = new_labels)

#exclude income

  best_model_coeffs <- best_model_coeffs %>% filter(!is.na(coefficient))  %>% filter(!coefficient=="income") %>% 
  select(-c(int_style, related_var, interaction)) 

# # Create a RdBu brewer palette
original_palette <- brewer.pal(11, "RdBu")

# Replace the midpoint color
# Assuming the midpoint is the 6th element in an 11-color palette
modified_palette <- original_palette
modified_palette[6] <- "darkred"
modified_palette[5] <- "#d7191c"   # Replace with your chosen midpoint color
modified_palette[4] <- "#fdae61"  # Replace with your chosen midpoint color
modified_palette[3] <-  "beige"# Replace with your chosen midpoint color
modified_palette[2] <- "#abd9e9"  # Replace with your chosen midpoint color
modified_palette[1] <- "#2c7bb6"  # Replace with your chosen midpoint color

quantile(best_model_coeffs$Estimate, na.rm = TRUE)


#lbs <- list(expression(x <= -1),  "(-1, -0.1]", "(-0.1, 0.1]", "(0.1, 1]", ">1", "NA")
best_model_coeffs <- best_model_coeffs %>% select(c("coefficient", 
                                     "Estimate",
                                     "mod_names",
                                     "mod_inc",
                                     "mod_type",
                                     "crossed" ))
# add the excluded vars----
library(readxl)
missing_vars <- read_excel(here("3.Model_Selection/missing variables for each model.xlsx")) %>% select(-c(rank))
missing_vars$"crossed" <- NA
missing_vars$"Estimate" <- NA
missing_vars <- missing_vars %>% rename(coefficient = variable_names)
missing_vars <- missing_vars %>% select(colnames(best_model_coeffs))
missing_vars_only_CHANGE <- missing_vars %>% filter(mod_type=="CHANGE" & mod_inc=="ALL")
best_model_coeffs <- rbind(missing_vars_only_CHANGE, best_model_coeffs) %>% group_by(mod_names, mod_type, mod_inc)
#---
# change into Linear and Categorical Trend
rep_str = c("CHANGE"="Linear Trend",
            "BINOMIAL" = "Categorical Trend")

best_model_coeffs$mod_type <- str_replace_all(best_model_coeffs$mod_type, rep_str)
mod_type_level2 <- c("Linear Trend", "Categorical Trend")
best_model_coeffs$mod_type <-
  factor(best_model_coeffs$mod_type, levels = mod_type_level2)



# Assuming best_model_coeffs is your dataframe and it already has the "crossed" column as a factor
best_model_coeffs_point <-best_model_coeffs %>% filter(!is.na(crossed)) 
best_model_coeffs_tile <- best_model_coeffs %>% filter(is.na(crossed))

best_change_plot <-   ggplot(data = best_model_coeffs) +
  geom_point(data =best_model_coeffs_point, 
             aes(x = mod_names, y = coefficient, shape = factor(crossed)), show.legend = TRUE) +
  guides(shape = guide_legend(override.aes = list(color = "black", fill = "NA",  linetype = 3, size = 5))) +
  geom_tile(data =  best_model_coeffs_tile, 
            aes(x =mod_names, y = coefficient, fill = cut(Estimate, 
                                                          breaks = c(-Inf, -0.2, -0.05, 0.05, 0.2, Inf))), show.legend = TRUE ) +
  scale_fill_manual(values = modified_palette,
                    labels = list(expression(phantom() <= -0.2),  "(-0.2, -0.05]", "(-0.05, 0.05]", "(0.05, 0.2]", "> 0.2", "NA"),
  #                                                          breaks = c(-0.5, -0.2, -0.1, 0, 0.1, 0.2))), show.legend = TRUE) +
  # scale_fill_manual(values = modified_palette,
  #                   labels = list(expression(phantom() <= -0.2),  "(-0.2, -0.1]", "(-0.1, 0]", "(0, 0.1]", "> 0.1", "NA"),
                     na.value = "gray",
  drop=FALSE) + 
  scale_shape_manual(values = c(3, 4)) + # Customize the shapes here. You can add more values if there are more levels.
  theme_light() +
  facet_grid(mod_type ~ mod_inc, scales = "free_x") +
  geom_hline(yintercept = c(9.5, 13.5)) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
        strip.background = element_rect(fill = "darkgray"),
        strip.text = element_text(colour = 'black'),
        panel.grid.major = element_blank(),  # Removes major grid lines
        panel.grid.minor = element_blank()) +  # Removes minor grid lines
  labs(x = "Model Names",
       y = "Variable Names",
       fill = "Estimated Coef.",
       shape = "Interaction Type",
       title = "Coefficient Estimates for Best Models") +
  scale_x_discrete(limit = c("DPSEA", "DPSEA.noDr", "aP.noDr", "aS.noDr", "aE.noDr", 
                                              "aP", "aS", "aE", "Dr", "P", "S", "E", "DPS", "PSE", 
                                              "DP", "PS", "SE")) +
  scale_y_discrete(limit = c(  "Animal Production",
                               "Mean Temperature",
                               "Population Density",
                               # "income",
                               "GDP",
                               "Gini",
                               "Vaccination",
                               "Workforce",
                               "Infection",
                               "Sanitation",
                               # "DPSE",
                               "Awareness and Education",
                               "General",
                               "Monitoring and Surveillance",
                               "Action",
                               "Baseline") , drop=FALSE)
# Display the plot
print(best_change_plot)
# HIC-----
# #For HIC and LMIC there won't be any plot but I'll be saving coeff estimates as csv files. 
# load("C:/Users/egepeh12/Box/AMR_trends/scripts/Statistics/3f.Multivariate_Comparison_Analysis/GLOBAL/HIC_global/HIC_global_workspace_output.RData")
# #use this result_list_all_HIC
# 
# # create a best model list
# best_mod <- list()
# mod_sum <- list()
# df_coeffs <- list()
# df_coeffs_dt <- list()
# for (i in 1:length(result_list_all_HIC)) {
#   # Apply get.models to each element of dredged_list_all
#   best_mod[i] <-  get.models(result_list_all_HIC[[i]]$mod_dredge, subset = 1)
#   # Extract summary and convert coefficients to a data frame
#   mod_sum[[i]] <- summary(best_mod[[i]])
#   df_coeffs[[i]] <- as.data.frame(mod_sum[[i]]$coefficients)
#   # Convert to data.table with coefficient names as a new column
#   df_coeffs_dt[[i]] <- setDT(df_coeffs[[i]], keep.rownames = "coefficient")
#   df_coeffs_dt[[i]]$coefficient <- factor(df_coeffs_dt[[i]]$coefficient)
#   # Exclude the intercept row
#   df_coeffs_dt[[i]] <- df_coeffs_dt[[i]] %>% filter(coefficient != "(Intercept)")
#   df_coeffs_dt[[i]]$mod_number <- i+1
#   df_coeffs_dt[[i]]$mod_type <- c("CHANGE")
#   df_coeffs_dt[[i]]$mod_inc <- c("HIC")
# }
# 
# # Combine the data frames generated in each model into a single data frame
# #best_model_coeffs <- do.call(rbind, df_coeffs_dt)
# best_model_coeffs <- rbindlist(df_coeffs_dt, fill = TRUE) 
# best_model_coeffs$mod_type <- as.factor(best_model_coeffs$mod_type)
# 
# # add mod_names according to mod_numbers 
# best_model_coeffs <- best_model_coeffs %>%
#   mutate(mod_names = case_when(
#     mod_number == 2 ~ "DPSEA",
#     mod_number == 3 ~ "DPSEA.noDr",
#     mod_number == 4 ~ "aP.noDr",
#     mod_number == 5 ~ "aS.noDr",
#     mod_number == 6 ~ "aE.noDr",
#     mod_number == 7 ~ "aP",
#     mod_number == 8 ~ "aS",
#     mod_number == 9 ~ "aE",
#     mod_number == 10 ~ "Dr",
#     mod_number == 11 ~ "P",
#     mod_number == 12 ~ "S",
#     mod_number == 13 ~ "E",
#     mod_number == 14 ~ "DPS",
#     mod_number == 15 ~ "PSE",
#     mod_number == 16 ~ "DP",
#     mod_number == 17 ~ "PS",
#     mod_number == 18 ~ "SE",
#     TRUE ~ NA_character_
#   )) %>%
#   mutate(mod_names = 
#            factor(mod_names, 
#                   levels = c("DPSEA", "DPSEA.noDr", "aP.noDr", "aS.noDr", "aE.noDr", 
#                              "aP", "aS", "aE", "Dr", "P", "S", "E", "DPS", "PSE", 
#                              "DP", "PS", "SE")))
# 
# coeffs_order <- c( "DPSIRDRI", 
#                    "DPSIRDRI:workTotal", 
#                    "DPSIRRESISTANCE:workTotal",
#                    "DPSIRRESISTANCE",
#                    "DPSIRUSE:workTotal",
#                    "DPSIRUSE:MonitoringandSurveillance",
#                    "DPSIRUSE",
#                    "incomeHIC:sanTotal",
#                    "incomeHIC:x0008",    
#                    "incomeHIC",
#                    "meantmpAreaPop",
#                    "prod.per.area",
#                    "gini",
#                    "PopDensity",
#                    "GDPcap_mean",
#                    "vacTotal",
#                    "workTotal",
#                    "infTotal",
#                    "sanTotal",
#                    "x0008",
#                    "AwarenessandEducation",
#                    "General",
#                    "MonitoringandSurveillance",
#                    "RESPONSE")
# 
# best_model_coeffs$coefficient <- factor(best_model_coeffs$coefficient, levels=coeffs_order) 
# #best_model_coeffs <- best_model_coeffs %>% na.omit()
# best_model_coeffs$mod_type <- as.factor(best_model_coeffs$mod_type)
# best_model_coeffs$mod_inc <- as.factor(best_model_coeffs$mod_inc)
# 
# # save best model coefficients as a file
# #write_csv(best_model_coeffs, file = "HIC_change_best_model_coeffs.csv")
# 
# 
# # LMIC-----
# load("C:/Users/egepeh12/Box/AMR_trends/scripts/Statistics/3f.Multivariate_Comparison_Analysis/GLOBAL/LMIC_global/LMIC_global_workspace_output_completed.RData")
# 
# # create a best model list
# best_mod <- list()
# mod_sum <- list()
# df_coeffs <- list()
# df_coeffs_dt <- list()
# for (i in 1:length(result_list_all_LMIC)) {
#   # Apply get.models to each element of dredged_list_all
#   best_mod[i] <-  get.models(result_list_all_LMIC[[i]]$mod_dredge, subset = 1)
#   # Extract summary and convert coefficients to a data frame
#   mod_sum[[i]] <- summary(best_mod[[i]])
#   df_coeffs[[i]] <- as.data.frame(mod_sum[[i]]$coefficients)
#   # Convert to data.table with coefficient names as a new column
#   df_coeffs_dt[[i]] <- setDT(df_coeffs[[i]], keep.rownames = "coefficient")
#   df_coeffs_dt[[i]]$coefficient <- factor(df_coeffs_dt[[i]]$coefficient)
#   # Exclude the intercept row
#   df_coeffs_dt[[i]] <- df_coeffs_dt[[i]] %>% filter(coefficient != "(Intercept)")
#   df_coeffs_dt[[i]]$mod_number <- i+1
#   df_coeffs_dt[[i]]$mod_type <- c("CHANGE")
#   df_coeffs_dt[[i]]$mod_inc <- c("LMIC")
# }
# 
# # Combine the data frames generated in each model into a single data frame
# #best_model_coeffs <- do.call(rbind, df_coeffs_dt)
# best_model_coeffs <- rbindlist(df_coeffs_dt, fill = TRUE) 
# best_model_coeffs$mod_type <- as.factor(best_model_coeffs$mod_type)
# 
# # add mod_names according to mod_numbers 
# best_model_coeffs <- best_model_coeffs %>%
#   mutate(mod_names = case_when(
#     mod_number == 2 ~ "DPSEA",
#     mod_number == 3 ~ "DPSEA.noDr",
#     mod_number == 4 ~ "aP.noDr",
#     mod_number == 5 ~ "aS.noDr",
#     mod_number == 6 ~ "aE.noDr",
#     mod_number == 7 ~ "aP",
#     mod_number == 8 ~ "aS",
#     mod_number == 9 ~ "aE",
#     mod_number == 10 ~ "Dr",
#     mod_number == 11 ~ "P",
#     mod_number == 12 ~ "S",
#     mod_number == 13 ~ "E",
#     mod_number == 14 ~ "DPS",
#     mod_number == 15 ~ "PSE",
#     mod_number == 16 ~ "DP",
#     mod_number == 17 ~ "PS",
#     mod_number == 18 ~ "SE",
#     TRUE ~ NA_character_
#   )) %>%
#   mutate(mod_names = 
#            factor(mod_names, 
#                   levels = c("DPSEA", "DPSEA.noDr", "aP.noDr", "aS.noDr", "aE.noDr", 
#                              "aP", "aS", "aE", "Dr", "P", "S", "E", "DPS", "PSE", 
#                              "DP", "PS", "SE")))
# 
# coeffs_order <- c( "DPSIRDRI:x0008",
#                    "DPSIRDRI", 
#                   # "DPSIRDRI:workTotal", 
#                    #"DPSIRRESISTANCE:workTotal",
#                    "DPSIRRESISTANCE:x0008",
#                    "DPSIRRESISTANCE",
#                    #"DPSIRUSE:workTotal",
#                    #"DPSIRUSE:MonitoringandSurveillance",
#                    "DPSIRUSE:x0008",
#                    "DPSIRUSE",
#                    "x0008",
#                    # "incomeLMIC:sanTotal",
#                    # "incomeLMIC:x0008",    
#                    # "incomeLMIC",
#                    "meantmpAreaPop",
#                    "prod.per.area",
#                    "gini",
#                    "PopDensity",
#                    "GDPcap_mean",
#                    "vacTotal",
#                    "workTotal",
#                    "infTotal",
#                    "sanTotal",
#                    "AwarenessandEducation",
#                    "General",
#                    "MonitoringandSurveillance",
#                    "RESPONSE")
# 
# best_model_coeffs$coefficient <- factor(best_model_coeffs$coefficient, levels=coeffs_order) 
# #best_model_coeffs <- best_model_coeffs %>% na.omit()
# best_model_coeffs$mod_type <- as.factor(best_model_coeffs$mod_type)
# best_model_coeffs$mod_inc <- as.factor(best_model_coeffs$mod_inc)
# 
# # save best model coefficients as a file
# #write_csv(best_model_coeffs, file = "LMIC_change_best_model_coeffs.csv")
# 

# BINOMIAL----------
load(here("3.Model_Selection/3.2.BINOMIAL/3.2.1.ALL_bin/ALL_bin_Models_results.RData"))
# create a best model list
best_mod <- list()
mod_sum <- list()
df_coeffs <- list()
df_coeffs_dt <- list()
for (i in 1:length(dredged_list_all)) {
  # Apply get.models to each element of dredged_list_all
  best_mod[i] <- get.models(dredged_list_all[[i]], subset = 1)
  # Extract summary and convert coefficients to a data frame
  mod_sum[[i]] <- summary(best_mod[[i]])
  df_coeffs[[i]] <- as.data.frame(mod_sum[[i]]$coefficients)
  # Convert to data.table with coefficient names as a new column
  df_coeffs_dt[[i]] <- setDT(df_coeffs[[i]], keep.rownames = "coefficient")
  df_coeffs_dt[[i]]$coefficient <- factor(df_coeffs_dt[[i]]$coefficient)
  # Exclude the intercept row
  df_coeffs_dt[[i]] <- df_coeffs_dt[[i]] %>% filter(coefficient != "(Intercept)")
  df_coeffs_dt[[i]]$mod_number <- i+1
  df_coeffs_dt[[i]]$mod_type <- c("BINOMIAL")
  df_coeffs_dt[[i]]$mod_inc <- c("ALL")
  # BACK TRANSFORM THE RESULTS
  df_coeffs_dt[[i]]$Estimate <- boot::inv.logit(df_coeffs_dt[[i]]$Estimate) 
}

# Combine the data frames generated in each model into a single data frame
best_model_coeffs <- do.call(rbind, df_coeffs_dt)

# add mod_names according to mod_numbers 
best_model_coeffs <- best_model_coeffs %>%
  mutate(mod_names = case_when(
    mod_number == 2 ~ "DPSEA",
    mod_number == 3 ~ "DPSEA.noDr",
    mod_number == 4 ~ "aP.noDr",
    mod_number == 5 ~ "aS.noDr",
    mod_number == 6 ~ "aE.noDr",
    mod_number == 7 ~ "aP",
    mod_number == 8 ~ "aS",
    mod_number == 9 ~ "aE",
    mod_number == 10 ~ "Dr",
    mod_number == 11 ~ "P",
    mod_number == 12 ~ "S",
    mod_number == 13 ~ "E",
    mod_number == 14 ~ "DPS",
    mod_number == 15 ~ "PSE",
    mod_number == 16 ~ "DP",
    mod_number == 17 ~ "PS",
    mod_number == 18 ~ "SE",
    TRUE ~ NA_character_
  )) %>%
  mutate(mod_names = 
           factor(mod_names, 
                  levels = c("DPSEA", "DPSEA.noDr", "aP.noDr", "aS.noDr", "aE.noDr", 
                             "aP", "aS", "aE", "Dr", "P", "S", "E", "DPS", "PSE", 
                             "DP", "PS", "SE")))
#exclude E
best_model_coeffs <- best_model_coeffs %>% filter(!mod_names=="E")
coeffs_order <- c( "DPSIRDRI",  
                   "DPSIRDRI:gini",
                   "DPSIRDRI:meantmpAreaPop",
                   "DPSIRDRI:sanTotal",
                   "DPSIRRESISTANCE", 
                   "DPSIRRESISTANCE:gini",
                   "DPSIRRESISTANCE:meantmpAreaPop",
                   "DPSIRRESISTANCE:sanTotal", 
                   "DPSIRUSE",
                   "DPSIRUSE:gini",
                   "DPSIRUSE:meantmpAreaPop",  
                   "meantmpAreaPop",
                   "prod.per.area",
                   "gini",
                   "PopDensity",
                   "GDPcap_mean",
                   "vacTotal",
                   "workTotal",
                   "infTotal",
                   "sanTotal",
                   "x0008",
                   "AwarenessandEducation",
                   "General",
                   "MonitoringandSurveillance",
                   "RESPONSE")

best_model_coeffs$coefficient <- factor(best_model_coeffs$coefficient, levels=coeffs_order) 
# save binomial best model coefficients as a file 
#write_csv(best_model_coeffs, file = "binomial_best_model_coeffs.csv")


#best_model_coeffs <- best_model_coeffs %>% na.omit()
best_model_coeffs$mod_type <- as.factor(best_model_coeffs$mod_type)
best_model_coeffs$mod_inc <- as.factor(best_model_coeffs$mod_inc)
# Update the dataframe to handle both positions of incomeHIC in interactions
best_model_coeffs <- best_model_coeffs %>%
  mutate(interaction = str_detect(coefficient, "incomeHIC:|:incomeHIC|DPSIRUSE:|DPSIRRESISTANCE:|DPSIRDRI:")) %>%
  # Extract the variable involved in the interaction with incomeHIC or DPSIR
  mutate(related_var = if_else(interaction,
                               str_replace(coefficient, "incomeHIC:|:incomeHIC|DPSIRUSE:|DPSIRRESISTANCE:|DPSIRDRI:", ""),
                               NA_character_)) %>%
  # Determine interaction type
  mutate(int_style = case_when(
    str_detect(coefficient, "DPSIRUSE:|DPSIRRESISTANCE:|DPSIRDRI:") ~ "DPSE interaction",
    str_detect(coefficient, "incomeHIC:|:incomeHIC") ~ "income interaction",
    TRUE ~ NA_character_
  )) %>%
  group_by(mod_names) %>%
  # Mark rows with the appropriate interaction type
  mutate(crossed = case_when(
    any(interaction & str_detect(coefficient, "DPSIRUSE:|DPSIRRESISTANCE:|DPSIRDRI:")) & coefficient %in% unique(related_var) ~ "DPSE interaction",
    any(interaction & str_detect(coefficient, "incomeHIC:|:incomeHIC")) & coefficient %in% unique(related_var) ~ "income interaction",
    TRUE ~ NA_character_
  )) %>%
  ungroup()
best_model_coeffs$crossed <- as.factor(best_model_coeffs$crossed)

#add new labs
# Named vector with new labels
new_labels <- c("incomeHIC" ="income",
                "meantmpAreaPop" = "Mean Temperature",
                "prod.per.area" = "Animal Production",
                "gini" = "Gini",
                "PopDensity" ="Population Density",
                "GDPcap_mean" = "GDP",
                "vacTotal"= "Vaccination",
                "workTotal" ="Workforce",
                "infTotal" ="Infection",
                "sanTotal"= "Sanitation",
                "AwarenessandEducation"="Awareness and Education",
                "General"= "General",
                "MonitoringandSurveillance"= "Monitoring and Surveillance",
                "RESPONSE"= "Action",
                "x0008" ="Baseline")


# # # Update coefficient names in heatmap_df
best_model_coeffs$coefficient <- factor(best_model_coeffs$coefficient,
                                        levels = names(new_labels),
                                        labels = new_labels)
best_model_coeffs <- best_model_coeffs %>% filter(!is.na(coefficient))
#quantile(best_model_coeffs$Estimate)
best_model_coeffs <- best_model_coeffs %>% select(c("coefficient", 
                                                    "Estimate",
                                                    "mod_names",
                                                    "mod_inc",
                                                    "mod_type",
                                                    "crossed" ))
# add the excluded vars----
library(readxl)
missing_vars <- read_excel(here("3.Model_Selection/missing variables for each model.xlsx")) %>% select(-c(rank))
missing_vars$"crossed" <- NA
missing_vars$"Estimate" <- NA
missing_vars <- missing_vars %>% rename(coefficient = variable_names)
missing_vars <- missing_vars %>% select(colnames(best_model_coeffs))
missing_vars_only_bin <- missing_vars %>% filter(mod_type=="BINOMIAL" &mod_inc=="ALL")
best_model_coeffs <- rbind(missing_vars_only_bin, best_model_coeffs) %>% group_by(mod_names, mod_type, mod_inc)

# change into Linear and Categorical Trend
rep_str = c("CHANGE"="Linear Trend",
            "BINOMIAL" = "Categorical Trend")

best_model_coeffs$mod_type <- str_replace_all(best_model_coeffs$mod_type, rep_str)
mod_type_level2 <- c("Linear Trend", "Categorical Trend")
best_model_coeffs$mod_type <-
  factor(best_model_coeffs$mod_type, levels = mod_type_level2)

#---

best_model_coeffs_point <-best_model_coeffs %>% filter(!is.na(crossed))
best_model_coeffs_tile <- best_model_coeffs %>% filter(is.na(crossed))

best_bin_plot <-ggplot() +
  geom_point(data = best_model_coeffs_point, 
             aes(x = mod_names, y = coefficient, shape = factor(crossed)), show.legend = TRUE) +
  guides(shape = guide_legend(override.aes = list(color = "black", fill = "NA",  linetype = 3, size = 5))) +
  geom_tile(data = best_model_coeffs_tile, 
            aes(x = mod_names, y = coefficient, fill = cut(Estimate, breaks =  c(0, 0.2, 0.4,0.6,0.8,1))), show.legend = TRUE) +
  scale_fill_manual(values = modified_palette,
                    labels = function(breaks) {breaks[is.na(breaks)] <- "NA"; breaks}, na.value = "gray") + 
  scale_shape_manual(values = c(3, 4)) + # Customize the shapes here. You can add more values if there are more levels.
  theme_light() +
  facet_grid(mod_type ~ mod_inc, scales = "free_x") +
  geom_hline(yintercept = c(9.5, 13.5)) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
        strip.background = element_rect(fill = "darkgray"),
        strip.text = element_text(colour = 'black'),
        panel.grid.major = element_blank(),  # Removes major grid lines
        panel.grid.minor = element_blank()) +  # Removes minor grid lines
  labs(x = "Model Names",
       y = "Variable Names",
       fill = "Estimated Coef.",
       shape = "Interaction Type") +
  scale_x_discrete(limit = c("DPSEA", "DPSEA.noDr", "aP.noDr", "aS.noDr", "aE.noDr", 
                             "aP", "aS", "aE", "Dr", "P", "S", "E", "DPS", "PSE", 
                             "DP", "PS", "SE")) +
  scale_y_discrete(limit = c(  "Animal Production",
                               "Mean Temperature",
                               "Population Density",
                               # "income",
                               "GDP",
                               "Gini",
                               "Vaccination",
                               "Workforce",
                               "Infection",
                               "Sanitation",
                               # "DPSE",
                               "Awareness and Education",
                               "General",
                               "Monitoring and Surveillance",
                               "Action",
                               "Baseline"), drop=FALSE)
# Display the plot
print(best_bin_plot)


pdf("new plots 241210/Fig S3.pdf",width=8,height=8)
cowplot::plot_grid(best_change_plot, best_bin_plot, ncol = 1, nrow = 2, align = "v", axis = 'l')
dev.off()
