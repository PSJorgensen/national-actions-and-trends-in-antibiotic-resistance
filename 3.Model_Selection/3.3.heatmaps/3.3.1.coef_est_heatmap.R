#heatmap
#read the dataframe for the coefficient estimates of ALL countries, GLOBAL MODELS---
library(readr)
library(dplyr)
library(ggplot2)
library(stringr)
library(RColorBrewer)
if("package:plyr" %in% search()) detach("package:plyr", unload=TRUE) 

# CHANGE -----------------
##All
#read the results

ALL_globmod_coef_est_results <-
  read_csv("3.Model_Selection/3.1.CHANGE/3.1.1.ALL_CHANGE/ALL_globmod_coef_est_results.csv")
#annotate the results by adding mod_inc (model includes ALL countries)
ALL_globmod_coef_est_results$mod_inc <- c("ALL")
# annotate the results df by adding model output (mod_type)-- explanatory variable in the model is change
ALL_globmod_coef_est_results$mod_type <- c("CHANGE")

## HIC
HIC_globmod_coef_est_results <-
  read_csv("3.Model_Selection/3.1.CHANGE/3.1.2.HIC_CHANGE/HIC_globmod_coef_est_results.csv")
HIC_globmod_coef_est_results$mod_inc <- c("HIC")
HIC_globmod_coef_est_results$mod_type <- c("CHANGE")

## LMIC
LMIC_globmod_coef_est_results <-
  read_csv("3.Model_Selection/3.1.CHANGE/3.1.3.LMIC_CHANGE/LMIC_globmod_coef_est_results.csv")
LMIC_globmod_coef_est_results$mod_inc <- c("LMIC")
LMIC_globmod_coef_est_results$mod_type <- c("CHANGE")
LMIC_globmod_coef_est_results <-LMIC_globmod_coef_est_results %>% filter(!mod_names == "S")

# bind all the data put it together
heatmap_df <- rbind(
  ALL_globmod_coef_est_results,
  HIC_globmod_coef_est_results,
  LMIC_globmod_coef_est_results
)
# ALL_bin_coef_est_results2, HIC_bin_coef_est_results2, LMIC_bin_coef_est_results2,
# ALL_mean_coef_est_results, HIC_mean_coef_est_results, LMIC_mean_coef_est_results)
heatmap_df$coefficient <- as.factor(heatmap_df$coefficient)
heatmap_df$mod_names <- as.factor(heatmap_df$mod_names)
heatmap_df$mod_inc <- as.factor(heatmap_df$mod_inc)
heatmap_df$mod_type <- as.factor(heatmap_df$mod_type)


## TIDYING-----
# Convert the coefficient variable to a factor with the desired ordering
heatmap_df$coefficient <- factor(heatmap_df$coefficient)
#exclude the intercept row
heatmap_df <- heatmap_df %>% filter(!coefficient == "(Intercept)")

# Define the desired order of the models
mod_type_level <- c("CHANGE", "BINOMIAL")
heatmap_df$mod_type <-
  factor(heatmap_df$mod_type, levels = mod_type_level)
heatmap_df$mod_names <-
  factor(heatmap_df$mod_names, levels = c( "DPSEA",
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
# Update the dataframe to handle both positions of incomeHIC in interactions
heatmap_df <- heatmap_df %>%
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
  group_by(mod_names, mod_inc) %>%
  # Mark rows with the appropriate interaction type
  mutate(crossed = case_when(
    any(interaction & str_detect(coefficient, "DPSIRUSE:|DPSIRRESISTANCE:|DPSIRDRI:")) & coefficient %in% unique(related_var) ~ "DPSE interaction",
    any(interaction & str_detect(coefficient, "incomeHIC:|:incomeHIC")) & coefficient %in% unique(related_var) ~ "income interaction",
    TRUE ~ NA_character_
  )) %>%
  ungroup()
heatmap_df$crossed <- as.factor(heatmap_df$crossed)

variable_names_order <- c(    "AwarenessandEducation:DPSIRRESISTANCE",
                              "AwarenessandEducation:DPSIRUSE",
                              "DPSIRDRI",
                              "DPSIRDRI:infTotal",
                              "DPSIRDRI:MonitoringandSurveillance",
                              "DPSIRDRI:RESPONSE",
                              "DPSIRDRI:sanTotal",
                              "DPSIRDRI:vacTotal",
                              "DPSIRDRI:workTotal",
                              "DPSIRDRI:x0008",
                              "DPSIRRESISTANCE",
                              "DPSIRRESISTANCE:infTotal",
                              "DPSIRRESISTANCE:MonitoringandSurveillance",
                              "DPSIRRESISTANCE:RESPONSE",
                              "DPSIRRESISTANCE:sanTotal",
                              "DPSIRRESISTANCE:vacTotal",
                              "DPSIRRESISTANCE:workTotal",
                              "DPSIRRESISTANCE:x0008",
                              "DPSIRUSE",
                              "DPSIRUSE:General",
                              "DPSIRUSE:infTotal",
                              "DPSIRUSE:meantmpAreaPop",
                              "DPSIRUSE:MonitoringandSurveillance",
                              "DPSIRUSE:RESPONSE",
                              "DPSIRUSE:sanTotal",
                              "DPSIRUSE:vacTotal",
                              "DPSIRUSE:workTotal",
                              "DPSIRUSE:x0008",
  "income:meantmpAreaPop",
  "gini:incomeHIC",
  "GDPcap_mean:incomeHIC",
  "incomeHIC:vacTotal",
  "incomeHIC:workTotal",
  "incomeHIC:infTotal",
  "incomeHIC:sanTotal",
  "AwarenessandEducation:incomeHIC",
  "GeneralHIC:incomeHIC",
  "incomeHIC:MonitoringandSurveillance",
  "incomeHIC:RESPONSE",
  "incomeHIC:x0008",
  "incomeHIC",
  # "DPSIR:vacTotal",
  # "DPSIR:workTotal",
  # "DPSIR:infTotal",
  # "DPSIR:sanTotal",
  # "DPSIR:meantmpAreaPop",
  # "AwarenessandEducation:DPSIR",
  # "DPSIR:General",
  # "DPSIR:MonitoringandSurveillance",
  # "DPSIR:RESPONSE",
  # "DPSIR:x0008",
  "DPSIR",
  "meantmpAreaPop",
  "prod.per.area",
  "gini",
  "PopDensity",
  "GDPcap_mean",
  "vacTotal",
  "workTotal",
  "infTotal",
  "sanTotal",
  "AwarenessandEducation",
  "General",
  "MonitoringandSurveillance",
  "RESPONSE",
  "x0008")

# Convert the variable_names variable to a factor with the desired ordering
 heatmap_df$coefficient <-
factor(heatmap_df$coefficient, levels = variable_names_order)
# #heatmap_df <- heatmap_df %>% na.omit()

# Plot the heatmap with the reordered coefficients
# save if you want to
# Create a RdBu brewer palette
 library(RColorBrewer)
original_palette <- brewer.pal(11, "RdBu")

# Replace the midpoint color
# Assuming the midpoint is the 6th element in an 11-color palette
modified_palette <- original_palette
modified_palette[5] <-
  "#d7191c"   # Replace with your chosen midpoint color
modified_palette[4] <-
  "#fdae61"  # Replace with your chosen midpoint color
modified_palette[3] <-
  "beige"# Replace with your chosen midpoint color
modified_palette[2] <-
  "#abd9e9"  # Replace with your chosen midpoint color
modified_palette[1] <-
  "#2c7bb6"  # Replace with your chosen midpoint color

#if you run the code below the you will get a whole plot with interaction terms as well
#pdf("coef_est_heatmap_main_vars_income_interaction.pdf", width=12,height=12)
# ggplot() +
#   geom_tile(data=heatmap_df,
#             aes(x=mod_names, y=coefficient, fill=cut(Estimate, breaks=breaks))) +
# #  scale_fill_brewer(palette = "RdBu", na.value = "darkgrey", direction=1)+
#   scale_fill_manual(values = modified_palette) +
# theme_light() +
#   facet_grid(mod_type~mod_inc,scales="free_x") +
#   theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
#         strip.background = element_rect(fill="gray"),
#         strip.text = element_text(colour = 'black'),
#         panel.grid.major = element_blank(),  # Removes major grid lines
#         panel.grid.minor = element_blank())  + # Removes minor grid lines +
#   geom_hline(yintercept = c(9.5,18.5)) +
#   labs(x = "Model Names",
#        y = "Variable Names",
#        fill = "Estimated Coef.",
#        title = "Estimated Coefficents for Comparison" )
# #dev.off()

# check in more detail for each model
# hist(heatmap_df$Estimate)
# heatmap_df %>% filter(mod_names=="DPSIR.noDr") %>% filter(mod_inc=="ALL") %>% filter(mod_type=="CHANGE")

## Supp Plot -------

# Check if 'coefficient' contains an interaction term and update the dataframe
# Update the dataframe to handle both positions of incomeHIC in interactions

#add new labs
# Named vector with new labels
new_labels <- c(
  "incomeHIC" = "income",
  "meantmpAreaPop" = "Mean Temperature",
  "prod.per.area" = "Animal Production",
  "gini" = "Gini",
  "PopDensity" = "Population Density",
  "GDPcap_mean" = "GDP",
  "vacTotal" = "Vaccination",
  "workTotal" = "Workforce",
  "infTotal" = "Infection",
  "sanTotal" = "Sanitation",
  "AwarenessandEducation" = "Awareness and Education",
  "General" = "General",
  "MonitoringandSurveillance" = "Monitoring and Surveillance",
  "RESPONSE" = "Action",
  "x0008" = "Baseline" ,
  "DPSIR"= "DPSE"
)

# # Update coefficient names in heatmap_df
heatmap_df$coefficient <- factor(heatmap_df$coefficient,
                                 levels = names(new_labels),
                                 labels = new_labels)
# Convert the variable_names variable to a factor with the desired ordering
#heatmap_df$coefficient <- factor(heatmap_df$coefficient, levels = variable_names_order_for_plot)

heatmap_df <- heatmap_df %>% filter(!is.na(coefficient)) %>% 
  select(-c(int_style, related_var, interaction)) 

# breaks_quantile <- quantile(heatmap_df$Estimate, na.rm = TRUE)
x_axis_levels <- c("DPSEA", "DPSEA.noDr", "aP.noDr", "aS.noDr", "aE.noDr", 
  "aP", "aS", "aE", "Dr", "P", "S", "E", "DPS", "PSE", 
  "DP", "PS", "SE")
#exclude DPSE and income from the coef est plots because they are factorial
y_axis_levels <- c(  "Animal Production",
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
                     "Baseline")

heatmap_df <- heatmap_df %>% select(c("coefficient", 
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
missing_vars <- missing_vars %>% rename(coefficient = variable_names) ## this is dplyr so make sure to unload the plyr package
missing_vars <- missing_vars %>% select(colnames(heatmap_df))
missing_vars_only_CHANGE <- missing_vars %>% filter(mod_type=="CHANGE")
heatmap_df <- rbind(missing_vars_only_CHANGE, heatmap_df) %>% group_by(mod_names, mod_type, mod_inc)

#---
## exclude income because it is factorial
heatmap_df <- heatmap_df %>% filter(!coefficient=="income")
heatmap_df$mod_inc <- as.factor(heatmap_df$mod_inc)
# change into Linear and Categorical Trend
rep_str = c("CHANGE"="Linear Trend",
            "BINOMIAL" = "Categorical Trend")

heatmap_df$mod_type <- str_replace_all(heatmap_df$mod_type, rep_str)
mod_type_level2 <- c("Linear Trend", "Categorical Trend")
heatmap_df$mod_type <-
  factor(heatmap_df$mod_type, levels = mod_type_level2)

heatmap_df_supp <- heatmap_df %>%  filter(mod_inc == "HIC" | mod_inc=="LMIC")
heatmap_df_point <-heatmap_df_supp %>% filter(!is.na(crossed)) 
heatmap_df_tile <- heatmap_df_supp %>% filter(is.na(crossed))

average_change_plot <-   ggplot(data = heatmap_df_supp) +
  geom_point(data = heatmap_df_point, 
             aes(x = mod_names, y = coefficient, shape = factor(crossed)), show.legend = TRUE) +
  guides(shape = guide_legend(override.aes = list(color = "black", fill = "NA",  linetype = 3, size = 5))) +
  geom_tile(data =  heatmap_df_tile, 
            aes(x =mod_names, y = coefficient, fill = cut(Estimate, 
                                                          breaks = c(-Inf, -0.5, -0.05, 0.05, 1, Inf))), show.legend = TRUE) +
  scale_fill_manual(values = modified_palette,
                    labels = list(expression(phantom() <= -5),  "(-5, -0.05]", "(-0.05, 0.05]", "(0.05, 1]", "> 1", "NA"),
                    na.value = "gray") + 
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
       title = "Coefficient Estimates for Averaged Models") +
  scale_y_discrete(limit = y_axis_levels, drop=FALSE)+
  scale_x_discrete(limit = x_axis_levels) 
# Display the plot
print(average_change_plot)
# Save the plot
pdf("new plots 241210/Fig S6.pdf",
    width = 12,
    height = 6)
average_change_plot
dev.off()

## Main plot----

heatmap_df_main <- heatmap_df %>% filter(mod_inc == c("ALL"))
heatmap_df_main_point <-heatmap_df_main %>% filter(!is.na(crossed)) 
heatmap_df_main_tile <- heatmap_df_main %>% filter(is.na(crossed))
coef.est_change_main <- ggplot(data = heatmap_df_main) +
  geom_point(data =heatmap_df_main_point, 
             aes(x = mod_names, y = coefficient, shape = factor(crossed)), show.legend = TRUE) +
  guides(shape = guide_legend(override.aes = list(color = "black", fill = "NA",  linetype = 3, size = 5))) +
  geom_tile(data =  heatmap_df_main_tile, 
            aes(x =mod_names, y = coefficient, fill = cut(Estimate, 
                                                          breaks = c(-Inf, -0.2, -0.05, 0.05, 0.2, Inf))), show.legend = TRUE) +
  scale_fill_manual(values = modified_palette,
                    labels = list(expression(phantom() <= -0.2),  "(-0.2, -0.05]", "(-0.05, 0.05]", "(0.05, 0.2]", "> 0.2", "NA"),
                    na.value = "gray") + 
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
       title = "Coefficient Estimates for Averaged Models") +
  scale_y_discrete(limit = y_axis_levels, drop=FALSE)+
  scale_x_discrete(limit = x_axis_levels) 


#source-----
source("3.Model_Selection/3.3.heatmaps/3.3.2.bin_coef_est_heatmap.R")

pdf("new plots 241210/Fig S2.pdf",
    width = 8,
    height = 8)
cowplot::plot_grid(
  coef.est_change_main,
  coef.est_bin_main,
  ncol = 1,
  nrow = 2,
  align = "v",
  axis = 'l'
)
dev.off()

# pdf("Coef_est_average_all.pdf",
#     width = 12,
#     height = 8)
# cowplot::plot_grid(
#   average_change_plot,
#   coef.est_bin_main,
#   ncol = 1,
#   nrow = 2,
#   align = "v",
#   axis = 'l'
# )
# dev.off()
