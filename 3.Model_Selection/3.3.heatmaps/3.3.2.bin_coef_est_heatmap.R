## BINOMIAL MODELS heatmap

#heatmap
#read the dataframe for the coefficient estimates of ALL countries, GLOBAL MODELS---
library(readr)
library(dplyr)
library(ggplot2)
library(stringr)
if("package:plyr" %in% search()) detach("package:plyr", unload=TRUE) 

# # BINOMIAL -----------------
# ##All
ALL_bin_coef_est_results <- read_csv("3.Model_Selection/3.2.BINOMIAL/3.2.1.ALL_bin/ALLbin_globmod_coef_est_results.csv")
ALL_bin_coef_est_results$mod_inc <- c("ALL")
ALL_bin_coef_est_results$mod_type <- c("BINOMIAL")
# back transform the results
ALL_bin_coef_est_results$Estimate <- boot::inv.logit(ALL_bin_coef_est_results$Estimate)
# save this 
#write_csv(ALL_bin_coef_est_results, file= "BINOMIAL/ALL_bin/ALL_bin_globmod_coef_est_results_back_transformed.csv")

## exclude E model
ALL_bin_coef_est_results <- ALL_bin_coef_est_results %>% filter(!mod_names== "E")


# # bind all the data put it together
heatmap_df_bin <- rbind(ALL_bin_coef_est_results)
# ALL_bin_coef_est_results2, HIC_bin_coef_est_results2, LMIC_bin_coef_est_results2,
# ALL_mean_coef_est_results, HIC_mean_coef_est_results, LMIC_mean_coef_est_results)
heatmap_df_bin$coefficient <- as.factor(heatmap_df_bin$coefficient)
heatmap_df_bin$mod_names <- as.factor(heatmap_df_bin$mod_names)
heatmap_df_bin$mod_inc <- as.factor(heatmap_df_bin$mod_inc)
heatmap_df_bin$mod_type <- as.factor(heatmap_df_bin$mod_type)


########### TIDYING-----

# Convert the coefficient variable to a factor with the desired ordering
heatmap_df_bin$coefficient <- factor(heatmap_df_bin$coefficient)
#exclude the intercept row
heatmap_df_bin <- heatmap_df_bin %>% filter(!coefficient =="(Intercept)")

# Define the desired order of the models
mod_type_level <- c("CHANGE", "BINOMIAL")
heatmap_df_bin$mod_type <- factor(heatmap_df_bin$mod_type, levels = mod_type_level)
# # update Impact to Exposure

heatmap_df_bin$mod_names <-
  factor(heatmap_df_bin$mod_names, levels = c( "DPSEA",
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
heatmap_df_bin <- heatmap_df_bin %>%
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
heatmap_df_bin$crossed <- as.factor(heatmap_df_bin$crossed)

#heatmap_df_bin <- heatmap_df_bin %>%  na.omit()
heatmap_df_bin$coefficient <- as.factor(heatmap_df_bin$coefficient)


# Plot the heatmap with the reordered coefficients
# save if you want to
library(RColorBrewer)

# Create a RdBu brewer palette
original_palette <- brewer.pal(11, "RdBu")

# Replace the midpoint color
# Assuming the midpoint is the 6th element in an 11-color palette
modified_palette <- original_palette
modified_palette[6] <- "black"
modified_palette[5] <- "#d7191c"   # Replace with your chosen midpoint color
modified_palette[4] <- "#fdae61"  # Replace with your chosen midpoint color
modified_palette[3] <-  "beige"# Replace with your chosen midpoint color
modified_palette[2] <- "#abd9e9"  # Replace with your chosen midpoint color
modified_palette[1] <- "#2c7bb6"  # Replace with your chosen midpoint color


### PLOT -------

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
                "x0008" ="Baseline",
                "DPSIR" ="DPSE")

# # Update coefficient names in heatmap_df_bin
heatmap_df_bin$coefficient <- factor(heatmap_df_bin$coefficient,
                                 levels = names(new_labels),
                                 labels = new_labels)
# Convert the variable_names variable to a factor with the desired ordering
heatmap_df_bin <- heatmap_df_bin %>% filter(!is.na(coefficient)) %>% 
  select(-c(int_style, related_var, interaction)) 

breaks_quantile <- quantile(heatmap_df_bin$Estimate, na.rm = TRUE)

breaks <- c(0, 0.2, 0.4 , 0.6, 0.8 , 1) #breaks are set according to quantile
# # Round to three decimal places                  
# heatmap_df_bin$Estimate<- round(heatmap_df_bin$Estimate, digits = 1)  

x_axis_levels <- c("DPSEA", "DPSEA.noDr", "aP.noDr", "aS.noDr", "aE.noDr", 
                   "aP", "aS", "aE", "Dr", "P", "S", "E", "DPS", "PSE", 
                   "DP", "PS", "SE")
y_axis_levels <- c(  "Animal Production",
                     "Mean Temperature",
                     "Population Density",
                     #"income",
                     "GDP",
                     "Gini",
                     "Vaccination",
                     "Workforce",
                     "Infection",
                     "Sanitation",
                     #"DPSE",
                     "Awareness and Education",
                     "General",
                     "Monitoring and Surveillance",
                     "Action",
                     "Baseline")
heatmap_df_bin <- heatmap_df_bin %>% select(c("coefficient", 
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
missing_vars <- missing_vars %>% select(colnames(heatmap_df_bin))
missing_vars_only_BIN <- missing_vars %>% filter(mod_type=="BINOMIAL")
heatmap_df_bin <- rbind(missing_vars_only_BIN, heatmap_df_bin) %>% group_by(mod_names, mod_type, mod_inc)
#---
rep_str = c("CHANGE"="Linear Trend",
            "BINOMIAL" = "Categorical Trend")

heatmap_df_bin$mod_type <- str_replace_all(heatmap_df_bin$mod_type, rep_str)
mod_type_level2 <- c("Linear Trend", "Categorical Trend")
heatmap_df_bin$mod_type <-
  factor(heatmap_df_bin$mod_type, levels = mod_type_level2)
#-----
heatmap_df_bin_point <-heatmap_df_bin %>% filter(!is.na(crossed)) 
heatmap_df_bin_tile <- heatmap_df_bin %>% filter(is.na(crossed))


coef.est_bin_main <-   ggplot(data = heatmap_df_bin) +
  geom_point(data =heatmap_df_bin_point, 
             aes(x = mod_names, y = coefficient, shape = factor(crossed)), show.legend = TRUE) +
  guides(shape = guide_legend(override.aes = list(color = "black", fill = "NA",  linetype = 3, size = 5))) +
  geom_tile(data =  heatmap_df_bin_tile, 
            aes(x =mod_names, y = coefficient, fill = cut(Estimate, 
                                                          breaks = c(0.0, 0.2, 0.4 , 0.6, 0.8 , 1.0))), show.legend = TRUE) +
  scale_fill_manual(values = modified_palette, na.value = "gray") +
                    #labels = lbs) + 
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
       #title = "Coefficient Estimates for Averaged Models") +
  scale_y_discrete(limit = y_axis_levels, drop=FALSE)+
  scale_x_discrete(limit = x_axis_levels) 
# Display the plot
print(coef.est_bin_main)
