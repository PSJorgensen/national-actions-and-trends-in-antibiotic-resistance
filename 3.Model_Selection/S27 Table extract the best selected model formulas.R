# # Kamil's method to extract the model selection tables
# #https://stackoverflow.com/questions/28154029/how-to-create-aic-model-selection-table-in-r-in-latex-format
# Load necessary packages
library(tidyverse)
library(broom) 
library(MuMIn) # Assuming simplify.formula and reformulate are loaded from here
library(data.table)

#Function--- 
export_change_formulas <- function(component, i = 1:(ncol(ms_table)-5), response = "CHANGE") {
  # Column numbers are different if the model has Driver components or not hence i = 1:(ncol(ms_table)-6) refers to 6 
  # columns before the last column  
  # Convert the component to a data frame
  ms_table <- component %>% as.data.frame()
  
  # Ensure the specified columns exist
  if (all(i %in% seq_along(names(ms_table)))) {
    # Extract the top 5 models and the null model
    best_table <- ms_table %>% head(5)
    null_table <- ms_table %>% filter(df == min(df)) 
    res <- rbind(best_table, null_table)
    
    # Get the names of the model terms
    v <- names(ms_table)[i]
    v[v == "(Intercept)"] <- 1
    
    # Create formula-like model names
    mnames <- apply(res[, i], 1, function(x) 
      deparse(simplify.formula(reformulate(v[!is.na(x)], response = response))))
    
    mnames_bound <- mnames %>% cbind.data.frame()
    res <- cbind(model = mnames_bound, res %>% select(c(df, logLik, AICc, delta, weight)))
    
    return(res)
  } else {
    # Return NA or an empty data frame if columns do not exist
    return(NA)
  }
}

# CHANGE------
########## Define the function to process each component

## ALL-----
load(here("3.Model_Selection/3.1.CHANGE/3.1.1.ALL_CHANGE/ALL_global_workspace_output.RData"))
# Apply the function to all components of the list
change_all_formulas <- lapply(dredged_list_all, export_change_formulas)

# Optionally, name the results list for easier reference
names(change_all_formulas) <- names(dredged_list_all)

# Display the result for the first component as an example
print(change_all_formulas[[1]])
change_all_formulas
export_change_all_table <- rbindlist(change_all_formulas, idcol = T)
colnames(export_change_all_table) <- c("model name", "variables", "df", "logLik", "AICc", "delta", "weight") 
export_change_all_table$income <- c("ALL")

# Define the mapping
mapping <- c(
  "globmod.rDPSIRir2_merged_unique" = "DPSEA",
  "globmod.rDPSIRir2.noDr_merged_unique" = "DPSEA.noDr",
  "globmod.dframe.gc.aP.noDr_merged_unique" = "aP.noDr",
  "globmod.dframe.gc.aS.noDr_merged_unique" = "aS.noDr",
  "globmod.dframe.gc.aI.noDr_merged_unique" = "aE.noDr",
  "globmod.dframe.gc.aP_merged_unique" = "aP",
  "globmod.dframe.gc.aS_merged_unique" = "aS",
  "globmod.dframe.gc.aI_merged_unique" = "aE",
  "globmod.dframe.iD_merged_unique" = "Dr",
  "globmod.dframe.iP_merged_unique" = "P",
  "globmod.dframe.iS_merged_unique" = "S",
  "globmod.dframe.gcI_merged_unique" = "E",
  "globmod.dframe.gc.DPS_merged_unique" = "DPS",
  "globmod.dframe.gc.PSI_merged_unique" = "PSE",
  "globmod.dframe.gc.DP_merged_unique" = "DP",
  "globmod.dframe.gc.PS_merged_unique" = "PS",
  "globmod.dframe.gc.SI_merged_unique" = "SE"
)

# Modify the column based on the mapping
export_change_all_table <- export_change_all_table %>%
  mutate(`model name` = recode(`model name`, !!!mapping))

#remove all but keep the table and function
rm(list=ls()[! ls() %in% c("export_change_all_table", "export_change_formulas")])

## HIC------
load(here("3.Model_Selection/3.1.CHANGE/3.1.2.HIC_CHANGE/HIC_global_workspace_output.RData"))
# export the dredge list as they are not saved before as a list but embedded into result_list_all_HIC
dredge_list_allHIC <- list()
for (i in 1:length(result_list_all_HIC)){
  dredge_list_allHIC[[i]] <-(result_list_all_HIC[[i]]$mod_dredge)
}

# Apply the function to all components of the list
change_HIC_formulas <- lapply(dredge_list_allHIC, export_change_formulas)
# Optionally, name the results list for easier reference
names(change_HIC_formulas) <- names(dredge_list_allHIC)

# Display the result for the first component as an example
print(change_HIC_formulas[[1]])
change_HIC_formulas
export_change_HIC_table <- rbindlist(change_HIC_formulas, idcol = T)
colnames(export_change_HIC_table) <- c("model name", "variables", "df", "logLik", "AICc", "delta", "weight") 
export_change_HIC_table$income <- c("HIC")
mapping_HIC <- c(
  "1" = "DPSEA",
  "2" = "DPSEA.noDr",
  "3" = "aP.noDr",
  "4" = "aS.noDr",
  "5" = "aE.noDr",
  "6" = "aP",
  "7" = "aS",
  "8" = "aE",
  "9" = "Dr",
  "10" = "P",
  "11" = "S",
  "12" = "E",
  "13" = "DPS",
  "14" = "PSE",
  "15" = "DP",
  "16" = "PS",
  "17" = "SE"
)

# Modify the column based on the mapping
export_change_HIC_table <- export_change_HIC_table %>%
  mutate(`model name` = recode(`model name`, !!!mapping_HIC))

#remove all but keep the tables and function
rm(list=ls()[! ls() %in% c("export_change_all_table", "export_change_HIC_table", "export_change_formulas")])

## LMIC----------
#Function--- 

load(here("3.Model_Selection/3.1.CHANGE/3.1.3.LMIC_CHANGE/LMIC_global_workspace_output_completed.RData"))
# export the dredge list as they are not saved before as a list but embedded into result_list_all_LMIC
dredge_list_allLMIC <- list()
for (i in 1:length(result_list_all_LMIC)){
  dredge_list_allLMIC[[i]] <-(result_list_all_LMIC[[i]]$mod_dredge)
}

# Apply the function to all components of the list
change_LMIC_formulas <- lapply(dredge_list_allLMIC, export_change_formulas)
# Optionally, name the results list for easier reference
names(change_LMIC_formulas) <- names(dredge_list_allLMIC)

# Display the result for the first component as an example
print(change_LMIC_formulas[[1]])
change_LMIC_formulas
export_change_LMIC_table <- rbindlist(change_LMIC_formulas, idcol = T)
colnames(export_change_LMIC_table) <- c("model name", "variables", "df", "logLik", "AICc", "delta", "weight") 
export_change_LMIC_table$income <- c("LMIC")

mapping_LMIC <- c(
  "1" = "DPSEA",
  "2" = "DPSEA.noDr",
  "3" = "aP.noDr",
  "4" = "aS.noDr",
  "5" = "aE.noDr",
  "6" = "aP",
  "7" = "aS",
  "8" = "aE",
  "9" = "Dr",
  "10" = "P",
  "11" = "S",
#  "12" = "E",
  "12" = "DPS",
  "13" = "PSE",
  "14" = "DP",
  "15" = "PS")
#  "17" = "SE")

# Modify the column based on the mapping
export_change_LMIC_table <- export_change_LMIC_table %>%
  mutate(`model name` = recode(`model name`, !!!mapping_LMIC))

#remove all but keep the tables 
rm(list=ls()[! ls() %in% c("export_change_all_table", "export_change_HIC_table", "export_change_LMIC_table")])

# BINOMIAL------
load(here("3.Model_Selection/3.2.BINOMIAL/3.2.1.ALL_bin/ALL_bin_Models_results.RData"))
## ALL ---

export_binomial_formulas <- function(component, i = 1:(ncol(ms_table)-5), response = "BINOMIAL") {
  # Column numbers are different if the model has Driver components or not hence i = 1:(ncol(ms_table)-6) refers to 6 
  # columns before the last column  
  # Convert the component to a data frame
  ms_table <- component %>% as.data.frame()
  
  # Ensure the specified columns exist
  if (all(i %in% seq_along(names(ms_table)))) {
    # Extract the top 5 models and the null model
    best_table <- ms_table %>% head(5)
    null_table <- ms_table %>% filter(df == min(df)) 
    res <- rbind(best_table, null_table)
    
    # Get the names of the model terms
    v <- names(ms_table)[i]
    v[v == "(Intercept)"] <- 1
    
    # Create formula-like model names
    mnames <- apply(res[, i], 1, function(x) 
      deparse(simplify.formula(reformulate(v[!is.na(x)], response = response))))
    
    mnames_bound <- mnames %>% cbind.data.frame()
    res <- cbind(model = mnames_bound, res %>% select(c(df, logLik, AICc, delta, weight)))
    
    return(res)
  } else {
    # Return NA or an empty data frame if columns do not exist
    return(NA)
  }
}

# Apply the function to all components of the list
binomial_all_formulas <- lapply(dredged_list_all, export_binomial_formulas)

# Optionally, name the results list for easier reference
names(binomial_all_formulas) <- names(dredged_list_all)

# Display the result for the first component as an example
print(binomial_all_formulas[[1]])
binomial_all_formulas
export_binomial_all_table <- rbindlist(binomial_all_formulas, idcol = T)
colnames(export_binomial_all_table) <- c("model name", "variables", "df", "logLik", "AICc", "delta", "weight") 
export_binomial_all_table$income <- c("ALL")
mapping <- c(
  "globmod.rDPSIRir2_merged_unique" = "DPSEA",
  "globmod.rDPSIRir2.noDr_merged_unique" = "DPSEA.noDr",
  "globmod.dframe.gc.aP.noDr_merged_unique" = "aP.noDr",
  "globmod.dframe.gc.aS.noDr_merged_unique" = "aS.noDr",
  "globmod.dframe.gc.aI.noDr_merged_unique" = "aE.noDr",
  "globmod.dframe.gc.aP_merged_unique" = "aP",
  "globmod.dframe.gc.aS_merged_unique" = "aS",
  "globmod.dframe.gc.aI_merged_unique" = "aE",
  "globmod.dframe.iD_merged_unique" = "Dr",
  "globmod.dframe.iP_merged_unique" = "P",
  "globmod.dframe.iS_merged_unique" = "S",
  "globmod.dframe.gcI_merged_unique" = "E",
  "globmod.dframe.gc.DPS_merged_unique" = "DPS",
  "globmod.dframe.gc.PSI_merged_unique" = "PSE",
  "globmod.dframe.gc.DP_merged_unique" = "DP",
  "globmod.dframe.gc.PS_merged_unique" = "PS",
  "globmod.dframe.gc.SI_merged_unique" = "SE"
)

# Modify the column based on the mapping
export_binomial_all_table <- export_binomial_all_table %>%
  mutate(`model name` = recode(`model name`, !!!mapping))

#remove all but keep the tables 
rm(list=ls()[! ls() %in% c("export_change_all_table", "export_change_HIC_table", "export_change_LMIC_table","export_binomial_all_table")])

model_selection_tables <- rbind.data.frame(export_change_all_table, export_change_HIC_table, export_change_LMIC_table, export_binomial_all_table)

model_selection_tables<- model_selection_tables %>% mutate_if(is.numeric, round, digits=3) 

#change words
rep_str = c("CHANGE"="Linear Trend",
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
            "DPSIR"= "DPSE",
            "BINOMIAL" = "Categorical Trend")
    
model_selection_tables$variables <- str_replace_all(model_selection_tables$variables, rep_str)

# Convert dataframe to flextable
ft <- flextable::flextable(model_selection_tables)

# Create a Word document
doc <- officer::read_docx()

# Add flextable to the Word document
doc <- flextable::body_add_flextable(doc, value = ft)

# Save the Word document
print(doc, target = "model selection summary table241210.docx")
# Don't forget to manually change the action categories into italic

#! For LMIC iS iE not existed, delete iS manually from the table. 
#! For Binomial iE excluded, delete E models manually from the table.