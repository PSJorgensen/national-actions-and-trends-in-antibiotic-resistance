#load the saved workspace from computer
load(here("3.Model_Selection/3.1.CHANGE/3.1.3.LMIC_CHANGE/LMIC_Define_Models_input.RData"))
#install.packages("pacman")
library("pacman")
# after installing pacman, basically you don't need to install any other packages
pacman::p_load(
  rio,
  dplyr,
  readr,
  tidyr,
  lme4,
  MuMIn,
  data.table,
  here,
  stringr,
  gtsummary,    # summary statistics and tests
  rstatix,      # summary statistics and statistical tests
  janitor,      # adding totals and percents to tables
  reshape2,
  tidyverse)     
# this function does the following:
# first creates dredge objects of each model.
# second creates a model average object
# third handles model average obj and adds coefficient intervals

###### Functions--------
#for the functions use lapply
### Function for var.max 5-----
##### This function below for the 2 models var.max =5. Same function with var.max=3 will be run and results will be merged later.
## input --> globmod_list5
options(na.action = "na.fail")
globmod_list5 <- LMIC_globmod_list[1:2]
## output --> result_list5

# this function combines the all the steps at once (step 2- dredge, 3- creates the model average, 4- handles model average. 5- calculate importance by sw)
fun_dredge_avg_sw5 <- function(globmod_list5, mod_dredge, mod_avg,sum_mod_avg, df_sum_mod_avg, importance_sw ){
  # # 2. Dredge
  mod_dredge <- dredge(globmod_list5, beta="sd",evaluate=T,rank="AICc",m.lim=c(0,5) )
  # # 3. Model averaging
  mod_avg <- model.avg(mod_dredge, subset=cumsum(weight) <= 0.95)
  # #4. Handling model averaging object
  # # average coefficient estimates and confidence intervals
  sum_mod_avg <-summary(mod_avg) #pulling out model averages
  df_sum_mod_avg <-as.data.frame(sum_mod_avg$coefmat.subset) #selecting full model coefficient averages
  df_sum_mod_avgDT <- setDT(df_sum_mod_avg, keep.rownames = "coefficient") #put rownames into column
  names(df_sum_mod_avg) <- gsub(" ", "", names(df_sum_mod_avg)) # remove spaces from column headers
  # #5. Importance (sw)
  importance_sw <-  as.data.frame(sw(mod_avg))
  colnames(importance_sw) <- c("importance score")
  importance_sw$variable_names <- rownames(importance_sw)
  importance_sw$rank <- 1:nrow(importance_sw)
  importance_sw <- importance_sw %>% select(c("variable_names", "importance score", "rank"))
  return(list(mod_dredge = mod_dredge, mod_avg = mod_avg, df_sum_mod_avg = df_sum_mod_avg, df_sum_mod_avgDT = df_sum_mod_avgDT, importance_sw = importance_sw))
}
# apply this function into each element of the list (element=model) -- this is a faster method than for loop
result_list5 <- lapply(globmod_list5, fun_dredge_avg_sw5)
#BEST MODEL
get.models(result_list5[[1]]$mod_dredge, subset = 1)

# 4. extract the coefficient estimate results
# first start with extracting df_sum_mod_avg to create the heatmaps of the model average doefficient estimates
## this for loop below will create sum_mod_avg_list5, summaries of the model averages as a list. 
sum_mod_avg_list5 <- list()
for (i in 1:length(result_list5)) {
  # Apply the function to the corresponding element of result_list and extract df_sum.mod_avg
  sum_mod_avg_list5[[i]] <- do.call(rbind, lapply(result_list5[i], function(x) {
    # Add a new column called mod_number with value i+1 (according to row number from the excel)
    x$df_sum_mod_avg$mod_number <- i+1
    x$df_sum_mod_avg
  }))
}

# Combine the data frames generated in each model into a single data frame
df_sum_mod_avg_5 <- do.call(rbind, sum_mod_avg_list5)

# add mod_names according to mod_numbers 
df_sum_mod_avg_5 <- df_sum_mod_avg_5 %>%
  mutate(mod_names = case_when(
    mod_number == 2 ~ "DPSEA",
    mod_number == 3 ~ "DPSEA.noDr",
    TRUE ~ NA_character_
  ))

# 5. extract the importance results

importance_sw_list5 <- list()
for (i in 1:length(result_list5)) {
  # Apply the function to the corresponding element of result_list and extract df_sum.mod_avg
  importance_sw_list5[[i]] <- do.call(rbind, lapply(result_list5[i], function(x) {
    # Add a new column called mod_number with value i+1 (according to row number from the excel)
    x$importance_sw$mod_number <- i+1
    x$importance_sw
  }))
}

# Combine the data frames generated in each model into a single data frame
importance_sw_df5 <- do.call(rbind, importance_sw_list5)

# add mod_names according to mod_numbers 
importance_sw_df5 <- importance_sw_df5 %>%
  mutate(mod_names = case_when(
    mod_number == 2 ~ "DPSEA",
    mod_number == 3 ~ "DPSEA.noDr",
    TRUE ~ NA_character_
  ))
row.names(importance_sw_df5)<- NULL

### Function for var-max=3-----

##### This function below for the models var.max =3. Results will be merged later.

## input --> globmod_list_....
globmod_list3 <- LMIC_globmod_list[3:length(LMIC_globmod_list)]
## output --> result_list3

fun_dredge_avg_sw3 <- function(globmod_list3){
  options(na.action = "na.fail")
  # # 2. Dredge
  mod_dredge <- dredge(globmod_list3, beta="sd",evaluate=T,rank="AICc",m.lim=c(0,3) )
  # # 3. Model averaging
  # HERE THERE IS A WORK AROUND. If a model has only 1 model with cumsum(weight <=0.95) duplicate the model and take the average. 
  #Hence do not trust the View(result_try[[1]][["mod_avg"]][["msTable"]]) but take the model selection tables from the dredge objects!
  tmp <- get.models(mod_dredge, subset = cumsum(weight) <= .95)
  if (length(tmp)==1){
    tmp2 <- c(tmp, tmp)
    mod_avg <- model.avg(tmp2)
  }  else {
    mod_avg <- model.avg(mod_dredge, subset = cumsum(weight) <= .95)}
  #4. Handling model averaging object
  # average coefficient estimates and confidence intervals
  sum_mod_avg <-summary(mod_avg) #pulling out model averages
  df_sum_mod_avg <-as.data.frame(sum_mod_avg$coefmat.subset) #selecting full model coefficient averages
  df_sum_mod_avgDT <- setDT(df_sum_mod_avg, keep.rownames = "coefficient") #put rownames into column
  names(df_sum_mod_avg) <- gsub(" ", "", names(df_sum_mod_avg)) # remove spaces from column headers
  # #5. Importance (sw)
  importance_sw <-  as.data.frame(sw(mod_avg))
  colnames(importance_sw) <- c("importance score")
  importance_sw$variable_names <- rownames(importance_sw)
  importance_sw$rank <- 1:nrow(importance_sw)
  importance_sw <- importance_sw %>% select(c("variable_names", "importance score", "rank"))
  return(list(mod_dredge = mod_dredge, mod_avg = mod_avg, df_sum_mod_avg = df_sum_mod_avg, df_sum_mod_avgDT = df_sum_mod_avgDT, importance_sw = importance_sw))
}

result_list3 <- lapply(globmod_list3, fun_dredge_avg_sw3)
# NOTE!!! check the models that had been excluded
#get.models(result_list3[[14]]$mod_dredge, subset = 1)


# 4. extract the coefficient estimate results
# first start with extracting df_sum_mod_avg to create the heatmaps of the model average doefficient estimates
## this for loop below will create sum_mod_avg_list5, summaries of the model averages as a list. 
sum_mod_avg_list3 <- list()
for (i in 1:length(result_list3)) {
  # Apply the function to the corresponding element of result_list and extract df_sum.mod_avg
  sum_mod_avg_list3[[i]] <- do.call(rbind, lapply(result_list3[i], function(x) {
    # Add a new column called mod_number with value i+1 (according to row number from the excel)
    x$df_sum_mod_avg$mod_number <- i+3
    x$df_sum_mod_avg
  }))
}
options(na.action = "na.omit")

# Combine the data frames generated in each model into a single data frame
df_sum_mod_avg_3 <- do.call(plyr::rbind.fill, sum_mod_avg_list3)

# add mod_names according to mod_numbers 
df_sum_mod_avg_3 <- df_sum_mod_avg_3 %>%
  mutate(mod_names = case_when(
    mod_number == 4 ~ "aP.noDr",
    mod_number == 5 ~ "aS.noDr",
    mod_number == 6 ~ "aE.noDr",
    mod_number == 7 ~ "aP",
    mod_number == 8 ~ "aS",
    mod_number == 9 ~ "aE",
    mod_number == 10 ~ "Dr",
    mod_number == 11 ~ "P",
    mod_number == 12 ~ "S",
#    mod_number == 13 ~ "E",
    mod_number == 13 ~ "DPS",
    mod_number == 14 ~ "PSE",
    mod_number == 15 ~ "DP",
    mod_number == 16 ~ "PS",
 #   mod_number == 18 ~ "SE",
    TRUE ~ NA_character_
  ))
# 5. extract the importance results

importance_sw_list3 <- list()
for (i in 1:length(result_list3)) {
  # Apply the function to the corresponding element of result_list and extract df_sum.mod_avg
  importance_sw_list3[[i]] <- do.call(rbind, lapply(result_list3[i], function(x) {
    # Add a new column called mod_number with value i+1 (according to row number from the excel)
    x$importance_sw$mod_number <- i+3
    x$importance_sw
  }))
}

# Combine the data frames generated in each model into a single data frame
importance_sw_df3 <- do.call(rbind, importance_sw_list3)

# add mod_names according to mod_numbers 
importance_sw_df3 <- importance_sw_df3 %>%
  mutate(mod_names = case_when(
    mod_number == 4 ~ "aP.noDr",
    mod_number == 5 ~ "aS.noDr",
    mod_number == 6 ~ "aE.noDr",
    mod_number == 7 ~ "aP",
    mod_number == 8 ~ "aS",
    mod_number == 9 ~ "aE",
    mod_number == 10 ~ "Dr",
    mod_number == 11 ~ "P",
    mod_number == 12 ~ "S",
#    mod_number == 13 ~ "E",
    mod_number == 13 ~ "DPS",
    mod_number == 14 ~ "PSE",
    mod_number == 15 ~ "DP",
    mod_number == 16 ~ "PS",
#    mod_number == 18 ~ "SE",
    TRUE ~ NA_character_
  ))
row.names(importance_sw_df3)<- NULL

### Merge the results-----

# Merge the result lists 
result_list_all_LMIC <- c(result_list5, result_list3)
# best models
best_mod <- list()
for (i in 1:length(result_list_all_LMIC)) {
  # Apply get.models to each element of dredged_list_all
  best_mod[i] <- MuMIn::get.models(result_list_all_LMIC[[i]]$mod_dredge, subset = 1)
 # print(best_mod[[i]]@call)
}
#for coef estimates
LMIC_globmod_coef_est_results <-rbind(df_sum_mod_avg_5, df_sum_mod_avg_3)
#for importance
LMIC_globmod_importance_results <-rbind(importance_sw_df5, importance_sw_df3)


#save results
write_csv(LMIC_globmod_coef_est_results, file= here("3.Model_Selection/3.1.CHANGE/3.1.3.LMIC_CHANGE/LMIC_globmod_coef_est_results.csv"))
write_csv(LMIC_globmod_importance_results, file= here("3.Model_Selection/3.1.CHANGE/3.1.3.LMIC_CHANGE/LMIC_globmod_importance.csv"))
# 
# #save workspace
save.image(file =here("3.Model_Selection/3.1.CHANGE/3.1.3.LMIC_CHANGE/", "LMIC_global_workspace_output_completed.RData"))

