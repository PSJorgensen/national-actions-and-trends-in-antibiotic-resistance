#Import data
# DO NOT LOAD WITHOUT RESAVING
load(here("3.Model_Selection/3.2.BINOMIAL/3.2.1.ALL_bin/ALL_bin_Define_Models_input.RData"))

# First 2 models ------------
# # Original list of models
models_DPSIR <- list(globmod.rDPSIRir2, globmod.rDPSIRir2.noDr)

# New list to contain both original and second versions
models_income <- list(globmod.rDPSIRir2_income, globmod.rDPSIRir2.noDr_income)

all_globmod_list5 <- models_DPSIR  # Start with the first list

for (item in models_income) {
  all_globmod_list5[[length(all_globmod_list5) + 1]] <- item    # add the second list items (models_income)
}


fun_dredge <- function(all_globmod_list5, mod_dredge){
  options(na.action = "na.fail")
  #2. Dredge
#  clust <- try(makeCluster(getOption("cl.cores", 2), type = clusterType)) #set the cluster for faster run, instead of pdredge
  mod_dredge <- dredge(all_globmod_list5, beta="sd", evaluate=T, rank="AICc", m.lim=c(0,5)) #, cluster = clust)
  return(list(mod_dredge = mod_dredge))
}

result_list5 <- lapply(all_globmod_list5, fun_dredge) ### DO NOT RUN IT TAKES TOO LONG INSTEAD RUN THE CODE BELOW
## Difference between deltas are checked and all good!

# Import what was saved from new function working with first 2 models
#load("C:/Users/egepeh12/Box/AMR_trends/scripts/Statistics/3f.Multivariate_Comparison_Analysis/GLOBAL/ALL_global/first_two_models_dredged.RData")

# merge the dredge objects, merge the 1st with the 3rd and the 2nd with 4th. So the same models will be together.
dredged_list5 <- list()
  dredged_list5$globmod.rDPSIRir2_merged_unique <- merge(result_list5[[1]]$mod_dredge, result_list5[[3]]$mod_dredge) %>%
    rapply(as.character, classes="factor", how="replace") %>%
    unique() # take the unique ones to eliminate the same models.

  dredged_list5$globmod.rDPSIRir2.noDr_merged_unique <- merge(result_list5[[2]]$mod_dredge, result_list5[[4]]$mod_dredge) %>%
    rapply(as.character, classes="factor", how="replace") %>%
    unique()
  
#name the models

names(dredged_list5)[1] <- "globmod.rDPSIRir2_merged_unique"
names(dredged_list5)[2] <- "globmod.rDPSIRir2.noDr_merged_unique"

# Rest of the models ---------
# Function is updated with m.lim=0,3
models_DPSIR <- list(
  globmod.dframe.gc.aP.noDr, #1
  globmod.dframe.gc.aS.noDr1, #2 
  globmod.dframe.gc.aS.noDr2, #2 new 3
  globmod.dframe.gc.aS.noDr3, #2 new 4
  globmod.dframe.gc.aI.noDr, #3 new 5
  globmod.dframe.gc.aP, #4 new 6
  globmod.dframe.gc.aS, #5 new 7
  globmod.dframe.gc.aI, #6 new 8
  # globmod.dframe.iD,         # this models are excluded since they can't be running with *DPSIR  
  # globmod.dframe.iP,
  # globmod.dframe.iS,
  # globmod.dframe.gcI,
  globmod.dframe.gcDPS, #7 new 9
  globmod.dframe.gcPSI,#8 new 10
  globmod.dframe.gcDP,#9 new 11
  globmod.dframe.gcPS,#10 new 12
  globmod.dframe.gcSI) #11 new 13


# New list to contain both original and second versions
models_income <- list(
  globmod.dframe.gc.aP.noDr_income, #12 new 14
  globmod.dframe.gc.aS.noDr_income, #13 new 15
  globmod.dframe.gc.aI.noDr_income, #14 new 16
  globmod.dframe.gc.aP_income, #15 new 17
  globmod.dframe.gc.aS_income, #16 new 18
  globmod.dframe.gc.aI_income,#17 new 19
  globmod.dframe.iD_income, #18     new 20
  globmod.dframe.iP_income, #19 new 21 these two will be changed see the code below
  globmod.dframe.iS_income, #20 new 22 these two will be changed see the code below
  globmod.dframe.gcI_income, #21 new 23
  globmod.dframe.gcDPS_income, #22 new 24
  globmod.dframe.gcPSI_income, #23 new 25
  globmod.dframe.gcDP_income, #24 new 26
  globmod.dframe.gcPS_income1, #25 new 27
  globmod.dframe.gcPS_income2, #25 new 28
  globmod.dframe.gcPS_income3, #25 new 29 
  globmod.dframe.gcSI_income) #26 new 30

all_globmod_list3 <- models_DPSIR  # Start with the first list

for (item in models_income) {
  all_globmod_list3[[length(all_globmod_list3) + 1]] <- item
}

fun_dredge3 <- function(all_globmod_list3, mod_dredge){
  options(na.action = "na.fail")
 # clust <- try(makeCluster(getOption("cl.cores", 2), type = clusterType)) #set the cluster for faster run, instead of pdredge
  #2. Dredge
  mod_dredge <- dredge(all_globmod_list3, beta="sd", evaluate=T, rank="AICc", m.lim=c(0,3)) #, cluster = clust)
  return(list(mod_dredge = mod_dredge))
}

result_list3 <- lapply(all_globmod_list3, fun_dredge3) ###  IT TAKES TOO LONG INSTEAD RUN THE CODE BELOW
# load("C:/Users/egepeh12/Box/AMR_trends/scripts/Statistics/3f.Multivariate_Comparison_Analysis/BINOMIAL/ALL_bin/ALL_bin_Models_results.RData")
# fixing iP_income and iS_income
# added the San, Inf, Vac, Work as explanatory variables into these models
iP_iS_income <- list( globmod.dframe.iP_income, #19 new 21
                      globmod.dframe.iS_income) #20 new 22)

fun_dredge3 <- function(iP_iS_income, mod_dredge){
  options(na.action = "na.fail")
  # clust <- try(makeCluster(getOption("cl.cores", 2), type = clusterType)) #set the cluster for faster run, instead of pdredge
  #2. Dredge
  mod_dredge <- dredge(iP_iS_income, beta="sd", evaluate=T, rank="AICc", m.lim=c(0,3)) #, cluster = clust)
  return(list(mod_dredge = mod_dredge))
}
result_list3_iP_iS_income <- lapply(iP_iS_income, fun_dredge3) 

 
dredged_list3 <- list()
  dredged_list3$globmod.dframe.gc.aP.noDr_merged_unique <- merge(result_list3[[1]]$mod_dredge, result_list3[[14]]$mod_dredge) %>%
    rapply(as.character, classes="factor", how="replace") %>%
    unique()

  dredged_list3$globmod.dframe.gc.aS.noDr_merged_unique <- merge(result_list3[[2]]$mod_dredge,
                                                                 result_list3[[3]]$mod_dredge,) %>%    
    rapply(as.character, classes="factor", how="replace") %>%
    unique() %>% 
                                                            merge(result_list3[[4]]$mod_dredge,  
                                                                  result_list3[[15]]$mod_dredge) %>%
    rapply(as.character, classes="factor", how="replace") %>%
    unique()
  

  dredged_list3$globmod.dframe.gc.aI.noDr_merged_unique <- merge(result_list3[[5]]$mod_dredge, result_list3[[16]]$mod_dredge) %>%
    rapply(as.character, classes="factor", how="replace") %>%
    unique()

  dredged_list3$globmod.dframe.gc.aP_merged_unique <- merge(result_list3[[6]]$mod_dredge, result_list3[[17]]$mod_dredge) %>%
    rapply(as.character, classes="factor", how="replace") %>%
    unique()
  dredged_list3$globmod.dframe.gc.aS_merged_unique <- merge(result_list3[[7]]$mod_dredge, result_list3[[18]]$mod_dredge) %>%
    rapply(as.character, classes="factor", how="replace") %>%
    unique()
  dredged_list3$globmod.dframe.gc.aI_merged_unique <- merge(result_list3[[8]]$mod_dredge, result_list3[[19]]$mod_dredge) %>%
    rapply(as.character, classes="factor", how="replace") %>%
    unique()
# only rename the others 
  dredged_list3$globmod.dframe.iD_merged_unique <- result_list3[[20]]$mod_dredge
#  dredged_list3$globmod.dframe.iP_merged_unique <- result_list3[[21]]$mod_dredge
  dredged_list3$globmod.dframe.iP_merged_unique <- result_list3_iP_iS_income[[1]]$mod_dredge
  dredged_list3$globmod.dframe.iS_merged_unique <- result_list3_iP_iS_income[[2]]$mod_dredge
  dredged_list3$globmod.dframe.gcI_merged_unique <- result_list3[[23]]$mod_dredge

  dredged_list3$globmod.dframe.gc.DPS_merged_unique <- merge(result_list3[[9]]$mod_dredge, result_list3[[24]]$mod_dredge) %>%
    rapply(as.character, classes="factor", how="replace") %>%
    unique()
  dredged_list3$globmod.dframe.gc.PSI_merged_unique <- merge(result_list3[[10]]$mod_dredge, result_list3[[25]]$mod_dredge) %>%
    rapply(as.character, classes="factor", how="replace") %>%
    unique()
  dredged_list3$globmod.dframe.gc.DP_merged_unique <- merge(result_list3[[11]]$mod_dredge, result_list3[[26]]$mod_dredge) %>%
    rapply(as.character, classes="factor", how="replace") %>%
    unique()
  dredged_list3$globmod.dframe.gc.PS_merged_unique <- merge(result_list3[[12]]$mod_dredge, 
                                                            result_list3[[27]]$mod_dredge) %>% 
    rapply(as.character, classes="factor", how="replace") %>%
    unique() %>%  
                                                      merge(result_list3[[28]]$mod_dredge, 
                                                            result_list3[[29]]$mod_dredge) %>%
    rapply(as.character, classes="factor", how="replace") %>%
    unique()
  dredged_list3$globmod.dframe.gc.SI_merged_unique <- merge(result_list3[[13]]$mod_dredge, result_list3[[30]]$mod_dredge) %>%
    rapply(as.character, classes="factor", how="replace") %>%
    unique()

# Model Averaging From Dredged Lists----- 
# merge the dredged_lists
dredged_list_all <- c(dredged_list5, dredged_list3)
 
   # Check the delta differences with a function Note that these formulas change the weights
  for (i in 1:length(dredged_list_all)){
print(subset(dredged_list_all[[i]], delta < 2 | df == min(df))) }
  
  # Delta differences are OK. 
  # BUT Weights of the best selected models in 
  #"globmod.dframe.gc.aI_merged_unique" is= 9.911529e-01
 #  and "globmod.dframe.gc.aP_merged_unique" is= 9.979802e-01

  
  # Check the best selected models
  for (i in 1:length(dredged_list_all)){
    print(get.models(dredged_list_all[[i]], subset =1)) }
  #check the best models of iP and iS
  print(get.models(dredged_list_all[["globmod.dframe.iP_merged_unique"]], subset =1))
  print(get.models(dredged_list_all[["globmod.dframe.iS_merged_unique"]], subset =1))
  
        
  fun_mod_avg_sw <- function(input, mod_avg, sum_mod_avg, df_sum_mod_avg, df_sum_mod_avgDT, importance_sw){
 #   options(na.action = "na.fail")
    # # 2. Dredge
  #  mod_dredge <- dredge(globmod_list3, beta="sd",evaluate=T,rank="AICc",m.lim=c(0,3) )
    # # 3. Model averaging
    # HERE THERE IS A WORK AROUND. If a model has only 1 model with cumsum(weight <=0.95) duplicate the model and take the average. 
    #Hence do not trust the View(result_try[[1]][["mod_avg"]][["msTable"]]) but take the model selection tables from the dredge objects!
    tmp <- get.models(input, subset = (weight) >= .95 ) 
    if (length(tmp)==1){
      tmp2 <- c(tmp, tmp) # HERE WEIGHTS CHANGE AS 0.5 FOR BOTH MODELS BUT THIS DOES NOT CHANGE THE SW RESULTS LATER
      mod_avg <- model.avg(tmp2)
    }  else {
      mod_avg <- model.avg(input, subset = cumsum(weight) <= .95) }
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
    return(list(mod_avg = mod_avg, df_sum_mod_avg = df_sum_mod_avg, df_sum_mod_avgDT = df_sum_mod_avgDT, importance_sw = importance_sw))
  }
  
  
results_all <- lapply(dredged_list_all, fun_mod_avg_sw)
# Models singular

# Back Transform the results ##
# this step will be done while plotting the heatmap

# Extract the results ###
# 4. extract the coefficient estimate results
# first start with extracting df_sum_mod_avg to create the heatmaps of the model average doefficient estimates
## this for loop below will create sum_mod_avg_list, summaries of the model averages as a list. 
sum_mod_avg_list <- list()
for (i in 1:length(results_all)) {
  # Apply the function to the corresponding element of result_list and extract df_sum.mod_avg
  sum_mod_avg_list[[i]] <- do.call(rbind, lapply(results_all[i], function(x) {
    # Add a new column called mod_number with value i+1 (according to row number from the excel)
    x$df_sum_mod_avg$mod_number <- i+1
    x$df_sum_mod_avg
  }))
}

# Combine the data frames generated in each model into a single data frame
df_sum_mod_avg <- do.call(rbind, sum_mod_avg_list)

# add mod_names according to mod_numbers 
df_sum_mod_avg <- df_sum_mod_avg %>%
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
  ))

# 5. extract the importance results
importance_sw_list <- list()
for (i in 1:length(results_all)) {
  # Apply the function to the corresponding element of result_list and extract df_sum.mod_avg
  importance_sw_list[[i]] <- do.call(rbind, lapply(results_all[i], function(x) {
    # Add a new column called mod_number with value i+1 (according to row number from the excel)
    x$importance_sw$mod_number <- i+1
    x$importance_sw
  }))
}

# Combine the data frames generated in each model into a single data frame
importance_sw_df <- do.call(rbind, importance_sw_list)

# add mod_names according to mod_numbers 
importance_sw_df <- importance_sw_df %>%
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
  ))
row.names(importance_sw_df)<- NULL

# save the csv files

#save results
write_csv(df_sum_mod_avg, file= here("3.Model_Selection/3.2.BINOMIAL/3.2.1.ALL_bin/ALLbin_globmod_coef_est_results.csv"))
write_csv(importance_sw_df, file= here("3.Model_Selection/3.2.BINOMIAL/3.2.1.ALL_bin/ALLbin_globmod_importance.csv"))


# Extract the all and best model formulas-----
all_mod <- list()
for (i in 1:length(all_globmod_list3)) {
  # Apply get.models to each element of dredged_list_all
 print(all_globmod_list3[[i]]@call)}

best_mod <- list()
for (i in 1:length(dredged_list_all)) {
  # Apply get.models to each element of dredged_list_all
  best_mod[i] <- MuMIn::get.models(dredged_list_all[[i]], subset = 1)
  print(best_mod[[i]]@call)
}



# #save workspace
save.image(file =here("3.Model_Selection/3.2.BINOMIAL/3.2.1.ALL_bin", "ALL_bin_Models_results.RData"))


# END----------------
