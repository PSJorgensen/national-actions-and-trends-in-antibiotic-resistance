#trying to figure out what is going on iS model, why the dredge view isnt the same as the model.sel table

get.models(globmod.dframe.iS_dredge, subset = c(1:5))
# fixed-effect model matrix is rank deficient so dropping 1 column / coefficient
# fixed-effect model matrix is rank deficient so dropping 1 column / coefficient
# fixed-effect model matrix is rank deficient so dropping 2 columns / coefficients
# Warning messages:
#   1: In checkConv(attr(opt, "derivs"), opt$par, ctrl = control$checkConv,  :
#                     unable to evaluate scaled gradient
#                   2: In checkConv(attr(opt, "derivs"), opt$par, ctrl = control$checkConv,  :
#                                     Model failed to converge: degenerate  Hessian with 1 negative eigenvalues
#                                   > a
#                                   $`1`
#                                   Linear mixed model fit by REML ['lmerModLmerTest']
#                                   Formula: change ~ (1 | ISO3) + (1 | SHORTNAME)
#                                   Data: dframe.iS
#                                   REML criterion at convergence: -2.4298
#                                   Random effects:
#                                     Groups    Name        Std.Dev.
#                                   ISO3      (Intercept) 0.273579
#                                   SHORTNAME (Intercept) 0.809387
#                                   Residual              0.004746
#                                   Number of obs: 4, groups:  ISO3, 2; SHORTNAME, 2
#                                   Fixed Effects:
#                                     (Intercept)  
#                                   0.3536  
#                                   optimizer (nloptwrap) convergence code: 0 (OK) ; 0 optimizer warnings; 2 lme4 warnings 
#                                   
#                                   $`1025`
#                                   Linear mixed model fit by REML ['lmerModLmerTest']
#                                   Formula: change ~ sanTotal + (1 | ISO3) + (1 | SHORTNAME)
#                                   Data: dframe.iS
#                                   REML criterion at convergence: -7.8245
#                                   Random effects:
#                                     Groups    Name        Std.Dev.
#                                   ISO3      (Intercept) 0.718936
#                                   SHORTNAME (Intercept) 0.809279
#                                   Residual              0.004747
#                                   Number of obs: 4, groups:  ISO3, 2; SHORTNAME, 2
#                                   Fixed Effects:
#                                     (Intercept)     sanTotal  
#                                   16.457       -3.591  
#                                   
#                                   $`3073`
#                                   Linear mixed model fit by REML ['lmerModLmerTest']
#                                   Formula: change ~ sanTotal + vacTotal + (1 | ISO3) + (1 | SHORTNAME)
#                                   Data: dframe.iS
#                                   REML criterion at convergence: -7.8245
#                                   Random effects:
#                                     Groups    Name        Std.Dev.
#                                   ISO3      (Intercept) 0.718936
#                                   SHORTNAME (Intercept) 0.809279
#                                   Residual              0.004747
#                                   Number of obs: 4, groups:  ISO3, 2; SHORTNAME, 2
#                                   Fixed Effects:
#                                     (Intercept)     sanTotal  
#                                   16.457       -3.591  
#                                   fit warnings:
#                                     fixed-effect model matrix is rank deficient so dropping 1 column / coefficient
#                                   
#                                   $`5121`
#                                   Linear mixed model fit by REML ['lmerModLmerTest']
#                                   Formula: change ~ sanTotal + workTotal + (1 | ISO3) + (1 | SHORTNAME)
#                                   Data: dframe.iS
#                                   REML criterion at convergence: -7.8245
#                                   Random effects:
#                                     Groups    Name        Std.Dev.
#                                   ISO3      (Intercept) 0.718936
#                                   SHORTNAME (Intercept) 0.809279
#                                   Residual              0.004747
#                                   Number of obs: 4, groups:  ISO3, 2; SHORTNAME, 2
#                                   Fixed Effects:
#                                     (Intercept)     sanTotal  
#                                   16.457       -3.591  
#                                   fit warnings:
#                                     fixed-effect model matrix is rank deficient so dropping 1 column / coefficient
#                                   
#                                   $`7169`
#                                   Linear mixed model fit by REML ['lmerModLmerTest']
#                                   Formula: change ~ sanTotal + vacTotal + workTotal + (1 | ISO3) + (1 |      SHORTNAME)
#                                   Data: dframe.iS
#                                   REML criterion at convergence: -7.8245
#                                   Random effects:
#                                     Groups    Name        Std.Dev.
#                                   ISO3      (Intercept) 0.718936
#                                   SHORTNAME (Intercept) 0.809279
#                                   Residual              0.004747
#                                   Number of obs: 4, groups:  ISO3, 2; SHORTNAME, 2
#                                   Fixed Effects:
#                                     (Intercept)     sanTotal  
#                                   16.457       -3.591  
#                                   fit warnings:
#                                     fixed-effect model matrix is rank deficient so dropping 2 columns / coefficients
#                                   
#                                   attr(,"rank")
#                                   function (x) 
#                                     do.call("rank", list(x))
#                                   <environment: 0x0000021512600a90>
#                                     attr(,"call")
#                                   AICc(x)
#                                   attr(,"class")
#                                   [1] "function"     "rankFunction"
#                                   attr(,"beta")
#                                   [1] "sd"
#                                   > model.sel(a)
#                                   Model selection table 
#                                   (Intrc)  snTtl vcTtl wrkTt df logLik  AICc delta weight
#                                   1     0.3536                     4  1.215 -34.4  0.00  0.872
#                                   1025 16.4600 -3.591              5  3.912 -27.8  6.61  0.032
#                                   3073 16.4600 -3.591     +        5  3.912 -27.8  6.61  0.032
#                                   5121 16.4600 -3.591           +  5  3.912 -27.8  6.61  0.032
#                                   7169 16.4600 -3.591     +     +  5  3.912 -27.8  6.61  0.032
#                                   Models ranked by AICc(x) 
#                                   Random terms (all models): 
#                                     1 | ISO3, 1 | SHORTNAME














#try with fixing the random effects

globmod.dframe.iS_dredge_random_fixed <- dredge(globmod.dframe.iS, fixed = c("ISO3", "SHORTNAME"),beta="sd",evaluate=T,rank="AICc",m.lim=c(0,3) )
globmod.dframe.iS_dredge_no_fixed <- dredge(globmod.dframe.iS, beta="sd", evaluate=T, rank="AICc", m.lim=c(0,3) ,fixed = NULL)
dframe.iS_new <- dframe.iS %>% select(-c("income"))
globmod.dframe.iS_noincome <- lme4:::lmer(formula= change  ~
                            RESPONSE+
                            General+
                            MonitoringandSurveillance+
                            AwarenessandEducation+
                            sanTotal + 
                            infTotal +
                            workTotal +
                            vacTotal + 
                            GDPcap_mean+
                            PopDensity+
                            gini+
                            prod.per.area+
                            meantmpAreaPop+
                            x0008+
                            (1|ISO3) + 
                            (1|SHORTNAME),
                          data = dframe.iS_new)

globmod.dframe.iSnoinc_dredge<- dredge(globmod.dframe.iS_noincome,beta="partial.sd",evaluate=T,rank="AICc",m.lim=c(0,3) )
View(globmod.dframe.iSnoinc_dredge)



globmod.dframe.iS1 <- lme4:::lmer(formula= change  ~
                                            RESPONSE+
                                            General+
                                            MonitoringandSurveillance+
                                            AwarenessandEducation+
                                            sanTotal + 
                                            infTotal +
                                            workTotal +
                                            vacTotal + 
                                            GDPcap_mean+
                                            PopDensity+
                                            gini+
                                            prod.per.area+
                                            meantmpAreaPop+
                                            x0008+
                                            (1|ISO3) + 
                                            (1|SHORTNAME),
                                          data = dframe.iS)
globmod.dframe.iS1_dredge<- dredge(globmod.dframe.iS1, beta="sd",evaluate=T,rank="AICc",m.lim=c(0,3) )



globmod.dframe.iS2 <- lme4:::lmer(formula= change  ~
                                            RESPONSE+
                                            General+
                                         #   MonitoringandSurveillance+
                                        #  AwarenessandEducation+
                                            sanTotal + 
                                            infTotal +
                                            workTotal +
                                            vacTotal + 
                                            GDPcap_mean+
                                            PopDensity+
                                            gini+
                                            prod.per.area+
                                            meantmpAreaPop+
                                            x0008+
                                            (1|ISO3) + 
                                            (1|SHORTNAME),
                                          data = dframe.iS_new)
globmod.dframe.iS2_dredge<- dredge(globmod.dframe.iS2, beta="sd",evaluate=T,rank="AICc",m.lim=c(0,3) )


globmod.dframe.iS3 <- lme4:::lmer(formula= change  ~
                                            RESPONSE+
                                            General+
                                            MonitoringandSurveillance+
                                            AwarenessandEducation+
                                            sanTotal + 
                                            infTotal +
                                            workTotal +
                                            vacTotal + 
                                         #   GDPcap_mean+
                                        #    PopDensity+
                                            gini+
                                            prod.per.area+
                                            meantmpAreaPop+
                                            x0008+
                                            (1|ISO3) + 
                                            (1|SHORTNAME),
                                          data = dframe.iS_new)
globmod.dframe.iS3_dredge<- dredge(globmod.dframe.iS3, beta="sd",evaluate=T,rank="AICc",m.lim=c(0,3) )

globmod.dframe.iS12_dredgelist <- merge(globmod.dframe.iS1_dredge, globmod.dframe.iS2_dredge) %>% 
  rapply(as.character, classes="factor", how="replace") %>%
  unique()

globmod.dframe.iS123_dredgelist <-  merge(globmod.dframe.iS12_dredgelist, globmod.dframe.iS3_dredge)%>%
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






# Try to extraxt model selections differently
LMIC_list <-list()
for (i in 1:length(result_list_all_HIC)){
new[[i]] <- get.models(result_list_all_HIC[[i]]$mod_dredge, subset = c((1:5) & delta=0))}
new[[13]]
new[[1]] %>% summary()

#load("C:/Users/egepeh12/OneDrive - Kungl. Vetenskapsakademien/Desktop/AMR_Trends_publication/3.Model_Selection/3.1.CHANGE/3.1.3.LMIC_CHANGE/LMIC_global_workspace_output_completed.RData")
LMIC_list <-list()
for (i in 1:length(result_list_all_LMIC)){
  LMIC_list[[i]] <- get.models(result_list_all_LMIC[[i]]$mod_dredge, subset = c(1:5))}


formula_list <-list()
formula_list[[i]] <- formula(LMIC_list[[i]]$"i")


8	# Fit the 'global model'
fm <- lmer(formula= change  ~
               RESPONSE+
               General+
               MonitoringandSurveillance+
               AwarenessandEducation+
               sanTotal + 
               infTotal +
               workTotal +
               vacTotal + 
               GDPcap_mean+
               PopDensity+
               gini+
               prod.per.area+
               meantmpAreaPop+
               x0008+
               (1|ISO3) + 
               (1|SHORTNAME),
             data = dframe.iS)
	
	# Suppose we want to have set of models that exclude combinations of colinear
	# variables, that are significantly (p < 0.05) correlated, with Pearson
	# correlation coefficient larger than r = 0.5.
	
	is.correlated <- function(i, j, data, conf.level = .95, cutoff = .5, ...) {
	if(j >= i) return(NA)
	ct <- cor.test(data[, i], data[, j], conf.level = conf.level, ...)
	ct$p.value > (1 - conf.level) || abs(ct$estimate) <= cutoff
	}
# Need vectorized function to use with 'outer'
	vCorrelated <- Vectorize(is.correlated, c("i", "j"))
	
# Create logical matrix
smat <- outer(1:5, 1:5, vCorrelated, data = Cement)
	nm <- colnames(Cement[1:5])
	dimnames(smat) <- list(nm, nm)
27	
28	### A simpler case: exclude only pairs of variables having cor. coefficient
29	### r > 0.5
30	# smat <- abs(cor(Cement[, -5])) <= .5
31	# smat[!lower.tri(smat)] <- NA
32	
33	# Alternatively, we can use logical expression of form:
34	# !((V1 && V2) || (V3 && V4))
35	# where V1 is collinear with V2, and V3 with V4.
36	# Rather that doing it by hand, we can generate it from the above matrix:
	i <- as.vector(smat == FALSE & !is.na(smat))
	sexpr <-parse(text = paste("!(", paste("(",
                                          	nm[col(smat)[i]], " && ",
                                          	nm[row(smat)[i]], ")",
                                          	sep = "", collapse = " || "), ")"))
42	
	smat
	sexpr
45	
46	## =============================================================================
47	
system.time(dd2 <- dredge(fm, subset = smat))
system.time(dd1 <- dredge(fm, subset = sexpr))
50	
51	# Using the argument 'subset' in a form of matrix is usually faster in this case.
52	# The results are identical:
53	dd1
54	dd2

