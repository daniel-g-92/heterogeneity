### fix one - fixing separate models if there is a significant interaction
## (and potentially then increasing sample size)

### code for paper 5 - potential bias of subgroup effects ###
setwd("//gobo/USER40/u/u1474467/Documents/PHD/Bias paper/r out")
library("flexsurv")
library("survminer")
set.seed(2020)

#### general parameters ####

armsize <- 448 # overall sample size per arm 

nsim <- 10000
subprop <- c(0.1, 0.25, 0.5)  # proportion of sample size in the subgroup. Designator: g  // subgroup varies in size



parafits <- c("exponential", "weibull", "lnorm", "llog", "gamma", "gengamma", "gompertz", "genf") # 
#these names match the commands to calculate the means survival.
meanfits <- c("rmst_exp", "rmst_weibull", "rmst_lnorm",  "rmst_llogis", "rmst_gamma", "rmst_gengamma", "rmst_gompertz", "rmst_genf") # 
timehorizon <- 30
pars <- matrix(, nrow=8, ncol=12)
pars2 <- matrix(, nrow=8, ncol=12)

#### NOTE  cont = control; int = intervention; s = subgroup; c = complement. ####

#### SCENARIO 1 Parameters #### select one set to run, and also change output file at the bottom of this code.


#### SCENARIO 2 Parameters etc..
haz_cont_c <- 0.25
haz_int_c <- 0.25
haz_cont_s <- 0.5
haz_int_s <- 0.35



###### CODE #####

g <- 3

compss <- (1-subprop[g])*armsize
subss <- subprop[g]*armsize   # sample size


## first digit: 0 = control, 1 = intervention
## second digit: 0 = complement, 1 = subgroup
armid00 <- rep(0,compss)  # complement control
armid01 <- rep(0,subss)   # subgroup control
armid10 <- rep(1,compss)  # complement intervention
armid11 <- rep(1,subss)   # subgroup intervention
subid00 <- rep(0, compss) # complement control
subid01 <- rep(1,subss)   # subgroup control
subid10 <- rep(0, compss) # complement intervention
subid11 <- rep(1,subss)   # subgroup intervention

#### prepping results table ####
results <- data.frame(id = 1:nsim)   # start of results matrix
results$truemean_cont <- subprop[g]*rmst_exp(timehorizon,haz_cont_s) + (1-subprop[g])*rmst_exp(timehorizon,haz_cont_c)
results$truemean_int <- subprop[g]*rmst_exp(timehorizon,haz_int_s) + (1-subprop[g])*rmst_exp(timehorizon,haz_int_c)
results$truemean_diff  <- results$truemean_int - results$truemean_cont

results$aicexp_i <- NA
results$bicexp_i <- NA
results$meanexp_i <- NA
results$medexp_i <- NA
results$aicwei_i <- NA
results$bicwei_i <- NA
results$meanwei_i <- NA
results$medwei_i <- NA
results$aiclnorm_i <- NA
results$biclnorm_i <- NA
results$meanlnorm_i <- NA
results$medlnorm_i <- NA
results$aicllog_i <- NA
results$bicllog_i <- NA 
results$meanllog_i <- NA
results$medllog_i <- NA
results$aicgam_i <- NA
results$bicgam_i <- NA
results$meangam_i <- NA
results$medgam_i <- NA
results$aicgeng_i <- NA
results$bicgeng_i <- NA 
results$meangeng_i <- NA
results$medgeng_i <- NA
results$aicgomp_i <- NA
results$bicgomp_i <- NA
results$meangomp_i <- NA
results$medgomp_i <- NA
results$aicgenf_i <- NA
results$bicgenf_i <- NA 
results$meangenf_i <- NA
results$medgenf_i <- NA
results$aicexp_c <- NA
results$bicexp_c <- NA
results$meanexp_c <- NA
results$medexp_c <- NA
results$aicwei_c <- NA
results$bicwei_c <- NA
results$meanwei_c <- NA
results$medwei_c <- NA
results$aiclnorm_c <- NA
results$biclnorm_c <- NA
results$meanlnorm_c <- NA
results$medlnorm_c <- NA
results$aicllog_c <- NA
results$bicllog_c <- NA 
results$meanllog_c <- NA
results$medllog_c <- NA
results$aicgam_c <- NA
results$bicgam_c <- NA
results$meangam_c <- NA
results$medgam_c <- NA
results$aicgeng_c <- NA
results$bicgeng_c <- NA 
results$meangeng_c <- NA
results$medgeng_c <- NA
results$aicgomp_c <- NA
results$bicgomp_c <- NA
results$meangomp_c <- NA
results$medgomp_c <- NA
results$aicgenf_c <- NA
results$bicgenf_c <- NA 
results$meangenf_c <- NA
results$medgenf_c <- NA
results$c_low_aic <- NA
results$c_low_aic_ind <- NA
results$c_low_aic_model <- NA
results$c_low_aic_mean <- NA
results$c_low_aic_median <- NA
results$c_low_bic <- NA
results$c_low_bic_ind <- NA
results$c_low_bic_model <- NA
results$c_low_bic_mean <- NA
results$c_low_bic_median <- NA
results$i_low_aic <- NA
results$i_low_aic_ind <- NA
results$i_low_aic_model <- NA
results$i_low_aic_mean <- NA
results$i_low_aic_median <- NA
results$i_low_bic <- NA
results$i_low_bic_ind <- NA
results$i_low_bic_model <- NA
results$i_low_bic_mean <- NA
results$i_low_bic_median <- NA

results$overall_low_aic_ind <- NA
results$overall_low_aic_model <- NA
results$overall_low_aic_mean_c <- NA
results$overall_low_aic_median_c <- NA
results$overall_low_aic_mean_i <- NA
results$overall_low_aic_median_i <- NA
results$overall_low_bic_ind <- NA
results$overall_low_bic_model <- NA
results$overall_low_bic_mean_c <- NA
results$overall_low_bic_median_c <- NA
results$overall_low_bic_mean_i <- NA
results$overall_low_bic_median_i <- NA

results$exp_aic_combi <- NA
results$wei_aic_combi <- NA
results$lnorm_aic_combi <- NA
results$llog_aic_combi <- NA
results$gam_aic_combi <- NA
results$geng_aic_combi <- NA
results$gomp_aic_combi <- NA
results$genf_aic_combi <- NA
results$exp_bic_combi <- NA
results$wei_bic_combi <- NA
results$lnorm_bic_combi <- NA
results$llog_bic_combi <- NA
results$gam_bic_combi <- NA
results$geng_bic_combi <- NA
results$gomp_bic_combi <- NA
results$genf_bic_combi <- NA

results$n_plausible <- NA
results$average_mean_c <- NA
results$average_median_c <- NA
results$average_mean_i <- NA
results$average_median_i <- NA
results$cox_hr <- NA
results$cox_p <- NA
results$cox_interact_p <- NA
results$km_mean_est_i <- NA
results$km_mean_est_c <- NA
results$km_mean_est_i_sub <- NA
results$km_mean_est_i_comp <- NA
results$km_mean_est_c_sub <- NA
results$km_mean_est_c_comp <- NA

#start of comp results
results$aicexp_i_comp <- NA
results$bicexp_i_comp <- NA
results$meanexp_i_comp <- NA
results$medexp_i_comp <- NA
results$aicwei_i_comp <- NA
results$bicwei_i_comp <- NA
results$meanwei_i_comp <- NA
results$medwei_i_comp <- NA
results$aiclnorm_i_comp <- NA
results$biclnorm_i_comp <- NA
results$meanlnorm_i_comp <- NA
results$medlnorm_i_comp <- NA
results$aicllog_i_comp <- NA
results$bicllog_i_comp <- NA 
results$meanllog_i_comp <- NA
results$medllog_i_comp <- NA
results$aicgam_i_comp <- NA
results$bicgam_i_comp <- NA
results$meangam_i_comp <- NA
results$medgam_i_comp <- NA
results$aicgeng_i_comp <- NA
results$bicgeng_i_comp <- NA 
results$meangeng_i_comp <- NA
results$medgeng_i_comp <- NA
results$aicgomp_i_comp <- NA
results$bicgomp_i_comp <- NA
results$meangomp_i_comp <- NA
results$medgomp_i_comp <- NA
results$aicgenf_i_comp <- NA
results$bicgenf_i_comp <- NA 
results$meangenf_i_comp <- NA
results$medgenf_i_comp <- NA
results$aicexp_c_comp <- NA
results$bicexp_c_comp <- NA
results$meanexp_c_comp <- NA
results$medexp_c_comp <- NA
results$aicwei_c_comp <- NA
results$bicwei_c_comp <- NA
results$meanwei_c_comp <- NA
results$medwei_c_comp <- NA
results$aiclnorm_c_comp <- NA
results$biclnorm_c_comp <- NA
results$meanlnorm_c_comp <- NA
results$medlnorm_c_comp <- NA
results$aicllog_c_comp <- NA
results$bicllog_c_comp <- NA 
results$meanllog_c_comp <- NA
results$medllog_c_comp <- NA
results$aicgam_c_comp <- NA
results$bicgam_c_comp <- NA
results$meangam_c_comp <- NA
results$medgam_c_comp <- NA
results$aicgeng_c_comp <- NA
results$bicgeng_c_comp <- NA 
results$meangeng_c_comp <- NA
results$medgeng_c_comp <- NA
results$aicgomp_c_comp <- NA
results$bicgomp_c_comp <- NA
results$meangomp_c_comp <- NA
results$medgomp_c_comp <- NA
results$aicgenf_c_comp <- NA
results$bicgenf_c_comp <- NA 
results$meangenf_c_comp <- NA
results$medgenf_c_comp <- NA
results$c_low_aic_comp <- NA
results$c_low_aic_ind_comp <- NA
results$c_low_aic_model_comp <- NA
results$c_low_aic_mean_comp <- NA
results$c_low_aic_median_comp <- NA
results$c_low_bic_comp <- NA
results$c_low_bic_ind_comp <- NA
results$c_low_bic_model_comp <- NA
results$c_low_bic_mean_comp <- NA
results$c_low_bic_median_comp <- NA
results$i_low_aic_comp <- NA
results$i_low_aic_ind_comp <- NA
results$i_low_aic_model_comp <- NA
results$i_low_aic_mean_comp <- NA
results$i_low_aic_median_comp <- NA
results$i_low_bic_comp <- NA
results$i_low_bic_ind_comp <- NA
results$i_low_bic_model_comp <- NA
results$i_low_bic_mean_comp <- NA
results$i_low_bic_median_comp <- NA

results$overall_low_aic_ind_comp <- NA
results$overall_low_aic_model_comp <- NA
results$overall_low_aic_mean_c_comp <- NA
results$overall_low_aic_median_c_comp <- NA
results$overall_low_aic_mean_i_comp <- NA
results$overall_low_aic_median_i_comp <- NA
results$overall_low_bic_ind_comp <- NA
results$overall_low_bic_model_comp <- NA
results$overall_low_bic_mean_c_comp <- NA
results$overall_low_bic_median_c_comp <- NA
results$overall_low_bic_mean_i_comp <- NA
results$overall_low_bic_median_i_comp <- NA

results$exp_aic_combi_comp <- NA
results$wei_aic_combi_comp <- NA
results$lnorm_aic_combi_comp <- NA
results$llog_aic_combi_comp <- NA
results$gam_aic_combi_comp <- NA
results$geng_aic_combi_comp <- NA
results$gomp_aic_combi_comp <- NA
results$genf_aic_combi_comp <- NA
results$exp_bic_combi_comp <- NA
results$wei_bic_combi_comp <- NA
results$lnorm_bic_combi_comp <- NA
results$llog_bic_combi_comp <- NA
results$gam_bic_combi_comp <- NA
results$geng_bic_combi_comp <- NA
results$gomp_bic_combi_comp <- NA
results$genf_bic_combi_comp <- NA

results$n_plausible_comp <- NA
results$average_mean_c_comp <- NA
results$average_median_c_comp <- NA
results$average_mean_i_comp <- NA
results$average_median_i_comp <- NA

## end of comp results
## start of subgroup results
#start of subgroup results
results$aicexp_i_sub <- NA
results$bicexp_i_sub <- NA
results$meanexp_i_sub <- NA
results$medexp_i_sub <- NA
results$aicwei_i_sub <- NA
results$bicwei_i_sub <- NA
results$meanwei_i_sub <- NA
results$medwei_i_sub <- NA
results$aiclnorm_i_sub <- NA
results$biclnorm_i_sub <- NA
results$meanlnorm_i_sub <- NA
results$medlnorm_i_sub <- NA
results$aicllog_i_sub <- NA
results$bicllog_i_sub <- NA 
results$meanllog_i_sub <- NA
results$medllog_i_sub <- NA
results$aicgam_i_sub <- NA
results$bicgam_i_sub <- NA
results$meangam_i_sub <- NA
results$medgam_i_sub <- NA
results$aicgeng_i_sub <- NA
results$bicgeng_i_sub <- NA 
results$meangeng_i_sub <- NA
results$medgeng_i_sub <- NA
results$aicgomp_i_sub <- NA
results$bicgomp_i_sub <- NA
results$meangomp_i_sub <- NA
results$medgomp_i_sub <- NA
results$aicgenf_i_sub <- NA
results$bicgenf_i_sub <- NA 
results$meangenf_i_sub <- NA
results$medgenf_i_sub <- NA
results$aicexp_c_sub <- NA
results$bicexp_c_sub <- NA
results$meanexp_c_sub <- NA
results$medexp_c_sub <- NA
results$aicwei_c_sub <- NA
results$bicwei_c_sub <- NA
results$meanwei_c_sub <- NA
results$medwei_c_sub <- NA
results$aiclnorm_c_sub <- NA
results$biclnorm_c_sub <- NA
results$meanlnorm_c_sub <- NA
results$medlnorm_c_sub <- NA
results$aicllog_c_sub <- NA
results$bicllog_c_sub <- NA 
results$meanllog_c_sub <- NA
results$medllog_c_sub <- NA
results$aicgam_c_sub <- NA
results$bicgam_c_sub <- NA
results$meangam_c_sub <- NA
results$medgam_c_sub <- NA
results$aicgeng_c_sub <- NA
results$bicgeng_c_sub <- NA 
results$meangeng_c_sub <- NA
results$medgeng_c_sub <- NA
results$aicgomp_c_sub <- NA
results$bicgomp_c_sub <- NA
results$meangomp_c_sub <- NA
results$medgomp_c_sub <- NA
results$aicgenf_c_sub <- NA
results$bicgenf_c_sub <- NA 
results$meangenf_c_sub <- NA
results$medgenf_c_sub <- NA
results$c_low_aic_sub <- NA
results$c_low_aic_ind_sub <- NA
results$c_low_aic_model_sub <- NA
results$c_low_aic_mean_sub <- NA
results$c_low_aic_median_sub <- NA
results$c_low_bic_sub <- NA
results$c_low_bic_ind_sub <- NA
results$c_low_bic_model_sub <- NA
results$c_low_bic_mean_sub <- NA
results$c_low_bic_median_sub <- NA
results$i_low_aic_sub <- NA
results$i_low_aic_ind_sub <- NA
results$i_low_aic_model_sub <- NA
results$i_low_aic_mean_sub <- NA
results$i_low_aic_median_sub <- NA
results$i_low_bic_sub <- NA
results$i_low_bic_ind_sub <- NA
results$i_low_bic_model_sub <- NA
results$i_low_bic_mean_sub <- NA
results$i_low_bic_median_sub <- NA

results$overall_low_aic_ind_sub <- NA
results$overall_low_aic_model_sub <- NA
results$overall_low_aic_mean_c_sub <- NA
results$overall_low_aic_median_c_sub <- NA
results$overall_low_aic_mean_i_sub <- NA
results$overall_low_aic_median_i_sub <- NA
results$overall_low_bic_ind_sub <- NA
results$overall_low_bic_model_sub <- NA
results$overall_low_bic_mean_c_sub <- NA
results$overall_low_bic_median_c_sub <- NA
results$overall_low_bic_mean_i_sub <- NA
results$overall_low_bic_median_i_sub <- NA

results$exp_aic_combi_sub <- NA
results$wei_aic_combi_sub <- NA
results$lnorm_aic_combi_sub <- NA
results$llog_aic_combi_sub <- NA
results$gam_aic_combi_sub <- NA
results$geng_aic_combi_sub <- NA
results$gomp_aic_combi_sub <- NA
results$genf_aic_combi_sub <- NA
results$exp_bic_combi_sub <- NA
results$wei_bic_combi_sub <- NA
results$lnorm_bic_combi_sub <- NA
results$llog_bic_combi_sub <- NA
results$gam_bic_combi_sub <- NA
results$geng_bic_combi_sub <- NA
results$gomp_bic_combi_sub <- NA
results$genf_bic_combi_sub <- NA

results$n_plausible_sub <- NA
results$average_mean_c_sub <- NA
results$average_median_c_sub <- NA
results$average_mean_i_sub <- NA
results$average_median_i_sub <- NA

## end of subgroup results
#### sim start #### 

for (k in 1:nsim) {
  
  exp_survcomp0 <- data.frame(rexp(compss, rate = haz_cont_c), armid00, subid00) # complement control
  exp_survcomp1 <- data.frame(rexp(compss, rate = haz_int_c), armid10, subid10) # complement intervention
  exp_survsub0 <- data.frame(rexp(subss, rate = haz_cont_s), armid01, subid01) # subgroup control
  exp_survsub1 <- data.frame(rexp(subss, rate = haz_int_s), armid11, subid11) # subgroup intervention
  
  
  colnames(exp_survcomp0) <- c("stime1", "armid", "subid")
  colnames(exp_survcomp1) <- c("stime1", "armid", "subid")
  colnames(exp_survsub0) <- c("stime1", "armid", "subid")
  colnames(exp_survsub1) <- c("stime1", "armid", "subid")
  
  
  exp_survsub <- rbind(exp_survsub0, exp_survsub1)
  exp_survcomp <- rbind(exp_survcomp0, exp_survcomp1)
  exp_survsub$censtime <- rgompertz(subss, shape =3.5, rate =0.00005)
  exp_survcomp$censtime <- rgompertz(compss, shape =3.5, rate =0.00005)
  
  exp_survcomp$ev1cens0[exp_survcomp$censtime<exp_survcomp$stime1] <- 0
  exp_survcomp$ev1cens0[exp_survcomp$censtime>exp_survcomp$stime1] <- 1
  exp_survcomp$stime2[exp_survcomp$ev1cens0==1] <- exp_survcomp$stime1[exp_survcomp$ev1cens0==1]
  exp_survcomp$stime2[exp_survcomp$ev1cens0==0] <- exp_survcomp$censtime[exp_survcomp$ev1cens0==0]  
  
  exp_survsub$ev1cens0[exp_survsub$censtime<exp_survsub$stime1] <- 0
  exp_survsub$ev1cens0[exp_survsub$censtime>exp_survsub$stime1] <- 1
  exp_survsub$stime2[exp_survsub$ev1cens0==1] <- exp_survsub$stime1[exp_survsub$ev1cens0==1]
  exp_survsub$stime2[exp_survsub$ev1cens0==0] <- exp_survsub$censtime[exp_survsub$ev1cens0==0] 
  
  exp_survpool <- rbind(exp_survsub, exp_survcomp) 
  exp_survpool$armid <- as.factor(exp_survpool$armid)
  exp_surv_c <- exp_survpool[exp_survpool$armid==0,]
  exp_surv_i <- exp_survpool[exp_survpool$armid==1,]
  exp_surv_c_comp <- exp_survpool[exp_survpool$armid==0 & exp_survpool$subid==0,]
  exp_surv_i_comp <- exp_survpool[exp_survpool$armid==1 & exp_survpool$subid==0,]
  exp_surv_c_sub <- exp_survpool[exp_survpool$armid==0 & exp_survpool$subid==1,]
  exp_surv_i_sub <- exp_survpool[exp_survpool$armid==1 & exp_survpool$subid==1,]
  
  
  ## fit km model and estimate mean that way.
  
  
  exp_km_i <- survfit(Surv(stime1)~armid, data = exp_surv_i)
  exp_km_c <- survfit(Surv(stime1)~armid, data = exp_surv_c)
  exp_km_i_sub <- survfit(Surv(stime1)~armid, data = exp_survsub1)
  exp_km_i_comp <- survfit(Surv(stime1)~armid, data = exp_survcomp1)
  exp_km_c_sub <- survfit(Surv(stime1)~armid, data = exp_survsub0)
  exp_km_c_comp <- survfit(Surv(stime1)~armid, data = exp_survcomp0)
  
  
  
  #print(exp_km_i,rmean=1000)
  km_out_i <- strsplit(capture.output(print(exp_km_i, rmean=25, digits=2))[4],split="     ")
  km_out_c <- strsplit(capture.output(print(exp_km_c, rmean=25, digits=2))[4],split="     ")
  km_out_i_sub <- strsplit(capture.output(print(exp_km_i_sub, rmean=25, digits=2))[4],split="     ")
  km_out_c_sub <- strsplit(capture.output(print(exp_km_c_sub, rmean=25, digits=2))[4],split="     ")
  km_out_i_comp <- strsplit(capture.output(print(exp_km_i_comp, rmean=25, digits=2))[4],split="    ")
  km_out_c_comp <- strsplit(capture.output(print(exp_km_c_comp, rmean=25, digits=2))[4],split="    ")
  
  
  results$km_mean_est_i[k] <-  as.numeric(km_out_i[[1]])[3]
  results$km_mean_est_c[k] <- as.numeric(km_out_c[[1]])[3]
  results$km_mean_est_i_sub[k] <- as.numeric(km_out_i_sub[[1]])[3]
  results$km_mean_est_i_comp[k] <- as.numeric(km_out_i_comp[[1]])[3]
  results$km_mean_est_c_sub[k] <- as.numeric(km_out_c_sub[[1]])[3]
  results$km_mean_est_c_comp[k] <- as.numeric(km_out_c_comp[[1]])[3]
  
  if (results$km_mean_est_i[k] > 30){
    results$km_mean_est_i[k] <- as.numeric(km_out_i[[1]])[4]
  }
  if (results$km_mean_est_c[k] > 30){
    results$km_mean_est_c[k] <- as.numeric(km_out_c[[1]])[4]
  }
  if (results$km_mean_est_i_sub[k] > 30){
    results$km_mean_est_i_sub[k] <- as.numeric(km_out_i_sub[[1]])[4]
  }
  if (results$km_mean_est_i_comp[k] > 30){
    results$km_mean_est_i_comp[k] <- as.numeric(km_out_i_comp[[1]])[4]
  }
  if (results$km_mean_est_c_sub[k] > 30){
    results$km_mean_est_c_sub[k] <- as.numeric(km_out_c_sub[[1]])[4]
  }
  if (results$km_mean_est_c_comp[k] > 30){
    results$km_mean_est_c_comp[k] <- as.numeric(km_out_c_comp[[1]])[4]
  }
  if (is.na(results$km_mean_est_i_sub[k])){
    results$km_mean_est_i_sub[k] <- as.numeric(km_out_i_sub[[1]])[5]
  }
  if (is.na(results$km_mean_est_i_comp[k])){
    results$km_mean_est_i_comp[k] <- as.numeric(km_out_i_comp[[1]])[5]
  }
  if (is.na(results$km_mean_est_c_sub[k])){
    results$km_mean_est_c_sub[k] <- as.numeric(km_out_c_sub[[1]])[5]
  }
  if (is.na(results$km_mean_est_c_comp[k])){
    results$km_mean_est_c_comp[k] <- as.numeric(km_out_c_comp[[1]])[5]
  }
  
  
  
  
  #### fit cox models ####
  cox_mod <- coxph(Surv(stime2, ev1cens0) ~ armid, data=exp_survpool)
  sum_cox_mod <- summary(cox_mod)
  results$cox_hr[k] <- sum_cox_mod$coefficients[2]
  results$cox_p[k] <- sum_cox_mod$coefficients[5]
  
  cox_mod_int <- coxph(Surv(stime2, ev1cens0) ~ armid*subid, data=exp_survpool)
  
  sum_cox_mod_int <- summary(cox_mod_int)
  results$cox_interact_p[k] <- sum_cox_mod_int$coefficients[3,5]
  
  if (results$cox_interact_p[k] < 0.05) {
    
    # establish plausibility thresholds
    est_i_5_comp <- c(rep(NA,8))
    est_i_10_comp <- c(rep(NA,8))
    est_c_5_comp <- c(rep(NA,8))
    est_c_10_comp <- c(rep(NA,8))
    
    c_5y_ind_comp <- c(rep(NA,8))
    c_10y_ind_comp <- c(rep(NA,8))
    i_5y_ind_comp <- c(rep(NA,8))
    i_10y_ind_comp <- c(rep(NA,8))
    
    i_5y_true_comp <-  exp(-5*haz_int_c)
    i_10y_true_comp <-  exp(-10*haz_int_c)
    c_5y_true_comp <-  exp(-5*haz_cont_c)
    c_10y_true_comp <-  exp(-10*haz_cont_c)
    
    i_lower_5y_comp <- max(i_5y_true_comp - 0.075,0)
    i_upper_5y_comp <-  i_5y_true_comp + 0.075
    i_lower_10y_comp <-  max(i_10y_true_comp - 0.05,0)
    i_upper_10y_comp <-  i_10y_true_comp + 0.05
    c_lower_5y_comp <- max(c_5y_true_comp - 0.075,0)
    c_upper_5y_comp <-  c_5y_true_comp + 0.075
    c_lower_10y_comp <-  max(c_10y_true_comp - 0.05,0)
    c_upper_10y_comp <-  c_10y_true_comp + 0.05
    
    est_i_5_sub <- c(rep(NA,8))
    est_i_10_sub <- c(rep(NA,8))
    est_c_5_sub <- c(rep(NA,8))
    est_c_10_sub <- c(rep(NA,8))
    
    c_5y_ind_sub <- c(rep(NA,8))
    c_10y_ind_sub <- c(rep(NA,8))
    i_5y_ind_sub <- c(rep(NA,8))
    i_10y_ind_sub <- c(rep(NA,8))
    
    i_5y_true_sub <- exp(-5*haz_int_s) 
    i_10y_true_sub <- exp(-10*haz_int_s) 
    c_5y_true_sub <- exp(-5*haz_cont_s) 
    c_10y_true_sub <- exp(-10*haz_cont_s) 
    
    i_lower_5y_sub <- max(i_5y_true_sub - 0.075,0)
    i_upper_5y_sub <-  i_5y_true_sub + 0.075
    i_lower_10y_sub <-  max(i_10y_true_sub - 0.05,0)
    i_upper_10y_sub <-  i_10y_true_sub + 0.05
    c_lower_5y_sub <- max(c_5y_true_sub - 0.075,0)
    c_upper_5y_sub <-  c_5y_true_sub + 0.075
    c_lower_10y_sub <-  max(c_10y_true_sub - 0.05,0)
    c_upper_10y_sub <-  c_10y_true_sub + 0.05
    
    
    # fit models to comp and sub separately
    #comp first
    for (i in 1:8) {
      
      tryCatch(expr = {
        m1_comp <- flexsurvreg(Surv(stime2,ev1cens0) ~ 1, data=exp_surv_i_comp, dist = parafits[i], sr.control = survreg.control(maxit = 1000))
        m2_comp <- flexsurvreg(Surv(stime2,ev1cens0) ~ 1, data=exp_surv_c_comp, dist = parafits[i], sr.control = survreg.control(maxit = 1000))
        
        m1_sum_comp <- NA 
        est_i_5_comp <- NA   
        est_i_10_comp <- NA 
        m2_sum_comp <- NA
        est_c_5_comp <-NA   
        est_c_10_comp <- NA
        
        m1_sum_comp <- summary(m1_comp, t=c(5,10))
        est_i_5_comp <- m1_sum_comp[[1]]$est[1]   
        est_i_10_comp <- m1_sum_comp[[1]]$est[2] 
        m2_sum_comp <- summary(m2_comp, t=c(5,10))
        est_c_5_comp <- m2_sum_comp[[1]]$est[1]   
        est_c_10_comp <- m2_sum_comp[[1]]$est[2] 
        
        
        
        if(est_i_5_comp<i_upper_5y_comp & est_i_5_comp>i_lower_5y_comp  & est_i_10_comp<i_upper_10y_comp & est_i_10_comp>i_lower_10y_comp & est_c_5_comp<c_upper_5y_comp & est_c_5_comp>c_lower_5y_comp & est_c_10_comp<c_upper_10y_comp & est_c_10_comp>c_lower_10y_comp ) {
          
          results[k,4*i+127] <- m1_comp$AIC
          results[k,4*i+128] <- log(m1_comp$N)*m1_comp$npars - 2*m1_comp$loglik  
          pars[i,] <- m1_comp$res[,1]
          if (i != 8) {
            med1 <- as.data.frame(summary(m1_comp, type = "median"))[1]
          }
          results[k,4*i+130] <- med1
          
          if (m1_comp$npars == 4){
            P1 <- pars[i,1]
            P2 <- pars[i,2]
            P3 <- pars[i,3]
            P4 <- pars[i,4]
            try(results[k,4*i+129] <- get(paste(meanfits[i]))(timehorizon,P1,P2,P3,P4))
          }
          if (m1_comp$npars == 3){
            P1 <- pars[i,1]
            P2 <- pars[i,2]
            P3 <- pars[i,3]
            try(results[k,4*i+129] <- get(paste(meanfits[i]))(timehorizon,P1,P2,P3))
          }
          if (m1_comp$npars == 2){
            P1 <- pars[i,1]
            P2 <- pars[i,2]
            try(results[k,4*i+129] <- get(paste(meanfits[i]))(timehorizon,P1,P2))
          }
          if (m1_comp$npars == 1){
            P1 <- pars[i,1]
            try(results[k,4*i+129] <- get(paste(meanfits[i]))(timehorizon,P1))
          }
          
          ## m2 ##
          
          results[k,159+4*i] <- m2_comp$AIC
          results[k,160+4*i] <- log(m2_comp$N)*m2_comp$npars - 2*m2_comp$loglik  
          pars2[i,] <- m2_comp$res[,1]
          if (i != 8) {
            med1 <- as.data.frame(summary(m2_comp, type = "median"))[1]
          }
          results[k,162+4*i] <- med1
          
          if (m2_comp$npars == 4){
            P1 <- pars[i,1]
            P2 <- pars[i,2]
            P3 <- pars[i,3]
            P4 <- pars[i,4]
            try(results[k,161+4*i] <- get(paste(meanfits[i]))(timehorizon,P1,P2,P3,P4))
          }
          if (m2_comp$npars == 3){
            P1 <- pars2[i,1]
            P2 <- pars2[i,2]
            P3 <- pars2[i,3]
            try(results[k,161+4*i] <- get(paste(meanfits[i]))(timehorizon,P1,P2,P3))
          }
          if (m2_comp$npars == 2){
            P1 <- pars2[i,1]
            P2 <- pars2[i,2]
            try(results[k,161+4*i] <- get(paste(meanfits[i]))(timehorizon,P1,P2))
          }
          if (m2_comp$npars == 1){
            P1 <- pars2[i,1]
            try(results[k,161+4*i] <- get(paste(meanfits[i]))(timehorizon,P1))
          }    
          
          rm(m1_comp,m2_comp)  }
        
      }, 
      error=function(e){ 
        message("Error"," ",k,"  ", 1, "  ", i)
        message(e)
        return(NA)
      }, 
      warning=function(w){
        message("Warning"," ", k,"  ", 1, "  ", i)
        message(w)
      },
      finally = {
      } )
      
    }
    
    # now subgroup model fitting
    
    for (i in 1:8) {
      
      tryCatch(expr = {
        m1_sub <- flexsurvreg(Surv(stime2,ev1cens0) ~ 1, data=exp_surv_i_sub, dist = parafits[i], sr.control = survreg.control(maxit = 1000))
        m2_sub <- flexsurvreg(Surv(stime2,ev1cens0) ~ 1, data=exp_surv_c_sub, dist = parafits[i], sr.control = survreg.control(maxit = 1000))
        
        m1_sum_sub <- NA 
        est_i_5_sub <- NA   
        est_i_10_sub <- NA 
        m2_sum_sub <- NA
        est_c_5_sub <-NA   
        est_c_10_sub <- NA
        
        m1_sum_sub <- summary(m1_sub, t=c(5,10))
        est_i_5_sub <- m1_sum_sub[[1]]$est[1]   
        est_i_10_sub <- m1_sum_sub[[1]]$est[2] 
        m2_sum_sub <- summary(m2_sub, t=c(5,10))
        est_c_5_sub <- m2_sum_sub[[1]]$est[1]   
        est_c_10_sub <- m2_sum_sub[[1]]$est[2] 
        
        
        
        if(est_i_5_sub<i_upper_5y_sub & est_i_5_sub>i_lower_5y_sub  & est_i_10_sub<i_upper_10y_sub & est_i_10_sub>i_lower_10y_sub & est_c_5_sub<c_upper_5y_sub & est_c_5_sub>c_lower_5y_sub & est_c_10_sub<c_upper_10y_sub & est_c_10_sub>c_lower_10y_sub ) {
          
          results[k,4*i+244] <- m1_sub$AIC
          results[k,4*i+245] <- log(m1_sub$N)*m1_sub$npars - 2*m1_sub$loglik  
          pars[i,] <- m1_sub$res[,1]
          if (i != 8) {
            med1 <- as.data.frame(summary(m1_sub, type = "median"))[1]
          }
          results[k,4*i+247] <- med1
          
          if (m1_sub$npars == 4){
            P1 <- pars[i,1]
            P2 <- pars[i,2]
            P3 <- pars[i,3]
            P4 <- pars[i,4]
            try(results[k,4*i+246] <- get(paste(meanfits[i]))(timehorizon,P1,P2,P3,P4))
          }
          if (m1_sub$npars == 3){
            P1 <- pars[i,1]
            P2 <- pars[i,2]
            P3 <- pars[i,3]
            try(results[k,4*i+246] <- get(paste(meanfits[i]))(timehorizon,P1,P2,P3))
          }
          if (m1_sub$npars == 2){
            P1 <- pars[i,1]
            P2 <- pars[i,2]
            try(results[k,4*i+246] <- get(paste(meanfits[i]))(timehorizon,P1,P2))
          }
          if (m1_sub$npars == 1){
            P1 <- pars[i,1]
            try(results[k,4*i+246] <- get(paste(meanfits[i]))(timehorizon,P1))
          }
          
          ## m2 ##
          
          results[k,276+4*i] <- m2_sub$AIC
          results[k,277+4*i] <- log(m2_sub$N)*m2_sub$npars - 2*m2_sub$loglik  
          pars2[i,] <- m2_sub$res[,1]
          if (i != 8) {
            med1 <- as.data.frame(summary(m2_sub, type = "median"))[1]
          }
          results[k,279+4*i] <- med1
          
          if (m2_sub$npars == 4){
            P1 <- pars[i,1]
            P2 <- pars[i,2]
            P3 <- pars[i,3]
            P4 <- pars[i,4]
            try(results[k,278+4*i] <- get(paste(meanfits[i]))(timehorizon,P1,P2,P3,P4))
          }
          if (m2_sub$npars == 3){
            P1 <- pars2[i,1]
            P2 <- pars2[i,2]
            P3 <- pars2[i,3]
            try(results[k,278+4*i] <- get(paste(meanfits[i]))(timehorizon,P1,P2,P3))
          }
          if (m2_sub$npars == 2){
            P1 <- pars2[i,1]
            P2 <- pars2[i,2]
            try(results[k,278+4*i] <- get(paste(meanfits[i]))(timehorizon,P1,P2))
          }
          if (m2_sub$npars == 1){
            P1 <- pars2[i,1]
            try(results[k,278+4*i] <- get(paste(meanfits[i]))(timehorizon,P1))
          }    
          
          rm(m1_sub,m2_sub)  }
        
      }, 
      error=function(e){ 
        message("Error"," ",k,"  ", 1, "  ", i)
        message(e)
        return(NA)
      }, 
      warning=function(w){
        message("Warning"," ", k,"  ", 1, "  ", i)
        message(w)
      },
      finally = {
      } )
      
    }
    
    
    # apply model selection methods and estimate 
    
    # model selection and averaging complement
    if (is.na(results$aicexp_c_comp[k]) & is.na(results$aicwei_c_comp[k]) & is.na(results$aiclnorm_c_comp[k]) & is.na(results$aicllog_c_comp[k]) & is.na(results$aicgam_c_comp[k]) & is.na(results$aicgeng_c_comp[k]) & is.na(results$aicgomp_c_comp[k]) & is.na(results$aicgenf_c_comp[k])){
    }else{
      results$c_low_aic_ind_comp[k] <- which.min(c(results$aicexp_c_comp[k],results$aicwei_c_comp[k], results$aiclnorm_c_comp[k], results$aicllog_c_comp[k], results$aicgam_c_comp[k], results$aicgeng_c_comp[k],results$aicgomp_c_comp[k],results$aicgenf_c_comp[k]))
      results$c_low_aic_model_comp[k] <- parafits[results$c_low_aic_ind_comp[k]]
      results$c_low_aic_mean_comp[k] <- c(results$meanexp_c_comp[k],results$meanwei_c_comp[k], results$meanlnorm_c_comp[k], results$meanllog_c_comp[k], results$meangam_c_comp[k], results$meangeng_c_comp[k],results$meangomp_c_comp[k],results$meangenf_c_comp[k])[results$c_low_aic_ind_comp[k]]
      results$c_low_aic_median_comp[k] <- c(results$medexp_c_comp[k],results$medwei_c_comp[k], results$medlnorm_c_comp[k], results$medllog_c_comp[k], results$medgam_c_comp[k], results$medgeng_c_comp[k],results$medgomp_c_comp[k],results$medgenf_c_comp[k])[results$c_low_aic_ind_comp[k]]
    }
    
    if (is.na(results$aicexp_i_comp[k]) & is.na(results$aicwei_i_comp[k]) & is.na(results$aiclnorm_i_comp[k]) & is.na(results$aicllog_i_comp[k]) & is.na(results$aicgam_i_comp[k]) & is.na(results$aicgeng_i_comp[k]) & is.na(results$aicgomp_i_comp[k]) & is.na(results$aicgenf_i_comp[k])){
    }else{
      results$i_low_aic_ind_comp[k] <- which.min(c(results$aicexp_i_comp[k],results$aicwei_i_comp[k], results$aiclnorm_i_comp[k], results$aicllog_i_comp[k], results$aicgam_i_comp[k], results$aicgeng_i_comp[k],results$aicgomp_i_comp[k],results$aicgenf_i_comp[k]))
      results$i_low_aic_model_comp[k] <- parafits[results$i_low_aic_ind_comp[k]]
      results$i_low_aic_mean_comp[k] <- c(results$meanexp_i_comp[k],results$meanwei_i_comp[k], results$meanlnorm_i_comp[k], results$meanllog_i_comp[k], results$meangam_i_comp[k], results$meangeng_i_comp[k],results$meangomp_i_comp[k],results$meangenf_i_comp[k])[results$i_low_aic_ind_comp[k]]
      results$i_low_aic_median_comp[k] <- c(results$medexp_i_comp[k],results$medwei_i_comp[k], results$medlnorm_i_comp[k], results$medllog_i_comp[k], results$medgam_i_comp[k], results$medgeng_i_comp[k],results$medgomp_i_comp[k],results$medgenf_i_comp[k])[results$i_low_aic_ind_comp[k]]
    }
    
    if (is.na(results$bicexp_c_comp[k]) & is.na(results$bicwei_c_comp[k]) & is.na(results$biclnorm_c_comp[k]) & is.na(results$bicllog_c_comp[k]) & is.na(results$bicgam_c_comp[k]) & is.na(results$bicgeng_c_comp[k]) & is.na(results$bicgomp_c_comp[k]) & is.na(results$bicgenf_c_comp[k])){
    }else{
      results$c_low_bic_ind_comp[k] <- which.min(c(results$bicexp_c_comp[k],results$bicwei_c_comp[k], results$biclnorm_c_comp[k], results$bicllog_c_comp[k], results$bicgam_c_comp[k], results$bicgeng_c_comp[k],results$bicgomp_c_comp[k],results$bicgenf_c_comp[k]))
      results$c_low_bic_model_comp[k] <- parafits[results$c_low_bic_ind_comp[k]]
      results$c_low_bic_mean_comp[k] <- c(results$meanexp_c_comp[k],results$meanwei_c_comp[k], results$meanlnorm_c_comp[k], results$meanllog_c_comp[k], results$meangam_c_comp[k], results$meangeng_c_comp[k],results$meangomp_c_comp[k],results$meangenf_c_comp[k])[results$c_low_bic_ind_comp[k]]
      results$c_low_bic_median_comp[k] <- c(results$medexp_c_comp[k],results$medwei_c_comp[k], results$medlnorm_c_comp[k], results$medllog_c_comp[k], results$medgam_c_comp[k], results$medgeng_c_comp[k],results$medgomp_c_comp[k],results$medgenf_c_comp[k])[results$c_low_bic_ind_comp[k]]
    }
    
    if (is.na(results$bicexp_i_comp[k]) & is.na(results$bicwei_i_comp[k]) & is.na(results$biclnorm_i_comp[k]) & is.na(results$bicllog_i_comp[k]) & is.na(results$bicgam_i_comp[k]) & is.na(results$bicgeng_i_comp[k]) & is.na(results$bicgomp_i_comp[k]) & is.na(results$bicgenf_i_comp[k])){
    }else{
      results$i_low_bic_ind_comp[k] <- which.min(c(results$bicexp_i_comp[k],results$bicwei_i_comp[k], results$biclnorm_i_comp[k], results$bicllog_i_comp[k], results$bicgam_i_comp[k], results$bicgeng_i_comp[k],results$bicgomp_i_comp[k],results$bicgenf_i_comp[k]))
      results$i_low_bic_model_comp[k] <- parafits[results$i_low_bic_ind_comp[k]]
      results$i_low_bic_mean_comp[k] <- c(results$meanexp_i_comp[k],results$meanwei_i_comp[k], results$meanlnorm_i_comp[k], results$meanllog_i_comp[k], results$meangam_i_comp[k], results$meangeng_i_comp[k],results$meangomp_i_comp[k],results$meangenf_i_comp[k])[results$i_low_bic_ind_comp[k]]
      results$i_low_bic_median_comp[k] <- c(results$medexp_i_comp[k],results$medwei_i_comp[k], results$medlnorm_i_comp[k], results$medllog_i_comp[k], results$medgam_i_comp[k], results$medgeng_i_comp[k],results$medgomp_i_comp[k],results$medgenf_i_comp[k])[results$i_low_bic_ind_comp[k]]
    }
    
    
    results$exp_aic_combi_comp[k] <- results$aicexp_c_comp[k] + results$aicexp_i_comp[k]
    results$wei_aic_combi_comp[k] <- results$aicwei_c_comp[k] + results$aicwei_i_comp[k]
    results$lnorm_aic_combi_comp[k] <- results$aiclnorm_c_comp[k] + results$aiclnorm_i_comp[k]
    results$llog_aic_combi_comp[k] <- results$aicllog_c_comp[k] + results$aicllog_i_comp[k]
    results$gam_aic_combi_comp[k] <- results$aicgam_c_comp[k] + results$aicgam_i_comp[k]
    results$geng_aic_combi_comp[k] <- results$aicgeng_c_comp[k] + results$aicgeng_i_comp[k]
    results$gomp_aic_combi_comp[k] <- results$aicgomp_c_comp[k] + results$aicgomp_i_comp[k]
    results$genf_aic_combi_comp[k] <- results$aicgenf_c_comp[k] + results$aicgenf_i_comp[k]
    results$exp_bic_combi_comp[k] <- results$bicexp_c_comp[k] + results$bicexp_i_comp[k]
    results$wei_bic_combi_comp[k] <- results$bicwei_c_comp[k] + results$bicwei_i_comp[k]
    results$lnorm_bic_combi_comp[k] <- results$biclnorm_c_comp[k] + results$biclnorm_i_comp[k]
    results$llog_bic_combi_comp[k] <- results$bicllog_c_comp[k] + results$bicllog_i_comp[k]
    results$gam_bic_combi_comp[k] <- results$bicgam_c_comp[k] + results$bicgam_i_comp[k]
    results$geng_bic_combi_comp[k] <- results$bicgeng_c_comp[k] + results$bicgeng_i_comp[k]
    results$gomp_bic_combi_comp[k] <- results$bicgomp_c_comp[k] + results$bicgomp_i_comp[k]
    results$genf_bic_combi_comp[k] <- results$bicgenf_c_comp[k] + results$bicgenf_i_comp[k]
    
    
    ## finding lowest AIC and BIC combined
    if (is.na(results$exp_aic_combi_comp[k]) & is.na(results$wei_aic_combi_comp[k]) & is.na(results$lnorm_aic_combi_comp[k]) & is.na(results$llog_aic_combi_comp[k]) & is.na(results$gam_aic_combi_comp[k]) & is.na(results$geng_aic_combi_comp[k]) & is.na(results$gomp_aic_combi_comp[k]) & is.na(results$genf_aic_combi_comp[k])){
    }else{
      results$overall_low_aic_ind_comp[k] <- which.min(c(results$exp_aic_combi_comp[k],results$wei_aic_combi_comp[k], results$lnorm_aic_combi_comp[k], results$llog_aic_combi_comp[k], results$gam_aic_combi_comp[k], results$geng_aic_combi_comp[k],results$gomp_aic_combi_comp[k],results$genf_aic_combi_comp[k]))
      results$overall_low_aic_model_comp[k] <- parafits[results$overall_low_aic_ind_comp[k]]
      
      results$overall_low_aic_mean_c_comp[k] <- c(results$meanexp_c_comp[k],results$meanwei_c_comp[k], results$meanlnorm_c_comp[k], results$meanllog_c_comp[k], results$meangam_c_comp[k], results$meangeng_c_comp[k],results$meangomp_c_comp[k],results$meangenf_c_comp[k])[results$overall_low_aic_ind_comp[k]]
      results$overall_low_aic_median_c_comp[k] <- c(results$medexp_c_comp[k],results$medwei_c_comp[k], results$medlnorm_c_comp[k], results$medllog_c_comp[k], results$medgam_c_comp[k], results$medgeng_c_comp[k],results$medgomp_c_comp[k],results$medgenf_c_comp[k])[results$overall_low_aic_ind_comp[k]]
      results$overall_low_aic_mean_i_comp[k] <- c(results$meanexp_i_comp[k],results$meanwei_i_comp[k], results$meanlnorm_i_comp[k], results$meanllog_i_comp[k], results$meangam_i_comp[k], results$meangeng_i_comp[k],results$meangomp_i_comp[k],results$meangenf_i_comp[k])[results$overall_low_aic_ind_comp[k]]
      results$overall_low_aic_median_i_comp[k] <- c(results$medexp_i_comp[k],results$medwei_i_comp[k], results$medlnorm_i_comp[k], results$medllog_i_comp[k], results$medgam_i_comp[k], results$medgeng_i_comp[k],results$medgomp_i_comp[k],results$medgenf_i_comp[k])[results$overall_low_aic_ind_comp[k]]
    }
    
    if (is.na(results$exp_bic_combi_comp[k]) & is.na(results$wei_bic_combi_comp[k]) & is.na(results$lnorm_bic_combi_comp[k]) & is.na(results$llog_bic_combi_comp[k]) & is.na(results$gam_bic_combi_comp[k]) & is.na(results$geng_bic_combi_comp[k]) & is.na(results$gomp_bic_combi_comp[k]) & is.na(results$genf_bic_combi_comp[k])){
    }else{
      results$overall_low_bic_ind_comp[k] <- which.min(c(results$exp_bic_combi_comp[k],results$wei_bic_combi_comp[k], results$lnorm_bic_combi_comp[k], results$llog_bic_combi_comp[k], results$gam_bic_combi_comp[k], results$geng_bic_combi_comp[k],results$gomp_bic_combi_comp[k],results$genf_bic_combi_comp[k]))
      results$overall_low_bic_model_comp[k] <- parafits[results$overall_low_bic_ind_comp[k]]
      
      results$overall_low_bic_mean_c_comp[k] <- c(results$meanexp_c_comp[k],results$meanwei_c_comp[k], results$meanlnorm_c_comp[k], results$meanllog_c_comp[k], results$meangam_c_comp[k], results$meangeng_c_comp[k],results$meangomp_c_comp[k],results$meangenf_c_comp[k])[results$overall_low_bic_ind_comp[k]]
      results$overall_low_bic_median_c_comp[k] <- c(results$medexp_c_comp[k],results$medwei_c_comp[k], results$medlnorm_c_comp[k], results$medllog_c_comp[k], results$medgam_c_comp[k], results$medgeng_c_comp[k],results$medgomp_c_comp[k],results$medgenf_c_comp[k])[results$overall_low_bic_ind_comp[k]]
      results$overall_low_bic_mean_i_comp[k] <- c(results$meanexp_i_comp[k],results$meanwei_i_comp[k], results$meanlnorm_i_comp[k], results$meanllog_i_comp[k], results$meangam_i_comp[k], results$meangeng_i_comp[k],results$meangomp_i_comp[k],results$meangenf_i_comp[k])[results$overall_low_bic_ind_comp[k]]
      results$overall_low_bic_median_i_comp[k] <- c(results$medexp_i_comp[k],results$medwei_i_comp[k], results$medlnorm_i_comp[k], results$medllog_i_comp[k], results$medgam_i_comp[k], results$medgeng_i_comp[k],results$medgomp_i_comp[k],results$medgenf_i_comp[k])[results$overall_low_bic_ind_comp[k]]
    }
    
    
    
    
    ### average across all plausible models ###
    
    
    
    results$n_plausible_comp[k] <- 8 - sum(is.na(c(results$meanexp_i_comp[k],results$meanwei_i_comp[k],results$meanlnorm_i_comp[k],results$meanllog_i_comp[k],results$meangam_i_comp[k], results$meangeng_i_comp[k],results$meangomp_i_comp[k],results$meangenf_i_comp[k])))
    
    results$average_mean_i_comp[k] <- sum(results$meanexp_i_comp[k],results$meanwei_i_comp[k],results$meanlnorm_i_comp[k],results$meanllog_i_comp[k],results$meangam_i_comp[k], results$meangeng_i_comp[k],results$meangomp_i_comp[k],results$meangenf_i_comp[k], na.rm=T )/ results$n_plausible_comp[k]
    results$average_median_i_comp[k] <- sum(results$medexp_i_comp[k],results$medwei_i_comp[k],results$medlnorm_i_comp[k],results$medllog_i_comp[k],results$medgam_i_comp[k], results$medgeng_i_comp[k],results$medgomp_i_comp[k],results$medgenf_i_comp[k], na.rm=T )/ results$n_plausible_comp[k]
    
    results$average_mean_c_comp[k] <- sum(results$meanexp_c_comp[k],results$meanwei_c_comp[k],results$meanlnorm_c_comp[k],results$meanllog_c_comp[k],results$meangam_c_comp[k], results$meangeng_c_comp[k],results$meangomp_c_comp[k],results$meangenf_c_comp[k], na.rm=T )/ results$n_plausible_comp[k]
    results$average_median_c_comp[k] <- sum(results$medexp_c_comp[k],results$medwei_c_comp[k],results$medlnorm_c_comp[k],results$medllog_c_comp[k],results$medgam_c_comp[k], results$medgeng_c_comp[k],results$medgomp_c_comp[k],results$medgenf_c_comp[k], na.rm=T )/ results$n_plausible_comp[k]
    
    
    
    # model selection and averaging subgroup
    
    if (is.na(results$aicexp_c_sub[k]) & is.na(results$aicwei_c_sub[k]) & is.na(results$aiclnorm_c_sub[k]) & is.na(results$aicllog_c_sub[k]) & is.na(results$aicgam_c_sub[k]) & is.na(results$aicgeng_c_sub[k]) & is.na(results$aicgomp_c_sub[k]) & is.na(results$aicgenf_c_sub[k])){
    }else{
      results$c_low_aic_ind_sub[k] <- which.min(c(results$aicexp_c_sub[k],results$aicwei_c_sub[k], results$aiclnorm_c_sub[k], results$aicllog_c_sub[k], results$aicgam_c_sub[k], results$aicgeng_c_sub[k],results$aicgomp_c_sub[k],results$aicgenf_c_sub[k]))
      results$c_low_aic_model_sub[k] <- parafits[results$c_low_aic_ind_sub[k]]
      results$c_low_aic_mean_sub[k] <- c(results$meanexp_c_sub[k],results$meanwei_c_sub[k], results$meanlnorm_c_sub[k], results$meanllog_c_sub[k], results$meangam_c_sub[k], results$meangeng_c_sub[k],results$meangomp_c_sub[k],results$meangenf_c_sub[k])[results$c_low_aic_ind_sub[k]]
      results$c_low_aic_median_sub[k] <- c(results$medexp_c_sub[k],results$medwei_c_sub[k], results$medlnorm_c_sub[k], results$medllog_c_sub[k], results$medgam_c_sub[k], results$medgeng_c_sub[k],results$medgomp_c_sub[k],results$medgenf_c_sub[k])[results$c_low_aic_ind_sub[k]]
    }
    
    if (is.na(results$aicexp_i_sub[k]) & is.na(results$aicwei_i_sub[k]) & is.na(results$aiclnorm_i_sub[k]) & is.na(results$aicllog_i_sub[k]) & is.na(results$aicgam_i_sub[k]) & is.na(results$aicgeng_i_sub[k]) & is.na(results$aicgomp_i_sub[k]) & is.na(results$aicgenf_i_sub[k])){
    }else{
      results$i_low_aic_ind_sub[k] <- which.min(c(results$aicexp_i_sub[k],results$aicwei_i_sub[k], results$aiclnorm_i_sub[k], results$aicllog_i_sub[k], results$aicgam_i_sub[k], results$aicgeng_i_sub[k],results$aicgomp_i_sub[k],results$aicgenf_i_sub[k]))
      results$i_low_aic_model_sub[k] <- parafits[results$i_low_aic_ind_sub[k]]
      results$i_low_aic_mean_sub[k] <- c(results$meanexp_i_sub[k],results$meanwei_i_sub[k], results$meanlnorm_i_sub[k], results$meanllog_i_sub[k], results$meangam_i_sub[k], results$meangeng_i_sub[k],results$meangomp_i_sub[k],results$meangenf_i_sub[k])[results$i_low_aic_ind_sub[k]]
      results$i_low_aic_median_sub[k] <- c(results$medexp_i_sub[k],results$medwei_i_sub[k], results$medlnorm_i_sub[k], results$medllog_i_sub[k], results$medgam_i_sub[k], results$medgeng_i_sub[k],results$medgomp_i_sub[k],results$medgenf_i_sub[k])[results$i_low_aic_ind_sub[k]]
    }
    
    if (is.na(results$bicexp_c_sub[k]) & is.na(results$bicwei_c_sub[k]) & is.na(results$biclnorm_c_sub[k]) & is.na(results$bicllog_c_sub[k]) & is.na(results$bicgam_c_sub[k]) & is.na(results$bicgeng_c_sub[k]) & is.na(results$bicgomp_c_sub[k]) & is.na(results$bicgenf_c_sub[k])){
    }else{
      results$c_low_bic_ind_sub[k] <- which.min(c(results$bicexp_c_sub[k],results$bicwei_c_sub[k], results$biclnorm_c_sub[k], results$bicllog_c_sub[k], results$bicgam_c_sub[k], results$bicgeng_c_sub[k],results$bicgomp_c_sub[k],results$bicgenf_c_sub[k]))
      results$c_low_bic_model_sub[k] <- parafits[results$c_low_bic_ind_sub[k]]
      results$c_low_bic_mean_sub[k] <- c(results$meanexp_c_sub[k],results$meanwei_c_sub[k], results$meanlnorm_c_sub[k], results$meanllog_c_sub[k], results$meangam_c_sub[k], results$meangeng_c_sub[k],results$meangomp_c_sub[k],results$meangenf_c_sub[k])[results$c_low_bic_ind_sub[k]]
      results$c_low_bic_median_sub[k] <- c(results$medexp_c_sub[k],results$medwei_c_sub[k], results$medlnorm_c_sub[k], results$medllog_c_sub[k], results$medgam_c_sub[k], results$medgeng_c_sub[k],results$medgomp_c_sub[k],results$medgenf_c_sub[k])[results$c_low_bic_ind_sub[k]]
    }
    
    if (is.na(results$bicexp_i_sub[k]) & is.na(results$bicwei_i_sub[k]) & is.na(results$biclnorm_i_sub[k]) & is.na(results$bicllog_i_sub[k]) & is.na(results$bicgam_i_sub[k]) & is.na(results$bicgeng_i_sub[k]) & is.na(results$bicgomp_i_sub[k]) & is.na(results$bicgenf_i_sub[k])){
    }else{
      results$i_low_bic_ind_sub[k] <- which.min(c(results$bicexp_i_sub[k],results$bicwei_i_sub[k], results$biclnorm_i_sub[k], results$bicllog_i_sub[k], results$bicgam_i_sub[k], results$bicgeng_i_sub[k],results$bicgomp_i_sub[k],results$bicgenf_i_sub[k]))
      results$i_low_bic_model_sub[k] <- parafits[results$i_low_bic_ind_sub[k]]
      results$i_low_bic_mean_sub[k] <- c(results$meanexp_i_sub[k],results$meanwei_i_sub[k], results$meanlnorm_i_sub[k], results$meanllog_i_sub[k], results$meangam_i_sub[k], results$meangeng_i_sub[k],results$meangomp_i_sub[k],results$meangenf_i_sub[k])[results$i_low_bic_ind_sub[k]]
      results$i_low_bic_median_sub[k] <- c(results$medexp_i_sub[k],results$medwei_i_sub[k], results$medlnorm_i_sub[k], results$medllog_i_sub[k], results$medgam_i_sub[k], results$medgeng_i_sub[k],results$medgomp_i_sub[k],results$medgenf_i_sub[k])[results$i_low_bic_ind_sub[k]]
    }
    
    
    results$exp_aic_combi_sub[k] <- results$aicexp_c_sub[k] + results$aicexp_i_sub[k]
    results$wei_aic_combi_sub[k] <- results$aicwei_c_sub[k] + results$aicwei_i_sub[k]
    results$lnorm_aic_combi_sub[k] <- results$aiclnorm_c_sub[k] + results$aiclnorm_i_sub[k]
    results$llog_aic_combi_sub[k] <- results$aicllog_c_sub[k] + results$aicllog_i_sub[k]
    results$gam_aic_combi_sub[k] <- results$aicgam_c_sub[k] + results$aicgam_i_sub[k]
    results$geng_aic_combi_sub[k] <- results$aicgeng_c_sub[k] + results$aicgeng_i_sub[k]
    results$gomp_aic_combi_sub[k] <- results$aicgomp_c_sub[k] + results$aicgomp_i_sub[k]
    results$genf_aic_combi_sub[k] <- results$aicgenf_c_sub[k] + results$aicgenf_i_sub[k]
    results$exp_bic_combi_sub[k] <- results$bicexp_c_sub[k] + results$bicexp_i_sub[k]
    results$wei_bic_combi_sub[k] <- results$bicwei_c_sub[k] + results$bicwei_i_sub[k]
    results$lnorm_bic_combi_sub[k] <- results$biclnorm_c_sub[k] + results$biclnorm_i_sub[k]
    results$llog_bic_combi_sub[k] <- results$bicllog_c_sub[k] + results$bicllog_i_sub[k]
    results$gam_bic_combi_sub[k] <- results$bicgam_c_sub[k] + results$bicgam_i_sub[k]
    results$geng_bic_combi_sub[k] <- results$bicgeng_c_sub[k] + results$bicgeng_i_sub[k]
    results$gomp_bic_combi_sub[k] <- results$bicgomp_c_sub[k] + results$bicgomp_i_sub[k]
    results$genf_bic_combi_sub[k] <- results$bicgenf_c_sub[k] + results$bicgenf_i_sub[k]
    
    
    ## finding lowest AIC and BIC combined
    if (is.na(results$exp_aic_combi_sub[k]) & is.na(results$wei_aic_combi_sub[k]) & is.na(results$lnorm_aic_combi_sub[k]) & is.na(results$llog_aic_combi_sub[k]) & is.na(results$gam_aic_combi_sub[k]) & is.na(results$geng_aic_combi_sub[k]) & is.na(results$gomp_aic_combi_sub[k]) & is.na(results$genf_aic_combi_sub[k])){
    }else{
      results$overall_low_aic_ind_sub[k] <- which.min(c(results$exp_aic_combi_sub[k],results$wei_aic_combi_sub[k], results$lnorm_aic_combi_sub[k], results$llog_aic_combi_sub[k], results$gam_aic_combi_sub[k], results$geng_aic_combi_sub[k],results$gomp_aic_combi_sub[k],results$genf_aic_combi_sub[k]))
      results$overall_low_aic_model_sub[k] <- parafits[results$overall_low_aic_ind_sub[k]]
      
      results$overall_low_aic_mean_c_sub[k] <- c(results$meanexp_c_sub[k],results$meanwei_c_sub[k], results$meanlnorm_c_sub[k], results$meanllog_c_sub[k], results$meangam_c_sub[k], results$meangeng_c_sub[k],results$meangomp_c_sub[k],results$meangenf_c_sub[k])[results$overall_low_aic_ind_sub[k]]
      results$overall_low_aic_median_c_sub[k] <- c(results$medexp_c_sub[k],results$medwei_c_sub[k], results$medlnorm_c_sub[k], results$medllog_c_sub[k], results$medgam_c_sub[k], results$medgeng_c_sub[k],results$medgomp_c_sub[k],results$medgenf_c_sub[k])[results$overall_low_aic_ind_sub[k]]
      results$overall_low_aic_mean_i_sub[k] <- c(results$meanexp_i_sub[k],results$meanwei_i_sub[k], results$meanlnorm_i_sub[k], results$meanllog_i_sub[k], results$meangam_i_sub[k], results$meangeng_i_sub[k],results$meangomp_i_sub[k],results$meangenf_i_sub[k])[results$overall_low_aic_ind_sub[k]]
      results$overall_low_aic_median_i_sub[k] <- c(results$medexp_i_sub[k],results$medwei_i_sub[k], results$medlnorm_i_sub[k], results$medllog_i_sub[k], results$medgam_i_sub[k], results$medgeng_i_sub[k],results$medgomp_i_sub[k],results$medgenf_i_sub[k])[results$overall_low_aic_ind_sub[k]]
    }
    
    if (is.na(results$exp_bic_combi_sub[k]) & is.na(results$wei_bic_combi_sub[k]) & is.na(results$lnorm_bic_combi_sub[k]) & is.na(results$llog_bic_combi_sub[k]) & is.na(results$gam_bic_combi_sub[k]) & is.na(results$geng_bic_combi_sub[k]) & is.na(results$gomp_bic_combi_sub[k]) & is.na(results$genf_bic_combi_sub[k])){
    }else{
      results$overall_low_bic_ind_sub[k] <- which.min(c(results$exp_bic_combi_sub[k],results$wei_bic_combi_sub[k], results$lnorm_bic_combi_sub[k], results$llog_bic_combi_sub[k], results$gam_bic_combi_sub[k], results$geng_bic_combi_sub[k],results$gomp_bic_combi_sub[k],results$genf_bic_combi_sub[k]))
      results$overall_low_bic_model_sub[k] <- parafits[results$overall_low_bic_ind_sub[k]]
      
      results$overall_low_bic_mean_c_sub[k] <- c(results$meanexp_c_sub[k],results$meanwei_c_sub[k], results$meanlnorm_c_sub[k], results$meanllog_c_sub[k], results$meangam_c_sub[k], results$meangeng_c_sub[k],results$meangomp_c_sub[k],results$meangenf_c_sub[k])[results$overall_low_bic_ind_sub[k]]
      results$overall_low_bic_median_c_sub[k] <- c(results$medexp_c_sub[k],results$medwei_c_sub[k], results$medlnorm_c_sub[k], results$medllog_c_sub[k], results$medgam_c_sub[k], results$medgeng_c_sub[k],results$medgomp_c_sub[k],results$medgenf_c_sub[k])[results$overall_low_bic_ind_sub[k]]
      results$overall_low_bic_mean_i_sub[k] <- c(results$meanexp_i_sub[k],results$meanwei_i_sub[k], results$meanlnorm_i_sub[k], results$meanllog_i_sub[k], results$meangam_i_sub[k], results$meangeng_i_sub[k],results$meangomp_i_sub[k],results$meangenf_i_sub[k])[results$overall_low_bic_ind_sub[k]]
      results$overall_low_bic_median_i_sub[k] <- c(results$medexp_i_sub[k],results$medwei_i_sub[k], results$medlnorm_i_sub[k], results$medllog_i_sub[k], results$medgam_i_sub[k], results$medgeng_i_sub[k],results$medgomp_i_sub[k],results$medgenf_i_sub[k])[results$overall_low_bic_ind_sub[k]]
    }
    
    
    
    
    ### average across all plausible models ###
    
    
    
    results$n_plausible_sub[k] <- 8 - sum(is.na(c(results$meanexp_i_sub[k],results$meanwei_i_sub[k],results$meanlnorm_i_sub[k],results$meanllog_i_sub[k],results$meangam_i_sub[k], results$meangeng_i_sub[k],results$meangomp_i_sub[k],results$meangenf_i_sub[k])))
    
    results$average_mean_i_sub[k] <- sum(results$meanexp_i_sub[k],results$meanwei_i_sub[k],results$meanlnorm_i_sub[k],results$meanllog_i_sub[k],results$meangam_i_sub[k], results$meangeng_i_sub[k],results$meangomp_i_sub[k],results$meangenf_i_sub[k], na.rm=T )/ results$n_plausible_sub[k]
    results$average_median_i_sub[k] <- sum(results$medexp_i_sub[k],results$medwei_i_sub[k],results$medlnorm_i_sub[k],results$medllog_i_sub[k],results$medgam_i_sub[k], results$medgeng_i_sub[k],results$medgomp_i_sub[k],results$medgenf_i_sub[k], na.rm=T )/ results$n_plausible_sub[k]
    
    results$average_mean_c_sub[k] <- sum(results$meanexp_c_sub[k],results$meanwei_c_sub[k],results$meanlnorm_c_sub[k],results$meanllog_c_sub[k],results$meangam_c_sub[k], results$meangeng_c_sub[k],results$meangomp_c_sub[k],results$meangenf_c_sub[k], na.rm=T )/ results$n_plausible_sub[k]
    results$average_median_c_sub[k] <- sum(results$medexp_c_sub[k],results$medwei_c_sub[k],results$medlnorm_c_sub[k],results$medllog_c_sub[k],results$medgam_c_sub[k], results$medgeng_c_sub[k],results$medgomp_c_sub[k],results$medgenf_c_sub[k], na.rm=T )/ results$n_plausible_sub[k]
    
    
    # pool back for combined single estimate of incremental benefit
    
    
    ### combine back, weighting by proportions
    
    results$i_low_aic_mean[k] <-  subprop[g]*results$i_low_aic_mean_sub[k] + (1-subprop[g])*results$i_low_aic_mean_comp[k]
    results$c_low_aic_mean[k] <-  subprop[g]*results$c_low_aic_mean_sub[k] + (1-subprop[g])*results$c_low_aic_mean_comp[k]
    results$i_low_bic_mean[k] <-  subprop[g]*results$i_low_bic_mean_sub[k] + (1-subprop[g])*results$i_low_bic_mean_comp[k]
    results$c_low_bic_mean[k] <-  subprop[g]*results$c_low_bic_mean_sub[k] + (1-subprop[g])*results$c_low_bic_mean_comp[k]
    results$overall_low_aic_mean_i[k] <-   subprop[g]*results$overall_low_aic_mean_i_sub[k] + (1-subprop[g])*results$overall_low_aic_mean_i_comp[k]
    results$overall_low_aic_mean_c[k] <-   subprop[g]*results$overall_low_aic_mean_c_sub[k] + (1-subprop[g])*results$overall_low_aic_mean_c_comp[k]
    results$overall_low_bic_mean_i[k] <-   subprop[g]*results$overall_low_bic_mean_i_sub[k] + (1-subprop[g])*results$overall_low_bic_mean_i_comp[k]
    results$overall_low_bic_mean_c[k] <-   subprop[g]*results$overall_low_bic_mean_c_sub[k] + (1-subprop[g])*results$overall_low_bic_mean_c_comp[k]
    results$average_mean_i[k] <-   subprop[g]*results$average_mean_i_sub[k] + (1-subprop[g])*results$average_mean_i_comp[k]
    results$average_mean_c[k] <-   subprop[g]*results$average_mean_c_sub[k] + (1-subprop[g])*results$average_mean_c_comp[k]
    
    
    
  }
  
  if (results$cox_interact_p[k] >= 0.05) {
    
    ### fit parametric models
    
    
    est_i_5 <- c(rep(NA,8))
    est_i_10 <- c(rep(NA,8))
    est_c_5 <- c(rep(NA,8))
    est_c_10 <- c(rep(NA,8))
    
    c_5y_ind <- c(rep(NA,8))
    c_10y_ind <- c(rep(NA,8))
    i_5y_ind <- c(rep(NA,8))
    i_10y_ind <- c(rep(NA,8))
    
    i_5y_true <- subprop[g]*exp(-5*haz_int_s) + (1-subprop[g])*exp(-5*haz_int_c)
    i_10y_true <- subprop[g]*exp(-10*haz_int_s) + (1-subprop[g])*exp(-10*haz_int_c)
    c_5y_true <- subprop[g]*exp(-5*haz_cont_s) + (1-subprop[g])*exp(-5*haz_cont_c)
    c_10y_true <- subprop[g]*exp(-10*haz_cont_s) + (1-subprop[g])*exp(-10*haz_cont_c)
    
    i_lower_5y <- max(i_5y_true - 0.075,0)
    i_upper_5y <-  i_5y_true + 0.075
    i_lower_10y <-  max(i_10y_true - 0.05,0)
    i_upper_10y <-  i_10y_true + 0.05
    c_lower_5y <- max(c_5y_true - 0.075,0)
    c_upper_5y <-  c_5y_true + 0.075
    c_lower_10y <-  max(c_10y_true - 0.05,0)
    c_upper_10y <-  c_10y_true + 0.05
    
    for (i in 1:8) {
      
      tryCatch(expr = {
        m1 <- flexsurvreg(Surv(stime2,ev1cens0) ~ 1, data=exp_surv_i, dist = parafits[i], sr.control = survreg.control(maxit = 1000))
        m2 <- flexsurvreg(Surv(stime2,ev1cens0) ~ 1, data=exp_surv_c, dist = parafits[i], sr.control = survreg.control(maxit = 1000))
        
        m1_sum <- NA 
        est_i_5 <- NA   
        est_i_10 <- NA 
        m2_sum <- NA
        est_c_5 <-NA   
        est_c_10 <- NA
        
        m1_sum <- summary(m1, t=c(5,10))
        est_i_5 <- m1_sum[[1]]$est[1]   
        est_i_10 <- m1_sum[[1]]$est[2] 
        m2_sum <- summary(m2, t=c(5,10))
        est_c_5 <- m2_sum[[1]]$est[1]   
        est_c_10 <- m2_sum[[1]]$est[2] 
        
        
        
        if(est_i_5<i_upper_5y & est_i_5>i_lower_5y  & est_i_10<i_upper_10y & est_i_10>i_lower_10y & est_c_5<c_upper_5y & est_c_5>c_lower_5y & est_c_10<c_upper_10y & est_c_10>c_lower_10y ) {
          
          results[k,4*i+1] <- m1$AIC
          results[k,4*i+2] <- log(m1$N)*m1$npars - 2*m1$loglik  
          pars[i,] <- m1$res[,1]
          if (i != 8) {
            med1 <- as.data.frame(summary(m1, type = "median"))[1]
          }
          results[k,4*i+4] <- med1
          
          if (m1$npars == 4){
            P1 <- pars[i,1]
            P2 <- pars[i,2]
            P3 <- pars[i,3]
            P4 <- pars[i,4]
            try(results[k,4*i+3] <- get(paste(meanfits[i]))(timehorizon,P1,P2,P3,P4))
          }
          if (m1$npars == 3){
            P1 <- pars[i,1]
            P2 <- pars[i,2]
            P3 <- pars[i,3]
            try(results[k,4*i+3] <- get(paste(meanfits[i]))(timehorizon,P1,P2,P3))
          }
          if (m1$npars == 2){
            P1 <- pars[i,1]
            P2 <- pars[i,2]
            try(results[k,4*i+3] <- get(paste(meanfits[i]))(timehorizon,P1,P2))
          }
          if (m1$npars == 1){
            P1 <- pars[i,1]
            try(results[k,4*i+3] <- get(paste(meanfits[i]))(timehorizon,P1))
          }
          
          ## m2 ##
          
          results[k,33+4*i] <- m2$AIC
          results[k,33+4*i+1] <- log(m2$N)*m2$npars - 2*m2$loglik  
          pars2[i,] <- m2$res[,1]
          if (i != 8) {
            med1 <- as.data.frame(summary(m2, type = "median"))[1]
          }
          results[k,33+4*i+3] <- med1
          
          if (m2$npars == 4){
            P1 <- pars[i,1]
            P2 <- pars[i,2]
            P3 <- pars[i,3]
            P4 <- pars[i,4]
            try(results[k,33+4*i+2] <- get(paste(meanfits[i]))(timehorizon,P1,P2,P3,P4))
          }
          if (m2$npars == 3){
            P1 <- pars2[i,1]
            P2 <- pars2[i,2]
            P3 <- pars2[i,3]
            try(results[k,33+4*i+2] <- get(paste(meanfits[i]))(timehorizon,P1,P2,P3))
          }
          if (m2$npars == 2){
            P1 <- pars2[i,1]
            P2 <- pars2[i,2]
            try(results[k,33+4*i+2] <- get(paste(meanfits[i]))(timehorizon,P1,P2))
          }
          if (m2$npars == 1){
            P1 <- pars2[i,1]
            try(results[k,33+4*i+2] <- get(paste(meanfits[i]))(timehorizon,P1))
          }    
          
          rm(m1,m2)  }
        
      }, 
      error=function(e){ 
        message("Error"," ",k,"  ", 1, "  ", i)
        message(e)
        return(NA)
      }, 
      warning=function(w){
        message("Warning"," ", k,"  ", 1, "  ", i)
        message(w)
      },
      finally = {
      } )
      
    }
    
    #### remove implausible models - 5 or 10 year prediction (presuming not subgroup specific) (already done)
    
    ### select most plausible according to AIC and to BIC (2 models)
    
    if (is.na(results$aicexp_c[k]) & is.na(results$aicwei_c[k]) & is.na(results$aiclnorm_c[k]) & is.na(results$aicllog_c[k]) & is.na(results$aicgam_c[k]) & is.na(results$aicgeng_c[k]) & is.na(results$aicgomp_c[k]) & is.na(results$aicgenf_c[k])){
    }else{
      results$c_low_aic_ind[k] <- which.min(c(results$aicexp_c[k],results$aicwei_c[k], results$aiclnorm_c[k], results$aicllog_c[k], results$aicgam_c[k], results$aicgeng_c[k],results$aicgomp_c[k],results$aicgenf_c[k]))
      results$c_low_aic_model[k] <- parafits[results$c_low_aic_ind[k]]
      results$c_low_aic_mean[k] <- c(results$meanexp_c[k],results$meanwei_c[k], results$meanlnorm_c[k], results$meanllog_c[k], results$meangam_c[k], results$meangeng_c[k],results$meangomp_c[k],results$meangenf_c[k])[results$c_low_aic_ind[k]]
      results$c_low_aic_median[k] <- c(results$medexp_c[k],results$medwei_c[k], results$medlnorm_c[k], results$medllog_c[k], results$medgam_c[k], results$medgeng_c[k],results$medgomp_c[k],results$medgenf_c[k])[results$c_low_aic_ind[k]]
    }
    
    if (is.na(results$aicexp_i[k]) & is.na(results$aicwei_i[k]) & is.na(results$aiclnorm_i[k]) & is.na(results$aicllog_i[k]) & is.na(results$aicgam_i[k]) & is.na(results$aicgeng_i[k]) & is.na(results$aicgomp_i[k]) & is.na(results$aicgenf_i[k])){
    }else{
      results$i_low_aic_ind[k] <- which.min(c(results$aicexp_i[k],results$aicwei_i[k], results$aiclnorm_i[k], results$aicllog_i[k], results$aicgam_i[k], results$aicgeng_i[k],results$aicgomp_i[k],results$aicgenf_i[k]))
      results$i_low_aic_model[k] <- parafits[results$i_low_aic_ind[k]]
      results$i_low_aic_mean[k] <- c(results$meanexp_i[k],results$meanwei_i[k], results$meanlnorm_i[k], results$meanllog_i[k], results$meangam_i[k], results$meangeng_i[k],results$meangomp_i[k],results$meangenf_i[k])[results$i_low_aic_ind[k]]
      results$i_low_aic_median[k] <- c(results$medexp_i[k],results$medwei_i[k], results$medlnorm_i[k], results$medllog_i[k], results$medgam_i[k], results$medgeng_i[k],results$medgomp_i[k],results$medgenf_i[k])[results$i_low_aic_ind[k]]
    }
    
    if (is.na(results$bicexp_c[k]) & is.na(results$bicwei_c[k]) & is.na(results$biclnorm_c[k]) & is.na(results$bicllog_c[k]) & is.na(results$bicgam_c[k]) & is.na(results$bicgeng_c[k]) & is.na(results$bicgomp_c[k]) & is.na(results$bicgenf_c[k])){
    }else{
      results$c_low_bic_ind[k] <- which.min(c(results$bicexp_c[k],results$bicwei_c[k], results$biclnorm_c[k], results$bicllog_c[k], results$bicgam_c[k], results$bicgeng_c[k],results$bicgomp_c[k],results$bicgenf_c[k]))
      results$c_low_bic_model[k] <- parafits[results$c_low_bic_ind[k]]
      results$c_low_bic_mean[k] <- c(results$meanexp_c[k],results$meanwei_c[k], results$meanlnorm_c[k], results$meanllog_c[k], results$meangam_c[k], results$meangeng_c[k],results$meangomp_c[k],results$meangenf_c[k])[results$c_low_bic_ind[k]]
      results$c_low_bic_median[k] <- c(results$medexp_c[k],results$medwei_c[k], results$medlnorm_c[k], results$medllog_c[k], results$medgam_c[k], results$medgeng_c[k],results$medgomp_c[k],results$medgenf_c[k])[results$c_low_bic_ind[k]]
    }
    
    if (is.na(results$bicexp_i[k]) & is.na(results$bicwei_i[k]) & is.na(results$biclnorm_i[k]) & is.na(results$bicllog_i[k]) & is.na(results$bicgam_i[k]) & is.na(results$bicgeng_i[k]) & is.na(results$bicgomp_i[k]) & is.na(results$bicgenf_i[k])){
    }else{
      results$i_low_bic_ind[k] <- which.min(c(results$bicexp_i[k],results$bicwei_i[k], results$biclnorm_i[k], results$bicllog_i[k], results$bicgam_i[k], results$bicgeng_i[k],results$bicgomp_i[k],results$bicgenf_i[k]))
      results$i_low_bic_model[k] <- parafits[results$i_low_bic_ind[k]]
      results$i_low_bic_mean[k] <- c(results$meanexp_i[k],results$meanwei_i[k], results$meanlnorm_i[k], results$meanllog_i[k], results$meangam_i[k], results$meangeng_i[k],results$meangomp_i[k],results$meangenf_i[k])[results$i_low_bic_ind[k]]
      results$i_low_bic_median[k] <- c(results$medexp_i[k],results$medwei_i[k], results$medlnorm_i[k], results$medllog_i[k], results$medgam_i[k], results$medgeng_i[k],results$medgomp_i[k],results$medgenf_i[k])[results$i_low_bic_ind[k]]
    }
    
    
    results$exp_aic_combi[k] <- results$aicexp_c[k] + results$aicexp_i[k]
    results$wei_aic_combi[k] <- results$aicwei_c[k] + results$aicwei_i[k]
    results$lnorm_aic_combi[k] <- results$aiclnorm_c[k] + results$aiclnorm_i[k]
    results$llog_aic_combi[k] <- results$aicllog_c[k] + results$aicllog_i[k]
    results$gam_aic_combi[k] <- results$aicgam_c[k] + results$aicgam_i[k]
    results$geng_aic_combi[k] <- results$aicgeng_c[k] + results$aicgeng_i[k]
    results$gomp_aic_combi[k] <- results$aicgomp_c[k] + results$aicgomp_i[k]
    results$genf_aic_combi[k] <- results$aicgenf_c[k] + results$aicgenf_i[k]
    results$exp_bic_combi[k] <- results$bicexp_c[k] + results$bicexp_i[k]
    results$wei_bic_combi[k] <- results$bicwei_c[k] + results$bicwei_i[k]
    results$lnorm_bic_combi[k] <- results$biclnorm_c[k] + results$biclnorm_i[k]
    results$llog_bic_combi[k] <- results$bicllog_c[k] + results$bicllog_i[k]
    results$gam_bic_combi[k] <- results$bicgam_c[k] + results$bicgam_i[k]
    results$geng_bic_combi[k] <- results$bicgeng_c[k] + results$bicgeng_i[k]
    results$gomp_bic_combi[k] <- results$bicgomp_c[k] + results$bicgomp_i[k]
    results$genf_bic_combi[k] <- results$bicgenf_c[k] + results$bicgenf_i[k]
    
    
    ## finding lowest AIC and BIC combined
    if (is.na(results$exp_aic_combi[k]) & is.na(results$wei_aic_combi[k]) & is.na(results$lnorm_aic_combi[k]) & is.na(results$llog_aic_combi[k]) & is.na(results$gam_aic_combi[k]) & is.na(results$geng_aic_combi[k]) & is.na(results$gomp_aic_combi[k]) & is.na(results$genf_aic_combi[k])){
    }else{
      results$overall_low_aic_ind[k] <- which.min(c(results$exp_aic_combi[k],results$wei_aic_combi[k], results$lnorm_aic_combi[k], results$llog_aic_combi[k], results$gam_aic_combi[k], results$geng_aic_combi[k],results$gomp_aic_combi[k],results$genf_aic_combi[k]))
      results$overall_low_aic_model[k] <- parafits[results$overall_low_aic_ind[k]]
      
      results$overall_low_aic_mean_c[k] <- c(results$meanexp_c[k],results$meanwei_c[k], results$meanlnorm_c[k], results$meanllog_c[k], results$meangam_c[k], results$meangeng_c[k],results$meangomp_c[k],results$meangenf_c[k])[results$overall_low_aic_ind[k]]
      results$overall_low_aic_median_c[k] <- c(results$medexp_c[k],results$medwei_c[k], results$medlnorm_c[k], results$medllog_c[k], results$medgam_c[k], results$medgeng_c[k],results$medgomp_c[k],results$medgenf_c[k])[results$overall_low_aic_ind[k]]
      results$overall_low_aic_mean_i[k] <- c(results$meanexp_i[k],results$meanwei_i[k], results$meanlnorm_i[k], results$meanllog_i[k], results$meangam_i[k], results$meangeng_i[k],results$meangomp_i[k],results$meangenf_i[k])[results$overall_low_aic_ind[k]]
      results$overall_low_aic_median_i[k] <- c(results$medexp_i[k],results$medwei_i[k], results$medlnorm_i[k], results$medllog_i[k], results$medgam_i[k], results$medgeng_i[k],results$medgomp_i[k],results$medgenf_i[k])[results$overall_low_aic_ind[k]]
    }
    
    if (is.na(results$exp_bic_combi[k]) & is.na(results$wei_bic_combi[k]) & is.na(results$lnorm_bic_combi[k]) & is.na(results$llog_bic_combi[k]) & is.na(results$gam_bic_combi[k]) & is.na(results$geng_bic_combi[k]) & is.na(results$gomp_bic_combi[k]) & is.na(results$genf_bic_combi[k])){
    }else{
      results$overall_low_bic_ind[k] <- which.min(c(results$exp_bic_combi[k],results$wei_bic_combi[k], results$lnorm_bic_combi[k], results$llog_bic_combi[k], results$gam_bic_combi[k], results$geng_bic_combi[k],results$gomp_bic_combi[k],results$genf_bic_combi[k]))
      results$overall_low_bic_model[k] <- parafits[results$overall_low_bic_ind[k]]
      
      results$overall_low_bic_mean_c[k] <- c(results$meanexp_c[k],results$meanwei_c[k], results$meanlnorm_c[k], results$meanllog_c[k], results$meangam_c[k], results$meangeng_c[k],results$meangomp_c[k],results$meangenf_c[k])[results$overall_low_bic_ind[k]]
      results$overall_low_bic_median_c[k] <- c(results$medexp_c[k],results$medwei_c[k], results$medlnorm_c[k], results$medllog_c[k], results$medgam_c[k], results$medgeng_c[k],results$medgomp_c[k],results$medgenf_c[k])[results$overall_low_bic_ind[k]]
      results$overall_low_bic_mean_i[k] <- c(results$meanexp_i[k],results$meanwei_i[k], results$meanlnorm_i[k], results$meanllog_i[k], results$meangam_i[k], results$meangeng_i[k],results$meangomp_i[k],results$meangenf_i[k])[results$overall_low_bic_ind[k]]
      results$overall_low_bic_median_i[k] <- c(results$medexp_i[k],results$medwei_i[k], results$medlnorm_i[k], results$medllog_i[k], results$medgam_i[k], results$medgeng_i[k],results$medgomp_i[k],results$medgenf_i[k])[results$overall_low_bic_ind[k]]
    }
    
    
    
    
    ### average across all plausible models ###
    
    
    
    results$n_plausible[k] <- 8 - sum(is.na(c(results$meanexp_i[k],results$meanwei_i[k],results$meanlnorm_i[k],results$meanllog_i[k],results$meangam_i[k], results$meangeng_i[k],results$meangomp_i[k],results$meangenf_i[k])))
    
    results$average_mean_i[k] <- sum(results$meanexp_i[k],results$meanwei_i[k],results$meanlnorm_i[k],results$meanllog_i[k],results$meangam_i[k], results$meangeng_i[k],results$meangomp_i[k],results$meangenf_i[k], na.rm=T )/ results$n_plausible[k]
    results$average_median_i[k] <- sum(results$medexp_i[k],results$medwei_i[k],results$medlnorm_i[k],results$medllog_i[k],results$medgam_i[k], results$medgeng_i[k],results$medgomp_i[k],results$medgenf_i[k], na.rm=T )/ results$n_plausible[k]
    
    results$average_mean_c[k] <- sum(results$meanexp_c[k],results$meanwei_c[k],results$meanlnorm_c[k],results$meanllog_c[k],results$meangam_c[k], results$meangeng_c[k],results$meangomp_c[k],results$meangenf_c[k], na.rm=T )/ results$n_plausible[k]
    results$average_median_c[k] <- sum(results$medexp_c[k],results$medwei_c[k],results$medlnorm_c[k],results$medllog_c[k],results$medgam_c[k], results$medgeng_c[k],results$medgomp_c[k],results$medgenf_c[k], na.rm=T )/ results$n_plausible[k]
    
    
  }  
  
  
  
}


results$inc_aic_ind <- results$i_low_aic_mean - results$c_low_aic_mean
results$inc_bic_ind <- results$i_low_bic_mean - results$c_low_bic_mean
results$inc_aic_combi <- results$overall_low_aic_mean_i - results$overall_low_aic_mean_c
results$inc_bic_combi <- results$overall_low_bic_mean_i - results$overall_low_bic_mean_c
results$inc_average_all <- results$average_mean_i - results$average_mean_c
results$inc_km <- results$km_mean_est_i - results$km_mean_est_c
results$inc_km_broken <- subprop[g]*(results$km_mean_est_i_sub - results$km_mean_est_c_sub) + (1-subprop[g])*(results$km_mean_est_i_comp - results$km_mean_est_c_comp)


write.csv(results, file=paste0("interaction modelling Scenario 2 ", subprop[g], " Results.csv"))

#### End ####







