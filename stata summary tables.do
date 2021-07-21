

// Pick your data
import delimited "\\GOBO\USER40\u\u1474467\Documents\PHD\Bias Paper\r out\new output\Scenario 1 0.5 Results_v2.csv" ,clear 
import delimited "\\GOBO\USER40\u\u1474467\Documents\PHD\Bias Paper\r out\new output\Scenario 2 0.5 Results_v2.csv" ,clear 
import delimited "\\GOBO\USER40\u\u1474467\Documents\PHD\Bias Paper\r out\new output\Scenario 3 0.5 Results_v2.csv" ,clear 
import delimited "\\GOBO\USER40\u\u1474467\Documents\PHD\Bias Paper\r out\new output\Scenario 4 0.5 Results_v2.csv" ,clear 
import delimited "\\GOBO\USER40\u\u1474467\Documents\PHD\Bias Paper\r out\new output\Scenario 5 0.5 Results_v2.csv" ,clear 
import delimited "\\GOBO\USER40\u\u1474467\Documents\PHD\Bias Paper\r out\new output\Scenario 6 0.5 Results_v2.csv" ,clear 
import delimited "\\GOBO\USER40\u\u1474467\Documents\PHD\Bias Paper\r out\Scenario 0 0.5 Results.csv" ,clear 

import delimited "\\GOBO\USER40\u\u1474467\Documents\PHD\Bias Paper\r out\Scenario 2 large sample size 0.5 Results.csv" ,clear  //   new scenarios.
import delimited "\\GOBO\USER40\u\u1474467\Documents\PHD\Bias Paper\r out\interaction modelling Scenario 2 0.5 Results.csv" ,clear  // new scenarios.
import delimited "\\GOBO\USER40\u\u1474467\Documents\PHD\Bias Paper\r out\scen 2 interaction large sample size 0.5 Results.csv" ,clear  // new scenarios.
 

destring aic*, force replace
destring bic*, force replace
destring mean*, force replace
destring med*, force replace
destring c_*, force replace
destring i_*, force replace 
destring over* , force replace
destring *combi , force replace
destring average* , force replace
destring inc*, force replace



global drawline = truemean_diff[1]
global plus10 = $drawline + 0.1*$drawline
global minus10 = $drawline - 0.1*$drawline

 
*how many runs had no plausible curves according to the threshold cutoff? (count missing values)
*has accuracy improved? maybe we can use mean this time, if the distributions are not so skewed?
*are noticeably more values within 10% of true value? probably not to be honest.
 
 
 ** Missing Values
 missings report inc_aic_ind inc_bic_ind inc_aic_combi inc_bic_combi inc_average_all cox_interact_p cox_hr cox_p


 ** Mean, median and 5% and 90% centiles
* trial follow up

  foreach var in inc_aic_ind inc_bic_ind inc_aic_combi inc_bic_combi inc_average_all inc_km  cox_hr cox_p cox_interact_p {
  
  qui summarize `var', detail
  local mean1 = r(mean)
  local meandiff = `mean1' - $drawline
  local meandiff2 = 100*(`mean1' - $drawline) / $drawline
  local quant5 = r(p5)
  local quant95 = r(p95)
  local variance1 = r(Var)
  local empSE = sqrt(`variance1')
  local median1 = r(p50)
  local mediandiff = `median1' - $drawline
  local mediandiff2 = 100*(`median1' - $drawline) / $drawline
  local n1 = r(N)
  local sd1 = r(sd)
  gen diff`var' =  ($drawline - `var')^2
  
   gen `var'id2 = 1 if `var' < $plus10 & `var'  > $minus10
 qui  summarize  `var'id2
 local within10 = r(N)
 local within10p = 100 * `within10'/ `n1'  

  qui summarize diff`var' 
  local sum1 = r(sum)
  local mse = `sum1' / `n1' 
  
  // calculate MCSE
local mcse = `sd1' / sqrt(`n1')
  
  
  // report all findings in one go to be copy pasted into a table...
  di " "
  di %-6.2f "`var'"
  di %-6.2f `mean1' 
  di "[+"  %-6.2f `meandiff' ", +" %-3.0f `meandiff2' "%]"   
  di %-3.0f `within10p' "%"  
  di " "
  di %-6.2f "`var'"
  di %-6.2f `mean1' " [+"  %-6.2f `meandiff' ", +" %-3.0f `meandiff2' "%]"  
  di "("  %-6.2f `quant5' ", "  %-6.2f `quant95' ")" 
  di %-6.2f `median1' %-6.2f " [+"  %-6.2f `mediandiff' ", +" %-3.0f `mediandiff2' "%]"
  di %-6.2f `mse'
  di %-6.2f `mcse'
  di %-6.2f `empSE' 
  di %-3.0f `within10p' "%"
 }

 
 
gen sig_p = 1 if cox_p < 0.05
replace sig_p = 0 if cox_p >= 0.05
gen sig_int_p = 1 if cox_interact_p < 0.05
replace sig_int_p = 0 if cox_interact_p >= 0.05 

 tab sig_p
  tab sig_int_p
 

 ** Data for Violin Plots **
 
 keep  inc_aic_ind inc_bic_ind inc_aic_combi inc_bic_combi inc_average_all id
 
 reshape long inc_, i(id) j(method) string

 replace method = "AIC - Independent" if method=="aic_ind"
 replace method = "BIC - Independent" if method=="bic_ind"
 replace method = "AIC - Combined" if method=="aic_combi"
 replace method = "BIC - Combined" if method=="bic_combi"
 replace method = "Average of all" if method=="average_all"
 drop id
 
 ** paste into excel **