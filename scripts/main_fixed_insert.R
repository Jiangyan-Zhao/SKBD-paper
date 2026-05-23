rm(list = ls())  # clear workspace variables

library(SKBD)
source("scripts/Scenario_Insert.R")
source("scripts/ADM.R")

target_prob = rep(0.3, 6)
cohort_size = 3
n_cohort = 10
n_scenario = nrow(Scenario)


OC_SKBD = OC_ADM = vector("list", n_scenario)
PCS_SKBD = PCS_ADM = matrix(NA, n_scenario, ncol(Scenario))
PCA_SKBD = PCA_ADM = matrix(NA, n_scenario, ncol(Scenario))
Ins_PCS_SKBD = Ins_PCS_ADM = rep(NA, n_scenario)
Ins_PCA_SKBD = Ins_PCA_ADM = rep(NA, n_scenario)
Ins_dose_mean_SKBD = Ins_dose_mean_ADM = rep(NA, n_scenario)
Ins_dose_sd_SKBD = Ins_dose_sd_ADM = rep(NA, n_scenario)
Ins_pct_SKBD = Ins_pct_ADM = rep(NA, n_scenario)

#------------------------------- Ins-SKBD ----------------------------------------------

for(s in 1:n_scenario){
  cat("s =", s, ", Ins-SKBD: \n")
  OC_SKBD[[s]] = get_OC_Insert_SKBD(target_prob = target_prob[s], tox_prob = Scenario[s, ],
                                    n_cohort = n_cohort, cohort_size = cohort_size,
                                    dose_set = Dose[s, ], n_trial = 1e4)
  
  PCS_SKBD[s, ] = OC_SKBD[[s]]$sel_pct_prespec
  PCA_SKBD[s, ]  = OC_SKBD[[s]]$pts_pct_prespec
  
  OC_SKBD_Ins = OC_SKBD[[s]]$insertion
  Ins_PCS_SKBD[s] = OC_SKBD_Ins$sel_pct
  Ins_PCA_SKBD[s] = OC_SKBD_Ins$pts_pct
  Ins_dose_mean_SKBD[s] = OC_SKBD_Ins$dose_mean
  Ins_dose_sd_SKBD[s] = OC_SKBD_Ins$dose_sd
  Ins_pct_SKBD[s] = OC_SKBD_Ins$trial_pct
}

save(PCS_SKBD, PCA_SKBD, 
     Ins_PCS_SKBD, Ins_PCA_SKBD, Ins_dose_mean_SKBD, Ins_dose_sd_SKBD, Ins_pct_SKBD,
     file = "results/Ins_SKBD_fixed.RData")

cat("OC_Insert_SKBD summary: \n\n")
for (s in 1:6) {
  cat("s =", s, ", Ins-SKBD: \n")
  cat("% Sel", sprintf( "%.2f", 
                c(PCS_SKBD[s, ], Ins_dose_mean_SKBD[s], 
                  Ins_PCS_SKBD[s], Ins_pct_SKBD[[s]]) ) , 
       sep = " & " ) 
  cat("\n")
  cat("% pts", sprintf( "%.2f", 
                c(PCA_SKBD[s, ], Ins_dose_sd_SKBD[s], 
                  Ins_PCA_SKBD[s]) ) , 
       sep = " & " ) 
  cat("\n\n")
}



#------------------------------- ADM -----------------------------------------------

for(s in 1:n_scenario){
  cat("s =", s, ", Ins-ADM: \n")
  OC_ADM[[s]] = ADM.oc(target = target_prob[s], 
                       dose = Dose[s, ], p.true = Scenario[s, ], 
                       ncohort = n_cohort, cohortsize = cohort_size, 
                       ntrial=1e4)
  
  PCS_ADM[s, ] = OC_ADM[[s]]$sel_pct_prespec
  PCA_ADM[s, ]  = OC_ADM[[s]]$pts_pct_prespec
  
  OC_ADM_Ins = OC_ADM[[s]]$insertion
  Ins_PCS_ADM[s] = OC_ADM_Ins$sel_pct
  Ins_PCA_ADM[s] = OC_ADM_Ins$pts_pct
  Ins_dose_mean_ADM[s] = OC_ADM_Ins$dose_mean
  Ins_dose_sd_ADM[s] = OC_ADM_Ins$dose_sd
  Ins_pct_ADM[s] = OC_ADM_Ins$trial_pct
}

save(PCS_ADM, PCA_ADM, 
     Ins_PCS_ADM, Ins_PCA_ADM, Ins_dose_mean_ADM, Ins_dose_sd_ADM, Ins_pct_ADM,
     file = "results/Ins_ADM_fixed.RData")

cat("OC_Insert_ADM summary: \n\n")
for (s in 1:6) {
  cat("s =", s, ", Ins-ADM: \n")
  cat("% Sel", sprintf( "%.2f", 
                        c(PCS_ADM[s, ], Ins_dose_mean_ADM[s], 
                          Ins_PCS_ADM[s], Ins_pct_ADM[[s]]) ) , 
      sep = " & " ) 
  cat("\n")
  cat("% pts", sprintf( "%.2f", 
                        c(PCA_ADM[s, ], Ins_dose_sd_ADM[s], 
                          Ins_PCA_ADM[s]) ) , 
      sep = " & " ) 
  cat("\n\n")
}
