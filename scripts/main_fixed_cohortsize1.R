rm(list = ls())  # clear workspace variables

# library(Keyboard)
# library(BOIN)

library(SKBD)
source("scripts/Scenario.R")

target_prob = c(rep(0.2, 10), rep(0.3, 10))
cohort_size = 1
n_cohort = 30

OC_SKBD = OC_KBD = OC_BOIN = vector("list", nrow(Scenario))
PCS_SKBD = PCS_KBD = PCS_BOIN = rep(NA, 20)
PCA_SKBD = PCA_KBD = PCA_BOIN = rep(NA, 20)
above_MTD_SKBD = above_MTD_KBD = above_MTD_BOIN = rep(NA, 20)
ROD60_SKBD = ROD60_KBD = ROD60_BOIN = rep(NA, 20)
ROD80_SKBD = ROD80_KBD = ROD80_BOIN = rep(NA, 20)
n_DLT_SKBD = n_DLT_KBD = n_DLT_BOIN = rep(NA, 20)
monotonic_SKBD = monotonic_KBD = monotonic_BOIN = rep(NA, 20)

#------------------------------- SKBD --------------------------------------------------
for (s in 1:nrow(Scenario)) {
  cat("\n=== Now running scenario", s, "===\n")
  OC_SKBD[[s]] = get_OC_SKBD(target_prob = target_prob[s], tox_prob = Scenario[s, ],
                             n_cohort = n_cohort, cohort_size = cohort_size,
                             # symmetric = FALSE, k_left = 0.3, k_right = 0.9,
                             n_trial = 1e4)
  PCS_SKBD[s] = OC_SKBD[[s]]$PCS
  
  PCA_SKBD[s] = OC_SKBD[[s]]$PCA
  
  above_MTD_SKBD[s] = OC_SKBD[[s]]$above_MTD
  
  ROD60_SKBD[s] = OC_SKBD[[s]]$ROD60
  
  ROD80_SKBD[s] = OC_SKBD[[s]]$ROD80
  
  n_DLT_SKBD[s] = OC_SKBD[[s]]$n_DLT
  
  monotonic_SKBD[s] = OC_SKBD[[s]]$monotonic_percent
}

# saveRDS(df_SKBD_fixed, paste0("df_SKBD_fixed", format(Sys.Date(), "%Y%m%d"), ".rds"))
save(PCS_SKBD, PCA_SKBD, above_MTD_SKBD, ROD60_SKBD, ROD80_SKBD, n_DLT_SKBD, monotonic_SKBD,
     file = "results/SKBD_fixed_cohortsize1.RData")

cat("OC_SKBD summary: \n\n")
for(i in 1:2){
  id = ((i-1)*10+1):(i*10)
  cat("group =", i, "| taget_prob =", target_prob[id[1]], "\n")
  cat("PCS", sprintf( "%.1f", c(PCS_SKBD[id], mean(PCS_SKBD[id]) ) ), sep = " & ")
  cat("\n")
  cat("PCA", sprintf( "%.1f", c(PCA_SKBD[id], mean(PCA_SKBD[id]) ) ), sep = " & ")
  cat("\n")
  cat("Above-MTD", sprintf( "%.1f", c(above_MTD_SKBD[id], mean(above_MTD_SKBD[id]) ) ), sep = " & ")
  cat("\n")
  cat("ROD 60%", sprintf( "%.1f", c(ROD60_SKBD[id], mean(ROD60_SKBD[id]) ) ), sep = " & ")
  cat("\n")
  cat("ROD 80%",  sprintf( "%.1f", c(ROD80_SKBD[id], mean(ROD80_SKBD[id]) ) ), sep = " & ")
  cat("\n")
  cat("#DLT", sprintf( "%.1f", c(n_DLT_SKBD[id], mean(n_DLT_SKBD[id]) ) ), sep = " & ")
  cat("\n")
  cat("PM", sprintf( "%.1f", c(monotonic_SKBD[id], mean(monotonic_SKBD[id]) ) ), sep = " & ")
  cat("\n\n")
}


#------------------------------- KBD --------------------------------------------------
for (s in 1:nrow(Scenario)) {
  cat("\n=== Now running scenario", s, "===\n")
  OC_KBD[[s]] = get_OC_SKBD(target_prob = target_prob[s], tox_prob = Scenario[s, ],
                            n_cohort = n_cohort, cohort_size = cohort_size,
                            shared = FALSE,
                            n_trial = 1e4)
  PCS_KBD[s] = OC_KBD[[s]]$PCS
  
  PCA_KBD[s] = OC_KBD[[s]]$PCA
  
  above_MTD_KBD[s] = OC_KBD[[s]]$above_MTD
  
  ROD60_KBD[s] = OC_KBD[[s]]$ROD60
  
  ROD80_KBD[s] = OC_KBD[[s]]$ROD80
  
  n_DLT_KBD[s] = OC_KBD[[s]]$n_DLT
  
  monotonic_KBD[s] = OC_KBD[[s]]$monotonic_percent
}


save(PCS_KBD, PCA_KBD, above_MTD_KBD, ROD60_KBD, ROD80_KBD, n_DLT_KBD, monotonic_KBD,
     file = "results/KBD_fixed_cohortsize1.RData")

cat("OC_KBD summary: \n\n")
for(i in 1:2){
  id = ((i-1)*10+1):(i*10)
  cat("group =", i, "| taget_prob =", target_prob[id[1]], "\n")
  cat("PCS", sprintf( "%.1f", c(PCS_KBD[id], mean(PCS_KBD[id]) ) ), sep = " & ")
  cat("\n")
  cat("PCA", sprintf( "%.1f", c(PCA_KBD[id], mean(PCA_KBD[id]) ) ), sep = " & ")
  cat("\n")
  cat("Above-MTD", sprintf( "%.1f", c(above_MTD_KBD[id], mean(above_MTD_KBD[id]) ) ), sep = " & ")
  cat("\n")
  cat("ROD 60%", sprintf( "%.1f", c(ROD60_KBD[id], mean(ROD60_KBD[id]) ) ), sep = " & ")
  cat("\n")
  cat("ROD 80%",  sprintf( "%.1f", c(ROD80_KBD[id], mean(ROD80_KBD[id]) ) ), sep = " & ")
  cat("\n")
  cat("#DLT", sprintf( "%.1f", c(n_DLT_KBD[id], mean(n_DLT_KBD[id]) ) ), sep = " & ")
  cat("\n")
  cat("PM", sprintf( "%.1f", c(monotonic_KBD[id], mean(monotonic_KBD[id]) ) ), sep = " & ")
  cat("\n\n")
}
