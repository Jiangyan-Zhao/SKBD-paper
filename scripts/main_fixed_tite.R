rm(list = ls())  # clear workspace variables

library(SKBD)
source("scripts/Scenario.R")

target_prob = c(rep(0.2, 10), rep(0.3, 10))
cohort_size = 3
n_cohort = 10

OC_SKBD = OC_KBD = list()
PCS_SKBD = PCS_KBD = rep(NA, 20)
PCA_SKBD = PCA_KBD = rep(NA, 20)
above_MTD_SKBD = above_MTD_KBD = rep(NA, 20)
ROD60_SKBD = ROD60_KBD = rep(NA, 20)
time_SKBD = time_KBD = rep(NA, 20)

#------------------------------- SKBD --------------------------------------------------
for (s in 1:nrow(Scenario)) {
  # cat("\n=== Now running scenario", s, "===\n")
  OC_SKBD[[s]] = get_OC_TITE_SKBD(target_prob = target_prob[s], tox_prob = Scenario[s, ],
                                  n_cohort = n_cohort, cohort_size = cohort_size,
                                  n_trial = 1e4)
  PCS_SKBD[s] = OC_SKBD[[s]]$PCS
  
  PCA_SKBD[s] = OC_SKBD[[s]]$PCA
  
  above_MTD_SKBD[s] = OC_SKBD[[s]]$above_MTD
  
  ROD60_SKBD[s] = OC_SKBD[[s]]$ROD60
  
  time_SKBD[s] = OC_SKBD[[s]]$duration_mean
}

# saveRDS(df_SKBD_fixed, paste0("df_SKBD_fixed", format(Sys.Date(), "%Y%m%d"), ".rds"))
save(PCS_SKBD, PCA_SKBD, above_MTD_SKBD, ROD60_SKBD, time_SKBD,
     file = "results/SKBD_fixed_tite.RData")

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
  cat("ROD", sprintf( "%.1f", c(ROD60_SKBD[id], mean(ROD60_SKBD[id]) ) ), sep = " & ")
  cat("\n")
  cat("time", sprintf( "%.1f", c(time_SKBD[id], mean(time_SKBD[id]) ) ), sep = " & ")
  cat("\n\n")
}


#------------------------------- KBD --------------------------------------------------
for (s in 1:nrow(Scenario)) {
  # cat("\n=== Now running scenario", s, "===\n")
  OC_KBD[[s]] = get_OC_TITE_SKBD(target_prob = target_prob[s], tox_prob = Scenario[s, ],
                                 n_cohort = n_cohort, cohort_size = cohort_size,
                                 shared = FALSE,
                                 n_trial = 1e4)
  PCS_KBD[s] = OC_KBD[[s]]$PCS
  
  PCA_KBD[s] = OC_KBD[[s]]$PCA
  
  above_MTD_KBD[s] = OC_KBD[[s]]$above_MTD
  
  ROD60_KBD[s] = OC_KBD[[s]]$ROD60
  
  time_KBD[s] = OC_KBD[[s]]$duration_mean
}

save(PCS_KBD, PCA_KBD, above_MTD_KBD, ROD60_KBD, time_KBD,
     file = "results/KBD_fixed_tite.RData")

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
  cat("ROD", sprintf( "%.1f", c(ROD60_KBD[id], mean(ROD60_KBD[id]) ) ), sep = " & ")
  cat("\n")
  cat("time", sprintf( "%.1f", c(time_KBD[id], mean(time_KBD[id]) ) ), sep = " & ")
  cat("\n\n")
}
