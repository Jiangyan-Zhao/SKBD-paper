rm(list = ls())  # clear workspace variables

library(SKBD)

#-------------------------- generate the random scenarios------------------------------
n_dose = 5
target_prob = 0.2
n_cohort = 10
cohort_size = 3
n_scenarios = 1000
scenarios = PUA(1:n_dose, target_prob, n_scenarios)

pdf("figures/rand_scen_J5p02.pdf", width=12, height=6)
par(ps = 15, mfrow = c(1, 2), family="serif")
matplot(t(scenarios[1:20,]), type = "l", lty = 1,
        xlab = "dose level", ylab = "toxicity probability", 
        main = "(a) dose-toxicity curves")
abline(h = target_prob, lty = 2)
boxplot(scenarios, xlab = "dose level", ylab = "toxicity probability", 
        main = "(b) toxicity distribution")
dev.off()
#--------------------------------------------------------------------------------------

OC_SKBD = OC_KBD = vector("list", n_scenarios)

PCS_SKBD = PCS_KBD = rep(NA, n_scenarios)
PCA_SKBD = PCA_KBD = rep(NA, n_scenarios)
above_MTD_SKBD = above_MTD_KBD = rep(NA, n_scenarios)
ROD60_SKBD = ROD60_KBD = rep(NA, n_scenarios)


#------------------------------- SKBD --------------------------------------------------
for (s in 1:n_scenarios) {
  if(s %% 100 == 1){
    cat("\n=== Now running scenario", s, "===\n")
  }
  OC_SKBD[[s]] = get_OC_SKBD(target_prob = target_prob, tox_prob = scenarios[s, ],
                             n_cohort = n_cohort, cohort_size = cohort_size,
                             # symmetric = TRUE,
                             # shared = FALSE,
                             n_trial = 1e4)
  PCS_SKBD[s] = OC_SKBD[[s]]$PCS
  
  PCA_SKBD[s] = OC_SKBD[[s]]$PCA
  
  above_MTD_SKBD[s] = OC_SKBD[[s]]$above_MTD
  
  ROD60_SKBD[s] = OC_SKBD[[s]]$ROD60
}

OC_SKBD_summary = c(
  mean(PCS_SKBD), mean(PCA_SKBD),
  mean(above_MTD_SKBD), mean(ROD60_SKBD))

cat("OC_SKBD summary: \n\n")
cat(sprintf("%.1f", OC_SKBD_summary), sep = " & ")

#------------------------------- KBD --------------------------------------------------
for (s in 1:n_scenarios) {
  if(s %% 100 == 1){
    cat("\n=== Now running scenario", s, "===\n")
  }
  OC_KBD[[s]] = get_OC_SKBD(target_prob = target_prob, tox_prob = scenarios[s, ],
                            n_cohort = n_cohort, cohort_size = cohort_size,
                            shared = FALSE,
                            n_trial = 1e4)
  PCS_KBD[s] = OC_KBD[[s]]$PCS
  
  PCA_KBD[s] = OC_KBD[[s]]$PCA
  
  above_MTD_KBD[s] = OC_KBD[[s]]$above_MTD
  
  ROD60_KBD[s] = OC_KBD[[s]]$ROD60
}

OC_KBD_summary = c(
  mean(PCS_KBD), mean(PCA_KBD),
  mean(above_MTD_KBD), mean(ROD60_KBD))

cat("OC_KBD summary: \n\n")
cat(sprintf("%.1f", OC_KBD_summary), sep = " & ")


OC_J5p02 = data.frame(
  PCS = PCS_SKBD - PCS_KBD,
  PCA = PCA_SKBD - PCA_KBD,
  above_MTD = above_MTD_SKBD - above_MTD_KBD,
  ROD = ROD60_SKBD - ROD60_KBD,
  PCS_SKBD = PCS_SKBD, PCS_KBD = PCS_KBD, 
  PCA_SKBD = PCA_SKBD, PCA_KBD = PCA_KBD,
  above_MTD_SKBD = above_MTD_SKBD, above_MTD_KBD = above_MTD_KBD, 
  ROD_SKBD = ROD60_SKBD, ROD_KBD = ROD60_KBD,
  OC_SKBD_summary = OC_SKBD_summary,
  OC_KBD_summary = OC_KBD_summary
)

save(OC_J5p02, file = "results/OC_J5p02.RData")
