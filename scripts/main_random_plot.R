# install.packages("vioplot")
# https://r-coder.com/violin-plot-r/
library("vioplot")

load("results/OC_J5p02.RData")
load("results/OC_J5p03.RData")
load("results/OC_J6p02.RData")
load("results/OC_J6p03.RData")


pdf("figures/rand_perf.pdf", width=9, height=9)

par(ps = 15, mfrow = c(2, 2), family="serif", mar=c(4,4,3,1))

#-------------- J = 5, phi = 0.2 -----------------------------------
vioplot(OC_J5p02[1:4], col = "grey",
        ylim = c(-18, 18),
        main = expression((a)*~J == 5*","~phi == 0.2),
        names = c("PCS", "PCA", "above-MTD", "ROD"))
mtext("Difference (SKBD − Keyboard)", side = 2, line = 2.5)
abline(h = 0, lty = 2)
points(1:4, colMeans(OC_J5p02[1:4]), 
       pch = 24, cex = 1.1, bg = "white", col = "black")
legend("topright", inset = 0.02, bty = "n",
       legend = c("Median", "Mean"),
       pch    = c(21, 24),
       pt.bg  = c("white", "white"),
       col    = c("black", "black"),
       pt.cex = c(1.0, 1.1))

#-------------- J = 5, phi = 0.3 -----------------------------------
vioplot(OC_J5p03[1:4], col = "grey",
        ylim = c(-18, 18),
        main = expression((b)*~J == 5*","~phi == 0.3),
        names = c("PCS", "PCA", "above-MTD", "ROD"))
mtext("Difference (SKBD − Keyboard)", side = 2, line = 2.5)
abline(h = 0, lty = 2)
points(1:4, colMeans(OC_J5p03[1:4]), 
       pch = 24, cex = 1.1, bg = "white", col = "black")
legend("topright", inset = 0.02, bty = "n",
       legend = c("Median", "Mean"),
       pch    = c(21, 24),
       pt.bg  = c("white", "white"),
       col    = c("black", "black"),
       pt.cex = c(1.0, 1.1))

#-------------- J = 6, phi = 0.2 -----------------------------------
vioplot(OC_J6p02[1:4], col = "grey",
        ylim = c(-18, 18),
        main = expression((c)*~J == 6*","~phi == 0.2),
        names = c("PCS", "PCA", "above-MTD", "ROD"))
mtext("Difference (SKBD − Keyboard)", side = 2, line = 2.5)
abline(h = 0, lty = 2)
points(1:4, colMeans(OC_J6p02[1:4]), 
       pch = 24, cex = 1.1, bg = "white", col = "black")
legend("topright", inset = 0.02, bty = "n",
       legend = c("Median", "Mean"),
       pch    = c(21, 24),
       pt.bg  = c("white", "white"),
       col    = c("black", "black"),
       pt.cex = c(1.0, 1.1))

#-------------- J = 6, phi = 0.3 -----------------------------------
vioplot(OC_J6p03[1:4], col = "grey",
        ylim = c(-18, 18),
        main = expression((d)*~J == 6*","~phi == 0.3),
        names = c("PCS", "PCA", "above-MTD", "ROD"))
mtext("Difference (SKBD − Keyboard)", side = 2, line = 2.5)
abline(h = 0, lty = 2)
points(1:4, colMeans(OC_J6p03[1:4]), 
       pch = 24, cex = 1.1, bg = "white", col = "black")
legend("topright", inset = 0.02, bty = "n",
       legend = c("Median", "Mean"),
       pch    = c(21, 24),
       pt.bg  = c("white", "white"),
       col    = c("black", "black"),
       pt.cex = c(1.0, 1.1))

dev.off()
