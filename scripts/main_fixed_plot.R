
load("results/KBD_fixed.RData")
load("results/SKBD_fixed.RData")

#------------------------------------------- plot -------------------------------------------
pdf("figures/fixed_accuracy.pdf", width=8, height=8)
par(ps = 15, mfrow = c(2, 2), family="serif", mar=c(4,4,3,1))
# PCS: Scenario 1-10, Target DLT = 0.2
plot(1:10, PCS_SKBD[1:10],
     type="b", lty=1, pch=15, lwd=1.5,
     ylim=c(30,80),
     main = expression((a)*~PCS~under~phi == 0.2),
     xlab ="scenario", ylab="PCS (%)",
     axes=FALSE)
lines(1:10, PCS_KBD[1:10], type="b",lwd=1.5, pch=1)
axis(1, at = 1:10)
axis(2, las=2)
legend(1, 80, legend=c("SKBD","Keyboard"), pch=c(15,1), lty=1, bty="n")

# PCS: Scenario 11-20, Target DLT = 0.3
plot(11:20, PCS_SKBD[11:20],
     type="b", lty=1, pch=15, lwd=1.5,
     ylim=c(40,90),
     main = expression((b)*~PCS~under~phi == 0.3),
     xlab ="scenario", ylab="PCS (%)",
     axes=FALSE)
lines(11:20, PCS_KBD[11:20], type="b", lwd=1.5, pch=1)
axis(1, at = 11:20, labels = 11:20)
axis(2, las=2)

# PCA: Scenario 1-10, Target DLT = 0.2
plot(1:10, PCA_SKBD[1:10],
     type="b", lty=1, pch=15, lwd=1.5,
     ylim=c(20,80),
     main = expression((c)*~PCA~under~phi == 0.2),
     xlab ="scenario", ylab="PCA (%)",
     axes=FALSE)
lines(1:10, PCA_KBD[1:10], type="b", lwd=1.5, pch=1)
axis(1, at = 1:10, labels = 1:10)
axis(2, las=2)

# PCA: Scenario 11-20, Target DLT = 0.3
plot(11:20, PCA_SKBD[11:20],
     type="b", lty=1, pch=15, col=1, lwd=1.5,
     ylim=c(20,80),
     main = expression((d)*~PCA~under~phi == 0.3),
     xlab ="scenario", ylab="PCA (%)",
     axes=FALSE)
lines(11:20, PCA_KBD[11:20], type="b", lwd=1.5, pch=1)
axis(1, at = 11:20, labels = 11:20)
axis(2, las=2)
dev.off()


# pdf("fixed_safety.pdf", width=8, height=12)
# par(ps = 15, mfrow = c(3, 2), family="serif", mar=c(4,4,3,1))
pdf("figures/fixed_safety.pdf", width=8, height=8)
par(ps = 15, mfrow = c(2, 2), family="serif", mar=c(4,4,3,1))
# above_MTD: Scenario 1-10, Target DLT = 0.2
plot(1:10, above_MTD_SKBD[1:10],
     type="b", lty=1, pch=15, lwd=1.5,
     ylim=c(0,40),
     main = expression((a)*~above~MTD~under~phi == 0.2),
     xlab ="scenario", ylab="above MTD (%)",
     axes=FALSE)
lines(1:10, above_MTD_KBD[1:10], type="b", lwd=1.5, pch=1)
axis(1, at = 1:10)
axis(2, las=2)
legend(7, 40, legend=c("SKBD","Keyboard"), pch=c(15,1), lty=1, bty="n")

# above_MTD: Scenario 11-20, Target DLT = 0.3
plot(11:20, above_MTD_SKBD[11:20],
     type="b", lty=1, pch=15, lwd=1.5,
     ylim=c(0,40),
     main = expression((b)*~above~MTD~under~phi == 0.3),
     xlab ="scenario", ylab="above MTD (%)",
     axes=FALSE)
lines(11:20, above_MTD_KBD[11:20], type="b", lwd=1.5, pch=1)
axis(1, at = 11:20, labels = 11:20)
axis(2, las=2)

# ROD 60%: Scenario 1-10, Target DLT = 0.2
plot(1:10, ROD60_SKBD[1:10],
     type="b", lty=1, pch=15, lwd=1.5,
     ylim=c(0,25),
     main = expression((c)*~ROD~under~phi == 0.2),
     xlab ="scenario", ylab="ROD (%)",
     axes=FALSE)
lines(1:10, ROD60_KBD[1:10], type="b", lwd=1.5, pch=1)
axis(1, at = 1:10, labels = 1:10)
axis(2, las=2)

# ROD 60%: Scenario 11-20, Target DLT = 0.3
plot(11:20, ROD60_SKBD[11:20],
     type="b", lty=1, pch=15, col=1, lwd=1.5,
     ylim=c(0,30),
     main = expression((d)*~ROD~under~phi == 0.3),
     xlab ="scenario", ylab="ROD (%)",
     axes=FALSE)
lines(11:20, ROD60_KBD[11:20], type="b", lwd=1.5, pch=1)
axis(1, at = 11:20, labels = 11:20)
axis(2, las=2)

dev.off()