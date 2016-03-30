load("simulation_metrics.Rdata")

pdf("simulation_metrics_boxplot.pdf", height = 12, width = 8)
par(mfrow = c(4,1))
boxplot(simulation_metrics$accuracymat, use.cols = T, xlab = "Simulation model", ylab = "Distance between sim and est", main = "Accuracy")
boxplot(simulation_metrics$precisionmat, use.cols = T, xlab = "Simulation model", ylab = "Node support", main = "Precision")
boxplot(simulation_metrics$steminessmat, use.cols = T, xlab = "Simulation model", ylab = "Stemminess", main = "Stemminess")
boxplot(simulation_metrics$imbalmat, use.cols = T, xlab = "Simulation model", ylab = "Colless Index", main = "Imbalance")
dev.off()