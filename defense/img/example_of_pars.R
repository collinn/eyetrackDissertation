library(data.table)
library(eyetrackSim)
library(bdots)
library(ggplot2)

## For logistic
params <- c(0.1, .95, 0.0019, 969.3)

ff <- logistic_f(params, 0:2000)

png("logistic.png", width = 720, height=720)
plot(0:2000, ff, type = 'l', lwd = 5, xaxt="n", yaxt="n", 
     xlab = "", ylab = "", xlim = c(0, 2000), ylim = c(0, 1.1))
title(ylab="Logistic Function", xlab = "Time (ms)", line =1, 
      cex.lab = 2)
dev.off()


## For use with saccades
png("logistic2.png", width = 1008, height=720)
plot(0:2000, ff, type = 'l', lwd = 5, xaxt="n", yaxt="n", 
     xlab = "", ylab = "", xlim = c(0, 2000), ylim = c(0, 1.1))
title(ylab="Logistic Function", xlab = "Time (ms)", line =1, 
      cex.lab = 2)
dev.off()