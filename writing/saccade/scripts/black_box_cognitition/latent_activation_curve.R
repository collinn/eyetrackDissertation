library(data.table)

dat <- fread("../make_vwp_trace_img/trace_plots.csv")
dat <- dat[cycle < 75, ]
dat$V7 <- NULL
names(dat) <- c("cycle", "partial", "apart", "pap", "da", "artial")

pdf("activation_plot.pdf", width = 4, height = 4)
with(dat, plot(cycle, partial, type = 'l', xaxt='n', yaxt='n', ylab="", xlab="",
               lwd = 2))
dev.off()
#
# dat1 <- copy(dat)
# dat1[, cycle := 0.9 * cycle]
#
# dat3 <- copy(dat)
#
# dat3[, `:=`(apart2 = 0.6 * apart, cycle = cycle * 1.25)]
#
# dat4 <- copy(dat)
# dat4[, `:=`(cycle = 0.9*cycle)]
#
# #dat[, apart2 := 0.6 * apart]
# ll <- 4
#
# with(dat1, plot(cycle, partial, type = 'l', lwd = ll))
# #with(dat4, lines(cycle, apart, col = 'green', lwd = ll))
# #with(dat3, lines(cycle, apart2, col = "red", lwd = ll))
#
# # newval <- (dat4$apart + dat3$apart2) /2
# # lines(dat3$cycle, newval, col = 'purple')
#
# dat2 <- copy(dat)
# dat2 <- dat2[, .(cycle, apart)]
# dat2[, cycle := cycle * 1.45]
# dat2[, apart := apart * 0.3]
#
# lines(dat2$cycle, dat2$apart, col = 'blue', lwd = ll)
#
#
# newdat <- dat4[cycle > 30, ]
# newdat <- newdat[cycle < 44, ]
# newdat$cycle <- seq(31, 66, length.out = 15)
# newdat[, apart := apart * exp(seq(0, -1.5, length.out = 15))]
#
# dat5 <- rbind(dat4[cycle < 30, ], newdat)
# lines(dat5$cycle, dat5$apart, col = 'orange', lwd = ll)
#
#
#
# newdat2 <- dat3[cycle > 45, ]
# newdat2 <- newdat2[cycle < 60, ]
# newdat2$cycle <- seq(45, 70, length.out = 11)
# newdat2[, apart2 := apart2 * exp(seq(0, -1, length.out = 11)) ]
#
# dat6 <- rbind(dat3[cycle <= 40, ], newdat2)
# lines(dat6$cycle, dat6$apart2, col = 'brown', lwd = ll)
#
#
# d1 <- dat1[, .(cycle, partial)]
# d2 <- dat2[, .(cycle, apart)]
# dat5[, apart3 := apart]
# d3 <- dat5[, .(cycle, apart3)]
# d4 <- dat6[, .(cycle, apart2)]
#
# dd1 <- melt(d1, id.vars = 'cycle', variable.name = "word", value.name = "activation")
# dd2 <- melt(d2, id.vars = 'cycle', variable.name = "word", value.name = "activation")
# dd3 <- melt(d3, id.vars = 'cycle', variable.name = "word", value.name = "activation")
# dd4 <- melt(d4, id.vars = 'cycle', variable.name = "word", value.name = "activation")
#
# dd3$activation <- dd3$activation*.85
# dd2$activation <- dd2$activation*.75
#
# dat <- rbindlist(list(dd1, dd2, dd3, dd4))
#
# dat <- rbindlist(list(dd2, dd3, dd1, dd4))
#
# library(ggplot2)
#
# pdf("fake_proportion.pdf", width = 3, height = 3)
# ggplot(dat[cycle < 70, ], aes(cycle, activation, color = word)) + geom_line(linewidth = 1) +
#   theme_classic() +
#   theme(legend.position = "none", axis.text = element_blank(), axis.ticks = element_blank()) +
#   labs(x = "Time", y = "Proportion of Fixation")
# dev.off()
