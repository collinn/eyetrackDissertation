

library(eyetrackSim)

pars <- eyetrackSim:::EMPIRICAL_START_PARS[[1]]
pars <- c(0, 1, 0.002, 1000)
tt <- seq(0, 2500, 4)
f <- function(rr) {
  logistic_f(p = pars, t = rr)
}
#f <- logistic_f(p = pars, t = tt)

## First the logistic function
pdf("../img/logistic_a.pdf")
plot(tt, f(tt), type = 'l', lwd = 5,
     xaxt='n', yaxt='n', xlab = "",
     ylab = "", ylim = c(-0.05, 1), axes = FALSE)
axis(1, labels = FALSE, lwd.ticks = 0, at = c(-100, 2500))
axis(2, labels = FALSE, lwd.ticks = 0, at = c(-0.1, 1.1))
#title(ylab = expression(paste("f(%.%", "|", theta, ")")), line = 0.5 , cex.lab=1.75)
dev.off()
# save this

## Then we get a point
pdf("../img/logistic_b.pdf")
plot(tt, f(tt), type = 'l', lwd = 5,
     xaxt='n', yaxt='n', xlab = "",
     ylab = "", ylim = c(-0.05, 1), axes = FALSE)
axis(1, labels = FALSE, lwd.ticks = 0, at = c(-100, 2500))
axis(2, labels = FALSE, lwd.ticks = 0, at = c(-0.1, 1.1))
lines(c(750, 750), c(-0.15, f(750)), xpd = TRUE, lty = 2, lwd = 2)
lines(c(-150, 750), c(f(750), f(750)), xpd = TRUE, lty = 2,  lwd = 2)
text(-275, f(750), xpd = TRUE, labels = "f(t)", cex = 2.5)
points(x = 750, y = f(750), col = 'steelblue', cex = 5, pch=16)
points(x = 750, y = f(750), col = 'black', cex = 5, lwd = 1)
text(750, -0.225, xpd = TRUE, labels = "t", cex = 3)
dev.off()


# save

## Then this line with dots
pdf("../img/logistic_c.pdf")
plot(tt, f(tt), type = 'l', lwd = 5,
     xaxt='n', yaxt='n', xlab = "",
     ylab = "", ylim = c(-0.05, 1), axes = FALSE)
axis(1, labels = FALSE, lwd.ticks = 0, at = c(-100, 2500))
axis(2, labels = FALSE, lwd.ticks = 0, at = c(-0.1, 1.1))
lines(c(-150, 750), c(f(750), f(750)), xpd = TRUE, lty = 2,  lwd = 2)
text(-275, f(750), xpd = TRUE, labels = "f(t)", cex = 2.5)
points(x = 750, y = f(750), col = 'steelblue', cex = 5, pch=16)
points(x = 750, y = f(750), col = 'black', cex = 5, lwd = 1)
arrows(750, f(750), 2000, f(750), lwd = 3)
lines(c(750, 750), c(-0.15, f(750)), xpd = TRUE, lty = 2, lwd = 2)
points(x = 750, y = f(750), col = 'steelblue', cex = 5, pch=16)
points(x = 750, y = f(750), col = 'black', cex = 5, lwd = 1)
text(750, -0.225, xpd = TRUE, labels = "t", cex = 3)
points(x = 750, y = f(750), col = 'steelblue', cex = 5, pch=16)
points(x = 750, y = f(750), col = 'black', cex = 5, lwd = 1)

xx <- 1000
lines(c(xx, xx), c(-0.15, f(750)), xpd = TRUE, lty = 2, lwd = 1.5)
text(xx+10, -0.2, xpd = TRUE, labels = "t+1", cex = 1.5)
points(x = xx, y = f(750), col = 'brown', cex = 2.5, pch=16)
points(x = xx, y = f(750), col = 'black', cex = 2.5, lwd = 1)

xx <- 1250
lines(c(xx, xx), c(-0.15, f(750)), xpd = TRUE, lty = 2, lwd = 1.5)
text(xx+10, -0.2, xpd = TRUE, labels = "t+2", cex = 1.5)
points(x = xx, y = f(750), col = 'brown', cex = 2.5, pch=16)
points(x = xx, y = f(750), col = 'black', cex = 2.5, lwd = 1)

xx <- 1500
text(xx+10, 0, xpd = TRUE, labels = expression(cdots),cex=2.5)
points(x = xx, y = f(750), col = 'brown', cex = 2.5, pch=16)
points(x = xx, y = f(750), col = 'black', cex = 2.5, lwd = 1)

xx <- 1750
lines(c(xx, xx), c(-0.15, f(750)), xpd = TRUE, lty = 2, lwd = 1.5)
text(xx+10, -0.2, xpd = TRUE, labels = "t+n", cex = 1.5)
points(x = xx, y = f(750), col = 'brown', cex = 2.5, pch=16)
points(x = xx, y = f(750), col = 'black', cex = 2.5, lwd = 1)
dev.off()

