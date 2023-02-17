

library(eyetrackSim)

pars <- eyetrackSim:::EMPIRICAL_START_PARS[[1]]
pars <- c(0, 1, 0.002, 1000)
tt <- seq(0, 2000, 4)
f <- function(rr) {
  logistic_f(p = pars, t = rr)
}
#f <- logistic_f(p = pars, t = tt)

## First the logistic function
plot(tt, f(tt), type = 'l', lwd = 5,
     xaxt='n', yaxt='n', xlab = "",
     ylab = "", ylim = c(-0.05, 1))
title(ylab = "Logistic Function", line = 0.5 , cex.lab=1.75)

# save this

## Then we get a point
points(x = 750, y = f(750), col = 'steelblue', cex = 5, pch=16)
points(x = 750, y = f(750), col = 'black', cex = 5, lwd = 1)



# save

## Then this line with dots
arrows(755, f(750), 1750, f(750), lwd = 3)
points(x = 750, y = f(750), col = 'steelblue', cex = 5, pch=16)
points(x = 750, y = f(750), col = 'black', cex = 5, lwd = 1)

xx <- 900
points(x = xx, y = f(750), col = 'brown', cex = 2, pch=16)
points(x = xx, y = f(750), col = 'black', cex = 2, lwd = 1)

xx <- 1050
points(x = xx, y = f(750), col = 'brown', cex = 2, pch=16)
points(x = xx, y = f(750), col = 'black', cex = 2, lwd = 1)

xx <- 1200
  points(x = xx, y = f(750), col = 'brown', cex = 2, pch=16)
points(x = xx, y = f(750), col = 'black', cex = 2, lwd = 1)

xx <- 1350
points(x = xx, y = f(750), col = 'brown', cex = 2, pch=16)
points(x = xx, y = f(750), col = 'black', cex = 2, lwd = 1)

xx <- 1500
points(x = xx, y = f(750), col = 'brown', cex = 2, pch=16)
points(x = xx, y = f(750), col = 'black', cex = 2, lwd = 1)


