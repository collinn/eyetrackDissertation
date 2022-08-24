
## Attempt to make splines to compare for identifying significant differences

# library(fda) <- stuff from FDA book

x <- seq(0, 2*pi, length.out = 100)

tt <- ns(x, df = 3)

## ----------
library(splines2)
knots <- c(0.3, 0.5, 0.6)
x <- seq(0, 1, 0.1)
bsMat <- bSpline(x, knots = knots, degree = 3, intercept = TRUE)
matplot(x, bsMat, type = 'l', ylab = 1)
abline(v = knots, lty = 2, col = 'gray')

knots <- c(0.3, 0.5, 0.6)
x <- seq(0, 1, 0.1)
bsMat <- bSpline(x, knots = knots, degree = 2, intercept = TRUE)
matplot(x, bsMat, type = 'l', ylab = 1)
abline(v = knots, lty = 2, col = 'gray')
