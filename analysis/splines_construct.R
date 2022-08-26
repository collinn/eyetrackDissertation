
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


y <- seq(-2, 2, by = 0.1)^2

ysp <- ns(y, df = 3)
matplot(ysp, type = 'l', col = 'gray50', ylim = c(-0.5, 2))
lines(rowSums(ysp))
lines(y)
abline(v = 16, col = 'red', lty = 2)
fit <- lm(y ~ ysp)


require(stats); require(graphics)
nsw <- ns(women$height, df = 5)
summary(fm1 <- lm(weight ~ ns(height, df = 5), data = women))
attr(terms(fm1), "predvars")

## example of safe prediction
plot(women, xlab = "Height (in)", ylab = "Weight (lb)")
ht <- seq(57, 73, length.out = 200) ; nD <- data.frame(height = ht)
lines(ht, p1 <- predict(fm1, nD))
stopifnot(all.equal(p1, predict(update(fm1, . ~
                                         splines::ns(height, df=5)), nD)))
