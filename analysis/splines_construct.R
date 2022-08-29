
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


#### Attempt step function in R
## note, this doesn't actually use any splines, probably doesn't
# even belong here

#' @param p height of step
s1 <- function(p) {
  stopifnot(length(p) == 2)
  x <- c(20, 60) # putting here for now
  x1 <- x[1]
  x2 <- x[2]
  y <- vector("numeric", length = 100L)
  y[`:`(x1, x1+15)] <- p[1]
  y[`:`(x2, x2+15)] <- p[2]
  y
}

# drawing parameters for group 1
p <- rmvnorm(100, mean = c(1,2), sigma = diag(2)*0.25)

# no need for trials
createStepData <- function(n = 25, trials = 100, pars, gp = "A") {
  p <- rmvnorm(n, mean = pars, sigma = diag(length(pars))*0.25)
  p <- abs(p)
  spars <- split(p, row(p))
  dts <- lapply(seq_len(n), function(x) {
    pp <- spars[[x]]
    dt <- data.table(id = x, 
                     time = 1:100, 
                     group = gp, 
                     true = s1(pp))
    dt[, fixations := mean(rbinom(trials, 1, 0.1)) + true, by = time]
  })
  dts <- rbindlist(dts)
  return(dts) # don't need pars here
}

dt1 <- createStepData(n = 25, pars = c(1,2), gp = "A")
dt2 <- createStepData(n = 25, pars = c(0,0), gp = "B")
dt2[, id := id + 25]
dt <- rbindlist(list(dt1, dt2))

#dt <- split(dt,by = "id")

#dat <- dt[[1]]

## Cool, this for bdots
stepPars <- function(dat, y, time, params = NULL, ...) {
  #time <- dat[[time]]
  y2 <- dat[[y]]
  
  p1 <- max(y2[20:30])
  p2 <- max(y2[60:70])
  p3 <- max(min(y2), 0)
  params <- c(p1, p2, p3)
  names(params) <- c("p1", "p2", "p3")
  y <- str2lang(y)
  time <- str2lang(time)
  ff <- bquote(.(y) ~ (.(time) >= 20 & .(time) <=35)*p1 +
                 (.(time) >= 60 & .(time) <= 75)*p2 + p3)
  attr(ff, "parnames") <- names(params)
  return(list(formula = ff, params = params))
}

fit <- bdotsFit(data = dt, 
                subject = "id",
                time = "time", 
                group = "group",
                y = "fixations", 
                curveType = stepPars())
debugonce(bdotsFit)


boot <- bdotsBoot(fixations ~ group(A, B), fit)

## Well, shit
plot(boot, plotDiffs = FALSE)

## Ok, so good -- we've determined that bdots can detect jump 
# discontiuities pretty well. Need to figure out a way to make this
# less the case, say with more continuous bumps, and then see where
# the line is drawn for bdots and permutation test


## Can maybe still do splines?
dat <- split(dt, by = "id")[[1]]

tt <- ns(dat$fixations, df = 3)

fit <- lm(dat$fixations ~ ns(dat$fixations, df = 3))

plot(tt %*% coef(fit)[-1] + coef(fit)[1], type = 'l')










