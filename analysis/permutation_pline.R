
library(bdots)
library(data.table)
library(mvtnorm)

## Idea is a piecewise linear function on domain (-5, 5)
# breakpoint at zero, horizontal prior (at y = 0) and then
# slope of different slope after in the condition group (else zero)

# time[251] == 0 ## Fucking hardcoded length of time here thats fine
TIME <- seq(-1, 1, length.out = 501)


## Pars for piecewise function in bdots
# (t < t1)*p1 + (t>=t1)*(mt + p1)
plinePars <- function(dat, y, time, params = NULL, ...) {
  y2 <- dat[[y]]

  ll <- length(y2)
  mm <- round(ll/2)
  r1 <- `:`(1, mm+1) # range first half
  r2 <- `:`(mm+1, ll) # range second half
  yy <- y2[r2]
  tt <- dat[[time]][r2]

  ## baseline
  b <- mean(y2[r1]) / 10 # let's move this b* closer to zero
  ## slope
  m <- max(0, (yy %*% tt) / (tt %*% tt))
  params <- c(b, m)
  names(params) <- c("b", "m")

  y <- str2lang(y)
  time <- str2lang(time)
  ff <- bquote(.(y) ~ (.(time) < 0)*b +
                 (.(time) >= 0)*(m*.(time) + b))
  attr(ff, "parnames") <- names(params)
  return(list(formula = ff, params = params))
}

## Function to create piecewise line
pline <- function(p, time, gp) {
  stopifnot(length(p) == 2)

  ll <- length(time)
  mm <- round(ll/2)
  r1 <- `:`(1, mm+1) # range first half
  r2 <- `:`(mm+1, ll) # range second half

  y <- vector("numeric", length = length(time))
  if (gp == "A") {
    y[r1] <- p[1]
    y[r2] <- p[1] + p[2] * time[r2]
  } else {
    y[] <- p[1]
  }
  y
}


# no need for trials (I want trials, so I will continue to use binomial here)
createPlineData <- function(n = 25, trials = 100, pars = c(0,1), gp = "A") {

  p <- rmvnorm(n, mean = pars, sigma = diag(length(pars))*0.05)


  ## We actually need there to be no slope
  if (gp == "B") p[, 2] <- p[, 1]

  p <- abs(p)
  spars <- split(p, row(p))
  dts <- lapply(seq_len(n), function(x) {
    pp <- spars[[x]]
    dt <- data.table(id = x,
                     time = TIME,
                     group = gp,
                     true = pline(pp, TIME, gp))
    dt[, fixations := rnorm(1, true, sd = 0.25/sqrt(trials)), by = time]
  })
  dts <- rbindlist(dts)
  return(dts)
}

dt1 <- createPlineData(n = 25, pars = c(0,0.5), gp = "A", trials = 100)
dt2 <- createPlineData(n = 25, pars = c(0,0), gp = "B", trials = 100)
dt2[, id := id + 25]
dt <- rbindlist(list(dt1, dt2))

fit <- bdotsFit(data = dt,
                subject = "id",
                time = "time",
                group = "group",
                y = "fixations",
                curveType = plinePars())

#plot(fit[1:4, ])
#plot(fit[47:50, ])

## Bootstrap
boots <- bdotsBoot(fixations ~ group(A, B), fit, singleMeans = TRUE, Niter = 250)
bootm <- bdotsBoot(fixations ~ group(A, B), fit, Niter = 250)
bootp <- bdotsBoot(fixations ~ group(A, B), fit, skipDist = TRUE,
                   permutation = TRUE, Niter = 250)

boots$sigTime
bootm$sigTime
bootp$sigTime

plot(bootp)
## Check with old bdots?
# fit0 <- bdots0::bdotsFit(data = dt,
#                 subject = "id",
#                 time = "time",
#                 group = "group",
#                 y = "fixations",
#                 curveType = plinePars())
## Bootstrap
boot0 <- bdots0::bdotsBoot(fixations ~ group(A, B), fit)



## Permutation component

getT <- function(x, idx, whole = FALSE) {
  x$group <- x$group[idx]
  fit_s <- split(x, by = "group")
  mvl <- lapply(fit_s, function(x) {
    cc <- coef(x)
    gp <- x[['group']][1] # unique to this sim
    #time <- seq(0, 2000, by = 4)
    time <- TIME
    cl <- apply(cc, 1, function(y) pline(y, time, gp))
    mm <- rowMeans(cl)
    vv <- apply(cl, 1, var)
    vvn <- vv/nrow(cc)
    list(mean = mm, nvar = vvn)
  })

  ## Guess I don't need a function for this
  x <- mvl[[1]]; y <- mvl[[2]]
  xm <- x$mean; xv <- x$nvar
  ym <- y$mean; yv <- y$nvar
  Tt <- abs(xm-ym) / sqrt(yv + xv)

  ifelse(whole, return(Tt), return(max(Tt)))
}

getTDist <- function(x, P = 1000) {
  n <- nrow(x)
  permmat <- replicate(P, sample(seq_len(n), n))
  tvec <- getT(x, seq_len(n), whole = TRUE)
  tnull <- apply(permmat, 2, function(y) {
    getT(x, y)
  })
}

actual_t <- getT(fit, seq_len(nrow(fit)), whole = TRUE)
t500 <- getTDist(fit, 500)
tq <- quantile(t500, probs = 0.975)

plot(actual_t, type = 'l')
abline(h = tq, lty = 2, col='red')
idx <- which(actual_t > tq)
sig_time <- TIME[idx]
abline(v = min(idx), lty = 2, col = 'green')

#boot <- bdotsBoot(fixations ~ group(A, B), fit)
summary(boot)
