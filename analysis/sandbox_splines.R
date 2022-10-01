

tt2 <- ci[Subject == 2 , ]
tt3 <- ci[Subject == 3 , ]

idx1 <- sort(sample(1:501, 200, replace = FALSE))
idx2 <- sort(sample(1:501, 200, replace = FALSE))

t2 <- tt2[idx1, ]
t3 <- tt3[idx2, ]

fit2 <- lm(Fixations ~ ns(Time, df = 5), data = t2)
fit3 <- lm(Fixations ~ ns(Time, df = 5), data = t3)


time <- seq(0, 2000, by = 4)

time1 <- tt2$Time
time2 <- t3$Time
time3 <- t2$Time

q1 <- predict(fit2, data.table(Time = time1))
q2 <- predict(fit3, data.table(Time = time1))
q3 <- predict(fit3, data.table(Time = time2))
q4 <- predict(fit3, data.table(Time = time3))
plot(q2)
plot(q3)
plot(t2$Fixations)
plot(t3$Fixations)

## Thankfully, these match
head(q2[idx2])
head(q3)
head(q2[idx1])
head(q4)


q2[idx2] - q3


attr(ns(t2$Time, df = 5), "knots")
attr(ns(t3$Time, df = 5), "knots")


#############
fit <- lm(Fixations ~ ns(Time, df = 5), data = t2)
ns(t2$Time, df = 5)
fit_gnls <- gnls()

dat <- t2
y <- "Fixations"
time <- "Time"
pp <- lm(dat[[y]] ~ ns(dat[[time]], df = 5))

pp <- lm(dat[[y]] ~ poly(dat[[time]], degree = degree, raw = raw))

degree <- 5
params <- setNames(coef(pp), c(paste0("beta", seq(degree + 1L))))


time_names <- paste0("I(Time^", seq(degree + 1L) - 1L , ")")

ff <- paste(names(params), time_names, sep = "*", collapse = "+")
ff <- str2lang(ff)
y <- str2lang(y)
ff <- bquote(.(y) ~ .(ff))

rr <- coef(pp)
names(rr) <- NULL
tt <- gnls(Fixations ~ I(ns(Time, df = 5)), data = dat, start = pp)


##########

fit <- lm(Fixations ~ ns(Time, df = 5), data = t2)
time <- 0:2000

x1 <- predict(fit, data.frame(Time = time))

cc <- coef(fit)
vv <- vcov(fit)
nc <- rmvnorm(1, mean = cc, sigma = vv)
fit$coefficients <- nc

x2 <- predict(fit, data.frame(Time = time))


fitg <- bdObj[1, ]$fit[[1]]

DT <- data.frame(Time = time)
y1 <- predict(fitg, DT)
