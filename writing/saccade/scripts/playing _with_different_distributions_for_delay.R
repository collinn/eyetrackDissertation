

f <- function(s,k, n = 1000) {
  y <- rweibull(n, shape = s, scale = k)
  print(mean(y)); print(sd(y))
  y
}

hist(f(1.8, 225))
hist(f(0.9, 200))
hist(f(1.8, 225))

ww <- f(1.8, 225, 100000)
z <- rnorm(100000, 200, sd = 30)

summary(z)
summary(ww)


bb <- rbeta(1000, shape1 = 10, shape2 = 4)
hist(bb)
p <- 100
q <- 300
b2 <- (bb*(q-p) + p)
hist(b2)
summary(b2)

rrbeta <- function(n, s1, s2, p, q) {
  b <- rbeta(n, s1, s2)
  y <- b*(q-p) + p
  hist(y)
  print(paste0("mean: ", round(mean(y))))
  y
}

x <- rrbeta(10000, 2, 1, 100, 250)
summary(x)


y <- rrbeta(10000, 2, 0.55, 20, 250)
summary(y)


mean(x)
mean(x)
