
library(bdots)

i <- 92
set.seed(i)
ts <- diffinv(rnorm(5))
rho <- bdots::ar1Solver(ts)
unadjp <- pt(ts, df = 10)
adjp <- p_adjust(unadjp, method = "oleson", df = 10, rho = rho, alpha = 0.05)

unadjp
adjp


spars <- structure(list(),
                   row.names = c(NA, -4L), class = c("data.table", "data.frame"))

spars <- data.table(param = c("mini", "peak", "slope", "cross"),
                    mean = c(0.115, 0.885, 0.0016, 765),
                    sd = c(0.12, 0.12, 0.00075, 85),
                    min = c(0, 0.5, 0.0009, 300),
                    max = c(0.3, 1, 0.01, 1100))
