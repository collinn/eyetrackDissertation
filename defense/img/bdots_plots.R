
library(bdots)
library(ggplot2)
library(gridExtra)

load("~/packages/bdots/data/ci.rda")
ci <- as.data.table(ci)
ci <- ci[LookType == "Target", ]
fit <- bdotsFit(data = ci,
                subject = "Subject",
                time = "Time",
                y = "Fixations",
                group = "protocol",
                curveType = logistic(),
                cor = TRUE,
                numRefits = 2)

## Just going to use this i guess
tt <- plot(fit[1:4, ])

ss <- 10
t1 <- tt[[1]] + theme_bw(base_size = ss)
t2 <- tt[[2]] + theme_bw(base_size = ss)
t3 <- tt[[3]] + theme_bw(base_size = ss)
t4 <- tt[[4]] + theme_bw(base_size = ss)

rr <- grid.arrange(t1, t2, t3, t4, ncol = 2)

boot <- bdotsBoot(formula = y ~ protocol(CI, NH),
                  bdObj = fit,
                  Niter = 250,
                  alpha = 0.05,
                  padj = "oleson",
                  cores = 7)

tt <- plot(boot, plotDiffs = FALSE)

library(ggplot2)
zz <- tt$bootPlot + theme_bw(base_size = 22)


grid.arrange(rr, zz)
grid.arrange(rr, zz, ncol = 2)
