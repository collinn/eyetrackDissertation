
library(bdots)
library(ggplot2)
library(gridExtra)

## On collin home machine
# mouse <- fread("~/packages/rat_data_tumr/hammond.csv")
# mm <- fread("~/packages/rat_data_tumr/SLB19-022.csv")
m2 <- fread("~/packages/rat_data_tumr/GJZ16-091.csv")

tt <- bdotsFit(data = m2,
        subject = "ID",
        time = "Day",
        y = "Volume",
        group = "Treatment",
        curveType = expCurve())


png("../img/mouse.png")
rr <- plot(tt[1:4, ])
## Trying to fix this is for fucking retards im done
# rr[[1]] <- rr[[1]] + theme(base_size=22)
# rr[[2]] <- rr[[2]] + theme(base_size=22)
# rr[[3]] <- rr[[3]] + theme(base_size=22)
# rr[[4]] <- rr[[4]] + theme(base_size=22)
# grid.arrange(rr[[1]], rr[[2]],
#              rr[[3]], rr[[4]])
dev.off()


## Let's do some bdots shit
# load("~/packages/bdots/data/ci.rda")
# ci <- as.data.table(ci)
# ci <- ci[LookType == "Target", ]
# res <- bdotsFit(data = ci,
#                 subject = "Subject",
#                 time = "Time",
#                 y = "Fixations",
#                 group = "protocol",
#                 curveType = logistic(),
#                 cor = TRUE,
#                 numRefits = 2)
#
# res[which.min(res$R2), ]$fitCode <- 1
# res$fitCode <- 1
# bdotsRefit(res)

res <- bdotsFit(data = cohort_unrelated,
                  subject = "Subject",
                  time = "Time",
                  y = "Fixations",
                  group = c("Group", "LookType"),
                  curveType = doubleGauss(concave = TRUE),
                  cor = TRUE,
                  numRefits = 2,
                  cores = 8,
                  verbose = FALSE)
bdotsRefit(res, fitCode = 4)

 # not used
res2 <- bdotsFit(data = df_cohort_unrelated,
                subject = "subjectID",
                time = "Time",
                y = "AvgOfCohort",
                group = c("Group", "trialcodecond"),
                curveType = doubleGauss(concave = TRUE),
                cor = TRUE,
                numRefits = 2,
                cores = 8,
                verbose = FALSE)
bdotsRefit(res2, fitCode = 4)


### OK here we do things with the fake car data
library(bdots)
fit <- readRDS("eightgrpfit.rds")

png("../img/car_fit.png", width = 480*1.4)
plot(fit[1:4, ])
dev.off()


boot <- bdotsBoot(y ~ Vehicle(car, truck) + Origin(domestic), fit)
png("../img/car_boot_both.png", width = 480*1.4)
plot(boot)
dev.off()


png("../img/car_boot_single.png", width = 480*1.4)
plot(boot, ciBands = FALSE, plotDiffs = FALSE)
dev.off()
