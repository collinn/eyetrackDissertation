
library(bdots)
library(ggplot2)
library(gridExtra)

## On collin home machine
# mouse <- fread("~/packages/rat_data_tumr/hammond.csv")
# mm <- fread("~/packages/rat_data_tumr/SLB19-022.csv")
mouse <- fread("~/packages/rat_data_tumr/GJZ16-091.csv")

head(mouse)
tail(mouse)

mouse_fit <- bdotsFit(data = mouse,
        subject = "ID",
        time = "Day",
        y = "Volume",
        group = "Treatment",
        curveType = expCurve())


pdf("../img/mouse_fit.pdf", width = 6, height = 5)
rr <- plot(mouse_fit[1:4, ])
## Trying to fix this is for fucking retards im done
# rr[[1]] <- rr[[1]] + theme(base_size=22)
# rr[[2]] <- rr[[2]] + theme(base_size=22)
# rr[[3]] <- rr[[3]] + theme(base_size=22)
# rr[[4]] <- rr[[4]] + theme(base_size=22)
# grid.arrange(rr[[1]], rr[[2]],
#              rr[[3]], rr[[4]])
dev.off()

mouse_refit <- bdotsRefit(mouse_fit)

mouse_boot <- bdotsBoot(Volume ~ Treatment(A, E),
                         mouse_fit, permutation = TRUE)
summary(mouse_boot)
pdf("../img/mouse_boot_plot.pdf", width = 6, height = 5)
plot(mouse_boot)
dev.off()

pdf("../img/mouse_boot_plot_extra.pdf", width = 6, height = 5)
plot(mouse_boot, ciBands = FALSE, plotDiffs = FALSE)
dev.off()

mft <- copy(mouse_fit)
tt <- attr(mft, "time")
tt <- tt[tt < 60]
attributes(mft)$time <- tt

mouse_boot_t <- bdotsBoot(Volume ~ Treatment(A, E),
                        mft, permutation = TRUE)
summary(mouse_boot_t)
plot(mouse_boot_t)

summary(mouse_boot)
pdf("../img/mouse_boot_plot.pdf", width = 6, height = 5)
plot(mouse_boot_t)
dev.off()

pdf("../img/mouse_boot_plot_extra.pdf", width = 6, height = 5)
plot(mouse_boot_t, ciBands = FALSE, plotDiffs = FALSE)
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

res <- bdotsFit(data = df_cohort_unrelated,
                  subject = "subjectID",
                  time = "Time",
                  y = "AvgOfCohort",
                  group = c("Group", "trialcodecond"),
                  curveType = doubleGauss(concave = TRUE),
                  cor = TRUE,
                  numRefits = 2,
                  cores = 8,
                  verbose = FALSE)
bdotsRefit(res, fitCode = 3)


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

bdotsRefit(res, fitCode = 3)
