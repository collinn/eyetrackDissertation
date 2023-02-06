
library(data.table)
library(bdots)
library(eyetrackSim)
library(mvtnorm)
library(ggplot2)

## These are probably wrong
#results <- readRDS("../../data/old_bdots_boot_coverage/coverage_results.rds")
#res_bdots <- readRDS("../../data/old_bdots_boot_coverage/bdots_coverage_results.rds")

# where do these come from?
results <- readRDS("~/dissertation/data/results.rds")
res_bdots <- readRDS("~/dissertation/data/bdots_coverage_results.rds")

pp <- lapply(results, `[[`, 1)
ppb <- lapply(res_bdots, `[[`, 1)
cc <- lapply(results, `[[`, 2)
ccb <- lapply(res_bdots, `[[`, 2)

## Parameters first, let's say 90 percent coverage
getParCoverage <- function(pp) {
  pp <- lapply(pp, function(x) {
    parCoverage <- apply(x, 2, function(y) sum(y > 0.05 & y < 0.95) / length(y))
  })

  mini <- lapply(pp, `[[`, 1) |> unlist()
  peak <- lapply(pp, `[[`, 2) |> unlist()
  slope <- lapply(pp, `[[`, 3) |> unlist()
  cross <- lapply(pp, `[[`, 4) |> unlist()

  return(list(mini = mini, peak = peak, slope = slope, cross = cross))
}



pp <- getParCoverage(pp)
ppb <- getParCoverage(ppb)

names(pp) <- c("Mini", "Peak", "Slope", "Crossover")
names(ppb) <- c("Mini", "Peak", "Slope", "Crossover")

## Make these bitches into dts
createDtfromPar <- function(p, simtype) {
  dtp <- data.table(val = unlist(p, use.names = FALSE))
  nn <- tstrsplit(names(unlist(p)), "\\.")
  dtp$par <- nn[[1]]
  tt <- vector("numeric", length = length(nn[[2]]))
  qq <- as.numeric(nn[[2]])
  for (i in seq_along(qq)) {
    tt[i] <- switch(qq[i], 10, 25, 50, 75, 100)
  }
  dtp$trial <- tt
  dtp$sim <- simtype
  dtp
}

dt1 <- createDtfromPar(pp, "bootstrap")
dt2 <- createDtfromPar(ppb, "bdots")
dt <- rbindlist(list(dt1, dt2))

png("img/par_coverage.png")
ggplot(data = dt, aes(x = trial, y = val, color = sim)) +
  geom_hline(yintercept = 0.9, linetype = "dashed", color = "red") +
  geom_line(size = 1.5, linetype = "solid") + geom_point(size=3) +
  facet_wrap(~par) + xlab("# Trials") + ylab("Coverage") +
  scale_y_continuous(breaks = seq(0, 1, by = 0.1)) +
  scale_x_continuous(breaks = c(10, 25, 50, 75, 100)) +
  ggtitle("Parameter Coverage") + theme_bw() + theme(legend.position = "bottom",
                                                     legend.text = element_text(size = 20),
                                                     legend.title = element_text(size = 20),
                                                     legend.key.width= unit(2, 'cm'),
                                                     axis.text=element_text(size = 15),
                                                     axis.title=element_text(size=15),
                                                     plot.title=element_text(size=25))
dev.off()

#### Average pointwise by trials
## This takes average across time points, then average across trials, then average total
cc_cover <- lapply(cc, function(x) {
  ## Here we just do full band for now
  mm <- apply(x, 1, function(y) sum(y > 0.05 & y < 0.95) / length(y))
  mean(mm)
}) |> unlist()

## Do this also for bdots, which only has binary indicators of coverage rather than quantile
cc_coverb <- lapply(ccb, function(x) {
  ## Here we just do full band for now
  mm <- apply(x, 1, function(y) mean(y))
  mean(mm)
}) |> unlist()


dt1 <- data.table(val = cc_cover,
                  trial = c(10, 25, 50, 75, 100),
                  Simulation = "bootstrap")

dt2 <- data.table(val = cc_coverb,
                  trial = c(10, 25, 50, 75, 100),
                  Simulation = "bdots")
dt <- rbindlist(list(dt1, dt2))

png("img/pointwise_cover.png")
ggplot(data = dt, aes(x = trial, y = val, color = Simulation)) +
  geom_hline(yintercept = 0.9, linetype = "dashed", color = "red") +
  geom_line(linetype = "solid", size=1.5) + geom_point(size = 3) +
  xlab("# Trials") + ylab("Coverage") +
  scale_y_continuous(breaks = seq(0, 1, by = 0.1)) +
  scale_x_continuous(breaks = c(10, 25, 50, 75, 100)) +
  ggtitle("Pointwise Coverage") + theme_bw() + theme(legend.position = "bottom",
                                                     legend.text = element_text(size = 20),
                                                     legend.title = element_text(size = 20),
                                                     legend.key.width= unit(2, 'cm'),
                                                     axis.text=element_text(size = 20),
                                                     axis.title=element_text(size=20),
                                                     plot.title=element_text(size=25))
dev.off()



