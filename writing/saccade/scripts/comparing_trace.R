
## Taking what I know from bob, lets try to do this whole analysis with trace right here
# though there are likely a few ways this can go, we will choose the one that looks best
library(data.table)
library(ggplot2)
library(gridExtra)
library(bdots)
library(eyetrackSim)

oldwd <- setwd("~/dissertation/analysis/")
## Relevant data
# looksrt <- fread("../data/bob_trace_data/human_looks_rt_cut.csv")
# sacsrt <- fread("../data/bob_trace_data/human_saccades_rt_cut.csv")

looks <- fread("../data/bob_trace_data/human_looks_rt_nocut.csv")
sacs <- fread("../data/bob_trace_data/human_saccades_rt_nocut.csv")
setwd(oldwd) # reset dir

## Should consider making group niot necessary in bdots
# looksrt[, group := "A"]
# sacsrt[, group := "A"]
looks[, group := "A"]
sacs[, group := "A"]



#sacs <- sacs[starttime > 0, ]


if (file.exists("~/dissertation/data/saccade_look_fits/target_fits_bdots.RData")) {
  load("~/dissertation/data/saccade_look_fits/target_fits_bdots.RData")
} else {
  fit_looks <- bdotsFit(data = looks,
                        subject = "subject",
                        time = "time",
                        y = "target",
                        group = "group",
                        curveType = logistic())
#
#   fit_looks_rt <- bdotsFit(data = looksrt,
#                          subject = "subject",
#                          time = "time",
#                          y = "target",
#                          group = "group",
#                          curveType = logistic())

  #sacs <- sacs[starttime <= 2000, ]
  ## logistic2() at bottom of this script
  fit_sacs  <- bdotsFit(data = sacs,
                        subject = "subject",
                        time = "starttime",
                        y = "target",
                        group = "group",
                        curveType = logistic(c(mini = 0, peak = 1, slope = 0.002, cross = 750)))
  # fit_sacs_rt  <- bdotsFit(data = sacsrt,
  #                        subject = "subject",
  #                        time = "starttime",
  #                        y = "target",
  #                        group = "group",
  #                        curveType = logistic(c(mini = 0, peak = 1, slope = 0.002, cross = 750)))

  # save(fit_looks, fit_looks_rt, fit_sacs, fit_sacs_rt,
  #      file = "~/dissertation/data/saccade_look_fits/target_fits_bdots.RData")
  save(fit_looks, fit_sacs,
       file = "~/dissertation/data/saccade_look_fits/target_fits_bdots.RData")
}

## Actually I should remove all of the same subjects
## But not going to compare those with rt removed bewcause nonstandard

# Remove these from looks
idxrm0 <- which(fit_looks$fitCode >= 5)
#idxrm1 <- which(fit_looks_rt$fitCode >= 5)

# Remove these from saccades
qq <- coef(fit_sacs)
idxrm2 <- which(qq[,3] < 0 | qq[,4] < 0 | qq[,1] > qq[,2])

## Remove these from  saccades rt
#qq <- coef(fit_sacs_rt)
#idxrm3 <- which(qq[,3] < 0 | qq[,4] < 0 | qq[,1] > qq[,2])

#idxrm <- Reduce(union, list(idxrm0, idxrm1, idxrm2, idxrm3))
idxrm <- Reduce(union, list(idxrm0, idxrm2)) # no need for RT stuff here

idx <- setdiff(1:40, idxrm)

fit_looks <- fit_looks[idx, ]
#fit_looks_rt <- fit_looks_rt[idx, ]

fit_sacs <- fit_sacs[idx, ]
#fit_sacs_rt <- fit_sacs_rt[idx, ]

mm1 <- colMeans(coef(fit_looks))
#mm2 <- colMeans(coef(fit_looks_rt))
mm3 <- colMeans(coef(fit_sacs))
#mm4 <- colMeans(coef(fit_sacs_rt))

time <- 0:1787
f_fix <- logistic_f(mm1, time)
#f_fix_rt <- logistic_f(mm2, time)
f_sac <- logistic_f(mm3, time)
#f_sac_rt <- logistic_f(mm4, time)

## Pretty sure I figured out trace (4 looks best)
#trace_luce <- fread("~/dissertation/data/bob_trace_data/trace_scaled_4.5.csv")
#trace_luce <- fread("~/dissertation/data/bob_trace_data/trace_scaled_3.csv")
#trace_luce <- fread("~/dissertation/data/bob_trace_data/trace_scaled_3.5.csv")
trace_luce <- fread("~/dissertation/data/bob_trace_data/trace_scaled_4.csv")
#trace_luce <- fread("~/dissertation/data/bob_trace_data/trace_scaled_7.csv")
trace_sigmoid <- fread("~/dissertation/data/bob_trace_data/trace_sigmoid.csv")
trace_sigmoid2 <- fread("~/dissertation/data/bob_trace_data/trace_sigmoid2.csv")

## OR in ggplot
dt1 <- data.table(time = time, y = f_fix, Method = "Fixation")
dt2 <- data.table(time = time, y = f_sac, Method = "Saccade")
# dt5 <- data.table(time = time, y = f_fix_rt, Method = "Fixation_RT")
# dt6 <- data.table(time = time, y = f_sac_rt, Method = "Saccade_RT")
dt3 <- data.table(time = trace_luce$time, y = trace_luce$targ_bp, Method = "TRACE")
dt4 <- data.table(time = trace_luce$time, y = trace_sigmoid$targ_bp, Method = "trace_sigmoid")
dt5 <- data.table(time = trace_luce$time, y = trace_sigmoid2$targ_bp, Method = "trace_sigmoid2")

dt <- rbindlist(list(dt1, dt2, dt3, dt4, dt5))

pdf("../img/sac_fix_trace_compare.pdf",
    width = 5.5, height = 4)
ggplot(dt, aes(x = time, y = y, color = Method)) +
  geom_line() + ylim(c(min(dt1$y),1)) + theme_bw() +
  ggtitle("Comparison of Fixation/Saccade\nMethods with TRACE") +
  labs(y = expression(f[theta](t)), x = "Time")
dev.off()


## And now let's get some tables up in this bitch
library(xtable)


TRACE_fun <- approxfun(x = trace_luce$time,
                       y = trace_luce$targ_bp)
TRACE_fun_sigmoid <- approxfun(x = trace_sigmoid$time,
                       y = trace_sigmoid2$targ_bp)

getRmvIdx <- function(ff) {
  rr <- coef(ff)
  idx <- which(rr[,4] == 0 | rr[,2] < rr[,1] | rr[,3] < 0)
  idx
}

mise <- function(pars, f, RMS = TRUE) {
  times <- seq(75, 1787, by = 10)

  pars <- split(pars, 1:nrow(pars))
  mv <- vector("numeric", length = length(pars))

  gg <- function(p) {
    g <- function(tt) {
      (logistic_f(p, tt) - f(tt))^2
    }
  }

  if (!RMS) {
    for (i in seq_along(mv)) {
      g <- gg(pars[[i]])
      mv[i] <- tryCatch({integrate(g, lower = min(times), upper = max(times))$value}, error = function(e) NA)
    }
  } else {
    for (i in seq_along(mv)) {
      g <- gg(pars[[i]])
      mv[i] <- sqrt(sum(g(times))/length(mv))
    }
  }
  mv
}

## Do just mean first
rr <- mise(coef(fit_looks), TRACE_fun)
ss <- mise(coef(fit_sacs), TRACE_fun)


ss1 <- setnames(transpose(data.table(as.numeric(summary(rr)))), names(summary(rr)))
ss2 <- setnames(transpose(data.table(as.numeric(summary(ss)))), names(summary(ss)))

nn <- data.table(Method = c("Fixation", "Saccade"))
ss <- cbind(nn, rbindlist(list(ss1, ss2)))

print(xtable(ss, digits = 8), include.rownames = FALSE)


## And again with sigmoid
rr_sig <- mise(coef(fit_looks), TRACE_fun_sigmoid)
ss_sig <- mise(coef(fit_sacs), TRACE_fun_sigmoid)


ss1_sig <- setnames(transpose(data.table(as.numeric(summary(rr_sig)))), names(summary(rr_sig)))
ss2_sig <- setnames(transpose(data.table(as.numeric(summary(ss_sig)))), names(summary(ss_sig)))

nn_sig <- data.table(Method = c("Fixation (Sigmoid)", "Saccade (Sigmoid)"))
ss_sig <- cbind(nn_sig, rbindlist(list(ss1_sig, ss2_sig)))





