
## Taking what I know from bob, lets try to do this whole analysis with trace right here
# though there are likely a few ways this can go, we will choose the one that looks best
library(data.table)
library(ggplot2)
library(gridExtra)
library(bdots)
library(eyetrackSim)


## Relevant data
## These are cut off at <2000ms
looks <- fread("~/dissertation/data/bob_trace_data/human_looks_FINAL.csv")
sacs <- fread("~/dissertation/data/bob_trace_data/human_saccades_FINAL.csv")
sacs_inflate <- fread("~/dissertation/data/bob_trace_data/human_saccades_FINAL_INFLATE.csv")


if (file.exists("~/dissertation/data/saccade_look_fits/target_fits_bdots.RData")) {
  load("~/dissertation/data/saccade_look_fits/target_fits_bdots.RData")
} else {
  fit_looks <- bdotsFit(data = looks,
                        subject = "subject",
                        time = "time",
                        y = "target",
                        group = "group",
                        curveType = logistic(c(mini = 0, peak = 1, slope = 0.002, cross = 750)))

  fit_sacs  <- bdotsFit(data = sacs,
                        subject = "subject",
                        time = "StartTime",
                        y = "target",
                        group = "group",
                        curveType = logistic(c(mini = 0, peak = 1, slope = 0.002, cross = 750)))

  fit_sacs_inflate  <- bdotsFit(data = sacs_inflate,
                        subject = "subject",
                        time = "StartTime",
                        y = "target",
                        group = "group",
                        curveType = logistic(c(mini = 0, peak = 1, slope = 0.002, cross = 750)))

  save(fit_looks, fit_sacs, fit_sacs_inflate,
       file = "~/dissertation/data/saccade_look_fits/target_fits_bdots.RData")
}

## Actually I should remove all of the same subjects
# Remove these from looks
idxrm0 <- which(fit_looks$fitCode >= 5)

# Remove these from saccades
qq <- coef(fit_sacs)
idxrm1 <- which(qq[,3] < 0 | qq[,4] < 0 | qq[,1] > qq[,2])

qq <- coef(fit_sacs_inflate)
idxrm2 <- which(qq[,3] < 0 | qq[,4] < 0 | qq[,1] > qq[,2])

idxrm <- Reduce(union, list(idxrm0, idxrm1)) # no need for RT stuff here

idx <- setdiff(1:40, idxrm)

## This actually a bad idea, kinda, just reassign
fit_looks_sub <- fit_looks[idx, ]
fit_sacs_sub <- fit_sacs[idx, ]
fit_sacs_inflate_sub <- fit_sacs_inflate[idx, ]


mm1 <- colMeans(coef(fit_looks_sub))
mm2 <- colMeans(coef(fit_sacs_sub))
mm3 <- colMeans(coef(fit_sacs_inflate_sub))


time <- 0:1787
f_fix <- logistic_f(mm1, time)
f_sac <- logistic_f(mm2, time)
f_sac_inflate <- logistic_f(mm3, time)


## Pretty sure I figured out trace (4 looks best)
trace_luce <- fread("~/dissertation/data/bob_trace_data/trace_scaled_4.csv")
trace_sigmoid <- fread("~/dissertation/data/bob_trace_data/trace_sigmoid.csv")
trace_sigmoid2 <- fread("~/dissertation/data/bob_trace_data/trace_sigmoid2.csv") # best for saccade
trace_sigmoid3 <- fread("~/dissertation/data/bob_trace_data/trace_sigmoid3.csv") # best for fixation

## OR in ggplot
dt1 <- data.table(time = time, y = f_fix, Method = "Fixation")
dt2 <- data.table(time = time, y = f_sac, Method = "Saccade")
#dt3 <- data.table(time = trace_luce$time, y = trace_luce$targ_bp, Method = "TRACE")
#dt4 <- data.table(time = trace_luce$time, y = trace_sigmoid$targ_bp, Method = "TRACE 1")
dt5 <- data.table(time = trace_luce$time, y = trace_sigmoid2$targ_bp, Method = "TRACE 1")
dt7 <- data.table(time = trace_luce$time, y = trace_sigmoid3$targ_bp, Method = "TRACE 2")
dt6 <- data.table(time = time, y = f_sac_inflate, Method = "Saccade (Inflated)")



## Comparing the inflated saccade method
dt_sac_compare <- rbindlist(list(dt1, dt2, dt6))
pdf("../img/sac_inflate_compare.pdf",
    width = 5.5, height = 4)
ggplot(dt_sac_compare, aes(x = time, y = y, color = Method)) +
  geom_line(size=1) + ylim(c(min(dt_sac_compare$y),0.91)) + theme_bw() +
  ggtitle("Comparison of Saccade\nwith Inflated Endpoint") +
  labs(y = expression(f[theta](t)), x = "Time")
dev.off()

dt <- rbindlist(list(dt1, dt6,  dt5,  dt7))

pdf("../img/sac_fix_trace_compare.pdf",
    width = 5.5, height = 4)
ggplot(dt, aes(x = time, y = y, color = Method)) +
  geom_line() + ylim(c(min(dt$y),0.91)) + theme_bw() +
  ggtitle("Comparison of Fixation/Saccade\nMethods with TRACE") +
  labs(y = expression(f[theta](t)), x = "Time")
dev.off()

## Now let's show that two traces can look best
dt8 <- data.table(time = time, y = f_sac_inflate, Method = "Saccade")
dt <- rbindlist(list(dt1, dt8, dt5))
pdf("../img/sac_fix_trace_1.pdf",
    width = 6, height = 6)
ggplot(dt, aes(x = time, y = y, color = Method)) +
  geom_line(size = 1.5) + ylim(c(min(dt$y),0.91)) + theme_bw() +
  ggtitle("Comparison of Fixation/Saccade\nMethods with TRACE") +
  labs(y = expression(f[theta](t)), x = "Time") + theme(legend.position = "bottom")
dev.off()

dt <- rbindlist(list(dt1, dt8, dt7))
pdf("../img/sac_fix_trace_2.pdf",
     width = 6, height = 6)
ggplot(dt, aes(x = time, y = y, color = Method)) +
  geom_line(size = 1.5) + ylim(c(min(dt$y),0.91)) + theme_bw() +
  ggtitle("Comparison of Fixation/Saccade\nMethods with TRACE") +
  labs(y = expression(f[theta](t)), x = "Time") + theme(legend.position = "bottom")
dev.off()



## And now let's get some tables up in this bitch
library(xtable)


TRACE_fun <- approxfun(x = trace_luce$time,
                       y = trace_sigmoid2$targ_bp)
TRACE_fun_sigmoid <- approxfun(x = trace_sigmoid$time,
                       y = trace_sigmoid3$targ_bp)

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
rr <- mise(coef(fit_looks_sub), TRACE_fun)
ss <- mise(coef(fit_sacs_inflate_sub), TRACE_fun)


ss1 <- setnames(transpose(data.table(as.numeric(summary(rr)))), names(summary(rr)))
ss2 <- setnames(transpose(data.table(as.numeric(summary(ss)))), names(summary(ss)))

nn <- data.table(Method = c("Fixation", "Saccade"), TRACE = "TRACE1")
ss <- cbind(nn, rbindlist(list(ss1, ss2)))

print(xtable(ss, digits = 8), include.rownames = FALSE)


## And again with sigmoid
rr_sig <- mise(coef(fit_looks_sub), TRACE_fun_sigmoid)
ss_sig <- mise(coef(fit_sacs_inflate_sub), TRACE_fun_sigmoid)


ss1_sig <- setnames(transpose(data.table(as.numeric(summary(rr_sig)))), names(summary(rr_sig)))
ss2_sig <- setnames(transpose(data.table(as.numeric(summary(ss_sig)))), names(summary(ss_sig)))

nn_sig <- data.table(Method = c("Fixation", "Saccade"), TRACE = "TRACE2")
ss_sig <- cbind(nn_sig, rbindlist(list(ss1_sig, ss2_sig)))


print(xtable(rbind(ss, ss_sig), digits = 4), include.rownames = TRUE)


#########################################################################################
##### Weird case where old saccade really matches trace ################################3

trace_luce <- fread("~/dissertation/data/bob_trace_data/trace_scaled_4.csv")
trace_sigmoid <- fread("~/dissertation/data/bob_trace_data/trace_sigmoid.csv")
trace_sigmoid2 <- fread("~/dissertation/data/bob_trace_data/trace_sigmoid2.csv") # best for saccade
trace_sigmoid3 <- fread("~/dissertation/data/bob_trace_data/trace_sigmoid3.csv") # best for fixation



sac_old <- fread("~/dissertation/data/bob_trace_data/human_saccades_rt_nocut.csv")
sac_old$group <- "A"
fit_sacs_old  <- bdotsFit(data = sac_old,
                      subject = "subject",
                      time = "starttime",
                      y = "target",
                      group = "group",
                      curveType = logistic(c(mini = 0, peak = 1, slope = 0.002, cross = 750)))

qq <- coef(fit_sacs_old)
idxrm <- which(qq[,3] < 0 | qq[,4] < 0 | qq[,1] > qq[,2])
fit_sacs_old <- fit_sacs_old[setdiff(1:40, idxrm), ]

mm <- colMeans(coef(fit_sacs_old))

time <- 0:1787
f_sac_old <- logistic_f(mm, time)

dto <- data.table(time = time, y = f_sac_old, Method = "Unadjusted Saccade")
dtf <- data.table(time = time, y = f_fix, Method = "Fixation")
dtn <- data.table(time = trace_sigmoid$time, y = trace_luce$targ_bp, Method = "TRACE")
#dt2 <- data.table(time = trace_sigmoid$time, y = trace_luce$targ_nobp, Method = "TRACE no bp")
#dt1 <- data.table(time = trace_sigmoid$time, y = trace_sigmoid2$targ_bp, Method = "TRACE1")

dt <- rbindlist(list(dto, dtn, dtf))

pdf("../img/unadjusted_sac_w_trace.pdf",
    width = 5.5, height = 3)
ggplot(dt, aes(x = time, y = y, color = Method)) +
  geom_line(size = 1.5) + ylim(c(min(dt$y),0.91)) + theme_bw() +
  ggtitle("Unadjusted Saccade against TRACE") +
  labs(y = expression(f[theta](t)), x = "Time")
dev.off()
