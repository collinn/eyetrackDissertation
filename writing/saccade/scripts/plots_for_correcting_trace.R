
library(ggplot2)

theme_set(theme_bw())

## Here is trace raw data
trace <- fread("~/dissertation/data/bob_trace_data/trace_curves.csv")

pdf("../img/TRACE_test/raw_trace.pdf",
    width = 5.5, height = 4)
ggplot(trace, aes(time, target)) + geom_line() + ylim(c(min(trace$target),1)) +
  ggtitle("Raw TRACE target data across\n14 trials") + ylab("target proportions")
dev.off()

scaler <- function(a, b = 0, p = 1) { #a is max activation
  s <- 4
  w <- 0.0001
  cc <- 0.25
  den <- (1 + w * exp(-s * (a - cc)))^(1/w)
  (ss <- (p-b)/den + b)
}

# Range tau = 2-4.5
lucer <- function(l, ttt, pp = NULL) {

  # temperature parmeters
  if (is.null(pp)) {
    temp_pars <- c(2, 4.5, 0.004, 1000)
  } else
    temp_pars <- pp
  }

  temp <- eyetrackSim:::logistic_f(p = temp_pars, t = trace$time)

  expl <- lapply(l, function(x) {
    exp(temp * x)
  })
  ss <- Reduce(`+`, expl)
  rr <- lapply(expl, function(x) as.data.table(x / ss))
  rr
}

## Implement luce
l <- as.list(trace[, -"time", with = FALSE])
trace_luce <- Reduce(`cbind`, (lucer(l)))
names(trace_luce) <- names(l)
trace_luce <- cbind(trace[, .(time)], trace_luce)

pdf("../img/TRACE_test/luce_choice.pdf",
    width = 5.5, height = 4)
ggplot(trace_luce, aes(time, target)) + geom_line() + ylim(0,1) +
  ggtitle("Luce Choice Rule Target\nusing sigmoidal temperature") + ylab("target proportions")
dev.off()

bb_t <- trace_luce[1, target] # first
pp_t <- trace_luce[108, target] # last

## Once using the correct p/b
trace_luce[, ss_targ := scaler(max(target, cohort, rhyme, ur), bb_t, pp_t), by = time]
trace_luce[, ss_targ2 := scaler(max(target, cohort, rhyme, ur), 0, 1), by = time]

trace_luce[, `:=`(targ_bp = target * ss_targ,
                  targ_nobp = target * ss_targ2)]


pdf("../img/TRACE_test/scaling_factor.pdf",
    width = 5.5, height = 4)
ggplot(trace_luce, aes(time, ss_targ)) + geom_line() + ylim(c(0,1)) +
  ggtitle("Scaling factor function") + ylab("target proportions")
dev.off()

pdf("../img/TRACE_test/scaling_times_luce.pdf",
    width = 5.5, height = 4)
ggplot(trace_luce, aes(time, targ_bp)) + geom_line() + ylim(c(0,1)) +
  ggtitle("Luce choice target times scaling factor") + ylab("target proportions")
dev.off()

test <- copy(trace)
test[, ss_targ := scaler(max(target, cohort, rhyme, ur), bb_t, pp_t), by = time]
test[, newtarg := target * ss_targ]

pdf("../img/TRACE_test/skipping_luce.pdf",
    width = 5.5, height = 4)
ggplot(test, aes(time, newtarg)) + geom_line() + ylim(c(min(test$newtarg),1)) +
  ggtitle("Original TRACE times scaling factor, no Luce") + ylab("target proportions")
dev.off()
