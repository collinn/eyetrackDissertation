
## This all desperately needs to be looked at by bob again


dat <- fread("../data/trace_n_things/Events-matched-sac-fix.txt")
dat2 <- fread("../data/trace_n_things/looks-matched_human.txt")


## These are things from trace that need to be scaled
trace <- fread("../data/trace_n_things/trace_query.txt")

## Make scaling based on bob's thing
scaler <- function(a) { #a is max activation
  s <- 4
  w <- 0.0001
  #w <- 1 # doesn't help really
  cc <- 0.25
  den <- 1 + w * exp(-s * (cc - a))^(1/w)
  (ss <- 1/den)
}

trace[, maxact := max(targetact, cohortact, rhymeact), by = .(simulation, frame)]
trace[, ss := scaler(maxact)]


############ Fuck forgot I had these
# granted, these are not the raw data and perhaps over transformed
trace <- fread("../data/bob_trace_data/trace_curves.csv")

dat <- fread("../data/bob_trace_data/human_looks.csv")

datn <- copy(dat)
datn[, `:=`(target = mean(target), cohort = mean(cohort)), by = time]
datn[, `:=`(subject = NULL)]
datn <- unique(datn)

trace[, time := 16*frame + 75]


## This clearly doesn't line up
plot(datn$time, datn$target)
lines(trace$time, trace$target)
