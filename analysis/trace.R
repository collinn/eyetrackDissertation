
## This all desperately needs to be looked at by bob again


# dat <- fread("../data/trace_n_things/Events-matched-sac-fix.txt")
# dat2 <- fread("../data/trace_n_things/looks-matched_human.txt")
# 
# 
# ## These are things from trace that need to be scaled
# trace <- fread("../data/trace_n_things/trace_query.txt")

looks <- fread("../data/bob_trace_data/human_looks.csv")

## Make scaling based on bob's thing
scaler <- function(a) { #a is max activation
  s <- 4
  w <- 0.0001
  #w <- 1 # doesn't help really
  cc <- 0.25
  den <- 1 + w * exp(-s * (cc - a))^(1/w)
  (ss <- 1/den)
}

## Across subjects
looks_s <- looks[, `:=`(unrelated = mean(unrelated), 
                        target = mean(target), 
                         cohort = mean(cohort), 
                        rhyme = mean(rhyme)), by = time]
looks_s[, `:=`(unrel = NULL, subject = NULL)]
looks_s <- unique(looks_s)


l <- as.list(looks_s[, .(unrelated, target, cohort, rhyme)])
lucer <- function(l, tau = 3) {
  expl <- lapply(l, function(x) {
    exp(tau * x)
  })
  ss <- Reduce(`+`, expl)
  rr <- lapply(expl, function(x) as.data.table(x / ss))
  setNames(rr, l)
}

## With luce choice rule
look_luce <- Reduce(`cbind`, (lucer(l)))
names(look_luce) <- names(l)
look_luce$time <- looks_s$time

## Get max activation
look_luce[, maxact := max(unrelated, target, cohort, rhyme), by = time]
look_luce[, ss := scaler(maxact), by = time]






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
