
## Also here should make transformation for
# either luce, luce + scale, or scale


## Finally, lets look at trace (do this at end or separate file)
trace <- fread("~/dissertation/data/trace_n_things/trace_query.txt")
sims <- fread("~/dissertation/data/trace_n_things/trace_sims.txt")

## Ok here we are going to find the "default" sim
sims <- sims[phonemedecay == 0.03 & Spread == 6, ] # 135 to 110
sims <- sims[featuredecay == 0.01 & inputnoise == 0 & lexsize == 1, ] # 110 to 88
sims <- sims[gammaw == 0.03 & alphapw == 0.05, ] # 88 to 54
sims <- sims[alphafp == 0.02 & gammaF == 0.05, ] # 54 to 1

## Now take the remaining trace sim for use
tsims <- unique(trace$simulation)

## Get ones starting with N5
idx <- grep("^N5", tsims)
tsims <- tsims[idx]
trace <- trace[simulation %in% tsims, ]

## Take mean across simulations
trace[, `:=`(target = mean(targetact),
             cohort = mean(cohortact),
             rhyme = mean(rhymeact),
             ur = mean(uract)), by = .(frame)]

## Time is 16 * frame + 75
trace[, time := 16 * frame + 75]

## Remove unnecessary vars
trace[, `:=`(simulation = NULL, targetact = NULL,
             cohortact = NULL, rhymeact = NULL,
             uract = NULL, frame = NULL)]

## Unique only
trace <- unique(trace)
fwrite(trace, file = "~/dissertation/data/bob_trace_data/trace_curves.csv")

## Now let's set up the correct adjustments

###############
## TARGET #####
###############

scale_trace <- function(tau = 4.5) {
  trace <- fread("~/dissertation/data/bob_trace_data/trace_curves.csv")
  scaler <- function(a, b = 0, p = 1) { #a is max activation
    s <- 4
    w <- 0.0001
    cc <- 0.25
    den <- (1 + w * exp(-s * (a - cc)))^(1/w)
    (ss <- (p-b)/den + b)
  }

  # Range tau = 2-4.5
  lucer <- function(l, tau = 4.5) {
    expl <- lapply(l, function(x) {
      exp(tau * x)
    })
    ss <- Reduce(`+`, expl)
    rr <- lapply(expl, function(x) as.data.table(x / ss))
    rr
  }

  ## Implement luce
  l <- as.list(trace[, -"time", with = FALSE])
  trace_luce <- Reduce(`cbind`, (lucer(l, tau)))
  names(trace_luce) <- names(l)
  trace_luce <- cbind(trace[, .(time)], trace_luce)

  #trace_luce <- trace

  ## Compute scaling term, using both p/b and 0/1
  # for target specifically
  bb_t <- trace_luce[1, target] # first
  pp_t <- trace_luce[108, target] # last

  ## Once using the correct p/b
  trace_luce[, ss_targ := scaler(max(target, cohort, rhyme, ur), bb_t, pp_t), by = time]
  trace_luce[, ss_targ2 := scaler(max(target, cohort, rhyme, ur), 0, 1), by = time]

  trace_luce[, `:=`(targ_bp = target * ss_targ,
                    targ_nobp = target * ss_targ2)]

  ###############
  ## COHORT #####
  ###############

  bb_c <- trace_luce[1, cohort] # first
  pp_c <- trace_luce[108, cohort] # last

  ## Once using the correct p/b
  trace_luce[, ss_co := scaler(max(target, cohort, rhyme, ur), bb_c, pp_c), by = time]
  trace_luce[, ss_co2 := scaler(max(target, cohort, rhyme, ur), 0, 1), by = time]

  trace_luce[, `:=`(cohort_bp = cohort * ss_co,
                    cohort_nobp = cohort * ss_co2)]


  ## Get rid of intermediate pieces
  trace_luce[, `:=`(ss_targ = NULL,
                    ss_targ2 = NULL,
                    ss_co = NULL,
                    ss_co2 = NULL)]
  trace_luce
}

# This allows us to scale trace, neat
aa <- scale_trace(tau = 4.5)
bb <- scale_trace(tau = 3)

aa <- scale_trace(tau = 4.5)
bb <- scale_trace(tau = 3)
cc <- scale_trace(tau = 3.5)
dd <- scale_trace(tau = 4)
ee <- scale_trace(tau = 7)
fwrite(aa, "~/dissertation/data/bob_trace_data/trace_scaled_4.5.csv")
fwrite(bb, "~/dissertation/data/bob_trace_data/trace_scaled_3.csv")
fwrite(cc, "~/dissertation/data/bob_trace_data/trace_scaled_3.5.csv")
fwrite(dd, "~/dissertation/data/bob_trace_data/trace_scaled_4.csv")
fwrite(ee, "~/dissertation/data/bob_trace_data/trace_scaled_7.csv")


## But now try with sigmoid instead of fixed tau

scale_trace_sigmoid <- function(pp = NULL) {
  trace <- fread("~/dissertation/data/bob_trace_data/trace_curves.csv")
  scaler <- function(a, b = 0, p = 1) { #a is max activation
    s <- 4
    w <- 0.0001
    cc <- 0.25
    den <- (1 + w * exp(-s * (a - cc)))^(1/w)
    (ss <- (p-b)/den + b)
  }

  # Range tau = 2-4.5
  lucer <- function(l, ttt) {

    # temperature parmeters
    if (is.null(pp)) {
      temp_pars <- c(2, 4.5, 0.004, 1000)
    } else {
      temp_pars <- pp
    }

    temp <- eyetrackSim:::logistic_f(p = temp_pars, t = ttt)

    expl <- lapply(l, function(x) {
      exp(temp * x)
    })
    ss <- Reduce(`+`, expl)
    rr <- lapply(expl, function(x) as.data.table(x / ss))
    rr
  }

  ## Implement luce
  TRACE_TIME <- unique(trace$time)
  l <- as.list(trace[, -"time", with = FALSE])
  trace_luce <- Reduce(`cbind`, (lucer(l, TRACE_TIME)))
  names(trace_luce) <- names(l)
  trace_luce <- cbind(trace[, .(time)], trace_luce)

  #trace_luce <- trace

  ## Compute scaling term, using both p/b and 0/1
  # for target specifically
  bb_t <- trace_luce[1, target] # first
  pp_t <- trace_luce[108, target] # last

  ## Once using the correct p/b
  trace_luce[, ss_targ := scaler(max(target, cohort, rhyme, ur), bb_t, pp_t), by = time]
  trace_luce[, ss_targ2 := scaler(max(target, cohort, rhyme, ur), 0, 1), by = time]

  trace_luce[, `:=`(targ_bp = target * ss_targ,
                    targ_nobp = target * ss_targ2)]

  ###############
  ## COHORT #####
  ###############

  bb_c <- trace_luce[1, cohort] # first
  pp_c <- trace_luce[108, cohort] # last

  ## Once using the correct p/b
  trace_luce[, ss_co := scaler(max(target, cohort, rhyme, ur), bb_c, pp_c), by = time]
  trace_luce[, ss_co2 := scaler(max(target, cohort, rhyme, ur), 0, 1), by = time]

  trace_luce[, `:=`(cohort_bp = cohort * ss_co,
                    cohort_nobp = cohort * ss_co2)]


  ## Get rid of intermediate pieces
  trace_luce[, `:=`(ss_targ = NULL,
                    ss_targ2 = NULL,
                    ss_co = NULL,
                    ss_co2 = NULL)]
  trace_luce
}


aa <- scale_trace(tau = 4.5)
bb <- scale_trace(tau = 3)
xx <- scale_trace_sigmoid()

xx2 <- scale_trace_sigmoid(c(2, 4.5, 0.002, 600))

fwrite(xx, "~/dissertation/data/bob_trace_data/trace_sigmoid.csv")
fwrite(xx2, "~/dissertation/data/bob_trace_data/trace_sigmoid2.csv")
