
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
