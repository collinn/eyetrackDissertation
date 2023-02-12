
library(bdots)
library(eyetrackSim)
library(xtable)

getPowerTab <- function(ff) {
  rr <- readRDS(ff)

  sm <- lapply(rr, `[[`, 1)
  mm <- lapply(rr, `[[`, 2)
  pm <- lapply(rr, `[[`, 3)

  powerdetector <- function(mm) {

    if (is.null(mm)) return(-200)
    time <- seq(-2, 2, length.out = 501)

    vec <- vector("numeric", length = length(time))

    # each of these into a list
    sm <- split(mm, row(mm))

    if (length(sm) == 2) {
      return(-100)
    } else if (length(sm) == 1) {
      res <- min(sm[[1]])
      if (res < 0) {
        return(-100)
      } else {
        return(res)
      }
    } else {
      stop("WHAT THE FUCK?????")
    }
  }



  smt <- vapply(sm, powerdetector, 1) #|> table()
  mmt <- vapply(mm, powerdetector, 1) #|> table()
  pmt <- vapply(pm, powerdetector, 1) #|> table()

  makeSummary <- function(vv) {
    tie <- mean(vv==-100)
    t2e <- mean(vv==-200)
    pwr <- 1 - mean(vv==-200 | vv==-100)
    ssm <- summary(vv[!(vv %in% c(-100,-200))])

    ssm <- c(tie, t2e, pwr, ssm)
    names(ssm)[1:3] <- c("Type I Error","Type II Error", "Power")
    ssm
  }

  return(list(sm = makeSummary(smt),
              mm = makeSummary(mmt),
              pm = makeSummary(pmt)))
}

# original fuck ups
ff <- list.files("rds_files", full.names = TRUE)
ff <- ff[c(1, 5:12, 2:4)]
res <- lapply(ff, getPowerTab)

res_sm <- lapply(res, function(z) setDT(as.list(z[[1]]))) |> rbindlist()
res_mm <- lapply(res, function(z) setDT(as.list(z[[2]]))) |> rbindlist()
res_pm <- lapply(res, function(z) setDT(as.list(z[[3]]))) |> rbindlist()


# Only doing power on these three things yay
simDataSettings <- data.table(mm = c(F, T, T),
                              ar = c(T, T, F),
                              bcor = c(T, F, F))
simDataSettings <- rbind(simDataSettings, simDataSettings)
simDataSettings$sigVal <- rep(c(0.005, 0.025), each = 3)
simDataSettings <- rbind(simDataSettings, simDataSettings)
simDataSettings$slope <- rep(c(0.005, 0.025), each = 6)
names(simDataSettings) <- c("manymeans", "ar1", "bdotscorr", "sigma", "slope")

#simDataSettings <- simDataSettings[1:6, ]

#neword <- c(1,4,2,5,3,6)
#neword <- 1:3 # for other cases
neword <- c(1,7,2,8,3,9,4,10,5,11,6,12)
#neword <- 1:6

res_sm <- cbind(simDataSettings, res_sm)[neword, ]
res_mm <- cbind(simDataSettings, res_mm)[neword, ]
res_pm <- cbind(simDataSettings, res_pm)[neword, ]

# res_sm <- cbind(simDataSettings[2:3, ], res_sm)
# res_mm <- cbind(simDataSettings[2:3, ], res_mm)
# res_pm <- cbind(simDataSettings[2:3, ], res_pm)


xtable(res_sm, caption = "Power for bad bootstrap",
       label = "tab:bad_boot_pwr2", digits = 4) |> print(include.rownames = FALSE)
xtable(res_mm, caption = "Power for good bootstrap",
       label = "tab:good_boot_pwr2", digits = 4) |> print(include.rownames = FALSE)
xtable(res_pm, caption = "Power for permutation",
       label = "tab:perm_pwr2", digits = 4) |> print(include.rownames = FALSE)
