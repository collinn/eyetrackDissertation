
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
    seq(-1, 1, length.out = 401)

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
    ssm <- summary(vv[!(vv %in% c(-100,-200))])[c(1:3, 5:6)]

    ssm <- c(tie, t2e, pwr, ssm)
    names(ssm)[1:3] <- c("Type I Error","Type II Error", "Power")
    ssm
  }

  return(list(sm = makeSummary(smt),
              mm = makeSummary(mmt),
              pm = makeSummary(pmt)))
}

# original
ff <- list.files("negative_one_to_one_rds", full.names = TRUE)
ff <- list.files("1000_rds_files", full.names = TRUE)
ff <- ff[c(1, 5:12, 2:4)]
ff <- ff[10:12] # Getting rid of slope = 0.005, sig = 0.005

ff <- list.files("new_dist_rds_files", full.names = TRUE)
ff <- list.files("tue_new_dist_rds_files", full.names = TRUE)

res <- lapply(ff, getPowerTab)

res_sm <- lapply(res, function(z) setDT(as.list(z[[1]]))) |> rbindlist()
res_mm <- lapply(res, function(z) setDT(as.list(z[[2]]))) |> rbindlist()
res_pm <- lapply(res, function(z) setDT(as.list(z[[3]]))) |> rbindlist()


# Only doing power on these three things yay
simDataSettings <- data.table(mm = c(F, T, T),
                              ar = c(T, T, F))
#simDataSettings <- rbind(simDataSettings, simDataSettings)
#simDataSettings$sigVal <- rep(c(0.005, 0.025), each = 3)
#simDataSettings <- rbind(simDataSettings, simDataSettings)
#simDataSettings$slope <- rep(c(0.005, 0.025), each = 6)
#names(simDataSettings) <- c("manymeans", "ar1", "bdotscorr", "sigma", "m")
names(simDataSettings) <- c("manymeans", "ar1")


res_sm <- cbind(simDataSettings, res_sm)[order(manymeans, ar1), ]
res_mm <- cbind(simDataSettings, res_mm)[order(manymeans, ar1), ]
res_pm <- cbind(simDataSettings, res_pm)[order(manymeans, ar1), ]


res_sm[,3:10] <- round(res_sm[,3:10], 3)
res_mm[,3:10] <- round(res_mm[,3:10], 3)
res_pm[,3:10] <- round(res_pm[,3:10], 3)

res_sm <- res_sm[, c(1:5, 7:9)]
res_mm <- res_mm[, c(1:5, 7:9)]
res_pm <- res_pm[, c(1:5, 7:9)]

res_sm <- cbind(data.table(Method = "Hom. Boot"), res_sm)
res_mm <- cbind(data.table(Method = "Het. Boot"), res_mm)
res_pm <- cbind(data.table(Method = "Perm"), res_pm)

tab <- rbind(res_sm, res_mm, res_pm)
tab$manymeans <- ifelse(tab$manymeans, "Yes", "No")
tab$ar1 <- ifelse(tab$ar1, "Yes", "No")

digs <- c(1,1,1,1,2,2,2,3,3,3)
xtable(tab, caption = "Power for methods", align = c("llllcccccc"),
       label = "tab:power_methods", digits = digs) |> print(include.rownames = FALSE)


finalSummary <- rbind(colMeans(res_sm[, 4:9]),
                      colMeans(res_mm[, 4:9]),
                      colMeans(res_pm[, 4:9])) |> as.data.table()
finalSummary <- cbind(data.table(Method = c("Hom. Bootstrap", "Het. Bootstrap", "Permtuation")),
                      finalSummary)

xtable(finalSummary, caption = "Summary of methods for Type II error",
       label = "tab:type_2_summary", digits = 3) |> print(include.rownames = FALSE)

#
#
# ###############################33
# # Let's repeat the above and remove shitty case where sigma = 0.025 from
# # single means
#
# ff <- list.files("1000_rds_files", full.names = TRUE)
# ff <- ff[c(1, 5:12, 2:4)]
# ff <- ff[7:12] # Getting rid of slope = 0.005
# res <- lapply(ff, getPowerTab)
#
# res_sm <- lapply(res, function(z) setDT(as.list(z[[1]]))) |> rbindlist()
# res_mm <- lapply(res, function(z) setDT(as.list(z[[2]]))) |> rbindlist()
# res_pm <- lapply(res, function(z) setDT(as.list(z[[3]]))) |> rbindlist()
#
#
# # Only doing power on these three things yay
# simDataSettings <- data.table(mm = c(F, T, T),
#                               ar = c(T, T, F),
#                               bcor = c(T, F, F))
# simDataSettings <- rbind(simDataSettings, simDataSettings)
# simDataSettings$sigVal <- rep(c(0.005, 0.025), each = 3)
# names(simDataSettings) <- c("manymeans", "ar1", "bdotscorr", "sigma")
#
#
# res_sm <- cbind(simDataSettings, res_sm)[order(manymeans, ar1, bdotscorr, sigma), ]
# res_mm <- cbind(simDataSettings, res_mm)[order(manymeans, ar1, bdotscorr, sigma), ]
# res_pm <- cbind(simDataSettings, res_pm)[order(manymeans, ar1, bdotscorr, sigma), ]
#
#
# res_sm[,4:12] <- round(res_sm[,4:12], 3)
# res_mm[,4:12] <- round(res_mm[,4:12], 3)
# res_pm[,4:12] <- round(res_pm[,4:12], 3)
#
# ## so much dumb processing on this
# res_sm[, bdotscorr := NULL]
# res_mm[, bdotscorr := NULL]
# res_pm[, bdotscorr := NULL]
#
#
# # Optional
# res_sm[, `:=`(Min. = NULL, Max. = NULL)]
# res_mm[, `:=`(Min. = NULL, Max. = NULL)]
# res_pm[, `:=`(Min. = NULL, Max. = NULL)]
#
# digg <- c(1,1,1,3,2,2,2,3,3,3)
#
# xtable(res_sm, caption = "Power for v1 bootstrap",
#        label = "tab:bad_boot_pwr", digits = digg) |> print(include.rownames = FALSE)
#
#
# xtable(res_mm, caption = "Power for v2 bootstrap",
#        label = "tab:good_boot_pwr", digits = digg) |> print(include.rownames = FALSE)
# xtable(res_pm, caption = "Power for permutation",
#        label = "tab:perm_pwr", digits = digg) |> print(include.rownames = FALSE)
#
# finalSummary <- rbind(colMeans(res_sm[, 4:9]),
#                       colMeans(res_mm[, 4:9]),
#                       colMeans(res_pm[, 4:9])) |> as.data.table()
# finalSummary <- cbind(data.table(Method = c("Bootstrap V1", "Bootstrap V2", "Permtuation")),
#                       finalSummary)
#
# xtable(finalSummary, caption = "Summary of methods for Type II error",
#        label = "tab:type_2_summary", digits = 3) |> print(include.rownames = FALSE)
#
#
# bestCaseforOriginal <- cbind(data.table(method = rep(c("V1", "V2", "Perm"), each = 6)),
#                              rbind(res_sm, res_mm, res_pm))
# bestCaseforOriginal <- bestCaseforOriginal[manymeans == FALSE, ]
#
# fuckyoudigits <- c(1,1,1,1,3,2,2,2,3,3,3)
# xtable(bestCaseforOriginal,
#        digits = fuckyoudigits) |> print(include.rownames = FALSE)
