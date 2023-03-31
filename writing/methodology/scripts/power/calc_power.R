
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
ff <- list.files("rds_files", full.names = TRUE, pattern = "rds")
ff <- ff[c(1, 3:10, 2)]

# this is slope 0.025
ff <- ff[1:5]

## For now, only doing slope = 0.25
ff <- ff[6:10]

res <- lapply(ff, getPowerTab)

res_sm <- lapply(res, function(z) setDT(as.list(z[[1]]))) |> rbindlist()
res_mm <- lapply(res, function(z) setDT(as.list(z[[2]]))) |> rbindlist()
res_pm <- lapply(res, function(z) setDT(as.list(z[[3]]))) |> rbindlist()


# Only doing power on these three things yay
sds <- data.table(mm = rep(c(F, T, T, T, T), 1),
                  ar = rep(c(T, T, T, F, F), 1),
                  bcor = rep(c(T, T, F, T, F), 1))#,
                  #sigVal = 0.05,
                  #slope = rep(c(0.025, 0.25), each = 5))

res_sm <- cbind(sds, res_sm)[order(mm, ar), ]
res_mm <- cbind(sds, res_mm)[order(mm, ar), ]
res_pm <- cbind(sds, res_pm)[order(mm, ar), ]

res_sm <- res_sm[, c(1:6, 8:10)]
res_mm <- res_mm[, c(1:6, 8:10)]
res_pm <- res_pm[, c(1:6, 8:10)]

res_sm[, 4:9] <- round(res_sm[, 4:9], 3)
res_mm[, 4:9] <- round(res_mm[, 4:9], 3)
res_pm[, 4:9] <- round(res_pm[, 4:9], 3)

res_sm <- cbind(data.table(Method = "Hom. Boot"), res_sm)
res_mm <- cbind(data.table(Method = "Het. Boot"), res_mm)
res_pm <- cbind(data.table(Method = "Perm"), res_pm)

tab <- rbind(res_sm, res_mm, res_pm)
tab$mm <- ifelse(tab$mm, "Yes", "No")
tab$ar <- ifelse(tab$ar, "Yes", "No")
tab$bcor <- ifelse(tab$bcor, "Yes", "No")

tab <- tab[order(mm, ar, bcor), ]

digs <- c(1,1,1,1,1,2,2,2,3,3,3)
xtable(tab, caption = "Power for methods", align = c("lllllcccccc"),
       label = "tab:power_methods", digits = digs) |> print(include.rownames = FALSE)


finalSummary <- rbind(colMeans(res_sm[, 5:10]),
                      colMeans(res_mm[, 5:10]),
                      colMeans(res_pm[, 5:10])) |> as.data.table()
finalSummary <- cbind(data.table(Method = c("Hom. Bootstrap", "Het. Bootstrap", "Permtuation")),
                      finalSummary)

xtable(finalSummary, caption = "Summary of methods for Type II error",
       label = "tab:type_2_summary", digits = 3) |> print(include.rownames = FALSE)


### Abbreviated version (0.25 only, but also only including NO bcor)
tab_abr <- tab[c(1:6, 10:12), ]
tab_abr$bcor <- NULL

digs <- c(1,1,1,1,2,2,2,3,3,3)
xtable(tab_abr, caption = "Power for methods", align = c("llllcccccc"),
       label = "tab:power_methods", digits = digs) |> print(include.rownames = FALSE)


finalSummary <- rbind(colMeans(res_sm[c(1,3,5), 5:10]),
                      colMeans(res_mm[c(1,3,5), 5:10]),
                      colMeans(res_pm[c(1,3,5), 5:10])) |> as.data.table()
finalSummary <- cbind(data.table(Method = c("Hom. Bootstrap", "Het. Bootstrap", "Permtuation")),
                      finalSummary)

xtable(finalSummary, caption = "Summary of methods for Type II error",
       label = "tab:type_2_summary", digits = 3) |> print(include.rownames = FALSE)
