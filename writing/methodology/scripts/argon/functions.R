library(bdots)
library(eyetrackSim)

#sigs <- readRDS(ff[16])

## This compute overall TIE across all time points
getFWER <- function(y) {
  sigs <- readRDS(y)

  sm <- sapply(sigs, function(x) !is.null(x[[1]])) |> mean()
  mm <- sapply(sigs, function(x) !is.null(x[[2]])) |> mean()
  pm <- sapply(sigs, function(x) !is.null(x[[3]])) |> mean()
  #pmm <- sapply(sigs, function(x) !is.null(x[[4]])) |> mean()

  smt <- sapply(sigs, function(x) timetie(x[[1]])) |> rowMeans()
  mmt <- sapply(sigs, function(x) timetie(x[[2]])) |> rowMeans()
  pmt <- sapply(sigs, function(x) timetie(x[[3]])) |> rowMeans()
  #pmtm <- sapply(sigs, function(x) timetie(x[[4]])) |> rowMeans()

  fwer <- data.table(sm = sm, mm = mm, pm = pm)
  tsmat <- matrix(c(smt, mmt, pmt), byrow = TRUE, nrow = 3,
                  dimnames = list(c("sm", "mm", "pm"), NULL))
  return(list(fwer = fwer, timeSliceMat = tsmat))
}

## This takes the list returned by getfwer
timeSliceFwer <- function(y, f = median) {
  nn <- length(y)
  y <- lapply(y, `[[`, 2)
  y <- lapply(y, function(z) {
    apply(z, 1, f)
  })
  y <- Reduce(rbind, y)
  #y <- sapply(y, rowMeans)
  #rownames(y) <- paste0("sim", 1:8)
  y
}

## Given signifiance mat, need to return length 401 bool vector
timetie <- function(mm) {
  time <- seq(0, 1600, 4)
  vec <- vector("numeric", length = length(time))

  if (is.null(dim(mm))) {
    vec <- as.logical(vec)
    return(vec)
  }

  # each of these into a list
  sm <- split(mm, row(mm))
  bv <- lapply(sm, function(m) {
    sigt <- do.call(seq, as.list(c(m, 4)))
  })
  bv <- Reduce(union, bv)
  rr <- time %in% bv
  return(rr)
}

##########3---------------------------------------------------------

ff <- list.files("~/dissertation/writing/methodology/scripts/argon/40_rds_files", full.names = TRUE)

ff <- list.files("~/dissertation/writing/methodology/scripts/argon/rds_files", full.names = TRUE)

ff <- ff[c(1, 9:16, 2:8)]

## Sim settings
sds <- expand.grid(bdotscor = c(TRUE, FALSE),
                   ar1 = c(TRUE, FALSE),
                   manymeans = c(FALSE, TRUE),
                   paired = c(FALSE, TRUE),
                   pairedType = 1)
sds <- as.data.table(sds)
sds <- sds[!(manymeans == FALSE & paired == TRUE), ]
dat <- sds[paired == TRUE, ]
dat1 <- copy(dat)
dat1$pairedType <- 2
sds <- rbind(sds, dat1)


gg <- lapply(ff, getFWER)
fwer <- rbindlist(lapply(gg, `[[`, 1))
tfwer <- timeSliceFwer(gg, median)

fwer <- as.data.table(cbind(sds, fwer))[order(rev(manymeans), ar1, decreasing = TRUE), ]
tfwer <- as.data.table(cbind(sds, tfwer))[order(rev(manymeans), ar1, decreasing = TRUE)]

fwer <- fwer[order(manymeans), ]
tfwer <- tfwer[order(manymeans), ]

fwer[, `:=`(bdotscor = ifelse(bdotscor, "Yes", "No"),
            ar1 = ifelse(ar1, "Yes", "No"),
            manymeans = ifelse(manymeans, "Yes", "No"),
            paired = ifelse(paired, "Yes", "No"))]

tfwer[, `:=`(bdotscor = ifelse(bdotscor, "Yes", "No"),
            ar1 = ifelse(ar1, "Yes", "No"),
            manymeans = ifelse(manymeans, "Yes", "No"),
            paired = ifelse(paired, "Yes", "No"))]

fwer <- fwer[, .(manymeans, ar1, bdotscor, paired, pairedType, sm, mm, pm)]
tfwer <- tfwer[, .(manymeans, ar1, bdotscor, paired, pairedType, sm, mm, pm)]

fwers <- split(fwer, by = c("paired", "pairedType"))
tfwers <- split(tfwer, by = c("paired", "pairedType"))

# rev(paired)
library(xtable)
## unpaired
fwers[[1]][, c(1:3,6:8)] |> xtable() |> print(include.rownames = FALSE)
# Paired #1
fwers[[2]][, c(1:3,6:8)] |> xtable() |> print(include.rownames = FALSE)
fwers[[3]][, c(1:3,6:8)] |> xtable() |> print(include.rownames = FALSE)

tfwers[[1]][, c(1:3,6:8)] |> xtable() |> print(include.rownames = FALSE)
tfwers[[2]][, c(1:3,6:8)] |> xtable() |> print(include.rownames = FALSE)
tfwers[[3]][, c(1:3,6:8)] |> xtable() |> print(include.rownames = FALSE)

twfer |> xtable() |> print(include.rownames = FALSE)

