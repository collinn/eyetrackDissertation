library(bdots)
library(eyetrackSim)

#sigs <- readRDS(ff[16])

## This compute overall TIE across all time points
getFWER <- function(y) {
  sigs <- readRDS(y)

  sm <- sapply(sigs, function(x) !is.null(x[[1]])) |> mean()
  mm <- sapply(sigs, function(x) !is.null(x[[2]])) |> mean()
  pm <- sapply(sigs, function(x) !is.null(x[[3]])) |> mean()

  smt <- sapply(sigs, function(x) timetie(x[[1]])) |> rowMeans()
  mmt <- sapply(sigs, function(x) timetie(x[[2]])) |> rowMeans()
  pmt <- sapply(sigs, function(x) timetie(x[[3]])) |> rowMeans()

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

ff <- list.files(path="~/dissertation/writing/methodology/scripts/argon/rds_files", full.names = TRUE)
ff <- ff[c(1, 9:16, 2:8)]

ff <- list.files("~/dissertation/writing/methodology/scripts/argon/2000_rds_files", full.names = TRUE, pattern = "rds")
ff <- ff[c(1, 5:12, 2:4)]

ff <- list.files("~/dissertation/writing/methodology/scripts/argon/feb_rds_files", full.names = TRUE, pattern = "rds")
ff <- ff[c(1, 5:12, 2:4)]

ff <- list.files("~/dissertation/writing/methodology/scripts/argon/old_lmer_rds_files", full.names = TRUE)
ff <- ff[c(1, 5:12, 2:4)]

sds <- expand.grid(bdotscor = c(TRUE, FALSE),
                   ar1 = c(TRUE, FALSE),
                   manymeans = c(FALSE, TRUE),
                   paired = c(FALSE, TRUE))
sds <- as.data.table(sds)
sds <- sds[!(manymeans == FALSE & paired == TRUE), ]



gg <- lapply(ff, getFWER)
fwer <- rbindlist(lapply(gg, `[[`, 1))
tfwer <- timeSliceFwer(gg, median)

fwer <- as.data.table(cbind(sds, fwer))[order(rev(manymeans), ar1, decreasing = TRUE), ]
tfwer <- as.data.table(cbind(sds, tfwer))[order(rev(manymeans), ar1, decreasing = TRUE)]

fwer <- fwer[order(manymeans), ]
tfwer <- tfwer[order(manymeans), ]

fwers <- split(fwer, by = "paired")
tfwers <- split(tfwer, by = "paired")

# rev(paired)
library(xtable)
## Paired
fwers[[2]][, c(1,3:7)] |> xtable() |> print(include.rownames = FALSE)
# Unpaired
fwers[[1]][, c(1,3:7)] |> xtable() |> print(include.rownames = FALSE)

tfwers[[2]][, c(1,3:7)] |> xtable() |> print(include.rownames = FALSE)
tfwers[[1]][, c(1,3:7)] |> xtable() |> print(include.rownames = FALSE)

twfer |> xtable() |> print(include.rownames = FALSE)

