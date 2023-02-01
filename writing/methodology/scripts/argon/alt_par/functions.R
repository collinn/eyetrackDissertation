
## This compute overall TIE across all time points
getFWER <- function(y) {
  tt <- readRDS(y)
  sigs <- lapply(tt,  function(y) {
    y <- lapply(y, `[[`, 1)
  })

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
timeSliceFwer <- function(y, f = mean) {
  y <- lapply(y, `[[`, 2)
  y <- lapply(y, function(z) {
    apply(z, 1, f)
  })
  y <- Reduce(rbind, y)
  #y <- sapply(y, rowMeans)
  rownames(y) <- paste0("sim", 1:8)
  y
}

fwertotal <- rbindlist(lapply(gg, `[[`, 1))
meant <- timeSliceFwer(gg, mean)
mediant <- timeSliceFwer(gg, median)

ff <- list.files(path = "~/dissertation/writing/methodology/scripts/argon/alt_par/rds_boot/", pattern = "rds", full.names = TRUE)
ff <- list.files(path = "~/dissertation/writing/methodology/scripts/argon/rds_boot/", pattern = "rds", full.names = TRUE)

# simDataSettings <- expand.grid(manymeans = c(TRUE, FALSE),
#                                paired = c(TRUE, FALSE),
#                                ar1 = c(TRUE, FALSE),
#                                bdotscor = c(TRUE, FALSE))


gg <- lapply(ff, getFWER)
fwer <- rbindlist(lapply(gg, `[[`, 1))
tfwer <- timeSliceFwer(gg, median)

fwer <- cbind(simDataSettings, fwer)
as.data.table(fwer)[order(rev(paired), manymeans, ar1, decreasing = TRUE), ] |> xtable() |> print(include.rownames = FALSE)
as.data.table(cbind(simDataSettings, tfwer))[order(manymeans, ar1, decreasing = TRUE)] |> xtable() |> print(include.rownames = FALSE)




tt <- readRDS(ff[[1]])
sigs <- lapply(tt,  function(y) {
  y <- lapply(y, `[[`, 1)
})

## Need to be able to turn these into 0:1600 boolean
mmat <- sigs[[2]][[1]]
mmat2 <- sigs[[1]][[1]]
mm3 <- sigs[[2]][[2]]

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

