

## Goal here is to make plot in time showing percentage of times flagged diff
ff <- list.files("rds_files", full.names = TRUE)
ff <- ff[c(1, 3:10, 2)]

rr <- readRDS(ff[1])

mm <- rr[[1]][[1]]

## Given signifiance mat, need to return length 401 bool vector
timetiePower <- function(mm) {
  time <- seq(-1, 1, length.out = 401)
  vec <- vector("numeric", length = length(time))

  if (is.null(dim(mm))) {
    vec <- as.logical(vec)
    return(vec)
  }

  # each of these into a list
  sm <- split(mm, row(mm))
  bv <- lapply(sm, function(m) {
    sigt <- do.call(seq, as.list(c(m, by = 0.005)))
  })
  bv <- Reduce(union, bv)
  rr <- round(time,3) %in% round(bv, 3)
  return(rr)
}

getDiffSlices <- funtion(ff) {
  rr <- readRDS(ff)

  sm <- lapply(rr, `[[`, 1)
  mm <- lapply(rr, `[[`, 2)
  pm <- lapply(rr, `[[`, 3)

  smm <- sapply(sm, timetiePower) |> rowSums()
  mmm <- sapply(mm, timetiePower) |> rowSums()
  pmm <- sapply(pm, timetiePower) |> rowSums()
  time <- seq(-1, 1, length.out = 401)
  plot(time, smm, type = 'l', col = 'black')
  lines(time, mmm, type = 'l', col = 'red')
  lines(time, pmm, type = 'l', col = 'blue')
}
