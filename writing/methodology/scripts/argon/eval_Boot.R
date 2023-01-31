simDataSettings <- expand.grid(manymeans = c(TRUE, FALSE),
                               paired = c(TRUE, FALSE),
                               ar1 = c(TRUE, FALSE),
                               bdotscor = c(TRUE, FALSE))

tt <- readRDS("rds_boot/boot1.rds")

## sig times
sigs <- lapply(tt,  function(y) {
  y <- lapply(y, `[[`, 1)
})

sm <- sapply(sigs, function(x) !is.null(x[[1]])) |> mean() |> print()
mm <- sapply(sigs, function(x) !is.null(x[[2]])) |> mean() |> print()
pm <- sapply(sigs, function(x) !is.null(x[[3]])) |> mean() |> print()



tt <- readRDS("rds_boot/boot2.rds")

## sig times
sigs <- lapply(tt,  function(y) {
  y <- lapply(y, `[[`, 1)
})

sm <- sapply(sigs, function(x) !is.null(x[[1]])) |> mean() |> print()
mm <- sapply(sigs, function(x) !is.null(x[[2]])) |> mean() |> print()
pm <- sapply(sigs, function(x) !is.null(x[[3]])) |> mean() |> print()



tt <- readRDS("rds_boot/boot3.rds")

## sig times
sigs <- lapply(tt,  function(y) {
  y <- lapply(y, `[[`, 1)
})

sm <- sapply(sigs, function(x) !is.null(x[[1]])) |> mean() |> print()
mm <- sapply(sigs, function(x) !is.null(x[[2]])) |> mean() |> print()
pm <- sapply(sigs, function(x) !is.null(x[[3]])) |> mean() |> print()



tt <- readRDS("rds_boot/boot4.rds")

## sig times
sigs <- lapply(tt,  function(y) {
  y <- lapply(y, `[[`, 1)
})

sm <- sapply(sigs, function(x) !is.null(x[[1]])) |> mean() |> print()
mm <- sapply(sigs, function(x) !is.null(x[[2]])) |> mean() |> print()
pm <- sapply(sigs, function(x) !is.null(x[[3]])) |> mean() |> print()


getMeans <- function(y) {
  tt <- readRDS(y)
  sigs <- lapply(tt,  function(y) {
    y <- lapply(y, `[[`, 1)
  })

  sm <- sapply(sigs, function(x) !is.null(x[[1]])) |> mean()
  mm <- sapply(sigs, function(x) !is.null(x[[2]])) |> mean()
  pm <- sapply(sigs, function(x) !is.null(x[[3]])) |> mean()
  data.table(sm = sm, mm = mm, pm = pm)
}

ff <- list.files(path = "rds_boot/", pattern = "rds", full.names = TRUE)
gg <- lapply(ff, getMeans)
gg1 <- gg[1:6]
gg3 <- list(data.table(sm = "NA", mm = "NA", pm = "NA"))
gg2 <- gg[7:15]
gg <- rbindlist(c(gg1,gg3 , gg2))

