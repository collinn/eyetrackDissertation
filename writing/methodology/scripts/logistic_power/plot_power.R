library(bdots)
library(eyetrackSim)
library(ggplot2)
library(gridExtra)

ff <- list.files("rds_files", full.names = TRUE)


sds <- expand.grid(paired = c(TRUE, FALSE),
                   pm = c(1, 2),
                   shift = c(100,200))

## Given signifiance mat, need to return length 401 bool vector
timetiePower <- function(mm) {
  time <- seq(0, 1600, 4)
  vec <- vector("numeric", length = length(time))

  if (is.null(dim(mm))) {
    vec <- as.logical(vec)
    return(vec)
  }

  # each of these into a list
  sm <- split(mm, row(mm))
  bv <- lapply(sm, function(m) {
    #sigt <- do.call(seq, as.list(c(m, by = 0.005)))
    time[time >= m[1] & time <= m[2]] # for rounding
  })
  bv <- Reduce(union, bv)
  rr <- time %in% bv #round(time,3) %in% round(bv, 3)
  return(rr)
}




getDiffSlicesgg <- function(ff, ww, leg = TRUE) {
  rr <- readRDS(ff)

  tt <- attributes(rr)[[1]] #|> unlist()

  vs <- ifelse(tt$pm == 1, "\nLow Var", "\nHigh Var")

  tit <- paste0("Paired: ", tt$paired, vs, "\nShift: ", tt$shift)
  sm <- lapply(rr, `[[`, 1)
  mm <- lapply(rr, `[[`, 2)
  pm <- lapply(rr, `[[`, 3)

  smm <- sapply(sm, timetiePower) |> rowSums()
  mmm <- sapply(mm, timetiePower) |> rowSums()
  pmm <- sapply(pm, timetiePower) |> rowSums()
  time <- seq(0, 1600, 4)

  dat <- data.table(Method = rep(c("Hom. Boot",
                                   "Het. Boot",
                                   "Permutation"), each = 401),
                    Power = c(smm, mmm, pmm),
                    Time = rep(time, 3))

  ll <- length(rr)

  pp <- ggplot(dat, aes(Time, Power, color = Method)) + theme_bw() + ggtitle(tit) +
    geom_line(linewidth=1) + #geom_abline(slope = 0, intercept = 5, color = 'red', linetype = "dotted")
    scale_color_manual(values = c("#00BFC4", "#7CAE00", "#C77CFF")) + theme(legend.position = "bottom")

  if (!leg) pp <- pp + theme(legend.position = "none")
  return(pp)
}

getDiffSlicesgg(ff[1], 1)
getDiffSlicesgg(ff[2], 2)
getDiffSlicesgg(ff[3], 3)
getDiffSlicesgg(ff[4], 4)

## Shift of 100
p1 <- getDiffSlicesgg(ff[1], 1)
p2 <- getDiffSlicesgg(ff[2], 2)
p3 <- getDiffSlicesgg(ff[3], 3)
p4 <- getDiffSlicesgg(ff[4], 4)

grid.arrange(p3, p4, nrow = 1) # little var
grid.arrange(p1, p2, nrow = 1) # big var

grid.arrange(p1, p2, p3, p4, nrow = 2)

## Shift of 200
p5 <- getDiffSlicesgg(ff[5], 5)
p6 <- getDiffSlicesgg(ff[6], 6)
p7 <- getDiffSlicesgg(ff[7], 7)
p8 <- getDiffSlicesgg(ff[8], 8)

grid.arrange(p5, p6, nrow = 1)
grid.arrange(p7, p8, nrow = 1)

grid.arrange(p5, p6, p7, p8, nrow = 2)
