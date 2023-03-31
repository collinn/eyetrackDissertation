library(bdots)
library(eyetrackSim)
library(ggplot2)

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
    #sigt <- do.call(seq, as.list(c(m, by = 0.005)))
    time[time > m[1] & time < m[2] + 0.005] # for rounding
  })
  bv <- Reduce(union, bv)
  rr <- time %in% bv #round(time,3) %in% round(bv, 3)
  return(rr)
}

getDiffSlices <- function(ff, ww) {
  rr <- readRDS(ff)

  tt <- attributes(rr) |> unlist()

  tit <- switch(ww,
                "Homogenous Means, AR(1) Error",
                "Heterogenous Means, AR(1) Error",
                "Heterogeneous Means, No AR(1) Error")
  #tit <- paste0("Manymeans: ", as.logical(tt[1]), ",\n AR(1): ", as.logical(tt[2]), ", sigma: ", tt[[4]])

  sm <- lapply(rr, `[[`, 1)
  mm <- lapply(rr, `[[`, 2)
  pm <- lapply(rr, `[[`, 3)

  smm <- sapply(sm, timetiePower) |> rowSums()
  mmm <- sapply(mm, timetiePower) |> rowSums()
  pmm <- sapply(pm, timetiePower) |> rowSums()
  time <- seq(-1, 1, length.out = 401)

  ll <- length(rr)

  plot(time, smm, type = 'l', col = 'green', main = tit, ylim = c(0,ll), ylab = "Power")
  lines(time, mmm, type = 'l', col = 'black')
  lines(time, pmm, type = 'l', col = 'blue')
  lines(seq(-1, 0, 0.5), rep(5,3), col = 'red', lty = 2, lwd = 2)
  lines(c(0,0), c(0.05*ll, ll), col = 'red', lty = 2, lwd = 2)
  legend("bottomright", legend = c("Hom. Boot", "Het. Boot", "Permutation"),
         col = c("green", "black", "blue"), lty = 1, lwd = 1)
  #abline(v = 0, col = 'red', lty = 2,  lwd = 2, ylim = c(5, 100))
}

getDiffSlicesgg <- function(ff, ww, leg = FALSE) {
  rr <- readRDS(ff)

  tt <- attributes(rr)[[1]] #|> unlist()

  # mean type
  mt <- ifelse(tt$mm, "Heterogeneous Means\n ", "Homogeneous Means\n")
  ## ar type
  art <- ifelse(tt$ar, "AR(1) Error, ", "IID Error, ")
  ## Ar specified?
  ars <- ifelse(tt$bcor, "AR(1) Specified", "AR(1) Not Specified")
  # slope
  #ss <- ifelse(tt$slope == 0.025, "Slope = 0.025", "Slope = 0.25")
  # pending finishing 0.025
  tit <- paste0(mt, art, ars)#, ss)

  # tit <- switch(ww,
  #               "Homogenous Means, AR(1) Error",
  #               "Heterogenous Means, AR(1) Error",
  #               "Heterogeneous Means, No AR(1) Error")
  #tit <- ""
  sm <- lapply(rr, `[[`, 1)
  mm <- lapply(rr, `[[`, 2)
  pm <- lapply(rr, `[[`, 3)

  smm <- sapply(sm, timetiePower) |> rowSums()
  mmm <- sapply(mm, timetiePower) |> rowSums()
  pmm <- sapply(pm, timetiePower) |> rowSums()
  time <- seq(-1, 1, length.out = 401)

  dat <- data.table(Method = rep(c("Hom. Boot",
                            "Het. Boot",
                            "Permutation"), each = 401),
             Power = c(smm, mmm, pmm),
             Time = rep(time, 3))

  ll <- length(rr)

  pp <- ggplot(dat, aes(Time, Power, color = Method)) + theme_bw() + ggtitle(tit) +
    geom_line(linewidth=1) + #geom_abline(slope = 0, intercept = 5, color = 'red', linetype = "dotted")
    geom_function(fun = Vectorize(function(x) {
      ifelse(x < 0, 0.05*ll, NA)
    }), color = 'red', linetype = "dashed", linewidth = 1.25) +
    geom_abline(slope = 1e5, intercept = 0, color = 'red', linetype = 'dashed',
                linewidth = 1.25) + ylim(c(0, ll)) +
    scale_color_manual(values = c("#00BFC4", "#7CAE00", "#C77CFF")) + theme(legend.position = "bottom")

  if (!leg) pp <- pp + theme(legend.position = "none")
  return(pp)
}

# Only doing power on these three things yay
sds <- data.table(mm = rep(c(F, T, T, T, T), 2),
                  ar = rep(c(T, T, T, F, F), 2),
                  bcor = rep(c(T, T, F, T, F), 2),
                  #sigVal = 0.05,
                  slope = rep(c(0.025, 0.25), each = 5))

ff <- list.files("~/dissertation/writing/methodology/scripts/power/rds_files", full.names = TRUE,
                 pattern = "rds")
ff <- ff[c(1, 3:10, 2)]



i <- 1

getDiffSlices(ff[i], i)
i <- i+1

print(i)

pdf("../../img/type_two_err_time_slice.pdf", width = 5, height = 7.5)
par(mfrow = c(3, 1))
getDiffSlices(ff[1], 1)
getDiffSlices(ff[2], 2)
getDiffSlices(ff[3], 3)
# getDiffSlices(ff[4])
# getDiffSlices(ff[3])
# getDiffSlices(ff[6])
# getDiffSlices(ff[2])
# getDiffSlices(ff[5])
dev.off()

## Now with gg yo
pdf("~/dissertation/writing/methodology/img/type_two_error_time_a.pdf", width = 5, height = 2.5)
getDiffSlicesgg(ff[1], 1, F)
dev.off()
pdf("~/dissertation/writing/methodology/img/type_two_error_time_b.pdf", width = 5, height = 2.5)
getDiffSlicesgg(ff[2], 2, F)
dev.off()
pdf("~/dissertation/writing/methodology/img/type_two_error_time_c.pdf", width = 5, height = 2.5)
getDiffSlicesgg(ff[3], 3, ll = TRUE)
dev.off()


a <- getDiffSlicesgg(ff[1], 1)
b <- getDiffSlicesgg(ff[2], 2)
cc <- getDiffSlicesgg(ff[3], 3, leg = TRUE)

pdf("~/dissertation/writing/methodology/img/typeII_time.pdf", width = 5.5, height = 7.5)
## 0.25 for main of paper
ggpubr::ggarrange(b3, b4, b5, nrow = 3,
                  common.legend = TRUE, legend = "bottom")
dev.off()



## Delete this


a <- getDiffSlicesgg(ff[1], 1)
b <- getDiffSlicesgg(ff[2], 2)
cc <- getDiffSlicesgg(ff[3], 3)
b1 <- getDiffSlicesgg(ff[4], 4)
b2 <- getDiffSlicesgg(ff[5], 5)

b3 <- getDiffSlicesgg(ff[6], 6)
b4 <- getDiffSlicesgg(ff[7], 7)
b5 <- getDiffSlicesgg(ff[8], 8)
b6 <- getDiffSlicesgg(ff[9], 9)
b7 <- getDiffSlicesgg(ff[10], 10,  leg = TRUE)





# gridExtra::grid.arrange(a, b, cc, b1, b2, b3, b4, b5, b6, b7, nrow = 5)
# ggpubr::ggarrange(a, b, cc, b1, b2, b3, b4, b5, b6, b7, nrow = 5, ncol = 2,
#                   common.legend = TRUE, legend = "bottom")

## With just slope = 0.25
pdf("~/dissertation/writing/methodology/img/full_power_25.pdf",
    width = 7, height = 8.5)
ggpubr::ggarrange(b4, b5, b6, b7, b3, nrow = 3, ncol = 2,
                  common.legend = TRUE, legend = "bottom")
dev.off()



b6 <- getDiffSlicesgg(ff[9], 9, leg = TRUE)
b6 <- b6 + ggtitle("Power Simulation")
pdf("~/dissertation/defense/img/power_sample.pdf", width = 5, height = 5)
b6
dev.off()

pdf("~/dissertation/defense/img/method_image.pdf", width = 5, height = 3.5)
gridExtra::grid.arrange(pp, b6, nrow = 1)
dev.off()
