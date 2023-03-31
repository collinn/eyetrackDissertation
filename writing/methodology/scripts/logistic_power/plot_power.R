library(bdots)
library(eyetrackSim)
library(ggplot2)
library(gridExtra)

ff <- list.files("rds_files", full.names = TRUE, pattern = "rds")


sds <- expand.grid(paired = c(TRUE, FALSE),
                   xosd = c(60, 120),
                   shift = c(50, 150))

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

  vs <- ifelse(tt$xosd == 60, "\nCrossover SD: 60", "\nCrossover SD: 120")
  pp <- ifelse(tt$paired, "Paired", "Not Paired")

  tit <- paste0(pp, vs, "\nCrossover Shift: ", tt$shift)
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
    scale_color_manual(values = c("#00BFC4", "#7CAE00", "#C77CFF")) +
    theme(legend.position = "bottom", text = element_text(size = 8))

  if (!leg) pp <- pp + theme(legend.position = "none")
  return(pp)
}


## Shift of 100
p1 <- getDiffSlicesgg(ff[1], 1, leg = FALSE)
p2 <- getDiffSlicesgg(ff[2], 2, leg = FALSE)
p3 <- getDiffSlicesgg(ff[3], 3, leg = FALSE)
p4 <- getDiffSlicesgg(ff[4], 4, leg = FALSE)

pdf("~/dissertation/writing/methodology/img/log_shift_1.pdf", width = 6, height = 5)
ggpubr::ggarrange(p2, p1, p4, p3, nrow = 2, ncol = 2,
                  common.legend = TRUE, legend = "bottom")
dev.off()

## Shift of 200
p5 <- getDiffSlicesgg(ff[5], 5, leg = FALSE)
p6 <- getDiffSlicesgg(ff[6], 6, leg = FALSE)
p7 <- getDiffSlicesgg(ff[7], 7, leg = FALSE)
p8 <- getDiffSlicesgg(ff[8], 8, leg = TRUE)

pdf("~/dissertation/writing/methodology/img/log_shift_2.pdf", width = 6, height = 5)
ggpubr::ggarrange(p6, p5, p8, p7, nrow = 2, ncol = 2,
                  common.legend = TRUE, legend = "bottom")
dev.off()
