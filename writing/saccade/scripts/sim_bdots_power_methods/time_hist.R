
# tt <- replicate(n = N, expr = runSim_lg(idx = 1))
# tt2 <- replicate(n = N, expr = runSim_lg(idx = 2))
# tt3 <- replicate(n = N, expr = runSim_lg(idx = 3))
#
# tt4 <- replicate(n = N, expr = runSim_dg(idx = 4))
# tt5 <- replicate(n = N, expr = runSim_dg(idx = 5))
# tt6 <- replicate(n = N, expr = runSim_dg(idx = 6))


library(bdots)
library(eyetrackSim)
library(ggplot2)

powerHist <- function(y, tit) {
  y <- readRDS(y)
  ons <- y[1, ]
  fix <- y[2, ]

  timesig <- function(mm) {
    time <- seq(0, 2000, 4)
    vec <- vector("numeric", length = length(time))

    if (is.null(dim(mm))) {
      vec <- as.logical(vec)
      return(vec)
    }

    sm <- split(mm, row(mm))
    bv <- lapply(sm, function(m) {
      sigt <- do.call(seq, as.list(c(m, 4)))
    })
    bv <- Reduce(union, bv)
    rr <- time %in% bv # logical vec
    return(rr)
  }

  TIME <- seq(0, 2000, 4)

  ons_mat <- sapply(ons, timesig)
  ons_mat <- apply(ons_mat, 2, function(z) TIME[z])
  ons_mat <- Reduce("c", ons_mat)

  fix_mat <- sapply(fix, timesig)
  fix_mat <- apply(fix_mat, 2, function(z) TIME[z])
  fix_mat <- Reduce("c", fix_mat)

  s1 <- hist(ons_mat, breaks = seq(0, 2000, 40), plot = FALSE)$counts
  s2 <- hist(fix_mat, breaks = seq(0, 2000, 40), plot = FALSE)$counts

  yy <- max(s1, s2)

  dat <- data.table(Method = rep(c("Onset", "Proportion"),
                                 times = c(length(ons_mat),
                                           length(fix_mat))),
                    Time = c(ons_mat, fix_mat))

  ggplot(dat, aes(x = Time, color = Method, fill = Method)) +
    geom_histogram(alpha = 0.5, position = "identity", binwidth = 40) +
    ylab("Frequency") + theme_bw() + scale_fill_manual(values = c("#619CFF", "#00BA38")) +
    scale_color_manual(values = c("#619CFF", "#00BA38")) + ggtitle(tit)


  # hist(ons_mat, breaks = seq(0, 2000, 40), ylim = c(0, yy), main = tit,
  #      xlab = "Time", col = alpha("steelblue", alpha = 0.5))
  # hist(fix_mat, breaks = seq(0, 2000, 40), ylim = c(0, yy), main = tit,
  #      xlab = "Time", col = alpha("tomato", alpha = 0.5), add = TRUE)
}

ff <- list.files("rds_files", full.names = TRUE, pattern = "rds")

powerHist(ff[1], "no delay lg")
powerHist(ff[2], "norm delay lg")
powerHist(ff[3], "weib delay lg")

powerHist(ff[4], "no delay dg")
powerHist(ff[5], "norm delay dg")
powerHist(ff[6], "weib delay dg")


### Let's also do here what the actual curves look like?
bp <- eyetrackSim:::baseParams
bp2 <- eyetrackSim:::baseParams2

lg1 <- bp[fn == 1, ]$mean
lg2 <- bp2[fn == 1, ]$mean
dg1 <- bp[fn == 2, ]$mean
dg2 <- bp2[fn == 2, ]$mean

time <- seq(0, 2000, by = 4)

lf1 <- data.table(Time = time, y = logistic_f(p = lg1, time), group = "A")
lf2 <- data.table(Time = time, y = logistic_f(p = lg2, time), group = "B")

ld <- rbind(lf1, lf2)


df1 <- data.table(Time = time, y = doubleGauss_f(p = dg1, time), group = "B")
df2 <- data.table(Time = time, y = doubleGauss_f(p = dg2, time), group = "A")
dd <- rbind(df2, df1)




p1 <- ggplot(ld, aes(Time, y, color = group)) + ylab("") + theme_bw() +
  geom_line(linewidth = 1) + theme(legend.position = "none",
                                   text = element_text(size = 10)) +
  ggtitle("Logistic, Crossover Shift")

p2 <- ggplot(dd, aes(Time, y, color = group)) + ylab("") + theme_bw() +
  geom_line(linewidth = 1) + theme(legend.position = "none",
                                   text = element_text(size = 10)) +
  ggtitle("Asymmetric Gaussian, Mean Shift")

pdf("~/dissertation/writing/saccade/img/group_shift.pdf", width = 7, height = 4)
gridExtra::grid.arrange(p1, p2, nrow = 1)
dev.off()




















