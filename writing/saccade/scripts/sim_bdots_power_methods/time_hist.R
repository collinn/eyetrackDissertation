
library(bdots)
library(eyetrackSim)
library(ggplot2)

## First create group shift so that we also have diff curve
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

## Difference vectors
diffl <- data.table(Time = time,
                    y = lf1$y - lf2$y)
diffg <- data.table(Time = time,
                    y = df1$y - df2$y)


diffl$Method <- "Difference"
diffg$Method <- "Difference"

powerHist <- function(idx, tit) {
  y <- readRDS(ff[idx])
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


  if (idx %in% 1:3) {
    ggplot(dat, aes(x = Time, color = Method, fill = Method)) +
      geom_histogram(alpha = 0.5, position = "identity", binwidth = 40, show_guide = FALSE) +
      ylab("Frequency") + theme_bw() + ggtitle(tit) +
      geom_line(data = diffl, aes(x = Time, y = abs(y*20000), col = Method), linewidth = 1) +
      scale_y_continuous(
        # Features of the first axis
        name = "Frequency",
        # Add a second axis and specify its features
        sec.axis = sec_axis( trans=~.*.000005, name="Absolute Difference")
      ) + scale_fill_manual(values = c( "white", "#619CFF", "#00BA38")) +
      scale_color_manual(values = c("brown1", "#619CFF", "#00BA38"))
  } else {
    ggplot(dat, aes(x = Time, color = Method, fill = Method)) +
      geom_histogram(alpha = 0.5, position = "identity", binwidth = 40, show_guide = FALSE) +
      ylab("Frequency") + theme_bw() + ggtitle(tit) +
      geom_line(data = diffg, aes(x = Time, y = abs(y*10000), col = Method), linewidth = 1) +
      scale_y_continuous(
        # Features of the first axis
        name = "Frequency",
        # Add a second axis and specify its features
        sec.axis = sec_axis( trans=~.*.00001, name="Absolute Difference")
      ) + scale_fill_manual(values = c( "white", "#619CFF", "#00BA38")) +
      scale_color_manual(values = c("brown1", "#619CFF", "#00BA38"))
  }

  # ggplot(dat, aes(x = Time, color = Method, fill = Method)) +
  #   geom_histogram(alpha = 0.5, position = "identity", binwidth = 40) +
  #   ylab("Frequency") + theme_bw() + scale_fill_manual(values = c("#619CFF", "#00BA38")) +
  #   scale_color_manual(values = c("#619CFF", "#00BA38")) + ggtitle(tit)
  #

}

ff <- list.files("rds_files", full.names = TRUE, pattern = "rds")

p1 <- powerHist(1, "Logistic, No Delay")
p2 <- powerHist(2, "Logistic, Normal Delay")
p3 <- powerHist(3, "Logistic, Weibull Delay")

p4 <- powerHist(4, "Asymmetric Gauss, No Delay")
p5 <- powerHist(5, "Asymmetric Gauss, Normal Delay")
p6 <- powerHist(6, "Asymmetric Gauss, Weibull Delay")

pdf("~/dissertation/writing/saccade/img/diff_hist_all.pdf", width = 7, height = 8)
ggpubr::ggarrange(p1, p4, p2, p5, p3, p6, nrow = 3, ncol = 2,
                  common.legend = TRUE, legend = "bottom")
dev.off()

### Let's also do here what the actual curves look like?


pdf("~/dissertation/writing/saccade/img/logistic_difference.pdf", width = 3, height = 3.5)#, width = 7, height = 4)
ggplot(diffl, aes(Time, abs(y))) + geom_line() + ylab("Absolute Difference") + theme_bw() +
  geom_line(linewidth = 1) + theme(legend.position = "none") +
  ggtitle("Logistic")
dev.off()

pdf("~/dissertation/writing/saccade/img/dg_difference.pdf", width = 3, height = 3.5)#, width = 7, height = 4)
ggplot(diffg, aes(Time, abs(y))) + geom_line() + ylab("Absolute Difference") + theme_bw() +
  geom_line(linewidth = 1) + theme(legend.position = "none")+
  ggtitle("Asymmetric Gauss")
dev.off()



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



########## Table of correlations between error and hist?

timeDiffCorr <- function(idx) {
  y <- readRDS(ff[idx])
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

  s1 <- hist(ons_mat, breaks = seq(0, 2000, 40), plot = FALSE)
  s2 <- hist(fix_mat, breaks = seq(0, 2000, 40), plot = FALSE)


  dat <- data.table(Method = rep(c("Onset", "Proportion"),
                                 times = c(length(ons_mat),
                                           length(fix_mat))),
                    Time = c(ons_mat, fix_mat))

  ## Get density from each hist
  ons_den <- s1$density
  prop_den <- s2$density

  if (idx %in% 1:3) {
    dd <- diffl$y[diffl$Time %in% s1$breaks[-1]]
  } else {
    dd <- diffg$y[diffl$Time %in% s1$breaks[-1]]
  }
  dd <- abs(dd)

  cv <- ifelse(idx %in% 1:3, "Logistic", "Asymmetric Gauss")
  dll <- ifelse(idx %in% c(1,4), "No Delay",
                ifelse(idx %in% c(2,5), "Normal Delay", "Weibull Delay"))
  val <- c(ons_cor = cor(ons_den, dd), prop_cor = cor(prop_den, dd))

  c(Curve = cv, Delay = dll, round(val, 4))

}

ff <- list.files("rds_files", full.names = TRUE, pattern = "rds")

tab <- sapply(1:6, timeDiffCorr) |> t()

xtable::xtable(tab) |> print(include.rownames = FALSE)


 p1 <- powerHist(1, "Logistic, No Delay")
p2 <- powerHist(2, "Logistic, Normal Delay")
p3 <- powerHist(3, "Logistic, Weibull Delay")

p4 <- powerHist(4, "Asymmetric Gauss, No Delay")
p5 <- powerHist(5, "Asymmetric Gauss, Normal Delay")
p6 <- powerHist(6, "Asymmetric Gauss, Weibull Delay")
