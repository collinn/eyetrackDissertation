
## Taking what I know from bob, lets try to do this whole analysis with trace right here
# though there are likely a few ways this can go, we will choose the one that looks best
library(data.table)
library(ggplot2)
library(gridExtra)
library(bdots)
library(eyetrackSim)

setwd("~/dissertation/analysis/")
## Relevant data
looksrt <- fread("../data/bob_trace_data/human_looks_rt_cut.csv")
sacsrt <- fread("../data/bob_trace_data/human_saccades_rt_cut.csv")

looks <- fread("../data/bob_trace_data/human_looks_rt_nocut.csv")
sacs <- fread("../data/bob_trace_data/human_saccades_rt_nocut.csv")

## Should consider making group niot necessary in bdots
looksrt[, group := "A"]
sacsrt[, group := "A"]
looks[, group := "A"]
sacs[, group := "A"]


if (file.exists("~/dissertation/data/saccade_look_fits/target_fits_bdots.RData")) {
  load("~/dissertation/data/saccade_look_fits/target_fits_bdots.RData")
} else {
  fit_looks <- bdotsFit(data = looks, 
                        subject = "subject", 
                        time = "time", 
                        y = "target", 
                        group = "group", 
                        curveType = logistic())
  
  fit_looks_rt <- bdotsFit(data = looksrt, 
                         subject = "subject", 
                         time = "time", 
                         y = "target", 
                         group = "group", 
                         curveType = logistic())
  
  #sacs <- sacs[starttime <= 2000, ]
  ## logistic2() at bottom of this script
  fit_sacs  <- bdotsFit(data = sacs, 
                        subject = "subject", 
                        time = "starttime", 
                        y = "target", 
                        group = "group", 
                        curveType = logistic2())  
  fit_sacs_rt  <- bdotsFit(data = sacsrt, 
                         subject = "subject", 
                         time = "starttime", 
                         y = "target", 
                         group = "group", 
                         curveType = logistic2())  
  
  save(fit_looks, fit_looks_rt, fit_sacs, fit_sacs_rt, 
       file = "~/dissertation/data/saccade_look_fits/target_fits_bdots.RData")
}

## Actually I should remove all of the same subjects
## But not going to compare those with rt removed bewcause nonstandard

# Remove these from looks
idxrm0 <- which(fit_looks$fitCode >= 5 | fit_looks$fitCode >= 5)
idxrm1 <- which(fit_looks_rt$fitCode >= 5 | fit_looks_rt$fitCode >= 5)

# Remove these from saccades 
qq <- coef(fit_sacs)
idxrm2 <- which(qq[,3] < 0 | qq[,4] < 0 | qq[,1] > qq[,2])

## Remove these from  saccades rt
qq <- coef(fit_sacs_rt)
idxrm3 <- which(qq[,3] < 0 | qq[,4] < 0 | qq[,1] > qq[,2])

#idxrm <- Reduce(union, list(idxrm0, idxrm1, idxrm2, idxrm3))
idxrm <- Reduce(union, list(idxrm0, idxrm2)) # no need for RT stuff here

idx <- setdiff(1:40, idxrm)

fit_looks <- fit_looks[idx, ]
fit_looks_rt <- fit_looks_rt[idx, ]

fit_sacs <- fit_sacs[idx, ]
fit_sacs_rt <- fit_sacs_rt[idx, ]

mm1 <- colMeans(coef(fit_looks))
mm2 <- colMeans(coef(fit_looks_rt))
mm3 <- colMeans(coef(fit_sacs))
mm4 <- colMeans(coef(fit_sacs_rt))

time <- 0:1787
f_fix <- logistic_f(mm1, time)
f_sac <- logistic_f(mm3, time)

## Pretty sure I figured out trace (4 looks best)
#trace_luce <- fread("~/dissertation/data/bob_trace_data/trace_scaled_4.5.csv")
#trace_luce <- fread("~/dissertation/data/bob_trace_data/trace_scaled_3.csv")
#trace_luce <- fread("~/dissertation/data/bob_trace_data/trace_scaled_3.5.csv")
trace_luce <- fread("~/dissertation/data/bob_trace_data/trace_scaled_4.csv")

## OR in ggplot
dt1 <- data.table(time = time, y = f_fix, Method = "Fixation")
dt2 <- data.table(time = time, y = f_sac, Method = "Saccade")
dt3 <- data.table(time = trace_luce$time, y = trace_luce$targ_bp, Method = "TRACE")

dt <- rbindlist(list(dt1, dt2, dt3))

png("../img/sac_fix_trace_compare.png", width = 480*1.4)
ggplot(dt, aes(x = time, y = y, color = Method)) + 
  geom_line(lwd = 1.5) + ylim(c(min(dt1$y),1)) + theme_bw(base_size=16) +
  ggtitle("Comparison of Fixation/Saccade\nMethods with TRACE") +
  labs(y = expression(f[theta](t)), x = "Time")
dev.off()


###---------------------------------------------------
############ New logistic
#' Logistic curve function for nlme (test use with saccade, NOT FOR GENERAL USE)
#'
#' Logistic function used in fitting nlme curve for observations
#'
#' @param dat subject data to be used
#' @param y outcome variable
#' @param time time variable
#' @param params \code{NULL} unless user wants to specify starting parameters for gnls
#' @param ... just in case
#'
#' @details \code{y ~ mini + (peak - mini) / (1 + exp(4 * slope * (cross - (time)) / (peak - mini)))}
#' @export
logistic2 <- function(dat, y, time, params = NULL, ...) {
  
  logisticPars <- function(dat, y, time, ...) {
    time <- dat[[time]]
    y <- dat[[y]]
    
    # idx <- order(time)
    # time <- time[idx]
    # y <- y[idx]
    
    ## Remove cases with zero variance
    if (var(y) == 0) {
      return(NULL)
    }
    
    ## Starting estimates based on thing
    mini <- 0
    peak <- 1
    cross <- 750
    slope <- 0.002
    
    return(c(mini = mini, peak = peak, slope = slope, cross = cross))
  }
  
  if (is.null(params)) {
    params <- logisticPars(dat, y, time)
  } else {
    if (length(params) != 4) stop("logistic requires 4 parameters be specified for refitting")
    if (!all(names(params) %in% c("mini", "peak", "slope", "cross"))) {
      stop("logistic parameters for refitting must be correctly labeled")
    }
  }
  ## Return NA list if var(y) is 0
  if (is.null(params)) {
    return(NULL)
  }
  y <- str2lang(y)
  time <- str2lang(time)
  ff <- bquote(.(y) ~ mini + (peak - mini) / (1 + exp(4 * slope * (cross - (.(time))) / (peak - mini))))
  attr(ff, "parnames") <- names(params)
  return(list(formula = ff, params = params))
}
