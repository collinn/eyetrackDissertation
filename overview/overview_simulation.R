library(data.table)
library(eyetrackSim)
library(bdots)

## For logistic
params <- c(0.05, .9, 0.0019, 969.3)

## No delay
sub1 <- runSub(pars = params, saccadeDelay = 0)

## 200ms delay
sub2 <- runSub(pars = params, saccadeDelay = 200)

## Gamma
sub3 <- runSub(pars = params, saccadeDelay = NULL)

## Weird error I should investigate
time <- 0:2000
ID <- c("No Delay", "200ms Delay", "Random Delay")
id_idx <- 1
tt <- lapply(list(sub1, sub2, sub3), function(x) {
  y <- buildSaccadeSub(x)
  y$ID <- 1
  y$group <- 1
  fit <- bdotsFit(data = y,
                  subject = "ID",
                  group = "group",
                  y = "looks", time = "starttime",
                  curveType = logistic())
  cc <- coef(fit)
  fv <- eyetrackSim:::logistic_f(p = cc, t = time)
  fv <- data.table(Condition = ID[id_idx], time = time, fit = fv)
  id_idx <<- id_idx + 1
  return(list("coef" = cc, "fit" = fv))
})

cc <- lapply(tt, `[[`, 1) |> (function(x) Reduce(rbind, x))()
dt <- lapply(tt, `[[`, 2) |> rbindlist()
dt[, Condition := factor(Condition, levels = ID)]

## Wth underlying curve
dt2 <- data.table(time = time, fit = eyetrackSim:::logistic_f(p = params, t = time))

ll <- list(fits = dt, original = dt2)
saveRDS(ll, "data/data.rds")
ggplot(data = dt2, aes(x = time, y = fit), color = col) +
  geom_line(size = 1, aes(color = "Underlying")) +
  geom_line(data = dt, aes(x = time, y = fit, color = Condition), size = 1) +
#  xlab("Time") + ylab("f(t)") +
  labs(x = "Time", y = "f(t)", color = "Condition") +
  facet_wrap(~Condition) +
  scale_color_manual(values = c("Underlying" = "black", "No Delay" = "#F8766D", "200ms Delay" = "#00BA38", "Random Delay" = "#619CFF"),
                     labels = c("Underlying", "No Delay", "200ms Delay", "Random Delay")) +
  theme_bw() +
  theme(legend.position = "bottom")



