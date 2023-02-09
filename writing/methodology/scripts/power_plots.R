
## Plots for power

library(ggplot2)

time <- seq(-2, 2, length.out = 501)

dt1 <- data.table(Condition = "Effect",
                  Time = time)
dt1[, y := pline(c(0, 0.5), time, "A")]

dt2 <- data.table(Condition = "No Effect",
                  Time = time)
dt2[, y := pline(c(0, 0), time, "B")]
dt <- rbindlist(list(dt1, dt2))

pdf("~/dissertation/writing/methodology/img/power_plot.pdf",
    width = 6, height = 4)
ggplot(dt, aes(Time, y, color = Condition)) +
  geom_line(linewidth = 1, alpha = 0.75) + theme_bw()
dev.off()
