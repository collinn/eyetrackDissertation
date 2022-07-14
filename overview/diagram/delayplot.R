
library(data.table)

library(ggplot2)
ll <- readRDS("../data/data.rds")
dt <- ll$fits
dt2 <- ll$original

labs <- c("Known Delay", "Unknown Fixed Delay", "Unknown Random Delay")
names(labs) <- unique(dt$Condition)
png("randomdelay.png", width = 900, height = 700)
ggplot(data = dt2, aes(x = time, y = fit), color = col) +
  geom_line(size = 2, aes(color = "Underlying")) +
  geom_line(data = dt[Condition == "Random Delay", ], aes(x = time, y = fit, color = Condition), size = 2) +
  #  xlab("Time") + ylab("f(t)") +
  labs(x = "Time", y = "f(t)", color = "Condition") +
  #facet_wrap(~Condition, labeller = labeller(Condition = labs)) +
  scale_color_manual(values = c("Underlying" = "black", "Random Delay" = "#619CFF"),
                     labels = c("Underlying", "Unknown Random Delay")) +
  theme_bw() +
  theme(legend.position = "bottom")
dev.off()
