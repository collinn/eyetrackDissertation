
library(data.table)

dat <- fread("jtrace_parti_graph.csv")
dat[, V7:=NULL]

cc <- c("")
matplot(x = dat[, 1], y = dat[, -1], type = 'l')

dat2 <- melt(dat, id.vars = "cycle")

library(ggplot2)
dat2[, word := variable]
png("trace_plot.png", width = 624)
ggplot(dat2, aes(cycle, value, color = word)) +
  geom_line(size = 1) + ylab("Activation") + theme_bw(base_size=22) +
  ggtitle("TRACE Word Activation")
dev.off()
