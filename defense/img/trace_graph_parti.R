
library(data.table)

dat <- fread("jtrace_parti_graph.csv")
dat[, V7:=NULL]

cc <- c("")
matplot(x = dat[, 1], y = dat[, -1], type = 'l')

dat2 <- melt(dat, id.vars = "cycle")

library(ggplot2)
dat2[, Word := variable]

# dumb way of rewording but it will work
ds <- split(dat2, by = "Word")
dat2 <- rbindlist(ds[c(5,2,4,3,1)])
#dat2$Word <- as.character(dat2$Word)
bb <- names(ds[c(5,2,4,3,1)])

png("trace_plot.png", width = 624)
ggplot(dat2, aes(cycle, value, color = Word)) +
  geom_line(size = 1) + ylab("Activation") + theme_bw(base_size=22) +
  ggtitle("TRACE Word Activation") +
  scale_fill_manual(labels = bb)
dev.off()

dat <- copy(dat2)
dat$Word <- as.character(dat$Word)
dat$Word <- ifelse(dat$Word == "parti", "party", dat$Word)
dat$Word <- ifelse(dat$Word == "pik", "peek", dat$Word)
dat$Word <- ifelse(dat$Word == "parS^l", "partial", dat$Word)

png("trace_plot_reg.png", width = 624)
ggplot(dat, aes(cycle, value, color = Word)) +
  geom_line(size = 1) + ylab("Activation") + theme_bw(base_size=22) +
  ggtitle("TRACE Word Activation: 'party'") +
  scale_fill_manual(labels = bb)
dev.off()
