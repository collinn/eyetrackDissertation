

res <- vector("list", length = 16)

ff <- list.files(pattern = "rds")

for (i in seq_along(ff)) {
  res[[i]] <- readRDS(ff[i])
  file.remove(ff[i])
}

saveRDS(res, "simFits.rds")
