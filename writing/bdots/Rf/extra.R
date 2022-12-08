
fit <- readRDS(file = "eightgrpfit.rds")
fit$fitCode |> table()
plot(fit[fitCode > 0, ])
