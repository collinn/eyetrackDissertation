
## Compile rmd documents to correct place
library(rmarkdown)

render("index.Rmd", output_dir = "../")
render("old.Rmd", output_dir = "../")

render("bdots.Rmd", output_dir = "../subdir/")
render("eyetrackSim.Rmd", output_dir = "../subdir/")
render("theory.Rmd", output_dir = "../subdir/")
render("recover_true_pars.Rmd", output_dir = "../subdir/")
