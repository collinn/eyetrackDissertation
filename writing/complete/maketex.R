

printToMain <- function(rr, fname) {
  fileName=rr
  con=file(fileName,open="r")
  line=readLines(con)

  ## They all start with this
  begdoc <- c()
  for (i in seq_along(line)) {
    if (grepl(pattern = "section{Introduction}", x = line[i], fixed=TRUE)) {
      begdoc <- c(begdoc, i)
    }
  }

  enddoc <- c()
  for (i in seq_along(line)) {
    if (grepl(pattern = "end{document}", x = line[i], fixed=TRUE)) {
      enddoc <- c(enddoc, i)
    }
  }

  nline <- line[begdoc:(enddoc-1L)]
  sink(file = fname)
  cat(nline, sep = "\n")
  sink()
  close(con)
}


## Print bdots
printToMain("~/dissertation/writing/bdots/bdots.tex",
            "~/dissertation/writing/complete/bdots_b.tex")

## Print Onset
printToMain("~/dissertation/writing/saccade/saccade.tex",
            "~/dissertation/writing/complete/onset_b.tex")

## Print Method
printToMain("~/dissertation/writing/methodology/methodology.tex",
            "~/dissertation/writing/complete/method_b.tex")
