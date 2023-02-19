
setwd("~/dissertation/writing/")

ff <- list.files(path = ".", recursive = TRUE, full.names = TRUE, pattern = ".tex")

rr <- ff[1]

fileName=rr
con=file(fileName,open="r")
line=readLines(con)

begdoc <- c()
for (i in seq_along(line)) {
  if (grepl(pattern = "begin{document}", x = line[i], fixed=TRUE)) {
    begdoc <- c(begdoc, i)
  }
}

enddoc <- c()
for (i in seq_along(line)) {
  if (grepl(pattern = "end{document}", x = line[i], fixed=TRUE)) {
    begdoc <- c(enddoc, i)
  }
}


long=length(line)
for (i in 1:long){
  linn=readLines(con,1)
  print(linn)
}
close(con)
