#!/bin/bash

Rscript maketex.R

#cp ~/dissertation/writing/bdots/img/* img/
#cp ~/dissertation/writing/saccade/img/* img/
#cp ~/dissertation/writing/methodology/img/* img/

# get this little bugger too
# cp ~/dissertation/writing/saccade/dg_appendix.tex .

pdflatex main
bibtex main
pdflatex main
pdflatex main

rm main.aux
rm main.bbl
rm main.lof
rm main.lot
rm main.log
rm main.blg
