#!/bin/bash

# run sims
R CMD BATCH --no-save --no-restore "--args $SGE_TASK_ID" createSimFitData.R .$SGE_TASK_ID.RoutS

#date +"%r"
#for i in {1..16}
#do
#        echo "sim $i"
#        R CMD BATCH --no-save --no-restore "--args $i" createSimFitData.R
#done
#date +"%r"
