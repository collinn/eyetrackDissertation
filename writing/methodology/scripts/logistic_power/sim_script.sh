#!/bin/bash

# run sims
R CMD BATCH --no-save --no-restore "--args $SGE_TASK_ID" logistic_shift_sim.R .$SGE_TASK_ID.Rout

#date +"%r"
#for i in {1..16}
#do
#        echo "sim $i"
#        R CMD BATCH --no-save --no-restore "--args $i" createSimFitData.R
#done
#date +"%r"
