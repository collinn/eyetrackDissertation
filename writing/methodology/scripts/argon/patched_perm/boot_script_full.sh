#!/bin/bash

# run sims
#R CMD BATCH --no-save --no-restore "--args $SGE_TASK_ID" createSimFitData.R
date +"%r"

#bootidx=("4" "12" "8" "16")

#for i in "${bootidx[@]}"
for i in {1..12}
do
        echo "sim $i"
        R CMD BATCH --no-save --no-restore "--args $i" bootSim_patched_perm.R
done
date +"%r"
