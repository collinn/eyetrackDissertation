#!/bin/bash

# run sims
date +"%r"
for i in {1..3}
do
        echo "sim $i"
        R CMD BATCH --no-save --no-restore "--args $i" power_simulation.R
done
date +"%r"
