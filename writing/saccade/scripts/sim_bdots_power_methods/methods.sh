#!/bin/bash

# run sims
R CMD BATCH --no-save --no-restore "--args $SGE_TASK_ID" methods_power.R .$SGE_TASK_ID.Rout

