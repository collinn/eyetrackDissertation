#!/bin/bash

R CMD BATCH --no-save --no-restore "--args $SGE_TASK_ID" tie_test.R
