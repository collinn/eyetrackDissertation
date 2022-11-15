## This requires two files that are located in my onedrive but too larget
# to keep in git repo

## This is on onedrive
dt <- fread("looks-matched_human.txt", fill = TRUE) #218 mb
dt[, `:=`(trial = NULL, lookID = NULL, StartTime = NULL,
          None = NULL, NoData = NULL, `vstim-Unrelated1` = NULL,
          `vstim-Unrelated2` = NULL, `vstim-Unrelated3` = NULL,
          `vstim-Unrelated4` = NULL, `vstim-Rhyme` = NULL)] # things i didn't need to download (72mb)

dt[, `:=`(target = mean(`vstim-Target`, na.rm = TRUE),
          cohort = mean(`vstim-Cohort`, na.rm = TRUE)), by = .(subject, time)]

dt[, `:=`(`vstim-Target` = NULL, `vstim-Cohort` = NULL,
          saccadenum = NULL)]
dt <- unique(dt)

fwrite(dt, file = "human_looks.csv")


## Also on onedrive
dts <- fread("Events-matched-sac-fix.txt")

## Remove unneeded (confrim starttimemod with bob eventually)
## Only collecting saccades for target and cohort, then will create
# vars indicating target/nottarget and cohort/notcohort
## We don't want to remove ROWS that were to neither target or not target,
# but we don't need to keep None/NoData (just like we don't need unrealted)
dts[, `:=`(eventID = NULL, Type = NULL, starttimemod = NULL,
           endtimemod = NULL, pupilsize = NULL, amplitude = NULL,
           peakvelocity = NULL, SaccadeNum = NULL, `vstim-Rhyme` = NULL,
           `vstim-Unrelated1` = NULL, `vstim-Unrelated2` = NULL,
           `vstim-Unrelated3` = NULL, `vstim-Unrelated4` = NULL,
           None = NULL, NoData = NULL)]
dts <- dts[!is.na(endtime), ]
dts$trial <- NULL
colnames(dts) <- c("subject", "trial", "starttime", "endtime",
                   "target", "cohort")
fwrite(dts, file = "human_saccades.csv")


## Finally, lets look at trace
ddt <- fread("trace_query.txt")
ddt[, `:=`(simulation = NULL, rhymeact = NULL)]
ddt[, `:=`(target = mean(targetact), cohort = mean(cohortact)), by = frame]
ddt[, `:=`(targetact = NULL, cohortact = NULL)]
ddt <- unique(ddt)
fwrite(ddt, file = "trace_curves.csv")
