## This requires two files that are located in my onedrive but too larget
# to keep in git repo
setwd("~/dissertation/data/bob_trace_data/")
# ## This is on onedrive
# dt <- fread("looks-matched_human.txt", fill = TRUE) #218 mb
# dt[, `:=`(trial = NULL, lookID = NULL, StartTime = NULL,
#           None = NULL, NoData = NULL, `vstim-Unrelated1` = NULL,
#           `vstim-Unrelated2` = NULL, `vstim-Unrelated3` = NULL,
#           `vstim-Unrelated4` = NULL, `vstim-Rhyme` = NULL)] # things i didn't need to download (72mb)
# 
# dt[, `:=`(target = mean(`vstim-Target`, na.rm = TRUE),
#           cohort = mean(`vstim-Cohort`, na.rm = TRUE)), by = .(subject, time)]
# 
# dt[, `:=`(`vstim-Target` = NULL, `vstim-Cohort` = NULL,
#           saccadenum = NULL)]
# dt <- unique(dt)
# 
# fwrite(dt, file = "human_looks.csv")
looks <- fread("../trace_n_things/looks-matched_human.txt")


## Get rid of non-regular subjects
sub <- fread("../trace_n_things/subject_protocol.txt")
sub <- sub[Protocol == "N", ] # 40 of them
looks <- looks[subject %in% sub$subjectID, ]

looks[, `:=`(unrel= sum(`vstim-Unrelated1`, `vstim-Unrelated2`, `vstim-Unrelated3`, `vstim-Unrelated4`)), 
      by = .(trial, time)]


## Get mean value of each activation by subject
looks[, `:=`(target = mean(`vstim-Target`, na.rm = TRUE),
             cohort = mean(`vstim-Cohort`, na.rm = TRUE), 
             unrelated = mean(unrel, na.rm = TRUE), 
             rhyme = mean(`vstim-Rhyme`, na.rm = TRUE)), 
      by = .(subject, time)]

## Remove those I don't want
looks[, `:=`(lookID = NULL, StartTime = NULL, 
             None = NULL, NoData = NULL, saccadenum = NULL, 
             `vstim-Unrelated1` = NULL,
             `vstim-Unrelated2` = NULL, `vstim-Unrelated3` = NULL,
             `vstim-Unrelated4` = NULL, `vstim-Rhyme` = NULL, 
             `vstim-Target` = NULL, `vstim-Cohort` = NULL, unrel,
             trial = NULL)]

## Only need one set of time points for each subject (since averaged across trials)
looks <- unique(looks)

fwrite(looks, "human_looks.csv")

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
# ddt <- fread("trace_query.txt")
# ddt[, `:=`(simulation = NULL, rhymeact = NULL)]
# ddt[, `:=`(target = mean(targetact), cohort = mean(cohortact)), by = frame]
# ddt[, `:=`(targetact = NULL, cohortact = NULL)]
# ddt <- unique(ddt)

trace <- fread("../trace_n_things/trace_query.txt")

## Take mean across simulations
trace[, `:=`(target = mean(targetact), 
             cohort = mean(cohortact), 
             rhyme = mean(rhymeact), 
             ur = mean(uract)), by = .(frame)]

## Time is 16 * frame + 75
trace[, time := 16 * frame + 75]

## Remove unnecessary vars
trace[, `:=`(simulation = NULL, targetact = NULL, 
             cohortact = NULL, rhymeact = NULL, 
             uract = NULL, frame = NULL)]

## Unique only
trace <- unique(trace)

fwrite(trace, file = "trace_curves.csv")
