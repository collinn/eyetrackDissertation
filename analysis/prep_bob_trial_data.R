## This requires two files that are located in my onedrive but too larget
# to keep in git repo
setwd("~/dissertation/data/bob_trace_data/")

trials <- fread("~/dissertation/data/trace_n_things/rt_trial.txt")
trials <- trials[, .(TrialID, RT)]
names(trials) <- c("trial", "rt")


## Only keep these subjects
## Get rid of non-regular subjects
sub <- fread("~/dissertation/data/trace_n_things/subject_protocol.txt")
sub <- sub[Protocol == "N", ] # 40 of them
keepsub <- sub$subjectID

###############
## LOOKS ######
###############

looks <- fread("~/dissertation/data/trace_n_things/looks-matched_human.txt")

## Get rid of non-regular subjects
looks <- looks[subject %in% keepsub, ]

looks <- merge(looks, trials)
# only keep before rt
looks <- looks[time < rt, ]

looks[, `:=`(unrel= sum(`vstim-Unrelated1`, `vstim-Unrelated2`, `vstim-Unrelated3`, `vstim-Unrelated4`)), 
      by = .(trial, time)]

## Any trials have zero looks to target?
looks[, anyTarget := sum(`vstim-Target`), by = trial]

noTarg <- looks[anyTarget == 0, trial]
noTarg <- unique(noTarg)
looks <- looks[anyTarget != 0, ]


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
             `vstim-Target` = NULL, `vstim-Cohort` = NULL, unrel = NULL, 
             anyTarget = NULL, trial = NULL, rt = NULL)]

## Only need one set of time points for each subject (since averaged across trials)
looks <- unique(looks)

fwrite(looks, "~/dissertation/data/bob_trace_data/human_looks_rt_cut.csv")


###################
## SACCADES #######
###################

## Also on onedrive
dts <- fread("~/dissertation/data/trace_n_things/Events-matched-sac-fix.txt")

# Keep only TD
dts <- dts[subject %in% keepsub, ]

dts <- merge(dts, trials)

# starttimemod not starttime and endtime to capture initial
dts <- dts[starttimemod <= 2300 & endtimemod >= 0, ]
dts <- dts[starttimemod < rt, ]

# Confirmed that subset of one above
## Check this one for no looks to target (make sure same)
# dts[, anyTarget := sum(`vstim-Target`), by = trial]
# noTarg2 <- unique(dts[anyTarget == 0, trial])

# sum(noTarg2 %in% noTarg) 
# be safe and just remove all noTarg
# Confirmed that one subsumes the other
dts <- dts[!(trial %in% noTarg), ]

## These are very short (not proper saccades) 
# so we should remove these rows as well since
dts <- dts[NoData == 0, ]

## Remove unneeded (confrim starttimemod with bob eventually)
## Only collecting saccades for target and cohort
dts[, `:=`(eventID = NULL, Type = NULL, starttime = NULL,
           endtime = NULL, pupilsize = NULL, amplitude = NULL,
           peakvelocity = NULL, SaccadeNum = NULL, `vstim-Rhyme` = NULL,
           `vstim-Unrelated1` = NULL, `vstim-Unrelated2` = NULL,
           `vstim-Unrelated3` = NULL, `vstim-Unrelated4` = NULL,
           None = NULL, NoData = NULL, rt = NULL)]
dts <- dts[!is.na(endtimemod), ]

dts$trial <- NULL
colnames(dts) <- c("subject", "starttime", "endtime",
                   "target", "cohort")
fwrite(dts, file = "~/dissertation/data/bob_trace_data/human_saccades_rt_cut.csv")




################
## LOOKS2 ######
################

looks2 <- fread("~/dissertation/data/trace_n_things/looks-matched_human.txt")

## Get rid of non-regular subjects
looks2 <- looks2[subject %in% keepsub, ]

# Not doing this here
#looks <- looks[time < rt, ]

looks2[, `:=`(unrel= sum(`vstim-Unrelated1`, `vstim-Unrelated2`, `vstim-Unrelated3`, `vstim-Unrelated4`)), 
      by = .(trial, time)]

## Computed previously, but sure we remove same
looks2 <- looks2[!(trial %in% noTarg), ]

## Get mean value of each activation by subject
looks2[, `:=`(target = mean(`vstim-Target`, na.rm = TRUE),
             cohort = mean(`vstim-Cohort`, na.rm = TRUE), 
             unrelated = mean(unrel, na.rm = TRUE), 
             rhyme = mean(`vstim-Rhyme`, na.rm = TRUE)), 
      by = .(subject, time)]

## Remove those I don't want
looks2[, `:=`(lookID = NULL, StartTime = NULL, 
             None = NULL, NoData = NULL, saccadenum = NULL, 
             `vstim-Unrelated1` = NULL,
             `vstim-Unrelated2` = NULL, `vstim-Unrelated3` = NULL,
             `vstim-Unrelated4` = NULL, `vstim-Rhyme` = NULL, 
             `vstim-Target` = NULL, `vstim-Cohort` = NULL, unrel = NULL, 
             trial = NULL)]

## Only need one set of time points for each subject (since averaged across trials)
looks2 <- unique(looks2)

fwrite(looks2, "~/dissertation/data/bob_trace_data/human_looks_rt_nocut.csv")


####################
## SACCADES2 #######
####################

dts2 <- fread("~/dissertation/data/trace_n_things/Events-matched-sac-fix.txt")
## Keep only TD
dts2 <- dts2[subject %in% keepsub, ]
dts2 <- merge(dts2, trials)

# starttimemod not starttime XX(let's let 2 be the worst and see how it compares)
# now lets try no RT and correct time to see
dts2 <- dts2[starttimemod <= 2300 & endtimemod >= 0, ]

# Not doing this here
#dts2 <- dts2[starttimemod < rt, ]

## Check this one for no looks to target (make sure same)
# dts2[, anyTarget := sum(`vstim-Target`), by = trial]
# noTarg2 <- unique(dts2[anyTarget == 0, trial])

# sum(noTarg2 %in% noTarg) 
# be safe and just remove all noTarg
dts2 <- dts2[!(trial %in% noTarg), ]

## These are very short so we should remove these rows as well
dts2 <- dts2[NoData == 0, ]

## Remove unneeded (confrim starttimemod with bob eventually)
## Only collecting saccades for target and cohort
dts2[, `:=`(eventID = NULL, Type = NULL, starttime = NULL,
           endtime = NULL, pupilsize = NULL, amplitude = NULL,
           peakvelocity = NULL, SaccadeNum = NULL, `vstim-Rhyme` = NULL,
           `vstim-Unrelated1` = NULL, `vstim-Unrelated2` = NULL,
           `vstim-Unrelated3` = NULL, `vstim-Unrelated4` = NULL,
           None = NULL, NoData = NULL,  rt = NULL)]
dts2 <- dts2[!is.na(endtimemod), ]


dts2$trial <- NULL
colnames(dts2) <- c("subject", "starttime", "endtime",
                   "target", "cohort")
fwrite(dts2, file = "~/dissertation/data/bob_trace_data/human_saccades_rt_nocut.csv")
