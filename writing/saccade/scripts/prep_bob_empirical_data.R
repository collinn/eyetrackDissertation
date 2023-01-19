## This is going to be the FINAL
# prep data for TRACE sim thingo

# Looks first, then *transform to saccades* for consistency
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

looks <- looks[time < 2000, ]

## Get rid of non-regular subjects
looks <- looks[subject %in% keepsub, ]

looks <- merge(looks, trials)

looks[, `:=`(unrel= sum(`vstim-Unrelated1`, `vstim-Unrelated2`, `vstim-Unrelated3`, `vstim-Unrelated4`)),
      by = .(trial, time)]

## Any trials have zero looks to target?
looks[, anyTarget := sum(`vstim-Target`), by = trial]

noTarg <- looks[anyTarget == 0, trial]
noTarg <- unique(noTarg)
looks <- looks[anyTarget != 0, ]

## For use at bottom
saccadeDF <- copy(looks)

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

looks[, group := "A"]

fwrite(looks, "~/dissertation/data/bob_trace_data/human_looks_FINAL.csv")


## Now saccades (ONLY target)
saccadeDF[, `:=`(`vstim-Unrelated1` = NULL,
                 `vstim-Unrelated2` = NULL, `vstim-Unrelated3` = NULL,
                 `vstim-Unrelated4` = NULL, `vstim-Rhyme` = NULL,
                 `vstim-Cohort` = NULL, unrel = NULL,
                 anyTarget = NULL, rt = NULL)]
saccadeDF[, sacid := rleid(saccadenum), by = trial]

sac_test <- copy(saccadeDF)

sac_split <- split(saccadeDF, by = "trial")

sac_list <- lapply(sac_split, function(x) {
  sac_time <- c(0, Reduce(`+`, rle(x$sacid)$lengths, accumulate = TRUE))
  x <- x[sac_time, .(time, subject, StartTime, `vstim-Target`)]


})
sacs <- rbindlist(sac_list)

## Set first saccade to be at 0
sacs$StartTime <- ifelse(is.na(sacs$StartTime), 0, sacs$StartTime)
sacs[, `:=`(target = `vstim-Target`, `vstim-Target` = NULL)]

sacs[, group := "A"]

fwrite(sacs, "~/dissertation/data/bob_trace_data/human_saccades_FINAL.csv")


### TEST
sac_test_list <- lapply(sac_split, function(x) {
  sac_time <- c(0, Reduce(`+`, rle(x$sacid)$lengths, accumulate = TRUE))
  x <- x[sac_time, .(time, subject, StartTime, `vstim-Target`)]

  # Now create inflated entry (by plugging in last obs, usually at 2300 (like looks))
  nn <- nrow(x)
  y <- x[nn, ]
  y$StartTime <- y$time
  x <- rbind(x, y)
  x
})
sacs <- rbindlist(sac_test_list)

## Set first saccade to be at 0
sacs$StartTime <- ifelse(is.na(sacs$StartTime), 0, sacs$StartTime)
sacs[, `:=`(target = `vstim-Target`, `vstim-Target` = NULL)]

sacs[, group := "A"]

fwrite(sacs, "~/dissertation/data/bob_trace_data/human_saccades_FINAL_INFLATE.csv")
