

tt <- looks[trial == 53981, ]

rr <- dts[trial == 53981, ]


rle(tt$saccadenum)

rr <- looks[trial == 80679, ]

##
tt1 <- looks[subject == 30, ]
tt2 <- dts[subject == 30, ]

tt1[, target := `vstim-Target`]
tt2[, target := `vstim-Target`]

tt1[, group := "group"]
tt2[, group := "group"]

tt1 <- tt1[, .(subject, time, target2, group)]
tt1 <- unique(tt1)

f1 <- bdotsFit(tt1, "subject", "time", "target2", "group", 
               logistic())

tt2 <- tt2[starttime <= 2000, ]

#tt2 <- tt2[NoData == 0 & None == 0, ]

f2 <- bdotsFit(tt2, "subject", "starttime", "target", "group", 
               logistic())

tt3 <- tt2[NoData == 0, ]
f3 <- bdotsFit(tt3, "subject", "starttime", "target", "group", 
               logistic())

tt4 <- tt2[None == 0, ]
f4 <- bdotsFit(tt4, "subject", "starttime", "target", "group", 
               logistic())


time <- "starttime"
y <- "target"
dat <- sacs[subject == 37, ]


table(sacs[subject == 30, target])
