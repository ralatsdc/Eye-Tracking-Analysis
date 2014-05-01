### TestEyeTrackingAnalysis
###
### Copyright (c) 2014 Jessica E. and Raymond A. LeClair
### 
### This program can be redistributed and/or modified under the terms
### of the MIT License as published by the Open Source Initiative.
### 
### See the LICENSE file or http://opensource.org/licenses/MIT.

source("EyeTrackingAnalysis.R")

data.1 <- selectData(kobe.file.1="../Data/Kobe-Social-Cue-1/case_data.csv",
                     kobe.file.2="../Data/Kobe-Social-Cue-2/case_data.csv",
                     ucsb.file.1="../Data/UCSB-Social-Cue-1/case_data.csv",
                     ucsb.file.2="../Data/UCSB-Social-Cue-2/case_data.csv")
data.2 <- assignFactors(data.1,
                        fact.file="../Data/treatment-ethnicity.csv")
data.3 <- rejectOutliers(data.2)

means <- computeMeans(data.3)

test_that("loadData accepts 'csv' files, which exist, only", {
  expect_that(loadData("../Data/case_list.dat"), shows_message("../Data/case_list.dat must be a 'csv' file"))
  expect_that(loadData("case_list.dat"), equals(NULL))
  expect_that(loadData("../Data/case-list.csv"), shows_message("../Data/case-list.csv does not exist"))
})

test_that("loadData reads and saves, or loads", {
  text.file <- "../Data/test_data.csv"
  data.file <- "../Data/test_data.RData"
  if (file.exists(data.file)) {
    file.remove(data.file)
  }
  expect_that(loadData(text.file), shows_message(paste("read", text.file, "and saved", data.file)))
  expect_that(loadData(text.file), shows_message(paste("loaded", data.file)))
})

test_that("loadData loads the expected data", {
  text.file <- "../Data/test_data.csv"
  data <- data.frame(
    subject=as.integer(c(1, 1, 1)),
    trialNumber=as.integer(c(1, 2, 3)),
    RTTime=as.integer(c(260747, 265472, 267725)),
    trialType=c("test", "test", "test"),
    SOA=as.integer(c(997, 200, 199)),
    TrialTypeFG=c("Neutral", "Congruent", "Congruent"),
    TrialTypeBG=c("Incongruent", "Neutral", "Incongruent"),
    Latency=as.integer(c(361, 460, 473)),
    X=c(NA, NA, NA))
  expect_that(loadData(text.file), equals(data))
})

test_that("selectData selects the first and last row of each Kobe or UCSB data set", {

  i.kobe <- 1
  expect_that(data.1$kobe$Subject[i.kobe], equals(1))
  expect_that(data.1$kobe$Session[i.kobe], equals(1))
  expect_that(data.1$kobe$TrialType[i.kobe],
              equals(factor("test", levels=c("catch", "test"))))
  expect_that(data.1$kobe$SOA[i.kobe], equals(997))
  expect_that(data.1$kobe$TrialTypeFG[i.kobe],
              equals(factor("Neutral", levels=c("catch", "Congruent", "Incongruent", "Neutral"))))
  expect_that(data.1$kobe$TrialTypeBG[i.kobe],
              equals(factor("Incongruent", levels=c("catch", "Congruent", "Incongruent", "Neutral"))))
  expect_that(data.1$kobe$Latency[i.kobe], equals(c(361)))

  i.kobe <- 19723
  expect_that(data.1$kobe$Subject[i.kobe], equals(2))
  expect_that(data.1$kobe$Session[i.kobe], equals(1))
  expect_that(data.1$kobe$TrialType[i.kobe],
              equals(factor("test", levels=c("catch", "test"))))
  expect_that(data.1$kobe$SOA[i.kobe], equals(199))
  expect_that(data.1$kobe$TrialTypeFG[i.kobe],
              equals(factor("Incongruent", levels=c("catch", "Congruent", "Incongruent", "Neutral"))))
  expect_that(data.1$kobe$TrialTypeBG[i.kobe],
              equals(factor("Neutral", levels=c("catch", "Congruent", "Incongruent", "Neutral"))))
  expect_that(data.1$kobe$Latency[i.kobe], equals(c(329)))

  i.kobe <- 19724
  expect_that(data.1$kobe$Subject[i.kobe], equals(1))
  expect_that(data.1$kobe$Session[i.kobe], equals(2))
  expect_that(data.1$kobe$TrialType[i.kobe],
              equals(factor("test", levels=c("catch", "test"))))
  expect_that(data.1$kobe$SOA[i.kobe], equals(598))
  expect_that(data.1$kobe$TrialTypeFG[i.kobe],
              equals(factor("Incongruent", levels=c("catch", "Congruent", "Incongruent", "Neutral"))))
  expect_that(data.1$kobe$TrialTypeBG[i.kobe],
              equals(factor("Incongruent", levels=c("catch", "Congruent", "Incongruent", "Neutral"))))
  expect_that(data.1$kobe$Latency[i.kobe], equals(c(0)))

  i.kobe <- 41482
  expect_that(data.1$kobe$Subject[i.kobe], equals(145))
  expect_that(data.1$kobe$Session[i.kobe], equals(2))
  expect_that(data.1$kobe$TrialType[i.kobe],
              equals(factor("test", levels=c("catch", "test"))))
  expect_that(data.1$kobe$SOA[i.kobe], equals(1017))
  expect_that(data.1$kobe$TrialTypeFG[i.kobe],
              equals(factor("Incongruent", levels=c("catch", "Congruent", "Incongruent", "Neutral"))))
  expect_that(data.1$kobe$TrialTypeBG[i.kobe],
              equals(factor("Neutral", levels=c("catch", "Congruent", "Incongruent", "Neutral"))))
  expect_that(data.1$kobe$Latency[i.kobe], equals(c(254)))

  i.ucsb <- 1
  expect_that(data.1$ucsb$Subject[i.ucsb], equals(1))
  expect_that(data.1$ucsb$Session[i.ucsb], equals(1))
  expect_that(data.1$ucsb$TrialName[i.ucsb],
              equals(factor("practice", levels=c("catch", "pracCatch", "practice", "test"))))
  expect_that(data.1$ucsb$CueSlide.RT[i.ucsb], equals(0))
  expect_that(data.1$ucsb$CueDur[i.ucsb], equals(1000))
  expect_that(data.1$ucsb$TrialTypeBG[i.ucsb],
              equals(factor("Incongruent", levels=c("catch", "Congruent", "Incongruent"))))
  expect_that(data.1$ucsb$TrialTypeFG[i.ucsb],
              equals(factor("Congruent", levels=c("catch", "Congruent", "Incongruent"))))
  expect_that(data.1$ucsb$TargetSlide.RT[i.ucsb], equals(0))

  i.ucsb <- 12150
  expect_that(data.1$ucsb$Subject[i.ucsb], equals(1))
  expect_that(data.1$ucsb$Session[i.ucsb], equals(1))
  expect_that(data.1$ucsb$TrialName[i.ucsb],
              equals(factor("test", levels=c("catch", "pracCatch", "practice", "test"))))
  expect_that(data.1$ucsb$CueSlide.RT[i.ucsb], equals(0))
  expect_that(data.1$ucsb$CueDur[i.ucsb], equals(200))
  expect_that(data.1$ucsb$TrialTypeBG[i.ucsb],
              equals(factor("Incongruent", levels=c("catch", "Congruent", "Incongruent"))))
  expect_that(data.1$ucsb$TrialTypeFG[i.ucsb],
              equals(factor("Congruent", levels=c("catch", "Congruent", "Incongruent"))))
  expect_that(data.1$ucsb$TargetSlide.RT[i.ucsb], equals(284))

  i.ucsb <- 12151
  expect_that(data.1$ucsb$Subject[i.ucsb], equals(1))
  expect_that(data.1$ucsb$Session[i.ucsb], equals(2))
  expect_that(data.1$ucsb$TrialName[i.ucsb],
              equals(factor("practice", levels=c("catch", "pracCatch", "practice", "test"))))
  expect_that(data.1$ucsb$CueSlide.RT[i.ucsb], equals(0))
  expect_that(data.1$ucsb$CueDur[i.ucsb], equals(1000))
  expect_that(data.1$ucsb$TrialTypeBG[i.ucsb],
              equals(factor("Congruent", levels=c("catch", "Congruent", "Incongruent"))))
  expect_that(data.1$ucsb$TrialTypeFG[i.ucsb],
              equals(factor("Congruent", levels=c("catch", "Congruent", "Incongruent"))))
  expect_that(data.1$ucsb$TargetSlide.RT[i.ucsb], equals(705))

  i.ucsb <- 26190
  expect_that(data.1$ucsb$Subject[i.ucsb], equals(82))
  expect_that(data.1$ucsb$Session[i.ucsb], equals(2))
  expect_that(data.1$ucsb$TrialName[i.ucsb],
              equals(factor("test", levels=c("catch", "pracCatch", "practice", "test"))))
  expect_that(data.1$ucsb$CueSlide.RT[i.ucsb], equals(0))
  expect_that(data.1$ucsb$CueDur[i.ucsb], equals(1000))
  expect_that(data.1$ucsb$TrialTypeBG[i.ucsb],
              equals(factor("Congruent", levels=c("catch", "Congruent", "Incongruent"))))
  expect_that(data.1$ucsb$TrialTypeFG[i.ucsb],
              equals(factor("Incongruent", levels=c("catch", "Congruent", "Incongruent"))))
  expect_that(data.1$ucsb$TargetSlide.RT[i.ucsb], equals(392))

})

test_that("assignFactors assigns the expected treatment and ethnicity", {

  data.1 <- selectData(kobe.file.1="../Data/Kobe-Social-Cue-1/case_data.csv",
                       kobe.file.2="../Data/Kobe-Social-Cue-2/case_data.csv",
                       ucsb.file.1="../Data/UCSB-Social-Cue-1/case_data.csv",
                       ucsb.file.2="../Data/UCSB-Social-Cue-2/case_data.csv")
  data.2 <- assignFactors(data.1,
                          fact.file="../Data/treatment-ethnicity.csv")
  
  expect_that(unique(data.2$ucsb$Treatment[data.2$ucsb$Subject == 10 & data.2$ucsb$Session == 1]),
              equals(factor("Placebo", levels=c("Oxytocin", "Placebo"))))
  expect_that(unique(data.2$ucsb$Treatment[data.2$ucsb$Subject == 10 & data.2$ucsb$Session == 2]),
              equals(factor("Oxytocin", levels=c("Oxytocin", "Placebo"))))
  expect_that(unique(data.2$ucsb$Treatment[data.2$ucsb$Subject == 82 & data.2$ucsb$Session == 1]),
              equals(factor("Oxytocin", levels=c("Oxytocin", "Placebo"))))
  expect_that(unique(data.2$ucsb$Treatment[data.2$ucsb$Subject == 82 & data.2$ucsb$Session == 2]),
              equals(factor("Placebo", levels=c("Oxytocin", "Placebo"))))

  expect_that(unique(data.2$kobe$Treatment[data.2$kobe$Subject == 101 & data.2$kobe$Session == 1]),
              equals(factor("Placebo", levels=c("Oxytocin", "Placebo"))))
  expect_that(unique(data.2$kobe$Treatment[data.2$kobe$Subject == 101 & data.2$kobe$Session == 2]),
              equals(factor("Oxytocin", levels=c("Oxytocin", "Placebo"))))
  expect_that(unique(data.2$kobe$Treatment[data.2$kobe$Subject == 145 & data.2$kobe$Session == 1]),
              equals(factor("Oxytocin", levels=c("Oxytocin", "Placebo"))))
  expect_that(unique(data.2$kobe$Treatment[data.2$kobe$Subject == 145 & data.2$kobe$Session == 2]),
              equals(factor("Placebo", levels=c("Oxytocin", "Placebo"))))

  expect_that(unique(data.2$ucsb$Ethnicity[data.2$ucsb$Subject == 10]),
              equals(factor("European American", levels=c("Asian American", "European American", "Japanese"))))
  expect_that(unique(data.2$ucsb$Ethnicity[data.2$ucsb$Subject == 82]),
              equals(factor("Asian American", levels=c("Asian American", "European American", "Japanese"))))

  expect_that(unique(data.2$kobe$Ethnicity[data.2$kobe$Subject == 101]),
              equals(factor("Japanese", levels=c("Asian American", "European American", "Japanese"))))

})


test_that("rejectOutliers returns a value without outliers", {

  threshold <- 100
  
  expect_that("catch" %in% data.3$kobe$TrialType, equals(FALSE))
  expect_that(sum(data.3$kobe$Latency < threshold), equals(0))
  expect_that(sum(data.3$kobe$Latency > 3.0 * sd(data.2$kobe$Latency, na.rm=TRUE)), equals(0))
  
  expect_that(sum(data.3$ucsb$CueSlide.RT > 0), equals(0))
  expect_that(sum(data.3$ucsb$TargetSlide.RT < threshold), equals(0))
  expect_that(sum(data.3$ucsb$TargetSlide.RT > 3.0 * sd(data.2$ucsb$TargetSlide.RT, na.rm=TRUE)), equals(0))

})

test_that("getBinIdx gets bin indexes by named intervals", {

  data <- list()
  bin.idx <- vector()

  expect_that(getBinIdx(data, ""), equals(bin.idx))
  expect_that(getBinIdx(data, ""), shows_message("No expected measurement found in the site data"))

  data <- list()
  data$SOA <- c(200, 600, 1000)

  expect_that(getBinIdx(data, "100"), shows_message("Unexpected named interval"))

  bin.idx <- c(TRUE, FALSE, FALSE)
  expect_that(getBinIdx(data, "200"), equals(bin.idx))

  bin.idx <- c(FALSE, TRUE, FALSE)
  expect_that(getBinIdx(data, "600"), equals(bin.idx))

  bin.idx <- c(FALSE, FALSE, TRUE)
  expect_that(getBinIdx(data, "1000"), equals(bin.idx))

  data <- list()
  data$CueDur <- c(200, 600, 1000)

  bin.idx <- c(TRUE, FALSE, FALSE)
  expect_that(getBinIdx(data, "200"), equals(bin.idx))

  bin.idx <- c(FALSE, TRUE, FALSE)
  expect_that(getBinIdx(data, "600"), equals(bin.idx))

  bin.idx <- c(FALSE, FALSE, TRUE)
  expect_that(getBinIdx(data, "1000"), equals(bin.idx))

})

test_that("computeSubjectMean computes the subject mean for one case", {

  subject.mean <- computeSubjectMean(data.2$kobe, "200", "Oxytocin", 1)
          
  ## ./Eye-Tracking-Analysis/Data/Kobe-Social-Cue-1/test_computeSubjectMean.xlsx
  expect_that(subject.mean$Subject, equals(1))
  expect_that(subject.mean$Treatment,
              equals(factor("Oxytocin", levels=c("Oxytocin"))))
  expect_that(subject.mean$Ethnicity,
              equals(factor("Japanese", levels=c("Asian American", "European American", "Japanese"))))
  expect_that(subject.mean$Bin,
              equals(factor("200", levels=c("200"))))
  expect_that(subject.mean$I.I - 318.45, is_less_than(1.0e-06))
  expect_that(subject.mean$C.C - 315.80, is_less_than(1.0e-06))
  expect_that(subject.mean$SCI.Matched - (318.45 - 315.80), is_less_than(1.0e-06))
  expect_that(subject.mean$I.C - 377.16, is_less_than(1.0e-06))
  expect_that(subject.mean$C.I - 337.14, is_less_than(1.0e-02))
  expect_that(subject.mean$SCI.Unmatched - (377.16 - 337.14), is_less_than(1.0e-06))

})

test_that("computeSubjectMeans returns the expected columns and number of rows", {

  expect_that(names(means$ucsb),
              equals(c("Bin", "Ethnicity", "Treatment", "Subject",
                       "I.I", "C.C", "SCI.Matched", "I.C", "C.I",
                       "SCI.Unmatched")))
  expect_that(length(means$ucsb$Subject), equals(6 * length(unique(data.3$ucsb$Subject))))

})

test_that("computeGrandMean computes the grand mean for one case", {

  grand.mean <- computeGrandMean(rbind(means$kobe, means$ucsb), "200", "Asian American", "Oxytocin")

  ## ./Eye-Tracking-Analysis/Data/means.subject.xlsx
  expect_that(grand.mean$Bin,
              equals(factor("200", levels=c("200"))))
  expect_that(grand.mean$Ethnicity,
              equals(factor("Asian American", levels=c("Asian American"))))
  expect_that(grand.mean$Treatment,
              equals(factor("Oxytocin", levels=c("Oxytocin"))))
  expect_that(grand.mean$mean.SCI.Matched - 8.983463553, is_less_than(1.0e-06))
  expect_that(grand.mean$mean.SCI.Unmatched - 5.177205929, is_less_than(1.0e-06))

})

test_that("computeGrandMeans returns the expected columns", {

  expect_that(names(means$grand),
              equals(c("Bin", "Ethnicity", "Treatment",
                       "mean.SCI.Matched","mean.SCI.Unmatched")))

})
