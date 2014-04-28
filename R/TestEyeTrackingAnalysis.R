### TestEyeTrackingAnalysis
###
### TODO: Complete
### 
### Copyright (c) 2014 Jessica E. and Raymond A. LeClair
### 
### This program can be redistributed and/or modified under the terms
### of the MIT License as published by the Open Source Initiative.
### 
### See the LICENSE file or http://opensource.org/licenses/MIT.

source("EyeTrackingAnalysis.R")

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

test_that("loadData loads the data", {
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

test_that("selectData selects the first and last row", {

  data <- selectData(kobe.file.1="../Data/Kobe-Social-Cue-1/case_data.csv",
                     kobe.file.2="../Data/Kobe-Social-Cue-2/case_data.csv",
                     ucsb.file.1="../Data/UCSB-Social-Cue-1/case_data.csv",
                     ucsb.file.2="../Data/UCSB-Social-Cue-2/case_data.csv")

  i.kobe <- 1
  expect_that(data$kobe$Subject[i.kobe], equals(1))
  expect_that(data$kobe$Session[i.kobe], equals(1))
  expect_that(data$kobe$TrialType[i.kobe],
              equals(factor("test", levels=c("catch", "test"))))
  expect_that(data$kobe$SOA[i.kobe], equals(997))
  expect_that(data$kobe$TrialTypeFG[i.kobe],
              equals(factor("Neutral", levels=c("catch", "Congruent", "Incongruent", "Neutral"))))
  expect_that(data$kobe$TrialTypeBG[i.kobe],
              equals(factor("Incongruent", levels=c("catch", "Congruent", "Incongruent", "Neutral"))))
  expect_that(data$kobe$Latency[i.kobe], equals(c(361)))

  i.kobe <- 19723
  expect_that(data$kobe$Subject[i.kobe], equals(2))
  expect_that(data$kobe$Session[i.kobe], equals(1))
  expect_that(data$kobe$TrialType[i.kobe],
              equals(factor("test", levels=c("catch", "test"))))
  expect_that(data$kobe$SOA[i.kobe], equals(199))
  expect_that(data$kobe$TrialTypeFG[i.kobe],
              equals(factor("Incongruent", levels=c("catch", "Congruent", "Incongruent", "Neutral"))))
  expect_that(data$kobe$TrialTypeBG[i.kobe],
              equals(factor("Neutral", levels=c("catch", "Congruent", "Incongruent", "Neutral"))))
  expect_that(data$kobe$Latency[i.kobe], equals(c(329)))

  i.kobe <- 19724
  expect_that(data$kobe$Subject[i.kobe], equals(1))
  expect_that(data$kobe$Session[i.kobe], equals(2))
  expect_that(data$kobe$TrialType[i.kobe],
              equals(factor("test", levels=c("catch", "test"))))
  expect_that(data$kobe$SOA[i.kobe], equals(598))
  expect_that(data$kobe$TrialTypeFG[i.kobe],
              equals(factor("Incongruent", levels=c("catch", "Congruent", "Incongruent", "Neutral"))))
  expect_that(data$kobe$TrialTypeBG[i.kobe],
              equals(factor("Incongruent", levels=c("catch", "Congruent", "Incongruent", "Neutral"))))
  expect_that(data$kobe$Latency[i.kobe], equals(c(0)))

  i.kobe <- 41482
  expect_that(data$kobe$Subject[i.kobe], equals(145))
  expect_that(data$kobe$Session[i.kobe], equals(2))
  expect_that(data$kobe$TrialType[i.kobe],
              equals(factor("test", levels=c("catch", "test"))))
  expect_that(data$kobe$SOA[i.kobe], equals(1017))
  expect_that(data$kobe$TrialTypeFG[i.kobe],
              equals(factor("Incongruent", levels=c("catch", "Congruent", "Incongruent", "Neutral"))))
  expect_that(data$kobe$TrialTypeBG[i.kobe],
              equals(factor("Neutral", levels=c("catch", "Congruent", "Incongruent", "Neutral"))))
  expect_that(data$kobe$Latency[i.kobe], equals(c(254)))

  i.ucsb <- 1
  expect_that(data$ucsb$Subject[i.ucsb], equals(1))
  expect_that(data$ucsb$Session[i.ucsb], equals(1))
  expect_that(data$ucsb$CueDur[i.ucsb], equals(1000))
  expect_that(data$ucsb$targetSlide.RT[i.ucsb], equals(0))
  expect_that(data$ucsb$TrialTypeBG[i.ucsb],
              equals(factor("Incongruent", levels=c("catch", "Congruent", "Incongruent"))))
  expect_that(data$ucsb$TrialTypeFG[i.ucsb],
              equals(factor("Congruent", levels=c("catch", "Congruent", "Incongruent"))))

  i.ucsb <- 12150
  expect_that(data$ucsb$Subject[i.ucsb], equals(1))
  expect_that(data$ucsb$Session[i.ucsb], equals(1))
  expect_that(data$ucsb$CueDur[i.ucsb], equals(200))
  expect_that(data$ucsb$targetSlide.RT[i.ucsb], equals(284))
  expect_that(data$ucsb$TrialTypeBG[i.ucsb],
              equals(factor("Incongruent", levels=c("catch", "Congruent", "Incongruent"))))
  expect_that(data$ucsb$TrialTypeFG[i.ucsb],
              equals(factor("Congruent", levels=c("catch", "Congruent", "Incongruent"))))

  i.ucsb <- 12151
  expect_that(data$ucsb$Subject[i.ucsb], equals(1))
  expect_that(data$ucsb$Session[i.ucsb], equals(2))
  expect_that(data$ucsb$CueDur[i.ucsb], equals(1000))
  expect_that(data$ucsb$targetSlide.RT[i.ucsb], equals(705))
  expect_that(data$ucsb$TrialTypeBG[i.ucsb],
              equals(factor("Congruent", levels=c("catch", "Congruent", "Incongruent"))))
  expect_that(data$ucsb$TrialTypeFG[i.ucsb],
              equals(factor("Congruent", levels=c("catch", "Congruent", "Incongruent"))))

  i.ucsb <- 26190
  expect_that(data$ucsb$Subject[i.ucsb], equals(82))
  expect_that(data$ucsb$Session[i.ucsb], equals(2))
  expect_that(data$ucsb$CueDur[i.ucsb], equals(1000))
  expect_that(data$ucsb$targetSlide.RT[i.ucsb], equals(392))
  expect_that(data$ucsb$TrialTypeBG[i.ucsb],
              equals(factor("Congruent", levels=c("catch", "Congruent", "Incongruent"))))
  expect_that(data$ucsb$TrialTypeFG[i.ucsb],
              equals(factor("Incongruent", levels=c("catch", "Congruent", "Incongruent"))))

})

test_that("assignTreatment assigns the treatment", {

  data <- selectData(kobe.file.1="../Data/Kobe-Social-Cue-1/case_data.csv",
                     kobe.file.2="../Data/Kobe-Social-Cue-2/case_data.csv",
                     ucsb.file.1="../Data/UCSB-Social-Cue-1/case_data.csv",
                     ucsb.file.2="../Data/UCSB-Social-Cue-2/case_data.csv")
  data <- assignTreatment(data,
                          rand.file="../Data/treatment.csv")
  
  expect_that(unique(data$ucsb$Treatment[data$ucsb$Subject == 10 & data$ucsb$Session == 1]),
              equals(factor("Placebo", levels=c("Oxytocin", "Placebo"))))
  expect_that(unique(data$ucsb$Treatment[data$ucsb$Subject == 10 & data$ucsb$Session == 2]),
              equals(factor("Oxytocin", levels=c("Oxytocin", "Placebo"))))
  expect_that(unique(data$ucsb$Treatment[data$ucsb$Subject == 82 & data$ucsb$Session == 1]),
              equals(factor("Oxytocin", levels=c("Oxytocin", "Placebo"))))
  expect_that(unique(data$ucsb$Treatment[data$ucsb$Subject == 82 & data$ucsb$Session == 2]),
              equals(factor("Placebo", levels=c("Oxytocin", "Placebo"))))

  expect_that(unique(data$kobe$Treatment[data$kobe$Subject == 101 & data$kobe$Session == 1]),
              equals(factor("Placebo", levels=c("Oxytocin", "Placebo"))))
  expect_that(unique(data$kobe$Treatment[data$kobe$Subject == 101 & data$kobe$Session == 2]),
              equals(factor("Oxytocin", levels=c("Oxytocin", "Placebo"))))
  expect_that(unique(data$kobe$Treatment[data$kobe$Subject == 145 & data$kobe$Session == 1]),
              equals(factor("Oxytocin", levels=c("Oxytocin", "Placebo"))))
  expect_that(unique(data$kobe$Treatment[data$kobe$Subject == 145 & data$kobe$Session == 2]),
              equals(factor("Placebo", levels=c("Oxytocin", "Placebo"))))

})
