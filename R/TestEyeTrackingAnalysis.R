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

test_that("loadData reads and saves, or loads", {
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
