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

source('EyeTrackingAnalysis.R')

eta <- EyeTrackingAnalysis()

test_that("loadData accepts 'csv' files, which exist, only", {
  expect_that(eta$loadData("../Data/case_list.dat"), shows_message("../Data/case_list.dat must be a 'csv' file"))
  expect_that(eta$loadData("case_list.dat"), equals(NULL))
  expect_that(eta$loadData("../Data/case-list.csv"), shows_message("../Data/case-list.csv does not exist"))
})

test_that("loadData reads and saves, or loads", {

  ## subject,trialNumber,RTTime,trialType,SOA,TrialTypeFG,TrialTypeBG,Latency,
  ## 1,1,260747,test,997,Neutral,Incongruent,361,
  ## 1,2,265472,test,200,Congruent,Neutral,460,
  ## 1,3,267725,test,199,Congruent,Incongruent,473,

  text.file <- "../Data/test_data.csv"
  data.file <- "../Data/test_data.RData"
  if (file.exists(data.file)) {
    file.remove(data.file)
  }

  expect_that(eta$loadData(text.file), shows_message(paste("read", text.file, "and saved", data.file)))
  expect_that(eta$loadData(text.file), shows_message(paste("loaded", data.file)))

})
