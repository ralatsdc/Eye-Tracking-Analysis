### EyeTrackingAnalysis
###
### TODO: Complete
### 
### Copyright (c) 2014 Jessica E. and Raymond A. LeClair
### 
### This program can be redistributed and/or modified under the terms
### of the MIT License as published by the Open Source Initiative.
### 
### See the LICENSE file or http://opensource.org/licenses/MIT.

kobe.file <- "./Data/Kobe-Social-Cue-1/case_data.csv"
ucsb.file <- "./Data/UCSB-Social-Cue-1/case_data.csv"

selectData <- function() {

  ## The Kobe data set contains the following fields, after processing:
  ## 
  ## subject
  ## trialNumber
  ## RTTime
  ## trialType
  ## SOA
  ## TrialTypeFG
  ## TrialTypeBG
  ## Latency
  kobe.data <- loadData(kobe.file)

  ## The UCSB data set contains the following fields. Those marked with
  ## a "+" are selected, those marked with a "-" are not selected.
  ##
  ## - ExperimentName
  ## + Subject
  ## + Session
  ## - Birthdate
  ## - Clock.Information
  ## - DataFile.Basename
  ## - Display.RefreshRate
  ## - ExperimentVersion
  ## - Group
  ## - Handedness
  ## - RandomSeed
  ## - RuntimeVersion
  ## - RuntimeVersionExpected
  ## - SessionDate
  ## - SessionStartDateTimeUtc
  ## - SessionTime
  ## - Sex
  ## - StudioVersion
  ## - Block
  ## - pracBlockList
  ## - pracBlockList.Cycle
  ## - pracBlockList.Sample
  ## - Procedure[Block]
  ## - Running[Block]
  ## - testBlockList
  ## - testBlockList.Cycle
  ## - testBlockList.Sample
  ## - Trial
  ## - ConditionSelector1000
  ## - ConditionSelector1000Prac
  ## - ConditionSelector200
  ## - ConditionSelector200Prac
  ## - ConditionSelector600
  ## - ConditionSelector600Prac
  ## - ConditionSelectorCatch
  ## - ConditionSelectorCatchPrac
  ## - correctResponse
  ## + cueDur
  ## - cueImage1
  ## - cueImage2
  ## - cueImage3
  ## - cueImage4
  ## - cueImage5
  ## - cueingTrialList
  ## - cueingTrialList.Cycle
  ## - cueingTrialList.Sample
  ## - cueSlide.ACC
  ## - cueSlide.CRESP
  ## - cueSlide.DurationError
  ## - cueSlide.OnsetDelay
  ## - cueSlide.OnsetTime
  ## - cueSlide.OnsetToOnsetTime
  ## - cueSlide.RESP
  ## + cueSlide.RT
  ## + cueSlide.RTTime
  ## - DirectionBG
  ## - DirectionFG
  ## - face
  ## - faceSelector
  ## - neutralImage1
  ## - neutralImage2
  ## - neutralImage3
  ## - neutralImage4,
  ## - neutralImage5
  ## - pracTrialList
  ## - pracTrialList.Cycle
  ## - pracTrialList.Sample
  ## - Procedure[Trial]
  ## - Running[Trial]
  ## - TargetAOI
  ## - targetSlide.ACC
  ## - targetSlide.CRESP
  ## - targetSlide.DurationError
  ## - targetSlide.OnsetDelay
  ## - targetSlide.OnsetTime
  ## - targetSlide.OnsetToOnsetTime
  ## - targetSlide.RESP
  ## + targetSlide.RT
  ## + targetSlide.RTTime
  ## - tgtDur
  ## - tgtLeft
  ## - tgtRight
  ## - TrialName
  ## + TrialTypeBG
  ## + TrialTypeFG

  ucsb.data <- subset(
    loadData(ucsb.file),
    select=c(
      Subject, Session, cueDur, cueSlide.RT, cueSlide.RTTime,
      targetSlide.RT, targetSlide.RTTime, TrialTypeBG, TrialTypeFG))

  return(list(kobe=kobe.data, ucsb=ucsb.data))
}
  
loadData <- function(text.file) {
  ## Reads the data in the text file, if a CSV file which exists, then
  ## saves the data as an RData file, or loads a previously saved
  ## RData file.

  ## Ensure the text file is a CSV file which exists
  if (file_ext(text.file) != "csv") {
    message(paste(text.file, "must be a 'csv' file"))
    return()
  }
  if (!file.exists(text.file)) {
    message(paste(text.file,"does not exist"))
    return()
  }

  ## Name the RData file, and check if it exists
  data.file <- paste0(file_path_sans_ext(text.file), ".RData")
  if (!file.exists(data.file)) {

    ## The RData file does not exist, so read from the CSV file, and
    ## save to the RData file
    data <- read.csv(text.file)
    save(data, file=data.file)
    message(paste("read", text.file, "and saved", data.file))

  } else {

    ## The RData file does exist, so load from the RData file
    load(data.file)
    message(paste("loaded", data.file))
  }
  data
}
