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

assignTreatment <- function(ucsb.data,
                            rand.file="./Data/UCSB-Social-Cue-1/randomization.csv") {

  ## Ensure the data frame contains UCSB data
  if (!identical(names(ucsb.data),
                 c("Subject", "Session", "cueDur", "cueSlide.RT",
                   "cueSlide.RTTime", "targetSlide.RT",
                   "targetSlide.RTTime", "TrialTypeBG",
                   "TrialTypeFG"))) { 
    message("Unexpected names for UCSB data")
    return(ucsb.data)
  }

  ## Ensure two and only two sessions equal to one and two present in
  ## the UCSB data, since session is used as index below
  session.nmbrs <- c(1, 2)
  if (length(setdiff(unique(ucsb.data$Session), session.nmbrs)) > 0) {
    message("Unexpected session numbers in UCSB data")
    return(ucsb.data)
  }

  ## Load the treatment assignment data
  rand.data <- loadData(rand.file)

  ## Ensure the treatment assignment data contains the expected
  ## session names
  session.names <- c("Session.1", "Session.2")
  if (length(setdiff(session.names, names(rand.data))) > 0) {
    message("Unexpected names for randomization sessions")
    return(ucsb.data)
  }

  ## Ensure the treatment assignment data frame session factors have
  ## identical levels
  if (!identical(levels(rand.data$Session.1), levels(rand.data$Session.2))) {
    message("Levels for session one and two factors differ")
    return(ucsb.data)
  }
  
  ## Column bind a treatment factor to the UCSB data frame
  ucsb.data <- cbind(ucsb.data, Treatment=factor("", levels(rand.data$Session.1)))

  ## Consider each unique UCSB subject
  for (unq.subject in unique(ucsb.data$Subject)) {

    ## Ensure that the current unique subject is found once and only
    ## once in the treatment assignment data
    if (sum(rand.data$Subject == unq.subject) > 1) {
      message(sprintf("Found more than one entry for subject %d", unq.subject))
      return(ucsb.data)
    }

    ## Consider each unique UCSB session
    for (unq.session in unique(ucsb.data$Session)) {

      ## Assign the treatment for the current unique subject and
      ## session
      cur.treatment <- rand.data[[session.names[unq.session]]][rand.data$Subject == unq.subject]
      ucsb.data$Treatment[ucsb.data$Subject==unq.subject & ucsb.data$Session==unq.session] <- cur.treatment
     }      
  }
  ucsb.data
}

selectData <- function(kobe.file="./Data/Kobe-Social-Cue-1/case_data.csv",
                       ucsb.file.1="./Data/UCSB-Social-Cue-1/case_data.csv",
                       ucsb.file.2="./Data/UCSB-Social-Cue-2/case_data.csv") {
  ## Selects the required columns from the Kobe and UCSB social cue
  ## data.
  
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

  ## The UCSB data sets contains the following fields. Those marked with
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
  ucsb.data.1 <- subset(
    loadData(ucsb.file.1),
    subset=TRUE,
    select=c(
      Subject, Session, cueDur, cueSlide.RT, cueSlide.RTTime,
      targetSlide.RT, targetSlide.RTTime, TrialTypeBG, TrialTypeFG))
  ucsb.data.2 <- subset(
    loadData(ucsb.file.2),
    subset=TRUE,
    select=c(
      Subject, Session, cueDur, cueSlide.RT, cueSlide.RTTime,
      targetSlide.RT, targetSlide.RTTime, TrialTypeBG, TrialTypeFG))
  ucsb.data <- rbind(ucsb.data.1, ucsb.data.2)
  
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
