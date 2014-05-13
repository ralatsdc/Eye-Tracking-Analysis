### EyeTrackingAnalysis
###
### Computes the mean Kobe Latency or UCSB TargetSlide.RT measurement
### for all subjects, treatments, and bins.
### 
### Gets bin indexes by named intervals using the Kobe SOA and the
### UCSB CueDur measurements.
###
### Reject trials in which a response was made prior to the target
### being presented, or in which the response time is below a
### threshold, or in which the response time is greater than three
### standard deviations of all measurements.
### 
### Assigns treatment and ethnicity factors to Kobe and UCSB data
### based on subject, session, and site.
### 
### Copyright (c) 2014 Jessica E. and Raymond A. LeClair
### 
### This program can be redistributed and/or modified under the terms
### of the MIT License as published by the Open Source Initiative.
### 
### See the LICENSE file or http://opensource.org/licenses/MIT.

computeMeans <- function(data) {
  ## Compute the subject means for the Kobe and UCSB data sets
  ## separately.
  
  ## Initialize the return value
  means <- list()

  ## Compute the subject means for the Kobe data set
  means$kobe <- computeSubjectMeans(data$kobe)

  ## Compute the subject means for the UCSB data set
  means$ucsb <- computeSubjectMeans(data$ucsb)

  ## Write the subject means
  write.csv(rbind(means$kobe, means$ucsb), "means.subject.csv")

  ## Compute the grand means for the combined Kobe and UCSB subject
  ## means
  means$grand <- computeGrandMeans(rbind(means$kobe, means$ucsb))

  ## Write the grand means
  write.csv(means$grand, "means.grand.csv")

  means
}

computeGrandMeans <- function(subject.means) {
  ## Compute the grand mean SCI for the matched and unmatched
  ## congruence cases for all bins, ethnicities, and treatments.

  ## Initialize the return value
  grand.means <- data.frame()

  ## Assign named bins
  bins <- c("200", "600", "1000")

  ## Consider each bin
  for (bin in bins) {
        
    ## Consider each unique ethnicity
    for (ethnicity in unique(subject.means$Ethnicity)) {
         
      ## Consider each unique treatment
      for (treatment in unique(subject.means$Treatment)) {

        ## Collect all grand means for each bin, ethnicity, and
        ## treatment
        grand.means <- rbind(grand.means,
                             computeGrandMean(subject.means, bin, ethnicity, treatment))
      }
    }
  }
  grand.means
}

computeGrandMean <- function(subject.means, bin, ethnicity, treatment) {
  ## Compute the grand mean SCI for the matched and unmatched
  ## congruence cases for a given bin, ethnicity, and treatment
  
  ## Initialize the return value
  grand.mean <- data.frame()
  
  ## Identify the bin, ethnicity, and treatment
  bin.idx <- subject.means$Bin == bin
  ethnicity.idx <- subject.means$Ethnicity == ethnicity
  treatment.idx <- subject.means$Treatment == treatment
  subject.means.idx <- bin.idx & ethnicity.idx & treatment.idx

  ## Compute the grand mean SCI for the matched and unmatched
  ## congruence cases
  mean.SCI.Matched <- mean(subject.means$SCI.Matched[subject.means.idx], na.rm=TRUE)
  mean.SCI.Unmatched <- mean(subject.means$SCI.Unmatched[subject.means.idx], na.rm=TRUE)

  ## Assemble the results as a data frame
  grand.mean <- data.frame(Bin=bin, Ethnicity=ethnicity, Treatment=treatment,
                           mean.SCI.Matched=mean.SCI.Matched, mean.SCI.Unmatched=mean.SCI.Unmatched)
}

computeSubjectMeans <- function(site.data) {
  ## Compute the mean Kobe Latency or UCSB TargetSlide.RT measurement
  ## for all bins, treatments, and subjects.

  ## Initialize the return value
  subject.means <- data.frame()

  ## Assign named bins
  bins <- c("200", "600", "1000")

  ## Consider each bin
  for (bin in bins) {
        
    ## Consider each unique treatment
    for (treatment in unique(site.data$Treatment)) {

      ## Consider each unique subject
      for (subject in unique(site.data$Subject)) {

        ## Collect all subject means for each bin, treatment, and subject
        subject.means <- rbind(subject.means,
                               computeSubjectMean(site.data, bin, treatment, subject))
      }
    }
  }
  subject.means
}

computeSubjectMean <- function(site.data, bin, treatment, subject) {
  ## Compute the mean Kobe Latency or UCSB TargetSlide.RT measurement
  ## for a given bin, treatment, and subject.
  
  ## Initialize the return value
  subject.mean <- data.frame()
  
  ## Ensure the expected measurement is found in the site data
  if ("Latency" %in% names(site.data)) {
    measurement <- "Latency"
  } else if ("TargetSlide.RT" %in% names(site.data)) {
    measurement <- "TargetSlide.RT"
  } else {
    message("No expected measurement found in the site data")
    return(subject.mean)
  }

  ## Identify the bin, treatment, and subject
  bin.idx <- getBinIdx(site.data, bin)
  treatment.idx <- site.data$Treatment == treatment
  subject.idx <- site.data$Subject == subject
  site.data.idx <- bin.idx & treatment.idx & subject.idx

  ## Identify the ethnicity
  ethnicity <- site.data$Ethnicity[site.data.idx][1]

  ## Identify the foreground-background congruence cases
  i.i.idx <- site.data$TrialTypeFG == "Incongruent" & site.data$TrialTypeBG == "Incongruent"
  c.c.idx <- site.data$TrialTypeFG == "Congruent"  & site.data$TrialTypeBG == "Congruent"
  i.c.idx <- site.data$TrialTypeFG == "Incongruent" & site.data$TrialTypeBG == "Congruent"
  c.i.idx <- site.data$TrialTypeFG == "Congruent"  & site.data$TrialTypeBG == "Incongruent"

  ## Compute the means for the foreground-background congruence cases
  i.i.mean <- mean(site.data[[measurement]][site.data.idx & i.i.idx], na.rm=TRUE)
  c.c.mean <- mean(site.data[[measurement]][site.data.idx & c.c.idx], na.rm=TRUE)
  i.c.mean <- mean(site.data[[measurement]][site.data.idx & i.c.idx], na.rm=TRUE)
  c.i.mean <- mean(site.data[[measurement]][site.data.idx & c.i.idx], na.rm=TRUE)
  
  ## Assemble the results as a data frame
  subject.mean <- data.frame(Bin=bin, Ethnicity=ethnicity, Treatment=treatment, Subject=subject,
                             I.I=i.i.mean, C.C=c.c.mean, SCI.Matched=i.i.mean - c.c.mean,
                             I.C=i.c.mean, C.I=c.i.mean, SCI.Unmatched=i.c.mean - c.i.mean)
}

createFactors <- function(data) {
  ## Creates factors from integer Subject and Latency columns, integer
  ## SOA or CueDur column by binning, and trial type columns by
  ## combination.

  ## data$kobe
  data$kobe$Subject <- as.factor(data$kobe$Subject)
  ## Session
  ## TrialType

  SOA <- vector(length=length(data$kobe$SOA))
  bins <- c("200", "600", "1000")
  for (bin in bins) {
    SOA[getBinIdx(data$kobe, bin)] <- bin
  }
  data$kobe$SOA <- as.factor(SOA)
  
  data$kobe$Congruence <- as.factor(paste(data$kobe$TrialTypeFG, data$kobe$TrialTypeBG, sep="-"))

  ## TrialTypeFG
  ## TrialTypeBG
  ## Latency
  ## Treatment
  ## Ethnicity

  ## data$ucsb
  data$ucsb$Subject <- as.factor(data$ucsb$Subject)
  ## Session
  ## TrialName
  ## CueSlide.RT
  ## CueDur

  CueDur <- vector(length=length(data$ucsb$CueDur))
  bins <- c("200", "600", "1000")
  for (bin in bins) {
    CueDur[getBinIdx(data$ucsb, bin)] <- bin
  }
  data$ucsb$CueDur <- as.factor(CueDur)

  data$ucsb$Congruence <- as.factor(paste(data$ucsb$TrialTypeFG, data$ucsb$TrialTypeBG, sep="-"))

  ## TrialTypeBG
  ## TrialTypeFG
  ## TargetSlide.RT
  ## Treatment
  ## Ethnicity

  data
}

getBinIdx <- function(site.data, bin) {
  ## Get bin indexes by named intervals using the Kobe SOA and the
  ## UCSB CueDur measurements.
  
  ## Initialize the return value
  bin.idx <- vector()

  ## Ensure the expected measurement is found in the site data
  if ("SOA" %in% names(site.data)) {
    measurement <- "SOA"
  } else if ("CueDur" %in% names(site.data)) {
    measurement <- "CueDur"
  } else {
    message("No expected measurement found in the site data")
    return(bin.idx)
  }

  ## Get bin indexes by named intervals
  if (bin == "200") {
    bin.idx <- site.data[[measurement]] < 400
  } else if (bin == "600") {
    bin.idx <- 400 <= site.data[[measurement]] & site.data[[measurement]] < 800
  } else if (bin == "1000") {
    bin.idx <- 800 <= site.data[[measurement]]
  } else {
    message("Unexpected named interval")
    return(bin.idx)
  }
  bin.idx
}

rejectOutliers <- function(data) {
  ## Reject trials in which a response was made prior to the target
  ## being presented, or in which the response time is below a
  ## threshold, or in which the response time is greater than three
  ## standard deviations of all measurements.

  ## Assign the threshold
  threshold <- 100 # ms

  ## Filter the Kobe data
  data$kobe <- subset(data$kobe,
                      subset = ! (data$kobe$TrialType == "catch"
                                  | data$kobe$Latency < threshold
                                  | data$kobe$Latency > 3.0 * sd(data$kobe$Latency, na.rm=TRUE)))

  ## Filter the UCSB data
  data$ucsb <- subset(data$ucsb,
                      subset = ! (data$ucsb$CueSlide.RT > 0
                                  | data$ucsb$TargetSlide.RT < threshold
                                  | data$ucsb$TargetSlide.RT > 3.0 * sd(data$ucsb$TargetSlide.RT, na.rm=TRUE)))
  
  data
}

assignFactors <- function(data,
                          fact.file="./Data/treatment-ethnicity.csv") {
  ## Assigns treatment and ethnicity factors to Kobe and UCSB data
  ## based on subject, session, and site.
  
  ## Ensure two and only two sessions equal to one and two present in
  ## the Kobe and UCSB data, since session is used as index below
  session.nmbrs <- c(1, 2)
  if (length(setdiff(unique(data$kobe$Session), session.nmbrs)) > 0) {
    message("Unexpected session numbers in Kobe data")
    return(data)
  }
  if (length(setdiff(unique(data$ucsb$Session), session.nmbrs)) > 0) {
    message("Unexpected session numbers in UCSB data")
    return(data)
  }

  ## Load the treatment and ethnicity assignment data
  fact.data <- loadData(fact.file)

  ## Ensure the treatment and ethnicity assignment data contains the
  ## expected names
  session.names <- c("Session.1", "Session.2")
  fact.names <- c("Subject", session.names, "Ethnicity")
  if (length(setdiff(fact.names, names(fact.data))) > 0) {
    message("Unexpected names for factors")
    return(data)
  }
  
  ## Ensure the treatment and ethnicity assignment data frame session
  ## factors have identical levels
  if (!identical(levels(fact.data$Session.1), levels(fact.data$Session.2))) {
    message("Levels for session one and two factors differ")
    return(data)
  }
  
  ## Ensure the treatment and ethnicity assignment data frame
  ## ethnicity factor has the expected levels
  ethn.levels <- c("Asian American", "European American", "Japanese")
  if (!identical(levels(fact.data$Ethnicity), ethn.levels)) {
    message("Levels for ethnicity have unexpected values")
    return(data)
  }

  ## Column bind a treatment factor to the Kobe and UCSB data frame
  data$kobe <- cbind(data$kobe, Treatment=factor("", levels(fact.data$Session.1)))
  data$ucsb <- cbind(data$ucsb, Treatment=factor("", levels(fact.data$Session.1)))

  ## Column bind an ethnicity factor to the Kobe and UCSB data frame
  data$kobe <- cbind(data$kobe, Ethnicity=factor("", ethn.levels))
  data$ucsb <- cbind(data$ucsb, Ethnicity=factor("", ethn.levels))

  ## Consider each unique Kobe subject
  for (unq.subject in unique(data$kobe$Subject)) {

    ## Ensure that the current unique subject is found once and only
    ## once in the treatment assignment data
    fact.idx <- (fact.data$Subject == unq.subject
                 & fact.data$Ethnicity == "Japanese")
    if (sum(fact.idx) > 1) {
      message(sprintf("Found more than one entry for subject %d", unq.subject))
      return(data)
    }

    ## Assign the ethnicity for the current unique subject
    cur.ethnicity <- fact.data$Ethnicity[fact.idx]
    data.idx <- data$kobe$Subject == unq.subject
    data$kobe$Ethnicity[data.idx] <- cur.ethnicity

    ## Consider each unique Kobe session
    for (unq.session in unique(data$kobe$Session)) {

      ## Assign the treatment for the current unique subject and
      ## session
      cur.treatment <- fact.data[[session.names[unq.session]]][fact.idx]
      data.idx <- data$kobe$Subject == unq.subject & data$kobe$Session == unq.session
      data$kobe$Treatment[data.idx] <- cur.treatment
     }
  }

  ## Consider each unique UCSB subject
  for (unq.subject in unique(data$ucsb$Subject)) {

    ## Ensure that the current unique subject is found once and only
    ## once in the treatment assignment data
    fact.idx <- (fact.data$Subject == unq.subject
                 & (fact.data$Ethnicity == "Asian American"
                    | fact.data$Ethnicity == "European American"))
    if (sum(fact.idx) > 1) {
      message(sprintf("Found more than one entry for subject %d", unq.subject))
      return(data)
    }

    ## Assign the ethnicity for the current unique subject
    cur.ethnicity <- fact.data$Ethnicity[fact.idx]
    data.idx <- data$ucsb$Subject == unq.subject
    data$ucsb$Ethnicity[data.idx] <- cur.ethnicity

    ## Consider each unique UCSB session
    for (unq.session in unique(data$ucsb$Session)) {

      ## Assign the treatment for the current unique subject and
      ## session
      cur.treatment <- fact.data[[session.names[unq.session]]][fact.idx]
      data.idx <- data$ucsb$Subject == unq.subject & data$ucsb$Session == unq.session
      data$ucsb$Treatment[data.idx] <- cur.treatment
     }
  }
  data
}

selectData <- function(kobe.file.1="./Data/Kobe-Social-Cue-1/case_data.csv",
                       kobe.file.2="./Data/Kobe-Social-Cue-2/case_data.csv",
                       ucsb.file.1="./Data/UCSB-Social-Cue-1/case_data.csv",
                       ucsb.file.2="./Data/UCSB-Social-Cue-2/case_data.csv") {
  ## Selects the required columns from the Kobe and UCSB social cue
  ## data.
  
  ## The Kobe data set contains the following fields, after processing:
  ## 
  ## + subject: renamed "Subject"
  ## + session: renamed "Session"
  ## - trialNumber: dropped
  ## - RTTime: dropped
  ## + trialType: renamed TrialType
  ## + SOA
  ## + TrialTypeFG
  ## + TrialTypeBG
  ## + Latency
  kobe.data.1 <- subset(
    loadData(kobe.file.1),
    subset=TRUE,
    select=c(
      subject, session, trialType, SOA, TrialTypeFG, TrialTypeBG, Latency))
  colnames(kobe.data.1) <- c(
    "Subject", "Session", "TrialType", "SOA", "TrialTypeFG", "TrialTypeBG", "Latency")
  kobe.data.2 <- subset(
    loadData(kobe.file.2),
    subset=TRUE,
    select=c(
      subject, session, trialType, SOA, TrialTypeFG, TrialTypeBG, Latency))
  colnames(kobe.data.2) <- c(
    "Subject", "Session", "TrialType", "SOA", "TrialTypeFG", "TrialTypeBG", "Latency")
  kobe.data <- rbind(kobe.data.1, kobe.data.2)

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
  ## + cueDur: renamed "CueDur"
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
  ## - cueSlide.RTTime
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
  ## - targetSlide.RTTime
  ## - tgtDur
  ## - tgtLeft
  ## - tgtRight
  ## + TrialName
  ## + TrialTypeBG
  ## + TrialTypeFG
  ucsb.data.1 <- subset(
    loadData(ucsb.file.1),
    subset=TRUE,
    select=c(
      Subject, Session, TrialName, cueSlide.RT, cueDur, TrialTypeBG, TrialTypeFG, targetSlide.RT))
  colnames(ucsb.data.1) <- c(
    "Subject", "Session", "TrialName", "CueSlide.RT", "CueDur", "TrialTypeBG", "TrialTypeFG", "TargetSlide.RT")
  ucsb.data.2 <- subset(
    loadData(ucsb.file.2),
    subset=TRUE,
    select=c(
      Subject, Session, TrialName, cueSlide.RT, cueDur, TrialTypeBG, TrialTypeFG, targetSlide.RT))
  colnames(ucsb.data.2) <- c(
    "Subject", "Session", "TrialName", "CueSlide.RT", "CueDur", "TrialTypeBG", "TrialTypeFG", "TargetSlide.RT")
  ucsb.data <- rbind(ucsb.data.1, ucsb.data.2)
  
  list(kobe=kobe.data, ucsb=ucsb.data)
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
