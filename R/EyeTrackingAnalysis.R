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

EyeTrackingAnalysis <- function() {
  ## TODO: Complete

  loadData <- function(text.file) {
    ## Reads the data in the text file, if a CSV file which exists,
    ## then saves the data as an RData file, or loads a previously
    ## saved RData file.

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

  list(loadData=loadData)
}
