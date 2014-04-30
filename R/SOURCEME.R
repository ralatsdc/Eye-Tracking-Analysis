### SOURCEME.R
###
### Source this file to process all eye tracking data.
### 
### Copyright (c) 2014 Jessica E. and Raymond A. LeClair
### 
### This program can be redistributed and/or modified under the terms
### of the MIT License as published by the Open Source Initiative.
### 
### See the LICENSE file or http://opensource.org/licenses/MIT.

source('./R/EyeTrackingAnalysis.R')

data <- selectData()
data <- assignFactors(data)
data <- rejectOutliers(data)

means <- computeMeans(data)
