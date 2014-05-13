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

data.1 <- selectData()
data.2 <- assignFactors(data.1)
data.3 <- rejectOutliers(data.2)
data.4 <- createFactors(data.3)

# means <- computeMeans(data.3) # ".3" Intended
