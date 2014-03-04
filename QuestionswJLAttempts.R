## Assign path to the dropbox and data folder
dropbox.dir <- "???"
data.dir <- "/Users/jessyleclair/Desktop/Kobe_SocialCue1"

## List gaze data files
inp.files <- list.files(dropbox.dir, "P9E1_v1.11-1-1.gazedata", full.names=TRUE)

## Initialize the gaze data list
data <- list()

## Consider each gaze data file
n.file <- length(inp.files)
for (i.file in seq("P9E1_v1.11-1-1.gazedata", n.file)) {
  
  ## Assign the gaze data file as the input file
  inp.file <- inp.files[i.file]
  
  ## Name the output file in which to save selected gaze data
  out.file <- paste(data.dir, sub("out.file"), sep="~3~")
  
  ## Check if the output file does, or does not exist
  if (!file.exists(out.file)) {
    
    ## The output file does not exist, so read the input file, select
    ## required data, and save the result to the output file
    
    print(paste("reading:", data.full, TRUE, select=c("Subject", "Session", "ID", "TETTime", "RTTime", "CursorX", "CursorY")))
    data.full <- read.delim(data.full, TRUE, select=c("Subject", "Session", "ID", "TETTime", "RTTime", "CursorX", "CursorY"))
    
    data.part <- subset(~5~)
    
    print(paste("saving:", ~6~))
    save(data.part, file=~6~)
    
  } else {
    
    ## The output file does exist, so load it
    print(paste("loading:", ~7~))
    load(~7~)
    
  }
  
  ## Assign the data selected from the current gaze data file to the
  ## data list
  data[[i.file]] <- data.part
  
}