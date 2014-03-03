
## Assign path to the dropbox and data folder
dropbox.dir <- "/your/path/to/your/drop/box/folder"
data.dir <- "/your/path/to/your/project/data/folder"

## List gaze data files
inp.files <- list.files(~1~)

## Initialize the gaze data list
data <- list()

## Consider each gaze data file
n.file <- length(inp.files)
for (i.file in seq(1, n.file)) {

  ## Assign the gaze data file as the input file
  inp.file <- inp.files[i.file]

  ## Name the output file in which to save selected gaze data
  out.file <- paste(data.dir, sub(~2~), sep="~3~")

  ## Check if the output file does, or does not exist
  if (!file.exists(out.file)) {

    ## The output file does not exist, so read the input file, select
    ## required data, and save the result to the output file
    
    print(paste("reading:", ~4~))
    data.full <- read.delim(~4~)

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
