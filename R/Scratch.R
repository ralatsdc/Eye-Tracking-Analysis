## Pseudo code for handling eye-tracking files.
##
## Any correspondence between the variable and function names in this
## file and in reality may be purely coincidental. Some variable and
## function names have been changed to preserve the pedagogy.

dropbox.dir <- path-to-your-dropbox-folder
data.dir <- path-to-the-project-data-folder

inp.files <- list.files(...)

data <- list()

n.file <- length(inp.files)
for (i.file in seq(1, n.file)) {
  inp.file <- inp.files[i.file]

  out.file <- paste(data.dir, sub(".gazedata", ".RData", basename(inp.file)), sep="/")
  if (!file.exists(out.file)) {

    print(paste("reading:", inp.file))
    data.full <- read.delim(...)

    data.part <- subset(data.full, ...)

    print(paste("saving:", out.file))
    save(data.part, ...)

  } else {
    print(paste("loading:", out.file))
    load(out.file)
  }
  data[[i.file]] <- data.part
}
