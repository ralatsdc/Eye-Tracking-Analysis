## Pseudo code for handling eye-tracking files.
##
## Any correspondence between the variable and function names in this
## file and in reality is purely coincidental. Some variable and
## function names have been changed to preserve the pedagogy.

dropbox.dir <- path-to-your-dropbox-folder
data.dir <- path-to-the-project-data-folder

inp.files <- list.files.in(dropbox.dir)

for (inp.file in inp.files) {
  out.file <- replace(inp.file, "this", "that")

  if (!exist(out.file)) {
    data.full <- read(file)
    data.part <- select(data)
    write(data.part, out.file)

  } else {
    data.part <- read(out.file)

  }
}
