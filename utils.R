library(tidyverse)

safely_dir_create <- function(dir_name, gitignore = TRUE) {
if (!dir.exists(dir_name)) {
  dir.create(dir_name)
  message(dir_name, " folder created!")
}
}

safely_dir_create("raw_pdf_files")
safely_dir_create("intermediate-data")
