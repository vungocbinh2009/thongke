library(devtools)
library(here)
library(magrittr)

# This function merge all text file to 1 file
merge_files <- function(file_list, output) {
  output_file <- NULL
  for(file in file_list) {
    output_file <- c(output_file, readLines(file))
    output_file <- c(output_file, "\n")
  }
  writeLines(output_file, output)
}

### BEGIN SCRIPT
# Thực hiện các bước: Tìm folder src, list các file, rồi lấy đường dẫn các file này.
file_list <- here("src") %>% list.files() %>% here("src", .)

merge_files(file_list, here("R", "thongke.R"))

build(
  pkg = ".",
  path = "./build",
  binary = FALSE,
  vignettes = TRUE,
  manual = FALSE,
  args = NULL,
  quiet = FALSE,
)

document(pkg = ".", roclets = NULL, quiet = FALSE)

### END SCRIPT




