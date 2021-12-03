library(devtools)
library(knitr)

build(
  pkg = ".",
  path = "./build",
  binary = FALSE,
  vignettes = TRUE,
  manual = FALSE,
  args = NULL,
  quiet = FALSE,
)

document(pkg = "build", roclets = NULL, quiet = FALSE)

knit(input="README.rmd", output = "README.md")





