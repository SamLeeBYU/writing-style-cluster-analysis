# Read in the data
collins <- read.table(
  "https://tofu.byu.edu/docs/files/stat666/datasets/collins.txt",
  header = TRUE,
  sep = "\t",
  skip = 10,             # skip the 10 lines of description before the header
  check.names = FALSE
)