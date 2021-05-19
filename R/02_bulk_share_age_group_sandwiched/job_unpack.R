
# Only unpack simulation outputs

setwd("../..")
(files <- list.files(path = "R/0_berkeley_bulk_estimates", pattern = ".R$", full.names = T))

# Load functions
source(files[1])


# Load data
source(files[2])