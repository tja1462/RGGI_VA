## Script to Generate Simple Codebook

# Initialize Settings -----------------------------------------------------

library(haven)
library(sjlabelled)

data <- read.csv('/data/final_dataset.csv')
get_labels(data)
