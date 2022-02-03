#--------------------------------------------------------------------#
#             Read spirometry image files: Run analysis              #
#--------------------------------------------------------------------#

# Required packages
library("magick") # Read and process images
library("tesseract") # OCR text extraction
library("tidyverse") # Data management

# Extraction utils
source("code/utils.R")

# Read table paths ----
tab_paths <- read_csv("output/image_paths.csv") %>%
  select(participantID, testnum, pathTable) %>%
  rename(path = pathTable)

# Run extraction ----
results <- purrr::map_df(tab_paths$path, generate_spiro_table)
spiro_data <- dplyr::bind_cols(tab_paths, results) %>%
  dplyr::select(-path)
  
# Write to disk ----
readr::write_csv(spiro_data, "output/spiro_tables.csv")