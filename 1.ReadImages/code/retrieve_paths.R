#--------------------------------------------------------------------#
#               Get image paths for reference in analysis            #
#--------------------------------------------------------------------#

library("tidyverse")

# List all directories in the path
all_paths <- paste0(getwd(), "/", list.dirs("input"))

# Let's only keep subfolders
all_paths <- all_paths[substr(all_paths, nchar(all_paths)-5, nchar(all_paths)) == "_files"]

# Define output data frame with one instance per path, we get IDs and test number fairly easily
spiro_data <- tibble(path = all_paths) %>%
  dplyr::group_by(path) %>%
  dplyr::mutate(participantID = strsplit(path, "/")[[1]][length(strsplit(path, "/")[[1]])-1],
                testnum = substr(path, nchar(path)-6, nchar(path)-6)) %>%
  dplyr::ungroup() 

# Get rid of duplicates
spiro_data <- spiro_data %>%
  dplyr::mutate(com = paste0(participantID, testnum)) %>%
  dplyr::filter(!duplicated(com)) %>%
  dplyr::select(-com)

# We can now create image paths 
spiro_data <- spiro_data %>%
  dplyr::mutate(pathTable = paste0(path, "/_Table.png"),
                curve1 = paste0(path, "/0.png"),
                curve2 = paste0(path, "/1.png")) %>%
  dplyr::select(-path)
  
# And write to disk
write_csv(spiro_data, "output/image_paths.csv")