#--------------------------------------------------------------------#
#                 Read spirometry image files: Utils                 #
#--------------------------------------------------------------------#

### Function to get the appropriate magick::geometry_area call for a given cell
get_cell_geometry <- function(row, col){
  
  # x offset - Column widths are different so we need to set manually
  x_offset <- dplyr::case_when(
    col == 3 ~ 237,
    col == 4 ~ 330,
    col == 5 ~ 415
  )
  
  # y offset - it's an arithmetic progression since all cell heights are the same
  y_offset <- 4 + 24*(row-1)
  # unfortunately we have to adjust a bit after row 9
  y_offset <- ifelse(row >= 9, y_offset + 2, y_offset)
  
  # Return geometry area call
  magick::geometry_area(width = 75, height = 20, x_off = x_offset, y_off = y_offset)
}

ocr_options <- function(txt, zoom, thresh, engine){
  
  txt_ocr <- image_resize(txt, geometry_area(width = 75*zoom, height = 20*zoom))
  txt_ocr <- image_threshold(txt_ocr, threshold = thresh, type = "black")
  txt_ocr <- ocr(txt_ocr, engine = engine)
  txt_ocr <- gsub("\n", "" , txt_ocr) # line breaks
  txt_ocr <- gsub(" ", "" , txt_ocr) # empty
  txt_ocr
}

### Function to read the content of a cell
read_cell <- function(img, row, col, engine){
  
  # Argument check
  if(!row %in% 2:11){stop("Only rows 2-11 are relevant")}
  if(!col %in% 3:4){stop("Only columns 3-4 are relevant")}
  
  # Crop the image 
  img_cropped <- image_crop(img, get_cell_geometry(row, col))
  img_cropped
  # Try different configurations
  img_text <- ocr_options(img_cropped, 2, "60%", engine = engine)
  
  # Coerce to numeric, if "" or "n/a" then NA otherwise throw 9999 as an error value
  return(tryCatch(ifelse(img_text == "n/a", NA, as.numeric(img_text)), warning = function(cond){9999}))
}

### Function to generate results table for a given image
generate_spiro_table <- function(path){
  
  # Read image and transform to b/w
  img <- magick::image_read(path)
  img <- magick::image_convert(img, type = "grayscale")
  
  # Tesseract options 
  engine <- tesseract(options = list(tessedit_char_whitelist = ".0123456789"))
  
  # Does it have the right format?
  if(magick::image_info(img)$width != 558 | magick::image_info(img)$height != 272){
    stop(paste0("The image ", path, " does not have the right format"))
  }
  
  # Now we create the dataframe to extract the data
  extraction_df <- tibble::as_tibble(data.frame(expand.grid(row = 2:11, col = 4)))
  
  # And we read the data
  extraction_df$cell <- purrr::map2(.x = extraction_df$row, .y = extraction_df$col,
                                    .f = ~read_cell(row = .x, col = .y, img = img, engine = engine))  %>%
    unlist()
  
  # And we tidy the data to return it
  extraction_df <- extraction_df %>%
    dplyr::mutate(col = ifelse(col == 3, "pred", "test"),
                  row = dplyr::case_when(
      row == 2 ~ "fvc",
      row == 3 ~ "fev1",
      row == 4 ~ "fev1fvc",
      row == 5 ~ "fev3",
      row == 6 ~ "fev3fvc",
      row == 7 ~ "pef",
      row == 8 ~ "pef25",
      row == 9 ~ "pef2575",
      row == 10 ~ "pef50",
      row == 11 ~ "pef75"
    )) %>%
    dplyr::mutate(key = paste(row, col, sep = "_")) %>%
    dplyr::select(-row, -col)
  extraction_df <- tidyr::spread(extraction_df, key = "key", value = "cell")
  
  # Sometimes the algorithm misses the period, correct if so
  extraction_df <- extraction_df %>%
    mutate(across(c("fev1fvc_test", "fev3fvc_test"),
                  ~ifelse(.x>100, .x/10, .x)))  %>%
    mutate(across(-c("fev1fvc_test", "fev3fvc_test"),
                   ~ifelse(.x>1000, .x/1000, .x)))
  
  return(extraction_df)
}