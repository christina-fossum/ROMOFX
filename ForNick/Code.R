

library(tidyverse)
library(dplyr)
library(writexl)
library(knitr)
library(EnvStats)


# Header loading function
load_headers <- function(directory_path, pattern) {
  list_files <- list.files(path = directory_path, pattern = pattern, full.names = TRUE)
  data_list <- lapply(list_files, function(file) {
    file_data <- read.csv(file) %>% select(1:10) %>% filter(Visited == "True")
    file_data <- file_data %>% mutate(Comment = "")
  })
  return(do.call(rbind, data_list))
}


# Data Loading Function
load_data <- function(directory_path, pattern) {
  list_files <- list.files(path = directory_path, pattern = pattern, full.names = TRUE)
  data_list <- lapply(list_files, function(file) {
    if (file.info(file)$size == 0) return(NULL)
    file_data <- tryCatch({
      read.csv(file, skip = 3) %>% mutate(Comment = "")
    }, error = function(e) {
      message("Error reading file: ", file, " - skipping this file.")
      return(NULL)
    })
    return(file_data)
  })
  do.call(rbind, data_list[!sapply(data_list, is.null)])
}



# Set "directory_path" to source file location

directory_path <- ""


# load data (Surface Fuels - Fine)
fuelsfine_df <- load_data(directory_path, "_Surface Fuels - Fine.csv$")
fuelsfine_headers <-load_headers(directory_path, "_Surface Fuels - Fine.csv$")

# load data (Surface Fuels - 1000Hr)
fuels1000_df <- load_data(directory_path, "_Surface Fuels - 1000Hr.csv$")
fuels1000_headers <-load_headers(directory_path, "_Surface Fuels - 1000Hr.csv$")

# load data (Surface Fuels - Duff/Litter)
fuelsDL_df <- load_data(directory_path, "_Surface Fuels - Duff_Litter.csv$")
fuelsDL_headers <-load_headers(directory_path, "_Surface Fuels - Duff_Litter.csv$")