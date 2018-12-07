#Merge Precip Files
#
library(tidyverse)
library(readr)
library(purrr)
library(stringr)
data_path <- "big_data/siteInputs5_1"   # path to the data
files <- dir(data_path, pattern = "*.csv") # get file names
data <- data_frame(filename = files) %>% # create a data frame holding the file names
  mutate(file_contents = map(filename,          # read files into
                             ~ read_csv(file.path(data_path, .))) # a new data column
  ) %>% 
  unnest() %>% 
  mutate(WBIC = str_extract(filename, "[^_]+[$\\d]"))
write_csv(x = data,path = "big_data/precip.csv")
