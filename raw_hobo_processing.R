########################################################
# script to process raw HOBO files into non-awful format,
# with logger no, datetime, temperature, and conductivity
# as exported columns
########################################################
# This script is slightly modified from a script by 
# Stephen Cook for the StreamCLIMES project.
# Script works on a folder in your working directory
# containing the raw HOBO csvs
########################################################

library(tidyverse)

hobo_logger_files <- list.files(path = "C:/Users/...")

for(csv_file in hobo_logger_files){
  
  logger_record <- read.csv(file = paste0("C:/Users/Christopher/Desktop/R_Directory/raw_files/", csv_file),
                            skip = 1)
  
  if ("Date" %in% names(logger_record)) {
    logger_record$datetime <- mdy_hms(paste0(logger_record$Date, " ", logger_record$Time..GMT.05.00))
  } else {
    logger_record$datetime <- mdy_hms(logger_record$Date.Time..GMT.05.00)
  }
  
  print(paste("Read in file", csv_file, "to process."))
  
  logger_no <- gsub(".csv", "", csv_file)
  
  processed <- logger_record %>%
    mutate(logger = logger_no) %>%
    relocate(logger) %>%
    rename_with(.cols = contains("Temp"),
                .fn = function(x){"temperature"}) %>%
    rename_with(.cols = contains("Lux"),
                .fn = function(x){"conductivity"})%>%
    select(any_of(c("logger", "datetime", "temperature", "conductivity")))
  
  write.csv(processed, file = paste0("processed_files/", csv_file),
            row.names = FALSE)
  
  print(paste("Finished processing and writing new file to processed__", csv_file, "."))
}