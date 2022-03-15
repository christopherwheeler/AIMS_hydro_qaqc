# STIC_TestFunctions.R
# Script to test each function in the STIC processing workflow.

library(dplyr)
library(lubridate)

## Step 1: Tidy HOBO data
source("tidy_hobo_data.R")  # load function
raw_hobo_infile <- file.path("TestData", "STIC", "20946489_Raw.csv")
tidy_hobo_outfile <- file.path("TestData", "STIC", "20946489_Tidy.csv")

tidy_data <- tidy_hobo_data(infile = raw_hobo_infile) # test with no saved output
head(tidy_data)

tidy_data <- tidy_hobo_data(infile = raw_hobo_infile, outfile = tidy_hobo_outfile) # test with saved output

head(tidy_data)
file.exists(tidy_hobo_outfile)
