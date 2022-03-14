# Function for tidying the original HOBO STIC files
#---------------------------------------------------------------------------------------------------------------
rm(list=ls()) #clearing workspace

library(tidyverse)
require(tidyverse)

### function above works, now trying to add if statement

tidy_hobo_data <- function(filepath, outfile = FALSE) {  
  
  x <- read_csv(filepath, 
                col_types = cols(.default = "c"),
                skip = 1)
  
  x <- x %>% 
    rename_with(.cols = contains("Temp"),
                .fn = function(x){"temperature"}) %>%
    rename_with(.cols = contains("Lux"),
                .fn = function(x){"conductivity"}) %>% 
    rename_with(.cols = contains("Date"),
                .fn = function(x){"datetime"}) %>% 
    select(datetime, conductivity, temperature) %>% 
    mutate(datetime = lubridate::mdy_hms(datetime)) %>% 
    mutate(temperature = as.numeric(temperature)) %>% 
    mutate(conductivity = as.numeric(conductivity))
  
  if(outfile == TRUE) {
    write_csv(x, "tidy_hobo.csv")
  }
}

# Testing
tidyhobo_test <- tidy_hobo_data("C:/Users/Christopher/Desktop/R_Directory/stic_package/20821662.csv", outfile = TRUE)

