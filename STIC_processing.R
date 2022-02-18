#### CTW - Feb 2022
#### Importing multiple "Clean" STIC files from the same folder and binding
### Creating binary wet/dry dataset from relative conductivity
### Determining basic flow metrics

library(tidyverse)
library(cowplot)
library(patchwork)


rm(list=ls()) #clearing workspace

### loading in processed stics 
### NOTE: for this script to work you need to have a single folder
### in your working directory with all the STIC files in it 

data_dir <- "processed_stic_names"

fs::dir_ls(data_dir)

length(data_dir)

stic_files <- fs::dir_ls(data_dir, regexp = "\\.csv$")

### using the map_dfr function from tidyverse to loop in individual csvs and bind rows
stic_files <- stic_files %>% 
  map_dfr(read_csv)

### Creating binary wet/dry column based of relative cond value of 1000
stic_files <- stic_files %>% 
  dplyr::mutate(wetdry = if_else(conductivity >= 1000, "wet", "dry" ))

### Making another datetime column in tidy date format 
stic_files <- stic_files %>% 
  mutate(date = lubridate::date(datetime))

### NOTE: this command is just trimming the date range to the time when all the loggers were active
### It is not something that is always neccessary
stic_files <- stic_files%>% 
  dplyr::filter(datetime >= "2021-05-23")

### using lubridate to separate out the individual days 
stic_files <- stic_files %>% 
  mutate(day = lubridate::yday(date))


### making a new dataframe with date of first drying for all individual logger
drying <- stic_files %>% 
  subset(wetdry == "dry") %>%
  group_by(logger) %>% 
  summarise(first_dry = min(date)) 



### Now calculating the percentage of wet STICs at each time step

### Calculate wet network proportion

stic_wet_prop <- stic_files %>% 
  group_by(datetime) %>% 
  summarise(n_wet = sum(wetdry == "wet"), n_sensors = n() ) %>% 
  mutate(percent = n_wet/n_sensors)


### Calculate flow duration for each sensor
### i,e, percentage of wet observations out of total obs for each logger

high_stic_duration <- stic_files %>% 
  group_by(logger) %>% 
  summarise(n_wet = sum(wetdry == "wet", na.rm = TRUE), count = n()) %>% 
  mutate(duration = n_wet/count)
