#### CTW - Feb 2022
#### Importing multiple "Clean" STIC files from the same folder and binding
### Creating binary wet/dry dataset from relative conductivity
### Determining basic flow metrics

library(tidyverse)
library(cowplot)
library(patchwork)
library(fs)


rm(list=ls()) #clearing workspace

### loading in processed stics 
### NOTE: for this script to work you need to have a single folder
### in your working directory with all the STIC files in it 

data_dir <- "konza_stics_round_1"

fs::dir_ls(data_dir)

length(data_dir)

stic_files <- fs::dir_ls(data_dir, regexp = "\\.csv$")

### using the map_dfr function from tidyverse to loop in individual csvs and bind rows
stic_files <- stic_files %>% 
  map_dfr(read_csv)

### Creating binary wet/dry column based of relative cond value of 1000
stic_files <- stic_files %>% 
  dplyr::mutate(wetdry1000 = if_else(conductivity >= 1000, "wet", "dry" )) %>% 
  dplyr::mutate(wetdry200 = if_else(conductivity >= 200, "wet", "dry" )) %>% 
  dplyr::mutate(wetdry400 = if_else(conductivity >= 400, "wet", "dry" )) %>% 
  dplyr::mutate(wetdry600 = if_else(conductivity >= 600, "wet", "dry" )) %>% 
  dplyr::mutate(wetdry800 = if_else(conductivity >= 800, "wet", "dry" ))



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

############################################################################################################################
rm(stic_wet_prop)

stic_wet_prop1000 <- stic_files %>% 
  group_by(datetime) %>% 
  summarise(n_wet = sum(wetdry1000 == "wet"), n_sensors = n() ) %>% 
  mutate(percent1000 = n_wet/n_sensors)

stic_wet_prop200 <- stic_files %>% 
  group_by(datetime) %>% 
  summarise(n_wet = sum(wetdry200 == "wet"), n_sensors = n() ) %>% 
  mutate(percent200 = n_wet/n_sensors)

stic_wet_prop400 <- stic_files %>% 
  group_by(datetime) %>% 
  summarise(n_wet = sum(wetdry400 == "wet"), n_sensors = n() ) %>% 
  mutate(percent400 = n_wet/n_sensors)

stic_wet_prop600 <- stic_files %>% 
  group_by(datetime) %>% 
  summarise(n_wet = sum(wetdry600 == "wet"), n_sensors = n() ) %>% 
  mutate(percent600 = n_wet/n_sensors)


stic_wet_prop800 <- stic_files %>% 
  group_by(datetime) %>% 
  summarise(n_wet = sum(wetdry800 == "wet"), n_sensors = n() ) %>% 
  mutate(percent800 = n_wet/n_sensors)




threshold <- left_join(stic_wet_prop1000, stic_wet_prop800, by = "datetime")

thresh2 <- left_join(threshold, stic_wet_prop600, by = "datetime")

thresh3 <- left_join(thresh2, stic_wet_prop400, by = "datetime")

thresh4 <- left_join(thresh3, stic_wet_prop200, by = "datetime")

write_csv(thresh4, "threshold.csv")

#############################################################################################################################
#############################################################################################################################

rm(threshold_stic)

threshold_stic <- read_csv("threshold_stic.csv")

threshold_stic <- threshold_stic %>% 
  rename(percent_wet = percent1000)


ggplot(threshold_stic, aes(x = datetime, y = percent_wet, color = Category)) + 
  geom_path() + 
  theme_cowplot()
  

threshold <- read_csv("threshold.csv")


ggplot(threshold) + 
  geom_path(aes(x = datetime, y = percent1000), color = "red") +
  geom_path(aes(x = datetime, y = percent200), color = "blue") + 
  theme_cowplot()
  