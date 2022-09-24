#### CTW - Feb 2022 - testing git
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
stic_files_b <- stic_files %>% 
  dplyr::mutate(wetdry1000 = if_else(conductivity >= 1000, "wet", "dry" )) %>% 
  dplyr::mutate(wetdry200 = if_else(conductivity >= 200, "wet", "dry" )) %>% 
  dplyr::mutate(wetdry400 = if_else(conductivity >= 400, "wet", "dry" )) %>% 
  dplyr::mutate(wetdry600 = if_else(conductivity >= 600, "wet", "dry" )) %>% 
  dplyr::mutate(wetdry800 = if_else(conductivity >= 800, "wet", "dry" ))



### Making another datetime column in tidy date format 
stic_files_b <- stic_files_b %>% 
  mutate(date = lubridate::date(datetime))

### NOTE: this command is just trimming the date range to the time when all the loggers were active
### It is not something that is always neccessary
stic_files_b <- stic_files_b %>% 
  dplyr::filter(datetime >= "2021-05-23")

### using lubridate to separate out the individual days 
stic_files_b <- stic_files_b %>% 
  mutate(day = lubridate::yday(date))


### making a new dataframe with date of first drying for all individual logger
drying <- stic_files_b %>% 
  subset(wetdry == "dry") %>%
  group_by(logger) %>% 
  summarise(first_dry = min(date)) 



### Now calculating the percentage of wet STICs at each time step

### Calculate wet network proportion

stic_wet_prop <- stic_files_b %>% 
  group_by(datetime) %>% 
  summarise(n_wet = sum(wetdry == "wet"), n_sensors = n() ) %>% 
  mutate(percent = n_wet/n_sensors)


### Calculate flow duration for each sensor
### i,e, percentage of wet observations out of total obs for each logger

high_stic_duration <- stic_files_b %>% 
  group_by(logger) %>% 
  summarise(n_wet = sum(wetdry == "wet", na.rm = TRUE), count = n()) %>% 
  mutate(duration = n_wet/count)

############################################################################################################################
rm(stic_wet_prop)

stic_wet_prop1000 <- stic_files_b %>% 
  group_by(datetime) %>% 
  summarise(n_wet = sum(wetdry1000 == "wet"), n_sensors = n() ) %>% 
  mutate(percent1000 = n_wet/n_sensors)

stic_wet_prop200 <- stic_files_b %>% 
  group_by(datetime) %>% 
  summarise(n_wet = sum(wetdry200 == "wet"), n_sensors = n() ) %>% 
  mutate(percent200 = n_wet/n_sensors)

stic_wet_prop400 <- stic_files_b %>% 
  group_by(datetime) %>% 
  summarise(n_wet = sum(wetdry400 == "wet"), n_sensors = n() ) %>% 
  mutate(percent400 = n_wet/n_sensors)

stic_wet_prop600 <- stic_files_b %>% 
  group_by(datetime) %>% 
  summarise(n_wet = sum(wetdry600 == "wet"), n_sensors = n() ) %>% 
  mutate(percent600 = n_wet/n_sensors)


stic_wet_prop800 <- stic_files_b %>% 
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


#==============================================================================================================================
# Now try thresholds with actual SPC


source("get_calibration.R")

source("apply_calibration.R")
  
## Step 2: Get calibration

cal_points <- read_csv("cal_points.csv")

cal_points <- cal_points %>% 
  rename(standard = std_val) %>% 
  rename(conductivity_uncal = measured_val)

calibration_data <- cal_points

calibration <- get_calibration(calibration_data, method = "linear")


## Step 3: Apply calibration

stic_files <- stic_files %>% 
  rename(conductivity_uncal = conductivity)

stic_data_with_spc <- apply_calibration(input = stic_files, calibration = calibration)



### Creating binary wet/dry column based of relative cond value of 1000
stic_data_with_spc <- stic_data_with_spc %>% 
  dplyr::mutate(wetdry50 = if_else(spc >= 50, "wet", "dry" )) %>% 
  dplyr::mutate(wetdry100 = if_else(spc >= 100, "wet", "dry" )) %>% 
  dplyr::mutate(wetdry200 = if_else(spc >= 200, "wet", "dry" )) %>% 
  dplyr::mutate(wetdry300 = if_else(spc >= 300, "wet", "dry" )) %>% 
  dplyr::mutate(wetdry400 = if_else(spc >= 400, "wet", "dry" )) %>% 
  dplyr::mutate(wetdry500 = if_else(spc >= 500, "wet", "dry" )) 



### Making another datetime column in tidy date format 
stic_data_with_spc <- stic_data_with_spc %>% 
  mutate(date = lubridate::date(datetime))

### NOTE: this command is just trimming the date range to the time when all the loggers were active
### It is not something that is always neccessary
stic_data_with_spc <- stic_data_with_spc %>% 
  dplyr::filter(datetime >= "2021-05-23")

### using lubridate to separate out the individual days 
stic_data_with_spc <- stic_data_with_spc %>% 
  mutate(day = lubridate::yday(date))


### making a new dataframe with date of first drying for all individual logger
drying <- stic_data_with_spc %>% 
  subset(wetdry == "dry") %>%
  group_by(logger) %>% 
  summarise(first_dry = min(date)) 



### Now calculating the percentage of wet STICs at each time step



stic_wet_prop50 <- stic_data_with_spc %>% 
  group_by(datetime) %>% 
  summarise(n_wet = sum(wetdry50 == "wet"), n_sensors = n() ) %>% 
  mutate(percent50 = n_wet/n_sensors)

stic_wet_prop100 <- stic_data_with_spc %>% 
  group_by(datetime) %>% 
  summarise(n_wet = sum(wetdry100 == "wet"), n_sensors = n() ) %>% 
  mutate(percent100 = n_wet/n_sensors)

stic_wet_prop200 <- stic_data_with_spc %>% 
  group_by(datetime) %>% 
  summarise(n_wet = sum(wetdry200 == "wet"), n_sensors = n() ) %>% 
  mutate(percent200 = n_wet/n_sensors)

stic_wet_prop300 <- stic_data_with_spc %>% 
  group_by(datetime) %>% 
  summarise(n_wet = sum(wetdry300 == "wet"), n_sensors = n() ) %>% 
  mutate(percent300 = n_wet/n_sensors)


stic_wet_prop400 <- stic_data_with_spc %>% 
  group_by(datetime) %>% 
  summarise(n_wet = sum(wetdry400 == "wet"), n_sensors = n() ) %>% 
  mutate(percent400 = n_wet/n_sensors)

stic_wet_prop500 <- stic_data_with_spc %>% 
  group_by(datetime) %>% 
  summarise(n_wet = sum(wetdry500 == "wet"), n_sensors = n() ) %>% 
  mutate(percent500 = n_wet/n_sensors)


threshold_spc <- left_join(stic_wet_prop50, stic_wet_prop100, by = "datetime")

thresh2_spc <- left_join(threshold_spc, stic_wet_prop200, by = "datetime")

thresh3_spc <- left_join(thresh2_spc, stic_wet_prop300, by = "datetime")

thresh4_spc <- left_join(thresh3_spc, stic_wet_prop400, by = "datetime")

thresh5_spc <- left_join(thresh4_spc, stic_wet_prop500, by = "datetime")

write_csv(thresh5_spc, "threshold_spc.csv")

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

#=============================================================================================================================


threshold_spc <- read_csv("threshold_spc.csv")


threshold_spc_s <- threshold_spc %>% 
  select(datetime, percent50, percent100, percent200, percent300, percent400, percent500)



ggplot(threshold_spc_s) + 
  geom_line(aes(x = datetime, y = percent50), color = "red") + 
  geom_line(aes(x = datetime, y = percent100), color = "blue") + 
  geom_line(aes(x = datetime, y = percent200), color = "green") + 
  geom_line(aes(x = datetime, y = percent300), color = "yellow") + 
  geom_line(aes(x = datetime, y = percent400), color = "steelblue") + 
  geom_line(aes(x = datetime, y = percent500), color = "magenta") + 
  theme_cowplot() + 
  ylab("wet network proportion based on spc")


# USE 50 SPC
 
daily_stic <- stic_data_with_spc %>% 
  select(logger, datetime, date, temperature, conductivity_uncal, spc, wetdry50)

stic_daily <- daily_stic %>% 
  group_by(logger, date) %>% 
  summarize(daily_temp = mean(temperature),
            daily_spc = mean(spc))

write_csv(stic_daily, "daily_stic.csv")


