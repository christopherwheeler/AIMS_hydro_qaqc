# classify_wetdry.R
# Classify wetdry function (start with absolute threshold)
# inputs: data frame with columns "datetime", "temperature", "conductivity", and "specific_conductivity"
# output: same data frame as input, but with a new column called wetdry

stic_data <- stic_data %>% 
  mutate(specific_conductivity = conductivity/80)

classify_wetdry <- function(input, threshold = 1000) {
  
input <- input %>% 
  dplyr::mutate(wetdry = if_else(measured_cond >= threshold, "wet", "dry" ))

}


# create dummy data to test this since I don't have apply_calibration working 
stic_data <- stic_data %>% 
  mutate(specific_cond = measured_cond/80)

classified <- classify_wetdry(stic_data, 500)






