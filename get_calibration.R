# get_calibration.R
# Function to fit standard and measured calibration data to model a object (lm and exponential to start)
# inputs: data frame with columns "standard" and "conductivity_uncal" and method = "linear" or "exponential"
# output: fitted model object

#input needs to be data frame. column names: conductivity_uncal, standard

get_calibration <- function(calibration_data, method = "linear") {
  
  calibration <-  lm(standard ~ conductivity_uncal, data = calibration_data)
  
  if(method == "exponential") {
    
    calibration <- lm(log(standard) ~ conductivity_uncal, data = calibration_data)
  } else{
    stop("This method is unknown. Please use the linear or exponential method")
  }
  
  return(calibration)  
}

