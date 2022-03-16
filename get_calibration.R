# get_calibration.R
# Function to fit standard and measured calibration data to model a object (lm and exponential to start)
# inputs: standard (vector) and measured (vector) and method = "linear" or "exponential"

get_calibration <- function(measured, standard, method = "linear") {
  
 calibration <-  lm(standard ~ measured)
  
  if(method != "linear") {
    
    calibration <- lm(log(standard) ~ measured)
  }
 
   return(calibration)  
}


# Testing ---------------------------------------------------------------------------------------------------------------

cal_points <- read.csv("cal_points.csv")

standard <- as.vector(cal_points$std_val)

measured <- as.vector(cal_points$measured_val)

lin_calibration <- get_calibration(measured, standard)

exp_calibration <- get_calibration(measured, standard, method = "exponential")
