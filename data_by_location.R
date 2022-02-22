### Script to generate data frame for each STIC location at Konza containing 
### All the observations from all download rounds

library(tidyverse)

#### bring in index csv
konza_stic_index <- read_csv("konza_stic_index.csv", 
                             col_types = cols(initial_deploy = col_character(), 
                             sept_jan_2022 = col_character()))

round_1_files <- list.files(path = "C:/Users/cwhee/Desktop/R_Directory/AIMS_hydro_qaqc/konza_stics_round_1")

round_2_files <- list.files(path = "C:/Users/cwhee/Desktop/R_Directory/AIMS_hydro_qaqc/konza_stics_round_2")

index_rows <- nrow(konza_stic_index)


location_sns <- map2_chr()


?map2_chr


v_1 <- as.vector(konza_stic_index$initial_deploy)

v_2 <- as.vector(konza_stic_index$sept_jan_2022)



combined_index <- map2_chr()







################################################################################################################################
################################################################################################################################

#calculate discharges by depth
ndepthrows<-nrow(streamvel)# number of rows in streamvel
ndepthrows
streamq<-data.frame(c(2:ndepthrows-1))#make new dataframe for 'slices'
colnames(streamq)[1]<-"adepth" #rename first column 'adepth'
streamq$avel<-NA #add a new column for velocities in slices
streamq$int<-NA # add new column for height of each slice
#need to loop through streamq to fill the slices with values
for(i in 1:ndepthrows-1){
  streamq$adepth[i]<-mean(c(streamvel$depth[i],streamvel$depth[i+1]))
  streamq$avel[i]<-mean(c(streamvel$vel[i],streamvel$vel[i+1]))
  streamq$int[i]<-streamvel$depth[i+1]-streamvel$depth[i]
}
streamq$q<-streamq$avel*streamq$int #new column for discharge values

################################################################################################################################
################################################################################################################################


