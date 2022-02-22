#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#GAPFILL METHOD Experimenting
#2/3/22
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#Clear workspace 
remove(list=ls())

#Gather libraries of interest
library(xts)
library(dygraphs)
library(lubridate)
library(tidyverse)

#Read custom R functions
source("R/download_fun.R") #this is how you pull in separate R files like this function
#source("R/download_fun_GMT5.R")
#source("R/download_fun_GMT6.R")
#source("R/check_fun.R")
#source("R/dygraph_ts_fun.R")

data_dir <- "/Users/Delaney/Workspace R/RESEARCH/Tanglewood_waterLevel/data/" #DONT FORGET BACKSLASH

#list pt and baro file locations
#this makes a new file of locations of data on your computer so that we can apply the download_fun to all of them
pt_files_jan <- list.files(paste0(data_dir, "20210127export"), full.names =  TRUE)
pt_files_mar <- list.files(paste0(data_dir, "20210318"), full.names =  TRUE)
pt_files_may <- list.files(paste0(data_dir, "20210506"), full.names =  TRUE)
pt_files_jun <- list.files(paste0(data_dir, "202106XX"), full.names =  TRUE)
pt_files_oct <- list.files(paste0(data_dir, "20211022"), full.names =  TRUE)
#I name my baros "_baro" after the serial number so that it can recognize them with this -- could change to S/N
baro_files_jan <- pt_files_jan[str_detect(pt_files_jan, "ba")] 
baro_files_mar <- pt_files_mar[str_detect(pt_files_mar, "ba")]
baro_files_may <- pt_files_may[str_detect(pt_files_may, "ba")]
baro_files_jun <- pt_files_jun[str_detect(pt_files_jun, "ba")]
baro_files_oct <- pt_files_oct[str_detect(pt_files_oct, "ba")]
#these are our download sheets 
field_files_jan <- paste0(data_dir, 'well_log1.csv')
field_files_mar <- paste0(data_dir, 'well_log2.csv')
field_files_may <- paste0(data_dir, 'well_log3.csv')
field_files_jun <- paste0(data_dir, 'well_log4.csv')
field_files_oct <- paste0(data_dir, 'well_log5.csv')


#Download Field Worksheet
field_log_jan <- read_csv(field_files_jan) %>% 
  #cut extra row off
  filter(Site_ID != "NA")
field_log_mar <- read_csv(field_files_mar) %>% 
  filter(Site_ID != "NA")
field_log_may <- read_csv(field_files_may) %>% 
  filter(Site_ID != "NA")
field_log_jun <- read_csv(field_files_jun) %>% 
  filter(Site_ID != "NA")
field_log_oct <- read_csv(field_files_oct) %>% 
  filter(Site_ID != "NA")

#Check to make sure pt files match field logs (YOU WONT DO THIS RN)
#check_fun(pt_files,field_log_oct)
#If final 

#create df of site name, sonde_id, and measured offset
field_log_jan <- field_log_jan %>% 
  select(Site_ID, Sonde_ID, Relative_Water_Level_m) #rel_water_level is the groundtruthing we do at download
field_log_mar <- field_log_mar %>% 
  select(Site_ID, Sonde_ID, Relative_Water_Level_m)
field_log_may <- field_log_may %>% 
  select(Site_ID, Sonde_ID, Relative_Water_Level_m)
field_log_jun <- field_log_jun %>% 
  select(Site_ID, Sonde_ID, Relative_Water_Level_m)
field_log_oct <- field_log_oct %>% 
  select(Site_ID, Sonde_ID, Relative_Water_Level_m)

#tell R which baros are good 
baro_key <- tibble(
  Site_ID = field_log_jan$Site_ID,
  baro_ID = NA) %>% 
  #Usable Baro transect 2
  mutate(baro_ID = ifelse(str_detect(Site_ID,"Ba1"), "bad_baro", baro_ID)) %>% 
  #DONT USE baro transect 3
  mutate(baro_ID = ifelse(str_detect(Site_ID,"Ba2"), "fixed_baro", baro_ID))

#--------------
#NOW apply the download function to our baro files to pull the data in
baro_jan <- lapply(baro_files_jan, download_fun) %>% #apply the function with lapply
  bind_rows() %>% #bind all of the data together into one df for this download (all sites)
  left_join(., field_log_jan) %>% #join with field log to get site information
  left_join(., baro_key) #join with baro key to get specific baro information
baro_mar <- lapply(baro_files_mar, download_fun) %>% 
  bind_rows() %>% 
  left_join(., field_log_mar) %>% 
  left_join(., baro_key)
baro_may <- lapply(baro_files_may, download_fun) %>% 
  bind_rows() %>% 
  left_join(., field_log_may) %>% 
  left_join(., baro_key)
baro_jun <- lapply(baro_files_jun, download_fun) %>% 
  bind_rows() %>% 
  left_join(., field_log_jun) %>% 
  left_join(., baro_key)
baro_jun <- lapply(baro_files_jun, download_fun) %>% 
  bind_rows() %>% 
  left_join(., field_log_jun) %>% 
  left_join(., baro_key)
baro_oct <- lapply(baro_files_oct, download_fun_GMT5) %>% 
  bind_rows() %>% 
  left_join(., field_log_oct)%>% 
  left_join(., baro_key)

#bind the whole period of record together
all_baro <- bind_rows(baro_jan, baro_mar) %>% 
  bind_rows(., baro_may) %>% 
  bind_rows(., baro_jun) %>% 
  bind_rows(., baro_oct)

#Plot to check for any issues
all_baro %>% 
  ggplot(aes(x = Timestamp, y = pressureAbsolute, group = baro_ID)) +
  geom_line(aes(color = baro_ID))



#LETS GAPFILL SOMETHING WITH TALLADEGA DATA --------------------
#-------------------------------------------------------------------------------------------

data_dir <- "/Users/Delaney/Workspace R/RESEARCH/Tanglewood_waterLevel/data/"

aug_tall <- read_csv(paste0(data_dir, "NEON_TALL_baropress_Aug.csv")) %>% 
  mutate(Timestamp = endDateTime,
         MeanPress = staPresMean) %>% 
  select(Timestamp, MeanPress) 
sept_tall <- read_csv(paste0(data_dir, "NEON_TALL_baropress_Sept.csv")) %>% 
  mutate(Timestamp = endDateTime,
         MeanPress = staPresMean) %>% 
  select(Timestamp, MeanPress) 
oct_tall <- read_csv(paste0(data_dir, "NEON_TALL_baropress_Oct.csv")) %>% 
  mutate(Timestamp = endDateTime,
         MeanPress = staPresMean) %>% 
  select(Timestamp, MeanPress) 

#format it into what we need
aug_tall <- aug_tall %>% 
  mutate(Day = day(Timestamp),
         Month = month(Timestamp),
         Year = year(Timestamp),
         Hour = hour(Timestamp),
         Minute = minute(Timestamp),
         Quarter = ifelse(Minute <= 15, 1, NA),
         Quarter = ifelse(Minute <= 30 & Minute > 15, 2, Quarter),
         Quarter = ifelse(Minute <= 45 & Minute > 30, 3, Quarter),
         Quarter = ifelse(Minute > 45, 4, Quarter)) %>% 
  group_by(Day, Month, Year, Hour, Quarter) %>% 
  summarise(MeanPress = mean(MeanPress)) %>% 
  mutate(Hour = ifelse(Quarter == 4, Hour + 1, Hour),
         Minute = ifelse(Quarter == 1, 15, NA),
         Minute = ifelse(Quarter == 2, 30, Minute),
         Minute = ifelse(Quarter == 3, 45, Minute),
         Minute = ifelse(Quarter == 4, 0, Minute)) %>% 
  unite(Date, Year, Month, Day, Hour, Minute, sep = "-") %>% 
  mutate(Timestamp = ymd_hm(Date)) %>%
  select(Timestamp, MeanPress)

sept_tall <- sept_tall %>% 
  mutate(Day = day(Timestamp),
         Month = month(Timestamp),
         Year = year(Timestamp),
         Hour = hour(Timestamp),
         Minute = minute(Timestamp),
         Quarter = ifelse(Minute <= 15, 1, NA),
         Quarter = ifelse(Minute <= 30 & Minute > 15, 2, Quarter),
         Quarter = ifelse(Minute <= 45 & Minute > 30, 3, Quarter),
         Quarter = ifelse(Minute > 45, 4, Quarter)) %>% 
  group_by(Day, Month, Year, Hour, Quarter) %>% 
  summarise(MeanPress = mean(MeanPress)) %>% 
  mutate(Hour = ifelse(Quarter == 4, Hour + 1, Hour),
         Minute = ifelse(Quarter == 1, 15, NA),
         Minute = ifelse(Quarter == 2, 30, Minute),
         Minute = ifelse(Quarter == 3, 45, Minute),
         Minute = ifelse(Quarter == 4, 0, Minute)) %>% 
  unite(Date, Year, Month, Day, Hour, Minute, sep = "-") %>% 
  mutate(Timestamp = ymd_hm(Date)) %>%
  select(Timestamp, MeanPress)

oct_tall <- oct_tall %>% 
  mutate(Day = day(Timestamp),
         Month = month(Timestamp),
         Year = year(Timestamp),
         Hour = hour(Timestamp),
         Minute = minute(Timestamp),
         Quarter = ifelse(Minute <= 15, 1, NA),
         Quarter = ifelse(Minute <= 30 & Minute > 15, 2, Quarter),
         Quarter = ifelse(Minute <= 45 & Minute > 30, 3, Quarter),
         Quarter = ifelse(Minute > 45, 4, Quarter)) %>% 
  group_by(Day, Month, Year, Hour, Quarter) %>% 
  summarise(MeanPress = mean(MeanPress)) %>% 
  mutate(Hour = ifelse(Quarter == 4, Hour + 1, Hour),
         Minute = ifelse(Quarter == 1, 15, NA),
         Minute = ifelse(Quarter == 2, 30, Minute),
         Minute = ifelse(Quarter == 3, 45, Minute),
         Minute = ifelse(Quarter == 4, 0, Minute)) %>% 
  unite(Date, Year, Month, Day, Hour, Minute, sep = "-") %>% 
  mutate(Timestamp = ymd_hm(Date)) %>%
  select(Timestamp, MeanPress)

tall_press <- full_join(aug_tall, sept_tall) %>% 
  full_join(., oct_tall)

#convert to the same timezone so we don't have timezone issues, as NEON reports in UTC
tall_press <- with_tz(tall_press, "America/Chicago") %>% 
  filter(Timestamp <=  "2021-10-22 13:15:00" )

ggplot(tall_press, aes(Timestamp, MeanPress)) +
  geom_line()

#now we want our TBS data in the same format
baro_TBS <- all_baro %>% filter(Site_ID == "TBSBa2") %>% 
  filter(Timestamp >= "2021-08-01 00:00:00") %>% 
  select(Timestamp, pressureAbsolute, Site_ID)

#plot them together to see how they do
ggplot(data = NULL, aes(x = Timestamp)) +
  geom_line(data = tall_press, aes(y = MeanPress), color = "darkgreen") +
  geom_line(data = baro_TBS, aes(y = pressureAbsolute), color = "steelblue")

#so let's combine these datasets so that we have everything in oneplace
all_press <- left_join(tall_press, baro_TBS, by = "Timestamp")

#THESE MATCH UP REALLY NICELY!!! NOW WE NEED TO BUILD OUR MODEL
predict_press <- lm(all_press$pressureAbsolute ~ all_press$MeanPress)
summary(predict_press)
#this looks pretty good -- Rsq is 0.78
  #slope is 0.906813
  #intercept is 10.378813

all_press <- all_press %>% 
  drop_na() %>% 
  mutate(predictedPress = (10.378813 + 0.906813*MeanPress),
         resid = abs(predictedPress - pressureAbsolute))
#plot our residuals to see how it does
ggplot(all_press) +
  geom_boxplot(aes(y = resid))

ggplot(all_press) +
  geom_line(aes(x = Timestamp, y = pressureAbsolute), color = "red") +
  geom_line(aes(x = Timestamp, y = predictedPress), color = "black") +
  geom_line(aes(x = Timestamp, y = MeanPress), color = "darkgreen")
#VERY NICE VERY FEW OUTLIERS

#bring in the rest of the talladega data and redo everything -----------------------------
jan_tall <- read_csv(paste0(data_dir, "NEON_TALL_baropress_jan.csv")) %>% 
  mutate(Timestamp = endDateTime,
         MeanPress = staPresMean) %>% 
  select(Timestamp, MeanPress) 
feb_tall <- read_csv(paste0(data_dir, "NEON_TALL_baropress_feb.csv")) %>% 
  mutate(Timestamp = endDateTime,
         MeanPress = staPresMean) %>% 
  select(Timestamp, MeanPress) 
mar_tall <- read_csv(paste0(data_dir, "NEON_TALL_baropress_mar.csv")) %>% 
  mutate(Timestamp = endDateTime,
         MeanPress = staPresMean) %>% 
  select(Timestamp, MeanPress) 
apr_tall <- read_csv(paste0(data_dir, "NEON_TALL_baropress_apr.csv")) %>% 
  mutate(Timestamp = endDateTime,
         MeanPress = staPresMean) %>% 
  select(Timestamp, MeanPress) 
may_tall <- read_csv(paste0(data_dir, "May_baropress_TALL.csv")) %>% 
  mutate(Timestamp = endDateTime,
         MeanPress = staPresMean) %>% 
  select(Timestamp, MeanPress) 
jun_tall <- read_csv(paste0(data_dir, "NEON_TALL_baropress_jun.csv")) %>% 
  mutate(Timestamp = endDateTime,
         MeanPress = staPresMean) %>% 
  select(Timestamp, MeanPress) 
jul_tall <- read_csv(paste0(data_dir, "NEON_TALL_baropress_july.csv")) %>% 
  mutate(Timestamp = endDateTime,
         MeanPress = staPresMean) %>% 
  select(Timestamp, MeanPress) 
aug_tall <- read_csv(paste0(data_dir, "NEON_TALL_baropress_Aug.csv")) %>% 
  mutate(Timestamp = endDateTime,
         MeanPress = staPresMean) %>% 
  select(Timestamp, MeanPress) 
sept_tall <- read_csv(paste0(data_dir, "NEON_TALL_baropress_Sept.csv")) %>% 
  mutate(Timestamp = endDateTime,
         MeanPress = staPresMean) %>% 
  select(Timestamp, MeanPress) 
oct_tall <- read_csv(paste0(data_dir, "NEON_TALL_baropress_Oct.csv")) %>% 
  mutate(Timestamp = endDateTime,
         MeanPress = staPresMean) %>% 
  select(Timestamp, MeanPress) 

#join them all now jesus fuck
tall_press <- full_join(jan_tall, feb_tall) %>% 
  full_join(., mar_tall) %>% 
  full_join(., apr_tall) %>% 
  full_join(., may_tall) %>% 
  full_join(., jun_tall) %>% 
  full_join(., jul_tall) %>% 
  full_join(., aug_tall) %>% 
  full_join(., sept_tall) %>% 
  full_join(., oct_tall)

#format it into what we need
tall_press <- tall_press %>% 
  mutate(Day = day(Timestamp),
         Month = month(Timestamp),
         Year = year(Timestamp),
         Hour = hour(Timestamp),
         Minute = minute(Timestamp),
         Quarter = ifelse(Minute <= 15, 1, NA),
         Quarter = ifelse(Minute <= 30 & Minute > 15, 2, Quarter),
         Quarter = ifelse(Minute <= 45 & Minute > 30, 3, Quarter),
         Quarter = ifelse(Minute > 45, 4, Quarter)) %>% 
  group_by(Day, Month, Year, Hour, Quarter) %>% 
  summarise(MeanPress = mean(MeanPress)) %>% 
  mutate(Hour = ifelse(Quarter == 4, Hour + 1, Hour),
         Minute = ifelse(Quarter == 1, 15, NA),
         Minute = ifelse(Quarter == 2, 30, Minute),
         Minute = ifelse(Quarter == 3, 45, Minute),
         Minute = ifelse(Quarter == 4, 0, Minute)) %>% 
  unite(Date, Year, Month, Day, Hour, Minute, sep = "-") %>% 
  mutate(Timestamp = ymd_hm(Date)) %>%
  select(Timestamp, MeanPress)

#so now we want to make a new column that is a hodgepodge of this data
all_model <- all_baro %>% filter(Site_ID == "TBSBa2") %>% 
  select(Timestamp, pressureAbsolute) %>% 
  left_join(., tall_press) %>% 
  drop_na() %>% 
  mutate(predictedPress = (10.378813 + 0.906813*MeanPress),
         resid = abs(predictedPress - pressureAbsolute))

ggplot(all_model) +
  geom_line(aes(x = Timestamp, y = pressureAbsolute), color = "red") +
  geom_line(aes(x = Timestamp, y = predictedPress), color = "black") +
  geom_line(aes(x = Timestamp, y = MeanPress), color = "darkgreen")

#now build a new column that decides which pressure to use
all_model <- all_model %>% 
  mutate(masterBaro = ifelse(resid > 0.35, predictedPress, pressureAbsolute)) 

ggplot(all_model) +
 geom_line(aes(x = Timestamp, y = pressureAbsolute), color = "red") +
 geom_line(aes(x = Timestamp, y = masterBaro), color = "black") 

#IT FUCKING WORKS!!!! ------------------------------------------------------------------------
#now apply it to the May/June period that was causing difficulty
df <- lapply(pt_files_jun, download_fun) %>% bind_rows()

df <- df %>% left_join(., field_log_jun) 

#we need a new interpolation function for our new data
baro_fun <- approxfun(all_model$Timestamp, all_model$masterBaro)

#Assign baro pressure
df <- df %>% 
  #Add baro key data
  left_join(., baro_key) %>% 
  #Interpolate barometric pressure from loggers
  mutate(pressureBaro = baro_fun(Timestamp)) %>% 
  #Clean up
  select(Sonde_ID, Site_ID, Timestamp, pressureAbsolute, pressureBaro)

#Estimate waterHeight
df <- df %>% 
  mutate(pressureGauge = pressureAbsolute-pressureBaro, 
         waterHeight   = pressureGauge/9.81)

df %>% 
  filter(Site_ID == "TBS01",
         waterHeight >= 0) %>% 
  ggplot(aes(Timestamp, waterHeight)) +
  geom_line()
#THIS LOOKS JUST AS GOOD AS THE OTHER! NOW TRY THE STREAM

df %>% 
  filter(Site_ID == "TBS03") %>% 
  ggplot(aes(Timestamp, waterHeight)) +
  geom_line()

df %>% 
  filter(Site_ID == "TBS04") %>% 
  ggplot(aes(Timestamp, waterHeight)) +
  geom_line()

df %>% 
  filter(Site_ID == "TBS05") %>% 
  ggplot(aes(Timestamp, waterHeight)) +
  geom_line()

df %>% 
  filter(Site_ID == "TBS06") %>% 
  ggplot(aes(Timestamp, waterHeight)) +
  geom_line()

df %>% 
  filter(Site_ID == "TBS16") %>% 
  ggplot(aes(Timestamp, waterHeight)) +
  geom_line()

df %>% 
  filter(Site_ID == "TBS26") %>% 
  ggplot(aes(Timestamp, waterHeight)) +
  geom_line()

df %>% 
  filter(Site_ID == "TBS36") %>% 
  ggplot(aes(Timestamp, waterHeight)) +
  geom_line() 

















