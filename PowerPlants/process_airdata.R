
library(tidyverse)

# download annual summary files for each year from AirData via the EPA

base_url <- "https://aqs.epa.gov/aqsweb/airdata/annual_conc_by_monitor_"

# Loop through each URL
for (year in 1980:2025) {
  url <- str_c(base_url, year, ".zip")
  
  # Get file name
  zip_name <- basename(url)
  dest_path <- file.path("Data/AirData", zip_name)
  
  # Download ZIP
  download.file(url, dest_path)
  
  # Unzip into the "unzipped" directory
  unzip(dest_path, exdir = "Data/AirData")
  
  file.remove(dest_path)
}

# function to filter df to only MN and PM2.5
filter_airdata <- function(df){
  df %>% 
    janitor::clean_names() %>% 
    filter(state_code == 27,  parameter_code == 88101) # filter to just MN and PM2.5
}

# read in all files in directory
data_frame_names <- list.files(path = "Data/AirData", pattern = "*.csv") 
data_frame_names <- str_c("Data/AirData/", data_frame_names)
data_frame_list <- map(data_frame_names, read_csv)

#apply function to them & combine into one
combined_df <- map(data_frame_list, filter_airdata) %>% list_rbind()


# save as new csv
write_csv(combined_df, "Data/airdata_clean.csv")
