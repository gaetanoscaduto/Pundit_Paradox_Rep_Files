#fix the date error
library(openxlsx)
library(dplyr)
library(lubridate)

swap_day_month <- function(date_string) {
  year <- substr(date_string, 1, 4)
  month <- substr(date_string, 6, 7)
  day <- substr(date_string, 9, 10)
  
  if(as.numeric(day)>12)
  {
    return(paste(day, month, year, sep="-"))  
  }
  else
  {
    return(paste(month, day, year, sep="-"))  
    
  }
  # Recompose into dd-mm-yyyy format
  
}


change_date_format <- function(date_string) {
  year <- substr(date_string, 1, 4)
  month <- substr(date_string, 6, 7)
  day <- substr(date_string, 9, 10)
    
  return(paste(day, month, year, sep="-"))  
 
  # Recompose into dd-mm-yyyy format
  
}


setwd("C:/Users/gasca/OneDrive - Università degli Studi di Milano-Bicocca/Dottorato/Papers and Chapters/Negri Scaduto 2024/negri_scaduto_SISP/data/talk/")

data = read.xlsx("Ospitate_to_fix.xlsx", detectDates = T)

data$Data_fixed = "placeholder"

date_to_write_differently = data[1:1180, ]$Data_tryfix

date_fixed = change_date_format(date_to_write_differently)

data$Data_fixed[1:1180] = date_fixed

data_to_swap = data$Data_tryfix[1181:nrow(data)]

head(data_to_swap)
 #okay it's them

# Apply the function to each date
swapped_dates <- sapply(data_to_swap, swap_day_month)

head(data_to_swap)
head(swapped_dates)
# Print the results
print(swapped_dates)

data$Data_tryfix = NULL
data$Data_fixed[1181:nrow(data)] = swapped_dates

View(data)
write.xlsx(data, "Ospitate_corrected.xlsx")
