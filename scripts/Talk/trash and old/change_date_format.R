#change date format from ChatGPT to italian date

library(rio)

data = import("reprocessed_data_with_wiki.xlsx")


# Convert the date column to Date class
Data_giusta <- as.Date(data$Data, format = "%Y-%m-%d UTC")

# Format the date column as "GG/MM/AAAA" and update the data frame
data$Data <- format(Data_giusta, format = "%d/%m/%Y")

# Print the updated data frame
print(data)

export(data, "Ospiti_full_no_AP.xlsx")
