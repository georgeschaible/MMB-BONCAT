library(tidyverse)
library(openxlsx)
library(writexl)
library(readxl)


# Set working directory if data and R file are saved in the same folder
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#############################################################
############# Function to process python output ############# 
#############################################################

# Read the input file. Change seq_len to fit data frame
data <- read.table("distance.csv", header = FALSE, sep = ",", 
                 col.names = paste0("V",seq_len(250)), fill = TRUE)

# Create custom row names with "ROI" followed by row numbers
custom_row_names <- paste("ROI", seq_len(nrow(data)), sep = "")
  
# Assign custom row names to the data frame
rownames(data) <- custom_row_names
  
# Convert row names to a regular column and rename it
data <- tibble::rownames_to_column(data, "ROI")
  
# Assign names to the second column
colnames(data)[2] <- "radius"
  
# Convert the column to numeric
data$radius <- as.numeric(data$radius)
  
# Calculate radius as ratio
data$ratio_of_radius <- 1/data$radius
  
# Add ratio of radius as the third column without removing other columns
data <- bind_cols(data[, 1:2], ratio_of_radius = data$ratio_of_radius, data[, 3:ncol(data)])
  
# Assign names to the second column
colnames(data)[3] <- "ratio_of_radius"
  
# Reshape the data frame to long format for RFI values
data_long <- pivot_longer(data, cols = starts_with("V"), names_to = "ID", values_to = "RFI")
  
# create the new data frame with the desired structure
new_data <- data_long %>%
  group_by(ROI) %>%
  mutate(
    ratio_of_radius_cumsum = cumsum(ratio_of_radius),
    ratio_of_radius_cumsum = ifelse(ratio_of_radius_cumsum <= 1, ratio_of_radius_cumsum, 1)  # Limit to 1
  ) %>%
  select(ROI, ratio_of_radius_cumsum, RFI)

new_data <- new_data %>% rename(Radius = ratio_of_radius_cumsum)
  
write.xlsx(new_data, "distance_for_R.xlsx", rowNames = TRUE)
  

#############################################################
############# Function to process python output ############# 
#############################################################

# name input and output files
write_corrected_values <- function(distance_for_R.xlsx, distance_for_R_out.xlsx, ROI, RFI) {
 
   # Read the input file
  metadata <- read_excel("distance_for_R.xlsx")
  
  
  # Get the unique ROIs
  identifiers <- unique(metadata[[ROI]])
  
  # Check if identifiers vector is empty or contains missing values
  if (length(identifiers) == 0) {
    stop("No unique identifiers found in the dataset.")
  }
  
  # Create a new workbook
  wb <- createWorkbook()
  
  # Create a new worksheet
  addWorksheet(wb, sheetName = "Corrected Values")
  
  # Initialize variables for row and column indices
  row_index <- 1
  
  # Convert the column to numeric
  metadata$RFI <- as.numeric(metadata$RFI)
  
  # Iterate through each ROI
  for (identifier in identifiers) {
    cat("Processing identifier:", identifier, "\n")
    
    # Filter data for the current ROI
    filtered_data <- filter(metadata, !!sym(ROI) == identifier)
    
    if (any(!is.na(filtered_data[[RFI]]))) {
      # Find the minimum and maximum values, excluding NAs
      min_value <- min(filtered_data[[RFI]], na.rm = TRUE)
      max_value <- max(filtered_data[[RFI]], na.rm = TRUE)
    } else {
      # If all values are missing for the current ROI, assign NA to min and max
      min_value <- NA
      max_value <- NA
    }
    
    # Calculate the correction factor based on the range
    correction_factor <- (max_value - min_value)
    
    # Calculate the corrected values
    corrected_values <- filtered_data[[RFI]] / correction_factor
    
    # Generate a valid sheet name
    sheet_name <- paste("ROI", gsub("[^[:alnum:]]", "", as.character(identifier)), sep = "")
    sheet_name <- substr(sheet_name, 1, 31)  # Truncate to a maximum of 31 characters
    
    # Combine original columns with corrected values
    combined_data <- cbind(filtered_data, CorrectedValue = corrected_values)
    
    # Write the combined data to the "Corrected Values" worksheet with appropriate headers
    if (identical(identifier, identifiers[1])) {
      # If it's the first ROI, add headers for ROI and CorrectedValue columns
      writeData(wb, sheet = "Corrected Values", x = combined_data, startRow = row_index, colNames = TRUE)
    } else {
      # If it's not the first ROI, append the data to the existing worksheet
      writeData(wb, sheet = "Corrected Values", x = combined_data, startRow = row_index, colNames = FALSE)
    }
    
    # Update the row index for the next ROI
    row_index <- row_index + nrow(combined_data)
  }
  
 
  # Save the workbook to the output file
  saveWorkbook(wb, distance_for_R_out.xlsx, overwrite = TRUE)
}

write_corrected_values("distance_for_R.xlsx", "distance_for_R_out.xlsx", "ROI", "RFI")


#############################################################
################## Create summary of data ###################
#############################################################

newmetadata <- read_xlsx("distance_for_R_out.xlsx")
head(newmetadata)

# Round radius values 
newmetadata$Radius <- round(newmetadata$Radius, 2)

# Create summary of data
summary_data <- newmetadata %>%
  group_by(Radius) %>%
  reframe(Average = mean(CorrectedValue),
          Minimum = min(CorrectedValue),
          Maximum = max(CorrectedValue),
          StandardDeviation = sd(CorrectedValue), 
          Unique_Names = paste(unique(ROI)))

wb <- createWorkbook()
addWorksheet(wb, sheetName = "Summary")
writeData(wb, sheet = "Summary", x = summary_data)
saveWorkbook(wb, "distance_for_R_final.xlsx", overwrite = TRUE)


#############################################################
#################### Create plot of data ####################
#############################################################

finalmetadata <- read_excel("distance_for_R_final.xlsx")

finalmetadata %>% 
  ggplot(aes(x = Radius, y = Average)) +
  geom_ribbon(aes(ymax = Average + StandardDeviation, ymin = Average - StandardDeviation), alpha = 0.3) +
  geom_line(size = 1.5) +
  theme_test() +
  ylab("Scaled Intensity") +
  xlab("Scaled Radius") +
  theme(axis.text.y = element_text(size = 24, color="gray1"),
        axis.title = element_text(size = 24, color="gray1"),
        axis.text.x = element_text(size = 24, color="gray1"))

