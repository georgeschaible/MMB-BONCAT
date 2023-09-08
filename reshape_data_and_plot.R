#############################################################
################## Introduction to R code ###################
#############################################################

# This file of R code was written to take the output from the
# python code and reshape the data so that ratios of the 
# radius and the relative fluorescence intensity (RFI) as 
# well as the summary statistics can be calculated. The final 
# output is a plot showing the average and standard deviation 
# of the BONCAT label across the analyzed MMB consortia. The
# plot shows the radius from the center of the averaged MMB
# with the center being represented by the value 0 and the 
# edge of the averaged MMB represented by the value 1, shown
# the the x-axis labeled "Scaled Radius". The Y-axis shows 
# the RFI as a ratio from 0 to 1, with 1 being the highest
# fluorescence intensity.

#############################################################
######### Load libraries and set working directory ##########
#############################################################

library(tidyverse)
library(rstatix)
library(ggpubr)
library(openxlsx)
library(writexl)
library(readxl)


# Set working directory if data and R file are saved in the same folder
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))


#############################################################
################# Processing python output ################## 
#############################################################

# Read the input file. Change seq_len to fit your data
data <- read.table("distance.csv", header = FALSE, sep = ",", 
                 col.names = paste0("V",seq_len(250)), fill = TRUE)

# Create custom row names with "Particle" followed by row numbers
custom_row_names <- paste("Particle", seq_len(nrow(data)), sep = "")
  
# Assign custom row names to the data frame
rownames(data) <- custom_row_names
  
# Convert row names to a regular column and rename it
data <- tibble::rownames_to_column(data, "Particle")
  
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
  
# Create the new data frame with the desired structure
new_data <- data_long %>%
  group_by(Particle) %>%
  mutate(
    ratio_of_radius_cumsum = cumsum(ratio_of_radius),
    ratio_of_radius_cumsum = ifelse(ratio_of_radius_cumsum <= 1, ratio_of_radius_cumsum, 1)  # Limit to 1
  ) %>%
  select(Particle, ratio_of_radius_cumsum, RFI)

# Assign shorter name for ratio of radius column
new_data <- new_data %>% rename(Radius = ratio_of_radius_cumsum)

# Save data frame as Excel file
write.xlsx(new_data, "distance_for_R.xlsx", rowNames = TRUE)
  

#############################################################
############# Function to process python output ############# 
#############################################################

# name input and output files
write_corrected_values <- function(distance_for_R.xlsx, distance_for_R_out.xlsx, Particle, RFI) {
 
   # Read the input file
  metadata <- read_excel("distance_for_R.xlsx")
  
  
  # Get the unique Particles
  identifiers <- unique(metadata[[Particle]])
  
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
  
  # Iterate through each Particle
  for (identifier in identifiers) {
    cat("Processing identifier:", identifier, "\n")
    
    # Filter data for the current Particle
    filtered_data <- filter(metadata, !!sym(Particle) == identifier)
    
    if (any(!is.na(filtered_data[[RFI]]))) {
      # Find the minimum and maximum values, excluding NAs
      min_value <- min(filtered_data[[RFI]], na.rm = TRUE)
      max_value <- max(filtered_data[[RFI]], na.rm = TRUE)
    } else {
      # If all values are missing for the current Particle, assign NA to min and max
      min_value <- NA
      max_value <- NA
    }
    
    # Calculate the correction factor based on the range
    correction_factor <- (max_value - min_value)
    
    # Calculate the corrected values
    corrected_values <- filtered_data[[RFI]] / correction_factor
    
    # Generate a sheet name
    sheet_name <- paste("Particle", gsub("[^[:alnum:]]", "", as.character(identifier)), sep = "")
    sheet_name <- substr(sheet_name, 1, 31)  # Truncate to a maximum of 31 characters
    
    # Combine original columns with corrected values
    combined_data <- cbind(filtered_data, CorrectedValue = corrected_values)
    
    # Write the combined data to the "Corrected Values" worksheet with appropriate headers
    if (identical(identifier, identifiers[1])) {
      # If it's the first Particle, add headers for Particle and CorrectedValue columns
      writeData(wb, sheet = "Corrected Values", x = combined_data, startRow = row_index, colNames = TRUE)
    } else {
      # If it's not the first Particle, append the data to the existing worksheet
      writeData(wb, sheet = "Corrected Values", x = combined_data, startRow = row_index, colNames = FALSE)
    }
    
    # Update the row index for the next Particle
    row_index <- row_index + nrow(combined_data)
  }
  
  # Save the workbook to the output file
  saveWorkbook(wb, distance_for_R_out.xlsx, overwrite = TRUE)
}

# Run function and save new output file
write_corrected_values("distance_for_R.xlsx", "distance_for_R_out.xlsx", "Particle", "RFI")


#############################################################
################## Create summary of data ###################
#############################################################

# Load in recent output
newmetadata <- read_xlsx("distance_for_R_out.xlsx")

# Round radius values 
newmetadata$Radius <- round(newmetadata$Radius, 2)

# Create summary of data
summary_data <- newmetadata %>%
  group_by(Radius) %>%
  reframe(Average = mean(CorrectedValue),
          StandardDeviation = sd(CorrectedValue), 
          Unique_Names = paste(unique(Particle)))

# Create and write data to workbook
wb <- createWorkbook()
addWorksheet(wb, sheetName = "Summary")
writeData(wb, sheet = "Summary", x = summary_data)
saveWorkbook(wb, "distance_for_R_final.xlsx", overwrite = TRUE)


#############################################################
#################### Create plot of data ####################
#############################################################

# Load in final output
finalmetadata <- read_excel("distance_for_R_final.xlsx")

# Replace any NA values in Average column with 0
finalmetadata <- finalmetadata %>%
  mutate(Average = coalesce(Average, 0))

# Create file to save plot
png("line_plot.png", 
    width = 250, 
    height = 250, 
    units='mm', 
    res = 600)

# Create plot (note that this plot shows the radius, not the diameter)
finalmetadata %>% 
  ggplot(aes(x = Radius, y = Average)) +
  geom_ribbon(aes(ymax = Average + StandardDeviation, ymin = Average - StandardDeviation), alpha = 0.3) +
  geom_line(linewidth = 1.5) +
  coord_cartesian(ylim = c(0, 1.05)) +
  theme_test(base_rect_size = 1) +
  theme(axis.text = element_text(size = 26, color="black"),
        axis.title = element_text(size = 26, color="black"),
        axis.text.x = element_text(size = 26, color="black")) +
  ylab("Scaled Intensity") +
  xlab("Scaled Radius")

# Save plot
dev.off()


#############################################################
############## Statistical analysis of data #################
#############################################################

# Remove rows with Radius value equal to 0
finalmetadata <- finalmetadata %>%
  filter(Radius != 0)

# Create a new variable 'quarter' to categorize the data along the radius
finalmetadata$quarter <- cut(finalmetadata$Radius, breaks = c(0, 0.25, 0.5, 0.75, 1), labels = c("Q1", "Q2", "Q3", "Q4"))

# ANOVA test of quarters
anova_test(Average ~ quarter, data = finalmetadata)

# Pairwise t-test of each quarter
pairwise_t_test(Average ~ quarter, data = finalmetadata, p.adjust.method = "bonferroni")

# Pairwise comparison of each quarter of radius against outer radius
compare_means(Average ~ quarter,  data = finalmetadata, ref.group = "Q4", method = "t.test")

# Comparison of each group against base-mean
compare_means(Average ~ quarter,  data = finalmetadata, ref.group = ".all.", method = "t.test")

# Create file to save plot
png("boxlpots.png", 
    width = 250, 
    height = 250, 
    units='mm', 
    res = 600)

# Create a boxplot to show spread of data across quartiles of radius
finalmetadata %>% 
  ggplot(aes(x = quarter, y = Average)) +
  geom_boxplot(fill = "mistyrose2") +
  coord_cartesian(ylim = c(0, 1)) +
  labs(x = "Quarter of radius from center of consortia", y = "Average RFI") +
  theme_test(base_rect_size = 1) +
  theme(axis.text = element_text(size = 26, color="black"),
        axis.title = element_text(size = 26, color="black"),
        axis.text.x = element_text(size = 26, color="black")) +
  stat_compare_means(label = "p.signif", method = "t.test", ref.group = ".all.", label.y = c(0.69, 0.935, 0.805, 0.45)) # lable.y values may need changed

# Save plot
dev.off()
