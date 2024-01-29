library(amerifluxr)
library(flextable)
library(ftExtra)
bif="../data/AMF_AA-Net_BIF_CCBY4_20231208.xlsx"

# Open BADM
badm = amf_read_bif(file = "data/AMF_AA-Net_BIF_CCBY4_20231208.xlsx")

# ------------------- US-NR1

# Subset US-NR1 info from BADM
nr1.badm = badm[badm$SITE_ID == 'US-NR1',]

# Get list of unique variables 
nr1.badm.vars = unique(nr1.badm$VARIABLE)

# Subset by variable and date
nr1.badm.vars.dates = as.data.frame(nr1.badm[grepl('DATE$', nr1.badm$VARIABLE),])

# Create summary table
table = flextable(nr1.badm.vars.dates[,c(3,5)]) %>%
  merge_v(j = c(1,2))



# Load the dplyr package for data manipulation. Install it if you haven't already.
# install.packages("dplyr")
library(dplyr)

# Assuming 'badm' is your data frame and is already loaded in R.

# Filter out zero values and non-numeric values in DATAVALUE
non_zero_values <- badm %>%
  filter(as.numeric(DATAVALUE) != 0, !is.na(as.numeric(DATAVALUE)))

# Group by VARIABLE_GROUP, count non-zero observations, and sort the summary
summary_table <- non_zero_values %>%
  group_by(VARIABLE_GROUP) %>%
  summarise(NonZeroCount = n(), .groups = "drop") %>%
  arrange(desc(NonZeroCount))  # Add this line to sort in descending order

# Display the summary table in a clean and elegant format
print(summary_table)




# Filter out zero values and non-numeric values in DATAVALUE
non_zero_values <- badm %>%
  filter(as.numeric(DATAVALUE) != 0, !is.na(as.numeric(DATAVALUE)))

# Group by VARIABLE_GROUP, and variable count non-zero observations, and sort the summary
summary_table_variable <- non_zero_values %>%
  group_by(VARIABLE_GROUP, VARIABLE) %>%
  summarise(NonZeroCount = n(), .groups = "drop") %>%
  arrange(desc(NonZeroCount))  # Add this line to sort in descending order

# Display the summary table in a clean and elegant format
print(summary_table_variable)




# Filter out zero values and non-numeric values in DATAVALUE
non_zero_values <- badm %>%
  filter(as.numeric(DATAVALUE) != 0, !is.na(as.numeric(DATAVALUE)))

# Group by VARIABLE_GROUP, VARIABLE and SITE_ID count non-zero observations, and sort the summary
summary_table_variableSITE <- non_zero_values %>%
  group_by(VARIABLE_GROUP, VARIABLE, SITE_ID) %>%
  summarise(NonZeroCount = n(), .groups = "drop") %>%
  arrange(desc(NonZeroCount))  # Add this line to sort in descending order

# Display the summary table in a clean and elegant format
print(summary_table_variableSITE)


install.packages("xlsx")
library("xlsx")
write.xlsx(x, file, sheetName = "Sheet1", 
           col.names = TRUE, row.names = TRUE, append = FALSE)

write.csv2(summary_table_variableSITE, file="BADM_byVariable_bySite.csv") 

# Assuming 'badm' is your data frame and is already loaded in R

# Filter out zero values and non-numeric values in DATAVALUE
non_zero_values <- badm %>%
  filter(as.numeric(DATAVALUE) != 0, !is.na(as.numeric(DATAVALUE)))

# Group by VARIABLE_GROUP, count non-zero observations, and sort the summary
summary_table <- non_zero_values %>%
  group_by(VARIABLE_GROUP) %>%
  summarise(NonZeroCount = n(), .groups = "drop") %>%
  arrange(desc(NonZeroCount))

# Filter for the top 20 VARIABLE_GROUPs
top_variable_groups <- summary_table %>%
  top_n(20, NonZeroCount)

# Plot the top 20 VARIABLE_GROUPs
ggplot(top_variable_groups, aes(x = reorder(VARIABLE_GROUP, NonZeroCount), y = NonZeroCount)) +
  geom_bar(stat = "identity") +
  coord_flip() +  # Flip the coordinates to make the bars horizontal
  labs(title = "Top 20 VARIABLE_GROUPs by Non-Zero DATAVALUE Observations", 
       x = "VARIABLE_GROUP", 
       y = "Count of Non-Zero Observations") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))  # To prevent overlapping text on x-axis



# Load the necessary packages
library(dplyr)
library(ggplot2)

# Assuming 'badm' is your data frame and is already loaded in R

# Filter out zero values, non-numeric values in DATAVALUE, and the specified groups
filtered_data <- badm %>%
  filter(as.numeric(DATAVALUE) != 0, !is.na(as.numeric(DATAVALUE))) %>%
  filter(!VARIABLE_GROUP %in% c("GRP_FLUX_MEASUREMENTS", "GRP_LOCATION", "GRP_UTC_OFFSET", "GRP_DOI_CONTRIBUTOR"))

# Group by VARIABLE_GROUP, count non-zero observations, and sort the summary
summary_table <- filtered_data %>%
  group_by(VARIABLE_GROUP) %>%
  summarise(NonZeroCount = n(), .groups = "drop") %>%
  arrange(desc(NonZeroCount))

# Determine the top 20 VARIABLE_GROUPs
top_variable_groups <- summary_table %>%
  slice_max(order_by = NonZeroCount, n = 20)

# Separate the top VARIABLE_GROUPs into those with counts above and below 2500
top_above_2500 <- filter(top_variable_groups, NonZeroCount > 2500)
top_below_2500 <- filter(top_variable_groups, NonZeroCount <= 2500)

# Plot for top VARIABLE_GROUPs with NonZeroCount more than 2500
plot_above_2500 <- ggplot(top_above_2500, aes(x = reorder(VARIABLE_GROUP, NonZeroCount), y = NonZeroCount)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +
  labs(title = "Top 20 VARIABLE_GROUPs with Non-Zero Counts Above 2500", 
       x = "VARIABLE_GROUP", 
       y = "Count of Non-Zero Observations") +
  theme_minimal()

# Plot for top VARIABLE_GROUPs with NonZeroCount less than or equal to 2500
plot_below_2500 <- ggplot(top_below_2500, aes(x = reorder(VARIABLE_GROUP, NonZeroCount), y = NonZeroCount)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +
  labs(title = "Top 20 VARIABLE_GROUPs with Non-Zero Counts Below or Equal to 2500", 
       x = "VARIABLE_GROUP", 
       y = "Count of Non-Zero Observations") +
  theme_minimal()

# Display the plots
plot_above_2500
plot_below_2500






# ... [earlier code remains the same]
# Define a fixed bar width
bar_width = 0.7  # Adjust this value as needed for the best appearance

# Plot for top VARIABLE_GROUPs with NonZeroCount more than 2500
plot_above_2500 <- ggplot(top_above_2500, aes(x = VARIABLE_GROUP, y = NonZeroCount)) +
  geom_bar(stat = "identity", fill = "steelblue", width = bar_width) +
  coord_flip() +
  labs(y = "Observations") +
  theme_minimal()

# Plot for top VARIABLE_GROUPs with NonZeroCount less than or equal to 2500
plot_below_2500 <- ggplot(top_below_2500, aes(x = VARIABLE_GROUP, y = NonZeroCount)) +
  geom_bar(stat = "identity", fill = "steelblue", width = bar_width) +
  coord_flip() +
  labs(y = "Observations") +
  theme_minimal()

# Display the plots
plot_above_2500
plot_below_2500




#### by SITE ID
# Load the dplyr package for data manipulation. Install it if you haven't already.
# install.packages("dplyr")
library(dplyr)

# Assuming 'badm' is your data frame and is already loaded in R.

# Filter out zero values and non-numeric values in DATAVALUE
non_zero_values <- badm %>%
  filter(as.numeric(DATAVALUE) != 0, !is.na(as.numeric(DATAVALUE)))

# Group by SITE_ID and VARIABLE_GROUP and count non-zero observations
summary_table_bysite <- non_zero_values %>%
  group_by(SITE_ID, VARIABLE_GROUP) %>%
  summarise(NonZeroCount = n(), .groups = "drop")

# View the summary table
print(summary_table)

# Export the summary table to a CSV file
write.csv(summary_table, "summary_table_by_site.csv", row.names = FALSE)


# Load the necessary packages
library(dplyr)
library(ggplot2)

# Assuming 'badm' is your data frame and is already loaded in R

# Filter out zero values and non-numeric values in DATAVALUE
filtered_data <- badm %>%
  filter(as.numeric(DATAVALUE) != 0, !is.na(as.numeric(DATAVALUE)))

# Group by VARIABLE_GROUP, count non-zero observations, and sort the summary
summary_table <- filtered_data %>%
  group_by(VARIABLE_GROUP) %>%
  summarise(NonZeroCount = n(), .groups = "drop") %>%
  arrange(desc(NonZeroCount))

# Determine the top 25 VARIABLE_GROUPs
top_variable_groups <- summary_table %>%
  slice_max(order_by = NonZeroCount, n = 25) %>%
  pull(VARIABLE_GROUP)

# Filter the original data for these top VARIABLE_GROUPs
top_group_data <- filtered_data %>%
  filter(VARIABLE_GROUP %in% top_variable_groups)


setwd("plots/BADMSummaries/")
# Loop through each VARIABLE_GROUP and create and save a plot
for (variable_group in top_variable_groups) {
  group_data <- top_group_data %>%
    filter(VARIABLE_GROUP == variable_group) %>%
    group_by(SITE_ID, VARIABLE) %>%
    summarise(NonZeroCount = n(), .groups = "drop")
  
  plot <- ggplot(group_data, aes(x = VARIABLE, y = NonZeroCount, fill = SITE_ID)) +
    geom_bar(stat = "identity", position = "stack") +
    coord_flip() +
    labs(title = paste("Non-Zero Counts for", variable_group), 
         x = "VARIABLE", 
         y = "Non-Zero Count") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1))
  
  # Construct the file name for the plot
  file_name <- paste0("Variable_Group_", gsub(" ", "_", variable_group), ".png")
  
  # Save the plot to a file
  ggsave(file_name, plot, width = 10, height = 7)
}



# Load the necessary packages
library(dplyr)
library(ggplot2)
library(scales)  # For additional color palettes

# Load the necessary packages
library(dplyr)
library(ggplot2)
library(RColorBrewer)  # For color palettes


# Assuming 'badm' is your data frame and is already loaded in R

# Filter out zero values and non-numeric values in DATAVALUE
filtered_data <- badm %>%
  filter(as.numeric(DATAVALUE) != 0, !is.na(as.numeric(DATAVALUE)))

# Determine the top 25 VARIABLE_GROUPs
top_variable_groups <- filtered_data %>%
  group_by(VARIABLE_GROUP) %>%
  summarise(NonZeroCount = n(), .groups = "drop") %>%
  arrange(desc(NonZeroCount)) %>%
  slice_max(order_by = NonZeroCount, n = 25) %>%
  pull(VARIABLE_GROUP)

# Filter the original data for these top VARIABLE_GROUPs
top_group_data <- filtered_data %>%
  filter(VARIABLE_GROUP %in% top_variable_groups)

# Define a color palette
color_palette <- brewer.pal(8, "Dark2")  # Adjust the palette and number of colors as needed

# Assign colors to SITE_IDs by cycling through the color palette
site_ids <- unique(filtered_data$SITE_ID)
color_assignment <- setNames(rep(color_palette, length.out = length(site_ids)), site_ids)

# Loop through each VARIABLE_GROUP and create and save a plot
for (variable_group in top_variable_groups) {
  group_data <- top_group_data %>%
    filter(VARIABLE_GROUP == variable_group) %>%
    group_by(SITE_ID, VARIABLE) %>%
    summarise(NonZeroCount = n(), .groups = "drop")
  
  plot <- ggplot(group_data, aes(x = VARIABLE, y = NonZeroCount, fill = SITE_ID)) +
    geom_bar(stat = "identity", position = "stack") +
    coord_flip() +
    scale_fill_manual(values = color_assignment) +
    labs(title = paste("Non-Zero Counts for", variable_group), 
         x = "VARIABLE", 
         y = "Non-Zero Count") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1))
  
  file_name <- paste0("Variable_Group_", gsub(" ", "_", variable_group), ".png")
  ggsave(file_name, plot, width = 10, height = 7, bg = "white")
}







# Load the necessary packages
library(dplyr)
library(ggplot2)
library(RColorBrewer)

# Assuming 'badm' is your data frame and is already loaded in R

# Filter out zero values and non-numeric values in DATAVALUE
filtered_data <- badm %>%
  filter(as.numeric(DATAVALUE) != 0, !is.na(as.numeric(DATAVALUE)))

# Determine the top 25 VARIABLE_GROUPs
top_variable_groups <- filtered_data %>%
  group_by(VARIABLE_GROUP) %>%
  summarise(NonZeroCount = n(), .groups = "drop") %>%
  arrange(desc(NonZeroCount)) %>%
  slice_max(order_by = NonZeroCount, n = 25) %>%
  pull(VARIABLE_GROUP)

# Filter the original data for these top VARIABLE_GROUPs
top_group_data <- filtered_data %>%
  filter(VARIABLE_GROUP %in% top_variable_groups)

# Define a color palette
color_palette <- brewer.pal(8, "Dark2")  # Adjust the palette and number of colors as needed

# Assign colors to SITE_IDs by cycling through the color palette
site_ids <- unique(filtered_data$SITE_ID)
color_assignment <- setNames(rep(color_palette, length.out = length(site_ids)), site_ids)

# Loop through each VARIABLE_GROUP and create and save a plot
for (variable_group in top_variable_groups) {
  group_data <- top_group_data %>%
    filter(VARIABLE_GROUP == variable_group) %>%
    group_by(SITE_ID, VARIABLE) %>%
    summarise(NonZeroCount = n(), .groups = "drop")
  
  plot <- ggplot(group_data, aes(x = VARIABLE, y = NonZeroCount, fill = SITE_ID)) +
    geom_bar(stat = "identity", position = "stack") +
    coord_flip() +
    scale_fill_manual(values = color_assignment) +
    labs(title = paste("Non-Zero Counts for", variable_group), 
         x = "VARIABLE", 
         y = "Non-Zero Count") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1),
          legend.position = "none")  # Suppress the legend
  
  file_name <- paste0("Variable_Group_", gsub(" ", "_", variable_group), ".png")
  ggsave(file_name, plot, width = 10, height = 7, bg = "white")
}



# Load the necessary packages
library(dplyr)
library(ggplot2)
library(RColorBrewer)

# Assuming 'badm' is your data frame and is already loaded in R

# Filter out zero values and non-numeric values in DATAVALUE
filtered_data <- badm %>%
  filter(as.numeric(DATAVALUE) != 0, !is.na(as.numeric(DATAVALUE)))

# Define the list of specific variables to be considered
selected_variables <- c('LAI', 'BASAL_AREA', 'TREES_NUM',  
                        'AG_LIT_PROD_TOT', 'AG_PROD_TREE', 'BIOMASS_N', 
                        'LAI_TOT', 'ROOT_BIOMASS_FINE', 'ROOT_BIOMASS_CRS', 
                        'SOIL_CHEM_N_TOT', 'SOIL_CHEM_C_ORG', 'SOIL_TEXT_SAND')

# Filter data for the selected VARIABLES
subset_data <- filtered_data %>%
  filter(VARIABLE %in% selected_variables)

# Define a color palette for SITE_IDs
color_palette <- brewer.pal(8, "Dark2")
site_ids <- unique(subset_data$SITE_ID)
color_assignment <- setNames(rep(color_palette, length.out = length(site_ids)), site_ids)

# Create the plot for the selected variables across all variable groups
plot_KeyVARS <- ggplot(subset_data, aes(x = VARIABLE, y = as.numeric(DATAVALUE), fill = SITE_ID)) +
  geom_bar(stat = "identity", position = "stack") +
  coord_flip() +
  scale_fill_manual(values = color_assignment) +
  labs(title = "Non-Zero Counts for Selected Variables", 
       x = "VARIABLE", 
       y = "Non-Zero Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        legend.position = "none")

# Display the plot
print(plot_KeyVARS)

# Optionally, save the plot
ggsave("selected_variables_plot.png", plot, width = 10, height = 7, bg = "white")


# Load the necessary package
library(dplyr)

# Assuming 'badm' is your data frame and is already loaded in R

# Define the list of specific variables to be considered
selected_variables <- c('LAI', 'BASAL_AREA', 'TREES_NUM', 'AG_BIOMASS_TREE', 
                        'AG_LIT_PROD_TOT', 'AG_PROD_TREE', 'BIOMASS_N', 
                        'LAI_TOT', 'ROOT_BIOMASS_FINE', 'ROOT_BIOMASS_CRS', 
                        'SOIL_CHEM_N_TOT', 'SOIL_CHEM_C_ORG', 'SOIL_TEX_SAND')

# Filter data for the selected VARIABLES and non-zero values
filtered_data <- badm %>%
  filter(VARIABLE %in% selected_variables, as.numeric(DATAVALUE) != 0, !is.na(as.numeric(DATAVALUE)))

# Summarize the number of unique SITE_IDs for each VARIABLE
site_id_summaryKEY <- filtered_data %>%
  group_by(VARIABLE) %>%
  summarise(UniqueSiteIDs = n_distinct(SITE_ID), .groups = 'drop')

# Export the summary to a CSV file
write.csv(site_id_summaryKEY, "site_id_summary_per_variable.csv", row.names = FALSE)

