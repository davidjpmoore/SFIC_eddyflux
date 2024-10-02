
library(dplyr)
library(purrr)
library(tidyr)
library(ggplot2)

#State Factors Interactive Controls paper Oct 2024
# read in FLUXNET formated Ameriflux Data
#Combine with Koppen Geiger 
#Combine with Geological Substrate
#Combine with an elevation model 

# carry out ANOVA
# extract the residuals
# compare residuals with Vegetation Types, Soil Types, Disturbance

load(file="../fluxnetreader/data/FLUXNET_YY_sum/fluxnet_YY_uniq_2024_cleaned.RData")


# Filter sites from Mexico, Canada, and the USA based on the first two characters of SITEID
NorthAm_FLUX <- fluxnet_YY_uniq_2024_cleaned %>%
  filter(substr(SITEID, 1, 2) %in% c("MX", "CA", "US"))

# Count the number of unique years for each site
years_per_site <- NorthAm_FLUX %>%
  group_by(SITEID) %>%
  summarise(num_years = n_distinct(TIMESTAMP))  # Count unique years (TIMESTAMP is already YYYY)

# Create a histogram of the number of years per site
ggplot(years_per_site, aes(x = num_years)) +
  geom_histogram(binwidth = 1, fill = "blue", color = "black") +
  labs(title = "Distribution of Number of Years of Data per Site",
       x = "Number of Years",
       y = "Count of Sites") +
  theme_minimal()

# 25 NEON SITES contain 5 years of data 
# # Filter for SITEID that matches 'US-x$$' pattern
# us_x_sites <- NorthAm_FLUX %>%
#   filter(grepl("^US-x", SITEID))
# 
# # Count the number of unique years for each site that matches the pattern
# years_per_site_us_x <- us_x_sites %>%
#   group_by(SITEID) %>%
#   summarise(num_years = n_distinct(TIMESTAMP))
# 
# # Create a histogram of the number of years per site for US-x$$ pattern
# ggplot(years_per_site_us_x, aes(x = num_years)) +
#   geom_histogram(binwidth = 1, fill = "blue", color = "black") +
#   labs(title = "NEON SITES ONLY Distribution of Number of Years of Data for US-x Sites",
#        x = "Number of Years",
#        y = "Count of Sites") +
#   theme_minimal()
# 

# Filter sites where num_years is 8 or greater
sites_with_8_or_more_years <- years_per_site %>%
  filter(num_years >= 8 )

# Filter sites where num_years is 8 or greater OR NEON sites (SITEID matches 'US-x$$')
NEON_plus_sites_with_8_or_more_years <- years_per_site %>%
  filter(num_years >= 8 | grepl("^US-x", SITEID))

# Create FluxMeans_AMF8 | Flux sites from Ameriflux in FLUXNET format with 8 or more years of data
FluxMeans_AMF8 <- NorthAm_FLUX %>%
  filter(SITEID %in% sites_with_8_or_more_years$SITEID) %>%  # Keep only sites with 8+ years
  group_by(SITEID, Latitude, Longitude, Elevation_m) %>%  # Retain these variables
  summarise(
    mean_GPP_DT_VUT_REF = mean(GPP_DT_VUT_REF, na.rm = TRUE),  # Calculate mean GPP_DT_VUT_REF
    mean_LE_F_MDS = mean(LE_F_MDS, na.rm = TRUE),              # Calculate mean LE_F_MDS
    mean_NEE_VUT_REF = mean(NEE_VUT_REF, na.rm = TRUE),        # Calculate mean NEE_VUT_REF
    mean_RECO_DT_VUT_REF = mean(RECO_DT_VUT_REF, na.rm = TRUE),# Calculate mean RECO_DT_VUT_REF
    
    # Calculate the 95th percentile for each variable
    p95_GPP_DT_VUT_REF = quantile(GPP_DT_VUT_REF, probs = 0.95, na.rm = TRUE),
    p95_LE_F_MDS = quantile(LE_F_MDS, probs = 0.95, na.rm = TRUE),
    p95_NEE_VUT_REF = quantile(NEE_VUT_REF, probs = 0.95, na.rm = TRUE),
    p95_RECO_DT_VUT_REF = quantile(RECO_DT_VUT_REF, probs = 0.95, na.rm = TRUE)
  ) %>%
  mutate(LE_F_MDS_mm_yr = mean_LE_F_MDS * 12.85)  # Convert LE_F_MDS from W m⁻² yr⁻¹ to mm yr⁻¹


# Save the FluxMeans_AMF8 dataframe as an RData file
save(FluxMeans_AMF8, file = "data/FluxMeans_AMF8.Rdata")

#create NEON_plus_FluxMeans_AMF8 | Flux sites from Ameriflux in FLUXNET format with 8 or more years of data
# Join this data with NorthAM_FLUX to calculate the mean for multiple variables (GPP_DT_VUT_REF, LE_F_MDS, NEE_VUT_REF)
NEON_plus_FluxMeans_AMF8 <- NorthAm_FLUX %>%
  filter(SITEID %in% NEON_plus_sites_with_8_or_more_years$SITEID) %>%  # Keep only sites with 8+ years
  group_by(SITEID, Latitude, Longitude, Elevation_m) %>%  # Retain these variables
  summarise(
    mean_GPP_DT_VUT_REF = mean(GPP_DT_VUT_REF, na.rm = TRUE),  # Calculate mean GPP_DT_VUT_REF
    mean_LE_F_MDS = mean(LE_F_MDS, na.rm = TRUE),              # Calculate mean LE_F_MDS
    mean_NEE_VUT_REF = mean(NEE_VUT_REF, na.rm = TRUE),        # Calculate mean NEE_VUT_REF
    mean_RECO_DT_VUT_REF = mean(RECO_DT_VUT_REF, na.rm = TRUE),# Calculate mean RECO_DT_VUT_REF
    
    # Calculate the 95th percentile for each variable
    p95_GPP_DT_VUT_REF = quantile(GPP_DT_VUT_REF, probs = 0.95, na.rm = TRUE),
    p95_LE_F_MDS = quantile(LE_F_MDS, probs = 0.95, na.rm = TRUE),
    p95_NEE_VUT_REF = quantile(NEE_VUT_REF, probs = 0.95, na.rm = TRUE),
    p95_RECO_DT_VUT_REF = quantile(RECO_DT_VUT_REF, probs = 0.95, na.rm = TRUE)
  ) %>%
  mutate(LE_F_MDS_mm_yr = mean_LE_F_MDS * 12.85)  # Convert LE_F_MDS from W m⁻² yr⁻¹ to mm yr⁻¹



# Save the FluxMeans_AMF8 dataframe as an RData file
save(NEON_plus_FluxMeans_AMF8, file = "data/NEON_plus_FluxMeans_AMF8.Rdata")


# Assuming you have a dataframe called mean_gpp_per_site with Latitude, Longitude, and mean_GPP_DT_VUT_REF

# Plotting the map focused on North America
ggplot(NEON_plus_FluxMeans_AMF8, aes(x = Longitude, y = Latitude)) +
  borders("world", colour = "gray85", fill = "gray80") +  # Add a world map background
  geom_point(aes(color = mean_GPP_DT_VUT_REF, size = 14)) +  # Plot the points, color by GPP
  scale_color_viridis_c(option = "plasma", name = "Mean GPP") +  # Use a color scale for GPP
  labs(title = "Mean GPP_DT_VUT_REF by Site (North America Focus)",
       x = "Longitude",
       y = "Latitude") +
  theme_minimal() +
  coord_fixed(1.3) +  # Fix aspect ratio to make the map look correct
  xlim(c(-170, -50)) +  # Set longitude limits to focus on North America
  ylim(c(10, 85))  # Set latitude limits to focus on North America


#Estimate effect of selecting 5 years from 8
#(this is designed to visualize the effect of choosing NEON Sites which have only 5 years of data)

# Step 1: Bootstrap function to calculate bootstrap mean for each SITEID
bootstrap_gpp <- function(df, n_draws = 30, n_years = 5) {
  replicate(n_draws, {
    sample_years <- sample(unique(df$TIMESTAMP), size = n_years, replace = TRUE)
    mean(df %>% filter(TIMESTAMP %in% sample_years) %>% pull(GPP_DT_VUT_REF), na.rm = TRUE)
  }) %>% mean(na.rm = TRUE)
}

# Step 2: Calculate the bootstrap mean for each SITEID without nesting
bootstrap_results <- list()

for (site in unique(NEON_plus_sites_with_8_or_more_years$SITEID)) {
  # Filter the data for the current site
  site_data <- NorthAm_FLUX %>% filter(SITEID == site)
  
  # Apply the bootstrap function to the current site data
  bootstrap_mean_value <- bootstrap_gpp(site_data)
  
  # Store the results in a list (you could also use a dataframe)
  bootstrap_results[[site]] <- data.frame(SITEID = site, bootstrap_mean = bootstrap_mean_value)
}

# Step 3: Convert the list to a dataframe
bootstrap_mean_df <- do.call(rbind, bootstrap_results)

# Step 4: Join the bootstrap_mean_df with NEON_plus_FluxMeans_AMF8
comparison_df <- left_join(
  NEON_plus_FluxMeans_AMF8, 
  bootstrap_mean_df, 
  by = "SITEID"
)

# Step 5: Plot the comparison (example)
ggplot(comparison_df, aes(x = mean_GPP_DT_VUT_REF, y = bootstrap_mean)) +
  geom_point(color = "blue") +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +  # Line y = x
  labs(title = "Bootstrap Mean vs Actual Mean GPP_DT_VUT_REF",
       x = "Actual Mean GPP_DT_VUT_REF",
       y = "Bootstrap Mean GPP_DT_VUT_REF") +
  theme_minimal()
