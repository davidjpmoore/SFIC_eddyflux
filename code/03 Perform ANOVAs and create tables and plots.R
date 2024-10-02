# Load necessary libraries
library(tidyverse)
library(raster)
library(sp)
library(readxl)
library(broom)
library(knitr)

# Set working directory (ensure your directory is correct)
setwd("/Users/davidmoore/Documents/RProjects/SFIC_eddyflux")

# Load data
load("data/NEON_plus_FluxMeans_AMF8.Rdata")
GPPCLM_Sites <- read.csv('data/CLM_Amerifluxsites_meanannualGPP_1995to2014_CLM50.csv')
Soil_data <- read.csv("data/FLUXNET_SoilData_04052024.csv", na.strings = c("", "NA"))
StateFactors <- read.csv('data/statefactors.csv')
sites <- read_excel("data/AmerifluxDataAvailability.xlsx")

# Clean and merge datasets
GPPCLM_Sites <- GPPCLM_Sites %>%
  rename(GPPCLM = Mean.Annual.GPP..gC.m2.day., SITEID = names)

Soil_data_clean <- Soil_data %>%
  filter(!is.na(Soil_Order)) %>%
  rename(SITEID = Site_ID) %>%
  mutate(SITEID = as.factor(SITEID))

sites <- sites %>%
  rename(SITEID = `Site ID`) %>%
  mutate(SITEID = as.factor(SITEID))

# Rename 'names' to 'SITEID' in StateFactors
StateFactors <- StateFactors %>%
  rename(SITEID = names)  # Rename the column 'names' to 'SITEID'

# Check if SITEID exists in all datasets
if (!"SITEID" %in% colnames(NEON_plus_FluxMeans_AMF8)) {
  stop("SITEID is missing in NEON_plus_FluxMeans_AMF8!")
}
if (!"SITEID" %in% colnames(StateFactors)) {
  stop("SITEID is missing in StateFactors!")
}
if (!"SITEID" %in% colnames(GPPCLM_Sites)) {
  stop("SITEID is missing in GPPCLM_Sites!")
}
if (!"SITEID" %in% colnames(Soil_data_clean)) {
  stop("SITEID is missing in Soil_data_clean!")
}

# Merge datasets
SF_Fluxes <- NEON_plus_FluxMeans_AMF8 %>%
  full_join(StateFactors, by = "SITEID") %>%
  full_join(GPPCLM_Sites, by = "SITEID") %>%
  inner_join(Soil_data_clean, by = "SITEID") %>%
  mutate(
    Latitude = coalesce(Latitude, Lat, lat),  # Combine latitude columns
    Longitude = coalesce(Longitude, Lon, lon)  # Combine longitude columns
  ) %>%
  filter(!is.na(Latitude) & !is.na(Longitude))  # Filter out missing lat/lon

# Continue with further processing...


# Plot GPP_DT_VUT_REF vs GPPCLM
plot(SF_Fluxes$mean_GPP_DT_VUT_REF, SF_Fluxes$GPPCLM * 1000, 
     xlab = "GPP DT VUT REF", ylab = "GPP CLM")

# Ensure Koppen_Geiger is treated as a factor and filter for ANOVA
SF_fluxes_red <- SF_Fluxes %>%
  filter(!is.na(RockType), mean_GPP_DT_VUT_REF > 0, 
         !is.na(Koppen_Geiger), !is.na(PARMATCLASS), !is.na(Elevation_m)) %>%
  mutate(Koppen_Geiger = as.factor(Koppen_Geiger))


# Function to perform ANOVA and return a formatted summary table
perform_anova <- function(dependent_var, df, independent_vars = c("Koppen_Geiger", "PARMATCLASS", "Elevation_m")) {
  # Create formula for the ANOVA dynamically
  formula <- as.formula(paste(dependent_var, "~", paste(independent_vars, collapse = " + ")))
  
  # Perform ANOVA
  fit <- aov(formula, data = df)
  
  # Extract summary as a tidy data frame
  anova_summary <- tidy(fit)
  
  # Add significance stars based on p-value
  anova_summary <- anova_summary %>%
    mutate(Signif = case_when(
      p.value < 0.001 ~ "***",
      p.value < 0.01  ~ "**",
      p.value < 0.05  ~ "*",
      p.value < 0.1   ~ ".",
      TRUE ~ ""
    ))
  
  # Return formatted summary table
  return(anova_summary)
}

# Variables to analyze
variables_to_analyze <- c("mean_GPP_DT_VUT_REF", "GPPCLM", "mean_LE_F_MDS", "mean_NEE_VUT_REF", "mean_RECO_DT_VUT_REF")

# Perform ANOVA for each variable
anova_results <- lapply(variables_to_analyze, function(var) {
  perform_anova(var, SF_fluxes_red)
})

# Print the summary tables for each variable
for (i in seq_along(variables_to_analyze)) {
  print(paste("ANOVA Summary for", variables_to_analyze[i]))
  print(kable(anova_results[[i]], col.names = c("Term", "Df", "Sum Sq", "Mean Sq", "F value", "Pr(>F)", "Signif"),
              caption = paste("ANOVA Summary for", variables_to_analyze[i])))
  cat("\n\n")  # Add some space between tables
}


# Function to streamline repetitive ggplot code
plot_variable_vs_gpp <- function(data, x_var, y_var, x_label, y_label, title, plot_type, fill_var = NULL) {
  p <- ggplot(data, aes(x = .data[[x_var]], y = .data[[y_var]]))  # Use .data[[ ]] for dynamic variables
  
  if (!is.null(fill_var)) {
    p <- p + aes(fill = .data[[fill_var]])  # Dynamically add fill if provided
  }
  
  if (plot_type == "boxplot") {
    p <- p + geom_boxplot()
  } else if (plot_type == "point") {
    p <- p + geom_point() + geom_smooth(method = "loess", se = FALSE, color = "red")
  } else {
    stop("Invalid plot type")
  }
  
  p + labs(title = title, x = x_label, y = y_label) +
    theme_minimal() +
    theme(legend.position = "none")
}


# Plot Mean GPP vs Koppen-Geiger, Parent Material Class, Elevation
# Plot Mean GPP vs Koppen-Geiger
plot_variable_vs_gpp(SF_fluxes_red, "Koppen_Geiger", "mean_GPP_DT_VUT_REF", 
                     "Koppen-Geiger Classification", "Mean GPP (gC m⁻² y⁻¹)", 
                     "Mean GPP vs Koppen-Geiger", "boxplot", "Koppen_Geiger")
# Plot Mean CLM GPP vs Koppen-Geiger
plot_variable_vs_gpp(SF_fluxes_red, "Koppen_Geiger", "GPPCLM", 
                     "Koppen-Geiger Classification", "CLM Mean GPP (gC m⁻² y⁻¹)", 
                     "Mean GPP vs Koppen-Geiger", "boxplot", "Koppen_Geiger")


# Plot Mean GPP vs Parent Material Class
plot_variable_vs_gpp(SF_fluxes_red, "PARMATCLASS", "mean_GPP_DT_VUT_REF", 
                     "Parent Material Class", "Mean GPP (gC m⁻² y⁻¹)", 
                     "Mean GPP vs Parent Material Class", "boxplot", "PARMATCLASS")

# Plot Mean GPP vs Elevation
plot_variable_vs_gpp(SF_fluxes_red, "Elevation_m", "mean_GPP_DT_VUT_REF", 
                     "Elevation (m)", "Mean GPP (gC m⁻² y⁻¹)", 
                     "Mean GPP vs Elevation", "point")


# Function to streamline map plotting
plot_map <- function(df, color_var, title, legend_title) {
  ggplot(df, aes(x = Longitude, y = Latitude)) +
    borders("world", colour = "gray85", fill = "gray80") +
    geom_point(aes_string(color = color_var, size = 14)) +
    scale_color_viridis_c(option = "plasma", name = legend_title) +
    labs(title = title, x = "Longitude", y = "Latitude") +
    theme_minimal() +
    coord_fixed(1.3) +
    xlim(c(-170, -50)) +
    ylim(c(10, 85))
}

# Plot maps
plot_map(SF_fluxes_red, "mean_GPP_DT_VUT_REF", "Mean GPP by Site (North America Focus)", "Mean GPP")
plot_map(SF_fluxes_red, "Elevation_m", "Elevation by Site (North America Focus)", "Elevation (m)")

# Save results (optional)
# write.csv(SF_fluxes_red, "SF_fluxes_cleaned_with_residuals.csv")


# Ungroup and reshape data for combined plotting
SF_fluxes_red <- SF_fluxes_red %>%
  mutate(GPPCLM_gC = GPPCLM*1000)

long_gpp <- SF_fluxes_red %>%
  ungroup() %>%
  pivot_longer(cols = c(GPPCLM_gC, mean_GPP_DT_VUT_REF),
               names_to = "GPP_Type",
               values_to = "GPP_Value")


# Function to streamline the creation of boxplots for GPP values vs different factors
plot_combined_gpp <- function(df, x_var, x_label) {
  ggplot(df, aes_string(x = x_var, y = "GPP_Value", fill = "GPP_Type")) +
    geom_boxplot() +
    labs(title = paste("GPPCLM and GPP_DT_VUT_REF vs", x_label),
         x = x_label,
         y = "GPP (gC m⁻² y⁻¹)") +
    theme_minimal() +
    facet_wrap(~ GPP_Type) +
    theme(legend.position = "top")
}

# Plot GPPCLM and GPP_DT_VUT_REF against Koppen_Geiger
plot_combined_gpp(long_gpp, "Koppen_Geiger", "Koppen-Geiger Classification")

# Plot GPPCLM and GPP_DT_VUT_REF against Parent Material Class (PARMATCLASS)
plot_combined_gpp(long_gpp, "PARMATCLASS", "Parent Material Class")

# Plot GPPCLM and GPP_DT_VUT_REF against Elevation
ggplot(long_gpp, aes(x = Elevation_m, y = GPP_Value, color = GPP_Type)) +
  geom_point() +
  geom_smooth(method = "loess", se = FALSE) +
  labs(title = "GPPCLM and GPP_DT_VUT_REF vs Elevation",
       x = "Elevation (m)", y = "GPP (gC m⁻² y⁻¹)") +
  theme_minimal()



