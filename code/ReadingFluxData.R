####
#### Read in GPP estimates for Ameriflux sites 
#### Data prepped by Matt Roby 
####

setwd("/Users/davidmoore/Documents/RProjects/SFIC_eddyflux")

library(tidyverse)
# Install packages if not already installed
if (!requireNamespace("raster", quietly = TRUE)) install.packages("raster")
if (!requireNamespace("dplyr", quietly = TRUE)) install.packages("dplyr")
if (!requireNamespace("sp", quietly = TRUE)) install.packages("sp")

library(raster)
library(dplyr)
library(sp) # Ensure the sp package is loaded for SpatialPoints
library(readxl)

GPPSites=read.csv(file='data/Avg_GPP.csv', header = T )
GPPCLM_Sites=read.csv(file='data/CLM_Amerifluxsites_meanannualGPP_1995to2014_CLM50.csv', header=T)
headers = read.csv(file='data/annualfluxes.csv', skip = 0, header = F, nrows = 1, as.is = T)
Site_fluxes = read.csv(file='data/annualfluxes.csv', skip = 2, header = F)
colnames(Site_fluxes)= headers

  GPPCLM_Sites= rename(GPPCLM_Sites, GPPCLM= Mean.Annual.GPP..gC.m2.day.)
  GPPCLM_Sites= rename(GPPCLM_Sites, site= names)
  GPPtest <- inner_join(GPPCLM_Sites, GPPSites, by="site")
  
  # Using plot for a simple dot plot
  
  GPPSites$SITEIND <- factor(GPPSites$site, exclude=NULL)
  plot(GPPSites$SITEIND, GPPSites$GPPnight, xlab="Site", ylab="GPP", pch=19, col="red", main="Dot Plot of Values by Category")
  GPPSites$site
  
  ####PLOT CLM vs GPP from towers
  plot(GPPtest$GPPday,GPPtest$GPPCLM)
  
  
  # Read the main data file
  Soil_data <- read.csv("SFIC_eddyflux/data/FLUXNET_SoilData_04052024.csv", na.strings = c("", "NA"))
  
  # Read the file with acronym and full names
  acronyms <- read.csv("SFIC_eddyflux/data/FLUXNET_SoilData_CODES.csv", na.strings = c("", "NA"))  # Replace with your actual filename
  # Filter out rows with NA in Soil_Order
  Soil_data_clean <- Soil_data %>%
    filter(!is.na(Soil_Order))  %>%
  rename(site = 'Site_ID' ) %>%
    mutate (site = as.factor(site))
  
  
  # load KoppenClim
  # If not, load it here again
  period <- '1986-2010'
  r <- raster(paste('SFIC_eddyflux/data/KOPPENCLIM/KG_', period, '.grd', sep=''))

# 
sites <- read_excel("SFIC_eddyflux/data/AmerifluxDataAvailability.xlsx")
# 
hist(sites$`Elevation (m)`)
StateFactors = read.csv(file = 'SFIC_eddyflux/data/statefactors.csv')
StateFactors = rename(StateFactors,site = names )

sites = sites %>%
  rename(site = 'Site ID' ) %>%
  mutate (site = as.factor(site))

hist (Site_fluxes$ET_mm_year)
hist (Site_fluxes$GPPdayPartition_gCm2y1)

#plot(sites$RockType, sites$`Mean Average Precipitation (mm)`)

#SF_Fluxes = left_join(sites, Site_fluxes, by="site")
SF_Fluxes = full_join(Site_fluxes, StateFactors, by="site") 
SF_Fluxes = full_join(SF_Fluxes, GPPCLM_Sites, by="site") 
SF_Fluxes = inner_join(SF_Fluxes, Soil_data_clean, by="site")


# Add an identifier column to the original dataframe
SF_Fluxes$id <- seq_len(nrow(SF_Fluxes))

# Filter out rows with NA values in latitude or longitude columns
SF_Fluxes_clean <- SF_Fluxes[!is.na(SF_Fluxes$`Latitude (degrees)...16`) & !is.na(SF_Fluxes$`Longitude (degrees)...3`), ]

SF_Fluxes_clean <- SF_Fluxes

# 
# # Assuming SF_Fluxes_clean$lon and SF_Fluxes_clean$lat are your longitude and latitude columns
# coordinates <- cbind(SF_Fluxes_clean$lon, SF_Fluxes_clean$lat)
# sp_points <- SpatialPoints(coordinates)
# crs(sp_points) <- crs(r)  # Ensuring sp_points has the same CRS as r
# 
# # Use the extract function from the raster package explicitly
# KG_classifications <- raster::extract(r, sp_points)
# 
# print(class(r))           # Should return 'RasterLayer'
# print(class(sp_points))   # Should return 'SpatialPoints'
# 
# crs(r) <- CRS("+proj=longlat +datum=WGS84")
# 
# 
# # Create SpatialPoints from the cleaned data
# coordinates <- cbind(SF_Fluxes_clean$lon, SF_Fluxes_clean$lat)
# sp_points <- SpatialPoints(coordinates, proj4string=crs(r))
# # Extract Köppen-Geiger classifications
# KG_classifications <- extract(r, sp_points)
# 
# # Add the classifications back into the cleaned dataframe
# SF_Fluxes_clean$KG_Classification <- KG_classifications


head(SF_Fluxes_clean)

# Add an identifier column to original DF if not already present
SF_Fluxes$id <- 1:nrow(SF_Fluxes)

# Perform the filtering and classification extraction here as shown earlier

# Then merge back, if necessary, to align with original data structure
#SF_Fluxes_final <- merge(SF_Fluxes, SF_Fluxes_clean[, c("id", "KG_Classification")], by = "id", all.x = TRUE)
SF_Fluxes_final <-SF_Fluxes_clean

plot(SF_Fluxes_final$GPPnightPartition_gCm2y1,SF_Fluxes$CLAY_DCP)

plot(SF_Fluxes_final$GPPnightPartition_gCm2y1,SF_Fluxes$GPPCLM*1000)

plot(SF_Fluxes_final$GPPnightPartition_gCm2y1, SF_Fluxes$GPPCLM*1000, axes=FALSE, xlab="X-Axis Label", ylab="Y-Axis Label")

# Adding X-axis with custom settings
axis(1, at=seq(0, 2500, by=500), las=1)  # las=1 makes the labels horizontal

# Adding Y-axis with custom settings
axis(2, at=seq(0, 3000, by=1000), las=1)

boxplot(GPPnightPartition_gCm2y1 ~ PARMATCLASS, data = SF_Fluxes_final, 
        xlab = "Parent Materia", ylab = "GPPnightPartition_gCm2y1", 
        las = 2, cex.axis = 0.7, 
        main = "Distribution of GPPnightPartition_gCm2y1 by Parent Material")

data=SF_Fluxes_final
# Ensure the directory exists
plots_dir <- "SFIC_eddyflux/plots/SoilExplore"
if (!dir.exists(plots_dir)) {
  dir.create(plots_dir, recursive = TRUE)
}

# Loop through each column in the dataframe
for (x_col in names(data)) {
  # Skip the GPPnightPartition_gCm2y1 column itself
  if (x_col == "GPPnightPartition_gCm2y1") next
  
  # Determine the type of the column
  if (is.numeric(data[[x_col]])) {
    # It's numeric, so make an XY plot
    plot_title <- paste("GPPnightPartition_gCm2y1 vs", x_col, "(XY Plot)")
    file_name <- paste0(plots_dir, "/", gsub("[ /]", "_", x_col), "_xy_plot.png")
    png(file_name, width = 800, height = 600)
    plot(data[[x_col]], data$GPPnightPartition_gCm2y1,
         xlab = x_col, ylab = "GPPnightPartition_gCm2y1",
         main = plot_title)
    dev.off()
  } else if (is.factor(data[[x_col]]) || is.character(data[[x_col]])) {
    # It's a factor or character, so make a boxplot
    plot_title <- paste("GPPnightPartition_gCm2y1 by", x_col, "(Box Plot)")
    file_name <- paste0(plots_dir, "/", gsub("[ /]", "_", x_col), "_box_plot.png")
    png(file_name, width = 800, height = 600)
    boxplot(data$GPPnightPartition_gCm2y1 ~ data[[x_col]],
            xlab = x_col, ylab = "GPPnightPartition_gCm2y1",
            main = plot_title, las = 2)
    dev.off()
  }
}

plot(data$Topsoil_Organic_Carbon[data$Topsoil_Organic_Carbon<10],data$GPPdayPartition_gCm2y1[data$Topsoil_Organic_Carbon<10])







SF_fluxes_red =SF_Fluxes %>%
  filter(!is.na(RockType)) %>%
  filter(!is.na(GPPdayPartition_gCm2y1)) %>%
  filter(GPPdayPartition_gCm2y1>0) %>%
  mutate (Koppen_Geiger = as.factor(Koppen_Geiger)) %>%
  mutate (Elevation = `Elevation..m.`) %>%
  mutate (PARMATCLASS = as.factor(`PARMATCLASS`))%>%
  mutate (CodeKoppen_Geiger = as.factor(`Climate.Class.Abbreviation..Koeppen.`))


SF_fluxes_aov = SF_fluxes_red %>%
  top_n(n() * .85)

#  filter(CodeKoppen_Geiger!="Cwa")

# Two Way Factorial Design
fit_Three <- aov(GPPnightPartition_gCm2y1 ~ 
                   Koppen_Geiger + 
                   PARMATCLASS + 
                   Elevation,
                 data=SF_fluxes_aov)
resFit3_GPPNight = residuals(fit_Three)
Output_fitThree=proj(fit_Three)

#Summary Statistics
summary(fit_Three)
resFit3_GPPNight
write.csv(Output_fitThree, "Output_Fit3_GPPNight.csv")

# Create a data frame of residuals

residuals_data <- data.frame(Residuals = resFit3_GPPNight)
SF_fluxes_aov$Residuals <- resFit3_GPPNight

length(resFit3_GPPNight)
resFit3_GPPNight


# Write to CSV
write.csv(residuals_data, "Residuals_Fit3_GPPNight.csv", row.names = FALSE)


#Residual Plots

data=SF_fluxes_aov
# Loop through each column in the dataframe
for (x_col in names(data)) {
  # Skip the Residuals column itself
  if (x_col == "Residuals") next
  
  # Determine the type of the column
  if (is.numeric(data[[x_col]])) {
    # It's numeric, so make an XY plot
    plot_title <- paste("Residuals vs", x_col, "(XY Plot)")
    file_name <- paste0(plots_dir, "/", gsub("[ /]", "_", x_col), "_residual_plot.png")
    png(file_name, width = 800, height = 600)
    plot(data[[x_col]], data$Residuals,
         xlab = x_col, ylab = "Residuals",
         main = plot_title)
    dev.off()
  } else if (is.factor(data[[x_col]]) || is.character(data[[x_col]])) {
    # It's a factor or character, so make a boxplot
    plot_title <- paste("Residuals by", x_col, "(Box Plot)")
    file_name <- paste0(plots_dir, "/", gsub("[ /]", "_", x_col), "_residuals_box_plot.png")
    png(file_name, width = 800, height = 600)
    boxplot(data$Residuals ~ data[[x_col]],
            xlab = x_col, ylab = "Residuals",
            main = plot_title, las = 2)
    dev.off()
  }
}




boxplot(SF_fluxes_aov$GPPnightPartition_gCm2y1 ~ SF_fluxes_aov$Soil_Order, data = SF_Fluxes, 
        xlab = "Soil Order", ylab = "GPPnightPartition_gCm2y1", 
        las = 2, cex.axis = 0.7, 
        main = "Distribution of GPPnightPartition_gCm2y1 by Soil_Order")


boxplot(SF_fluxes_aov$GPPnightPartition_gCm2y1 ~ SF_fluxes_aov$Soil_Order, data = SF_Fluxes, 
        xlab = "Soil Order", ylab = "GPPnightPartition_gCm2y1", 
        las = 2, cex.axis = 0.7, 
        main = "Distribution of GPPnightPartition_gCm2y1 by Soil_Order")


ResAnova = aov(resFit3_GPPNight~SF_fluxes_aov$CLAY_DCP)

summary(ResAnova)

#####
fit_Three_ET <- aov(ET_mm_year ~ 
                      Koppen_Geiger + 
                      PARMATCLASS + 
                      Elevation,
                    data=SF_fluxes_aov)

resFit3_ET = residuals(fit_Three_ET)
Output_fitThree=proj(fit_Three_ET)


write.csv(Output_fitThree, "Output_Fit3_ET.csv")
######
summary(fit_Three_ET)

#Residual Plots
plot(SF_fluxes_aov$ET_mm_year,resFit3_ET)
plot(SF_fluxes_aov$Topsoil_Organic_Carbon,resFit3_ET)


boxplot(SF_fluxes_aov$GPPnightPartition_gCm2y1 ~ SF_fluxes_aov$Soil_Order, data = SF_Fluxes, 

boxplot(resFit3_ET ~ SF_fluxes_aov$Soil_Order)


SF_fluxes_aovPOST = SF_fluxes_aov

SF_fluxes_aovPOST['resFit3_ET'] <- resFit3_ET
SF_fluxes_aovPOST['resFit3_GPPNight'] <- resFit3_GPPNight


SF_fluxes_aovPOST %>%
  mutate(PFT = as.factor(Vegetation.Description..IGBP.)) %>%
  group_by(PFT) %>%
  summarise(across(where(is.numeric), ~sum(., na.rm = TRUE)))  # Sum only numeric columns, ignoring NA values



#write.csv(SF_fluxes_aovPOST, "SF_fluxes_aovPOST.csv")
# 
# 
# RES_PFT=read.csv(file='./data/Residuals_PFT.csv', header = T )
# x1  = factor(RES_PFT$PFT, levels=c("CSH", "DBF", "WET", "GRA", "WSA", "ENF", "MF", "OSH","CRO"))
# 
# op<-par(no.readonly=TRUE) #this is done to save the default settings 
# par(cex.lab=1.5,cex.axis=1.3, tcl=0.4)
# #change the sizes of the axis labels and axis title
# plot(x1,RES_PFT$resFit3_GPPNight,
# xlab="Plant Functional Type", ylab="GPP Residuals gC/year", ylim=c(-500, 500))
# #if we want big axis titles and labels we need to set more space for them
# par(mar=c(6,6,3,3),cex.axis=1.5,cex.lab=1.5)

# plot (SF_fluxes_red$CodeKoppen_Geiger, SF_fluxes_red$ET_mm_year)
# plot (SF_fluxes_aov$CodeKoppen_Geiger, SF_fluxes_aov$GPPdayPartition_gCm2y1)


#################
#### CLM analysis
#################
# Two Way Factorial Design
fit_CLMGPP_sf <- aov(GPPCLM*365 ~ 
                       Koppen_Geiger + 
                       PARMATCLASS + 
                       Elevation,
                     data=SF_fluxes_aov)
resfit_CLMGPP_sf = residuals(fit_CLMGPP_sf)
Output_fit_CLMGPP_sf=proj(fit_CLMGPP_sf)
write.csv(Output_fit_CLMGPP_sf, "Output_fit_CLMGPP_sf.csv")
#Summary Statistics
summary(fit_CLMGPP_sf)
summary(fit_Three)


##########
##END CLM 
#########




#Organic content
SF_fluxes_aovPOST_res = full_join (SF_fluxes_aovPOST, RES_PFT, by="site" )

op<-par(no.readonly=TRUE) #this is done to save the default settings 
par(cex.lab=1.5,cex.axis=1.3, tcl=0.4)
#change the sizes of the axis labels and axis title
plot(SF_fluxes_aovPOST_res$Subsoil_Organic_Carbon,SF_fluxes_aovPOST_res$resFit3_GPPNight.y,
     xlab="Sub Soil Organic C", ylab="GPP Residuals gC/year", ylim=c(-500, 500))
#if we want big axis titles and labels we need to set more space for them
par(mar=c(6,6,3,3),cex.axis=1.5,cex.lab=1.5)

plot(SF_fluxes_aovPOST_res$resFit3_GPPNight)


plot(SF_fluxes_aovPOST$GPPdayPartition_gCm2y1, SF_fluxes_aovPOST$Subsoil_Organic_Carbon)
     
     
op<-par(no.readonly=TRUE) #this is done to save the default settings 
par(cex.lab=1.5,cex.axis=1.3, tcl=0.4)
#change the sizes of the axis labels and axis title
plot(SF_fluxes_aovPOST$Topsoil_Organic_Carbon,RES_PFT$resFit3_GPPNight,
     xlab="Top Soil Organic C", ylab="GPP Residuals gC/year", ylim=c(-500, 500))
#if we want big axis titles and labels we need to set more space for them
par(mar=c(6,6,3,3),cex.axis=1.5,cex.lab=1.5)


#
op<-par(no.readonly=TRUE) #this is done to save the default settings 
par(cex.lab=2.5,cex.axis=2.5, tcl=0.4)
#change the sizes of the axis labels and axis title
plot (SF_fluxes_aov$CodeKoppen_Geiger, SF_fluxes_aov$GPPdayPartition_gCm2y1,
      xlab="Koppen Geiger", ylab="GPP  gC/year")
#if we want big axis titles and labels we need to set more space for them
par(mar=c(6,8,3,3),cex.axis=3,cex.lab=3)

op<-par(no.readonly=TRUE) #this is done to save the default settings 
par(cex.lab=2.5,cex.axis=2.5, tcl=0.4)
#change the sizes of the axis labels and axis title
axis(side=1, at = labels, labels=labels )
plot (SF_fluxes_red$RockType, SF_fluxes_red$GPPdayPartition_gCm2y1,
      xlab="", ylab="GPP  gC/year")
#if we want big axis titles and labels we need to set more space for them
par(mar=c(6,8,3,3),cex.axis=3,cex.lab=3)

op<-par(no.readonly=TRUE) #this is done to save the default settings 
par( cex.lab=2.5,cex.axis=2.5, tcl=0.4, pch=19)
#change the sizes of the axis labels and axis title
axis(side=1, at = labels, labels=labels )
plot (SF_fluxes_red$Elevation,SF_fluxes_red$GPPdayPartition_gCm2y1, 
      xlab="", ylab="GPP  gC/year")
#if we want big axis titles and labels we need to set more space for them
par(mar=c(6,8,3,3),cex.axis=3,cex.lab=3)


plot (SF_Fluxes$Koppen_Geiger, SF_Fluxes$Topsoil_Organic_Carbon)

SF_Fluxes$`Elevation (m)`


fit_KOP <- aov(ET_mm_year ~ Koppen_Geiger, data=SF_Fluxes)
summary(fit_KOP)

fit_Rock <- aov(ET_mm_year ~ RockType, data=SF_Fluxes)
summary(fit_Rock)


fit <- aov(y ~ A*B, data=mydataframe) # same thing




