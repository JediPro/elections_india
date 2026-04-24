# Analyze voting patterns in West Bengal Assemble Elections

# Load libraries -------------------------------------
library(tidyverse)
library(sf)

# Set working directory ------------------------------
setwd("C:\\Stuff\\Datasets\\GitHub\\elections_india\\")

# Fetch Assemble constituency shapefile for WB ---------------------
sf_wb_ac <- st_read(dsn = "state_vs_ac/S25_AC.shp")





