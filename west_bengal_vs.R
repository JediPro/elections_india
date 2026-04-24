# Analyze voting patterns in West Bengal Assemble Elections

# Load libraries -------------------------------------
library(tidyverse)
library(sf)

# Set working directory ------------------------------
setwd("C:\\Stuff\\Datasets\\GitHub\\elections_india\\")

# Fetch Assemble constituency data for WB ---------------------
url_prefix <- paste0("https://github.com/yashveeeeeeer/india-geodata/",
                     "blob/main/data/electoral/assembly-constituencies/eci-statewise/")
table_ac_shp <- tibble(state_num = seq.int(from = 1, to = 28, by = 1)) %>% 
  mutate(state_num = str_pad(string = state_num, width = 2, side = "left", pad = "0") %>% paste0("S", ., "_AC"),
         index = list(c(".dbf",".shp",".shx"))) %>% 
  unnest_longer(col = index) %>% 
  mutate(file_name = paste0(state_num, index),
         download = map(.x = file_name, .f = function(x) {
           download.file(url = paste0(url_prefix, x), destfile = paste0("state_vs_ac/", x))}))

download.file(url = paste0(url_prefix, "S19_AC.dbf"), destfile = "S19_AC.dbf")