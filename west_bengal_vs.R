# Analyze voting patterns in West Bengal Assemble Elections

# Load libraries -------------------------------------
library(tidyverse)
library(sf)

# Set working directory ------------------------------
setwd("C:\\Stuff\\Datasets\\GitHub\\elections_india\\")

# Load data ------------------------------------
# Fetch Assemble constituency shapefile for WB
sf_wb_ac_raw <- st_read(dsn = "state_vs_ac/S25_AC.shp") %>% 
  st_set_crs(value = 4326)

# Fetch data of past elections from Lok Dhaba https://lokdhaba.ashoka.edu.in/
table_ac_elections <- read_csv("West_Bengal_AE.csv")

# Process files ------------------------------------
# Shapefile
sf_wb_ac <- sf_wb_ac_raw %>% 
  select(ST_CODE, AC_NO, PC_NO, AC_NAME) %>% 
  arrange(AC_NO) %>% 
  rename_with(.fn = snakecase::to_snake_case, .cols = everything())

# Results
data_wb_ac_elections <- table_ac_elections %>% 
  # Remove bye-election data
  drop_na(month) %>% 
  select(Constituency_No, Constituency_Name, Constituency_Type,  
         Year, Assembly_No, Electors, Valid_Votes, ENOP,
         Position, Party, Party_ID, Votes, pid, Candidate, 
         Sex, Age, Candidate_Type, Contested, No_Terms, 
         MyNeta_education, TCPD_Prof_Main, Turncoat) %>% 
  rename_with(.fn = snakecase::to_snake_case, .cols = everything()) %>% 
  # Rename manually
  rename(candidate_id = pid, education = my_neta_education, 
         profession = tcpd_prof_main) %>% 
  # Process columns
  mutate(enop = replace_na(data = enop, replace = 1),
         # Recode NOTA
         candidate = case_when(str_detect(candidate, pattern = "None.*") ~ "NOTA", 
                               TRUE ~ candidate),
         # Replace NAs in candidate type
         candidate_type = case_when(candidate == "NOTA" ~ "NOTA",
                                    candidate != "NOTA" & is.na(candidate_type) & 
                                      constituency_type == "ST" ~ "ST",
                                    candidate != "NOTA" & is.na(candidate_type) & 
                                      constituency_type == "SC" ~ "SC",
                                    candidate_type == "GENERAL" ~ "GEN",
                                    TRUE ~ candidate_type),
         candidate_id = case_when(candidate == "NOTA" ~ "NOTA", 
                                  TRUE ~ candidate_id),
         sex = case_when(candidate == "NOTA" ~ "N",
                         sex == "MALE" ~ "M", TRUE ~ sex)
         ) %>% 
  # Replace missing values with Mode for select columns
  group_by(party) %>% 
  mutate(party_id = case_when(is.na(party_id) ~ median(party_id, na.rm = TRUE), TRUE ~ party_id)) %>% 
  ungroup()

