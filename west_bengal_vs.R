# Analyze voting patterns in West Bengal Assemble Elections

# Load libraries -------------------------------------
library(tidyverse)
library(sf)
library(ggfittext)

# Set working directory ------------------------------
setwd("C:\\Stuff\\Datasets\\GitHub\\elections_india\\")

# Load themes script
source(file = "C:\\Stuff\\Datasets\\vaw_themes.R")

# Load data ------------------------------------
# Fetch Assemble constituency shapefile for WB
sf_wb_ac_raw <- st_read(dsn = "state_vs_ac/S25_AC.shp") %>% 
  st_set_crs(value = 4326)

# Fetch data of past elections from Lok Dhaba https://lokdhaba.ashoka.edu.in/
table_ac_elections <- read_csv("West_Bengal_AE.csv")

# Load AC wise list of eligible voters
table_ac_voter <- read_csv("wb_ac_elector_2026.csv")

# Load turnout data
table_ac_turnout <- readxl::read_xlsx("wb_ac_turnout_2026.xlsx")

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
  rename_with(.fn = ~str_replace(string = .x, pattern = "constituency", replacement = "ac")) %>% 
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
                                      ac_type == "ST" ~ "ST",
                                    candidate != "NOTA" & is.na(candidate_type) & 
                                      ac_type == "SC" ~ "SC",
                                    candidate_type == "GENERAL" ~ "GEN",
                                    TRUE ~ candidate_type),
         candidate_id = case_when(candidate == "NOTA" ~ "NOTA", 
                                  TRUE ~ candidate_id),
         sex = case_when(candidate == "NOTA" ~ "N",
                         sex == "MALE" ~ "M", TRUE ~ sex)
         ) %>% 
  # Replace missing values with Mode for select columns
  group_by(party) %>% 
  mutate(party_id = case_when(is.na(party_id) ~ median(party_id, na.rm = TRUE), 
                              TRUE ~ party_id)) %>% 
  ungroup()

# Process data for voter list
data_ac_voter <- table_ac_voter %>% 
  rename(ac_no = `AC No.`, ac_name = `AC Name`, 
         district = `District Name`, n_eligible = Total) %>% 
  select(ac_no, ac_name, district, n_eligible)
  
# Process turnout data
data_ac_turnout <- table_ac_turnout %>% 
  mutate(constituency = toupper(constituency)) %>% 
  # Recode names
  mutate(constituency = replace_values(x = constituency,
                                   "BAISHNABNAGAR" ~ "BAISHNAB NAGAR",
                                   "BHAGAWANGOLA" ~ "BHAGABANGOLA",
                                   "DABGRAM-FULBARI" ~ "DABGRAM-PHULBARI",
                                   "INDAS" ~ "INDUS",
                                   "KOTULPUR" ~ "KATULPUR",
                                   "LABPUR" ~ "LABHPUR",
                                   "NOWDA" ~ "NAODA",
                                   "RANIBANDH" ~ "RANIBUNDH")) %>% 
  # Get constituency number
  left_join(y = data_wb_ac_elections %>% filter(year == 2021) %>%
              distinct(ac_no, ac_name), by = c("constituency" = "ac_name"))

# Election wise eligible voters and votes polled -----------------------
data_wb_ac_elections %>% 
  group_by(assembly_no, ac_no) %>% 
  summarise(electors = max(electors), votes = max(valid_votes), 
            .groups = "drop") %>% 
  # Yearly counts
  group_by(assembly_no) %>% 
  summarise(across(.cols = c(electors, votes), .fns = sum)) %>% 
  mutate(turnout = votes/electors) %>% 
  pivot_longer(cols = c(electors, votes), 
               names_to = "metric", values_to = "value") %>% 
  # plot
  ggplot() +
  # Column chart for total and actual
  geom_col(mapping = aes(x = assembly_no, y = value, 
                         group = metric, fill = metric), position = "identity",
           colour = "white", linewidth = 2) +
  

# Mosaic plot showing eligible voters, turnout, top party shares --------------
plot_mosaic_election_turnouts <- data_wb_ac_elections %>% 
  # Total votes polled
  group_by(year, party) %>% 
  summarise(votes = sum(votes), .groups = "drop") %>% 
  # Keep top three parties and group the rest
  group_by(year) %>% 
  mutate(votes_polled = sum(votes),
         vote_share = votes/votes_polled,
         # Do not consider Independents and NOTA when finding top parties
         party_rank = row_number(desc(
           case_when(party %notin% c("IND", "NOTA") ~ votes, 
                     TRUE ~ NA_integer_))),
         # Recode parties not in top 3
         party = case_when(party_rank > 2 | is.na(party_rank) ~ "OTH", 
                           TRUE ~ party)) %>% 
  group_by(year, party) %>% 
  summarise(votes = sum(votes), votes_polled = max(votes_polled), 
            vote_share = sum(vote_share), 
            party_rank = min(party_rank, na.rm = TRUE),
            .groups = "drop") %>% 
  ungroup() %>% 
  arrange(year, party_rank)