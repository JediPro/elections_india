# Analyze voting patterns in West Bengal Assemble Elections

# Load libraries -------------------------------------
library(tidyverse)
library(sf)
library(ggfittext)
library(ggalluvial)

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

# Define look up vector for mapping years to assembly number
vec_assembly_year <- data_wb_ac_elections %>% distinct(assembly_no, year) %>% deframe()

# Election wise eligible voters and votes polled -----------------------
plot_election_turnout_trend <- data_wb_ac_elections %>% 
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
  geom_col(mapping = aes(x = assembly_no, y = value, group = metric, fill = metric, 
                         width = ifelse(metric == "electors", 1, 0.7),
                         linewidth = metric), linejoin = "round", lineend = "round",
           position = "identity", colour = "grey7") +
  # Turnout ratio
  geom_text(data = . %>% filter(metric == "votes"), 
            mapping = aes(x = assembly_no, y = value, 
                          label = scales::percent(x = turnout, accuracy = 1)),
            family = "Titillium Web", fontface = "bold", size = 3,
            colour = "white", vjust = 1) +
  # Scales
  scale_x_continuous(name = NULL, breaks = as.numeric(names(vec_assembly_year)), 
                     labels = vec_assembly_year, 
                     expand = expansion(mult = c(0.01, 0.01))) +
  scale_y_continuous(name = "Count (crore)", 
                     labels = scales::label_comma(scale = 1e-7),
                     expand = expansion(mult = c(0.01, 0.01))) +
  scale_fill_manual(breaks = c("electors", "votes"), name = NULL, guide = NULL,
                    values = c("lightblue4", "#06038D"),
                    labels = c("Eligible Voters", "Votes Polled")) +
  scale_linewidth_manual(breaks = c("electors", "votes"), values = c(2, 0.5),
                         labels = c("Eligible Voters", "Votes Polled"), 
                         name = NULL, guide = NULL) +
  # Labels
  labs(title = "Since 1996, turnout for assembly elections has been above 80% every time save for 2001",
       subtitle = "<b style='color:lightblue4'>Light blue bars</b> indicate indicate number of eligible voters, <b style='color:#06038D'>Dark blue bars</b> indicate actual votes polled. The numbers indicate turnout percentage.",
       caption = "Data: yashveeeeeeer.github.io/india-geodata, lokdhaba.ashoka.edu.in, data.opencity.in, News18 | Design: @JediPro"
       ) +
  theme_vaw_dark_mobile() +
  theme(panel.grid.major.x = element_blank(),
        axis.ticks.x = element_blank(),
        legend.position = "inside", legend.position.inside = c(0.2, 0.7),
        plot.subtitle = element_textbox_simple(fill = "white", 
                                               colour = "grey7", 
                                               padding = unit(1, "mm"),
                                               r = unit(x = 2, "mm")))

ggsave(filename = paste0("plot_election_turnout_trend", ".png"), 
       plot = plot_election_turnout_trend, device = "png", 
       width = 16, height = 20, units = "cm", dpi = 300, limitsize = FALSE)
  

# Mosaic plot showing eligible voters, turnout, top party shares --------------
data_mosaic_election_turnouts <- data_wb_ac_elections %>% 
  # Total votes polled
  group_by(assembly_no, year, party) %>% 
  summarise(votes = sum(votes), .groups = "drop") %>% 
  # Keep top three parties and group the rest
  group_by(assembly_no) %>% 
  mutate(votes_polled = sum(votes),
         vote_share = votes/votes_polled,
         # Do not consider Independents and NOTA when finding top parties
         party_rank = row_number(desc(
           case_when(party %notin% c("IND", "NOTA") ~ votes, 
                     TRUE ~ NA_integer_))),
         # Recode parties not in top 3
         party = case_when(party_rank > 2 | is.na(party_rank) ~ "OTH", 
                           TRUE ~ party)) %>% 
  # Group all but top two into the others category
  group_by(assembly_no, year, party) %>% 
  summarise(votes = sum(votes), votes_polled = max(votes_polled), 
            vote_share = sum(vote_share), 
            party_rank = min(party_rank, na.rm = TRUE),
            .groups = "drop") %>% 
  ungroup() %>% 
  arrange(assembly_no, year, party_rank) %>% 
  # Create rectangles for each row
  # First, find relative width in the x direction
  (function(df) left_join(x = df, 
                          y = df %>% distinct(assembly_no, votes_polled) %>% 
                            arrange(assembly_no) %>% 
                            mutate(xmax = cumsum(votes_polled)/sum(votes_polled),
                                   xmin = lag(xmax, default = 0)) %>% 
                            select(assembly_no, xmin, xmax),
                          by = "assembly_no")) %>% 
  # Calculate relative height in y direction
  group_by(assembly_no) %>% 
  mutate(ymax = cumsum(vote_share),
         # limit to 1
         ymax = case_when(ymax > 1 ~ 1, TRUE ~ ymax),
         ymin = lag(ymax, default = 0)) %>% 
  ungroup()

# Plot
data_mosaic_election_turnouts %>% 
  ggplot() +
  # Mosaic
  geom_rect(mapping = aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax,
                          fill = party), colour = "grey7", linewidth = 2) +
  
  # Scales
  scale_x_continuous(name = NULL, 
                     breaks = data_mosaic_election_turnouts %>% 
                       distinct(assembly_no, xmax, xmin) %>% 
                       arrange(assembly_no) %>% 
                       mutate(xmid = (xmax + xmin)/2) %>% 
                       select(xmid) %>% 
                       unlist(use.names = FALSE), 
                     labels = data_mosaic_election_turnouts %>% 
                       distinct(assembly_no, year) %>% 
                       arrange(assembly_no) %>% 
                       select(year) %>% 
                       unlist(use.names = FALSE), 
                     guide = guide_axis(n.dodge = 1),
                     expand = expansion(mult = c(0.01, 0.01))) +
  scale_y_continuous(name = "Share of votes", 
                     labels = scales::label_percent(accuracy = 1),
                     expand = expansion(mult = c(0.01, 0.01))) +
  scale_fill_manual(breaks = c("AITC", "BJP", "CPM", "CPI", "INC", "OTH"), 
                    name = NULL, guide = guide_legend(nrow = 1),
                    values = c("limegreen", "orange", "firebrick", 
                               "firebrick1", "dodgerblue", "grey50")) +
  theme_vaw_dark_mobile() +
  theme(panel.grid.major.x = element_blank(),
        axis.ticks.x = element_blank(), axis.text.x = element_text(angle = 45),
        plot.subtitle = element_textbox_simple(fill = "white", 
                                               colour = "grey7", 
                                               padding = unit(1, "mm"),
                                               r = unit(x = 2, "mm")))
  
  