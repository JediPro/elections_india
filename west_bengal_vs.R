# Analyze voting patterns in West Bengal Assemble Elections

# Load libraries -------------------------------------
library(tidyverse)
library(sf)
library(ggfittext)
# library(ggalluvial)

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
                         sex == "MALE" ~ "M", TRUE ~ sex),
         ac_name = str_replace(string = ac_name, pattern = "NORTH$", replace = "UTTAR"),
         ac_name = str_replace(string = ac_name, pattern = "SOUTH$", replace = "DAKSHIN"),
         ac_name = str_replace(string = ac_name, pattern = "EAST$", replace = "PURBA"),
         ac_name = str_replace(string = ac_name, pattern = "WEST$", replace = "PASCHIM"),
         ac_name = str_replace(string = ac_name, pattern = "CENTRAL$", replace = "MADHYA"),
         # Recode constituency names
         ac_name = replace_values(x = ac_name,
                                  "MAKLIGANJ" ~ "MEKLIGANJ",
                                  c("ALIPORE DUARS", "ALIPUR DUARS", 
                                    "ALIPURDUARAS", "ALIPURDVARS") ~ "ALIPURDUARS",
                                  "ARAMBAG" ~ "ARAMBAGH",
                                  "ARSHA" ~ "ARSA",
                                  "ASHOKNAGAR" ~ "ASHOKENAGAR",
                                  "BAGDAHA" ~ "BAGDAH",
                                  c("BANGNAN", "BEGNAN") ~ "BAGNAN",
                                  "BERHAMPORE" ~ "BAHARAMPUR",
                                  "BALRAMPUR" ~ "BALARAMPUR",
                                  "BALTY" ~ "BALLY",
                                  "BANDUAN" ~ "BANDWAN",
                                  "BARWAN" ~ "BURWAN",
                                  "BURDWAN DAKSHIN" ~ "BARDHAMAN DAKSHIN",
                                  "BURDWAN UTTAR" ~ "BARDHAMAN UTTAR",
                                  "BELIAGHATA" ~ "BELEGHATA",
                                  "BELIAGHATYA UTTAR" ~ "BELIAGHATA UTTAR",
                                  "BHOWANIPUR" ~ "BHABANIPUR",
                                  c("BHAGARANGOLA", "BHAGAWANGOLA") ~ "BHAGABANGOLA",
                                  "BHAGBANPUR" ~ "BHAGABANGOLA",
                                  c("BHANAGAR", "BHANGAR") ~ "BHANGORE",
                                  "BHATRARA" ~ "BHATPARA",
                                  "BISHNUPUR(SC)" ~ "BISHNUPUR",
                                  "BOWBAZAR" ~ "BOW BAZAR",
                                  "CHAKDAH" ~ "CHAKDAHA",
                                  "CHANDERNAGORE" ~ "CHANDANNAGORE",
                                  "CHANDITAIA" ~ "CHANDITALA",
                                  "CHINSURAH" ~ "CHUNCHURA",
                                  "CHOWRINGHEE" ~ "CHOWRANGEE",
                                  "CONTAI DAKSHIN" ~ "KANTHI DAKSHIN",
                                  "CONTAI UTTAR" ~ "KANTHI UTTAR",
                                  "DHANIAKHALI" ~ "DHANEKHALI",
                                  "DURGAPUR-I" ~ "DURGAPUR PASCHIM",
                                  "DURGAPUR-II" ~ "DURGAPUR PURBA",
                                  "ERGA" ~ "EGRA",
                                  "FORT" ~ "FARIDPUR",
                                  "GAJOL" ~ "GAZOLE",
                                  "GANGJALGHATI" ~ "GANGAJALGHATI",
                                  "GOAL POKHAR" ~ "GOALPOKHAR",
                                  "HABBA" ~ "HABRA",
                                  "HABIBPUR S N" ~ "HABIBPUR",
                                  c("HARISHCHANDRAPUR", "HARISH CHANDRAPUR") ~ "HARISCHANDRAPUR",
                                  "HOWRAM UTTAR" ~ "HOWRAH UTTAR",
                                  "INDAS" ~ "INDUS",
                                  "JADHAVPUR" ~ "JADAVPUR",
                                  "JAIPUR" ~ "JOYPUR",
                                  "JAMAL PUR" ~ "JAMALPUR",
                                  "JAQYNAGAR" ~ "JOYNAGAR",
                                  "JORA SANKO" ~ "JORASANKO",
                                  "JORE BUNGALOW" ~ "JOREBUNGALOW",
                                  "KABITHIRTHA" ~ "KABITIRTHA",
                                  "KAILMPONG" ~ "KALIMPONG",
                                  "KASHIPURA" ~ "KASHIPUR",
                                  "KESHIARI" ~ "KESHIARY",
                                  "KHAJURI" ~ "KHEJURI",
                                  "KOTULPUR" ~ "KATULPUR",
                                  c("KHARAGPUR RURAL", "KHARAGPUR LOCAL") ~ "KHARAGPUR",
                                  "KHARAGPUR TOWN" ~ "KHARAGPUR SADAR",
                                  "KHARDAH" ~ "KHARDAHA",
                                  "KRISHNAGAR PASCHIM" ~ "KRISHNANAGAR DAKSHIN",
                                  "KRISHNAGAR PURBA" ~ "KRISHNANAGAR UTTAR",
                                  "LABPUR" ~ "LABHPUR",
                                  c("MAHESHTOIA", "MAHESHTOLA") ~ "MAHESHTALA",
                                  "MAHISHADAL" ~ "MAHISADAL",
                                  "MAINAGURI" ~ "MAYNAGURI",
                                  "MALDA" ~ "MALDAHA",
                                  "MANIKCHAK" ~ "MANICKCHAK",
                                  "MANICKTOLA" ~ "MANIKTALA",
                                  "MANTESWAR" ~ "MONTESWAR",
                                  "MAYURESHWAR" ~ "MAYURESWAR",
                                  "MIDNAPORE" ~ "MEDINIPUR",
                                  "MAYNA" ~ "MOYNA",
                                  "NANUR" ~ "NANOOR",
                                  "NAYAGARM" ~ "NAYAGRAM",
                                  "PATASPUR" ~ "PATASHPUR",
                                  "PATHARPRATHIMA" ~ "PATHARPRATIMA",
                                  "RAGHUNATHPURA" ~ "RAGHUNATHPUR",
                                  "RANGAGHAT PURBA" ~ "RANAGHAT PURBA",
                                  "RANIBANDH" ~ "RANIBUNDH",
                                  "RASHBEHARI AVENUE" ~ "RASHBEHARI",
                                  "RATDA" ~ "RATUA",
                                  "RAYGANJ" ~ "RAJGANJ",
                                  "RAJNAGAR" ~ "REJINAGAR",
                                  "SALBANI" ~ "SALBONI",
                                  c("SERANPORE", "SERAMPORE") ~ "SREERAMPUR",
                                  "SHAMPUKUR" ~ "SHYAMPUKUR",
                                  "SITAL KUCHI" ~ "SITALKUCHI",
                                  "SUZAPUR" ~ "SUJAPUR",
                                  "SYAMPUR" ~ "SHYAMPUR",
                                  "TALANGRA" ~ "TALDANGRA",
                                  c("TOLLY GUNGE", "TOLLYGUNGE") ~ "TOLLYGUNJ",
                                  c("ULIBERIA DAKSHIN", "ULUBERIA SUOTH") ~ "ULUBERIA DAKSHIN",
                                  "UTTARAPARA" ~ "UTTARPARA",
                                  "VISHNUPUR" ~ "BISHNUPUR",
                                  "BANSBERIA" ~ "SAPTAGRAM",
                                  "JHALDA" ~ "BAGHMUNDI",
                                  "MAHAMMAD BAZAR"~ "SAINTHIA",
                                  "TITAGARH" ~ "BARRACKPUR")
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
       subtitle = "<b style='color:lightblue4'>Light blue bars</b> indicate number of eligible voters, <b style='color:#06038D'>Dark blue bars</b> indicate actual votes polled. The numbers indicate turnout percentage.",
       caption = "Data: yashveeeeeeer.github.io/india-geodata, lokdhaba.ashoka.edu.in, data.opencity.in, News18 | Design: @JediPro") +
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
plot_mosaic_election_turnouts <- data_mosaic_election_turnouts %>% 
  ggplot() +
  # Mosaic
  geom_rect(mapping = aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax,
                          fill = party), colour = "grey7", linewidth = 1) +
  # Labels
  geom_fit_text(mapping = aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax,
                              label = scales::percent(x = vote_share, accuracy = 1)),
                family = "Titillium Web", fontface = "bold", size = 10, 
                min.size = 5, grow = FALSE, outside = FALSE,
                colour = "white", place = "centre") +
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
  scale_y_reverse(name = "Share of votes", 
                     labels = scales::label_percent(accuracy = 1),
                     expand = expansion(mult = c(0.01, 0.01))) +
  scale_fill_manual(breaks = c("AITC", "BJP", "CPM", "CPI", "INC", "OTH"), 
                    labels = c("TMC", "BJP", "CPM", "CPI", "INC", "OTH"), 
                    name = NULL, guide = guide_legend(nrow = 1),
                    values = c("#046A38", "#FF671F", "#de0000", 
                               "firebrick", "#06038D", "grey50")) +
  # Labels
  labs(title = "TMC has kept on increasing its vorte share for the past 4 elections. BJP has never been in the top two till the 2021 edition",
       subtitle = "The width of the bars are proportional to the total votes polled in that election. The height of each segment corresponds to the party's vote share",
       caption = "Data: yashveeeeeeer.github.io/india-geodata, lokdhaba.ashoka.edu.in, data.opencity.in, News18 | Design: @JediPro") +
  theme_vaw_dark_mobile() +
  theme(panel.grid.major.x = element_blank(),
        axis.ticks.x = element_blank(), axis.text.x = element_text(angle = 45),
        plot.subtitle = element_textbox_simple(fill = "white", 
                                               colour = "grey7", 
                                               padding = unit(1, "mm"),
                                               r = unit(x = 2, "mm")))

ggsave(filename = paste0("plot_mosaic_election_turnouts", ".png"), 
       plot = plot_mosaic_election_turnouts, device = "png", 
       width = 16, height = 20, units = "cm", dpi = 300, limitsize = FALSE)

# ENOP trends -----------------------------------
plot_enop_year_trend <- data_wb_ac_elections %>% 
  # Remove NOTA
  filter(party != "NOTA") %>% 
  # Consider each independent as a separate party
  mutate(party = case_when(party == "IND" ~ paste(party, candidate_id, sep = "_"),
                           TRUE ~ party)) %>% 
  # Total votes polled
  group_by(assembly_no, year, party) %>% 
  summarise(votes = sum(votes), n_candidate = n_distinct(candidate),
            .groups = "drop") %>% 
  # Keep top three parties and group the rest
  group_by(assembly_no, year) %>% 
  mutate(votes_polled = sum(votes),
         vote_share = votes/votes_polled) %>% 
  # Calculate ENOP per year
  summarise(enop = 1/sum(vote_share^2), n_candidate = sum(n_candidate),
            n_party = sum(str_detect(string = party, pattern = "IND_.*", negate = TRUE)),
            .groups = "drop") %>% 
  # plot
  ggplot() +
  # trace line
  geom_smooth(mapping = aes(x = assembly_no, y = enop), colour = "lightblue",
              formula = y ~ x, se = FALSE, method = "loess", linewidth = 2) +
  # Plot point as number of parties
  geom_point(mapping = aes(x = assembly_no, y = enop, size = n_party),
             colour = "gold") +
  geom_text(mapping = aes(x = assembly_no, y = enop, label = n_party),
             colour = "grey7", family = "Titillium Web", size = 3, fontface = "bold") +
  # Scales
  scale_x_continuous(name = NULL, breaks = as.numeric(names(vec_assembly_year)), 
                     labels = vec_assembly_year, 
                     expand = expansion(mult = c(0.05, 0.05))) +
  scale_y_continuous(name = "Effective Number of Parties (ENOP)", 
                     labels = scales::label_comma(scale = 1, accuracy = 1),
                     expand = expansion(mult = c(0.07, 0.07))) +
  scale_size_continuous(name = NULL, guide = NULL, range = c(5, 15)) +
  # Labels
  labs(title = "Elections have become increasingly bipolar. 2021 saw the largest number of parties competing, but ENOP was the lowest",
       subtitle = "<b style='color:gold3'>The gold circles</b> represent number of parties while <b style='color:lightblue3'>the light blue line</b> indicates smoothed trend of ENOP, which is a statistical measure showing fragmentation of the vote, with higher values indicating more fragmentation.",
       caption = "Data: yashveeeeeeer.github.io/india-geodata, lokdhaba.ashoka.edu.in, data.opencity.in, News18 | Design: @JediPro") +
  theme_vaw_dark_mobile() +
  theme(panel.grid.major.x = element_blank(),
        axis.ticks.x = element_blank(),
        plot.subtitle = element_textbox_simple(fill = "white", 
                                               colour = "grey7", 
                                               padding = unit(1, "mm"),
                                               r = unit(x = 2, "mm")))

ggsave(filename = paste0("plot_enop_year_trend", ".png"), 
       plot = plot_enop_year_trend, device = "png", 
       width = 16, height = 20, units = "cm", dpi = 300, limitsize = FALSE)

# Relation between turnout and change ----------------------------------
data_turnout_incumbency <- data_wb_ac_elections %>% 
  # Remove NOTA
  filter(party != "NOTA") %>% 
  # calculate winning margin
  group_by(assembly_no, ac_no, ac_name) %>% 
  arrange(assembly_no, ac_no, ac_name, position) %>% 
  mutate(vote_share = votes/valid_votes,
         margin = vote_share - lead(vote_share),
         party = case_when(party == "IND" ~ paste(party, candidate_id, sep = "_"), TRUE ~ party)) %>% 
  ungroup() %>% 
  # keep only winners
  filter(position == 1) %>% 
  group_by(ac_name) %>% 
  arrange(ac_name, assembly_no) %>% 
  mutate(prev_party = lag(party),
         prev_margin = lag(margin),
         cum_mean_turnout = cumsum(valid_votes)/cumsum(electors),
         turnout = valid_votes/electors,
         prev_turnout = lag(turnout),
         incumbency = ifelse(test = party == prev_party, 1, 0),
         # Number of incumbent wins
         n_win_incumbent = accumulate(.x = incumbency, 
                                      .f = function(prev, curr) {
                                        if (curr == 0 | is.na(prev)) curr 
                                        else prev + curr
                                      })) %>% 
  ungroup() %>% 
  select(ac_name, assembly_no, year, electors, valid_votes, turnout,
         party, vote_share, margin, prev_party, prev_margin, incumbency,
         n_win_incumbent, prev_turnout, cum_mean_turnout) %>% 
  drop_na(incumbency) %>% 
  # Recode incumbent wins
  mutate(incumbency_degree = case_when(n_win_incumbent == 0 ~ "A.NON_INCUMBENT",
                                       n_win_incumbent == 1 ~ "B.INCUMBENT_1",
                                       n_win_incumbent == 2 ~ "C.INCUMBENT_2",
                                       n_win_incumbent %in% c(3,4) ~ "D.INCUMBER_3_4",
                                       TRUE ~ "E.INCUMBENT_GT5"))

data_turnout_incumbency %>% count(incumbency, n_win_incumbent)


cor(data_turnout_incumbency$turnout, data_turnout_incumbency$margin)
data_turnout_incumbency %>% 
  mutate(incumbency = if_else(condition = incumbency == 1, TRUE, FALSE),
         turnout_delta = turnout - prev_turnout) %>% 
  ggplot() +
  geom_density(mapping = aes(x = turnout_delta, group = incumbency_degree, colour = incumbency_degree))
  facet_wrap(facets = vars(n_win_incumbent))