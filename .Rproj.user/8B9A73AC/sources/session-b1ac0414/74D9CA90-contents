################################################################################
# Author: J. Jameson

# Purpose: Prepare PMEDS data for analysis
################################################################################
rm(list = ls())

# Load libraries ---------------------------------------------------------------
library(tidyverse)
library(MEPS)
library(stringr)

################################################################################
# Prepare PMEDS data -----------------------------------------------------------

# Load PMEDS data --------------------------------------------------------------
pmeds2014 = read_MEPS(year = 2014, type = "RX")
pmeds2015 = read_MEPS(year = 2015, type = "RX")
pmeds2016 = read_MEPS(year = 2016, type = "RX")
pmeds2017 = read_MEPS(year = 2017, type = "RX")
pmeds2018 = read_MEPS(year = 2018, type = "RX")
pmeds2019 = read_MEPS(year = 2019, type = "RX")
pmeds2020 = read_MEPS(year = 2020, type = "RX")
pmeds2021 = read_MEPS(year = 2021, type = "RX")

# Clean data -------------------------------------------------------------------
pmeds2014 = pmeds2014 %>% mutate(year = 2014, id = paste(DUPERSID, year, sep = "_"))
pmeds2015 = pmeds2015 %>% mutate(year = 2015, id = paste(DUPERSID, year, sep = "_"))
pmeds2016 = pmeds2016 %>% mutate(year = 2016, id = paste(DUPERSID, year, sep = "_"))
pmeds2017 = pmeds2017 %>% mutate(year = 2017, id = paste(DUPERSID, year, sep = "_"))
pmeds2018 = pmeds2018 %>% mutate(year = 2018, id = paste(DUPERSID, year, sep = "_"))
pmeds2019 = pmeds2019 %>% mutate(year = 2019, id = paste(DUPERSID, year, sep = "_"))
pmeds2020 = pmeds2020 %>% mutate(year = 2020, id = paste(DUPERSID, year, sep = "_"))
pmeds2021 = pmeds2021 %>% mutate(year = 2021, id = paste(DUPERSID, year, sep = "_"))

for (i in 14:21) {
  assign(paste0("pmeds20", i), get(paste0("pmeds20", i)) %>% 
           rename_all(~str_replace_all(., as.character(i), "")))
}

pmeds_list <- list(pmeds2014, pmeds2015, pmeds2016, pmeds2017, 
                   pmeds2018, pmeds2019, pmeds2020, pmeds2021)
common_vars <- Reduce(intersect, lapply(pmeds_list, names))

pmeds <- pmeds_list %>%
  lapply(function(df) select(df, all_of(common_vars))) %>%
  bind_rows()

rm(list = ls()[!ls() %in% c("pmeds")])

pmeds <- pmeds %>% 
  select(id, year, RXNAME, RXDRGNAM) %>%
  mutate(across(everything(), tolower)) 
  

################################################################################

pgx <- read.csv("PGx Medications/pgx_meds.csv") %>%
  pull(Drug) %>% 
  unique()

# split RXDRGNAM into individual words ----------------------------------------
pmeds <- pmeds %>% 
  mutate(RX = RXDRGNAM,
         RXDRGNAM = str_replace_all(RXDRGNAM, "[[:punct:]]", " "),
         pgx_conservative = ifelse(RXDRGNAM %in% pgx, 1, 0)) %>% 
  separate(RXDRGNAM, into = paste0("RX", 1:5), sep = " ") %>% 
  mutate(across(starts_with("word"), tolower)) 

pmeds <- pmeds %>% 
  mutate(pgx = case_when(
    RX1 %in% pgx | RX2 %in% pgx | RX3 %in% pgx | RX4 %in% pgx | RX5 %in% pgx ~ 1,
    TRUE ~ 0
  ))

# count unique drugs per person ------------------------------------------------
pmeds <- pmeds %>% 
  unique() %>%
  group_by(id) %>% 
  summarise(n_drugs = n_distinct(RX),
            n_pgx = sum(pgx, na.rm = TRUE), 
            pgx_conservative = sum(pgx_conservative, na.rm = TRUE), 
            .groups = "drop")


save(pmeds, file = "outputs/data/pmeds.Rdata")
################################################################################