################################################################################
# Author: J. Jameson

# Purpose: Prepare HC data for analysis
################################################################################
rm(list = ls())

# Load libraries ---------------------------------------------------------------
library(tidyverse)
library(MEPS)
library(labelled)

################################################################################
# Prepare HC data --------------------------------------------------------------

# Load HC data -----------------------------------------------------------------

hc2014 = read_MEPS(year = 2014, type = "FYC")
hc2015 = read_MEPS(year = 2015, type = "FYC")
hc2016 = read_MEPS(year = 2016, type = "FYC")
hc2017 = read_MEPS(year = 2017, type = "FYC")
hc2018 = read_MEPS(year = 2018, type = "FYC")
hc2019 = read_MEPS(year = 2019, type = "FYC")
hc2020 = read_MEPS(year = 2020, type = "FYC")
hc2021 = read_MEPS(year = 2021, type = "FYC")

# Clean data -------------------------------------------------------------------
hc2014 = hc2014 %>% mutate(year = 2014, id = paste(DUPERSID, year, sep = "_"))

hc2014$educ_cat <- with(hc2014, case_when(
  EDUYRDG %in% c(1, 2) ~ "No Degree",
  EDUYRDG %in% c(3, 4, 5) ~ "HS Degree/GED",
  EDUYRDG %in% c(6, 7, 8) ~ "College Degree",
  EDUYRDG == 9 ~ "Graduate Degree",
  TRUE ~ "Other"
))

hc2015 = hc2015 %>% mutate(year = 2015, id = paste(DUPERSID, year, sep = "_"))

hc2015$educ_cat <- with(hc2015, case_when(
  EDUYRDG %in% c(1, 2) ~ "No Degree",
  EDUYRDG %in% c(3, 4, 5) ~ "HS Degree/GED",
  EDUYRDG %in% c(6, 7, 8) ~ "College Degree",
  EDUYRDG == 9 ~ "Graduate Degree",
  TRUE ~ "Other"
))

hc2016 = hc2016 %>% mutate(year = 2016, id = paste(DUPERSID, year, sep = "_"))

hc2016$educ_cat <- with(hc2016, case_when(
  HIDEG == 1 ~ "No Degree",
  HIDEG %in% c(2, 3) ~ "HS Degree/GED",
  HIDEG %in% c(4, 5, 7) ~ "College Degree",
  HIDEG == 6 ~ "Graduate Degree",
  TRUE ~ "Other"
))

hc2017 = hc2017 %>% mutate(year = 2017, id = paste(DUPERSID, year, sep = "_"))

hc2017$educ_cat <- with(hc2017, case_when(
  HIDEG == 1 ~ "No Degree",
  HIDEG %in% c(2, 3) ~ "HS Degree/GED",
  HIDEG %in% c(4, 5, 7) ~ "College Degree",
  HIDEG == 6 ~ "Graduate Degree",
  TRUE ~ "Other"
))

hc2018 = hc2018 %>% mutate(year = 2018, id = paste(DUPERSID, year, sep = "_"))

hc2018$educ_cat <- with(hc2018, case_when(
  HIDEG == 1 ~ "No Degree",
  HIDEG %in% c(2, 3) ~ "HS Degree/GED",
  HIDEG %in% c(4, 5, 7) ~ "College Degree",
  HIDEG == 6 ~ "Graduate Degree",
  TRUE ~ "Other"
))


hc2019 = hc2019 %>% mutate(year = 2019, id = paste(DUPERSID, year, sep = "_"))

hc2019$educ_cat <- with(hc2019, case_when(
  HIDEG == 1 ~ "No Degree",
  HIDEG %in% c(2, 3) ~ "HS Degree/GED",
  HIDEG %in% c(4, 5, 7) ~ "College Degree",
  HIDEG == 6 ~ "Graduate Degree",
  TRUE ~ "Other"
))

hc2020 = hc2020 %>% mutate(year = 2020, id = paste(DUPERSID, year, sep = "_"))

hc2020$educ_cat <- with(hc2020, case_when(
  HIDEG == 1 ~ "No Degree",
  HIDEG %in% c(2, 3) ~ "HS Degree/GED",
  HIDEG %in% c(4, 5, 7) ~ "College Degree",
  HIDEG == 6 ~ "Graduate Degree",
  TRUE ~ "Other"
))

hc2021 = hc2021 %>% mutate(year = 2021, id = paste(DUPERSID, year, sep = "_"))

hc2021$educ_cat <- with(hc2021, case_when(
  HIDEG == 1 ~ "No Degree",
  HIDEG %in% c(2, 3) ~ "HS Degree/GED",
  HIDEG %in% c(4, 5, 7) ~ "College Degree",
  HIDEG == 6 ~ "Graduate Degree",
  TRUE ~ "Other"
))

for (i in 14:21) {
  assign(paste0("hc20", i), get(paste0("hc20", i)) %>% 
           rename_all(~str_replace_all(., as.character(i), "")))
}

hc_list <- list(hc2014, hc2015, hc2016, hc2017, hc2018, hc2019, hc2020, hc2021)
common_vars <- Reduce(intersect, lapply(hc_list, names))

hc <- hc_list %>%
  lapply(function(df) select(df, all_of(common_vars))) %>%
  bind_rows()

rm(list = ls()[!ls() %in% c("hc")])

################################################################################

# Build variables --------------------------------------------------------------
hc <- hc %>%
  mutate(
    poolwt = PERWTF / 8,
    
    # Create adult variable
    adult = ifelse(AGELAST > 17, 1, 0),

    # Create female variable
    female = ifelse(SEX == 2, 1, 0),

    # Create age category variable
    age_cat = case_when(
      AGELAST < 65 ~ "< 65",
      AGELAST >= 65 & AGELAST <= 74 ~ "65 - 74",
      AGELAST >= 75 & AGELAST <= 84 ~ "75 - 84",
      AGELAST >= 85 ~ "85+"),

    
    # Create race category variable
    race_cat = case_when(
      RACETHX == 2 ~ "White (Non-Hispanic)",
      RACETHX == 3 ~ "Black (Non-Hispanic)",
      RACETHX == 1 ~ "Hispanic",
      RACETHX == 4 ~ "Asian (Non-Hispanic)",
      TRUE ~ "Other (Non-Hispanic)"
    ),
    

    # creat region category variable
    region_cat = case_when(
      REGION == 1 ~ "Northeast",
      REGION == 2 ~ "Midwest",
      REGION == 3 ~ "South",
      REGION == 4 ~ "West",
      TRUE ~ "Other"
    ),
    
    # Poverty level category variable
    POVCAT = case_when(
      POVCAT == 1 ~ "Poor (<100% FPL)",
      POVCAT == 2 ~ "Near Poor (100% to <125% FPL)",
      POVCAT == 3 ~ "Low Income (125% to <200% FPL)",
      POVCAT == 4 ~ "Middle Income (200% to <400% FPL)",
      POVCAT == 5 ~ "High Income (≥ 400% FPL)",
    ),
    
    # Create condition variables
    cancer_cond = ifelse(CANCERDX == 1, 1, 0),
    resp_cond = ifelse(EMPHDX == 1 | ASTHDX == 1, 1, 0),
    cardio_cond = ifelse(HIBPDX == 1 | CHDDX == 1 | ANGIDX == 1 | MIDX == 1 | 
                           OHRTDX == 1 | STRKDX == 1 | CHOLDX == 1, 1, 0),
    other_cond = ifelse(DSDIA53 == 1 | ARTHDX == 1, 1, 0),
    
    # Create condition counter
    condition_counter = rowSums(select(., CABLADDR, CABREAST, CACERVIX, CACOLON, CALUNG, CALYMPH,
                                       CAMELANO, CAOTHER, CAPROSTA, CASKINNM, CASKINDK, CAUTERUS,
                                       HIBPDX, CHDDX, ANGIDX, MIDX, OHRTDX, STRKDX, CHOLDX,
                                       EMPHDX, ASTHDX, DSDIA53, ARTHDX) == 1, na.rm = TRUE),
    
    # Create condition count category 
    cond_count_cat = case_when(
      condition_counter == 0 ~ "None",
      condition_counter == 1 ~ '1',
      condition_counter >= 2 & condition_counter <= 3 ~ "2-3",
      condition_counter >= 4 & condition_counter <= 5 ~ "4-5",
      condition_counter >= 6 ~ "6+"
    ),
    
    # Create INSCOVX variable
    INSCOVX = case_when(
      INSCOV == 3 ~ 'Uninsured',
      INSCOV == 2 ~ 'Public Only',
      INSCOV == 1 ~ 'Any Private'
    ),

    # Create individual condition variables
    cabladdr = ifelse(CABLADDR == 1, 1, 0),
    cabreast = ifelse(CABREAST == 1, 1, 0),
    cacervix = ifelse(CACERVIX == 1, 1, 0),
    cacolon = ifelse(CACOLON == 1, 1, 0),
    calung = ifelse(CALUNG == 1, 1, 0),
    calymph = ifelse(CALYMPH == 1, 1, 0),
    camelano = ifelse(CAMELANO == 1, 1, 0),
    caother = ifelse(CAOTHER == 1, 1, 0),
    caprosta = ifelse(CAPROSTA == 1, 1, 0),
    caskinnm = ifelse(CASKINNM == 1, 1, 0),
    caskindk = ifelse(CASKINDK == 1, 1, 0),
    cauterus = ifelse(CAUTERUS == 1, 1, 0),
    hibpdx = ifelse(HIBPDX == 1, 1, 0),
    chhdx = ifelse(CHDDX == 1, 1, 0),
    angidx = ifelse(ANGIDX == 1, 1, 0),
    midx = ifelse(MIDX == 1, 1, 0),
    ohrtdx = ifelse(OHRTDX == 1, 1, 0),
    strkdx = ifelse(STRKDX == 1, 1, 0),
    choldx = ifelse(CHOLDX == 1, 1, 0),
    emphdx = ifelse(EMPHDX == 1, 1, 0),
    asthdx = ifelse(ASTHDX == 1, 1, 0),
    diab = ifelse(DSDIA53 == 1, 1, 0),
    arthdx = ifelse(ARTHDX == 1, 1, 0)
  )


# Add value labels -------------------------------------------------------------
hc$age_cat <- factor(hc$age_cat, levels = c("< 65", "65 - 74", "75 - 84", "85+"))

hc$race_cat <- factor(hc$race_cat, levels = c("White (Non-Hispanic)", 
                                       "Black (Non-Hispanic)", 
                                       "Hispanic", 
                                       "Asian (Non-Hispanic)",
                                       'Other (Non-Hispanic)'))

hc$cond_count_cat <- factor(hc$cond_count_cat, levels = c("None","1","2-3", "4-5", "6+"))

hc$INSCOVX <- factor(hc$INSCOVX, levels = c("Uninsured", 
                                     "Public Only", 
                                     "Any Private"))

hc$POVCAT <- factor(hc$POVCAT, levels = c("Poor (<100% FPL)", 
                                   "Near Poor (100% to <125% FPL)", 
                                   "Low Income (125% to <200% FPL)", 
                                   "Middle Income (200% to <400% FPL)", 
                                   "High Income (≥ 400% FPL)"))

hc$region_cat <- factor(hc$region_cat, levels = c("Northeast", "Midwest", "South", "West", 'Other'))

hc$educ_cat <- factor(hc$educ_cat, 
                      levels = c("No Degree", "HS Degree/GED", "College Degree", 
                                 "Graduate Degree", 'Other'))

# Define the variables to keep -------------------------------------------------
vars_to_keep <- c(
  "id", "year", "DUPERSID", "AGELAST", "SEX", "RACETHX", "CANCERDX",
  "EMPHDX", "ASTHDX", "HIBPDX", "CHDDX", "ANGIDX", "MIDX", "OHRTDX",
  "STRKDX", "CHOLDX", "DSDIA53", "ARTHDX", "CABLADDR", "CABREAST",
  "CACERVIX", "CACOLON", "CALUNG", "CALYMPH", "CAMELANO", "CAOTHER",
  "CAPROSTA", "CASKINNM", "CASKINDK", "CAUTERUS", "INSCOV", "PRVEV",
  "PUBATX", "MCDEV", "MCREV", "MCARE", "MCARE31", "MCARE42", "MCARE53",
  "MCAREX", "MCARE31X", "MCARE42X", "MCARE53X", "TRIEV", "MCRPHO",
  "MCRPHO31", "MCRPHO42", "region_cat", "POVCAT", 'educ_cat',
  "adult", "female", "age_cat", "race_cat", "cancer_cond", "resp_cond",
  "cardio_cond", "other_cond", "condition_counter", "cond_count_cat", "INSCOVX",
  "cabladdr", "cabreast", "cacervix", "cacolon", "calung", "calymph",
  "camelano", "caother", "caprosta", "caskinnm", "caskindk", "cauterus",
  "hibpdx", "chhdx", "angidx", "midx", "ohrtdx", "strkdx", "choldx",
  "emphdx", "asthdx", "diab", "arthdx", 'poolwt', 'VARPSU', 'VARSTR'
)

# Select only the variables to keep --------------------------------------------
hc <- hc %>% select(all_of(vars_to_keep))

# Save the data ---------------------------------------------------------------
save(hc, file = file.path("outputs/data/hc.Rdata"))
