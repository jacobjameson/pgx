################################################################################
# Author: J. Jameson

# Purpose: Generate tables for the manuscript
################################################################################
rm(list = ls())

# Load libraries ---------------------------------------------------------------
library(tidyverse)
library(survey)

# Load data -------------------------------------------------------------------
load("outputs/data/hc.Rdata")
load("outputs/data/pmeds.Rdata")

# Join data --------------------------------------------------------------------
data <- left_join(hc, pmeds, by = "id") %>%
  mutate(n_drugs = ifelse(is.na(n_drugs), 0, n_drugs),
         n_presc_cat = case_when(
            n_drugs == 0 ~ 'None',
            n_drugs == 1 ~ '1',
            n_drugs >= 2 & n_drugs <= 3 ~ "2-3",
            n_drugs >= 4 & n_drugs <= 5 ~ "4-5",
            n_drugs >= 6 & n_drugs <= 7 ~ "6-7",
            n_drugs >= 8 & n_drugs <= 9 ~ "8-9",
            n_drugs >= 10 ~ '10+'
  ),
  subpop = ifelse(adult == 1, 1, 0))

data$n_presc_cat <- factor(data$n_presc_cat, 
                           levels = c('None',"1", "2-3", "4-5", "6-7", "8-9", "10+"))

################################################################################
# Table 1 ----------------------------------------------------------------------

svy_data <- svydesign(
  id = ~VARPSU, 
  strata = ~VARSTR,
  weights = ~poolwt, 
  data = data, 
  nest = TRUE
)


# Function to extract and format survey means
get_survey_means <- function(variable, design, subset_condition = NULL) {
  if (!is.null(subset_condition)) {
    design <- subset(design, eval(parse(text = subset_condition)))
  }
  
  results <- svymean(as.formula(paste0("~", variable)), design = design, na.rm = TRUE)
  
  df <- data.frame(
    Category = names(results),
    Percentage = as.vector(results) * 100
  ) %>%
    mutate(
      Percentage = sprintf("%.1f", Percentage)
    )
  
  return(df)
}

# Function to get median and range
get_median_range <- function(variable, design, subset_condition = NULL) {
  if (!is.null(subset_condition)) {
    design <- subset(design, eval(parse(text = subset_condition)))
  }
  
  quantiles <- svyquantile(as.formula(paste0("~", variable)), 
                           design, quantiles = c(0, 0.5, 1), na.rm = TRUE)
  
  df <- data.frame(
    Category = "Median (Range)",
    Percentage = sprintf("%.0f (%.0f, %.0f)", 
                         as.numeric(quantiles[[1]][2]), # median
                         as.numeric(quantiles[[1]][1]), # min
                         as.numeric(quantiles[[1]][3])) # max
  )
  
  return(df)
}

# List of variables to analyze
variables <- c("race_cat", "AGELAST", "age_cat", 'female', 'educ_cat', 
               'POVCAT', 'region_cat', "INSCOVX", 'condition_counter', 
               'cond_count_cat', 'n_drugs', 'n_presc_cat')

# Create an empty list to store results
results_list <- list()

# Loop through variables and get survey means or median/range
for (var in variables) {
  if (var %in% c("AGELAST", "condition_counter", "n_drugs")) {
    results_list[[var]] <- get_median_range(var, svy_data, "adult == 1")
  } else {
    results_list[[var]] <- get_survey_means(var, svy_data, "adult == 1")
  }
}

# Combine all results into a single data frame
final_table <- bind_rows(results_list, .id = "Variable")

# Reorder the variables
variable_order <- c("race_cat", "AGELAST", "age_cat", "female", "educ_cat", 
                    "POVCAT", "region_cat", "INSCOVX", "condition_counter", "cond_count_cat",
                    "n_drugs", "n_presc_cat")

final_table <- final_table %>%
  mutate(Variable = factor(Variable, levels = variable_order)) %>%
  arrange(Variable) %>%
  select(Variable, Category, Percentage)

# Print the final table
print(final_table)


library(survey)
library(dplyr)

generate_table <- function(data, subset_condition = NULL) {
  
  svy_data <- svydesign(
    id = ~VARPSU, 
    strata = ~VARSTR,
    weights = ~poolwt, 
    data = data, 
    nest = TRUE
  )
  
  if (!is.null(subset_condition)) {
    svy_data <- subset(svy_data, eval(parse(text = subset_condition)))
  }
  
  get_survey_means <- function(variable, design) {
    results <- svymean(as.formula(paste0("~", variable)), design = design, na.rm = TRUE)
    df <- data.frame(
      Category = names(results),
      Percentage = as.vector(results) * 100
    ) %>%
      mutate(
        Percentage = sprintf("%.1f", Percentage)
      )
    return(df)
  }
  
  get_median_range <- function(variable, design) {
    quantiles <- svyquantile(as.formula(paste0("~", variable)), 
                             design, quantiles = c(0, 0.5, 1), na.rm = TRUE)
    df <- data.frame(
      Category = "Median (Range)",
      Percentage = sprintf("%.0f (%.0f, %.0f)", 
                           as.numeric(quantiles[[1]][2]), # median
                           as.numeric(quantiles[[1]][1]), # min
                           as.numeric(quantiles[[1]][3])) # max
    )
    return(df)
  }
  
  variables <- c("race_cat", "AGELAST", "age_cat", 'female', 'educ_cat', 
                 'POVCAT', 'region_cat', "INSCOVX", 'condition_counter', 
                 'cond_count_cat', 'n_drugs', 'n_presc_cat')
  
  results_list <- list()
  for (var in variables) {
    if (var %in% c("AGELAST", "condition_counter", "n_drugs")) {
      results_list[[var]] <- get_median_range(var, svy_data)
    } else {
      results_list[[var]] <- get_survey_means(var, svy_data)
    }
  }
  
  final_table <- bind_rows(results_list, .id = "Variable")
  
  variable_order <- c("race_cat", "AGELAST", "age_cat", "female", "educ_cat", 
                      "POVCAT", "region_cat", "INSCOVX", "condition_counter", "cond_count_cat",
                      "n_drugs", "n_presc_cat")
  
  final_table <- final_table %>%
    mutate(Variable = factor(Variable, levels = variable_order)) %>%
    arrange(Variable) %>%
    select(Variable, Category, Percentage)
  
  # Add the unweighted N
  unweighted_n <- nrow(svy_data$variables)
  final_table <- rbind(data.frame(Variable = '', 
                                  Category = '', 
                                  Percentage = paste("(unweighted) N=", as.character(unweighted_n))),
                       final_table)
  
  return(final_table)
}

# Generate tables for each subgroup
table_all <- generate_table(data, "adult == 1") %>%
  rename("Total Survey Respondents %" = Percentage)

table_with_drugs <- generate_table(data, "adult == 1 & n_drugs > 0") %>%
  rename("At least One Prescription %" = Percentage)

table_with_pgx <- generate_table(data, "adult == 1 & n_pgx > 0") %>%
  rename("At least One PGx Prescription %" = Percentage)

table_with_drugs_no_pgx <- generate_table(data, "adult == 1 & n_drugs > 0 & n_pgx == 0") %>%
  rename("At least One Prescription, No PGx %" = Percentage)


# Join the tables together on the Variable column
final_table <- full_join(table_all, table_with_drugs, c("Variable", 'Category')) %>%
  full_join(table_with_pgx, by = c("Variable", 'Category')) %>%
  full_join(table_with_drugs_no_pgx, by = c("Variable", 'Category'))


# Rename Variable column
variable_mapping <- c(
  "race_cat" = "Race/ethnicity",
  "AGELAST" = "Age",
  "age_cat" = "Age",
  "female" = "Female",
  "educ_cat" = "Highest Degree Earned",
  "POVCAT" = "Income Status",
  "region_cat" = "Region",
  "INSCOVX" = "Full Year Insurance",
  "condition_counter" = "Number of Chronic Health Conditions",
  "cond_count_cat" = "Number of Chronic Health Conditions",
  "n_drugs" = "Number of Unique Prescriptions",
  "n_presc_cat" = "Number of Unique Prescriptions"
)

final_table <- final_table %>%
  mutate(Variable = case_when(
    Variable %in% names(variable_mapping) ~ variable_mapping[Variable],
    TRUE ~ Variable
  ))

# Rename Category column
category_mapping <- c(
  "race_catWhite (Non-Hispanic)" = "White (Non-Hispanic)",
  "race_catBlack (Non-Hispanic)" = "Black (Non-Hispanic)",
  "race_catHispanic" = "Hispanic",
  "race_catAsian (Non-Hispanic)" = "Asian (Non-Hispanic)",
  "race_catOther (Non-Hispanic)" = "Other (Non-Hispanic)",
  "age_cat< 65" = "Under 65",
  "age_cat65 - 74" = "65 - 74",
  "age_cat75 - 84" = "75 - 84",
  "age_cat85+" = "85+",
  "female" = "",
  "educ_catNo Degree" = "No Degree",
  "educ_catHS Degree/GED" = "HS Degree",
  "educ_catCollege Degree" = "College Degree",
  "educ_catGraduate Degree" = "Graduate Degree",
  "educ_catOther" = "Other",
  "POVCATPoor (<100% FPL)" = "Poor (<100% FPL)",
  "POVCATNear Poor (100% to <125% FPL)" = "Near Poor (100% to <125% FPL)",
  "POVCATLow Income (125% to <200% FPL)" = "Low Income (125% to <200% FPL)",
  "POVCATMiddle Income (200% to <400% FPL)" = "Middle Income (200% to <400% FPL)",
  "POVCATHigh Income (≥ 400% FPL)" = "High Income (≥ 400% FPL)",
  "region_catNortheast" = "Northeast",
  "region_catMidwest" = "Midwest",
  "region_catSouth" = "South",
  "region_catWest" = "West",
  "region_catOther" = "Other",
  "INSCOVXUninsured" = "Uninsured",
  "INSCOVXPublic Only" = "Public",
  "INSCOVXAny Private" = "Private",
  "cond_count_catNone" = "None",
  "cond_count_cat1" = "1",
  "cond_count_cat2-3" = "2-3",
  "cond_count_cat4-5" = "4-5",
  "cond_count_cat6+" = "6+",
  "n_presc_catNone" = "None",
  "n_presc_cat1" = "1",
  "n_presc_cat2-3" = "2-3",
  "n_presc_cat4-5" = "4-5",
  "n_presc_cat6-7" = "6-7",
  "n_presc_cat8-9" = "8-9",
  "n_presc_cat10+" = "10+"
)

final_table <- final_table %>%
  mutate(Category = case_when(
    Category %in% names(category_mapping) ~ category_mapping[Category],
    TRUE ~ Category
  ))


write.csv(final_table, "outputs/tables/Table 1.csv", row.names = FALSE)
################################################################################


data$can



