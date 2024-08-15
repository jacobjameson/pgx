################################################################################
# Author: J. Jameson

# Purpose: Generate regression tables for the manuscript
################################################################################
rm(list = ls())

# Load libraries ---------------------------------------------------------------
library(tidyverse)
library(survey)
library(broom)

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
         ))

data$n_presc_cat <- factor(data$n_presc_cat, 
                           levels = c('None',"1", "2-3", "4-5", "6-7", "8-9", "10+"))

data$pgx_exposure <- ifelse(data$n_pgx >= 1, 1, 0)

data$subpop = (data$adult == 1 & data$n_drugs > 0 & data$region_cat != 'Other')

################################################################################
# Table 3 ----------------------------------------------------------------------

svy_data <- svydesign(
  id = ~VARPSU, 
  strata = ~VARSTR,
  weights = ~poolwt, 
  data = data, 
  nest = TRUE
)

race <- svyglm(
  pgx_exposure ~ race_cat, 
  design = subset(svy_data, subpop), 
  family = quasibinomial) 

age <- svyglm(
  pgx_exposure ~ age_cat, 
  design = subset(svy_data, subpop), 
  family = quasibinomial) 

fem <- svyglm(
  pgx_exposure ~ female, 
  design = subset(svy_data, subpop), 
  family = quasibinomial) 

ed <- svyglm(
  pgx_exposure ~ educ_cat, 
  design = subset(svy_data, subpop), 
  family = quasibinomial) 

inc <- svyglm(
  pgx_exposure ~ POVCAT, 
  design = subset(svy_data, subpop), 
  family = quasibinomial) 

inc <- svyglm(
  pgx_exposure ~ POVCAT, 
  design = subset(svy_data, subpop), 
  family = quasibinomial) 

region <- svyglm(
  pgx_exposure ~ region_cat,
  design = subset(svy_data, subpop), 
  family = quasibinomial) 

ins <- svyglm(
  pgx_exposure ~ INSCOVX,
  design = subset(svy_data, subpop), 
  family = quasibinomial) 

cond <- svyglm(
  pgx_exposure ~ cond_count_cat,
  design = subset(svy_data, subpop), 
  family = quasibinomial) 

presc <- svyglm(
  pgx_exposure ~ n_presc_cat,
  design = subset(svy_data, subpop), 
  family = quasibinomial) 

# Collecting results for unadjusted models
models <- list(race, age, fem, ed, inc, region, ins, cond, presc)
names(models) <- c("Race", "Age", "Gender", "Education", "Income", 
                   "Region", "Insurance", "Condition Count", "Prescription Count")

# Function to extract results
extract_results <- function(model, name) {
  tidy(model, conf.int = TRUE, exponentiate = TRUE) %>%
    mutate(
      Independent_Variables = name,
      OR = sprintf("%.2f", estimate),
      CI = sprintf("(%.2f, %.2f)", conf.low, conf.high),
      P_value = ifelse(p.value < 0.001, "<0.001", sprintf("%.3f", p.value))
    ) %>%
    select(Independent_Variables, Term = term, `Unadjusted OR` = OR, 
           ` Unadjusted P_value`= P_value, `Unadjusted 95% CI` = CI)
}

# Applying the function to each model
unadjusted_results <- lapply(names(models), function(x) extract_results(models[[x]], x))

# Combine into a single data frame
unadjusted_table <- do.call(rbind, unadjusted_results) %>%
  filter(Term != "(Intercept)")

# Adjusted models -------------------------------------------------------------

adjusted_model <- svyglm(
  pgx_exposure ~ race_cat + age_cat + female + educ_cat + POVCAT + 
                 region_cat + INSCOVX + cond_count_cat + n_presc_cat,
  design = subset(svy_data, subpop),
  family = quasibinomial
)

# Extracting and formatting the results
adjusted_results <- tidy(adjusted_model, conf.int = TRUE, 
                         exponentiate = TRUE) %>%
  mutate(
    OR = sprintf("%.2f", estimate),
    CI = sprintf("(%.2f, %.2f)", conf.low, conf.high),
    P_value = ifelse(p.value < 0.001, "<0.001", sprintf("%.3f", p.value))
  ) %>%
  select(Term = term, `Adjusted OR` = OR, 
         `Adjusted P_value`= P_value, `Adjusted 95% CI` = CI) %>%
  filter(Term != "(Intercept)")

# Combine into a single data frame
final_table <- left_join(unadjusted_table, adjusted_results, by = "Term")

# Save the table
write.csv(final_table, "outputs/tables/Table 3.csv", row.names = FALSE)

################################################################################


