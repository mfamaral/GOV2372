# Data Analysis - Nov 29
# Maria Francesca Arruda de Amaral

library(scales)
library(tidyverse)
library(stargazer)
library(lfe)
library(knitr)
library(haven)
library(broom)
library(tidycat)
library(knitr)
library(kableExtra)
library(gridExtra)
library(ggpubr)

#load and prepare data using replication code
dat <- read_dta(file = "data/final_replication_survey_dataset.dta") %>%
  filter(wave %in% 1:4) %>%
  rename(Q147_would_get_covid_vaccine_yes_binary   = Q147_would_get_covid_vaccine_yb,
         Q146_flu_vaccine_2020_yes_binary          = Q146_flu_vaccine_2020_yes_binar,
         zip_hh_inc_2018_median_range_middle       = zip_hh_inc_2018_median_range_mi,
         zip_educ_2018_high_school_or_less         = zip_educ_2018_high_school_or_le,
         demo_household_income_range_middle        = demo_household_income_range_mid,
         demo_race_ethnicity_binned_hispanic       = demo_race_ethnicity_binned_hisp,
         demo_household_income_top_quartile_binary = demo_household_income_top_quart,
         zip_hh_inc_2018_prop_top_quartile         = zip_hh_inc_2018_prop_top_quarti,
         Q10x_plan_tried_vaccinated_binary         = Q10x_plan_tried_vaccinated_bina,
         deaths_county_pc_1k_quantile_wave_start   = deaths_county_pc_1k_quant_w_st
  ) %>%
  mutate_if(is.labelled, as_factor) %>%
  mutate(party = ifelse(party == "", NA, party))

# create separate datasets for high and low risk (i.e. # of diagnoses)

dat_low <- dat %>%
  filter(Q7_health_dx_three_bins %in% "No Diagnoses")

dat_high <- dat %>%
  filter(Q7_health_dx_three_bins %in% "3 or more Diagnoses")

get_dat_long_dvs <- function(dat) {
  dat %>%
    select(
      weights,
      wave,
      # DVs
      Q31_mask_wearing_always_binary,
      Q147_would_get_covid_vaccine_yes_binary,
      Q146_flu_vaccine_2020_yes_binary,
      Q145_flu_vaccine_last_binary,
      # IVs
      party_with_independents,
      Q7_health_dx_three_bins,
      deaths_county_pc_1k_quantile_wave_start
    ) %>%
    gather(key = dv_name, value = dv_value,
           Q31_mask_wearing_always_binary,
           Q147_would_get_covid_vaccine_yes_binary,
           Q146_flu_vaccine_2020_yes_binary,
           Q145_flu_vaccine_last_binary
    ) %>%
    mutate(dv_name_original = dv_name,
           dv_name          = str_remove(dv_name, "^Q[0-9]*_"),
           dv_name          = str_remove(dv_name, "_binary$"),
           dv_name          = str_remove(dv_name, "^activity_"))
}

get_dat_long <- function(dat_long_dvs) {
  dat_long_dvs %>%
    # Independent Variables
    gather(key = iv_name, value = iv_value,
           party_with_independents,
           Q7_health_dx_three_bins,
           deaths_county_pc_1k_quantile_wave_start) %>%
    # Clear out Missing Values
    filter(!is.na(dv_value),
           !is.na(iv_value),
           !is.na(weights)) %>%
    mutate(iv_name = recode(iv_name,
                            "party_with_independents" = "Party ID",
                            "Q7_health_dx_three_bins" = "Major Health Diagnoses",
                            "deaths_county_pc_1k_quantile_wave_start" = "County COVID Severity"
    ))
}

dat_long_dvs_low <- get_dat_long_dvs(dat_low)
dat_long_dvs_high <- get_dat_long_dvs(dat_high)

dat_long_low <- get_dat_long(dat_long_dvs_low)
dat_long_high <- get_dat_long(dat_long_dvs_high)

blank_data <- tibble(
  wave_date = as.Date(c("2020-10-01", "2020-12-01")),
  dv_value_mean = 0.5
) %>%
  crossing(
    dv_name_original = c("mask_wearing_always", "would_get_covid_vaccine_yes", "flu_vaccine_2020_yes")
  ) %>%
  mutate(dv_name_original = fct_relevel(dv_name_original,
                                        "mask_wearing_always", "would_get_covid_vaccine_yes", "flu_vaccine_2020_yes")) %>%
  mutate(iv_value = "[MANUALL ENTER iv_value]")

get_figure1_party <- function(dat_long) {
  dat_long %>%
    filter(iv_name %in% c("Party ID",
                          "Major Health Diagnoses",
                          "County COVID Severity")) %>%
    group_by(wave, dv_name, iv_name, iv_value) %>%
    summarise(dv_value_mean = weighted.mean(x = dv_value,
                                            w = weights)) %>%
    ungroup() %>%
    drop_na() %>%
    left_join(tribble(
      ~dv_name, ~dv_name_long,
      "mask_wearing_always",   "Always wear mask",
      "would_get_covid_vaccine_yes", "COVID-19 vaccine",
      "flu_vaccine_last",     "Flu vaccine (2019 season)",
      "flu_vaccine_2020_yes", "Flu vaccine (2020 season)"
    ),
    by = "dv_name"
    ) %>%
    rename(dv_name_original = dv_name,
           dv_name = dv_name_long) %>%
    mutate(wave_date = as.Date(
      recode(wave,
             `1` = "2020-05-11",
             `2` = "2020-07-09",
             `3` = "2020-10-01",
             `4` = "2020-12-04"
      ))) %>%
    mutate(dv_name_original = fct_relevel(
      dv_name_original,
      "mask_wearing_always",
      "would_get_covid_vaccine_yes",
      "flu_vaccine_2020_yes",
      "flu_vaccine_last",
    )) %>%
    filter(iv_name %in% c("Party ID")) %>%
    filter(dv_name_original %in% c(
      "mask_wearing_always",
      "would_get_covid_vaccine_yes",
      "flu_vaccine_2020_yes"
    )) %>%
    mutate(temp_col = "") %>%
    mutate(iv_value = fct_relevel(iv_value,
                                  # "Overall",
                                  "Democrat",
                                  "Independent",
                                  "Republican")) %>%
    filter(iv_value != "") %>%
    # Plot
    ggplot(aes(x = wave_date,
               y = dv_value_mean,
               color = iv_value,
               group = iv_value)) +
    geom_blank(data = blank_data %>%
                 mutate(iv_value = "Democrat")) +
    geom_line() +
    geom_point() +
    # Theme and structure
    facet_grid(temp_col ~ dv_name_original, scales = "free_x",
               space = "free_x"  ) +
    theme_bw() +
    theme(legend.direction = "vertical",
          legend.position = "bottom",
          strip.background = element_blank(),
          strip.text.y = element_blank(),
          panel.spacing = unit(1., "lines")) +
    # Scales and Labels
    guides(color = guide_legend(ncol = 1,
                                bycol = TRUE)) +
    expand_limits(y = c(0, 1)) +
    scale_x_date(
      breaks = seq(as.Date("2020-01-01"), as.Date("2021-01-01"), "1 months"),
      date_labels = "%b",
      expand = expand_scale(mult = 0.1),
      position = "top",
    ) +
    scale_y_continuous(breaks = seq(0, 1, 0.2),
                       labels = scales::percent) +
    labs(x = NULL, y = NULL)
}


get_figure1_party(dat_long_low)
get_figure1_party(dat_long_high)


# FIGURE 2
get_plot_prop_r_x_masks_vaccines_y <- function(dat) {
  dat %>%
    select(weights,
           wave,
           party, prop_r,
           Q31_mask_wearing_always_binary,
           Q147_would_get_covid_vaccine_yes_binary,
           Q146_flu_vaccine_2020_yes_binary,
           Q145_flu_vaccine_last_binary
    ) %>%
    # Make long
    pivot_longer(cols = c("Q31_mask_wearing_always_binary",
                          "Q147_would_get_covid_vaccine_yes_binary",
                          "Q146_flu_vaccine_2020_yes_binary",
                          "Q145_flu_vaccine_last_binary"),
                 names_to = "dv_name", values_to = "dv_value") %>%
    # Remove Independents + missing values in DVs
    drop_na(party, dv_value) %>%
    left_join(tribble(
      ~dv_name, ~dv_name_long,
      "Q31_mask_wearing_always_binary",   "Always wear mask",
      "Q147_would_get_covid_vaccine_yes_binary", "COVID-19 vaccine",
      "Q145_flu_vaccine_last_binary",     "Flu vaccine (2019 season)",
      "Q146_flu_vaccine_2020_yes_binary", "Flu vaccine (2020 season)"
    ),
    by = "dv_name"
    ) %>%
    rename(dv_name_original = dv_name,
           dv_name = dv_name_long) %>%
    mutate(dv_name = fct_relevel(
      dv_name,
      "Always wear mask",
      "COVID-19 vaccine",
      "Flu vaccine (2019 season)",
      "Flu vaccine (2020 season)"
    )) %>%
    mutate(dv_name_wrapped = factor(str_replace_all(dv_name, " ", "\n"),
                                    levels = str_replace_all(levels(dv_name), " ", "\n"))) %>%
    filter(dv_name %in% c(
      "Always wear mask",
      "COVID-19 vaccine",
      "Flu vaccine (2020 season)"
    )) %>%
    # Plot
    ggplot(data = .,
           aes(x= prop_r, color = party,fill=party, y = dv_value))+
    geom_smooth(aes(weight=weights),
                method = "loess") +
    # Theme and Structure
    theme_minimal() +
    theme(legend.position = c(0.75, 0.25),
          strip.placement = "outside",
          legend.direction = "vertical") +
    facet_wrap( ~ dv_name, nrow = 2, scales = "free_x") +
    # Labels and Scales
    labs(y = "Percent",
         x = '% GOP in ZIP Code') +
    scale_y_continuous(labels = percent) +
    scale_x_continuous(labels = percent, expand = expand_scale(mult = 0.1)) +
    expand_limits(y = c(0, 1)) +
    coord_cartesian(ylim = c(0,1))
}

get_plot_prop_r_x_masks_vaccines_y(dat_low)
get_plot_prop_r_x_masks_vaccines_y(dat_high)

## MODELING
data_for_model_list <- dat %>%
  # Make long
  pivot_longer(cols = c("Q31_mask_wearing_always_binary",
                        "Q147_would_get_covid_vaccine_yes_binary",
                        "Q146_flu_vaccine_2020_yes_binary",
                        "Q145_flu_vaccine_last_binary"),
               names_to = "dv_name",
               values_to = "dv_value")

dat_long_dvs <- dat %>%
  select(
    weights,
    wave,
    demo_zip_scrambled,
    prop_r,
    # DVs
    Q31_mask_wearing_always_binary,
    Q147_would_get_covid_vaccine_yes_binary,
    Q146_flu_vaccine_2020_yes_binary,
    Q145_flu_vaccine_last_binary,
    # IVs
    party_with_independents
  ) %>%
  gather(key = dv_name, value = dv_value,
         Q31_mask_wearing_always_binary,
         Q147_would_get_covid_vaccine_yes_binary,
         Q146_flu_vaccine_2020_yes_binary,
         Q145_flu_vaccine_last_binary
  ) %>%
  mutate(dv_name_original = dv_name,
         dv_name          = str_remove(dv_name, "^Q[0-9]*_"),
         dv_name          = str_remove(dv_name, "_binary$"),
         dv_name          = str_remove(dv_name, "^activity_"))

dv_name_vec_four_primary <- c(
  "Q31_mask_wearing_always_binary" = "Always Wear Mask",
  "Q147_would_get_covid_vaccine_yes_binary" = "COVID Vaccine",
  "Q146_flu_vaccine_2020_yes_binary" = "Flu Vaccine 2020",
  "Q145_flu_vaccine_last_binary" = "Flu Vaccine 2019"
)

formula_zc_interactions_white_college_income <-
  dv_value ~ republican +
  republican:prop_r +
  republican:zip_white +
  republican:zip_educ_2018_college_and_above +
  republican:zip_hh_inc_2018_median_range_middle +
  republican:prop_r:Q7_health_dx_sum +
  log(deaths_14_days_county_pc_1k + 0.1) +
  # Time Fixed Effects
  as.factor(wave) +
  # Individual Controls
  factor(demo_gender) +
  demo_household_income_range_middle +
  demo_household_income_missing +
  as_factor(demo_education_binned) +
  demo_age +
  demo_race_ethnicity_binned_hispanic +
  Q7_health_dx_sum |
  # FE + SE
  demo_zip_scrambled | 0 | demo_zip_scrambled

model_zc_interactions_white_college_income <- data_for_model_list %>%
  # Run models
  split(.$dv_name) %>%
  map( ~ felm(data = drop_na(., party),
              weights = drop_na(., party)$weights,
              formula = formula_zc_interactions_white_college_income))


source("helper-functions.R")
source("helper-objects.R")

model_zc_interactions_white_college_income_kable_ready <-
  tidy_felm_prep_kable(
    input_model_list = model_zc_interactions_white_college_income,
    input_term_names = df_display_names_regressions,
    dv_name_vec = dv_name_vec_four_primary
  )

model_zc_interactions_white_college_income_kable_fe_info <- create_kable_felm(
  data = model_zc_interactions_white_college_income_kable_ready,
  dv_name_vec = dv_name_vec_four_primary,
  input_term_names = df_display_names_regressions,
  packed_rows = FALSE,
  escape_first_space = TRUE,
  caption = "Within-ZIP Code Models of Neighborhood Partisanship (All Coefficients)",
  fixed_effect_info = list(
    vars = names(dv_name_vec_four_primary),
    fe   = c("\\ ZIP Code Fixed Effect"),
    cell_value = "\\ \\ \\ \\ \\ \\ \\checkmark"
  )
)
