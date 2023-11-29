df_display_names_regressions <- tribble(
  ~packed_rows, ~term, ~display_name,
  "Primary Variables", "prop_r", "Share GOP",
  "Primary Variables", "republican", "Republican",
  "Primary Variables", "republican:prop_r", "Republican * Share GOP",
  "Primary Variables", "republican:zip_white", "Republican * Share White ('18 Zip)",
  "Primary Variables", "republican:zip_educ_2018_college_and_above", "Republican * Share College and Above ('18 ZIP)",
  "Primary Variables", "republican:zip_hh_inc_2018_median_range_middle","Republican * Median HH Income, Thousands ('18 ZIP)",
  "Primary Variables", "republican:prop_r:Q7_health_dx_sum","Republican * Share GOP * Health Diagnoses",
  # # WHITE INTERACTIONS VERSION
  "Primary Variables", "white",                                     "White",
  "Primary Variables", "white:zip_white",                           "White * Share White ('18 ZIP)",
  "Primary Variables", "white:prop_r",                              "White * Share GOP",
  "Primary Variables", "white:zip_educ_2018_college_and_above",     "White * Share College and Above ('18 ZIP)",
  "Primary Variables", "white:zip_hh_inc_2018_median_range_middle", "White * Median HH Income, Thousands ('18 ZIP)",
  # # COLLEGE AND ABOVE INTERACTIONS VERSION
  "Primary Variables", "college_and_above",                                     "College and Above",
  "Primary Variables", "college_and_above:zip_educ_2018_college_and_above",     "College and Above * Share College and Above ('18 ZIP)",
  "Primary Variables", "college_and_above:prop_r",                              "College and Above * Share GOP",
  "Primary Variables", "college_and_above:zip_white",                           "College and Above * Share White ('18 ZIP)",
  "Primary Variables", "college_and_above:zip_hh_inc_2018_median_range_middle", "College and Above * Median HH Income, Thousands ('18 ZIP)",
  # HOUSEHOLD INCOME INTERACTIONS VERSION
  "Primary Variables", "demo_household_income_top_quartile_binary",                                     "High Income",
  "Primary Variables", "demo_household_income_top_quartile_binary:zip_hh_inc_2018_prop_top_quartile",   "High Income * Share High Income ('18 ZIP)",
  "Primary Variables", "demo_household_income_top_quartile_binary:prop_r",                              "High Income * Share GOP",
  "Primary Variables", "demo_household_income_top_quartile_binary:zip_white",                           "High Income * Share White ('18 ZIP)",
  "Primary Variables", "demo_household_income_top_quartile_binary:zip_educ_2018_college_and_above",     "High Income * Share College and Above ('18 ZIP)",
  # MASK POLICY INTERACTIONS
  "Primary Variables", "mask_policy_active",                   "Mask Policy",
  "Primary Variables", "republican:mask_policy_active",        "Republican * Mask Policy",
  "Primary Variables", "prop_r:mask_policy_active",            "Share GOP * Mask Policy",
  "Primary Variables", "republican:prop_r:mask_policy_active", "Republican * Share GOP * Mask Policy",
  # LOCAL COVID-19 DEATHS
  "COVID Deaths", "log(deaths_14_days_county_pc_1k + 0.1)", "Log(Deaths per 1K People)",
  # INDIVIDUAL CONTROLS
  "Individual Controls", "factor(demo_gender)Female", "Female",
  "Individual Controls", "demo_household_income_range_middle", "Household Income",
  "Individual Controls", "demo_household_income_missing", "Household Income Missing",
  "Individual Controls", "demo_age", "Age",
  "Individual Controls", "demo_race_ethnicity_binned_hispanicWhite", "White",
  "Individual Controls", "demo_race_ethnicity_binned_hispanicBlack, or African American", "Black",
  "Individual Controls", "demo_race_ethnicity_binned_hispanicHispanic", "Hispanic",
  "Individual Controls", "demo_race_ethnicity_binned_hispanicAsian or Pacific Islander", "Asian or Pacific Islander",
  "Individual Controls", "demo_race_ethnicity_binned_hispanicSome other race", "Some other race",
  "Individual Controls", "as_factor(demo_education_binned)Some College", "Some College",
  "Individual Controls", "as_factor(demo_education_binned)College and Above", "College and Above",
  "Individual Controls", "Q7_health_dx_sum", "Health Diagnoses (0-6)",
  "ZIP Code Controls", "zip_educ_2018_college_and_above", "Share College and Above ('18 ZIP)",
  "ZIP Code Controls", "zip_hh_inc_2018_median_range_middle", "Median HH Income ('18 ZIP)",
  "ZIP Code Controls", "zip_pop_density_2018_over_2010", "Pop. Density ('18 ZIP)",
  "ZIP Code Controls", "zip_white", "Share White ('18 ZIP)",
  "Ideology", "demo_ideology_fLibertarian",       "Libertarian",
  "Ideology", "demo_ideology_fVery Conservative", "Very Conservative",
  "Ideology", "demo_ideology_fConservative",      "Conservative",
  "Ideology", "demo_ideology_fLiberal",           "Liberal",
  "Ideology", "demo_ideology_fVery Liberal",      "Very Liberal",
  "Ideology", "demo_ideology_fI don't know",      "I don't know",
  "Time Fixed Effects", "as.factor(wave)2", "Wave 2",
  "Time Fixed Effects", "as.factor(wave)3", "Wave 3",
  "Time Fixed Effects", "as.factor(wave)4", "Wave 4",
  "Time Fixed Effects", "as.factor(wave)5", "Wave 5",
  "Time Fixed Effects", "as.factor(wave)6", "Wave 6",
  "Time Fixed Effects", "as.factor(wave)7", "Wave 7",
)

# Colors

colors = c(`Democrat` = 'blue', `Republican`='red')

colors_with_independents = c(`Democrat` = 'blue', `Republican`='red',
                             `Independent`='gold', `Overall`='gray')

colors_multiple_variables = c(
  `Democrat` = 'blue',
  `Republican`='red',
  `Independent`='gold',
  `No Diagnoses` = "forestgreen",
  `1 or 2 Diagnoses` = "darkblue",
  `3 or more Diagnoses` = "darkorange",
  `1st Quartile` = "#999999",
  `2nd Quartile` = "#FFB900",
  `3rd Quartile` = "#DD4224",
  `4th Quartile` = "#660F56",
  `Overall`='gray'

)
