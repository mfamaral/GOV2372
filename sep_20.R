# Data Analysis - Sep 20
# Maria Francesca Arruda de Amaral

library(tidytable)
library(naniar)
library(ggplot2)

country <- haven::read_dta("data/CountryData.dta")

#The motivation: this below gave me NAs
 country |>
   summarize(across(starts_with("s_"),
                      mean),
              .by = colony_prt)

#Check whether maybe it's not generalized?
colMeans(is.na(country[, c("s_diff_trust_out_in", "s_religion_hell",
                           "s_values_uniform", "s_mfq_disgusting",
                           "s_diff_shame_guilt_overall", "s_gps_punish_revenge")]))

#let's focus on the variables the paper uses for Figure V, drop variables we don't use
missing_check <- country |>
  select(-c(contains(c("contribution", "cheating")),
            starts_with(c("trust",
                          "religion",
                          "mfq",
                          "gps",
                          "diff",
                          "values"))))

#When one of them is missing, most often all are, since they come from the same source. 118/216 (54.63%) countries in the sample don't actually have measures for what Figure V claims to show
gg_miss_upset(missing_check[, c("s_diff_trust_out_in", "s_religion_hell",
                                "s_values_uniform", "s_mfq_disgusting",
                                "s_diff_shame_guilt_overall", "s_gps_punish_revenge")],
              nsets = 6)

#But at least it seems to be happening at all kinship levels, with more accumulation on the higher end

#lowest missing rate
ggplot(missing_check,
       aes(x = kinship_score,
           y = s_religion_hell)) +
  geom_miss_point()

#highest missing rate
ggplot(missing_check,
       aes(x = kinship_score,
           y = s_diff_shame_guilt_overall)) +
  geom_miss_point()

#Make some categorical variables to check distribution there as well
missing_check <- missing_check |>
  mutate.(continent = case_when.(continents_africa == 1 ~ "africa",
                                 continents_asia == 1 ~ "asia",
                                 continents_europe == 1 ~ "europe",
                                 continents_oceania == 1 ~ "oceania",
                                 continents_north_america == 1 ~ "north_america",
                                 continents_south_america == 1 ~ "south_america",
                                 TRUE ~ "other"),
          region = case_when.(cont_ssa == 1 ~ "sub_saharan_africa",
                              cont_mena == 1 ~ "middle_east_north_africa",
                              cont_eca == 1 ~ "europe_central_asia",
                              cont_sas == 1 ~ "south_asia",
                              cont_eap == 1 ~ "east_asia_pacific",
                              cont_nam == 1 ~ "north_america",
                              cont_lac == 1 ~ "latin_america_caribbean",
                              TRUE ~ "other"),
          colonizer = case_when.(colony_esp == 1 ~ "esp",
                                 colony_gbr == 1 ~ "gbr",
                                 colony_fra == 1 ~ "fra",
                                 colony_prt == 1 ~ "prt",
                                 colony_oeu == 1 ~ "oeu",
                                 TRUE ~ "none"),
          was_colony = colony_esp == 1 | colony_gbr == 1 | colony_fra == 1 |
                                 colony_prt == 1 | colony_oeu == 1,
          catholic = cath00 > 0.5,
          muslim = muslim00 > 0.5)

#Try by category
gg_miss_fct(x = missing_check[, c("s_diff_trust_out_in", "s_religion_hell",
                                  "s_values_uniform", "s_mfq_disgusting",
                                  "s_diff_shame_guilt_overall", "s_gps_punish_revenge",
                                  "continent")], fct = continent)
gg_miss_fct(x = missing_check[, c("s_diff_trust_out_in", "s_religion_hell",
                                  "s_values_uniform", "s_mfq_disgusting",
                                  "s_diff_shame_guilt_overall", "s_gps_punish_revenge",
                                  "region")], fct = region)
gg_miss_fct(x = missing_check[, c("s_diff_trust_out_in", "s_religion_hell",
                                  "s_values_uniform", "s_mfq_disgusting",
                                  "s_diff_shame_guilt_overall", "s_gps_punish_revenge",
                                  "colonizer")], fct = colonizer)

#Finally, map missingness by country

world <- map_data("world")
world$isocode <- countrycode::countrycode(world$region, "country.name", "iso3c")

data_to_map <- missing_check[, c("s_diff_trust_out_in", "s_religion_hell",
                                 "s_values_uniform", "s_mfq_disgusting",
                                 "s_diff_shame_guilt_overall", "s_gps_punish_revenge",
                                 "isocode")] |>
  summarise(across(starts_with("s_"),
                   is.na),
            .by = "isocode") |>
  mutate(miss_rate = rowMeans(across(where(is.logical))))

data_with_geo <- merge(world, data_to_map, by = "isocode", all.x = TRUE)

data_with_geo |>
  ggplot(aes(fill = miss_rate, map_id = region)) +
  geom_map(map = world, color = "black") +
  expand_limits(x = world$long, y = world$lat) +
  theme_bw() +
  scale_fill_continuous(na.value="grey")
