# Data Analysis - Sep 27
# Maria Francesca Arruda de Amaral

library(tidytable)
library(kableExtra)

#load data from Fournier, Soroka, and Nir (2020)
individual <- haven::read_dta("data/Individual-data.dta") |>
  filter(country2 != "")
stimulus <- haven::read_dta("data/Stimulus-data.dta")
#timeseries <- haven::read_dta("data/Time-series-data-photos.dta")

#no raw ISI as far as I can tell, already scaled relative to baseline

#get correlation between reactions to positive vs negative videos AND positive vs negative images
neg_pos_cor <- individual |>
  summarise(n = n(),
            video_cor = cor(mean_neg2,
                            mean_pos2),
            photo_cor = cor(mean_ph_neg_all,
                            mean_ph_pos_all,
                            use = "complete.obs"),
            .by = country2)


#get alpha and average interitem correlation for images
#can only use the ones from IAPS that have the actual positive/negative flag

#data for alpha
neg_images <- stimulus |>
  filter(p_negative == 1, country2 != "CA.e") |>
  select(country2, resp, stimulus, p_gslmc_all) |>
  pivot_wider(names_from = stimulus, values_from = p_gslmc_all)

disgust_images <- stimulus |>
  filter(disgusting == 1, country2 != "CA.e") |>
  select(country2, resp, stimulus, p_gslmc_all) |>
  pivot_wider(names_from = stimulus, values_from = p_gslmc_all)

threat_images <- stimulus |>
  filter(threatening == 1, country2 != "CA.e") |>
  select(country2, resp, stimulus, p_gslmc_all) |>
  pivot_wider(names_from = stimulus, values_from = p_gslmc_all)

# pos_images <- stimulus |>
#   filter(p_negative == 0, country2 != "CA.e") |>
#   select(country2, resp, stimulus, p_gslmc_all) |>
#   pivot_wider(names_from = stimulus, values_from = p_gslmc_all)

#Helper functions
get_alpha <- function(scale, country) {
  scale <- as.data.frame(scale)
  psych::alpha(scale[scale["country2"] == country, -c(1,2)])$total$std.alpha
}

get_avg_r <- function(scale, country) {
  scale <- as.data.frame(scale)
  psych::alpha(scale[scale["country2"] == country, -c(1,2)])$total$average_r
}

#Negative Images
neg_alpha <- map_dbl(unique(neg_images$country2),
     function(x) get_alpha(neg_images, x))
neg_avg_r <- map_dbl(unique(neg_images$country2),
                      function(x) get_avg_r(neg_images, x))

#Threatening Images
threat_alpha <- map_dbl(unique(threat_images$country2),
                        function(x) get_alpha(threat_images, x))
threat_avg_r <- map_dbl(unique(threat_images$country2),
                         function(x) get_avg_r(threat_images, x))

#Disgusting Images
disgust_alpha <- map_dbl(unique(disgust_images$country2),
                         function(x) get_alpha(disgust_images, x))
disgust_avg_r <- map_dbl(unique(disgust_images$country2),
                          function(x) get_avg_r(disgust_images, x))

#bring them all together so we can have a table
data_for_table <- neg_pos_cor |>
  left_join(c(neg_alpha, "CA.e" = NA) |>
          as.list() |>
          as.data.frame() |>
          pivot_longer(names_to = "country2",
                       values_to = "neg_alpha")) |>
  left_join(c(neg_avg_r, "CA.e" = NA) |>
              as.list() |>
              as.data.frame() |>
              pivot_longer(names_to = "country2",
                           values_to = "neg_avg_r")) |>
  left_join(c(threat_alpha, "CA.e" = NA) |>
              as.list() |>
              as.data.frame() |>
              pivot_longer(names_to = "country2",
                           values_to = "threat_alpha")) |>
  left_join(c(threat_avg_r, "CA.e" = NA) |>
              as.list() |>
              as.data.frame() |>
              pivot_longer(names_to = "country2",
                           values_to = "threat_avg_r")) |>
  left_join(c(disgust_alpha, "CA.e" = NA) |>
              as.list() |>
              as.data.frame() |>
              pivot_longer(names_to = "country2",
                           values_to = "disgust_alpha")) |>
  left_join(c(disgust_avg_r, "CA.e" = NA) |>
              as.list() |>
              as.data.frame() |>
              pivot_longer(names_to = "country2",
                           values_to = "disgust_avg_r"))

#some formatting
data_for_table[,3:10] <- lapply(data_for_table[,3:10], function(x) {
  cell_spec(round(x,2), bold = T,
            color = spec_color(round(x,2), end = 0.9,
                               option = "inferno"))
})

#get table
data_for_table |>
  kable(escape = FALSE) |>
  kableExtra::kable_classic(full_width = F)

