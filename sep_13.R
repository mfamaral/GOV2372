# Data Analysis - Tybur et al. 2016
# Maria Francesca Arruda de Amaral
library(tidytable)
library(ggplot2)

#load in dataset
rep_data <- readxl::read_excel("data/pnas.1607398113.sd01.xlsx")

#using info from Table 1, re-identify countries
nation_lookup <- tidytable(nation_name = c("Argentina", "Australia", "Belgium",
                                           "Bosnia and Herzegovina", "Brazil", "Canada",
                                           "Chile", "China", "Croatia", "Denmark",
                                           "Finland", "France", "Germany", "Greece",
                                           "India", "Ireland", "Israel", "Japan",
                                           "Netherlands", "New Zealand", "Poland",
                                           "Republic of Korea", "Serbia", "Singapore",
                                           "Slovakia", "Spain", "Sweden", "Turkey",
                                           "United Kingdom", "United States"),
                           nation_abbr = c("AR", "AU", "BE", "BA", "BR", "CA",
                                           "CL", "CN", "HR", "DK", "FI", "FR",
                                           "DE", "GR", "IN", "IE", "IL", "JP",
                                           "NL", "NZ", "PL", "KR", "RS", "SG",
                                           "SK", "ES", "SE", "TR", "UK", "US"),
                           nation_obs = c(827, 300, 448, 326, 288, 307, 262, 377, 554, 126,
                                          190, 266, 374, 317, 504, 150, 339, 394, 574, 595,
                                          210, 137, 402, 239, 338, 699, 117, 1082, 276, 483))
rep_data <- rep_data |>
  mutate.(nation_obs = n.(),
          .by = nation) |>
  left_join.(nation_lookup,
             by = "nation_obs")


#Noticed from Table 1 that the correlations vary widely across countries
#Recalculate to better visualize
correlations <- rep_data |>
  summarize.(trad_ds = cor(Traditionalim, DS),
             sdo_ds = cor(SDO, DS),
             .by = nation_name) |>
  arrange.(desc.(trad_ds))

#Plot mean disgust sensitivity and mean traditionalism/sdo
data_to_plot <- rep_data |>
  summarize.(trad_agg = mean(Traditionalim),
             DS_agg = mean(DS),
             SDO_agg = mean(SDO),
             trad_cor = cor(DS,Traditionalim),
             SDO_cor = cor(DS,SDO),
             HistPath = max(HistPath), #all same anyway
             .by = nation_abbr) %>%
  mutate.(trad_cor_class = factor(case_when.(trad_cor > quantile(trad_cor, 0.75) ~ "HIGH",
                                      trad_cor < quantile(trad_cor, 0.25) ~ "LOW",
                                      TRUE ~ "AVG"),
                                  levels = c("LOW", "AVG", "HIGH")),
          SDO_cor_class = factor(case_when.(SDO_cor > quantile(SDO_cor, 0.75) ~ "HIGH",
                                            SDO_cor < quantile(SDO_cor, 0.25) ~ "LOW",
                                            TRUE ~ "AVG"),
                                 levels = c("LOW", "AVG", "HIGH")))

#Graph 1
data_to_plot |>
  ggplot(aes(x = DS_agg, y = trad_agg, label = nation_abbr, color = trad_cor_class)) +
  geom_text() +
  scale_color_manual(values = c("blue", "black", "red"), name = "Correlation Level") +
  labs(x = "Disgust Sensitivity",
       y = "Traditionalism") +
  theme_bw() +
  theme(legend.position = "bottom")

#Graph 2
data_to_plot |>
  ggplot(aes(x = DS_agg, y = SDO_agg, label = nation_abbr, color =  SDO_cor_class)) +
  geom_text() +
  scale_color_manual(values = c("blue", "black", "red"), name = "Correlation Level") +
  labs(x = "Disgust Sensitivity",
       y = "SDO") +
  theme_bw() +
  theme(legend.position = "bottom")

#Graph 3
data_to_plot |>
  mutate.(HistPath = HistPath + 1.31) |>
  ggplot(aes(x = HistPath, y = trad_cor, label = nation_abbr)) +
  geom_text() +
  geom_smooth(method = "lm", formula = y ~ x) +
  labs(x = "Parasite Stress",
       y = "Trad-DS Correlation") +
  theme_bw()

#Graph 4
data_to_plot |>
  mutate.(HistPath = HistPath + 1.31) |>
  ggplot(aes(x = HistPath, y = SDO_cor, label = nation_abbr)) +
  geom_text() +
  geom_smooth(method = "lm", formula = y ~ x) +
  labs(x = "Parasite Stress",
       y = "SDO-DS Correlation") +
  theme_bw()
