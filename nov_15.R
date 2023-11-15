# Data Analysis - Nov 15
# Maria Francesca Arruda de Amaral

library(tidytable)
library(ggplot2)
library(modelsummary)
library(kableExtra)

#load data
load("data/gidron_hall_data.RData")

#inspired by Gidron & Hall's code:

data |>
  filter(!is.na(foreign.born), !is.na(sss)) |>
  ggplot() +
  geom_bar(mapping = aes(x = factor(sss),
                         y = after_stat(prop),
                         fill = factor(foreign.born),
                         group = factor(foreign.born)),
           color = "black") +
  facet_wrap(~ foreign.born) +
  scale_fill_discrete(labels = c("Native", "Foreign")) +
  guides(fill = guide_legend(title = "")) +
  theme_classic()

#as can be seen, not much difference
summary(data$sss[data$foreign.born == 1])
summary(data$sss[data$foreign.born == 0])


#now, look by country to see some statistics

data_for_table <- data |>
  filter(!CNTRY %in% c("Israel",
                       "Russian Federation",
                       "Ukraine",
                       "Kosovo")) |>
  summarise(Mean = mean(sss, na.rm = TRUE),
            SD = sd(sss, na.rm = TRUE),
            .by = CNTRY)

data_for_table$CNTRY <- cell_spec(data_for_table$CNTRY,
                                 color = case_when(data_for_table$Mean < 5 ~ "red",
                                                   data_for_table$Mean < 6 ~ "indianred",
                                                   data_for_table$Mean < 6.50 ~ "orange",
                                                   TRUE ~ "black"))

data_for_table$Mean <- cell_spec(round(data_for_table$Mean, 4),
                                  color = case_when(data_for_table$Mean < 5 ~ "red",
                                                    data_for_table$Mean < 6 ~ "indianred",
                                                    data_for_table$Mean < 6.50 ~ "orange",
                                                    TRUE ~ "black"))

kbl(data_for_table, escape = F) %>%
  kable_paper("striped", full_width = F)

#create a flag and redo the histograms from earlier
data |>
  filter(!CNTRY %in% c("Israel",
                       "Russian Federation",
                       "Ukraine",
                       "Kosovo"),
         !is.na(sss)) |>
  mutate(low_sss = CNTRY %in% c("Bulgaria", "Estonia", "Spain",
                                "France", "United Kingdom", "Hungary",
                                "Italy", "Lithuania",
                                "Poland", "Portugal",
                                "Slovenia", "Slovakia")) |>
  ggplot() +
  geom_bar(mapping = aes(x = factor(sss),
                         y = after_stat(prop),
                         fill = factor(low_sss),
                         group = factor(low_sss)),
           color = "black") +
  facet_wrap(~ low_sss) +
  scale_fill_discrete(labels = c("Above-mean countries", "Below-mean countries")) +
  guides(fill = guide_legend(title = "")) +
  theme_classic()

#specifically, do for Bulgaria, Hungary, and Portugal (under 6)
data |>
  filter(CNTRY %in% c("Bulgaria",
                      "Portugal",
                      "Hungary"),
         !is.na(sss)) |>
  ggplot() +
  geom_bar(mapping = aes(x = factor(sss),
                         y = after_stat(prop),
                         fill = factor(CNTRY),
                         group = factor(CNTRY)),
           color = "black") +
  facet_wrap(~ CNTRY) +
  guides(fill = guide_legend(title = "")) +
  theme_classic()

#map that shows means
world <- map_data("world")

subset_map <- world |>
  filter(region %in% unique(data$CNTRY))

subset_map <- world |>
  filter(long >= min(subset_map$long),
         long <= max(subset_map$long),
         lat >= min(subset_map$lat),
         lat <= max(subset_map$lat))

data_to_map <- data |>
  filter(!CNTRY %in% c("Israel",
                       "Russian Federation",
                       "Ukraine",
                       "Kosovo")) |>
  summarise(Mean = mean(sss, na.rm = TRUE),
            SD = sd(sss, na.rm = TRUE),
            .by = CNTRY) |>
  rename("region" = "CNTRY")

map_to_plot <- merge(subset_map, data_to_map, by = "region", all.x = TRUE) |>
  distinct(region, long, lat, Mean, SD)

lab_data <- map_to_plot %>%
  group_by(region) %>%
  summarise(long = mean(long), lat = mean(lat))
map_to_plot |>
  ggplot() +
  geom_map(aes(fill = Mean, map_id = region), map = subset_map, color = "black") +
  expand_limits(x = subset_map$long, y = subset_map$lat) +
  geom_label(aes(label = region, x = long, y = lat), data = lab_data,  size = 3, hjust = 0.5,
             fill = "white") +
  theme_bw() +
  scale_fill_continuous(na.value="grey")
