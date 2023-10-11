# Data Analysis - Oct 11
# Maria Francesca Arruda de Amaral

library(tidytable)
library(ggplot2)
library(cowplot)

#load data
croke <- haven::read_dta("data/croke_data.dta")

#replicate figure 5 separated by gender

sum_data <- croke |>
  filter(year_birth %in% 1945:1985) |>
  summarise(across(c(edu, setdiff(contains("complete"), contains("mean"))), mean),
            weight_cohort = n(),
            .by = c(year_birth, male))


sum_data |>
  ggplot(aes(x = year_birth,
             y = edu,
             color = as.factor(male))) +
  geom_point(aes(size = weight_cohort,
                 shape = as.factor(male))) +
  geom_vline(xintercept = 1963.5) +
  geom_vline(xintercept = 1966.5) +
  geom_smooth(data = sum_data[sum_data$year_birth>=1945 & sum_data$year_birth<=1963, ],
              se = FALSE) +
  geom_smooth(data = sum_data[sum_data$year_birth>1966 & sum_data$year_birth<=1985, ],
              se = FALSE) +
  theme_cowplot(12)

#get columns
columns <- names(sum_data)[3:8]
names(columns) <- c("Education", "Incomplete primary school",
                    "Complete primary school", "Incomplete secondary school",
                    "Complete secondary school", "Incomplete college")

#premake list of plots
plot_list <- vector(mode = "list", length = 6)
names(plot_list) <- columns

plot_list[["edu"]] <- sum_data |>
  ggplot(aes(x = year_birth,
             y = .data[["edu"]],
             color = as.factor(male))) +
  geom_point(aes(size = weight_cohort,
                 shape = as.factor(male))) +
  geom_vline(xintercept = 1963.5) +
  geom_vline(xintercept = 1966.5) +
  geom_smooth(data = sum_data[sum_data$year_birth>=1945 & sum_data$year_birth<=1963, ],
              method = "loess",
              se = FALSE) +
  geom_smooth(data = sum_data[sum_data$year_birth>1966 & sum_data$year_birth<=1985, ],
              method = "loess",
              se = FALSE) +
  theme_cowplot(12) +
  guides(size = "none", shape = "none", color = "none") +
  scale_x_continuous(breaks = seq(1945, 1985, 5)) +
  scale_y_continuous(breaks = seq(1, 3.5, 0.5),
                     limits = c(1, 3.7)) +
  labs(x = "Year of birth",
       y = "Mean",
       title = "Education")

for (i in 2:6) {
  plot_list[[columns[i]]] <- sum_data |>
    ggplot(aes(x = year_birth,
               y = .data[[columns[i]]],
               color = as.factor(male))) +
    geom_point(aes(size = weight_cohort,
                   shape = as.factor(male))) +
    geom_vline(xintercept = 1963.5) +
    geom_vline(xintercept = 1966.5) +
    geom_smooth(data = sum_data[sum_data$year_birth>=1945 & sum_data$year_birth<=1963, ],
                method = "loess",
                se = FALSE) +
    geom_smooth(data = sum_data[sum_data$year_birth>1966 & sum_data$year_birth<=1985, ],
                method = "loess",
                se = FALSE) +
    theme_cowplot(12) +
    guides(size = "none", shape = "none", color = "none") +
    scale_x_continuous(breaks = seq(1945, 1985, 5)) +
    scale_y_continuous(breaks = seq(0, 1, 0.2)) +
    coord_cartesian(ylim = c(0,1)) +
    labs(x = "Year of birth",
         y = "Proportion",
         title = names(columns[i]))
}

plot_grid(plotlist = plot_list, ncol = 3)
