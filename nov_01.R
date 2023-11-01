# Data Analysis - Oct 25
# Maria Francesca Arruda de Amaral

library(tidytable)
library(kableExtra)
library(ggplot2)

#load data
pgg <- read.delim("data/Rand_et_al_2014_Nature_Comm_extractionPGG_data.txt")
names(pgg)[3] <- "coop"
names(pgg) <- tolower(names(pgg))

#check log10 number of studies
intime_subjects <- lm(coop ~ factor(fast)*l_numstudies,
                      data = pgg[pgg$intime == 1,])

intime_subjects_ctrl <- lm(coop ~ factor(fast)*l_numstudies
                   + failedcomp + age + gender + factor(ed) + social_cons + fiscal_cons + god,
                   data = pgg[pgg$intime == 1,])

all_subjects <- lm(coop ~ factor(fast)*l_numstudies,
                        data = pgg)

all_subjects_ctrl <- lm(coop ~ factor(fast)*l_numstudies
                   + failedcomp + age + gender + factor(ed) + social_cons + fiscal_cons + god,
                   data = pgg)

#print table
extra_rows <- as.data.frame(rbind(c("Dummies: Education", "No", "Yes", "No", "Yes")))
attr(extra_rows, 'position') <- 21

coef_mapping <- c("factor(fast)1" = "Time pressure",
        "l_numstudies" = "Num. Studies",
        "factor(fast)1:l_numstudies" = "Time pressure x Num. Studies",
        "failedcomp" = "Failed comprehension",
        "age" = "Age",
        "gender" = "Female",
        "social_cons" = "Social conservatism (1–7 scale)",
        "fiscal_cons" = "Fiscal conservatism (1–7 scale)",
        "god" = "Belief in God (1–10 scale)",
        "(Intercept)" ="Constant")

modelsummary::modelsummary(list(intime_subjects,
                                intime_subjects_ctrl,
                                all_subjects,
                                all_subjects_ctrl),
                           vcov = "HC1",
                           coef_map = coef_mapping,
                           gof_map = c("nobs", "r.squared"),
                           stars = TRUE,
                           add_rows = extra_rows) %>%
  add_header_above(c(" " = 1, "Obeyed time constraint" = 2, "All subjects" = 2))

#check distribution of num_studies among naive and experienced
#exponentiate and then replace the 3 NAs with 0
pgg$numstudies <- replace_na(10^(pgg$l_numstudies), 0)

#data balance
modelsummary::datasummary_balance(~experienced, pgg, stars = TRUE, fmt = 3)

#different in social_cons, god, number of studies

ggplot(pgg, aes(x = numstudies, fill = factor(experienced))) +
  geom_histogram(alpha = 0.5, position = "dodge") +
  scale_fill_discrete(labels = c("Naive", "Experienced")) +
  guides(fill = guide_legend(title = "")) +
  theme_classic()

ggplot(pgg[pgg$numstudies <= 2001,], aes(x = numstudies, fill = factor(experienced))) +
  geom_histogram(alpha = 0.5, position = "dodge") +
  scale_fill_discrete(labels = c("Naive", "Experienced")) +
  guides(fill = guide_legend(title = "")) +
  theme_classic()

ggplot(pgg[pgg$numstudies <= 2001,], aes(x = factor(experienced), y = numstudies,
                                         fill = factor(experienced))) +
  geom_boxplot() +
  scale_fill_discrete(labels = c("Naive", "Experienced")) +
  guides(fill = guide_legend(title = "")) +
  labs(x = "", y = "Num. Studies") +
  theme_classic()
