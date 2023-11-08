# Data Analysis - Nov 08
# Maria Francesca Arruda de Amaral

library(tidytable)
library(kableExtra)

#load data
load("data/Experiment_1.RData")

sort(table(tolower(data$gk.Cheney.original)))

#print these for demonstration

data.frame(answers = levels(data$gk.Cheney.original)[c(16, 19, 23, 31, 43, 44, 45, 46, 47, 57, 93, 150, 152, 153, 171, 200, 201, 205, 206, 209, 210, 211, 237, 238, 239, 241, 272, 275, 285)]) |>
  kable() |>
  kable_classic(full_width = F)

#are there any characteristics that stand out for these respondents?
respondents <- data |>
  filter(gk.Cheney.original %in% levels(data$gk.Cheney.original)[c(16, 19, 23, 31, 43, 44, 45, 46, 47, 57, 93, 150, 152, 153, 171, 200, 201, 205, 206, 209, 210, 211, 237, 238, 239, 241, 272, 275, 285)])


#get proportions of key characteristics

props <- data |>
  summarise(northeast = mean(Northeast),
            north_central = mean(North.Central),
            south = mean(South),
            west = mean(West),
            female = mean(female),
            younger = mean(age <= 30),
            older = mean(age > 55),
            low = mean(educ.low, na.rm = TRUE),
            advanced = mean(education > 'B.A.', na.rm = TRUE),
            complex = mean(ncog1 > 'no preference', na.rm = TRUE),
            responsibility = mean(ncog6>'neither like nor dislike', na.rm = TRUE),
            both = mean(ncog6 >'neither like nor dislike'  & ncog1 >'no preference', na.rm = TRUE),
            cheney = mean(gk.Cheney)) |>
  bind_rows(respondents |>
              summarise(northeast = mean(Northeast),
                        north_central = mean(North.Central),
                        south = mean(South),
                        west = mean(West),
                        female = mean(female),
                        younger = mean(age <= 30),
                        older = mean(age > 55),
                        low = mean(educ.low, na.rm = TRUE),
                        advanced = mean(education > 'B.A.', na.rm = TRUE),
                        complex = mean(ncog1 > 'no preference', na.rm = TRUE),
                        responsibility = mean(ncog6>'neither like nor dislike', na.rm = TRUE),
                        both = mean(ncog6 >'neither like nor dislike'  & ncog1 >'no preference',
                                    na.rm = TRUE),
                        cheney = mean(gk.Cheney))) |>
  t() |>
  as.data.frame()

names(props) <- c("all", "subset")

props |>
  kable(digits = 3) |>
  kableExtra::kable_classic(full_width = F)

#test for the two groups
data_for_test <- data |>
  mutate(subset = as.numeric(gk.Cheney.original %in% levels(data$gk.Cheney.original)[c(16, 19, 23, 31, 43, 44, 45, 46, 47, 57, 93, 150, 152, 153, 171, 200, 201, 205, 206, 209, 210, 211, 237, 238, 239, 241, 272, 275, 285)])) |>
  select(subset,
         Northeast,
         North.Central,
         South,
         West,
         female,
         age,
         educ.low,
         ncog1,
         ncog6,
         gk.Cheney) |>
  mutate(across(c(ncog1, ncog6), as.numeric))

modelsummary::modelsummary(lm(subset ~ ., data = data_for_test),
                           stars = TRUE,
                           vcov = "HC2") |>
  kable_classic()
