# Data Analysis - Oct 4
# Maria Francesca Arruda de Amaral
researchers <- readRDS("data/petersen_data_researchers.rds")

#fix column types
researchers$department <- as.character(researchers$department) # 1 econometrics, 2 sociology, 3 medicine
researchers$trust_factor <- as.character(researchers$trust_factor) # 1 education, 2 income, 3 weight,
researchers$position <- as.character(researchers$position) # 1 PhD, 2 Postdoc, 3 Adjunct, 4 Lecturer, 5 Prof, 6 Other
researchers$sex <- as.character(researchers$sex) # 1 Man, 2 Woman

#detect what was the top factor
top_factor_by_row <- colnames(researchers[, c("importance_mom_education",
                                              "importance_mom_income",
                                              "importance_birth_weight")])[apply(researchers[, c("importance_mom_education","importance_mom_income","importance_birth_weight")],1,which.max)]

#table depts and rank to find out matching given supp material
table(researchers$department) #1 = econometrics, 2 = sociology, 3 = clinical medicine
table(researchers$position)/nrow(researchers)*100 #1 grad student, 2+3 = asst prof + postdoc, #4 associate prof, #5 prof, #6 other

library(tidytable)

#recode to be 0-1, add dept and rank flag
researchers <- researchers |>
  mutate(across(starts_with("importance"),
                function(x) {x/100}),
         top_factor = top_factor_by_row,
         trust_factor = case_when(trust_factor == "1" ~ "importance_mom_education",
                                  trust_factor == "2" ~ "importance_mom_income",
                                  trust_factor == "3" ~ "importance_birth_weight"),
         department = case_when(department == 1 ~ "econometrics",
                                department == 2 ~ "sociology",
                                department == 3 ~ "clin_med"),
         position = case_when(position == "1" ~ "grad_student",
                              position == "2" ~ "post_doc",
                              position == "3" ~ "asst_prof",
                              position == "4" ~ "assoc_prof",
                              position == "5" ~ "prof",
                              position == "6" ~ "other"),
         sex = ifelse(sex == "1", "Man", "Woman"),
         age = 2013 - as.numeric(birth_year))

#not sure why the "trust factor" column doesn't match, but with recoding, we have:
table(researchers$top_factor, researchers$trust_factor)

table(researchers$department, researchers$top_factor)

weight_importance_continuous <- lm(importance_birth_weight ~ department + position + age + sex, data = researchers)

#recode for binomial
researchers$top_is_bw <- researchers$top_factor == "importance_birth_weight"

weight_binary <- glm(top_is_bw ~ department + position + age + sex, family = "binomial", data = researchers)

#get tables
modelsummary::modelsummary(list("Importance of Birth weight - OLS" = weight_importance_continuous,
                                "Birth weight as top factor - Logit" = weight_binary),
                           vcov = "HC2",
                           exponentiate = c(FALSE, TRUE),
                           stars = TRUE)
