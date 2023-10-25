# Data Analysis - Oct 25
# Maria Francesca Arruda de Amaral

library(tidytable)
library(kableExtra)

#load data
mehrez <- read.csv("data/mehrez_data.csv")

colMeans(is.na(mehrez))

#fix vote variables

#parliament (vote1)
##Category 1 = Islamists
## Category 2 = Center-left
## Category 3 = Secular-Nationalists
## Category 4 = Social Democrats
## Category 5 = Independents
## Category 6 = Did not vote
## Category 7 = DK/refuse/blank vote

#president (vote2)
#category 1 = Right-wing
#category 2 = Center-left
#category 3 = secular-nationalist
#category 4 = social democrats
#category 5 = Independents
#category 6 = did not vote
#category 7 = DK/ refuse to answer/ Blank

mehrez <- mehrez |>
  mutate(Q51 = if_else(Q51 == 9, Q51other, Q51),
         Q53 = if_else(Q53 == 13, Q53other, Q53)) |>
  mutate(vote1 = car::recode(Q51, "1=1; 4=1; 10=1; 23=1;
                   2=2; 3=4;6=4;14=4;21=4;
                   5=3;7=3;8=3;13=3;17=3;18=3;20=3;24=3;
                   12=5; 15=5; 16=5; 22=5; 25=5; 26=5; 27=5;
                   NA= 6; 19=6;
                   11=7; 96=7; 98=7; 99=7"),
         vote2 = car::recode(Q53, "7=1; 11=1; 15=1; 18=1; 2=1;
                   12=2;
                   1=3;6=3;9=3;17=3; 14=3;
                   NA=6; 5=4;4=4;
                   10=5; 3=5; 16=5; 8=5;
                   96=7; 98=7; 99=7")) |>
  mutate(nonvoter_parl = vote1 %in% 6:7,
         nonvoter_pres = vote2 %in% 6:7)

#check whether voters vs non-voters are similar

nonvoter_parl <- glm(nonvoter_parl ~ Income + Gender + Prayer + SI + Milieu + Age,
   data = mehrez, family = "binomial")
nonvoter_pres <- glm(nonvoter_pres ~ Income + Gender + Prayer + SI + Milieu + Age,
                     data = mehrez, family = "binomial")

modelsummary::modelsummary(list("Parliament" = nonvoter_parl,
                                "President" = nonvoter_pres),
                           vcov = "HC2",
                           exponentiate = TRUE,
                           stars = TRUE) |>
  kable_classic()

#check Kalmoe's concerns
 #how many people in periphery?

# First, get the scores on the two measures
# For now, trust that the factors picked by Mehrez are correct (but see below)

#Q11 Whether or not someone's action showed love for his or her country
#Q12 Whether or not someone showed a lack of respect for authority
#Q18 Whether or not someone conformed to the traditions of society
#Q29 I am proud of my country's history
#Q30 Respect for authority is something all children need to learn
#Q36 Men and women each have different roles to play in society
#Q41 If I were a soldier and disagreed with my commanding officer's orders, I would obey anyway because that is my duty
#Q25 Whether or not private property was respected
#Q26 Whether or not everyone was free to do as they wanted
#Q10 Whether or not some people were treated differently from others
#Q21 Whether or not someone was denied his or her rights
#Q16 Whether or not someone acted unfairly
#Q39 I think it's morally wrong that rich children inherit a lot of money while poor children inherit nothing
#Q34 Justice is the most important requirement for a society

###Take the mean out the three authority-nationalism items
mehrez$auth_nat <- rowMeans(mehrez[ , c("Q29", "Q30","Q41")], na.rm=TRUE)

###Take the mean out the five liberty-justice items
mehrez$lib_jus <- rowMeans(mehrez[ , c("Q25","Q26","Q10","Q21", "Q16")], na.rm=TRUE)

#without info on political knowledge, just do histograms
hist(mehrez$auth_nat, main = "Authority-Nationalism")
abline(v=mean(mehrez$auth_nat, na.rm = TRUE), col='red', lwd=3, lty='dashed')

hist(mehrez$lib_jus, main = "Liberty-Justice")
abline(v=mean(mehrez$lib_jus, na.rm = TRUE), col='red', lwd=3, lty='dashed')

#now, look for item consistency
#first show the factor loadings obtained

factors <- mehrez |>
  select("Q11", "Q12", "Q18", "Q29", "Q30", "Q36", "Q41",
         "Q25", "Q26", "Q10", "Q21", "Q16", "Q39", "Q34")
EFA <- psych::fa(factors, nfactors=2, rotate="oblimin", fm="mle")

#CFI
((EFA$null.chisq-EFA$null.dof)-(EFA$STATISTIC-EFA$dof))/(EFA$null.chisq-EFA$null.dof)

#Used fm="mle" instead
unclass(EFA$loadings) |>
  kable(digits = 3) |>
  add_footnote("CFI = 0.928; TLI = 0.898; RMSEA = 0.039") |>
  column_spec(2, bold = unclass(EFA$loadings)[, "ML1"] > 0.4) |>
  column_spec(3, bold = unclass(EFA$loadings)[, "ML2"] > 0.4) |>
  kableExtra::kable_classic(full_width = F)

#get alphas for whole sample, then voters vs nonvoters
get_alpha <- function(scale, nonvoter = c(TRUE, FALSE)) {
  scale <- as.data.frame(scale)
  scale <- scale[scale[,"nonvoter_pres"] %in% nonvoter,]
  psych::alpha(scale[, -1])$total$std.alpha
}

get_avg_r <- function(scale, nonvoter = c(TRUE, FALSE)) {
  scale <- as.data.frame(scale)
  scale <- scale[scale[,"nonvoter_pres"] %in% nonvoter,]
  psych::alpha(scale[, -1])$total$average_r
}

alpha_nat_auth <- map_dbl(list(c(TRUE, FALSE), TRUE, FALSE),
        function(x) get_alpha(mehrez[, c("nonvoter_pres",
                                         "Q29", "Q30","Q41")], x))

avg_r_nat_auth <- map_dbl(list(c(TRUE, FALSE), TRUE, FALSE),
                          function(x) get_avg_r(mehrez[, c("nonvoter_pres",
                                                           "Q29", "Q30","Q41")], x))

alpha_lib_jus <- map_dbl(list(c(TRUE, FALSE), TRUE, FALSE),
                          function(x) get_alpha(mehrez[, c("nonvoter_pres",
                                                           "Q25","Q26","Q10","Q21", "Q16")], x))

avg_r_lib_jus <- map_dbl(list(c(TRUE, FALSE), TRUE, FALSE),
                          function(x) get_avg_r(mehrez[, c("nonvoter_pres",
                                                           "Q25","Q26","Q10","Q21", "Q16")], x))

names(alpha_nat_auth) <- names(avg_r_nat_auth) <- names(alpha_lib_jus) <- names(avg_r_lib_jus) <- c("all", "nonvoter", "voter")

alpha_nat_auth |>
  as.list() |>
  as.data.frame() |>
  pivot_longer(names_to = "voting",
               values_to = "alpha_nat_auth") |>
  left_join(avg_r_nat_auth |>
              as.list() |>
              as.data.frame() |>
              pivot_longer(names_to = "voting",
                           values_to = "avg_r_nat_auth")) |>
  left_join(alpha_lib_jus |>
              as.list() |>
              as.data.frame() |>
              pivot_longer(names_to = "voting",
                           values_to = "alpha_lib_jus")) |>
  left_join(avg_r_lib_jus |>
              as.list() |>
              as.data.frame() |>
              pivot_longer(names_to = "voting",
                           values_to = "avg_r_lib_jus")) |>
  kable(digits = 3) |>
  add_footnote("Voting status is for presidential elections") |>
  kableExtra::kable_classic(full_width = F)
