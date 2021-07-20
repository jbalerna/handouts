# Linear models

library(readr)
library(dplyr)

pums <- read_csv(
  file = 'data/census_pums/sample.csv',
  col_types = cols_only(
    ... = 'i',  # Age
    ... = 'd',  # Wages or salary income past 12 months
    ... = 'i',  # Educational attainment
    ... = 'f',   # Sex
    ... = 'f', # Marital status
    ... = 'f', # has insurance through employer
    ... = 'i')) # Usual hours worked per week past 12 months

pums <- within(pums, {
  SCHL <- factor(SCHL)
  ...(SCHL) <- list(
    'Incomplete' = c(1:15),
    'High School' = 16:17,
    'College Credit' = 18:20,
    'Bachelor\'s' = 21,
    'Master\'s' = 22:23,
    'Doctorate' = 24)}) %>%
  ...(
    WAGP > 0,
    WAGP < max(WAGP, na.rm = TRUE))

# Linear regression

fit <- lm(
  formula = ...,
  data = ...)

library(ggplot2)

ggplot(pums,
       aes(x = ..., y = ...)) +
  geom_...()

fit.schl <- lm(
  ...,
  data = pums)

# Predictor class

fit.agep <- lm(
  ...,
  data = pums)

summary(...)

# GLM families

fit <- ...(...,
  ...,
  data = pums)

summary(fit)

# Logistic Regression

fit <- glm(...,
  family = ...,
  data = pums)

...(pums$HINS1)

pums$... <- factor(pums$HINS1, ... = c("2", "1"))
levels(pums$HINS1)

fit <- glm(...,
           family = ...,
           data = pums)

...(fit, update(fit, ...), test = 'Chisq')

# Random Intercept

library(...)
fit <- ...(
  ...,
  data = pums)

... <- lmer(
  ...,
  data = pums)

...(null.model, fit)

# Random Slope

fit <- lmer(
  ...,
  data = pums)

fit <- lmer(
  log(WAGP) ~ (WKHP | SCHL),
  data = pums,
  ... = lmerControl(... = "bobyqa"))

ggplot(pums,
  aes(x = ..., y = ..., color = ...)) +
  geom_point() +
  geom_line(...) +
  labs(title = 'Random intercept and slope with lmer')
