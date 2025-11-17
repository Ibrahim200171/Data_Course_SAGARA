###############################################
# Assignment 8 – Mushroom Growth Modeling
# Ibrahim Michael Sagara
###############################################

library(tidyverse)
library(modelr)
library(broom)
library(easystats)   # you already installed this

# ---------------------------------------------------------
# 1. Load and clean data
# ---------------------------------------------------------

mush <- read_csv("Data/mushroom_growth.csv")

# Treat Species and Humidity as categorical variables
mush <- mush %>%
  mutate(
    Species  = factor(Species),
    Humidity = factor(Humidity)
  )

glimpse(mush)

# ---------------------------------------------------------
# 2. Exploratory plots
# ---------------------------------------------------------

# GrowthRate vs Light
ggplot(mush, aes(x = Light, y = GrowthRate)) +
  geom_point() +
  geom_smooth(method = "lm") +
  theme_minimal()

# GrowthRate vs Nitrogen
ggplot(mush, aes(x = Nitrogen, y = GrowthRate)) +
  geom_point() +
  geom_smooth(method = "lm") +
  theme_minimal()

# GrowthRate vs Humidity
ggplot(mush, aes(x = Humidity, y = GrowthRate)) +
  geom_point() +
  theme_minimal()

# GrowthRate vs Temperature
ggplot(mush, aes(x = Temperature, y = GrowthRate)) +
  geom_point() +
  geom_smooth(method = "lm") +
  theme_minimal()

# Boxplot: Species differences
ggplot(mush, aes(x = Species, y = GrowthRate)) +
  geom_boxplot() +
  theme_minimal()

# ---------------------------------------------------------
# 3. Define at least 4 models
# ---------------------------------------------------------

mse <- function(m) mean(residuals(m)^2)

mod1 <- lm(GrowthRate ~ Light, data = mush)

mod2 <- lm(GrowthRate ~ Light + Nitrogen, data = mush)

mod3 <- lm(GrowthRate ~ Light + Nitrogen + Temperature, data = mush)

mod4 <- lm(GrowthRate ~ Light + Nitrogen + Temperature +
             Humidity + Species, data = mush)

# ---------------------------------------------------------
# 4. Compare models with mean squared error
# ---------------------------------------------------------

model_compare <- tibble(
  model = c("mod1", "mod2", "mod3", "mod4"),
  mse   = c(mse(mod1), mse(mod2), mse(mod3), mse(mod4))
)

print(model_compare)

# Pick best model (smallest MSE)
best_name <- model_compare$model[which.min(model_compare$mse)]
best_name

best_mod <- get(best_name)
summary(best_mod)

# Optional English summary
report(best_mod)

# ---------------------------------------------------------
# 5. Add predictions for actual data
# ---------------------------------------------------------

mush_pred <- mush %>%
  add_predictions(best_mod) %>%
  mutate(Type = "Real")

head(mush_pred)

# ---------------------------------------------------------
# 6. Hypothetical predictions
# ---------------------------------------------------------

# Check levels so we match them correctly
levels(mush$Humidity)
levels(mush$Species)

hypothetical <- tibble(
  Light       = c(20, 40, 60),
  Nitrogen    = c(1,  2,  3),
  Temperature = c(20, 25, 30),
  Humidity    = factor(c("Low", "Medium", "High"),
                       levels = levels(mush$Humidity)),
  Species     = factor(c("P.ostreotus",
                         "P.ostreotus",
                         "P.cornucopiae"),
                       levels = levels(mush$Species))
)

hypothetical$pred <- predict(best_mod, newdata = hypothetical)
hypothetical

# ---------------------------------------------------------
# 7. Plot real vs hypothetical predictions
# ---------------------------------------------------------

hypothetical_plot <- hypothetical %>%
  mutate(GrowthRate = NA_real_, Type = "Hypothetical")

fullpreds <- bind_rows(
  mush_pred %>% select(Light, Nitrogen, Temperature,
                       Humidity, Species, GrowthRate, pred, Type),
  hypothetical_plot %>% select(Light, Nitrogen, Temperature,
                               Humidity, Species, GrowthRate, pred, Type)
)

ggplot(fullpreds, aes(x = Light, y = pred, color = Type)) +
  geom_point(size = 3) +
  geom_point(data = mush_pred,
             aes(x = Light, y = GrowthRate),
             inherit.aes = FALSE,
             color = "black", alpha = 0.5) +
  theme_minimal()

# ---------------------------------------------------------
# 8. Non-linear example (for the Canvas question 3)
# ---------------------------------------------------------

nonlin <- read_csv("Data/non_linear_relationship.csv")

nonlin <- nonlin %>%
  rename(
    x = predictor,
    y = response
  ) %>%
  mutate(x2 = x^2)

mod_nonlin <- lm(y ~ x + x2, data = nonlin)
summary(mod_nonlin)