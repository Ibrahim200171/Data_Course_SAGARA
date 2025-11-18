##############################
# BIOL 3100 Exam 2 - LASTNAME
##############################

library(tidyverse)

#------------------------------
# 1. Read in UNICEF data
#------------------------------

# IMPORTANT:
# Replace "YOUR_CSV_FILE_NAME_HERE.csv" with the actual csv name
# once you can see it in the Files pane or from the GitHub Exam_2 folder.
unicef_raw <- read_csv("YOUR_CSV_FILE_NAME_HERE.csv")

# Take a quick look (helps you adjust column names if needed)
glimpse(unicef_raw)
names(unicef_raw)

# I will assume the columns are something like:
#   country, continent (or region), and many year columns (e.g. 1950, 1955,...)
# Adjust "country" and "continent" below if your names are different.

#------------------------------
# 2. Make the data tidy
#------------------------------

# Identify which columns are the years.
# If your year columns are literally "1950", "1951", etc. this should work:
year_cols <- names(unicef_raw)[grepl("^\\d{4}$", names(unicef_raw))]

# If instead they look like "X1950", "X1951", etc., comment the line above
# and use this instead:
# year_cols <- names(unicef_raw)[grepl("^X\\d{4}$", names(unicef_raw))]

unicef_tidy <- unicef_raw %>%
  pivot_longer(
    cols = all_of(year_cols),
    names_to = "year",
    values_to = "u5mr"
  ) %>%
  mutate(
    # If you used "X1950" style, strip the X first:
    # year = str_remove(year, "^X"),
    year = as.integer(year)
  )

glimpse(unicef_tidy)

# At this point you want columns like:
# country | continent | year | u5mr

#------------------------------
# 3. Plot each country's U5MR over time
#------------------------------

p1 <- ggplot(unicef_tidy,
             aes(x = year, y = u5mr,
                 group = country,
                 colour = continent)) +   # change "continent" if your column name differs
  geom_line(alpha = 0.5, linewidth = 0.4) +
  theme_minimal() +
  labs(
    title = "Under-5 Mortality Rate by Country Over Time",
    x = "Year",
    y = "U5MR (deaths per 1,000 live births)",
    colour = "Continent"
  )

# Save plot 1 (replace LASTNAME with your actual last name)
ggsave("LASTNAME_Plot_1.png", p1, width = 8, height = 5, dpi = 300)

#------------------------------
# 4. Mean U5MR by continent and year
#------------------------------

u5_continent_year <- unicef_tidy %>%
  group_by(continent, year) %>%           # adjust "continent" name if needed
  summarise(
    mean_u5mr = mean(u5mr, na.rm = TRUE),
    .groups = "drop"
  )

p2 <- ggplot(u5_continent_year,
             aes(x = year, y = mean_u5mr, colour = continent)) +
  geom_line(linewidth = 1) +
  theme_minimal() +
  labs(
    title = "Mean U5MR by Continent Over Time",
    x = "Year",
    y = "Mean U5MR (per 1,000 live births)",
    colour = "Continent"
  )

ggsave("LASTNAME_Plot_2.png", p2, width = 8, height = 5, dpi = 300)

#------------------------------
# 5. Three models for U5MR (example: Ecuador)
#------------------------------

# If your country column is named something else, change "country"
ecuador <- unicef_tidy %>%
  filter(country == "Ecuador")

# Check it
glimpse(ecuador)

# Model 1: simple linear
m1 <- lm(u5mr ~ year, data = ecuador)

# Model 2: quadratic polynomial in year
m2 <- lm(u5mr ~ poly(year, 2), data = ecuador)

# Model 3: model log(U5MR) vs year (approx exponential)
m3 <- lm(log(u5mr) ~ year, data = ecuador)

# Compare model summaries (R^2, etc.)
summary(m1)
summary(m2)
summary(m3)

# AIC comparison
AIC(m1, m2, m3)

# RMSE comparison (optional but nice)
ecuador_preds <- ecuador %>%
  mutate(
    pred_m1 = predict(m1),
    pred_m2 = predict(m2),
    pred_m3 = exp(predict(m3))  # back-transform from log scale
  )

rmse_m1 <- sqrt(mean((ecuador_preds$u5mr - ecuador_preds$pred_m1)^2))
rmse_m2 <- sqrt(mean((ecuador_preds$u5mr - ecuador_preds$pred_m2)^2))
rmse_m3 <- sqrt(mean((ecuador_preds$u5mr - ecuador_preds$pred_m3)^2))

rmse_m1
rmse_m2
rmse_m3

# In your own words (in comments or separate write-up),
# you can say which model you prefer (e.g. smallest AIC / RMSE).

#------------------------------
# 6. Plot the 3 models' predictions
#------------------------------

ecuador_long_preds <- ecuador_preds %>%
  select(year, u5mr, pred_m1, pred_m2, pred_m3) %>%
  pivot_longer(
    cols = starts_with("pred_"),
    names_to = "model",
    values_to = "predicted_u5mr"
  )

p_models <- ggplot() +
  geom_point(data = ecuador,
             aes(x = year, y = u5mr),
             size = 2) +
  geom_line(data = ecuador_long_preds,
            aes(x = year, y = predicted_u5mr, colour = model),
            linewidth = 1) +
  theme_minimal() +
  labs(
    title = "Observed vs Predicted U5MR for Ecuador",
    x = "Year",
    y = "U5MR (per 1,000 live births)",
    colour = "Model"
  )

# You can optionally save this too:
# ggsave("LASTNAME_Models.png", p_models, width = 8, height = 5, dpi = 300)

#------------------------------
# 7. BONUS – Predict Ecuador 2020
#------------------------------

# Choose your preferred model (example: m2 or m3)
new_2020 <- tibble(year = 2020)

# Example using m2:
pred_2020_m2 <- predict(m2, newdata = new_2020)
pred_2020_m2

# If you prefer m3 (log model):
pred_2020_m3 <- exp(predict(m3, newdata = new_2020))
pred_2020_m3

# Real value given: 13 deaths / 1000 live births
real_2020 <- 13

error_m2  <- pred_2020_m2 - real_2020
abs_error_m2 <- abs(error_m2)

error_m3  <- pred_2020_m3 - real_2020
abs_error_m3 <- abs(error_m3)

error_m2
abs_error_m2
error_m3
abs_error_m3

# In your comments/write-up, you can say, e.g.:
# "Using Model 2, the predicted 2020 U5MR for Ecuador was XX,
# which is YY away from the true value of 13."

