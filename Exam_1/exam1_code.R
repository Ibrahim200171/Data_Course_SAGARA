install.packages(c("tidyverse","lubridate","forcats"))
# ---- Setup ----
library(tidyverse)
library(lubridate)
library(forcats)

if (!dir.exists("figs")) dir.create("figs")

# ---- I) Read data ----
df <- read.csv("cleaned_covid_data.csv", stringsAsFactors = FALSE)
df$Last_Update <- ymd(df$Last_Update)

# ---- II) States that start with "A" ----
A_states <- df %>% filter(str_starts(Province_State, "A"))

# ---- III) Deaths over time, faceted ----
p1 <- ggplot(A_states, aes(Last_Update, Deaths)) +
  geom_point(alpha = 0.6, size = 0.8) +
  geom_smooth(method = "loess", se = FALSE, linewidth = 0.6, color = "steelblue") +
  facet_wrap(~ Province_State, scales = "free_y") +
  labs(title = "Deaths over Time (States beginning with 'A')",
       x = "Date", y = "Deaths") +
  theme_minimal(base_size = 11)
ggsave("figs/task3_deaths_over_time_A_states.png", p1, width = 10, height = 6, dpi = 300)

# ---- IV) Peak fatality ratio per state ----
state_max_fatality_rate <- df %>%
  group_by(Province_State) %>%
  summarise(Maximum_Fatality_Ratio = max(Case_Fatality_Ratio, na.rm = TRUE),
            .groups = "drop") %>%
  mutate(Maximum_Fatality_Ratio = ifelse(is.infinite(Maximum_Fatality_Ratio),
                                         NA_real_, Maximum_Fatality_Ratio)) %>%
  drop_na(Maximum_Fatality_Ratio) %>%
  arrange(desc(Maximum_Fatality_Ratio))

# ---- V) Bar plot ordered by peak CFR ----
plot_df <- state_max_fatality_rate %>%
  mutate(Province_State = fct_reorder(Province_State,
                                      Maximum_Fatality_Ratio, .desc = TRUE))

p2 <- ggplot(plot_df, aes(Province_State, Maximum_Fatality_Ratio)) +
  geom_col() +
  coord_cartesian(clip = "off") +
  theme_minimal(base_size = 11) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
  labs(title = "Peak Case Fatality Ratio by State",
       x = "Province_State", y = "Maximum_Fatality_Ratio")
ggsave("figs/task5_peak_cfr_barplot.png", p2, width = 11, height = 7, dpi = 300)

# ---- VI) BONUS: US cumulative deaths over time ----
us_daily <- df %>%
  group_by(Last_Update) %>%
  summarise(daily_deaths_US = sum(Deaths, na.rm = TRUE), .groups = "drop") %>%
  arrange(Last_Update) %>%
  mutate(cum_deaths_US = cumsum(daily_deaths_US))

p_bonus <- ggplot(us_daily, aes(Last_Update, cum_deaths_US)) +
  geom_line(linewidth = 0.6, color = "firebrick") +
  theme_minimal(base_size = 11) +
  labs(title = "Cumulative US Deaths Over Time",
       x = "Date", y = "Cumulative deaths (US)")
ggsave("figs/bonus_US_cumulative_deaths.png", p_bonus, width = 10, height = 6, dpi = 300)

save(A_states, state_max_fatality_rate, file = "exam1_objects.RData")
message("Done. Check the figs/ folder.")
