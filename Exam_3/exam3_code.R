# Exam 3 Script
# Your professor will open this project and run this file.

library(tidyverse)

# 1. Load data from the course Data folder
grad <- read_csv("../Data/GradSchool_Admissions.csv")

# 2. Clean / label variables
grad <- grad %>%
  mutate(
    admit = factor(admit, levels = c(0, 1),
                   labels = c("Rejected", "Admitted")),
    rank = factor(rank)
  )

# 3. Look at the data
print("First rows of the data:")
print(head(grad))

# 4. Overall admission rates
overall_rates <- grad %>%
  count(admit) %>%
  mutate(prop = n / sum(n))

print("Overall admission rates:")
print(overall_rates)

# 5. Admission rate by prestige rank of school
rank_rates <- grad %>%
  count(rank, admit) %>%
  group_by(rank) %>%
  mutate(prop = n / sum(n))

print("Admission rates by rank of undergraduate institution:")
print(rank_rates)

# 6. Plot: GPA vs GRE colored by admission outcome
ggplot(grad, aes(x = gre, y = gpa, color = admit)) +
  geom_point(alpha = 0.6) +
  labs(
    title = "GPA and GRE by Admission Outcome",
    x = "GRE score",
    y = "GPA"
  )