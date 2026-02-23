library(tidyverse)

# 1. Load data
plants <- read_csv("https://uwmadison.box.com/shared/static/qg9gwk2ldjdtcmmmiropcunf34ddonya.csv", 
                   show_col_types = FALSE)

# 2. Tidy data 
# We rename by position: 1st column is ID, 2nd is Treatment
plants_tidy <- plants %>%
  rename(plant_id = 1, treatment_group = 2) %>% 
  pivot_longer(
    cols = starts_with("height"),
    names_to = "day",
    names_prefix = "height.",
    values_to = "height"
  ) %>%
  mutate(day = as.numeric(day))

# 3. Generate the plot
p <- ggplot(plants_tidy, aes(x = day, y = height, color = factor(treatment_group))) +
  # Individual plant lines
  geom_line(aes(group = plant_id), alpha = 0.3) + 
  # Bold average line
  stat_summary(fun = mean, geom = "line", linewidth = 1.2) +
  geom_point(alpha = 0.5) +
  facet_wrap(~treatment_group) +
  theme_classic() +
  labs(
    title = "Plant Growth: Individual Samples vs. Group Average",
    x = "Day",
    y = "Height (cm)",
    color = "Treatment"
  )
print(p)