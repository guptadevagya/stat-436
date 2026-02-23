library(tidyverse)
library(janitor)

# Load data
olympics <- read_csv("https://uwmadison.box.com/shared/static/rzw8h2x6dp5693gdbpgxaf2koqijo12l.csv") %>%
  clean_names()

# ----------------------------
# (a) birth_city + birth_country
# ----------------------------
olympics2 <- olympics %>%
  mutate(
    birth_city = if_else(
      is.na(place_of_birth),
      NA_character_,
      str_trim(str_remove(place_of_birth, "\\s*\\([^)]*\\)\\s*$"))
    ),
    birth_country = if_else(
      is.na(place_of_birth),
      NA_character_,
      str_extract(place_of_birth, "(?<=\\().*(?=\\))")
    )
  )

# ANSWER (a): show proof it worked
olympics2 %>%
  select(name, place_of_birth, birth_city, birth_country) %>%
  head(10)

# ----------------------------
# (b) SD of age within each sport + widest SD
# ----------------------------
age_sd_by_sport <- olympics2 %>%
  filter(!is.na(age), !is.na(sport)) %>%
  group_by(sport) %>%
  summarize(
    sd_age = sd(age),
    n = n(),
    .groups = "drop"
  ) %>%
  arrange(desc(sd_age))

# ANSWER (b): table (top few) + the sport with the widest SD
age_sd_by_sport %>% head(10)

widest <- age_sd_by_sport %>% slice(1)
widest   # this prints the sport name + SD (this is your answer)

# ----------------------------
# (c) Plot: age vs sport ordered by SD
# (simple but cleaner than what you have)
# ----------------------------
sport_order <- age_sd_by_sport %>% arrange(sd_age) %>% pull(sport)

plot_df <- olympics2 %>%
  filter(!is.na(age), !is.na(sport)) %>%
  mutate(sport_sorted = factor(sport, levels = sport_order))

ggplot(plot_df, aes(x = sport_sorted, y = age)) +
  geom_boxplot(width = 0.6, outlier.alpha = 0.08) +
  coord_flip() +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 22)) +
  labs(
    title = "Athlete age by sport (London 2012)",
    subtitle = "Ordered by within-sport age SD",
    x = NULL,
    y = "Age"
  ) +
  theme_bw() +
  theme(
    axis.text.y = element_text(size = 9),
    plot.title = element_text(face = "bold", size = 14),
    panel.grid.minor = element_blank()
  )