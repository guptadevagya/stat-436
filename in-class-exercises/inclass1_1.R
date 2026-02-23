# stat 436 in class 1

library(tidyverse)

# folder
dir <- "datasets/formula1"

# load data
r22 <- read_csv(paste0(dir, "/Formula1_2022season_raceResults.csv"), show_col_types = FALSE)
r23 <- read_csv(paste0(dir, "/Formula1_2023season_raceResults.csv"), show_col_types = FALSE)
r24 <- read_csv(paste0(dir, "/Formula1_2024season_raceResults.csv"), show_col_types = FALSE)

# add year
r22$year <- 2022
r23$year <- 2023
r24$year <- 2024

# combine
race <- bind_rows(r22, r23, r24) %>%
  select(year, Team, Position)

# helper for simple matching
has <- function(x, s) grepl(s, x, fixed = TRUE)

# manual team names
race <- race %>%
  mutate(team = case_when(
    has(Team, "Red Bull") ~ "Red Bull",
    has(Team, "AlphaTauri") ~ "RB",
    has(Team, "Visa Cash App") ~ "RB",
    has(Team, "VCARB") ~ "RB",
    has(Team, "Kick Sauber") ~ "Sauber",
    has(Team, "Sauber") ~ "Sauber",
    has(Team, "Alfa Romeo") ~ "Sauber",
    has(Team, "Aston Martin") ~ "Aston Martin",
    has(Team, "McLaren") ~ "McLaren",
    has(Team, "Alpine") ~ "Alpine",
    has(Team, "Williams") ~ "Williams",
    has(Team, "Haas") ~ "Haas",
    has(Team, "Ferrari") ~ "Ferrari",
    has(Team, "Mercedes") ~ "Mercedes",
    TRUE ~ Team
  ))

# convert position to number (dnf becomes NA)
race <- race %>%
  mutate(pos = suppressWarnings(as.numeric(Position)))

# podium flag
race <- race %>%
  mutate(pod = if_else(!is.na(pos) & pos <= 3, 1, 0))

# podium rate by team and year
sum <- race %>%
  group_by(year, team) %>%
  summarise(
    n = sum(!is.na(pos)),
    pod = sum(pod),
    rate = pod / n,
    .groups = "drop"
  )

# keep major teams only (easy and readable)
keep <- c("Red Bull", "Ferrari", "Mercedes", "McLaren", "Aston Martin", "Alpine")
sum <- sum %>% filter(team %in% keep)

# team colors (real-ish)
col <- c(
  "Red Bull" = "#1E41FF",
  "Ferrari" = "#DC0000",
  "Mercedes" = "#00D2BE",
  "McLaren" = "#FF8700",
  "Aston Martin" = "#006F62",
  "Alpine" = "#0090FF"
)

# plot
p <- ggplot(sum, aes(x = factor(year), y = rate, fill = team)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.7) +
  scale_fill_manual(values = col) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(
    title = "Podium rate by team, 2022–2024",
    x = "Season",
    y = "Podium rate",
    fill = "Team"
  ) +
  theme_minimal() +
  theme(
    text = element_text(size = 14),
    plot.title = element_text(face = "bold"),
    legend.position = "right"
  )

print(p)
ggsave("f1_podium_rate.png", p, width = 10, height = 6)