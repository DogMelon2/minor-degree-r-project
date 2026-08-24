# Load libraries
if (!require("ggplot2")) install.packages("ggplot2")
if (!require("dplyr")) install.packages("dplyr")
if (!require("ggrepel")) install.packages("ggrepel")
library(ggplot2)
library(dplyr)
library(ggrepel)

# 1. Load data
ev_sales <- read.csv("data/raw/electric-car-sales-share.csv")
colnames(ev_sales)[4] <- "EV_Share"

ev_world <- ev_sales %>% 
  filter(Entity == "World", Year >= 2010) %>%
  mutate(
    Conflict_Event = case_when(
      Year == 2011 ~ "Arab Spring & Iran Sanctions",
      Year == 2019 ~ "Strait of Hormuz Tanker Incidents",
      Year == 2022 ~ "Russia-Ukraine Energy Shock",
      Year == 2024 ~ "Middle East Crisis & Red Sea Disruptions",
      TRUE ~ NA_character_
    )
  )

# 2. Build plot with ggrepel for text alignment
p_geo <- ggplot(ev_world, aes(x = Year, y = EV_Share)) +
  geom_col(fill = "#2b5c8f", alpha = 0.85) +
  geom_point(data = filter(ev_world, !is.na(Conflict_Event)),
             aes(x = Year, y = EV_Share), color = "#d95f02", size = 3.5) +
  geom_text_repel(
    data = filter(ev_world, !is.na(Conflict_Event)),
    aes(x = Year, y = EV_Share, label = Conflict_Event),
    nudge_y = 6,
    angle = 0,
    direction = "y",
    fontface = "bold",
    size = 3,
    segment.color = "#d95f02",
    segment.size = 0.5
  ) +
  scale_y_continuous(limits = c(0, max(ev_world$EV_Share) + 12)) +
  theme_minimal() +
  labs(
    title = "Geopolitical Conflicts, Energy Security, and EV Adoption Growth",
    subtitle = "High-stress conflict milestones accelerating policy pushes toward electrification",
    x = "Year",
    y = "Global EV Market Share (%)"
  ) +
  theme(plot.title = element_text(face = "bold", size = 13))

# 3. Save output
if (!dir.exists("output/figures")) dir.create("output/figures", recursive = TRUE)
ggsave("output/figures/04_geopolitical_shocks.png", plot = p_geo, width = 9.5, height = 6)
cat("=== Success! Cleaned plot saved to output/figures/04_geopolitical_shocks.png ===\n")