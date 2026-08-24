# Load libraries
if (!require("ggplot2")) install.packages("ggplot2")
if (!require("dplyr")) install.packages("dplyr")
library(ggplot2)
library(dplyr)

# 1. Load data
oil_annual <- read.csv("data/raw/crude-oil-prices.csv")
ev_sales   <- read.csv("data/raw/electric-car-sales-share.csv")

# 2. Extract and clean metrics
colnames(ev_sales)[4]   <- "EV_Share"
colnames(oil_annual)[4] <- "Oil_Price"

ev_world <- ev_sales %>%
  filter(Entity == "World") %>%
  select(Entity, Year, EV_Share)

oil_recent <- oil_annual %>%
  filter(Entity == "World", Year >= 2010) %>%
  select(Entity, Year, Oil_Price)

# 3. Merge datasets
merged_df <- inner_join(ev_world, oil_recent, by = c("Year", "Entity"))

# 4. Calculate dynamic scaling factor between Oil Price and EV Share
max_oil <- max(merged_df$Oil_Price, na.rm = TRUE)
max_ev  <- max(merged_df$EV_Share, na.rm = TRUE)
scale_factor <- max_oil / max_ev

# 5. Build dual-axis plot
p <- ggplot(merged_df, aes(x = Year)) +
  geom_line(aes(y = EV_Share, color = "EV Market Share (%)"), size = 1.2) +
  geom_line(aes(y = Oil_Price / scale_factor, color = "Oil Price (scaled)"), size = 1.2, linetype = "dashed") +
  scale_y_continuous(
    name = "EV Share of New Cars (%)",
    sec.axis = sec_axis(~ . * scale_factor, name = "Crude Oil Price (USD/m³)")
  ) +
  scale_color_manual(values = c("EV Market Share (%)" = "#2b5c8f", "Oil Price (scaled)" = "#d95f02")) +
  theme_minimal() +
  labs(
    title = "EV Market Share vs. Crude Oil Prices (2010-2025)",
    x = "Year",
    color = "Metric"
  ) +
  theme(
    legend.position = "bottom",
    plot.title = element_text(face = "bold", size = 14)
  )

# 6. Save plot
if (!dir.exists("output/figures")) dir.create("output/figures", recursive = TRUE)
ggsave("output/figures/ev_vs_oil_prices.png", plot = p, width = 8, height = 5)
cat("=== Success! Dynamic scaled plot saved to output/figures/ev_vs_oil_prices.png ===\n")