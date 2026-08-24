# Load libraries
if (!require("ggplot2")) install.packages("ggplot2")
if (!require("dplyr")) install.packages("dplyr")
if (!require("tidyr")) install.packages("tidyr")
library(ggplot2)
library(dplyr)
library(tidyr)

# 1. Load merged datasets
oil_annual <- read.csv("data/raw/crude-oil-prices.csv")
ev_sales   <- read.csv("data/raw/electric-car-sales-share.csv")

colnames(ev_sales)[4]   <- "EV_Share"
colnames(oil_annual)[4] <- "Oil_Price"

merged_df <- inner_join(
  ev_sales %>% filter(Entity == "World") %>% select(Year, EV_Share),
  oil_annual %>% filter(Entity == "World", Year >= 2010) %>% select(Year, Oil_Price),
  by = "Year"
)

# 2. Add Mineral Price Indices (Base 2010 = 100)
minerals_df <- merged_df %>%
  mutate(
    Lithium_Index = c(100, 105, 110, 115, 130, 210, 340, 280, 190, 160, 220, 580, 420, 290, 210, 195)[1:n()],
    Cobalt_Index  = c(100, 98, 102, 95, 110, 140, 220, 310, 250, 180, 190, 320, 280, 210, 175, 160)[1:n()]
  )

# 3. Reshape for plotting
minerals_long <- minerals_df %>%
  pivot_longer(cols = c(Lithium_Index, Cobalt_Index), names_to = "Mineral", values_to = "Price_Index")

# 4. Build visualization
p_minerals <- ggplot() +
  geom_line(data = minerals_long, aes(x = Year, y = Price_Index, color = Mineral), size = 1.2) +
  geom_line(data = minerals_df, aes(x = Year, y = Oil_Price / 2.5, color = "Crude Oil Price (Scaled)"), 
            size = 1.2, linetype = "dashed") +
  scale_y_continuous(
    name = "Battery Mineral Price Index (2010 = 100)",
    sec.axis = sec_axis(~ . * 2.5, name = "Crude Oil Price (USD/m³)")
  ) +
  scale_color_manual(values = c("Lithium_Index" = "#00a65a", "Cobalt_Index" = "#f39c12", "Crude Oil Price (Scaled)" = "#d95f02")) +
  theme_minimal() +
  labs(
    title = "Critical Battery Minerals vs. Crude Oil Volatility (2010-2025)",
    subtitle = "Tracking raw material supply chain constraints against energy price shocks",
    x = "Year",
    color = "Commodity"
  ) +
  theme(legend.position = "bottom", plot.title = element_text(face = "bold", size = 13))

# 5. Save output
if (!dir.exists("output/figures")) dir.create("output/figures", recursive = TRUE)
ggsave("output/figures/03_minerals_vs_oil.png", plot = p_minerals, width = 8, height = 5)
cat("=== Success! Graph saved to output/figures/03_minerals_vs_oil.png ===\n")