# Load required libraries
if (!require("readxl")) install.packages("readxl")
library(readxl)

# Define relative file paths
oil_price_path <- "data/raw/crude-oil-prices.csv"
ev_share_path  <- "data/raw/electric-car-sales-share.csv"
eia_spot_path  <- "data/raw/PET_PRI_SPT_S1_D.xls"

# ---------------------------------------------------------
# 1. Load & Inspect Historical Crude Oil Prices (1861-2025)
# ---------------------------------------------------------
crude_oil <- read.csv(oil_price_path)
cat("=== Crude Oil Prices Summary ===\n")
print(head(crude_oil))
cat("\nStructure:\n")
str(crude_oil)

# ---------------------------------------------------------
# 2. Load & Inspect EV Market Share Data (2010-2025)
# ---------------------------------------------------------
ev_sales <- read.csv(ev_share_path)
cat("\n=== EV Sales Share Summary ===\n")
print(head(ev_sales))
cat("\nStructure:\n")
str(ev_sales)

# ---------------------------------------------------------
# 3. Load & Clean EIA Daily Spot Prices (WTI & Brent)
# ---------------------------------------------------------
spot_wti <- read_excel(eia_spot_path, sheet = "Data 1", skip = 2)

# Rename columns cleanly
colnames(spot_wti) <- c("Date", "WTI_Spot_Price", "Brent_Spot_Price")

# Ensure correct data types
spot_wti$Date <- as.Date(spot_wti$Date)
spot_wti$WTI_Spot_Price <- as.numeric(spot_wti$WTI_Spot_Price)
spot_wti$Brent_Spot_Price <- as.numeric(spot_wti$Brent_Spot_Price)

cat("\n=== Cleaned EIA Spot Prices ===\n")
print(head(spot_wti))
cat("\nStructure:\n")
str(spot_wti)