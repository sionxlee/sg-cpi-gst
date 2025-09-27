# Singapore CPI & GST Effect — Reproducible R Pipeline (demo)
Compute YoY/MoM CPI indices and visualize potential GST step effects (Jan 2023 → Jan 2024).

## Data
- `data/sample_cpi.csv` — synthetic monthly CPI index (2019=100 base) for demo.

## Run (R)
```r
# (Optional) use renv for reproducibility
# install.packages(c("renv","tidyverse","ggplot2","lubridate"))
# renv::init()

source("scripts/02_clean.R")  # reads sample data and writes processed.csv
source("scripts/03_plot.R")   # outputs charts to reports/
```