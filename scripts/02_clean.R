
library(tidyverse)
library(lubridate)

dir.create("reports", showWarnings = FALSE)

df <- read_csv("data/sample_cpi.csv")
df <- df %>% mutate(date = as_date(date))

# Compute MoM & YoY
df <- df %>% arrange(date) %>%
  mutate(
    cpi_mom = (cpi/lag(cpi) - 1) * 100,
    cpi_yoy = (cpi/lag(cpi, 12) - 1) * 100
  )

# GST step flags (demo: Jan 2023 and Jan 2024)
df <- df %>% mutate(
  gst_step = if_else(month(date) == 1 & (year(date) %in% c(2023, 2024)), 1, 0)
)

write_csv(df, "data/processed.csv")
cat("Processed written to data/processed.csv\n")
