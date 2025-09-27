
library(tidyverse)
library(lubridate)
library(ggplot2)

df <- read_csv("data/processed.csv")
dir.create("reports", showWarnings = FALSE)

# CPI index over time
p1 <- ggplot(df, aes(date, cpi)) + geom_line() + ggtitle("CPI Index (demo)")
ggsave("reports/cpi_index.png", p1, width = 7, height = 4, dpi = 160)

# YoY
p2 <- ggplot(df, aes(date, cpi_yoy)) + geom_line() + ggtitle("CPI YoY % (demo)")
ggsave("reports/cpi_yoy.png", p2, width = 7, height = 4, dpi = 160)

# Mark GST steps
p3 <- ggplot(df, aes(date, cpi)) + geom_line() + 
  geom_vline(xintercept = as.numeric(as.Date("2023-01-01")), linetype="dashed") +
  geom_vline(xintercept = as.numeric(as.Date("2024-01-01")), linetype="dashed") +
  ggtitle("CPI with GST Step Markers (demo)")
ggsave("reports/cpi_gst_steps.png", p3, width = 7, height = 4, dpi = 160)

cat("Plots saved to reports/\n")
