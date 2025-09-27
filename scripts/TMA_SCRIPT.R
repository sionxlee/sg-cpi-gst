# =================================================
# ANL501 TMA R script by Lee Sean Xiang PI:W2572937
# =================================================
library(readxl)
library(dplyr)
library(tidyr)
library(janitor)
library(lubridate)
library(stringr)
library(ggplot2)
library(gganimate)
library(zoo)

# =================================================
# Figure 1: All Items CPI YoY
# =================================================
# --- Load monthly CPI sheet T6 ---
cpi_raw <- read_excel("ConsumerPriceIndex.xlsx", sheet = "T6", skip = 10) %>%
  clean_names()

cpi_tidy <- cpi_raw %>%
  pivot_longer(
    cols = starts_with("x"),
    names_to = "month",
    values_to = "value",
    values_transform = list(value = as.numeric)   # force numeric
  ) %>%
  mutate(
    month = str_remove(month, "^x"),
    month = str_replace(month, "_", " "),
    date = parse_date_time(month, orders = "Y b"),
    series = data_series
  ) %>%
  filter(!is.na(date), !is.na(value)) %>%
  group_by(series) %>%
  arrange(date) %>%
  mutate(yoy = (value / lag(value, 12) - 1) * 100) %>%
  ungroup()

# --- Focus 2019 onwards ---
cpi_recent <- cpi_tidy %>% filter(date >= as.Date("2019-01-01"))
gst_dates <- as.Date(c("2023-01-01","2024-01-01"))


fig1 <- ggplot(cpi_recent %>% filter(series == "All Items"), aes(date, yoy)) +
  geom_line(color="steelblue") +
  geom_vline(xintercept=gst_dates, linetype="dashed", color="red") +
  labs(
    title="Figure 1. CPI Inflation (YoY) — All Items, 2019–2025",
    subtitle="Dashed lines mark GST hikes: Jan 2023 and Jan 2024",
    x="Date", y="YoY %"
  ) +
  theme_minimal()
fig1

# =================================================
# Figure 1a: Animated All Items CPI YoY
# =================================================
fig1a <- fig1 + transition_reveal(date)
animate(fig1a, duration=8, fps=25, width=800, height=500)

# =================================================
# Figure 1b: Top 6 Volatile Categories, Faceted
# =================================================
top6 <- cpi_recent %>%
  group_by(series) %>%
  summarise(volatility = sd(yoy, na.rm=TRUE)) %>%
  arrange(desc(volatility)) %>%
  slice(1:6) %>%
  pull(series)

fig1b <- ggplot(cpi_recent %>% filter(series %in% top6),
                aes(date, yoy, color=series)) +
  geom_line(show.legend=FALSE) +
  geom_vline(xintercept=gst_dates, linetype="dashed", color="red") +
  facet_wrap(~series, scales="free_y") +
  labs(
    title="Figure 1b. CPI Inflation (YoY) — Top 6 Categories, 2019–2025",
    subtitle="Dashed lines = GST hikes",
    x="Date", y="YoY %"
  ) +
  theme_minimal()
fig1b
# =================================================
# Figure 2: HDB Resale Price Index
# =================================================
hdb_raw <- read_csv("HDBResalePriceIndex1Q2009100Quarterly.csv") %>%
  clean_names()

hdb_tidy <- hdb_raw %>%
  mutate(date = as.Date(as.yearqtr(quarter, format = "%Y-Q%q")),
         index = as.numeric(index)) 

ggplot(hdb_tidy, aes(date, index)) +
  geom_line(color="darkorange") +
  labs(
    title="Figure 2. HDB Resale Price Index (1Q2009=100)",
    x="Date", y="Index"
  ) +
  theme_minimal()


# =================================================
# Figure 3: Median Income (Nominal vs Real)
# =================================================
income_raw <- read_csv("MedianGrossMonthlyIncomeFromEmploymentofFullTimeEmployedResidentsTotal.csv") %>%
  clean_names()

income_tidy <- income_raw %>%
  rename(income_nominal = 2) %>%
  mutate(year = as.integer(year),
         income_nominal = as.numeric(income_nominal))

# Compute CPI annual average (All Items only)
cpi_annual <- cpi_tidy %>%
  filter(series == "All Items") %>%
  mutate(year = year(date)) %>%
  group_by(year) %>%
  summarise(cpi_index = mean(value, na.rm=TRUE))

# Merge & compute real income
income_tidy <- left_join(income_tidy, cpi_annual, by="year") %>%
  mutate(income_real = income_nominal * (last(cpi_index)/cpi_index))

ggplot(income_tidy, aes(year, income_nominal)) +
  geom_line(color="blue", size=1) +
  geom_line(aes(y=income_real), color="red", linetype="dashed", size=1) +
  labs(
    title="Figure 3. Median Income (Nominal vs Real)",
    subtitle="Dashed = Real (CPI-adjusted)",
    x="Year", y="SGD"
  ) +
  theme_minimal()


# =================================================
# Figure 4: Bank Interest Rates
# =================================================
ir_raw <- read_csv("CurrentBanksInterestRatesEndOfPeriodMonthly.csv") %>%
  clean_names()
ir_tidy <- ir_raw %>%
  pivot_longer(
    cols = -1,                   # all columns except the first one
    names_to = "month",
    values_to = "value",
    values_transform = list(value = as.numeric)
  ) %>%
  mutate(
    series = !!sym(names(ir_raw)[1]),          # first column = series name
    month = str_remove(month, "^x"),           # remove leading x
    month = str_replace(month, "_", " "),      # 2023_jan -> 2023 jan
    date = parse_date_time(month, orders="Y b")
  ) %>%
  filter(!is.na(date), !is.na(value)) %>%
  select(date, series, value)

ir_recent <- ir_tidy %>% filter(date >= as.Date("2022-01-01"))

# Plot with GST hike markers
ggplot(ir_recent %>% filter(series %in% c("Singapore Overnight Rate Average",
                                          "Compounded Singapore Overnight Rate Average (SORA) - 3 Month",
                                          "Fixed Deposits Rate (12 months)")),
       aes(date, value, color=series)) +
  geom_line(size=1) +
  geom_vline(xintercept=as.Date(c("2023-01-01","2024-01-01")),
             linetype="dashed", color="red") +
  labs(
    title="Figure 4. Selected Bank Interest Rates (2022–2025)",
    subtitle="Dashed lines = GST hikes",
    x="Date", y="%"
  ) +
  theme_minimal()

# =================================================
# Figure 5: Industrial Production by Cluster
# =================================================
# Load and clean
iip_raw <- read_csv("IndexOfIndustrialProduction2015100Monthlybymanufacturingclusters.csv") %>%
  clean_names()

# First column = cluster name
first_col <- names(iip_raw)[1]

# Clean column names
iip_tidy <- iip_raw %>%
  clean_names() %>%
  mutate(
    date = as.Date(paste0(month, "-01")),   # convert YYYY-MM to YYYY-MM-01
    cluster = level_1,
    index = as.numeric(value)
  ) %>%
  filter(!is.na(date), !is.na(index)) %>%
  group_by(cluster) %>%
  arrange(date) %>%
  mutate(yoy = (index / lag(index, 12) - 1) * 100) %>%
  ungroup() %>%
  select(date, cluster, index, yoy)

iip_recent <- iip_tidy %>% filter(date >= as.Date("2010-01-01"))

ggplot(iip_recent, aes(date, yoy, color=cluster)) +
  geom_line() +
  facet_wrap(~cluster, scales="free_y") +
  labs(
    title="Figure 5. Industrial Production by Cluster (YoY %), 2010–2019",
    subtitle="Latest data ends in 2019; GST hikes (2023, 2024) not covered",
    x="Date", y="YoY %"
  ) +
  theme_minimal()
