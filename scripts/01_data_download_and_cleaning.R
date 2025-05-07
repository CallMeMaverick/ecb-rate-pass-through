library(tidyverse)
library(lubridate)

# === retail loans
loan_data <- read_csv(
    "data/raw/ECB Data Portal_20250507115252.csv",
    col_names = c("date", "period", "loan_rate"),
    skip = 1
)

loan_monthly <- loan_data %>%
    mutate(
        date = ymd(date),
        month = floor_date(date, "month")
    ) %>%
    select("month", "loan_rate")



# === mro monthly rate
mro_data <- read_csv(
    "data/raw/ECB Data Portal_20250507134102.csv",
    col_names = c("date", "period", "mro_rate"),
    skip = 1
)

mro_monthly <- mro_data %>%
    mutate(date = ymd(date)) %>%
    filter(date >= ymd("2003-01-31")) %>%
    group_by(month = floor_date(date, "month")) %>%
    summarise(mro_rate = last(mro_rate), .groups = "drop")

write.csv(loan_monthly, "data/processed/loan_monthly.csv")
write.csv(mro_monthly, "data/processed/mro_monthly.csv")

