library(tidyverse)
library(lubridate)
library(ggplot2)

loan_monthly <- read_csv("data/processed/loan_monthly.csv") %>%
    select(-1)
mro_monthly <- read_csv("data/processed/mro_monthly.csv") %>%
    select(-1)

merged <- loan_monthly %>%
    left_join(mro_monthly, by = "month")

write_csv(merged, "data/processed/merged_rates.csv")


# plot the relationship between MRO rates and Retail Housing Loan Rate
ggplot(merged, aes(x = month)) +
    geom_line(aes(y = loan_rate, color = "Retail Housing Loan Rate"), linewidth = 1) +
    geom_line(aes(y = mro_rate, color = "MRO Rate"), linewidth = 1, linetype = "dashed") +
    labs(title = "ECB MRO vs. Retail Housing Loan Rate (Germany)",
         x = "Month", y = "Interest rate, (%)", color="Rate Type") +
    theme_minimal(base_family = "Arial", base_size = 12) +
    theme(
        plot.background = element_rect(fill = "white", color = NA),
        panel.background = element_rect(fill = "white", color = NA),
        legend.position = "bottom",
        legend.title = element_text(color = "black"),
        legend.text = element_text(color = "black"),
        plot.title = element_text(face = "bold", size = 14, color = "black"),
        axis.text = element_text(color = "black"),
        axis.title = element_text(color = "black")
    )


ggsave("output/figures/mro_vs_retail_rate.png")
