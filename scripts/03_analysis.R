library(dplyr)
library(sandwich)
library(lmtest)


cor_data <- merged %>%
    drop_na(mro_rate, loan_rate)

cor_value <- cor(cor_data$mro_rate, cor_data$loan_rate, method = "pearson")
cat("Pearson correlation between MRO and retail rate:", round(cor_value, 3), "\n")

# create a regression model
model <- lm(loan_rate ~ mro_rate, data = cor_data) # ---> loan_rate = 2.23293 + 0.74626 × mro_rate + ε
summary(model)

# add lag to regression
# Economically, it makes sense to test if:
# 	•	Retail rates respond with a delay, not instantly
# 	•	The pass-through happens in the next month
# add 1-month lag of MRO to the dataset
cor_data <- cor_data %>%
    arrange(month) %>%
    mutate(mro_lag1 = lag(mro_rate, 1)) %>%
    drop_na(mro_lag1)

# fit lagged regression
lag_model <- lm(loan_rate ~ mro_lag1, data = cor_data)
summary(lag_model)

# try incorporating both current and lagged mro
multi_model <- lm(loan_rate ~ mro_rate + mro_lag1, data = cor_data)
summary(multi_model)

# basic diagnostic plots
# Save diagnostics to file
png("output/figures/diagnostic_plots.png", width = 1000, height = 800)
par(mfrow = c(2, 2))
plot(multi_model)
par(mfrow = c(1, 1))  # reset layout
dev.off()

saveRDS(multi_model, file = "data/processed/multi_model.rds")
capture.output(summary(multi_model), file = "output/tables/multi_model_summary.txt")


# plot predicted vs actual loan rates
cor_data$predicted <- predict(multi_model)
ggplot(cor_data, aes(x = month)) +
    geom_line(aes(y = loan_rate, color = "Actual"), linewidth = 1) +
    geom_line(aes(y = predicted, color = "Predicted"), linewidth = 1, linetype = "dashed") +
    labs(title = "Actual vs Predicted Loan Rates",
         x = "Month", y = "Interest Rate (%)", color = "") +
    theme_minimal() +
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

ggsave("output/figures/predicted_actual_loan_rate.png", width = 10, height = 6, dpi=300)


# --- Heteroskedasticity-Robust Standard Errors ---
# Calculate robust standard errors
robust_se <- coeftest(multi_model, vcov. = vcovHC(multi_model, type ="HC1"))
capture.output(robust_se, file = "output/tables/multi_modal_robust.txt")

log_model <- lm(log(loan_rate) ~ mro_rate + mro_lag1, data = cor_data)
summary(log_model)
robust_log <- coeftest(log_model, vcov. = vcovHC(log_model, type = "HC1"))
print(robust_log)


# Model comparison
model_comparison <- data.frame(
    AIC = c(AIC(model), AIC(lag_model), AIC(multi_model)),
    BIC = c(BIC(model), BIC(lag_model), BIC(multi_model)),
    R_adj = c(summary(model)$adj.r.squared,
              summary(lag_model)$adj.r.squared,
              summary(log_model)$adj.r.squared
    )
)
write_csv(model_comparison, file = "output/tables/model_comparison.csv")

