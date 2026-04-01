library(tidyverse)
Pb_df <- load("Pb_all.rda")
print(summary(Pb_df))

Pb_myregion <- filter(Pb_all, region == "Vasternorrland")
print(summary(Pb_myregion))
print(head(Pb_myregion))

# Fit exponential model: log(Pb) ~ year
m_exp <- lm(log(Pb) ~ year, data = Pb_myregion)
print(summary(m_exp))

# Plot Pb vs year with exponential fit
year_seq <- seq(min(Pb_myregion$year), max(Pb_myregion$year), by = 0.5)
pred_exp <- exp(predict(m_exp, newdata = data.frame(year = year_seq)))

par(mfrow = c(1, 2))

plot(Pb_myregion$year, Pb_myregion$Pb,
     main = "Pb vs Year (Västernorrland)",
     xlab = "Year", ylab = "Pb (mg/kg)",
     pch = 16, col = "steelblue", cex = 0.8)
lines(year_seq, pred_exp, col = "red", lwd = 2)
legend("topright", legend = c("Data", "Exponential fit"),
       col = c("steelblue", "red"), pch = c(16, NA), lty = c(NA, 1), lwd = c(NA, 2))

plot(Pb_myregion$year, log(Pb_myregion$Pb),
     main = "log(Pb) vs Year",
     xlab = "Year", ylab = "log(Pb)",
     pch = 16, col = "steelblue", cex = 0.8)
abline(m_exp, col = "red", lwd = 2)


# -------------------------------------------Task 3,4 -----------------------------------------------

# Fit linear model: Pb = β0 + β1*(year - 1975)
m_lin <- lm(Pb ~ I(year - 1975), data = Pb_myregion)
print(summary(m_lin))

# β0 = estimated mean Pb in 1975, with 95% CI
print(confint(m_lin))




# -------------------------------------------Task 5 -------------------------------------------

# Estimate expected average Pb in 2004 with 95% CI
predict(m_lin, newdata = data.frame(year = 2004), interval = "confidence")

# -------------------------------------------Task 5 -------------------------------------------
predict(m_lin, newdata = data.frame(year = 2004), interval = "prediction")

# -------------------------Task i-j -------------------------

# Residuals vs fitted values
par(mfrow = c(1, 2))
plot(fitted(m_lin), residuals(m_lin),
     main = "Residuals vs Fitted",
     xlab = expression(hat(Y)), ylab = "Residuals",
     pch = 16, col = "steelblue", cex = 0.8)
abline(h = 0, col = "red", lwd = 2)

# -------------------------Q-Q plot-------------------------
qqnorm(residuals(m_lin), pch = 16, col = "steelblue", cex = 0.8)
qqline(residuals(m_lin), col = "red", lwd = 2)







##------------------------- LAB 1.B Log-transformation -------------------------

#-------------------------1.B(b)-------------------------
ggplot(Pb_myregion, aes(x = year - 1975, y = log(Pb))) +
  geom_point() +
  labs(x = "year - 1975", y = "log(Pb)")

###-------------------------1.B(c)-------------------------####
m_log <- lm(log(Pb) ~ I(year - 1975), data = Pb_myregion)
summary(m_log)
confint(m_log)

#------------------------- 1.B(d): Intercept = average log(Pb) in 1975-------------------------
#------------------------- 1.B(e): Slope = yearly change in log(Pb)-------------------------

###-------------------------1.B(f)####-------------------------
predict(m_log, newdata = data.frame(year = 2004), interval = "confidence")

###-------------------------1.B(g)-------------------------####
predict(m_log, newdata = data.frame(year = 2004), interval = "prediction")

###-------------------------1.B(h)-------------------------####
new_years <- data.frame(year = seq(1975, 2010, by = 0.5))
pred_ci <- cbind(new_years,
  predict(m_log, newdata = new_years, interval = "confidence"))
pred_pi <- cbind(new_years,
  predict(m_log, newdata = new_years, interval = "prediction"))

ggplot(Pb_myregion, aes(x = year - 1975, y = log(Pb))) +
  geom_point() +
  geom_line(data = pred_ci,
    aes(x = year - 1975, y = fit), col = "red") +
  geom_ribbon(data = pred_ci,
    aes(x = year - 1975, ymin = lwr, ymax = upr),
    alpha = 0.2, fill = "blue") +
  geom_ribbon(data = pred_pi,
    aes(x = year - 1975, ymin = lwr, ymax = upr),
    alpha = 0.1, fill = "red") +
  labs(x = "year - 1975", y = "log(Pb)")

###-------------------------1.B(i)-------------------------####
Pb_myregion$res_log <- residuals(m_log)
Pb_myregion$fit_log <- fitted(m_log)

ggplot(Pb_myregion, aes(x = fit_log, y = res_log)) +
  geom_point() +
  geom_hline(yintercept = 0, col = "red") +
  geom_smooth() +
  labs(x = "Fitted log(Pb)", y = "Residuals",
    title = "Residuals vs Fitted (log model)")

ggplot(Pb_myregion, aes(sample = res_log)) +
  stat_qq() + stat_qq_line(col = "red") +
  labs(title = "Q-Q plot (log model)")

ggplot(Pb_myregion, aes(x = res_log)) +
  geom_histogram(bins = 15) +
  labs(x = "Residuals", title = "Histogram of residuals (log model)")




##------------------------------1.C Back to the original scale######------------------------------

###1.C(b)####
a <- exp(coef(m_log)[1])
b <- exp(coef(m_log)[2])
cat("a =", a, "\nb =", b, "\n")
exp(confint(m_log))

# 1.C(c): Median Pb in 1975 = a = exp(β0)
# 1.C(d): Rate of decrease per year = b = exp(β1)

###1.C(e)####
exp(predict(m_log, newdata = data.frame(year = 2004),
  interval = "confidence"))

###1.C(f)####
exp(predict(m_log, newdata = data.frame(year = 2004),
  interval = "prediction"))

###1.C(g)####
ggplot(Pb_myregion, aes(x = year - 1975, y = Pb)) +
  geom_point() +
  geom_line(data = pred_ci,
    aes(x = year - 1975, y = exp(fit)), col = "red") +
  geom_ribbon(data = pred_ci,
    aes(x = year - 1975, ymin = exp(lwr), ymax = exp(upr)),
    alpha = 0.2, fill = "blue") +
  geom_ribbon(data = pred_pi,
    aes(x = year - 1975, ymin = exp(lwr), ymax = exp(upr)),
    alpha = 0.1, fill = "red") +
  labs(x = "year - 1975", y = "Pb (mg/kg)")

###1.C(h)####
ggplot(Pb_myregion, aes(x = year, y = Pb)) +
  geom_point() +
  geom_line(data = pred_ci,
    aes(x = year, y = exp(fit)), col = "red") +
  geom_ribbon(data = pred_ci,
    aes(x = year, ymin = exp(lwr), ymax = exp(upr)),
    alpha = 0.2, fill = "blue") +
  geom_ribbon(data = pred_pi,
    aes(x = year, ymin = exp(lwr), ymax = exp(upr)),
    alpha = 0.1, fill = "red") +
  labs(x = "Year", y = "Pb (mg/kg)")
