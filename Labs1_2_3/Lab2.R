#Lab2####
library(tidyverse)

# Robust load: works whether the working directory is the repo root
# (MASM22) or the lab folder (Labs1_2_3).
pb_candidates <- c(
  "Pb_all.rda",
  "Labs1_2_3/Pb_all.rda",
  "Data/Pb_all.rda"
)
pb_path <- pb_candidates[file.exists(pb_candidates)][1]
if (is.na(pb_path)) {
  stop("Could not find Pb_all.rda. Working dir: ", getwd())
}
load(pb_path)
print(summary(Pb_all))

# Replace with the values from Mozquizto:
yr_mzq  <- 2004              # [mzq] year for 2.A(d), 2.B(e), 2.B(h)
ref_reg <- "Vasternorrland"  # [mzq] reference region for 2.B(d)


##------------------------- 2.A Does the lead level change over time? -----

###2.A(a)#### Pb vs year, all regions together
ggplot(Pb_all, aes(x = year, y = Pb)) +
  geom_point() +
  labs(x = "Year", y = "Pb (mg/kg)",
       title = "Pb vs Year (all regions)")

###2.A(b)#### log-linear model on all data
m_2a <- lm(log(Pb) ~ I(year - 1975), data = Pb_all)
summary(m_2a)
confint(m_2a)

###2.A(c)#### test H0: beta1 = 0
# t-statistic, std. error and p-value are in the coefficient table:
summary(m_2a)$coefficients
# Degrees of freedom:
m_2a$df.residual

###2.A(d)#### 95% CI and PI for Pb (mg/kg) in year [mzq]
exp(predict(m_2a, newdata = data.frame(year = yr_mzq),
            interval = "confidence"))
exp(predict(m_2a, newdata = data.frame(year = yr_mzq),
            interval = "prediction"))

###2.A(e)#### residuals vs fitted, Q-Q plot
Pb_all$fit_2a <- fitted(m_2a)
Pb_all$res_2a <- residuals(m_2a)

  ggplot(Pb_all, aes(x = fit_2a, y = res_2a)) +                                                                             
    geom_point() +
    geom_hline(yintercept = 0, col = "red") +
    geom_smooth(se = FALSE) +                                                                                               
    labs(x = "Fitted log(Pb)", y = "Residuals")

ggplot(Pb_all, aes(sample = res_2a)) +
  stat_qq() + stat_qq_line(col = "red") +
  labs(title = "Q-Q plot (2.A)")

###2.A(f)#### same plots facetted by region
ggplot(Pb_all, aes(x = fit_2a, y = res_2a)) +
  geom_point() +
  geom_hline(yintercept = 0, col = "red") +
  geom_smooth() +
  facet_wrap(~ region) +
  labs(x = "Fitted log(Pb)", y = "Residuals",
       title = "Residuals vs Fitted by region (2.A)")

ggplot(Pb_all, aes(sample = res_2a)) +
  stat_qq() + stat_qq_line(col = "red") +
  facet_wrap(~ region) +
  labs(title = "Q-Q plot by region (2.A)")


##------------------------- 2.B Add region to the model ------------------

###2.B(a)#### Pb vs year, facet by region
ggplot(Pb_all, aes(x = year, y = Pb)) +
  geom_point() +
  facet_wrap(~ region) +
  labs(x = "Year", y = "Pb (mg/kg)")

###2.B(b)#### log(Pb) vs year, facet by region
ggplot(Pb_all, aes(x = year, y = log(Pb))) +
  geom_point() +
  facet_wrap(~ region) +
  labs(x = "Year", y = "log(Pb)")

###2.B(c)#### log(Pb) ~ I(year - 1975) + region
m_2b_c <- lm(log(Pb) ~ I(year - 1975) + region, data = Pb_all)
summary(m_2b_c)
levels(Pb_all$region)   # first one is the reference category



region1 = "Vasternorrland"
###2.B(d)#### relevel to use [mzq] as reference, then refit
Pb_all <- mutate(Pb_all, region = relevel(region, ref_reg))
m_2b_d <- lm(log(Pb) ~ I(year - 1975) + region, data = Pb_all)
summary(m_2b_d)
confint(m_2b_d)

# Average (median) Pb in ref region in 1975:
exp(coef(m_2b_d)[1])
exp(confint(m_2b_d)[1, ])

# Rate of change per year (multiplicative):
exp(coef(m_2b_d)["I(year - 1975)"])
exp(confint(m_2b_d)["I(year - 1975)", ])

###2.B(e)#### 95% CI and PI for Pb in ref region in year [mzq]
new_ref <- data.frame(
  year = yr_mzq,
  region = factor(ref_reg, levels = levels(Pb_all$region))
)
exp(predict(m_2b_d, newdata = new_ref, interval = "confidence"))
exp(predict(m_2b_d, newdata = new_ref, interval = "prediction"))

###2.B(f)#### ratio between Orebro and ref region (same year)
# = exp(coefficient for regionOrebro)
orebro_name <- grep("rebro", levels(Pb_all$region), value = TRUE)
orebro_coef <- paste0("region", orebro_name)
exp(coef(m_2b_d)[orebro_coef])
exp(confint(m_2b_d)[orebro_coef, ])

###2.B(g)#### expected Pb in Orebro in 1975, 95% CI
new_orebro_1975 <- data.frame(
  year = 1975,
  region = factor(orebro_name, levels = levels(Pb_all$region))
)
exp(predict(m_2b_d, newdata = new_orebro_1975, interval = "confidence"))

###2.B(h)#### expected Pb in Orebro in year [mzq], 95% CI
new_orebro_mzq <- data.frame(
  year = yr_mzq,
  region = factor(orebro_name, levels = levels(Pb_all$region))
)
exp(predict(m_2b_d, newdata = new_orebro_mzq, interval = "confidence"))


##------------------------- 2.C Tests and residuals -----------------------

###2.C(a)#### test beta1 = 0 in the model with region
summary(m_2b_d)$coefficients
m_2b_d$df.residual
###2.C(b)#### partial F-test: 2.A(b) vs 2.B(d)
anova(m_2a, m_2b_d)

###2.C(c)#### add fitted lines, CI and PI to the plots in 2.B(a)/(b)
new_grid <- expand.grid(
  year   = seq(min(Pb_all$year), max(Pb_all$year), by = 0.5),
  region = levels(Pb_all$region)
)
pred_ci_2b <- cbind(
  new_grid,
  predict(m_2b_d, newdata = new_grid, interval = "confidence")
)
pred_pi_2b <- cbind(
  new_grid,
  predict(m_2b_d, newdata = new_grid, interval = "prediction")
)

# log(Pb) scale (goes with 2.B(b))
ggplot(Pb_all, aes(x = year, y = log(Pb))) +
  geom_point() +
  geom_line(data = pred_ci_2b, aes(x = year, y = fit), col = "red") +
  geom_ribbon(data = pred_ci_2b,
              aes(x = year, y = fit, ymin = lwr, ymax = upr),
              alpha = 0.2, fill = "blue") +
  geom_ribbon(data = pred_pi_2b,
              aes(x = year, y = fit, ymin = lwr, ymax = upr),
              alpha = 0.1, fill = "red") +
  facet_wrap(~ region) +
  labs(x = "Year", y = "log(Pb)")

# Original scale (goes with 2.B(a))
ggplot(Pb_all, aes(x = year, y = Pb)) +
  geom_point() +
  geom_line(data = pred_ci_2b,
            aes(x = year, y = exp(fit)), col = "red") +
  geom_ribbon(data = pred_ci_2b,
              aes(x = year, y = exp(fit),
                  ymin = exp(lwr), ymax = exp(upr)),
              alpha = 0.2, fill = "blue") +
  geom_ribbon(data = pred_pi_2b,
              aes(x = year, y = exp(fit),
                  ymin = exp(lwr), ymax = exp(upr)),
              alpha = 0.1, fill = "red") +
  facet_wrap(~ region) +
  labs(x = "Year", y = "Pb (mg/kg)")

###2.C(d)#### residuals vs fitted + Q-Q plot for the 2.B(d) model
Pb_all$fit_2b <- fitted(m_2b_d)
Pb_all$res_2b <- residuals(m_2b_d)

ggplot(Pb_all, aes(x = fit_2b, y = res_2b)) +
  geom_point() +
  geom_hline(yintercept = 0, col = "red") +
  geom_smooth() +
  labs(x = "Fitted log(Pb)", y = "Residuals",
       title = "Residuals vs Fitted (2.B(d))")

ggplot(Pb_all, aes(sample = res_2b)) +
  stat_qq() + stat_qq_line(col = "red") +
  labs(title = "Q-Q plot (2.B(d))")

###2.C(e)#### same residual plots by region
ggplot(Pb_all, aes(x = fit_2b, y = res_2b)) +
  geom_point() +
  geom_hline(yintercept = 0, col = "red") +
  geom_smooth() +
  facet_wrap(~ region) +
  labs(x = "Fitted log(Pb)", y = "Residuals",
       title = "Residuals vs Fitted by region (2.B(d))")

ggplot(Pb_all, aes(sample = res_2b)) +
  stat_qq() + stat_qq_line(col = "red") +
  facet_wrap(~ region) +
  labs(title = "Q-Q plot by region (2.B(d))")
