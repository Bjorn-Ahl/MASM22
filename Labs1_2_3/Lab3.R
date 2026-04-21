#Lab3####
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

# Replace with the values from Mozquizto:
ref_reg <- "Vasternorrland"   # [mzq] reference region from Lab 2.B(d)

Pb_all <- mutate(Pb_all, region = relevel(region, ref_reg))


##------------------------- 3.A Regression diagnostics -------------------

###3.A(a)#### leverage for the two full-data models
m_log <- lm(log(Pb) ~ I(year - 1975), data = Pb_all)
m_bad <- lm(Pb      ~ I(year - 1975), data = Pb_all)

n_all <- nrow(Pb_all)
p_1   <- length(coef(m_log)) - 1          # number of slope params
thr_1 <- 2 * (p_1 + 1) / n_all
thr_n <- 1 / n_all

Pb_all$v_log <- hatvalues(m_log)
Pb_all$v_bad <- hatvalues(m_bad)

# Leverage plot for the log model
ggplot(Pb_all, aes(x = year, y = v_log)) +
  geom_jitter(width = 1) +
  geom_hline(yintercept = c(thr_n, thr_1), col = c("blue", "red")) +
  expand_limits(y = 0) +
  labs(x = "Year", y = "Leverage",
       title = "Leverage vs Year: log(Pb) ~ I(year-1975)")

# Leverage plot for the "bad" non-log model
ggplot(Pb_all, aes(x = year, y = v_bad)) +
  geom_jitter(width = 1) +
  geom_hline(yintercept = c(thr_n, thr_1), col = c("blue", "red")) +
  expand_limits(y = 0) +
  labs(x = "Year", y = "Leverage",
       title = "Leverage vs Year: Pb ~ I(year-1975)")

# Year that would minimise the leverage = sample mean of year
1
###3.A(b)#### leverage for the 2.B(d) model, coloured by region
m_2b_d <- lm(log(Pb) ~ I(year - 1975) + region, data = Pb_all)
p_2    <- length(coef(m_2b_d)) - 1
thr_2  <- 2 * (p_2 + 1) / n_all

Pb_all$v_2b <- hatvalues(m_2b_d)

ggplot(Pb_all, aes(x = year, y = v_2b, color = region)) +
  geom_jitter(width = 1) +
  geom_hline(yintercept = c(thr_n, thr_2), col = c("blue", "red")) +
  expand_limits(y = 0) +
  labs(x = "Year", y = "Leverage",
       title = "Leverage vs Year by region (2.B(d) model)")

# Number of observations per region (helps explain the ordering)
Pb_all |> count(region)

###3.A(c)#### studentized residuals vs linear predictor
Pb_all$yhat_2b <- predict(m_2b_d)
Pb_all$rstud   <- rstudent(m_2b_d)

ggplot(Pb_all, aes(x = yhat_2b, y = rstud)) +
  geom_point() +
  geom_hline(yintercept = 0, col = "red") +
  geom_hline(yintercept = c(-2, 2), col = "blue", linetype = "dashed") +
  geom_hline(yintercept = c(-3, 3), col = "blue") +
  labs(x = "Predicted log(Pb)", y = "Studentized residual",
       title = "Studentized residuals vs fitted (2.B(d) model)")



###3.A(d)#### same plot, separately per region
ggplot(Pb_all, aes(x = yhat_2b, y = rstud)) +
  geom_point() +
  geom_hline(yintercept = 0, col = "red") +
  geom_hline(yintercept = c(-2, 2), col = "blue", linetype = "dashed") +
  geom_hline(yintercept = c(-3, 3), col = "blue") +
  facet_wrap(~ region) +
  labs(x = "Predicted log(Pb)", y = "Studentized residual",
       title = "Studentized residuals by region")

###3.A(e)#### sqrt(|r*|) vs fitted, per region
# lambda_{0.25}: reference level under normal errors for sqrt(|r*|).
# For Z ~ N(0,1), P(|Z| > qnorm(0.875)) = 0.25, so the reference
# value in the sqrt scale is sqrt(qnorm(0.875)).
lambda_025 <- sqrt(qnorm(0.875))

ggplot(Pb_all, aes(x = yhat_2b, y = sqrt(abs(rstud)))) +
  geom_point() +
  geom_hline(yintercept = lambda_025,     col = "blue") +
  geom_hline(yintercept = sqrt(2),        col = "orange") +
  geom_hline(yintercept = sqrt(3),        col = "red") +
  expand_limits(y = 0) +
  facet_wrap(~ region) +
  labs(x = "Predicted log(Pb)",
       y = expression(sqrt(group("|", r[i]^"*", "|"))),
       title = "sqrt(|studentized residuals|) by region")

###3.A(f)#### Cook's distance vs year, per region
Pb_all$cook <- cooks.distance(m_2b_d)
f_ref <- qf(0.5, p_2 + 1, n_all - (p_2 + 1))
c_ref <- 4 / n_all

ggplot(Pb_all, aes(x = year, y = cook)) +
  geom_point() +
  geom_hline(yintercept = f_ref, col = "red") +
  geom_hline(yintercept = c_ref, col = "blue", linetype = "dashed") +
  expand_limits(y = 0) +
  facet_wrap(~ region) +
  labs(x = "Year", y = "Cook's distance",
       title = "Cook's distance by region")

# All values are far below F_{0.5}; redo without that reference line
ggplot(Pb_all, aes(x = year, y = cook)) +
  geom_point() +
  geom_hline(yintercept = c_ref, col = "blue", linetype = "dashed") +
  expand_limits(y = 0) +
  facet_wrap(~ region) +
  labs(x = "Year", y = "Cook's distance",
       title = "Cook's distance by region (without F_{0.5} line)")

###3.A(g)#### DFBETAS for the time coefficient
dfb <- dfbetas(m_2b_d)
Pb_all$dfb_time <- dfb[, "I(year - 1975)"]

ggplot(Pb_all, aes(x = year, y = dfb_time)) +
  geom_point() +
  geom_hline(yintercept = 0, col = "red") +
  geom_hline(yintercept = c(-2, 2) / sqrt(n_all),
             col = "blue", linetype = "dashed") +
  facet_wrap(~ region) +
  labs(x = "Year", y = "DFBETAS for I(year - 1975)",
       title = "DFBETAS (time coefficient) by region")

###3.A(h)#### identify the influential point in Västra Götaland
vg_name  <- grep("tland", levels(Pb_all$region), value = TRUE)
vg_idx   <- which(Pb_all$region == vg_name)
vg_point <- Pb_all[vg_idx, ][which.max(abs(Pb_all$dfb_time[vg_idx])), ]
print(vg_point)

ggplot(Pb_all, aes(x = year, y = Pb)) +
  geom_point() +
  geom_point(data = vg_point, aes(x = year, y = Pb),
             color = "red", size = 3) +
  facet_wrap(~ region) +
  labs(x = "Year", y = "Pb (mg/kg)",
       title = "2.B(a) plot with influential Västra Götaland point highlighted")


##------------------------- 3.B Model selection ---------------------------

###3.B(a)#### interaction model: does the rate of decline differ by region?
m_3b <- lm(log(Pb) ~ I(year - 1975) * region, data = Pb_all)
summary(m_3b)
anova(m_2b_d, m_3b)

# Residual plot for the interaction model (cf. 3.A(d))
Pb_all$yhat_3b <- predict(m_3b)
Pb_all$rstud_3b <- rstudent(m_3b)

ggplot(Pb_all, aes(x = yhat_3b, y = rstud_3b)) +
  geom_point() +
  geom_hline(yintercept = 0, col = "red") +
  geom_hline(yintercept = c(-2, 2), col = "blue", linetype = "dashed") +
  geom_hline(yintercept = c(-3, 3), col = "blue") +
  facet_wrap(~ region) +
  labs(x = "Predicted log(Pb)", y = "Studentized residual",
       title = "Studentized residuals by region (interaction model)")

###3.B(b)#### compare the three models
collect <- function(model, name) {
  s <- summary(model)
  data.frame(
    model = name,
    R2    = s$r.squared,
    R2adj = s$adj.r.squared,
    AIC   = AIC(model),
    BIC   = BIC(model)
  )
}

model_stats <- rbind(
  collect(m_2a    <- lm(log(Pb) ~ I(year - 1975), data = Pb_all), "2.A(b) time"),
  collect(m_2b_d,                                                 "2.B(d) time+region"),
  collect(m_3b,                                                   "3.B(a) interaction")
)
print(model_stats)


# How much of the variability is explained by the AIC-best model?
best_idx <- which.min(model_stats$AIC)
cat("Best by AIC:", model_stats$model[best_idx],
    "— R^2 =", round(model_stats$R2[best_idx], 4), "\n")
