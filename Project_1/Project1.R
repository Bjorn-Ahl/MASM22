library(tidyverse)
library(readxl)
library(car)
library(rstatix)
library(GGally)
xlsx_path <- if (file.exists("Project1data.xlsx")) "Project1data.xlsx" else "/Users/bjorn_ahl/Downloads/GitHub/MASM22/Project_1/Project1data.xlsx"
df <- as.data.frame(read_excel(xlsx_path))


library(broom)
library(patchwork)

m_raw    <- lm(PM10      ~ Fuel,       data = df)
m_logy   <- lm(log(PM10) ~ Fuel,       data = df)
m_loglog <- lm(log(PM10) ~ log(Fuel),  data = df)

plot_resid <- function(mod, title) {
  augment(mod) |>
    ggplot(aes(.fitted, .resid)) +
    geom_point(alpha = .6) +
    geom_hline(yintercept = 0, linetype = 2) +
    geom_smooth(se = FALSE, method = "loess", formula = y ~ x, colour = "steelblue") +
    labs(title = title, x = "fitted", y = "residual")
}

plot_resid(m_raw,    "PM10 ~ Fuel") +
  plot_resid(m_logy,   "log(PM10) ~ Fuel") +
  plot_resid(m_loglog, "log(PM10) ~ log(Fuel)")

plot_qq <- function(mod, title) {
  augment(mod) |>
    ggplot(aes(sample = .resid)) +
    stat_qq(alpha = .6) +
    stat_qq_line(colour = "steelblue") +
    labs(title = title, x = "theoretical quantiles", y = "sample quantiles")
}
plot_qq(m_raw,    "PM10 ~ Fuel") +
    plot_qq(m_logy,   "log(PM10) ~ Fuel") +
    plot_qq(m_loglog, "log(PM10) ~ log(Fuel)")

another_model <- lm(log(PM10) ~ log(Fuel), data = df)
another_model$coefficients
confint(another_model)

new_data <- data.frame(Fuel = seq(min(df$Fuel), max(df$Fuel), length.out = 100))

pred_int <- predict(another_model, newdata = new_data, interval = "prediction")
conf_int <- predict(another_model, newdata = new_data, interval = "confidence")

plot_df <- data.frame(
  Fuel = new_data$Fuel,
  fit = conf_int[, "fit"],
  ci_lower = conf_int[, "lwr"],
  ci_upper = conf_int[, "upr"],
  pi_lower = pred_int[, "lwr"],
  pi_upper = pred_int[, "upr"]
)

# Plot on log scale
p1 <- ggplot() +
  geom_point(data = df, aes(x = log(Fuel), y = log(PM10))) +
  geom_ribbon(data = plot_df, aes(x = log(Fuel), ymin = pi_lower, ymax = pi_upper),
              alpha = 0.2, fill = "blue") +
  geom_ribbon(data = plot_df, aes(x = log(Fuel), ymin = ci_lower, ymax = ci_upper),
              alpha = 0.4, fill = "blue") +
  geom_line(data = plot_df, aes(x = log(Fuel), y = fit), color = "red") +
  labs(x = "ln(Fuel)", y = "ln(PM10)",
       title = "Log scale: ln(PM10) ~ ln(Fuel)",
       subtitle = "red = fit, dark band = 95% CI, light band = 95% PI") +
  theme_minimal()

# Transforming back to original scale
plot_df_e <- data.frame(
  Fuel = new_data$Fuel,
  fit = exp(conf_int[, "fit"]),
  ci_lower = exp(conf_int[, "lwr"]),
  ci_upper = exp(conf_int[, "upr"]),
  pi_lower = exp(pred_int[, "lwr"]),
  pi_upper = exp(pred_int[, "upr"])
)

p2 <- ggplot() +
  geom_point(data = df, aes(x = Fuel, y = PM10)) +
  geom_ribbon(data = plot_df_e, aes(x = Fuel, ymin = pi_lower, ymax = pi_upper),
              alpha = 0.2, fill = "blue") +
  geom_ribbon(data = plot_df_e, aes(x = Fuel, ymin = ci_lower, ymax = ci_upper),
              alpha = 0.4, fill = "blue") +
  geom_line(data = plot_df_e, aes(x = Fuel, y = fit), color = "red") +
  labs(x = "Fuel", y = "PM10",
       title = "Original scale: PM10 vs Fuel (back-transformed)",
       subtitle = "red = fit, dark band = 95% CI, light band = 95% PI") +
  theme_minimal()

# Show both side by side so the log-scale and back-transformed versions are easy to compare
p1 + p2 + plot_annotation(title = "Model 1(b): ln(PM10) = beta0 + beta1 * ln(Fuel)")

# Back-transformed relationship: PM10 = exp(beta0) * Fuel^beta1 = a * Fuel^beta1
intercept <- cbind(
  expbeta = exp(another_model$coefficients["(Intercept)"]),
  exp(confint(another_model, parm = "(Intercept)"))
) |>
  round(digits = 4)

slope <- cbind(
  expbeta = exp(another_model$coefficients["log(Fuel)" ]),
  exp(confint(another_model, parm = "log(Fuel)"))
) |>
  round(digits = 4)

intercept   # a  = exp(beta0) with 95% CI
slope       # exp(beta1) with 95% CI (the elasticity factor per unit change in log(Fuel))

b1  <- coef(another_model)["log(Fuel)"]
ci  <- confint(another_model, parm = "log(Fuel)")
L   <- ci[1]; U <- ci[2]

# (i) 10 % decrease in Fuel
r_hat <- 0.9^b1
r_ci  <- c(0.9^U, 0.9^L)
pct_change <- (c(r_hat, r_ci) - 1) * 100

# (ii) fuel reduction to halve PM10
k_hat <- 0.5^(1/b1)
k_ci  <- c(0.5^(1/L), 0.5^(1/U))
reduction <- (1 - c(k_hat, k_ci[2], k_ci[1])) * 100

round(rbind(
  "PM10 change (%) from -10% Fuel" = pct_change,
  "Fuel reduction (%) to halve PM10" = reduction
), 2)

another_model_summary <- summary(another_model)
another_model_summary$coefficients
another_model$df.residual



# This compute R^2 for the model with log-transformation
another_model_summary$r.squared

# Adjusted R^2 for the model with log-transformation
another_model_summary$adj.r.squared


df |> select(Kommun, Fuel:PM10) |>
  pivot_longer(cols = Fuel:GRP) |>
  ggplot(aes(x = value, y = PM10)) +
  geom_point() +
  facet_wrap(~ name, scales = "free_x") +
  scale_y_log10()

library(tidyverse)
library(GGally)

not_logged <- c("HighEd", "Seniors", "Vehicles")

df2 <- df |> 
  select(Kommun, Fuel:GRP)

df_long <- df2 |> 
  pivot_longer(cols = Fuel:GRP) |> 
  mutate(
    value = if_else(
      name %in% not_logged,
      value,
      log(value)
    ),
    name = if_else(
      name %in% not_logged,
      name,
      paste0("log_", name)
    )
  )

df_wide <- df_long |> 
  pivot_wider(
    id_cols = Kommun,
    names_from = name,
    values_from = value
  ) |> 
  select(-Kommun)

ggpairs(df_wide)

cor_matrix <- cor(df_wide, use = "complete.obs")


model_seven <- lm(log(PM10) ~ log(Fuel) + log(GRP) + log(Income) + log(Vehicles) + log(Builton) + log(Higheds) + log(Seniors), data = df)
summary(model_seven)

vif(model_seven)

vif_values <- vif(model_seven)
barplot(vif_values, main = "VIF Values for Predictors", xlab = "Predictors", ylab = "VIF", col = "steelblue")
abline(h = 5, col = "red")

model_six <- lm(log(PM10) ~ log(Fuel) + log(GRP) + log(Income) + log(Vehicles) + log(Higheds) + log(Seniors), data = df) # This is without Builton
summary(model_six)

vif(model_six)

vif_values1 <- vif(model_six)
barplot(vif_values1, main = "VIF Values for Predictors", xlab = "Predictors", ylab = "VIF", col = "steelblue")
abline(h = 5, col = "red")


# t-test on log(Fuel) in Model 2(c)
summary(model_six)$coefficients["log(Fuel)", ]
model_six$df.residual

# Partial F-test: Model 1(b) vs Model 2(c)
anova(another_model, model_six)

# R^2 and adj R^2 for Model 2(c)
c(R2    = summary(model_six)$r.squared,
  adjR2 = summary(model_six)$adj.r.squared)

df$Part <- as.factor(df$Part)
table(df$Part)

model_eight <- lm(log(PM10) ~ log(Fuel) + log(GRP) + log(Income) +
                  log(Vehicles) + log(Higheds) + log(Seniors) + Part,
                  data = df)

#summary(model_eight)$coefficients |> round(4) |> knitr::kable(
#  caption = "Model 2(e): coefficients and standard errors")

#vif(model_eight) |> knitr::kable(caption = "GVIF with Df for Model 2(e)")

# Partial F-test: does Part matter on top of Model 2(c)?
anova(model_six, model_eight)

interaction_model <- lm(log(PM10) ~ (log(Fuel) + log(GRP) + log(Income) +
                        log(Vehicles) + log(Higheds) + log(Seniors)) * Part,
                        data = df)

# Model 2(e) vs Model 2(f): does adding the 12 interactions help?
anova(model_eight, interaction_model)

pred <- predict(interaction_model)
hat <- hatvalues(interaction_model)
top3 <- order(hat, decreasing = TRUE)[1:3]

# Create a data frame for plotting
plot_data <- data.frame(
  pred = pred,
  hat = hat,
  is_top3 = FALSE
)
plot_data$is_top3[top3] <- TRUE

ggplot(plot_data, aes(x = pred, y = hat, color = is_top3)) +
  geom_point(size = ifelse(plot_data$is_top3, 3, 1)) +
  scale_color_manual(values = c("FALSE" = "black", "TRUE" = "red"), 
                     labels = c("FALSE" = "Regular points", "TRUE" = "Top 3 leverage")) +
  geom_hline(yintercept = 2*length(coef(interaction_model))/nrow(df),
             linetype = "dashed", color = "red") +
  geom_hline(yintercept = 3*length(coef(interaction_model))/nrow(df),
             linetype = "dashed", color = "blue") +
  labs(title = "Leverage vs Predicted Values",
       x = "Predicted values",
       y = "Leverage (hat values)",
       color = "Point type") +
  theme_minimal()


pike_pred <- mutate(
  df, 
  yhat = predict(interaction_model),
  r = rstudent(interaction_model),
  v = hatvalues(interaction_model),
  D = cooks.distance(interaction_model)
)

# with 1/n and 2(p+1)/n horizontal lines:
# p+1 = 

pplus1 <- length(interaction_model$coefficients)
n <- nobs(interaction_model)

f1.pike <- pplus1
f2.pike <- interaction_model$df.residual
cook.limit.pike <- qf(0.5, f1.pike, f2.pike)
names(pike_pred)

# Find top 3 by Cook's D
top3_cook <- pike_pred |> slice_max(D, n = 3)

ggplot(pike_pred, aes(yhat, D)) + 
  geom_point(size = 3) +
  geom_point(data = top3_cook, aes(yhat, D), color = "red", size = 3) +
  geom_text(data = top3_cook, aes(label = Kommun), 
            vjust = -0.8, color = "red", size = 3) +
  geom_hline(yintercept = cook.limit.pike, color = "red") +
  geom_hline(yintercept = 4/n, linetype = 2, color = "red") +
  xlab("Fitted values") +
  ylab("D_i") +
  labs(title = "Pike: Cook's Distance",
       caption = "4/n (dashed), F_0.5, p+1, n-(p+1) (solid)")

# Here we compute DFBETAS for the beta estimation(s)
# head(dfbetas(interaction_model))

# DFBETAS for the top 3 Cook's D municipalities
dfb       <- dfbetas(interaction_model)
top3_idx  <- order(pike_pred$D, decreasing = TRUE)[1:3]
top3_name <- pike_pred$Kommun[top3_idx]
dfb_top3  <- dfb[top3_idx, ]
rownames(dfb_top3) <- top3_name

# Top-3 most-affected coefficients per influential municipality
for (i in seq_along(top3_name)) {
  ord <- order(abs(dfb_top3[i, ]), decreasing = TRUE)[1:3]
  cat(top3_name[i], ":\n")
  print(round(dfb_top3[i, ord], 3))
  cat("\n")
}

# Plot log(PM10) vs log(Fuel) and log(Vehicles), faceted by Part
plot_df_3b <- df |>
  mutate(highlight = Kommun %in% top3_name)

part_lab <- as_labeller(c("1" = "Götaland", "2" = "Svealand", "3" = "Norrland"))

p_fuel <- ggplot(plot_df_3b, aes(log(Fuel), log(PM10))) +
  geom_point(data = filter(plot_df_3b, !highlight), color = "grey60", size = 1.5) +
  geom_point(data = filter(plot_df_3b,  highlight), color = "red",    size = 3) +
  geom_text( data = filter(plot_df_3b,  highlight), aes(label = Kommun),
             color = "red", size = 3, vjust = -0.8) +
  facet_wrap(~ Part, labeller = part_lab) +
  labs(title = "log(PM10) vs log(Fuel)") +
  theme_minimal()

p_veh <- ggplot(plot_df_3b, aes(log(Vehicles), log(PM10))) +
  geom_point(data = filter(plot_df_3b, !highlight), color = "grey60", size = 1.5) +
  geom_point(data = filter(plot_df_3b,  highlight), color = "red",    size = 3) +
  geom_text( data = filter(plot_df_3b,  highlight), aes(label = Kommun),
             color = "red", size = 3, vjust = -0.8) +
  facet_wrap(~ Part, labeller = part_lab) +
  labs(title = "log(PM10) vs Vehicles") +
  theme_minimal()

p_fuel / p_veh

pred   <- predict(interaction_model)
r_star <- rstudent(interaction_model)

highlight_names <- c("Oxelösund", "Kiruna", "Gällivare")
plot_df <- df |>
  mutate(pred = pred, r_star = r_star,
         highlight = Kommun %in% highlight_names)


# |r*| > 3 but NOT in the Cook's D top 3
plot_df |> filter(abs(r_star) > 3, !highlight) |>
  select(Kommun, pred, r_star)

cook_d <- cooks.distance(interaction_model)
high_cook_names <- df$Kommun[order(cook_d, decreasing = TRUE)[1:3]]

plot_df_3c <- plot_df |>
  mutate(
    cook_d  = cook_d,
    big_r   = abs(r_star) > 3,
    big_D   = Kommun %in% high_cook_names,
    Highlight = case_when(
      big_r & big_D ~ "|r*| > 3 & High Cook's D",
      big_r         ~ "|r*| > 3",
      big_D         ~ "High Cook's D",
      TRUE          ~ "none"
    )
  )

ggplot(plot_df_3c, aes(x = pred, y = sqrt(abs(r_star)))) +
  geom_point(data = filter(plot_df_3c, Highlight == "none"),
             colour = "grey40", size = 1.8, alpha = 0.7) +
  geom_point(data = filter(plot_df_3c, Highlight != "none"),
             aes(colour = Highlight), size = 3) +
  geom_text(data = filter(plot_df_3c, Highlight != "none"),
            aes(label = Kommun, colour = Highlight),
            vjust = -0.8, size = 3, show.legend = FALSE) +
  geom_hline(yintercept = c(0, sqrt(qnorm(0.75)), sqrt(2)),
             colour = "black") +
  geom_hline(yintercept = sqrt(3), linetype = "dashed", colour = "black") +
  scale_colour_manual(values = c("|r*| > 3" = "tomato",
                                 "High Cook's D" = "turquoise3")) +
  labs(x = "yhat", y = "sqrt(abs(r))",
       title = "sqrt(|r*|) vs fitted values",
       caption = "Reference lines at 0, sqrt(Phi(0.75)), sqrt(2) and sqrt(3) dashed") +
  theme_minimal()

qqnorm(r_star) 
qqline(r_star, col = "red")

# Kommun strings look like "0481 Oxelösund" — strip the leading code for matching
df_new <- df |>
  mutate(.name = str_squish(str_remove(Kommun, "^\\d+"))) |>
  filter(!.name %in% c("Oxelösund", "Kiruna", "Gällivare")) |>
  select(-.name)

stopifnot(nrow(df_new) == nrow(df) - 3)
nrow(df_new)

# Refit Model 2(f) on the reduced data (6 numerical vars, as in Model 2(c))
model_new <- lm(log(PM10) ~ (log(Fuel) + log(GRP) + log(Income) +
                log(Vehicles) + log(Higheds) + log(Seniors)) * Part,
                data = df_new)

pred2   <- predict(model_new)
r_star2 <- rstudent(model_new)
cook2   <- cooks.distance(model_new)
p_new   <- length(coef(model_new))
n_new   <- nrow(df_new)

# Cook's D plot, with updated reference lines for the new n and p
ggplot(data.frame(pred2 = pred2, cook2 = cook2), aes(pred2, cook2)) +
  geom_point() +
  geom_hline(yintercept = 4 / n_new, linetype = 2, colour = "red") +
  geom_hline(yintercept = qf(0.5, p_new, n_new - p_new), colour = "red") +
  labs(x = "Predicted values", y = "Cook's D",
       title = "Cook's D on reduced data (3 municipalities removed)")

# Studentised residuals
ggplot(mutate(df_new, pred2 = pred2, r_star2 = r_star2),
       aes(pred2, r_star2)) +
  geom_point() +
  geom_hline(yintercept = c(-3, 0, 3),
             linetype = c("dashed", "solid", "dashed"), colour = "red") +
  labs(x = "Predicted values", y = "Studentised residuals")

qqnorm(r_star2); qqline(r_star2, col = "red")

# Remaining |r*| > 3 on reduced data
df_new |> mutate(r_star2 = r_star2) |>
  filter(abs(r_star2) > 3) |>
  select(Kommun, Part, r_star2)

# Null model (intercept only), refit on reduced data
model_null <- lm(log(PM10) ~ 1, data = df_new)

# Refitted Model 1(b): log(PM10) ~ log(Fuel)
model_log_fuel_new <- lm(log(PM10) ~ log(Fuel), data = df_new)

# Refitted Model 2(c): six log-transformed numerical variables (no log(Builton))
# This is the starting point for both stepwise searches.
model_num_var <- lm(log(PM10) ~ log(Fuel) + log(GRP) + log(Income) +
                    log(Vehicles) + log(Higheds) + log(Seniors),
                    data = df_new)

# Refitted Model 2(f): Model 2(c) plus Part and all Part-by-numeric interactions.
# Upper scope must be fit on the same (reduced) data as the start model.
interaction_model_new <- lm(log(PM10) ~ (log(Fuel) + log(GRP) + log(Income) +
                            log(Vehicles) + log(Higheds) + log(Seniors)) * Part,
                            data = df_new)

n_new <- nrow(df_new)

# Stepwise AIC
aic_model <- step(model_num_var,
                  scope = list(lower = model_null, upper = interaction_model_new),
                  direction = "both",
                  trace = 0)

# Stepwise BIC (penalty k = log(n))
bic_model <- step(model_num_var,
                  scope = list(lower = model_null, upper = interaction_model_new),
                  direction = "both",
                  k = log(n_new),
                  trace = 0)

# Selected formulas
cat("AIC-selected model:\n"); print(formula(aic_model))
cat("\nBIC-selected model:\n"); print(formula(bic_model))

# Beta-estimates and standard errors for the two resulting models
aic_tab <- summary(aic_model)$coefficients[, c("Estimate", "Std. Error")] |> round(4)
bic_tab <- summary(bic_model)$coefficients[, c("Estimate", "Std. Error")] |> round(4)

knitr::kable(aic_tab, caption = "AIC-selected model: beta-estimates and standard errors")
knitr::kable(bic_tab, caption = "BIC-selected model: beta-estimates and standard errors")

grep("Part", attr(terms(aic_model), "term.labels"), value = TRUE)
grep("Part", attr(terms(bic_model), "term.labels"), value = TRUE)

# Build NewPart: Norrland (Part 3) vs the other two parts
df_new <- df_new |>
  mutate(NewPart = factor(ifelse(as.character(Part) == "3", "Norrland", "South"),
                          levels = c("South", "Norrland")))

table(df_new$NewPart)

# Model 3(f): Model 2(f) but with NewPart instead of Part
model_3f <- lm(log(PM10) ~ (log(Fuel) + log(GRP) + log(Income) +
                log(Vehicles) + log(Higheds) + log(Seniors)) * NewPart,
               data = df_new)

# Stepwise AIC with Model 3(f) as the new upper scope
aic_model_3f <- step(model_num_var,
                     scope = list(lower = model_null, upper = model_3f),
                     direction = "both",
                     trace = 0)

# Stepwise BIC with Model 3(f) as the new upper scope
bic_model_3f <- step(model_num_var,
                     scope = list(lower = model_null, upper = model_3f),
                     direction = "both",
                     k = log(n_new),
                     trace = 0)

cat("AIC-selected model (3f):\n"); print(formula(aic_model_3f))
cat("\nBIC-selected model (3f):\n"); print(formula(bic_model_3f))

aic_3f_tab <- summary(aic_model_3f)$coefficients[, c("Estimate", "Std. Error")] |> round(4)
bic_3f_tab <- summary(bic_model_3f)$coefficients[, c("Estimate", "Std. Error")] |> round(4)

knitr::kable(aic_3f_tab, caption = "3(f) AIC-selected model: beta-estimates and standard errors")
knitr::kable(bic_3f_tab, caption = "3(f) BIC-selected model: beta-estimates and standard errors")

model_list <- list(
  "Null"                       = model_null,
  "Model 1(b) (log Fuel)"      = model_log_fuel_new,
  "Model 2(c) (6 numerical)"   = model_num_var,
  "Model 2(f) (Part + int.)"   = interaction_model_new,
  "3(e) AIC"                   = aic_model,
  "3(e) BIC"                   = bic_model,
  "3(f) AIC (NewPart)"         = aic_model_3f,
  "3(f) BIC (NewPart)"         = bic_model_3f
)

comparison <- data.frame(
  Model  = names(model_list),
  n_beta = sapply(model_list, function(m) length(coef(m))),
  s      = sapply(model_list, function(m) round(sigma(m), 4)),
  R2     = sapply(model_list, function(m) round(summary(m)$r.squared, 4)),
  adjR2  = sapply(model_list, function(m) round(summary(m)$adj.r.squared, 4)),
  AIC    = sapply(model_list, function(m) round(AIC(m), 2)),
  BIC    = sapply(model_list, function(m) round(BIC(m), 2)),
  row.names = NULL
)

knitr::kable(comparison,
             caption = "3(g): model comparison on the reduced data set")

best_aic <- comparison$Model[which.min(comparison$AIC)]
best_bic <- comparison$Model[which.min(comparison$BIC)]
best_adj <- comparison$Model[which.max(comparison$adjR2)]

cat("Lowest AIC      :", best_aic, "\n")
cat("Lowest BIC      :", best_bic, "\n")
cat("Highest adj R^2 :", best_adj, "\n")

r2_fuel_only <- summary(model_log_fuel_new)$r.squared
r2_best      <- summary(model_list[[which.min(comparison$BIC)]])$r.squared
