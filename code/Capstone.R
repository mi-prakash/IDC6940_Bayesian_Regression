# All packages used in the analysis
pkgs <- c("dplyr", "ggplot2", "sf", "tigris", "rstanarm", "tidyr")

sapply(pkgs, function(x){
  if (!require(x, character.only = TRUE)) install.packages(x)
  library(x, character.only = TRUE)
})

# Load data
gsoy <- read.csv("/Users/miprakash/UWF/4 - Capsone Project/Project/IDC6940_Bayesian_Regression/code/4163416.csv")

gsoy$YEAR <- as.integer(substr(gsoy$DATE, 1, 4))

# Convert station coordinates into spatial data
stations_sf <- st_as_sf(gsoy, coords = c("LONGITUDE", "LATITUDE"), crs = 4326)

# -------------------------
# 1) Distribution of annual precipitation across stations (2024)
# -------------------------
ggplot(gsoy, aes(x = PRCP)) +
  geom_histogram(aes(y = after_stat(density)),
                 bins = 25,
                 color = "white",
                 fill = "steelblue",
                 alpha = 0.9) +
  geom_density(linewidth = 1, color = "black") +
  labs(
    title = "Distribution of Annual Precipitation (2024)",
    x = "Precipitation (mm)",
    y = "Density"
  ) +
  theme_minimal()

# ------------------------------------
# 2) Spatial visualization (Florida map)
# ------------------------------------
options(tigris_use_cache = TRUE)

florida <- states(cb = TRUE) |>
  filter(STUSPS == "FL") |>
  st_transform(4326)

ggplot() +
  geom_sf(data = florida, fill = "white", color = "black") +
  geom_sf(data = stations_sf, aes(color = PRCP), size = 1.5, alpha = 0.85) +
  scale_color_viridis_c(option = "roma", name = "Rainfall (mm)") +
  coord_sf(
    xlim = st_bbox(florida)[c("xmin","xmax")],
    ylim = st_bbox(florida)[c("ymin","ymax")]
  ) +
  labs(
    title = "Spatial Distribution of Average Annual Rainfall (Florida)",
    x = "Longitude",
    y = "Latitude"
  ) +
  theme_minimal()

# Keep only needed columns
gsoy <- gsoy %>%
  mutate(YEAR = as.integer(substr(DATE, 1, 4))) %>%
  select(STATION, LATITUDE, LONGITUDE, ELEVATION, YEAR, PRCP) %>%
  filter(
    !is.na(PRCP),
    !is.na(LATITUDE),
    !is.na(LONGITUDE),
    !is.na(ELEVATION)
  )

# -------------------------
# 3) Baseline Linear Regression
# -------------------------
lm_model <- lm(PRCP ~ LATITUDE + LONGITUDE + ELEVATION, data = gsoy)

summary(lm_model)

# Predictions
lm_pred <- predict(lm_model, gsoy)

# RMSE
rmse_lm <- sqrt(mean((gsoy$PRCP - lm_pred)^2))
rmse_lm

# -------------------------
# 4) Bayesian Linear Regression
# -------------------------
bayes_model <- stan_glm(
  PRCP ~ LATITUDE + LONGITUDE + ELEVATION,
  data = gsoy,
  family = gaussian(),
  prior = normal(0, 10),
  prior_intercept = normal(0, 10),
  chains = 4,
  iter = 2000,
  seed = 123
)

print(bayes_model)

# Posterior summary
summary(bayes_model)

# Credible intervals
posterior_interval(bayes_model)

# Posterior predictions
bayes_pred <- posterior_predict(bayes_model)

# Mean prediction
bayes_mean <- colMeans(bayes_pred)

# RMSE
rmse_bayes <- sqrt(mean((gsoy$PRCP - bayes_mean)^2))
rmse_bayes

rmse_lm
rmse_bayes

# -------------------------
# 5) Additional values used for results visuals
# -------------------------
gsoy$lm_fitted <- lm_pred
gsoy$bayes_fitted <- bayes_mean
gsoy$bayes_resid <- gsoy$PRCP - gsoy$bayes_fitted
gsoy$bayes_pred_sd <- apply(bayes_pred, 2, sd)

# Rebuild sf object from cleaned modeling data
stations_sf_model <- st_as_sf(gsoy, coords = c("LONGITUDE", "LATITUDE"), crs = 4326)

# -------------------------
# 6) Observed vs Predicted (both models)
# -------------------------
pred_df <- bind_rows(
  data.frame(
    observed = gsoy$PRCP,
    predicted = lm_pred,
    Model = "Linear Regression"
  ),
  data.frame(
    observed = gsoy$PRCP,
    predicted = bayes_mean,
    Model = "Bayesian Regression"
  )
)

ggplot(pred_df, aes(x = observed, y = predicted)) +
  geom_point(alpha = 0.7, color = "steelblue") +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
  facet_wrap(~ Model) +
  labs(
    title = "Observed vs Predicted Precipitation",
    x = "Observed PRCP (mm)",
    y = "Predicted PRCP (mm)"
  ) +
  theme_minimal()

# -------------------------
# 7) Coefficient comparison:
#    LM confidence intervals vs Bayesian credible intervals
# -------------------------

# Linear model coefficients and confidence intervals
lm_coef <- coef(summary(lm_model))
lm_ci <- confint(lm_model)

lm_coef_df <- data.frame(
  term = rownames(lm_coef),
  estimate = lm_coef[, "Estimate"],
  lower = lm_ci[, 1],
  upper = lm_ci[, 2],
  Model = "Linear Regression"
)

# Bayesian coefficients and credible intervals
bayes_ci <- posterior_interval(bayes_model, prob = 0.95)
bayes_ci <- bayes_ci[rownames(bayes_ci) != "sigma", ]

bayes_est <- coef(bayes_model)

bayes_coef_df <- data.frame(
  term = names(bayes_est),
  estimate = as.numeric(bayes_est),
  lower = bayes_ci[, 1],
  upper = bayes_ci[, 2],
  Model = "Bayesian Regression"
)

# Combine both and remove intercept for cleaner comparison
coef_df <- bind_rows(lm_coef_df, bayes_coef_df) %>%
  filter(term != "(Intercept)")

# Plot coefficient intervals
ggplot(coef_df, aes(x = term, y = estimate, ymin = lower, ymax = upper, color = Model)) +
  geom_pointrange(position = position_dodge(width = 0.5)) +
  coord_flip() +
  labs(
    title = "Coefficient Interval Comparison",
    x = "Predictor",
    y = "Estimate"
  ) +
  theme_minimal()

# -------------------------
# 8) Posterior distributions of Bayesian coefficients
# -------------------------
posterior_draws <- as.data.frame(as.matrix(bayes_model))

posterior_long <- posterior_draws %>%
  select(LATITUDE, LONGITUDE, ELEVATION) %>%
  pivot_longer(cols = everything(), names_to = "Parameter", values_to = "Value")

ggplot(posterior_long, aes(x = Value)) +
  geom_density(fill = "steelblue", alpha = 0.6, color = "black") +
  facet_wrap(~ Parameter, scales = "free") +
  labs(
    title = "Posterior Distributions of Bayesian Regression Coefficients",
    x = "Posterior Value",
    y = "Density"
  ) +
  theme_minimal()

# -------------------------
# 9) RMSE comparison chart
# -------------------------
rmse_df <- data.frame(
  Model = c("Linear Regression", "Bayesian Regression"),
  RMSE = c(rmse_lm, rmse_bayes)
)

ggplot(rmse_df, aes(x = Model, y = RMSE, fill = Model)) +
  geom_col(width = 0.6, show.legend = FALSE) +
  labs(
    title = "RMSE Comparison of Models",
    x = "Model",
    y = "RMSE"
  ) +
  theme_minimal()

# -------------------------
# 10) Spatial distribution of Bayesian residuals
# -------------------------
ggplot() +
  geom_sf(data = florida, fill = "gray95", color = "black") +
  geom_sf(data = stations_sf_model, aes(color = bayes_resid), size = 2) +
  scale_color_viridis_c(option = "cividis", name = "Residual") +
  labs(
    title = "Spatial Distribution of Bayesian Residuals"
  ) +
  theme_minimal() +
  coord_sf(expand = FALSE)

# -------------------------
# 11) Bayesian prediction uncertainty by station
# -------------------------
ggplot() +
  geom_sf(data = florida, fill = "gray95", color = "black") +
  geom_sf(data = stations_sf_model, aes(color = bayes_pred_sd), size = 2) +
  scale_color_viridis_c(option = "magma", name = "Posterior SD") +
  labs(
    title = "Bayesian Prediction Uncertainty by Station"
  ) +
  theme_minimal() +
  coord_sf(expand = FALSE)