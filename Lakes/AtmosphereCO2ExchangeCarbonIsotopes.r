# ============================================================================
# Diel d13C-DIC box model with optional fitting of air–water CO2 exchange (F_aw)
# ----------------------------------------------------------------------------
# Assumptions:
# - Mixed layer box with fixed DIC pool (approximate) over 24 h
# - Isotope mass balance in d-units (linearized), suitable for small changes
# - GPP and R from diel O2 (provided here as a schedule)
# - Constant CO2 invasion from atmosphere with d_aw = d_atm + e_aw
# - Ignores carbonate speciation and temperature effects for clarity
# ============================================================================

# -------------------------
# 1) Parameters and inputs
# -------------------------

hours <- 0:24                         # 25 points for a 24-h cycle (hourly step)
dt <- 1                               # hours per step

# Mixed layer and DIC pool
DIC_mmol_per_L <- 2                   # mmol C L^-1
depth_m <- 2                          # m
liters_per_m2 <- depth_m * 1000       # L m^-2
pool_mmol_m2 <- DIC_mmol_per_L * liters_per_m2  # mmol C m^-2 (== 4000)

# Isotope constants (per mil)
delta_atm <- -8.5                     # d13C of atmospheric CO2 (‰)
eps_aw <- -1.0                        # air-water fractionation (invasion) (‰)
delta_aw <- delta_atm + eps_aw        # isotopic signature of invading CO2 (‰)
eps_p <- -20.0                        # photosynthetic fractionation (‰)
delta_R <- -26.0                      # d13C of respired OM (‰)

# Initial d13C-DIC at midnight
delta0 <- -6.5                        # ‰ at 00:00

# Diel metabolism schedule (mmol C m^-2 h^-1)
# Matches the narrative example: GPP=R on the day total, but varies hourly.
GPP <- rep(0, 24)
R   <- rep(4, 24)                     # respiration always 4

# Daytime GPP schedule
# 06–09: 4 ; 09–15: 10 ; 15–18: 4 ; others 0
GPP[7:9]  <- 4                        # hours 06-09 are indices 7-9 (since hours start at 0)
GPP[10:15] <- 10                      # hours 09-15 are 10-15
GPP[16:18] <- 4                       # hours 15-18 are 16-18

# Daily totals (sanity check)
GPP_total <- sum(GPP) * dt
R_total   <- sum(R)   * dt

cat(sprintf("Daily totals -> GPP: %.1f, R: %.1f (mmol C m^-2 d^-1)\n", GPP_total, R_total))

# Observed d13C-DIC at 3-hour intervals (from the example narrative)
obs_times <- seq(0, 24, by = 3)
obs_delta <- c(-6.5, -6.8, -7.0, -6.2, -5.0, -4.7, -5.4, -6.0, -6.5)


# ---------------------------------------------------
# 2) Box-model simulator for a given daily F_aw (>0)
# ---------------------------------------------------
# F_aw_d: daily total invasion (mmol C m^-2 d^-1), assumed constant over the day
simulate_delta <- function(F_aw_d,
                           delta_init = delta0,
                           pool = pool_mmol_m2,
                           GPP_hour = GPP,
                           R_hour = R,
                           eps_p = eps_p,
                           delta_R = delta_R,
                           delta_aw = delta_aw,
                           dt = 1) {
  stopifnot(length(GPP_hour) == 24, length(R_hour) == 24)
  # Convert daily F_aw to hourly
  F_aw_h <- F_aw_d / 24.0
  
  # Storage
  delta <- numeric(25)
  delta[1] <- delta_init
  
  # Time integration (explicit Euler on d*pool)
  for (t in 1:24) {
    # Current d of DIC
    dDIC <- delta[t]
    
    # Isotope mass balance (per hour):
    # d(d*pool)/dt = R*d_R - GPP*(d + e_p) + F_aw_h*(d_aw)
    d_delta_pool <- R_hour[t] * delta_R - GPP_hour[t] * (dDIC + eps_p) + F_aw_h * delta_aw
    
    # Convert to d change assuming pool ~ constant
    delta[t + 1] <- dDIC + (d_delta_pool / pool) * dt
  }
  
  data.frame(hour = 0:24, delta = delta)
}


# -----------------------------------------------------------------
# 3) Fit F_aw by least squares to the observed 3-hour d time series
# -----------------------------------------------------------------

# Helper to compute SSE for a candidate F_aw
sse_for_F <- function(F_aw_d) {
  sim <- simulate_delta(F_aw_d = F_aw_d)
  # Extract model at observation times
  model_at_obs <- sim$delta[match(obs_times, sim$hour)]
  sum((model_at_obs - obs_delta)^2)
}

# Search F_aw in a reasonable range (mmol C m^-2 d^-1)
fit <- optimize(sse_for_F, interval = c(0, 100))
F_aw_best <- fit$minimum
SSE_best <- fit$objective

cat(sprintf("Best-fit F_aw ˜ %.2f mmol C m^-2 d^-1 (SSE = %.3f)\n", F_aw_best, SSE_best))

# Simulate with best-fit F_aw and a reference value (e.g., 25 from the narrative)
sim_best <- simulate_delta(F_aw_d = F_aw_best)
sim_ref  <- simulate_delta(F_aw_d = 25)


# -----------------------------
# 4) Plot model vs observations
# -----------------------------
# Base R plot (no dependencies)

# Set plotting region
op <- par(mfrow = c(1, 1), mar = c(4.5, 5, 3, 2))

plot(sim_best$hour, sim_best$delta, type = "l", lwd = 2, col = "steelblue",
     xlab = "Hour of day",
     ylab = expression(paste(delta^{13}, "C-DIC (‰)")),
     main = expression(paste("Diel ", delta^{13}, "C-DIC: Model vs Observations")),
     ylim = range(c(sim_best$delta, obs_delta, sim_ref$delta)))

lines(sim_ref$hour, sim_ref$delta, lwd = 2, col = "gray50", lty = 2)

points(obs_times, obs_delta, pch = 19, col = "firebrick")
lines(obs_times, obs_delta, col = "firebrick", lwd = 1, lty = 3)

legend("topright",
       legend = c(
         sprintf("Model (best-fit F_aw = %.1f)", F_aw_best),
         "Model (reference F_aw = 25)",
         "Observations (3-h)")
       ),
       col = c("steelblue", "gray50", "firebrick"),
       lty = c(1, 2, 3), lwd = c(2, 2, 1), pch = c(NA, NA, 19), bty = "n")

par(op)

# -----------------------------
# 5) Print simple diagnostics
# -----------------------------
cat("\nDiagnostics:\n")
cat(sprintf("  Mixed-layer DIC pool: %.0f mmol m^-2\n", pool_mmol_m2))
cat(sprintf("  d_aw (d_atm + e_aw):  %.1f ‰  (atm = %.1f, e_aw = %.1f)\n", delta_aw, delta_atm, eps_aw))
cat(sprintf("  e_p (photosynthesis): %.1f ‰,  d_R (respired OM): %.1f ‰\n", eps_p, delta_R))
cat(sprintf("  Diel d amplitude (obs): %.1f ‰\n",
            max(obs_delta) - min(obs_delta)))
cat(sprintf("  Diel d amplitude (best-fit model): %.1f ‰\n",
            max(sim_best$delta) - min(sim_best$delta)))

# -------------------------------------------------------------------------
# (Optional) If you want to explore evasion instead of invasion:
#   - Set F_aw_d negative (e.g., -25) and set d_aw accordingly.
#   - For evasion, the effective isotopic expression is typically +8 to +10 ‰.
#     You can set delta_aw <- +9 for a first-order approximation and allow F_aw_d < 0.
# -------------------------------------------------------------------------