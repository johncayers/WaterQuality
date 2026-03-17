# =============================================================================
# Diel carbon-isotope carbonate model
# - Tracks DIC concentration and 13C/12C inventories in CO2(aq), HCO3-, CO3--
# - Solves pH and speciation from DIC and TA each time step (freshwater)
# - Includes isotope fractionation for:
#     * Photosynthetic uptake (e_p)
#     * Gas exchange (invasion and evasion kinetic effects)
#     * Equilibrium among carbonate species (HCO3-, CO3-- heavier than CO2)
# - Air-water exchange computed from k and [CO2] disequilibrium (Henry's law)
#
# Units:
#   - Concentrations in mmol m^-3 (== µmol L^-1)
#   - Inventories in mmol m^-2 (per m^2 surface area, using depth)
#   - Fluxes in mmol m^-2 h^-1
#   - Time step = 1 hour (modifiable)
#
# References for constants/approximations (typical values used):
#   - Weiss (1974) for CO2 solubility (K0; mol L^-1 atm^-1)
#   - Freshwater carbonate: TA ~ [HCO3-] + 2[CO3--] + [OH-] - [H+]
#   - pK1 ~ 6.3, pK2 ~ 10.3 (25 °C, freshwater; simple representation)
#   - e(HCO3- - CO2) ~ +9 ‰, e(CO3-- - CO2) ~ +18 ‰ at ~25 °C
#   - Gas exchange kinetic fractionation: invasion ~ -1 ‰; evasion ~ +9 ‰
#
# NOTE:
#   This is a didactic but physically consistent model. For seawater or
#   high-precision work, replace (pK1,pK2,Kw) with T/S-dependent formulations
#   and include borate, phosphate, sulfate contributions to TA.
# =============================================================================

# ---------------------------
# 0) Helper: d <-> R (13C/12C)
# ---------------------------
R_STD <- 0.0111802  # VPDB ~ 0.01118

delta_to_R <- function(delta_permil) R_STD * (1 + delta_permil / 1000)
R_to_delta <- function(R) 1000 * (R / R_STD - 1)

# Fraction of 13C given R
f13_from_R <- function(R) R / (1 + R)

# ---------------------------
# 1) Physicochemical constants
# ---------------------------

# Weiss (1974) CO2 solubility in water/seawater (mol L^-1 atm^-1)
# T in °C, S = salinity (here we use S = 0 for freshwater)
K0_CO2_Weiss <- function(T_C, S = 0) {
  T_K <- T_C + 273.15
  A1 <- -58.0931; A2 <- 90.5069; A3 <- 22.2940
  B1 <- 0.027766; B2 <- -0.025888; B3 <- 0.0050578
  lnK0 <- A1 + A2 * (100 / T_K) + A3 * log(T_K / 100) +
    S * (B1 + B2 * (T_K / 100) + B3 * (T_K / 100)^2)
  exp(lnK0) # mol L^-1 atm^-1
}

# Simple freshwater carbonate constants at ~25 °C
pK1 <- 6.3
pK2 <- 10.3
pKw <- 14.0

K1 <- 10^(-pK1)
K2 <- 10^(-pK2)
Kw <- 10^(-pKw)

# Equilibrium isotope fractionation (25 °C, approximate)
# a_HCO3 = R_HCO3 / R_CO2 ; a_CO3 = R_CO3 / R_CO2
alpha_eq_HCO3 <- 1.009   # +9 ‰ heavier than CO2
alpha_eq_CO3  <- 1.018   # +18 ‰ heavier than CO2

# Gas-exchange kinetic isotope effects
eps_inv <- -1.0  # invasion: d entering = d_atm + e_inv  (‰)
eps_eva <- +9.0  # evasion: emitted d is ~9‰ lighter than aqueous CO2 (residual enriched)
alpha_inv <- 1 + eps_inv / 1000
alpha_eva <- 1 + eps_eva / 1000

# Photosynthetic fractionation (biomass vs CO2(aq))
eps_p <- -20.0
alpha_p <- 1 + eps_p / 1000

# ---------------------------
# 2) Carbonate speciation solver (freshwater)
# ---------------------------
# Given DIC (mol L^-1) and TA (mol L^-1), solve for [H+] via TA balance:
#   TA = [HCO3-] + 2[CO3--] + [OH-] - [H+]
# with
#   a0 = 1 / (1 + K1/H + K1*K2/H^2)
#   a1 = (K1/H)*a0 ; a2 = (K1*K2/H^2)*a0
#   [CO2] = a0*DIC ; [HCO3-] = a1*DIC ; [CO3--] = a2*DIC
#
speciate_carbonate <- function(DIC_mol_L, TA_mol_L, K1, K2, Kw) {
  f <- function(logH) {
    H <- 10^logH
    alpha0 <- 1 / (1 + K1 / H + K1 * K2 / H^2)
    alpha1 <- (K1 / H) * alpha0
    alpha2 <- (K1 * K2 / H^2) * alpha0
    CO2  <- alpha0 * DIC_mol_L
    HCO3 <- alpha1 * DIC_mol_L
    CO3  <- alpha2 * DIC_mol_L
    OH   <- Kw / H
    TA_calc <- HCO3 + 2 * CO3 + OH - H
    TA_calc - TA_mol_L
  }
  # Search pH range ~ 4–10
  root <- uniroot(f, interval = c(-10, -4))  # log10(H+)
  logH <- root$root
  H <- 10^logH
  pH <- -log10(H)
  alpha0 <- 1 / (1 + K1 / H + K1 * K2 / H^2)
  alpha1 <- (K1 / H) * alpha0
  alpha2 <- (K1 * K2 / H^2) * alpha0
  CO2  <- alpha0 * DIC_mol_L
  HCO3 <- alpha1 * DIC_mol_L
  CO3  <- alpha2 * DIC_mol_L
  list(pH = pH,
       CO2 = CO2, HCO3 = HCO3, CO3 = CO3,
       alpha = c(alpha0 = alpha0, alpha1 = alpha1, alpha2 = alpha2))
}

# ---------------------------
# 3) Isotope partition at equilibrium among species
# ---------------------------
# Given total N12 and N13 (mmol m^-2), and species fractions f_i (by moles),
# find R_CO2 such that:
#   R_HCO3 = R_CO2 * a_HCO3 ; R_CO3 = R_CO2 * a_CO3
# and species totals match (f_CO2*N_total, etc.) while N13 sums to N13_total.
#
isotope_equilibrium_partition <- function(N12_total, N13_total, f_CO2, f_HCO3, f_CO3,
                                          alpha_HCO3, alpha_CO3) {
  N_total <- N12_total + N13_total
  N_CO2  <- f_CO2  * N_total
  N_HCO3 <- f_HCO3 * N_total
  N_CO3  <- f_CO3  * N_total
  
  # Solve for R_CO2 by matching total N13
  f_target <- function(R_CO2) {
    R_HCO3 <- R_CO2 * alpha_HCO3
    R_CO3  <- R_CO2 * alpha_CO3
    f13_CO2  <- f13_from_R(R_CO2)
    f13_HCO3 <- f13_from_R(R_HCO3)
    f13_CO3  <- f13_from_R(R_CO3)
    N13_sum <- f13_CO2  * N_CO2  +
               f13_HCO3 * N_HCO3 +
               f13_CO3  * N_CO3
    N13_sum - N13_total
  }
  # Reasonable R range around R_STD
  sol <- uniroot(f_target, interval = c(R_STD * 0.5, R_STD * 1.5))
  R_CO2 <- sol$root
  R_HCO3 <- R_CO2 * alpha_HCO3
  R_CO3  <- R_CO2 * alpha_CO3
  
  # Species 13C and 12C inventories
  f13_CO2  <- f13_from_R(R_CO2)
  f13_HCO3 <- f13_from_R(R_HCO3)
  f13_CO3  <- f13_from_R(R_CO3)
  
  N13_CO2  <- f13_CO2  * N_CO2
  N13_HCO3 <- f13_HCO3 * N_HCO3
  N13_CO3  <- f13_CO3  * N_CO3
  
  N12_CO2  <- N_CO2  - N13_CO2
  N12_HCO3 <- N_HCO3 - N13_HCO3
  N12_CO3  <- N_CO3  - N13_CO3
  
  list(
    R_CO2 = R_CO2, R_HCO3 = R_HCO3, R_CO3 = R_CO3,
    N12 = c(CO2 = N12_CO2, HCO3 = N12_HCO3, CO3 = N12_CO3),
    N13 = c(CO2 = N13_CO2, HCO3 = N13_HCO3, CO3 = N13_CO3)
  )
}

# ---------------------------
# 4) Simulation setup
# ---------------------------

# Time grid
hours <- 0:24
dt_h <- 1

# Physical
depth_m <- 2
vol_m3_per_m2 <- depth_m

# Temperature and air
T_C <- 25
pCO2_uatm <- 420  # atmospheric pCO2 (µatm)
pCO2_atm  <- pCO2_uatm * 1e-6
K0_mol_L_atm <- K0_CO2_Weiss(T_C, S = 0)  # mol L^-1 atm^-1
CO2_eq_mol_L <- K0_mol_L_atm * pCO2_atm
CO2_eq_mmol_m3 <- CO2_eq_mol_L * 1000 * 1000  # to mmol m^-3

# Gas transfer velocity (choose a value or parameterize by wind)
k_m_d <- 2.0                 # m d^-1 (moderate turbulence)
k_m_h <- k_m_d / 24.0        # m h^-1

# Biogeochemistry
# Initial conditions
DIC0_mmol_m3 <- 2000         # 2 mmol L^-1
TA0_mmol_m3  <- 2200         # ~2.2 mmol L^-1
delta_DIC0   <- -6.5         # initial d13C-DIC (‰)

# Convert to inventories (per m^2)
DIC0_mmol_m2 <- DIC0_mmol_m3 * vol_m3_per_m2
TA0_mol_L    <- TA0_mmol_m3 / 1000   # for speciation solver
DIC0_mol_L   <- DIC0_mmol_m3 / 1000

# Initial isotope inventories from d
R_DIC0 <- delta_to_R(delta_DIC0)
f13_DIC0 <- f13_from_R(R_DIC0)
N_total0 <- DIC0_mmol_m2
N13_total <- f13_DIC0 * N_total0
N12_total <- N_total0 - N13_total

# Initial speciation and isotope partitioning at equilibrium
sp0 <- speciate_carbonate(DIC0_mol_L, TA0_mol_L, K1, K2, Kw)
f_CO2  <- sp0$alpha["alpha0"]
f_HCO3 <- sp0$alpha["alpha1"]
f_CO3  <- sp0$alpha["alpha2"]

iso0 <- isotope_equilibrium_partition(N12_total, N13_total,
                                      f_CO2, f_HCO3, f_CO3,
                                      alpha_eq_HCO3, alpha_eq_CO3)

# Initialize species inventories (mmol m^-2)
N12 <- iso0$N12
N13 <- iso0$N13

# Metabolism schedules (mmol C m^-2 h^-1)
R_hour <- rep(4, 24)
GPP_hour <- rep(0, 24)
GPP_hour[7:9]   <- 4
GPP_hour[10:15] <- 10
GPP_hour[16:18] <- 4

# Source/sink isotopic signatures
delta_atm <- -8.5
R_atm <- delta_to_R(delta_atm)

delta_R_OM <- -26
R_resp <- delta_to_R(delta_R_OM)

# ---------------------------
# 5) Storage for outputs
# ---------------------------
out <- data.frame(
  hour = hours,
  pH = NA_real_,
  DIC = NA_real_,      # mmol m^-3
  CO2 = NA_real_,
  HCO3 = NA_real_,
  CO3  = NA_real_,
  delta_DIC = NA_real_,
  delta_CO2 = NA_real_,
  delta_HCO3 = NA_real_,
  delta_CO3  = NA_real_,
  F_gas = NA_real_,    # mmol m^-2 h^-1 (positive = evasion, out of water)
  F_GPP = NA_real_,    # uptake (positive removal from water)
  F_R   = NA_real_     # addition (positive into water)
)

# ---------------------------
# 6) Time stepping
# ---------------------------
for (t in 1:24) {
  # Current totals (before fluxes at this hour)
  N12_total <- sum(N12)
  N13_total <- sum(N13)
  N_total   <- N12_total + N13_total
  
  # Compute speciation from current DIC and TA (TA assumed constant)
  DIC_mmol_m3 <- N_total / vol_m3_per_m2
  DIC_mol_L   <- DIC_mmol_m3 / 1000
  sp <- speciate_carbonate(DIC_mol_L, TA0_mol_L, K1, K2, Kw)
  f_CO2  <- sp$alpha["alpha0"]
  f_HCO3 <- sp$alpha["alpha1"]
  f_CO3  <- sp$alpha["alpha2"]
  pH <- sp$pH
  
  # Redistribute isotopes to instantaneous equilibrium among species
  iso <- isotope_equilibrium_partition(N12_total, N13_total,
                                       f_CO2, f_HCO3, f_CO3,
                                       alpha_eq_HCO3, alpha_eq_CO3)
  N12 <- iso$N12
  N13 <- iso$N13
  
  # Diagnostics for output
  R_CO2  <- iso$R_CO2
  R_HCO3 <- iso$R_HCO3
  R_CO3  <- iso$R_CO3
  
  delta_CO2  <- R_to_delta(R_CO2)
  delta_HCO3 <- R_to_delta(R_HCO3)
  delta_CO3  <- R_to_delta(R_CO3)
  delta_DIC  <- R_to_delta((N13_total / N12_total))
  
  # Concentrations (mmol m^-3) for output
  CO2_conc_mmol_m3  <- (N12["CO2"]  + N13["CO2"])  / vol_m3_per_m2
  HCO3_conc_mmol_m3 <- (N12["HCO3"] + N13["HCO3"]) / vol_m3_per_m2
  CO3_conc_mmol_m3  <- (N12["CO3"]  + N13["CO3"])  / vol_m3_per_m2
  
  # ---- Air-water CO2 exchange (based on aqueous CO2 only) ----
  CO2_eq <- CO2_eq_mmol_m3
  F_gas <- k_m_h * (CO2_conc_mmol_m3 - CO2_eq)      # + = evasion (out)
  # Isotopic composition of gas flux:
  if (F_gas > 0) {
    # Evasion: emitted CO2 is isotopically lighter than aqueous CO2
    R_emit <- R_CO2 / alpha_eva
    f13_emit <- f13_from_R(R_emit)
    F13_emit <- F_gas * f13_emit
    F12_emit <- F_gas - F13_emit
    # Remove from aqueous CO2 pool
    # Limit by availability
    avail_CO2 <- c(N12["CO2"], N13["CO2"])
    take12 <- min(F12_emit, avail_CO2[1])
    take13 <- min(F13_emit, avail_CO2[2])
    N12["CO2"] <- N12["CO2"] - take12
    N13["CO2"] <- N13["CO2"] - take13
  } else if (F_gas < 0) {
    # Invasion: incoming CO2 carries atmospheric ratio *alpha_inv
    R_in <- R_atm * alpha_inv
    f13_in <- f13_from_R(R_in)
    F_in <- -F_gas
    F13_in <- F_in * f13_in
    F12_in <- F_in - F13_in
    N12["CO2"] <- N12["CO2"] + F12_in
    N13["CO2"] <- N13["CO2"] + F13_in
  }
  
  # ---- Respiration adds CO2 (to aqueous CO2 pool) ----
  F_R <- R_hour[t]
  if (F_R > 0) {
    f13_R <- f13_from_R(R_resp)
    N13["CO2"] <- N13["CO2"] + F_R * f13_R
    N12["CO2"] <- N12["CO2"] + F_R * (1 - f13_R)
  }
  
  # ---- Photosynthesis removes CO2 (from aqueous CO2 pool) ----
  F_GPP <- GPP_hour[t]
  if (F_GPP > 0) {
    # Biomass ratio ~ R_CO2 / alpha_p (biomass is lighter than CO2 by e_p)
    R_bio <- R_CO2 / alpha_p
    f13_bio <- f13_from_R(R_bio)
    # Remove from available CO2 pool (with biomass isotopic composition)
    avail_CO2 <- c(N12["CO2"], N13["CO2"])
    rem13 <- min(F_GPP * f13_bio, avail_CO2[2])
    rem12 <- min(F_GPP * (1 - f13_bio), avail_CO2[1])
    N13["CO2"] <- N13["CO2"] - rem13
    N12["CO2"] <- N12["CO2"] - rem12
  }
  
  # ---- Store outputs (state AFTER fluxes this hour) ----
  N12_total <- sum(N12); N13_total <- sum(N13); N_total <- N12_total + N13_total
  DIC_mmol_m3 <- N_total / vol_m3_per_m2
  
  # Recompute d for reporting (before next equilibrium redistribution)
  R_CO2_post  <- (N13["CO2"] / N12["CO2"])
  delta_CO2_post <- R_to_delta(R_CO2_post)
  delta_DIC_post <- R_to_delta(N13_total / N12_total)
  
  out$hour[t]       <- t - 1
  out$pH[t]         <- pH
  out$DIC[t]        <- DIC_mmol_m3
  out$CO2[t]        <- CO2_conc_mmol_m3
  out$HCO3[t]       <- HCO3_conc_mmol_m3
  out$CO3[t]        <- CO3_conc_mmol_m3
  out$delta_DIC[t]  <- delta_DIC
  out$delta_CO2[t]  <- delta_CO2
  out$delta_HCO3[t] <- delta_HCO3
  out$delta_CO3[t]  <- delta_CO3
  out$F_gas[t]      <- F_gas
  out$F_GPP[t]      <- F_GPP
  out$F_R[t]        <- F_R
}

# Final point replicate last step for plotting continuity
out[nrow(out), c("hour","pH","DIC","CO2","HCO3","CO3",
                 "delta_DIC","delta_CO2","delta_HCO3","delta_CO3",
                 "F_gas","F_GPP","F_R")] <-
  out[nrow(out)-1, c("hour","pH","DIC","CO2","HCO3","CO3",
                     "delta_DIC","delta_CO2","delta_HCO3","delta_CO3",
                     "F_gas","F_GPP","F_R")]
out$hour[nrow(out)] <- 24

# ---------------------------
# 7) Quick plots (base R)
# ---------------------------
op <- par(mfrow = c(3,2), mar = c(4,4.5,2,1))

plot(out$hour, out$pH, type="l", lwd=2, col="steelblue",
     xlab="Hour", ylab="pH", main="pH")

plot(out$hour, out$DIC, type="l", lwd=2, col="black",
     xlab="Hour", ylab=expression("DIC (mmol m"^{-3}*")"), main="DIC")

matplot(out$hour, cbind(out$CO2, out$HCO3, out$CO3), type="l", lwd=2,
        col=c("firebrick","darkgreen","goldenrod"), lty=1,
        xlab="Hour", ylab=expression("Species (mmol m"^{-3}*")"),
        main="Carbonate Species")
legend("topright", c("CO2","HCO3-","CO3--"),
       col=c("firebrick","darkgreen","goldenrod"), lty=1, bty="n")

plot(out$hour, out$delta_DIC, type="l", lwd=2, col="black",
     xlab="Hour", ylab=expression(delta^{13}*C~"DIC (‰)"),
     main=expression(delta^{13}*C*" DIC"))

matplot(out$hour, cbind(out$delta_CO2, out$delta_HCO3, out$delta_CO3),
        type="l", lwd=2, col=c("firebrick","darkgreen","goldenrod"), lty=1,
        xlab="Hour", ylab=expression(delta^{13}*C~"(‰)"),
        main=expression(delta^{13}*C*" species"))
legend("bottomright", c(expression(CO[2]), expression(HCO[3]^"-"), expression(CO[3]^"2-")),
       col=c("firebrick","darkgreen","goldenrod"), lty=1, bty="n")

plot(out$hour, out$F_gas, type="h", lwd=3, col="gray40",
     xlab="Hour", ylab=expression("F"[gas]~"(mmol m"^{-2}*" h"^{-1}*")"),
     main="Gas Exchange (+ out)")

par(op)

# ---------------------------
# 8) Diagnostics
# ---------------------------
cat(sprintf("\nDaily totals: GPP = %.1f, R = %.1f mmol m^-2 d^-1\n",
            sum(GPP_hour), sum(R_hour)))
cat(sprintf("Mean pH: %.2f | Mean d13C-DIC: %.2f ‰\n",
            mean(out$pH[1:24]), mean(out$delta_DIC[1:24])))
cat(sprintf("Equilibrium [CO2] at %d µatm: %.1f mmol m^-3\n",
            pCO2_uatm, CO2_eq_mmol_m3))