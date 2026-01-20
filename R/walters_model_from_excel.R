# Sockeye retrospective model from Carl Walters excel model 
# January 2026
# Haley Oleynik 


# Reproduces spreadsheet logic for:
#   * Observed exploitation (Ut) and ENS
#   * Fitting ln(R/S) = ra - rb * S
#   * Retrospective predictionwith 4-year lag using historical residuals

# load packages 
suppressPackageStartupMessages({
  library(readxl)
  library(dplyr)
  library(tidyr)
  library(purrr)
})


# Configure  -----------------------------

# If you run this script from the command line, it will look for the Excel
# file in the same folder unless you edit this path.
INPUT_XLSX <- "Walters_model_simplified.xlsx"
SHEET      <- "Sheet1"

# Years used in the Excel regression ranges:
#   ra = INTERCEPT(Y4:Y71, W4:W71)
#   rb = -SLOPE(    Y4:Y71, W4:W71)
# Row 4 is year 1952, row 71 is 2019.
FIT_YEARS  <- 1952:2019

# Lag in rows for R/S in the spreadsheet (X4 = O8, i.e., +4 rows)
LAG_YEARS  <- 4

# Read observed data -----------------------------

# The main data table starts at row 3 (headers) and runs through year 2023.
# We read the first 9 columns shown in the sheet.
obs <- read_excel(
  path      = INPUT_XLSX,
  sheet     = SHEET,
  range     = "A3:I75",
  col_names = TRUE
) %>%
  rename(
    Year            = `Year`,
    AdultEscapement = `Sum of Adult Escapement`,
    JackEscapement  = `Sum of Jack Escapement`,
    TotalEscapement = `Sum of Total Escapement`,
    DBE             = `Sum of DBE`,
    BelowMissionC   = `Sum of Below Mission Catch (excluding Alaska catch)`,
    AboveMissionC   = `Sum of Above Mission Catch`,
    AlaskaCatch     = `Sum of Alaska Catch`,
    RunSize         = `Sum of Run Size`
  ) %>%
  mutate(Year = as.integer(Year)) %>%
  filter(!is.na(Year)) %>%
  arrange(Year)

# Set parameters ---------------------------

retroU <- 0.3
useretro <- 1 # 1 for true, 0 for false 
yrretro <- 1990 


#  Reproduce the observed-derived columns ---------------

obs2 <- obs %>%
  mutate(
    RunJacks = RunSize - JackEscapement,                     # O
    Catch    = BelowMissionC + AboveMissionC,                # P
    Ut_obs   = pmin(0.999, Catch / RunJacks),                # Q
    ENS      = pmin(1, pmax(0.0001, AdultEscapement / RunJacks / (1 - Ut_obs))),
    migmort  = 1 - ENS
  ) %>%
  # Adult return is Run-Jacks 4 years in the future
  mutate(
    AdultReturn = lead(RunJacks, n = LAG_YEARS),              # X
    lnR_S       = log(AdultReturn / AdultEscapement)         # Y
  )

# Fit a & b parameters ----------------------------------------
# using 1952-2019
fit_df <- obs2 %>%
  filter(Year %in% FIT_YEARS) %>%
  filter(is.finite(lnR_S), is.finite(AdultEscapement))

fit <- lm(lnR_S ~ AdultEscapement, data = fit_df)

ra <- unname(coef(fit)[[1]])
rb <- unname(-coef(fit)[[2]])  # Excel sets rb = -SLOPE(...)

obs3 <- obs2 %>%
  mutate(
    wt = lnR_S - (ra - rb * AdultEscapement)       # residuals
  )

# Retrospective predictions  -----------------------------

n <- nrow(obs3)

retroR <- rep(NA_real_, n)  # AA
retroU_vec <- if (useretro) ifelse(obs3$Year >= yrretro, retroU, obs3$Ut_obs) else obs3$Ut_obs  # AB
retroS <- rep(NA_real_, n)  # AC
retroC <- rep(NA_real_, n)  # AD

# Seed with observed RunJacks for first LAG_YEARS years
retroR[1:LAG_YEARS] <- obs3$RunJacks[1:LAG_YEARS]

# Compute spawners/catch for those seeded rows
retroS[1:LAG_YEARS] <- retroR[1:LAG_YEARS] * (1 - retroU_vec[1:LAG_YEARS]) * obs3$ENS[1:LAG_YEARS]
retroC[1:LAG_YEARS] <- retroR[1:LAG_YEARS] * retroU_vec[1:LAG_YEARS]

# Iterate forward
for (i in (LAG_YEARS + 1):n) {
  j <- i - LAG_YEARS
  # recruit/run from spawners 4 years prior
  retroR[i] <- retroS[j] * exp(ra - rb * retroS[j] + obs3$wt[j])
  retroS[i] <- retroR[i] * (1 - retroU_vec[i]) * obs3$ENS[i]
  retroC[i] <- retroR[i] * retroU_vec[i]
}

out <- obs3 %>%
  transmute(
    Year,
    AdultEscapement,
    JackEscapement,
    TotalEscapement,
    DBE,
    BelowMissionC,
    AboveMissionC,
    AlaskaCatch,
    RunSize,
    RunJacks,
    Catch,
    Ut_obs,
    ENS,
    migmort,
    AdultReturn,
    lnR_S,
    wt,
    retroR = retroR,
    retroU = retroU_vec,
    retroS = retroS,
    retroC = retroC
  )

# Save outputs -----------------------------

# message("Fitted parameters:")
# message(sprintf("  ra = %.8f", ra))
# message(sprintf("  rb = %.12f", rb))
# message("Retro toggles:")
# message(sprintf("  useretro = %s", useretro))
# message(sprintf("  retroU   = %.4f", retroU))
# message(sprintf("  yrretro  = %d", yrretro))
# 
# write.csv(out, "walters_model_outputs.csv", row.names = FALSE)
# message("Wrote: walters_model_outputs.csv")

