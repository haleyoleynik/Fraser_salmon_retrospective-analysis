# Retrospective Model 
# Haley Oleynik Murdoch McAllister
# October 2025

# load libraries 
require(tidyverse)
require(ggplot2)
require(readr)
require(slider)

# read data ------------------
data <- read_csv("data/s-r_data.csv")
covariates <- read_csv("data/covariates.csv")

# calculations ----------------
# from Fraser Chum data v19_alpha_SSL_est_fin_yrs_v8 sheet 

# estimate the coefficients 
# calculate alpha with coefficients using this formula: 

# coefficients (from lnrs model)
intercept = 1.03737862843252
pdo_adult_coef = 0.093
npgo_coef = 0.103
pdo_smolt_coef = -0.106
ssl_coef = -0.224
spawners_coef = -4.95E-07

# control settings to change 
U_apply = 0.2
bycatch_rate = 0.67
SSL_control = 0 

# mutate dataframe with calculations from exel 
new.data <- data %>%
  left_join(covariates, by = "Year") %>%
  arrange(Year) %>%  # ensure chronological order
  mutate(
    catch = chum_total_stock - chum_spawners,
    U_chum = catch / chum_total_stock,
    base_alpha = intercept + PDO_adult * pdo_adult_coef + NPGO * npgo_coef +
      PDO_smolt * pdo_smolt_coef + SSL * ssl_coef,
    alpha_running_avg = slide_dbl(alpha, mean, .before = 9, .complete = TRUE),
    model_recruits = chum_spawners * exp(alpha + chum_spawners * spawners_coef),
    ln_obs_pred = log(chum_recruits_obs / model_recruits),
    Nage3_obs = chum_total_stock * prop3,
    Nage4_obs = chum_total_stock * prop4,
    Nage5_obs = chum_total_stock * prop5,
    Nage6_obs = chum_total_stock * prop6,
    Nage3_pred = case_when(          # column X 
      Year <= 1953 ~ Nage3_obs,
      Year >= 1954 ~ lag(model_recruits, 3) * prop3 * exp(lag(ln_obs_pred, 3))),
    Nage4_pred = case_when(
      Year <= 1954 ~ Nage4_obs,
      Year >= 1955 ~ lag(model_recruits, 4) * prop4 * exp(lag(ln_obs_pred, 4))),
    Nage5_pred = case_when(
      Year <= 1955 ~ Nage5_obs,
      Year >= 1956 ~ lag(model_recruits, 5) * prop5 * exp(lag(ln_obs_pred, 5))), # why ln_obs_pred here? 
    Nage6_pred = case_when(
      Year <= 1956 ~ Nage6_obs,
      Year >= 1957 ~ lag(model_recruits, 6) * prop6 * exp(lag(ln_obs_pred, 6))),
    recruits_pred = rowSums(across(c(Nage3_pred, Nage4_pred, Nage5_pred, Nage6_pred)), na.rm = TRUE),
    recruits_dif = chum_total_stock - recruits_pred, # deviation from the obs total stock
    U_chum_pred = catch / recruits_pred,     # U calc 
    U_chum_dif = U_chum_pred - U_chum,       # U dif 
    chum_commercial_harvest = case_when(
      Year <= 1990 ~ U_chum_pred,
      Year >= 1991 ~ U_chum),
    chum_commercial_harvest_uapply = case_when(
      Year <= 1990 ~ U_chum_pred,
      Year >= 1991 ~ U_apply))

# need to define chum_spawners_pred !!!!!!!!!!!!!!!!
# uses recruits that are based

# set 1978 value 
SSL_1978 <- new.data %>%
  filter(Year == 1978) %>%
  pull(SSL)

scenario <- new.data %>%
  arrange(Year) %>%  
  mutate(
    chum_recruits_alt = ifelse(  #column AU 
    SSLcontrol == 0,
    chum_spawners_pred * exp(base_alpha + spawners_coef * chum_spawners_pred) * exp(ln_obs_pred),
    chum_spawners_pred * exp(CY14 + spawners_coef * chum_spawners_pred) * exp(ln_obs_pred)),
    SSL_alt =  case_when(   # column AS 
      Year <= 1978 ~ SSL,
      Year >= 1979 ~ (1-SSLcontrol)*SSL+SSLcontrol*SSL_1978),
    SSL_control_alpha = intercept + PDO_adult * pdo_adult_coef + NPGO * npgo_coef +   # column CY 
      PDO_smolt * pdo_smolt_coef + SSL_alt * ssl_coef,
    Nage3_alt = case_when(   # column AI 
      Year <= 1953 ~ Nage3_pred,
      Year >= 1954 ~ lag(chum_recruits_alt,3) * prop3) ,
    Nage4_alt = case_when(
      Year <= 1954 ~ Nage4_pred,
      Year >= 1955 ~ lag(chum_recruits_alt,4) * prop4),
    Nage5_alt = case_when(
      Year <= 1955 ~ Nage5_pred,
      Year >= 1956 ~ lag(chum_recruits_alt,5) * prop5), 
    Nage6_alt = case_when(
      Year <= 1956 ~ Nage6_pred,
      Year >= 1957 ~ lag(chum_recruits_alt,6) * prop6),
    sum_alt = rowSums(across(c(Nage3_alt, Nage4_alt, Nage5_alt, Nage6_alt)), na.rm = TRUE), # column AM
    )

# stopped at chum catch, AM based on AH, U_alt -- new column? 
