# Try AI 

# set fixed parameters: 
SSL_control <- 0
U_historic  <- 1
byrate      <- 0.69

sh_thompson_SSL_1978 <- all_data %>%
  filter(Year == 1978) %>%
  pull(sh_thompson_SL) %>%
  as.numeric()

FN_thompson_2018 <- all_data %>%
  filter(Year == 2018) %>%
  pull(sh_thompson_FN_mortalities) %>%
  as.numeric()

# initalize state 
df <- all_data %>%
  arrange(Year) %>%
  filter(!is.na(Year)) %>%
  mutate(
    sh_thompson_base_alpha = # column BM
      sh_thompson_intercept +
      sh_thompson_SST * sh_thompson_sst_coef +
      sh_thompson_SL  * sh_thompson_ssl_coef +
      sh_thompson_NPGO * sh_thompson_npgo_coef,
    
    sh_thompson_model_recruits = # column BN 
      sh_thompson_spawners *
      exp(sh_thompson_base_alpha +
            sh_thompson_spawners * sh_thompson_spawners_coef),
    
    sh_thompson_ln_obs_pred = # column BO 
      log(sh_thompson_recruits / sh_thompson_model_recruits),
    
    sh_thompson_pred_bycatch = # column BU 
      sh_thompson_prefishery_N -
      sh_thompson_sport_mortalities -
      sh_thompson_FN_mortalities -
      1000 * sh_thompson_spawners,
    
    sh_thompson_U =    # column BS and BT 
      sh_thompson_pred_bycatch / sh_thompson_prefishery_N,
    
    # state variables (empty)
    sh_thompson_recruits_alt = NA_real_,
    sh_thompson_spawners_pred = sh_thompson_spawners,
    
    sh_thompson_Nage4_pred = NA_real_,
    sh_thompson_Nage5_pred = NA_real_,
    sh_thompson_Nage6_pred = NA_real_,
    sh_thompson_Nage7_pred = NA_real_,
    sh_thompson_Nage8_pred = NA_real_,
    
    sh_thompson_SSL_alt = NA_real_,
    sh_thompson_SSL_alpha = NA_real_,
    sh_thompson_sum_pred = NA_real_,
    sh_thompson_bycatch_pred = NA_real_,
    sh_thompson_FN_catch_pred = NA_real_,
    sh_thompson_total_catch_pred = NA_real_,
    sh_thompson_U_comm = NA_real_
  )

# forward simulation loop 
for (i in seq_len(nrow(df))) {
  
  yr <- df$Year[i]
  
  # --- commercial U (column BZ) ---
  df$sh_thompson_U_comm[i] <-
    if (yr <= 1990) {
      df$sh_thompson_U[i]
    } else if (U_historic == 1) {
      df$sh_thompson_U[i]
    } else {
      byrate * df$chum_commercial_harvest_uapply[i] 
    }
  
  # --- SSL ---
  SSL_alt <- if (yr <= 1978) {
    df$sh_thompson_SL[i]
  } else {
    (1 - SSL_control) * df$sh_thompson_SL[i] +
      SSL_control * sh_thompson_SSL_1978
  }
  
  df$sh_thompson_SSL_alt[i] <- SSL_alt
  
  # --- SSL alpha (column CT) ---
  df$sh_thompson_SSL_alpha[i] <-
    sh_thompson_intercept +
    df$sh_thompson_SST[i]  * sh_thompson_sst_coef +
    df$sh_thompson_NPGO[i] * sh_thompson_npgo_coef +
    SSL_alt               * sh_thompson_ssl_coef
  
  # --- recruits (thousands) ---
  df$sh_thompson_recruits_alt[i] <-
    if (SSL_control == 0) {
      df$sh_thompson_spawners_pred[i] / 1000 *
        exp(df$sh_thompson_base_alpha[i] +
              sh_thompson_spawners_coef * df$sh_thompson_spawners_pred[i] / 1000)
    } else {
      df$sh_thompson_spawners_pred[i] / 1000 *
        exp(df$sh_thompson_SSL_alpha[i] +
              sh_thompson_spawners_coef * df$sh_thompson_spawners_pred[i] / 1000)
    }
  
  # --- age structure ---
  # if (i > 4) df$sh_thompson_Nage4_pred[i] <- df$sh_thompson_recruits_alt[i-4] * df$sh_thompson_p4[i] * 1000
  # if (i > 5) df$sh_thompson_Nage5_pred[i] <- df$sh_thompson_recruits_alt[i-5] * df$sh_thompson_p5[i] * 1000
  # if (i > 6) df$sh_thompson_Nage6_pred[i] <- df$sh_thompson_recruits_alt[i-6] * df$sh_thompson_p6[i] * 1000
  # if (i > 7) df$sh_thompson_Nage7_pred[i] <- df$sh_thompson_recruits_alt[i-7] * df$sh_thompson_p7[i] * 1000
  # if (i > 8) df$sh_thompson_Nage8_pred[i] <- df$sh_thompson_recruits_alt[i-8] * df$sh_thompson_p8[i] * 1000
  
  # --- age structure ---
  if (i <= 4) {
    df$sh_thompson_Nage4_pred[i] <-
      df$sh_thompson_prefishery_N[i] * df$sh_thompson_p4[i]
  } else {
    df$sh_thompson_Nage4_pred[i] <-
      df$sh_thompson_recruits_alt[i-4] * df$sh_thompson_p4[i] * 1000
  }
  
  if (i <= 5) {
    df$sh_thompson_Nage5_pred[i] <-
      df$sh_thompson_prefishery_N[i] * df$sh_thompson_p5[i]
  } else {
    df$sh_thompson_Nage5_pred[i] <-
      df$sh_thompson_recruits_alt[i-5] * df$sh_thompson_p5[i] * 1000
  }
  
  if (i <= 6) {
    df$sh_thompson_Nage6_pred[i] <-
      df$sh_thompson_prefishery_N[i] * df$sh_thompson_p6[i]
  } else {
    df$sh_thompson_Nage6_pred[i] <-
      df$sh_thompson_recruits_alt[i-6] * df$sh_thompson_p6[i] * 1000
  }
  
  if (i <= 7) {
    df$sh_thompson_Nage7_pred[i] <-
      df$sh_thompson_prefishery_N[i] * df$sh_thompson_p7[i]
  } else {
    df$sh_thompson_Nage7_pred[i] <-
      df$sh_thompson_recruits_alt[i-7] * df$sh_thompson_p7[i] * 1000
  }
  
  if (i <= 8) {
    df$sh_thompson_Nage8_pred[i] <-
      df$sh_thompson_prefishery_N[i] * df$sh_thompson_p8[i]
  } else {
    df$sh_thompson_Nage8_pred[i] <-
      df$sh_thompson_recruits_alt[i-8] * df$sh_thompson_p8[i] * 1000
  }
  
  
  # --- totals ---
  df$sh_thompson_sum_pred[i] <-
    sum(df$sh_thompson_Nage4_pred[i],
        df$sh_thompson_Nage5_pred[i],
        df$sh_thompson_Nage6_pred[i],
        df$sh_thompson_Nage7_pred[i],
        df$sh_thompson_Nage8_pred[i],
        na.rm = TRUE)
  
  df$sh_thompson_bycatch_pred[i] <-
    df$sh_thompson_sum_pred[i] * df$sh_thompson_U_comm[i] #column CI
  
  # --- FN scaling (capture 2018 values internally) ---
  if (yr <= 2018) {
    df$sh_thompson_FN_catch_pred[i] <- df$sh_thompson_FN_mortalities[i]
  } else {
    df$sh_thompson_FN_catch_pred[i] <-
      FN_thompson_2018 /
      (df$sh_thompson_sum_pred[df$Year == 2018] -
         df$sh_thompson_bycatch_pred[df$Year == 2018]) *
      (df$sh_thompson_sum_pred[i] -
         df$sh_thompson_bycatch_pred[i])
  }
  
  df$sh_thompson_total_catch_pred[i] <-
    df$sh_thompson_FN_catch_pred[i] +
    df$sh_thompson_sport_mortalities[i] +
    df$sh_thompson_bycatch_pred[i]
  
  df$sh_thompson_spawners_pred[i] <-
    df$sh_thompson_sum_pred[i] -
    df$sh_thompson_total_catch_pred[i]
}

# CHECKS - corssref with spreadsheet 
# sh_thompson_total_catch_pred - column CJ - WRONG 
# sh_thompson_bycatch_pred - column CI - WRONG
# sh_thompson_sum_pred - column CF - WRONG 


# sh_thompson_sport_mortalities - column CH - right
# sh_thompson_FN_catch_pred - column CG - right


# sh_thompson_spawners_pred - column CM - WRONG
# sh_thompson_recruits_alt = WRONG (and spawners) 

# wrong because total catch pred is wrong because bycatch pred is wrong because sum pred is wrong

# sh_thompson_prefishery_N

# NOTES 
# add in CT82 -- projections of alpha (based on averages)
# base alpha slightly off - fix 
# need to add Uchum grid (BW) and Ush grid (BX) 
# do projections for alpha (starting CN104)

# STOPPED - checking through sh df -- somethings are right, but some are off 

##########################################

## Try a second time -----------------------------------------------
# set fixed parameters:
SSL_control <- 0
U_historic  <- 1
byrate      <- 0.69

start_year  <- 1978  # <-- key change

sh_thompson_SSL_1978 <- all_data %>%
  dplyr::filter(Year == 1978) %>%
  dplyr::pull(sh_thompson_SL) %>%
  as.numeric()

FN_thompson_2018 <- all_data %>%
  dplyr::filter(Year == 2018) %>%
  dplyr::pull(sh_thompson_FN_mortalities) %>%
  as.numeric()

# initialize state
df <- all_data %>%
  dplyr::arrange(Year) %>%
  dplyr::filter(!is.na(Year)) %>%
  dplyr::mutate(
    sh_thompson_base_alpha =
      sh_thompson_intercept +
      sh_thompson_SST  * sh_thompson_sst_coef +
      sh_thompson_SL   * sh_thompson_ssl_coef +
      sh_thompson_NPGO * sh_thompson_npgo_coef,
    
    sh_thompson_model_recruits =
      sh_thompson_spawners *
      exp(sh_thompson_base_alpha +
            sh_thompson_spawners * sh_thompson_spawners_coef),
    
    sh_thompson_ln_obs_pred =
      log(sh_thompson_recruits / sh_thompson_model_recruits),
    
    sh_thompson_pred_bycatch =
      sh_thompson_prefishery_N -
      sh_thompson_sport_mortalities -
      sh_thompson_FN_mortalities -
      1000 * sh_thompson_spawners,
    
    sh_thompson_U =
      sh_thompson_pred_bycatch / sh_thompson_prefishery_N,
    
    # state variables (empty)
    sh_thompson_recruits_alt   = NA_real_,
    sh_thompson_spawners_pred  = sh_thompson_spawners,
    
    sh_thompson_Nage4_pred = NA_real_,
    sh_thompson_Nage5_pred = NA_real_,
    sh_thompson_Nage6_pred = NA_real_,
    sh_thompson_Nage7_pred = NA_real_,
    sh_thompson_Nage8_pred = NA_real_,
    
    sh_thompson_SSL_alt        = NA_real_,
    sh_thompson_SSL_alpha      = NA_real_,
    sh_thompson_sum_pred       = NA_real_,
    sh_thompson_bycatch_pred   = NA_real_,
    sh_thompson_FN_catch_pred  = NA_real_,
    sh_thompson_total_catch_pred = NA_real_,
    sh_thompson_U_comm         = NA_real_
  )

# identify start index (first row with Year >= start_year)
start_i <- which(df$Year >= start_year)[1]

# forward simulation loop
for (i in seq(from = start_i, to = nrow(df))) {
  
  yr <- df$Year[i]
  
  # --- commercial U (column BZ) ---
  df$sh_thompson_U_comm[i] <-
    if (yr <= 1990) {
      df$sh_thompson_U[i]
    } else if (U_historic == 1) {
      df$sh_thompson_U[i]
    } else {
      byrate * df$chum_commercial_harvest_uapply[i]
    }
  
  # if U is missing even after 1978, don't let it nuke the run
  # (optional: choose NA vs 0; here we keep NA)
  if (is.na(df$sh_thompson_U_comm[i])) {
    df$sh_thompson_U_comm[i] <- NA_real_
  }
  
  # --- SSL ---
  SSL_alt <- if (yr <= 1978) {
    df$sh_thompson_SL[i]
  } else {
    (1 - SSL_control) * df$sh_thompson_SL[i] +
      SSL_control * sh_thompson_SSL_1978
  }
  df$sh_thompson_SSL_alt[i] <- SSL_alt
  
  # --- SSL alpha (column CT) ---
  df$sh_thompson_SSL_alpha[i] <-
    sh_thompson_intercept +
    df$sh_thompson_SST[i]  * sh_thompson_sst_coef +
    df$sh_thompson_NPGO[i] * sh_thompson_npgo_coef +
    SSL_alt                * sh_thompson_ssl_coef
  
  # --- recruits (thousands) ---
  df$sh_thompson_recruits_alt[i] <-
    if (SSL_control == 0) {
      df$sh_thompson_spawners_pred[i] / 1000 *
        exp(df$sh_thompson_base_alpha[i] +
              sh_thompson_spawners_coef * df$sh_thompson_spawners_pred[i] / 1000)
    } else {
      df$sh_thompson_spawners_pred[i] / 1000 *
        exp(df$sh_thompson_SSL_alpha[i] +
              sh_thompson_spawners_coef * df$sh_thompson_spawners_pred[i] / 1000)
    }
  
  # --- age structure ---
  # Use "years since start_year" rather than i<=k
  # Need recruits from (yr - age), so earliest modeled N4 is start_year+4, etc.
  if (yr < start_year + 4) {
    df$sh_thompson_Nage4_pred[i] <- df$sh_thompson_prefishery_N[i] * df$sh_thompson_p4[i]
  } else {
    df$sh_thompson_Nage4_pred[i] <- df$sh_thompson_recruits_alt[which(df$Year == (yr - 4))] * df$sh_thompson_p4[i] * 1000
  }
  
  if (yr < start_year + 5) {
    df$sh_thompson_Nage5_pred[i] <- df$sh_thompson_prefishery_N[i] * df$sh_thompson_p5[i]
  } else {
    df$sh_thompson_Nage5_pred[i] <- df$sh_thompson_recruits_alt[which(df$Year == (yr - 5))] * df$sh_thompson_p5[i] * 1000
  }
  
  if (yr < start_year + 6) {
    df$sh_thompson_Nage6_pred[i] <- df$sh_thompson_prefishery_N[i] * df$sh_thompson_p6[i]
  } else {
    df$sh_thompson_Nage6_pred[i] <- df$sh_thompson_recruits_alt[which(df$Year == (yr - 6))] * df$sh_thompson_p6[i] * 1000
  }
  
  if (yr < start_year + 7) {
    df$sh_thompson_Nage7_pred[i] <- df$sh_thompson_prefishery_N[i] * df$sh_thompson_p7[i]
  } else {
    df$sh_thompson_Nage7_pred[i] <- df$sh_thompson_recruits_alt[which(df$Year == (yr - 7))] * df$sh_thompson_p7[i] * 1000
  }
  
  if (yr < start_year + 8) {
    df$sh_thompson_Nage8_pred[i] <- df$sh_thompson_prefishery_N[i] * df$sh_thompson_p8[i]
  } else {
    df$sh_thompson_Nage8_pred[i] <- df$sh_thompson_recruits_alt[which(df$Year == (yr - 8))] * df$sh_thompson_p8[i] * 1000
  }
  
  # --- totals ---
  df$sh_thompson_sum_pred[i] <-
    sum(df$sh_thompson_Nage4_pred[i],
        df$sh_thompson_Nage5_pred[i],
        df$sh_thompson_Nage6_pred[i],
        df$sh_thompson_Nage7_pred[i],
        df$sh_thompson_Nage8_pred[i],
        na.rm = TRUE)
  
  df$sh_thompson_bycatch_pred[i] <-
    df$sh_thompson_sum_pred[i] * df$sh_thompson_U_comm[i]
  
  # --- FN scaling ---
  if (yr <= 2018) {
    df$sh_thompson_FN_catch_pred[i] <- df$sh_thompson_FN_mortalities[i]
  } else {
    # safer to compute the 2018 denominator once (but keeping your structure)
    denom_2018 <- (df$sh_thompson_sum_pred[df$Year == 2018] -
                     df$sh_thompson_bycatch_pred[df$Year == 2018])
    
    df$sh_thompson_FN_catch_pred[i] <-
      FN_thompson_2018 / denom_2018 *
      (df$sh_thompson_sum_pred[i] - df$sh_thompson_bycatch_pred[i])
  }
  
  df$sh_thompson_total_catch_pred[i] <-
    df$sh_thompson_FN_catch_pred[i] +
    df$sh_thompson_sport_mortalities[i] +
    df$sh_thompson_bycatch_pred[i]
  
  df$sh_thompson_spawners_pred[i] <-
    df$sh_thompson_sum_pred[i] -
    df$sh_thompson_total_catch_pred[i]
}


## Old steelhead code --------------------------------
# sh_new_data <- sh_data %>%
#   arrange(Year) %>%  # ensure chronological order
#   mutate(
#     sh_thompson_base_alpha = sh_thompson_intercept  +
#       sh_thompson_SST * sh_thompson_sst_coef + sh_thompson_SL * sh_thompson_ssl_coef + sh_thompson_NPGO * sh_thompson_npgo_coef, # column BM
#     sh_thompson_model_recruits = sh_thompson_spawners * exp(sh_thompson_base_alpha + sh_thompson_spawners * sh_thompson_spawners_coef), # column BN
#     sh_thompson_ln_obs_pred = log(sh_thompson_recruits / sh_thompson_model_recruits), # column BO
#     sh_thompson_pred_bycatch = sh_thompson_prefishery_N - sh_thompson_sport_mortalities - sh_thompson_FN_mortalities - 1000 * sh_thompson_spawners, # column BU
#     sh_thompson_U = sh_thompson_pred_bycatch/sh_thompson_prefishery_N, # column BS
# sh_thompson_U_commercial = case_when( # column BZ
#     Year <= 1990 ~ sh_thompson_U,
#     Year >= 1991 ~ ifelse(  
#       U_historic == 1,
#       sh_thompson_U,  # column BR
#       byrate*chum_commercial_harvest_uapply)),
# sh_thompson_recruits_alt = case_when(
#   SSL_control == 0 ~ sh_thompson_spawners_pred / 1000 * exp(sh_thompson_base_alpha + sh_thompson_spawners_coef * sh_thompson_spawners_pred / 1000),
#   SSL_control == 1 ~ sh_thompson_spawners_pred / 1000 * exp(sh_thompson_SSL_control_alpha + sh_thompson_spawners_coef * sh_thompson_spawners_pred / 1000)
# ),             # column CO 
# sh_thompson_Nage4_pred = case_when(      # column CA 
#   Year <= 2018 ~ sh_thompson_prefishery_N * sh_thompson_p4,
#   Year >= 2019 ~ lag(sh_thompson_recruits_alt,4) * sh_thompson_p4 * 1000),
# sh_thompson_Nage5_pred = case_when(
#   Year <= 1955 ~ sh_thompson_prefishery_N * sh_thompson_p5,
#   Year >= 1956 ~ lag(sh_thompson_recruits_alt,5) * sh_thompson_p5 * 1000),
# sh_thompson_Nage6_pred = case_when(
#   Year <= 1956 ~ sh_thompson_prefishery_N * sh_thompson_p6,
#   Year >= 1957 ~ lag(sh_thompson_recruits_alt,6) * sh_thompson_p6 * 1000),
# sh_thompson_Nage7_pred = case_when(
#   Year <= 1957 ~ sh_thompson_prefishery_N * sh_thompson_p7,
#   Year >= 1958 ~ lag(sh_thompson_recruits_alt,7) * sh_thompson_p7 * 1000),
# sh_thompson_Nage8_pred = case_when(
#   Year <= 1958 ~ sh_thompson_prefishery_N * sh_thompson_p8,
#   Year >= 1959 ~ lag(sh_thompson_recruits_alt,8) * sh_thompson_p8 * 1000),
# sh_thompson_sum_pred = rowSums(across(c(sh_thompson_Nage4_pred, sh_thompson_Nage5_pred, sh_thompson_Nage6_pred, sh_thompson_Nage7_pred, sh_thompson_Nage8_pred)), na.rm = TRUE), # column CF
# sh_thompson_bycatch_pred = sh_thompson_sum_pred * sh_thompson_U_commercial, # column CI
# sh_thompson_FN_catch_pred = case_when( # column CG 
#   Year <= 2018 ~ sh_thompson_FN_mortalities,
#   Year >= 2019 ~ FN_thompson_2018/(sh_thompson_sum_pred_2018 - sh_thompson_bycatch_2018)*(sh_thompson_sum_pred - sh_thompson_bycatch_pred)),
# sh_thompson_sport_catch_pred = sh_thompson_sport_mortalities, # column CH
# sh_thompson_total_catch_pred = sh_thompson_FN_catch_pred + sh_thompson_sport_catch_pred + sh_thompson_bycatch_pred,         # column CJ
# sh_thompson_F_pred = sh_thompson_FN_catch_pred/(sh_thompson_sum_pred - sh_thompson_bycatch_pred), # column CK 
# sh_thompson_fishing_mortality =  sh_thompson_total_catch_pred/sh_thompson_sum_pred,  # column CL
# sh_thompson_spawners_pred = sh_thompson_sum_pred - sh_thompson_total_catch_pred, # column CM
# sh_thompson_SSL_alt =  case_when(   # column AS (look at delta pred)
#   Year <= 1978 ~ sh_thompson_SSL,
#   Year >= 1979 ~ (1-SSL_control) * sh_thompson_SSL + SSL_control*sh_thompson_SSL_1978), # column CQ
# sh_thompson_SSL_control_alpha = sh_thompson_intercept  +
#   sh_thompson_SST * sh_thompson_sst_coef + sh_thompson_SSL_alt * sh_thompson_ssl_coef # column CT 
#   )
# 
# sh_thompson_total_catch_pred
# sh_thompson_FN_catch_pred
# sh_thompson_spawners_pred
# sh_thompson_recruits_alt
# 
# 
# # FN mortalities 
# FN_thompson_2018 <- sh_new_data %>%
#   filter(Year == 2018) %>%
#   pull(sh_thompson_FN_mortalities)
# 
# # 1978 SSL 
# sh_thompson_SSL_1978 <- sh_new_data %>%
#   filter(Year == 1978) %>%
#   pull(sh_thompson_SL)
# 
# # total stock in 2018 (CF)
# sh_thompson_sum_pred_2018 <- sh_scenarios %>%
#   filter(Year == 2018) %>%
#   pull(sh_thompson_sum_pred)
# 
# # bycatch 2018
# sh_thompson_bycatch_2018 <- sh_scenarios %>%
#   filter(Year == 2018) %>%
#   pull(sh_thompson_bycatch_pred)
# 
# # total stock in 2018 (CF)
# sh_thompson_sum_pred_2018 <- sh_scenarios %>%
#   filter(Year == 2018) %>%
#   pull(sh_thompson_sum_pred)
# 
# # bycatch 2018
# sh_thompson_bycatch_2018 <- sh_scenarios %>%
#   filter(Year == 2018) %>%
#   pull(sh_thompson_bycatch_pred)

# old steelhead -----------------------------------------------------------
## Thompson ---------------
# coefficients (from lnrs model)
# can estimate these directly in r and then input (to do)
sh_thompson_intercept = 1.572107637
sh_thompson_sst_coef = -0.203463091
sh_thompson_ssl_coef = -0.764277677
sh_thompson_npgo_coef = -0.0402
sh_thompson_spawners_coef = -0.804438793

# add controls 
U_historic = 1
byrate = 0.6


# set fixed parameters:
SSL_control <- 0
U_historic  <- 1
byrate      <- 0.69

start_year  <- 1978  # <-- key change

sh_thompson_SSL_1978 <- all_data %>%
  dplyr::filter(Year == 1978) %>%
  dplyr::pull(sh_thompson_SL) %>%
  as.numeric()

FN_thompson_2018 <- all_data %>%
  dplyr::filter(Year == 2018) %>%
  dplyr::pull(sh_thompson_FN_mortalities) %>%
  as.numeric()

# initialize state
df <- all_data %>%
  dplyr::arrange(Year) %>%
  dplyr::filter(!is.na(Year)) %>%
  dplyr::mutate(
    sh_thompson_base_alpha =
      sh_thompson_intercept +
      sh_thompson_SST  * sh_thompson_sst_coef +
      sh_thompson_SL   * sh_thompson_ssl_coef +
      sh_thompson_NPGO * sh_thompson_npgo_coef,
    
    sh_thompson_model_recruits =
      sh_thompson_spawners *
      exp(sh_thompson_base_alpha +
            sh_thompson_spawners * sh_thompson_spawners_coef),
    
    sh_thompson_ln_obs_pred =
      log(sh_thompson_recruits / sh_thompson_model_recruits),
    
    sh_thompson_pred_bycatch =
      sh_thompson_prefishery_N -
      sh_thompson_sport_mortalities -
      sh_thompson_FN_mortalities -
      1000 * sh_thompson_spawners,
    
    sh_thompson_U =
      sh_thompson_pred_bycatch / sh_thompson_prefishery_N,
    
    # state variables (empty)
    sh_thompson_recruits_alt   = NA_real_,
    sh_thompson_spawners_pred  = sh_thompson_spawners,
    
    sh_thompson_Nage4_pred = NA_real_,
    sh_thompson_Nage5_pred = NA_real_,
    sh_thompson_Nage6_pred = NA_real_,
    sh_thompson_Nage7_pred = NA_real_,
    sh_thompson_Nage8_pred = NA_real_,
    
    sh_thompson_SSL_alt        = NA_real_,
    sh_thompson_SSL_alpha      = NA_real_,
    sh_thompson_sum_pred       = NA_real_,
    sh_thompson_bycatch_pred   = NA_real_,
    sh_thompson_FN_catch_pred  = NA_real_,
    sh_thompson_total_catch_pred = NA_real_,
    sh_thompson_U_comm         = NA_real_,
    df$sh_thompson_alpha_CN <- NA_real_ # add this to try 
  )

# identify start index (first row with Year >= start_year)
start_i <- which(df$Year >= start_year)[1]

# forward simulation loop
for (i in seq(from = start_i, to = nrow(df))) {
  
  yr <- df$Year[i]
  
  # --- commercial U (column BZ) ---
  df$sh_thompson_U_comm[i] <-
    if (yr <= 1990) {
      df$sh_thompson_U[i]
    } else if (U_historic == 1) {
      df$sh_thompson_U[i]
    } else {
      byrate * df$chum_commercial_harvest_uapply[i]
    }
  
  # if U is missing even after 1978, don't let it nuke the run
  # (optional: choose NA vs 0; here we keep NA)
  if (is.na(df$sh_thompson_U_comm[i])) {
    df$sh_thompson_U_comm[i] <- NA_real_
  }
  
  # --- SSL ---
  SSL_alt <- if (yr <= 1978) {
    df$sh_thompson_SL[i]
  } else {
    (1 - SSL_control) * df$sh_thompson_SL[i] +
      SSL_control * sh_thompson_SSL_1978
  }
  df$sh_thompson_SSL_alt[i] <- SSL_alt
  
  # --- SSL alpha (column CT) ---
  df$sh_thompson_SSL_alpha[i] <-
    sh_thompson_intercept +
    df$sh_thompson_SST[i]  * sh_thompson_sst_coef +
    df$sh_thompson_NPGO[i] * sh_thompson_npgo_coef +
    SSL_alt                * sh_thompson_ssl_coef
  
  # CN-equivalent (Excel CN): uses *observed SL* for that year
  df$sh_thompson_alpha_CN[i] <-
    sh_thompson_intercept +
    df$sh_thompson_SST[i]  * sh_thompson_sst_coef +
    df$sh_thompson_NPGO[i] * sh_thompson_npgo_coef +
    df$sh_thompson_SL[i]   * sh_thompson_ssl_coef
  
  # --- recruits (thousands) ---
  # df$sh_thompson_recruits_alt[i] <-
  #   if (SSL_control == 0) {
  #     df$sh_thompson_spawners_pred[i] / 1000 *
  #       exp(df$sh_thompson_base_alpha[i] +
  #             sh_thompson_spawners_coef * df$sh_thompson_spawners_pred[i] / 1000)
  #   } else {
  #     df$sh_thompson_spawners_pred[i] / 1000 *
  #       exp(df$sh_thompson_SSL_alpha[i] +
  #             sh_thompson_spawners_coef * df$sh_thompson_spawners_pred[i] / 1000)
  #   }
  
  
  spk <- df$sh_thompson_spawners_pred[i] / 1000  # CM41/1000
  
  df$sh_thompson_recruits_alt[i] <-
    if (SSL_control == 0) {
      spk * exp(df$sh_thompson_alpha_CN[i] +
                  sh_thompson_spawners_coef * spk)  # BE10 * CM41/1000
    } else {
      spk * exp(df$sh_thompson_SSL_alpha[i] +
                  sh_thompson_spawners_coef * spk)  # CT41 + BE10*...
    }
  
  # --- age structure ---
  # Use "years since start_year" rather than i<=k
  # Need recruits from (yr - age), so earliest modeled N4 is start_year+4, etc.
  if (yr < start_year + 4) {
    df$sh_thompson_Nage4_pred[i] <- df$sh_thompson_prefishery_N[i] * df$sh_thompson_p4[i]
  } else {
    df$sh_thompson_Nage4_pred[i] <- df$sh_thompson_recruits_alt[which(df$Year == (yr - 4))] * df$sh_thompson_p4[i] * 1000
  }
  
  if (yr < start_year + 5) {
    df$sh_thompson_Nage5_pred[i] <- df$sh_thompson_prefishery_N[i] * df$sh_thompson_p5[i]
  } else {
    df$sh_thompson_Nage5_pred[i] <- df$sh_thompson_recruits_alt[which(df$Year == (yr - 5))] * df$sh_thompson_p5[i] * 1000
  }
  
  if (yr < start_year + 6) {
    df$sh_thompson_Nage6_pred[i] <- df$sh_thompson_prefishery_N[i] * df$sh_thompson_p6[i]
  } else {
    df$sh_thompson_Nage6_pred[i] <- df$sh_thompson_recruits_alt[which(df$Year == (yr - 6))] * df$sh_thompson_p6[i] * 1000
  }
  
  if (yr < start_year + 7) {
    df$sh_thompson_Nage7_pred[i] <- df$sh_thompson_prefishery_N[i] * df$sh_thompson_p7[i]
  } else {
    df$sh_thompson_Nage7_pred[i] <- df$sh_thompson_recruits_alt[which(df$Year == (yr - 7))] * df$sh_thompson_p7[i] * 1000
  }
  
  if (yr < start_year + 8) {
    df$sh_thompson_Nage8_pred[i] <- df$sh_thompson_prefishery_N[i] * df$sh_thompson_p8[i]
  } else {
    df$sh_thompson_Nage8_pred[i] <- df$sh_thompson_recruits_alt[which(df$Year == (yr - 8))] * df$sh_thompson_p8[i] * 1000
  }
  
  # --- totals ---
  df$sh_thompson_sum_pred[i] <-
    sum(df$sh_thompson_Nage4_pred[i],
        df$sh_thompson_Nage5_pred[i],
        df$sh_thompson_Nage6_pred[i],
        df$sh_thompson_Nage7_pred[i],
        df$sh_thompson_Nage8_pred[i],
        na.rm = TRUE)
  
  df$sh_thompson_bycatch_pred[i] <-  # column CI
    df$sh_thompson_sum_pred[i] * df$sh_thompson_U_comm[i]
  
  # --- FN scaling ---
  if (yr <= 2018) {
    df$sh_thompson_FN_catch_pred[i] <- df$sh_thompson_FN_mortalities[i]
  } else {
    # safer to compute the 2018 denominator once (but keeping your structure)
    denom_2018 <- (df$sh_thompson_sum_pred[df$Year == 2018] -
                     df$sh_thompson_bycatch_pred[df$Year == 2018])
    
    df$sh_thompson_FN_catch_pred[i] <-
      FN_thompson_2018 / denom_2018 *
      (df$sh_thompson_sum_pred[i] - df$sh_thompson_bycatch_pred[i])
  }
  
  df$sh_thompson_total_catch_pred[i] <- # column CJ
    df$sh_thompson_FN_catch_pred[i] +
    df$sh_thompson_sport_mortalities[i] +
    df$sh_thompson_bycatch_pred[i]
  
  df$sh_thompson_spawners_pred[i] <-
    df$sh_thompson_sum_pred[i] -
    df$sh_thompson_total_catch_pred[i]
}

# NOTES ----------------------------------------------------------
# old notes from before I could get it running 
# cell references to cross-check 

# CHECKS - corssref with spreadsheet 
# sh_thompson_total_catch_pred - column CJ - WRONG 
# sh_thompson_bycatch_pred - column CI - WRONG
# sh_thompson_sum_pred - column CF - WRONG 


# sh_thompson_sport_mortalities - column CH - right
# sh_thompson_FN_catch_pred - column CG - right


# sh_thompson_spawners_pred - column CM - WRONG
# sh_thompson_recruits_alt = WRONG (and spawners) 

# wrong because total catch pred is wrong because bycatch pred is wrong because sum pred is wrong

# sh_thompson_prefishery_N

# NOTES 
# add in CT82 -- projections of alpha (based on averages)
# base alpha slightly off - fix 
# need to add Uchum grid (BW) and Ush grid (BX) 
# do projections for alpha (starting CN104)





## Thompson ---------------
# coefficients (from lnrs model)
sh_thompson_intercept      <- 1.572107637
sh_thompson_sst_coef       <- -0.203463091
sh_thompson_ssl_coef       <- -0.764277677
sh_thompson_npgo_coef      <- -0.0402
sh_thompson_spawners_coef  <- -0.804438793

# controls (set ONCE)
SSL_control <- 0
U_historic  <- 1
byrate      <- 0.69
start_year  <- 1978

# constants pulled from data
sh_thompson_SSL_1978 <- all_data %>%
  filter(Year == 1978) %>%
  pull(sh_thompson_SL) %>%
  as.numeric()

FN_thompson_2018 <- all_data %>%
  filter(Year == 2018) %>%
  pull(sh_thompson_FN_mortalities) %>%
  as.numeric()

# initialize state
df <- all_data %>%
  arrange(Year) %>%
  filter(!is.na(Year)) %>%
  mutate(
    sh_thompson_base_alpha =
      sh_thompson_intercept +
      sh_thompson_SST  * sh_thompson_sst_coef +
      sh_thompson_SL   * sh_thompson_ssl_coef +
      sh_thompson_NPGO * sh_thompson_npgo_coef,
    
    sh_thompson_model_recruits =
      sh_thompson_spawners *
      exp(sh_thompson_base_alpha +
            sh_thompson_spawners * sh_thompson_spawners_coef),
    
    sh_thompson_ln_obs_pred =
      log(sh_thompson_recruits / sh_thompson_model_recruits),
    
    sh_thompson_pred_bycatch =
      sh_thompson_prefishery_N -
      sh_thompson_sport_mortalities -
      sh_thompson_FN_mortalities -
      1000 * sh_thompson_spawners,
    
    sh_thompson_U =
      sh_thompson_pred_bycatch / sh_thompson_prefishery_N,
    
    # state variables (empty)
    sh_thompson_recruits_alt      = NA_real_,
    sh_thompson_spawners_pred     = sh_thompson_spawners,
    
    sh_thompson_Nage4_pred = NA_real_,
    sh_thompson_Nage5_pred = NA_real_,
    sh_thompson_Nage6_pred = NA_real_,
    sh_thompson_Nage7_pred = NA_real_,
    sh_thompson_Nage8_pred = NA_real_,
    
    sh_thompson_SSL_alt           = NA_real_,
    sh_thompson_SSL_alpha         = NA_real_,
    sh_thompson_alpha_CN          = NA_real_,   # <-- correct way to add
    sh_thompson_sum_pred          = NA_real_,
    sh_thompson_bycatch_pred      = NA_real_,
    sh_thompson_FN_catch_pred     = NA_real_,
    sh_thompson_total_catch_pred  = NA_real_,
    sh_thompson_U_comm            = NA_real_
  )

# start index
start_i <- which(df$Year >= start_year)[1]

# build a fast year->row lookup (assumes one row per Year)
year_to_i <- setNames(seq_len(nrow(df)), df$Year)

# forward simulation loop
for (i in seq(from = start_i, to = nrow(df))) {
  
  yr <- df$Year[i]
  
  # --- commercial U (column BZ) ---
  df$sh_thompson_U_comm[i] <-
    if (yr <= 1990) {
      df$sh_thompson_U[i]
    } else if (U_historic == 1) {
      df$sh_thompson_U[i]
    } else {
      byrate * df$chum_commercial_harvest_uapply[i]
    }
  
  # --- SSL ---
  df$sh_thompson_SSL_alt[i] <- if (yr <= 1978) {
    df$sh_thompson_SL[i]
  } else {
    (1 - SSL_control) * df$sh_thompson_SL[i] + SSL_control * sh_thompson_SSL_1978
  }
  
  # --- SSL alpha (CT) ---
  df$sh_thompson_SSL_alpha[i] <-
    sh_thompson_intercept +
    df$sh_thompson_SST[i]  * sh_thompson_sst_coef +
    df$sh_thompson_NPGO[i] * sh_thompson_npgo_coef +
    df$sh_thompson_SSL_alt[i] * sh_thompson_ssl_coef
  
  # --- CN-equivalent (Excel CN): uses observed SL ---
  df$sh_thompson_alpha_CN[i] <-
    sh_thompson_intercept +
    df$sh_thompson_SST[i]  * sh_thompson_sst_coef +
    df$sh_thompson_NPGO[i] * sh_thompson_npgo_coef +
    df$sh_thompson_SL[i]   * sh_thompson_ssl_coef
  
  # --- recruits (Excel: CM/1000 * EXP(CN + BE10*CM/1000) OR CT + BE10*CM/1000) ---
  spk <- df$sh_thompson_spawners_pred[i] / 1000
  
  df$sh_thompson_recruits_alt[i] <-
    if (SSL_control == 0) {
      spk * exp(df$sh_thompson_alpha_CN[i] +
                  sh_thompson_spawners_coef * spk)
    } else {
      spk * exp(df$sh_thompson_SSL_alpha[i] +
                  sh_thompson_spawners_coef * spk)
    }
  
  # helper to safely get lagged recruits by brood year
  lag_recruits <- function(lag_year) {
    j <- year_to_i[as.character(lag_year)]
    if (is.na(j)) NA_real_ else df$sh_thompson_recruits_alt[j]
  }
  
  # --- age structure ---
  if (yr < start_year + 4) {
    df$sh_thompson_Nage4_pred[i] <- df$sh_thompson_prefishery_N[i] * df$sh_thompson_p4[i]
  } else {
    df$sh_thompson_Nage4_pred[i] <- lag_recruits(yr - 4) * df$sh_thompson_p4[i] * 1000
  }
  
  if (yr < start_year + 5) {
    df$sh_thompson_Nage5_pred[i] <- df$sh_thompson_prefishery_N[i] * df$sh_thompson_p5[i]
  } else {
    df$sh_thompson_Nage5_pred[i] <- lag_recruits(yr - 5) * df$sh_thompson_p5[i] * 1000
  }
  
  if (yr < start_year + 6) {
    df$sh_thompson_Nage6_pred[i] <- df$sh_thompson_prefishery_N[i] * df$sh_thompson_p6[i]
  } else {
    df$sh_thompson_Nage6_pred[i] <- lag_recruits(yr - 6) * df$sh_thompson_p6[i] * 1000
  }
  
  if (yr < start_year + 7) {
    df$sh_thompson_Nage7_pred[i] <- df$sh_thompson_prefishery_N[i] * df$sh_thompson_p7[i]
  } else {
    df$sh_thompson_Nage7_pred[i] <- lag_recruits(yr - 7) * df$sh_thompson_p7[i] * 1000
  }
  
  if (yr < start_year + 8) {
    df$sh_thompson_Nage8_pred[i] <- df$sh_thompson_prefishery_N[i] * df$sh_thompson_p8[i]
  } else {
    df$sh_thompson_Nage8_pred[i] <- lag_recruits(yr - 8) * df$sh_thompson_p8[i] * 1000
  }
  
  # --- totals ---
  df$sh_thompson_sum_pred[i] <-
    sum(df$sh_thompson_Nage4_pred[i],
        df$sh_thompson_Nage5_pred[i],
        df$sh_thompson_Nage6_pred[i],
        df$sh_thompson_Nage7_pred[i],
        df$sh_thompson_Nage8_pred[i],
        na.rm = TRUE)
  
  df$sh_thompson_bycatch_pred[i] <-
    df$sh_thompson_sum_pred[i] * df$sh_thompson_U_comm[i]
  
  # --- FN scaling ---
  if (yr <= 2018) {
    df$sh_thompson_FN_catch_pred[i] <- df$sh_thompson_FN_mortalities[i]
  } else {
    denom_2018 <- (df$sh_thompson_sum_pred[df$Year == 2018] -
                     df$sh_thompson_bycatch_pred[df$Year == 2018])
    
    df$sh_thompson_FN_catch_pred[i] <-
      FN_thompson_2018 / denom_2018 *
      (df$sh_thompson_sum_pred[i] - df$sh_thompson_bycatch_pred[i])
  }
  
  df$sh_thompson_total_catch_pred[i] <-     # CJ
    df$sh_thompson_FN_catch_pred[i] +       # CG 
    df$sh_thompson_sport_mortalities[i] +   # CH
    df$sh_thompson_bycatch_pred[i]          # CI 
  
  df$sh_thompson_spawners_pred[i] <-
    df$sh_thompson_sum_pred[i] -
    df$sh_thompson_total_catch_pred[i]
}

# CHUM outtakes -------------------------------------------------
# scenario <- new.data %>%
#   arrange(Year) %>%  
#   mutate(
#     chum_recruits_alt = ifelse(  #column AU 
#       SSL_control == 0,
#     chum_spawners_pred * exp(chum_base_alpha + spawners_coef * chum_spawners_pred) * exp(chum_ln_obs_pred),
#     chum_spawners_pred * exp(SSL_control_alpha + spawners_coef * chum_spawners_pred) * exp(chum_ln_obs_pred)),
#     SSL_alt =  case_when(   # column AS (look at delta pred)
#       Year <= 1978 ~ SSL,
#       Year >= 1979 ~ (1-SSL_control)*SSL+SSL_control*SSL_1978),
#     SSL_control_alpha = intercept + PDO_adult * pdo_adult_coef + NPGO * npgo_coef +   # column AT & CY 
#       PDO_smolt * pdo_smolt_coef + SSL_alt * ssl_coef,
#     Nage3_alt = case_when(   # column AI 
#       Year <= 1953 ~ Nage3_pred,
#       Year >= 1954 ~ lag(chum_recruits_alt,3) * prop3) ,
#     Nage4_alt = case_when(
#       Year <= 1954 ~ Nage4_pred,
#       Year >= 1955 ~ lag(chum_recruits_alt,4) * prop4),
#     Nage5_alt = case_when(
#       Year <= 1955 ~ Nage5_pred,
#       Year >= 1956 ~ lag(chum_recruits_alt,5) * prop5), 
#     Nage6_alt = case_when(
#       Year <= 1956 ~ Nage6_pred,
#       Year >= 1957 ~ lag(chum_recruits_alt,6) * prop6),
#     sum_alt = rowSums(across(c(Nage3_alt, Nage4_alt, Nage5_alt, Nage6_alt)), na.rm = TRUE), # column AM
#     catch_alt = sum_alt * U_chum_pred,        # column AN
#     chum_spawners_pred = catch_alt - sum_alt  # column AO 
#     )


# Chum notes 
################################

# LEFT OFF -- check to see if this works - particularly these calculations in these columns: 
# sum_alt = rowSums(across(c(Nage3_alt, Nage4_alt, Nage5_alt, Nage6_alt)), na.rm = TRUE), # column AM
# catch_alt = sum_alt * U_chum_pred,        # column AN
# chum_spawners_pred = catch_alt - sum_alt  # column AO 

####################################

## Chilcotin ---------------

# coefficients (from lnrs model)
sh_chilcotin_intercept = 1.053608979
sh_chilcotin_sst_coef = -0.127949278
sh_chilcotin_ssl_coef = -0.792741195
sh_chilcotin_NPGO_coef = 0.152526045
sh_chilcotin_PDO_coef = 0.202708011
sh_chilcotin_spawners_coef = -1.022467631


sh_new_data <- sh_data %>%
  arrange(Year) %>%  # ensure chronological order
  mutate(
    sh_chilcotin_base_alpha = sh_chilcotin_intercept  +
      sh_chilcotin_SST * sh_chilcotin_sst_coef + sh_chilcotin_SL * sh_chilcotin_ssl_coef + sh_chilcotin_NPGO * sh_chilcotin_NPGO_coef + sh_chilcotin_PDO * sh_chilcotin_PDO_coef,
    sh_chilcotin_model_recruits = sh_chilcotin_spawners * exp(sh_chilcotin_base_alpha + sh_chilcotin_spawners * sh_chilcotin_spawners_coef),
    sh_chilcotin_ln_obs_pred = log(sh_chilcotin_recruits / sh_chilcotin_model_recruits),
    sh_chilcotin_pred_bycatch = sh_chilcotin_prefishery_N - sh_chilcotin_sport_mortalities - sh_chilcotin_FN_mortalities - 1000 * sh_chilcotin_spawners,
    sh_chilcotin_U = sh_chilcotin_pred_bycatch/sh_chilcotin_prefishery_N
  )

# need to add Uchum grid (BW) and Ush grid (BX) 

# add controls 
U_historic = 1
byrate = 0.6

# FN mortalities 
FN_chilcotin_2018 <- sh_new_data %>%
  filter(Year == 2018) %>%
  pull(sh_chilcotin_FN_mortalities)


sh_scenarios <- sh_new_data %>%
  mutate(sh_chilcotin_U_commercial = case_when( # column BZ
    Year <= 1990 ~ sh_chilcotin_U,
    Year >= 1991 ~ ifelse(  
      U_historic == 1,
      st_chilcotin_U,  # column BR
      byrate*chum_commercial_harvest_uapply)), 
    sh_chilcotin_recruits_alt = ,             # column CO 
    sh_chilcotin_Nage4_pred = case_when(      # column CA 
      Year <= 2018 ~ sh_chilcotin_prefishery_N * sh_chilcotin_p4,
      Year >= 2019 ~ lag(sh_chilcotin_recruits_alt,4) * sh_chilcotin_p4 * 1000),
    sh_chilcotin_Nage5_pred = case_when(
      Year <= 1955 ~ sh_chilcotin_prefishery_N * sh_chilcotin_p5,
      Year >= 1956 ~ lag(sh_chilcotin_recruits_alt,5) * sh_chilcotin_p5 * 1000),
    sh_chilcotin_Nage6_pred = case_when(
      Year <= 1956 ~ sh_chilcotin_prefishery_N * sh_chilcotin_p6,
      Year >= 1957 ~ lag(sh_chilcotin_recruits_alt,6) * sh_chilcotin_p6 * 1000),
    sh_chilcotin_Nage7_pred = case_when(
      Year <= 1957 ~ sh_chilcotin_prefishery_N * sh_chilcotin_p7,
      Year >= 1958 ~ lag(sh_chilcotin_recruits_alt,7) * sh_chilcotin_p7 * 1000),
    sh_chilcotin_Nage8_pred = case_when(
      Year <= 1958 ~ sh_chilcotin_prefishery_N * sh_chilcotin_p8,
      Year >= 1959 ~ lag(sh_chilcotin_recruits_alt,8) * sh_chilcotin_p8 * 1000),
    sh_chilcotin_sum_pred = rowSums(across(c(sh_chilcotin_Nage4_pred, sh_chilcotin_Nage5_pred, sh_chilcotin_Nage6_pred, sh_chilcotin_Nage7_pred, sh_chilcotin_Nage8_pred)), na.rm = TRUE), # column CF
    sh_chilcotin_bycatch_pred = sh_chilcotin_sum_pred * sh_chilcotin_U_commercial)# column CI


# bycatch 2018
sh_chilcotin_bycatch_2018 <- sh_scenarios %>%
  filter(Year == 2018) %>%
  pull(sh_chilcotin_bycatch_pred)

# Total stock 2018 -- this is duplicate 
sh_chilcotin_total_stock_pred_2018 <- sh_scenarios %>%
  filter(Year == 2018) %>%
  pull(sh_chilcotin_total_stock_pred)

# 1978 SSL 
sh_chilcotin_SSL_1978 <- sh_new_data %>%
  filter(Year == 1978) %>%
  pull(sh_chilcotin_SL)

sh_scenarios %>%
  mutate(sh_chilcotin_FN_catch_pred = case_when( # column EL 
    Year <= 2018 ~ sh_chilcotin_FN_mortalities,
    Year >= 2019 ~ FN_chilcotin_2018/(sh_chilcotin_sum_pred_2018 - sh_chilcotin_bycatch_2018)*(sh_chilcotin_sum_pred - sh_chilcotin_bycatch_pred)),
    sh_chilcotin_sport_catch_pred = sh_chilcotin_sport_mortalities, # column EM
    sh_chilcotin_total_catch_pred = sh_chilcotin_FN_catch_pred + sh_chilcotin_sport_catch_pred + sh_chilcotin_bycatch_pred,         # column EO
    # sh_chilcotin_F_pred = sh_chilcotin_FN_catch_pred/(sh_chilcotin_sum_pred - sh_chilcotin_bycatch_pred), # (not in chilcotin)  
    sh_chilcotin_fishing_mortality =  sh_chilcotin_total_catch_pred/sh_chilcotin_sum_pred,  # column EP
    sh_chilcotin_spawners_pred = sh_chilcotin_sum_pred - sh_chilcotin_total_catch_pred, # column EQ
    sh_chilcotin_SSL_alt =  case_when(   
      Year <= 1978 ~ sh_chilcotin_SSL,
      Year >= 1979 ~ (1-SSL_control) * sh_chilcotin_SSL + SSL_control*sh_chilcotin_SSL_1978), # column EU
    sh_chilcotin_SSL_control_alpha = sh_chilcotin_intercept  +
      sh_chilcotin_SST * sh_chilcotin_sst_coef + sh_chilcotin_SSL_alt * sh_chilcotin_ssl_coef # column EY 
  )


# NOTES
# retrospective -- all different productivity 
## in future -- have to assume a productivity 
# deltaPred estimated with solver -- needs to be done in optim in R 
# base case where you estimate, basecase where you make deltaPred 1 
## deltaPred = 1 = not changing from year to year (delta predation)
## doesn't correspond to a direct change because of the standardization 
## standardized sea lions -- can be negative 
# delta pred not real change in predation








