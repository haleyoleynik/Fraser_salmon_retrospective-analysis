# Remake Carl's sockeye model 

# load packages -----------
require(tidyverse)
require(readr)
require(ggplot2)

# read data ---------------
sockeye <- read_csv("data/walters_sockeye-data.csv")

# parameter toggles --------
retroU <- 0.3
useretro <- 1 # 1 for true, 0 for false 
yrretro <- 1990 

# model set up -------------
sockeye2 <- sockeye %>% 
  group_by(Stock) %>%
  arrange(Year) %>%
  rowwise() %>%
  mutate(run_wo_jacks = Run_Size - Jack_Escapement,
         Catch = Below_Mission_Catch + Above_Mission_Catch,
         UtC = min(0.999, Catch / run_wo_jacks),
         ENS = min(1,max(0.0001, Adult_Escapement / run_wo_jacks / (1 - UtC))),
         migmort = 1 - ENS,
         run_less_catch_escapement = run_wo_jacks - (Catch + Adult_Escapement),
         minus_DBE = -DBE,
         loss_run_less_catch = run_less_catch_escapement / (run_wo_jacks-Catch)) %>%
  ungroup()

sockeye2 %>%
  group_by(Stock) %>%
  arrange(Year) %>%
  mutate(adult_return = lag(run_wo_jacks, 4),
         lnrs = log(adult_return / Adult_Escapement)) 

min(1,max(0.0001, 42491 / 263191 / (1 - 0.838553251)))

# TRY AI 
sockeye_fixed <- sockeye %>%
  group_by(Stock) %>%
  arrange(Year, .by_group = TRUE) %>%
  mutate(
    run_wo_jacks = Run_Size - Jack_Escapement,
    Catch        = Below_Mission_Catch + Above_Mission_Catch,
    
    # Excel-like UtC (rounded)
    UtC_raw = Catch / run_wo_jacks,
    UtC     = pmin(0.999, round(UtC_raw, 9)),
    
    # ENS now matches Excel
    ENS_raw = Adult_Escapement / run_wo_jacks / (1 - UtC),
    ENS     = pmin(1, pmax(0.0001, ENS_raw)),
    
    migmort = if_else(abs(1 - ENS) < 1e-12, 0, 1 - ENS)
  ) %>%
  ungroup()













