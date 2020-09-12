# METADATA
# This script is associated with the article 'COVID-19 among people experiencing homelessness in England: a modelling study'
# [ Add publication details ]
# This script runs the model a set number of times (e.g. 200), and produces summary results for the manuscript
# It includes probabilistic variation in key input parameters, with results used in a sensitivity analysis
# These computations are not memory-intensive (i.e. do not require a lot of RAM) but may take a long time to run on a single machine. You can set 'n_runs' to a smaller number of runs.
# The script is written in R4.0.0 but is compatible with most recent versions of R

# ------
# inputs
# ======

set.seed(20)
library(mc2d) # for PERT distribution

nmc <- 200

d_r0 <- rpert(nmc, min = 1.5, mode = 2.5, max = 3.5, shape = 4)
d_k <- exp(rnorm(nmc, mean = 0, sd = (log(0.2) - log(0.1)) / qnorm(0.975)))
d_m <- rpert(nmc, min = 0.1, mode = 0.5, max = 1, shape = 4)
d_ifr_multiplier <- rpert(nmc, min = 1, mode = 3, max = 5, shape = 4)
d_ifr_multiplier_simple <- d_ifr_multiplier / 3

args <- data.frame(r0_h = d_r0, k = d_k, gen_pop_mix_para = d_m, cfr_multiplier = d_ifr_multiplier_simple, seed = seq_len(nmc))

#  -----------------
#  general functions
#  =================

# extract key results from model

pe <- function (fm, start_day = 121) { 
  fm <- fm[,start_day:ncol(fm)]
  x.admissions <- sum(rowSums(fm == 14) > 0)
  x.itu <- sum(rowSums(fm == 15) > 0)
  c(
    cases = sum(rowSums(fm == 2 | fm == 8) > 0),
    deaths = sum(fm[, ncol(fm)] == 16) - sum(fm[,1] == 16),
    admissions_itu = x.admissions + x.itu, # all hospital admissions
    itu = x.itu
  )
}

new_cases <- function(fm) {
  nd <- ncol(fm)
  x <- fm == 2 | fm == 8
  case_day <- max.col(x, ties.method = 'first')
  anyCase <- rowSums(x) == 0
  case_day[anyCase] <- NA
  sapply(seq_len(nd), function(x) sum(case_day == x, na.rm = T))
}

#  ----------
#  scenario A
#  ==========

mcA <- NULL
mcA_cases <- NULL
TICKER <- 0
for (i in seq_len(nmc)) {
  START_TIME <- proc.time()
  m <- run_model(model_duration = 120,
                 #gen_pop_mix_para = 0.5,
                 gen_pop_mix_start = 1,
                 gen_pop_mix_end = 121,
                 suppressValue = 0.75,
                 suppress_start = 1,
                 suppress_end = 121,
                 r0_h = args$r0_h[i],
                 k = args$k[i],
                 gen_pop_mix_para = args$gen_pop_mix_para[i],
                 cfr_multiplier = args$cfr_multiplier[i],
                 seed = args$seed[i])
  mcA <- rbind(mcA, pe(m, start_day = 1))
  mcA_cases <- rbind(mcA_cases, new_cases(m))
  TICKER <- TICKER + 1
  tt <- proc.time() - START_TIME
  print(paste0(TICKER, '; ', round(tt[3], 2))) 
}

#  -----------------------------------
#  scenario B: first wave 'do nothing'
#  ===================================

mcB <- NULL
mcB_cases <- NULL
TICKER <- 0
for (i in seq_len(nmc)) {
  START_TIME <- proc.time()
  m <- run_model(model_duration = 120,
                    interventions_start = 121,
                    intervention_ends = 121,
                    gen_pop_mix_start = 1,
                    gen_pop_mix_end = 121,
                    gen_pop_mix_para = 1,
                    suppress_start = NA,
                    r0_h = args$r0_h[i],
                    k = args$k[i],
                    # gen_pop_mix_para = args$gen_pop_mix_para[i],
                    cfr_multiplier = args$cfr_multiplier[i],
                    seed = args$seed[i])
  mcB <- rbind(mcB, pe(m, start_day = 1))
  mcB_cases <- rbind(mcB_cases, new_cases(m))
  TICKER <- TICKER + 1
  tt <- proc.time() - START_TIME
  print(paste0(TICKER, '; ', round(tt[3], 2))) 
}

# ----------
# scenario C
# ==========

mcC <- NULL
mcC_cases <- NULL
TICKER <- 0
for (i in seq_len(nmc)) {
  START_TIME <- proc.time()
  m <- run_model(r0_h = args$r0_h[i],
                 k = args$k[i],
                 cfr_multiplier = args$cfr_multiplier[i],
                 gen_pop_mix_para = args$gen_pop_mix_para[i],
                 model_duration = 365,
                 gen_pop_mix_start = 1,
                 gen_pop_mix_end = 365,
                 suppress_start = 1,
                 suppress_end = 365,
                 suppressValue = 0.75,
                 intervention_ends = 366,
                 seed = args$seed[i])
  mcC <- rbind(mcC, pe(m, start_day = 121))
  mcC_cases <- rbind(mcC_cases, new_cases(m))
  TICKER <- TICKER + 1
  tt <- proc.time() - START_TIME
  print(paste0(TICKER, '; ', round(tt[3], 2))) 
}

# ----------
# scenario D
# ==========

mcD <- NULL
mcD_cases <- NULL
TICKER <- 0
for (i in seq_len(nmc)) {
  START_TIME <- proc.time()
  m <- run_model(r0_h = args$r0_h[i],
                    k = args$k[i],
                    gen_pop_mix_para = args$gen_pop_mix_para[i],
                    cfr_multiplier = args$cfr_multiplier[i],
                    model_duration = 365,
                    gen_pop_mix_start = 1,
                    gen_pop_mix_end = 182,
                    suppress_start = 1,
                    suppress_end = 182,
                    suppressValue = 0.75,
                    intervention_ends = 182,
                    seed = args$seed[i])
  mcD <- rbind(mcD, pe(m, start_day = 121))
  mcD_cases <- rbind(mcD_cases, new_cases(m))
  TICKER <- TICKER + 1
  tt <- proc.time() - START_TIME
  print(paste0(TICKER, '; ', round(tt[3], 2))) 
}

#  -------------------------------------------------------
#  scenario E: second wave (sharp); all measures continued
#  =======================================================

mcE <- NULL
mcE_cases <- NULL
TICKER <- 0
for (i in seq_len(nmc)) {
  START_TIME <- proc.time()
  m <- run_model(model_duration = 365,
                 gen_pop_mix_start = 1,
                 gen_pop_mix_end = 365,
                 # gen_pop_mix_para = 0.5,
                 suppress_start = 1,
                 suppress_end = 365,
                 suppressValue = 0.75,
                 intervention_ends = 365,
                 bi = 'wave2_sharp',
                 r0_h = args$r0_h[i], # suppressed to 0.75
                 k = args$k[i],
                 gen_pop_mix_para = args$gen_pop_mix_para[i],
                 cfr_multiplier = args$cfr_multiplier[i],
                 seed = args$seed[i])
  mcE <- rbind(mcE, pe(m, start_day = 121))
  mcE_cases <- rbind(mcE_cases, new_cases(m))
  TICKER <- TICKER + 1
  tt <- proc.time() - START_TIME
  print(paste0(TICKER, '; ', round(tt[3], 2))) 
}

#  --------------------------------------------------------------------
#  scenario F: second wave (sharp); measures only in general population
#  ====================================================================

mcF <- NULL
mcF_cases <- NULL
TICKER <- 0
for (i in seq_len(nmc)) {
  START_TIME <- proc.time()
  m <- run_model(model_duration = 365,
                 gen_pop_mix_start = 1,
                 gen_pop_mix_end = 365,
                 # gen_pop_mix_para = 0.5,
                 suppress_start = 1,
                 suppress_end = 182,
                 suppressValue = 0.75,
                 intervention_ends = 182,
                 bi = 'wave2_sharp',
                 r0_h = args$r0_h[i],
                 k = args$k[i],
                 gen_pop_mix_para = args$gen_pop_mix_para[i],
                 cfr_multiplier = args$cfr_multiplier[i],
                 seed = args$seed[i])
  mcF <- rbind(mcF, pe(m, start_day = 121))
  mcF_cases <- rbind(mcF_cases, new_cases(m))
  TICKER <- TICKER + 1
  tt <- proc.time() - START_TIME
  print(paste0(TICKER, '; ', round(tt[3], 2))) 
}

#  -------------------------------------------------------------
#  scenario G: scenario D, but with COVID-PROTECT and COVID-CARE
#  =============================================================

mcG <- NULL
mcG_cases <- NULL
TICKER <- 0
for (i in seq_len(nmc)) {
  START_TIME <- proc.time()
  m <- run_model(model_duration = 365,
                 gen_pop_mix_start = 1,
                 gen_pop_mix_end = 182,
                 # gen_pop_mix_para = 0.5,
                 suppress_start = 1,
                 suppress_end = 182,
                 suppressValue = 0.75,
                 intervention_ends = 366,
                 bi = 'wave1_ons',
                 r0_h = args$r0_h[i],
                 k = args$k[i],
                 gen_pop_mix_para = args$gen_pop_mix_para[i],
                 cfr_multiplier = args$cfr_multiplier[i],
                 seed = args$seed[i])
  mcG <- rbind(mcG, pe(m, start_day = 121))
  mcG_cases <- rbind(mcG_cases, new_cases(m))
  TICKER <- TICKER + 1
  tt <- proc.time() - START_TIME
  print(paste0(TICKER, '; ', round(tt[3], 2))) 
}

#  -----------------------------------------------------------------------------------------------------------
#  scenario H: second wave (flat); measures only in general population (as per H but with flatter second wave)
#  ===========================================================================================================

mcH <- NULL
mcH_cases <- NULL
TICKER <- 0
for (i in seq_len(nmc)) {
  START_TIME <- proc.time()
  m <- run_model(model_duration = 365,
                 gen_pop_mix_start = 1,
                 gen_pop_mix_end = 365,
                 # gen_pop_mix_para = 0.5,
                 suppress_start = 1,
                 suppress_end = 182,
                 suppressValue = 0.75,
                 intervention_ends = 182,
                 bi = 'wave2_flat',
                 r0_h = args$r0_h[i],
                 k = args$k[i],
                 gen_pop_mix_para = args$gen_pop_mix_para[i],
                 cfr_multiplier = args$cfr_multiplier[i],
                 seed = args$seed[i])
  mcH <- rbind(mcH, pe(m, start_day = 121))
  mcH_cases <- rbind(mcH_cases, new_cases(m))
  TICKER <- TICKER + 1
  tt <- proc.time() - START_TIME
  print(paste0(TICKER, '; ', round(tt[3], 2))) 
}
