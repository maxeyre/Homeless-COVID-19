# METADATA
# This script is associated with the article 'COVID-19 among people experiencing homelessness in England: a modelling study'
# [ Add publication details ]
# This script runs the model a set number of times (e.g. 200)
# It also compares the first and second half of model runs (to check stability of results)
# These computations are not memory-intensive (i.e. do not require a lot of RAM) but may take a long time to run on a single machine. You can set 'n_runs' to a smaller number of runs.
# The script is written in R4.0.0 but is compatible with most recent versions of R

#  --------------
#  number of runs
#  ==============

n_runs <- 200 # set the number of runs here
seeds <- seq_len(n_runs) # this variable can also be changed to any set of seeds, but there is no real benefit to this

#  -----------------
#  general functions
#  =================

# extract key results from model

pe <- function (fm, start_day = 1) { 
  fm <- fm[,start_day:ncol(fm)]
  x.itu <- sum(rowSums(fm == 15) > 0)
  c(
    n = nrow(fm),
    peak_PROTECT = max(colSums((fm > 6) & (fm < 12))),
    peak_CARE = max(colSums((fm == 12) | (fm == 13))),
    cases = sum(rowSums(fm == 2 | fm == 8) > 0),
    deaths = sum(fm[, ncol(fm)] == 16) - sum(fm[,1] == 16),
    admissions_itu = sum(rowSums(fm == 14) > 0) + x.itu,
    itu = x.itu
  )
}

# number of new cases per day (input is raw output from 'run_model')

new_cases <- function(fm) {
  nd <- ncol(fm)
  x <- fm == 2 | fm == 8
  case_day <- max.col(x, ties.method = 'first')
  anyCase <- rowSums(x) == 0
  case_day[anyCase] <- NA
  sapply(seq_len(nd), function(x) sum(case_day == x, na.rm = T))
}

# function for doing multiple runs. returns a list of 2 objects
# the first is a matrix of key results defined in 'pe' (starting from day 'start_day'); the second is the number of new cases each day (starting from day 1)

mr <- function(..., start_day = 121) {
  ts <- proc.time()
  r <- NULL
  rc <- NULL
  for (i in seeds) {
    t0 <- proc.time()
    m <- run_model(..., seed = i)
    r <- rbind(r,pe(m, start_day = start_day))
    rc <- rbind(rc, new_cases(m))
    print(paste0(i, '; ', round((proc.time() - t0)[3], 2))) # prints off a number and time each time a model iteration is completed
  }
  print(proc.time() - ts) # total time to run all seeds
  print((proc.time() - ts) / length(seeds)) # average time to run model
  list(r, rc)
}

#  ----------
#  scenario A
#  ==========

sA <- mr(model_duration = 120,
         gen_pop_mix_para = 0.5,
         gen_pop_mix_start = 1,
         gen_pop_mix_end = 121,
         suppressValue = 0.75,
         suppress_start = 1,
         suppress_end = 121,
         start_day = 1) # start_day is for 'pe' rather than run_model

#  -----------------------------------
#  scenario B: first wave 'do nothing'
#  ===================================

sB <- mr(model_duration = 120,
         interventions_start = 121,
         intervention_ends = 121,
         gen_pop_mix_start = 1,
         gen_pop_mix_end = 121,
         gen_pop_mix_para = 1,
         suppress_start = NA,
         start_day = 1) # start_day is for 'pe' rather than run_model

#  --------------------------------------------------
#  scenario C: no second wave; all measures continued
#  ==================================================

sC <- mr(model_duration = 365,
         gen_pop_mix_start = 1,
         gen_pop_mix_end = 365,
         gen_pop_mix_para = 0.5,
         suppress_start = 1,
         suppress_end = 365,
         suppressValue = 0.75,
         intervention_ends = 366,
         bi = 'wave1_ons')

#  ----------------------------------------------
#  scenario D: no second wave; all measures lifed
#  ==============================================

sD <- mr() # model inputs default to scenario D


#  -------------------------------------------------------
#  scenario E: second wave (sharp); all measures continued
#  =======================================================

sE <- mr(model_duration = 365,
         gen_pop_mix_start = 1,
         gen_pop_mix_end = 365,
         gen_pop_mix_para = 0.5,
         suppress_start = 1,
         suppress_end = 365,
         suppressValue = 0.75,
         intervention_ends = 365,
         bi = 'wave2_sharp')

#  --------------------------------------------------------------------
#  scenario F: second wave (sharp); measures only in general population
#  ====================================================================

sF <- mr(model_duration = 365,
         gen_pop_mix_start = 1,
         gen_pop_mix_end = 365,
         gen_pop_mix_para = 0.5,
         suppress_start = 1,
         suppress_end = 182,
         suppressValue = 0.75,
         intervention_ends = 182,
         bi = 'wave2_sharp')

#  -------------------------------------------------------------
#  scenario G: scenario D, but with COVID-PROTECT and COVID-CARE
#  =============================================================

sG <- mr(model_duration = 365,
         gen_pop_mix_start = 1,
         gen_pop_mix_end = 182,
         gen_pop_mix_para = 0.5,
         suppress_start = 1,
         suppress_end = 182,
         suppressValue = 0.75,
         intervention_ends = 366,
         bi = 'wave1_ons')

#  -----------------------------------------------------------------------------------------------------------
#  scenario H: second wave (flat); measures only in general population (as per H but with flatter second wave)
#  ===========================================================================================================

sH <- mr(model_duration = 365,
         gen_pop_mix_start = 1,
         gen_pop_mix_end = 365,
         gen_pop_mix_para = 0.5,
         suppress_start = 1,
         suppress_end = 182,
         suppressValue = 0.75,
         intervention_ends = 182,
         bi = 'wave2_flat')
