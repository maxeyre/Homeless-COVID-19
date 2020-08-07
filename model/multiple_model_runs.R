# METADATA
# This script is associated with the article 'COVID-19 among people experiencing homelessness in England: a modelling study'
# [ Add publication details ]
# This script runs the model a set number of times (e.g. 200), and produces summary results for the manuscript
# It also compares the first and second half of model runs (to check stability of results)
# These computations are not memory-intensive (i.e. do not require a lot of RAM) but may take a long time to run on a single machine. You can set 'n_runs' to a smaller number of runs.
# The script is written in R4.0.0 but is compatible with most recent versions of R

#  --------------
#  number of runs
#  ==============

n_runs <- 200 # set the number of runs here. should be an even number, or the comparison of first and second half of runs will not work
seeds <- seq_len(n_runs) # this variable can also be changed to any set of seeds, but there is no real benefit to this

#  -----------------
#  general functions
#  =================

# extract key results from model

pe <- function (fm, start_day = 1) { 
  fm <- fm[,start_day:ncol(fm)]
  x.stillsick <- sum(fm[, ncol(fm)] %in% c(2:5, 8:10, 12:15))
  x.cases <- sum(rowSums(fm == 2 | fm == 8) > 0)
  x.deaths <- sum(fm[, ncol(fm)] == 16) - sum(fm[,1] == 16)
  x.admissions <- sum(rowSums(fm == 14) > 0)
  x.itu <- sum(rowSums(fm == 15) > 0)
  c(
    n = nrow(fm),
    peak_PROTECT = max(colSums((fm > 6) & (fm < 12))),
    peak_CARE = max(colSums((fm == 12) | (fm == 13))),
    cases = x.cases,
    deaths = x.deaths,
    admissions = x.admissions, # admissions without ITU
    itu = x.itu,
    admissions_itu = x.admissions + x.itu, # all hospital admissions
    ifr = round(x.deaths / x.cases * 10000, 0), # for example, 162 = 1.62%
    ifr2 = round(x.deaths / (x.cases - x.stillsick) * 10000, 0) # accounting for bias due to patients who are still sick
  )
}

# extract median and 2.5% and 97.5% quantiles of key results across multiple runs

sf <- function(m) {
  m <- t(m)
  r <- apply(m, 1, quantile, probs = c(0.025, 0.5, 0.975))
  r <- formatC(r, digit = 0, format = 'd', big.mark = ',')
  apply(r, 2, function(x) paste0(x[2], ' (', x[1], '-', x[3], ')'))
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

# compare first and second half of runs. 
# 'm' is a matrix of model summaries (produced by function 'sf')
# output is the medians of each value, plus a panel chart of histograms from each set

titles <- c(NA, 'Peak PROTECT', 'Peak CARE', 'Cases', 'Deaths', 'Admissions', 'ITU admissions')
compareHalves <- function(m) { 
  a <- split.data.frame(m, rep(1:2, each = length(seeds)/2))
  meds <- sapply(a, function(x) apply(x, 2, median))
  print(meds)
  d <- lapply(a, asplit, MARGIN = 2) # function 'asplit' was added to R 3.6.0
  d <- lapply(d, function(x) lapply(x, function(y) density(y))) # densities for histograms
  par(mfrow = c(2, 3))
  for (i in 2:7) {
    med12 <- paste0(c('M1 = ', 'M2 = '), round(meds[i,], 0))
    med12 <- paste0(med12, collapse = '; ')
    plot(d$`1`[[i]], col = 'blue', main = paste0(titles[i], '\n', med12), xlab = NA)
    lines(d$`2`[[i]], col = 'red')
    abline(v = meds[i,], col = c('blue', 'red'), lty = 3)
    abline(v = median(t(m)[i,]))
  }
}

# example dates used in the model (e.g. for parameters 'intervention_start' or 'suppress_start')
# 1 = 1 February 2020
# 29 = 1 March 2020
# 120 = 31 May 2020
# 182 = 1 August 2020
# 365 = 31 January 2021

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
sf(sA[[1]])

png('compare_results_A.png', height = 6, width = 9, units = 'in', res = 300)
compareHalves(sA[[1]])
dev.off()

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
sf(sB[[1]])

png('compare_results_B.png', height = 6, width = 9, units = 'in', res = 300)
compareHalves(sB[[1]])
dev.off()

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
sf(sC[[1]])

png('compare_results_C121.png', height = 6, width = 9, units = 'in', res = 300)
compareHalves(sC[[1]])
dev.off()

#  ----------------------------------------------
#  scenario D: no second wave; all measures lifed
#  ==============================================

sD <- mr() # model inputs default to scenario D
sf(sD[[1]])

png('compare_results_D121.png', height = 6, width = 9, units = 'in', res = 300)
compareHalves(sD[[1]])
dev.off()

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
sf(sE[[1]])

png('compare_results_E121.png', height = 6, width = 9, units = 'in', res = 300)
compareHalves(sE[[1]])
dev.off()

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
sf(sF[[1]])

png('compare_results_F121.png', height = 6, width = 9, units = 'in', res = 300)
compareHalves(sF[[1]])
dev.off()

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
sf(sG[[1]])

png('compare_results_G121.png', height = 6, width = 9, units = 'in', res = 300)
compareHalves(sG[[1]])
dev.off()

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
sf(sH[[1]])

png('compare_results_H121.png', height = 6, width = 9, units = 'in', res = 300)
compareHalves(sH[[1]])
dev.off()

#  -----------------
#  table for article
#  =================

table3 <- rbind(sf(sA[[1]]), sf(sB[[1]]), sf(sB[[1]] - sA[[1]]), sf(sC[[1]]), sf(sD[[1]]), sf(sE[[1]]), sf(sF[[1]]), sf(sG[[1]]), sf(sH[[1]]))
write.csv(table3, 'table3.csv')

#  --------------------------------
#  plot of new cases - scenario C:H
#  ================================

nd <- ncol(sC[[2]])
model_start <- as.Date('2020-02-01')

library(RColorBrewer)

xl <- seq(model_start, model_start + 366, by = 'day')
xl <- xl[format(xl, '%d') == '01']
xl <- tail(head(xl, -1), -1)
xx <- as.numeric(xl - model_start)
xlab <- paste0('1 ', format(xl, format = '%b %Y'))

pf <- function(cases, ymax = 300, xoff = 0, yoff = 0, cols = brewer.pal(9, 'Blues')[c(3, 8)]) {
  tot <- rowSums(cases)
  med <- cases[which.min(abs(tot - median(tot))),]
  xp <- seq_along(med)
  apply(replace(cases, cases > ymax, ymax), 1, function(y) lines(xp + xoff, y + yoff, lwd = 0.5, col = cols[1]))
  lines(xp + xoff, med + yoff, col = cols[2])
}

wave1end <- as.numeric(as.Date('2020-05-31') - model_start)

pdf('fig2.pdf', height = 10, width = 15)

plot(1, type = 'n', xlim = c(0, nd * 3), ylim = c(0, 600), axes = F, xlab = NA, ylab = NA)

rect(0, 0, nd * 3, 600, col = 'grey98')

pf(sC[[2]], yoff = 300)
pf(sD[[2]])
pf(sE[[2]], yoff = 300, xoff = nd)
pf(sF[[2]], xoff = nd)
pf(sG[[2]], yoff = 300, xoff = nd * 2)
pf(sH[[2]], xoff = nd * 2)

rect(0, 0, nd * 3, 300)
rect(nd, 0, nd*2, 600)

axis(1, xx, xlab, las = 2, pos = 0)
axis(1, xx + nd, xlab, las = 2, pos = 0)
axis(1, xx + nd * 2, xlab, las = 2, pos = 0)

axis(2, 0:5 * 50, las = 2, pos = 0)
axis(2, 0:5 * 50 + 300, 0:5 * 50, las = 2, pos = 0)

mtext('Number of new cases per day', 2, line = 3)
text(rep(c(nd/2 + wave1end/2, nd * 1.5 + wave1end/2, nd * 2.5 + wave1end/2), each = 2), c(590, 290, 590, 290, 590, 290),
     c('Scenario C\n(No second wave;\nall measures continued)',
       'Scenario D\n(No second wave;\nall measures lifted)',
       'Scenario E\n(Sharp second wave;\nall measures continued)',
       'Scenario F\n(Sharp second wave;\nMeasures only continued\nin general population)',
       'Scenario G\n(as per scenario D,\nbut with COVID-CARE\nand COVID-PROTECT)',
       'Scenario H\n(Flat second wave;\nMeasures only continued\nin general population)'),
     adj = c(0.5, 1))

text(rep(c(wave1end/2, nd + wave1end/2, nd * 2 + wave1end/2), each = 2), c(590, 290, 590, 290, 590, 290),
     rep('Scenario A', 4),
     adj = c(0.5, 1))

segments(c(wave1end, wave1end + nd, wave1end + nd*2), 0, y1 = 600, lty = 3)

dev.off()
