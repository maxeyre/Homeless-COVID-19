# METADATA
# This script is associated with the article 'COVID-19 among people experiencing homelessness in England: a modelling study'
# [ Add publication details ]
# This script estimates daily incidence in the general population, based on estimates of cumulative incidence at the end of the 'first wave'
# The script produces the file 'gp_cases31july2020.csv' that is already included in the repository. This file is a model input
# There is no need to run this script prior to running the model; it is only provided for completeness
# The script is written in R4.0.0 but is compatible with most recent versions of R

options(scipen = 999)
library(RColorBrewer)

#--------------------------
# model start and end dates
#==========================

model_start <- as.Date('2020-02-01')
model_end <- as.Date('2021-01-31')

#------------------------------------------------------------
# cumulative incidence of infection in the general population
#============================================================

ons <- 0.054 # ONS infection survey. https://www.ons.gov.uk/peoplepopulationandcommunity/healthandsocialcare/conditionsanddiseases/bulletins/coronaviruscovid19infectionsurveypilot/latest#antibody-data
mrc <- 0.08
low <- ons - (mrc - ons)
population <- 55977178 # England mid-year 2019. https://www.ons.gov.uk/peoplepopulationandcommunity/populationandmigration/populationestimates/bulletins/annualmidyearpopulationestimates/mid2019
wave1_cases_ons <- population * ons
wave1_cases_mrc <- population * mrc
wave1_cases_low <- population * low
baseline <- 5000 # daily infections outside of 'wave'

#---------------------------
# wave 1 shape from PCR data
#===========================

# https://coronavirus.data.gov.uk/; accessed 19 June 2020

cases <- read.csv("https://raw.githubusercontent.com/maxeyre/Homeless-COVID-19/master/model/SUPPORTING_covid_cases_19june2020.csv")
cases$date <- as.Date(cases$date)

wave1_start <- cases$date[min(which(cases$cases > 50))]
wave1_peak <- cases$date[which.max(cases$cases)]
wave1_end <- cases$date[max(which(cases$cases > 50))]

#-------------------
# wave 2 assumptions
#===================

# sharp peak

wave2_peak_sharp <- as.Date('2020-11-01')
wave2_start_sharp <- wave2_peak_sharp - (wave1_peak - wave1_start)
wave2_end_sharp <- wave2_start_sharp + (wave1_end - wave1_start)
ratio_sharp <- 0.5 # total cases compared to first wave
wave2_cases_sharp <- wave1_cases_ons * ratio_sharp

# flatter

wave2_peak_flat <- as.Date('2020-11-01')
wave2_start_flat <- wave2_peak_flat - (wave1_peak - wave1_start) * 3
wave2_end_flat <- wave2_start_flat + (wave1_end - wave1_start) * 3
ratio_flat <- 0.5 # total cases compared to first wave
wave2_cases_flat <- wave1_cases_ons * ratio_flat

#--------------------------
# make daily infection data
#==========================

# function for making daily number of cases following a curve
s <- function(x, k = 2) 1/(1 + (x/(1-x))^-k)

# function for making wave
wave_cases <- function(start_date, peak_date, end_date, total_cases, k = 1.5, lab = 'cases') {
  ds <- seq(start_date, end_date, by = 'day')
  days <- length(ds)
  xs1 <- seq_len(as.numeric(peak_date - start_date))
  xs2 <- seq(max(xs1), 1, length.out = length(ds) - length(xs1))
  xs <- c(xs1, xs2)
  cv <- xs/max(xs) # curve
  cv <- s(cv, k)
  pc <- cv / sum(cv) # proportion of cases each day
  cases <- pc * total_cases
  cases <- round(cases, 0)
  df <- data.frame(day = ds, cases = cases)
  names(df)[2] <- lab
  df
}

wave1_ons <- wave_cases(wave1_start, wave1_peak, wave1_end, wave1_cases_ons, lab = 'wave1_ons')
wave1_mrc <- wave_cases(wave1_start, wave1_peak, wave1_end, wave1_cases_mrc, lab = 'wave1_mrc')
wave1_low <- wave_cases(wave1_start, wave1_peak, wave1_end, wave1_cases_low, lab = 'wave1_low')

wave2_sharp <- wave_cases(wave2_start_sharp, wave2_peak_sharp, wave2_end_sharp, wave2_cases_sharp, lab = 'wave2_sharp')
wave2_flat <- wave_cases(wave2_start_flat, wave2_peak_flat, wave2_end_flat, wave2_cases_flat, lab = 'wave2_flat')

all_dates <- data.frame(day = seq(model_start, model_end, by = 'day'))
waves <- list(all_dates, wave1_ons, wave1_mrc, wave1_low, wave2_sharp, wave2_flat)
waves <- Reduce(function(...) merge(..., all = T), waves)
waves[is.na(waves)] <- 0
waves_after_peak1 <- data.frame(lapply(waves[waves$day > wave1_peak, -1], function(x) pmax(x, baseline)))
waves[waves$day > wave1_peak, -1] <- waves_after_peak1
waves$day2 <- as.numeric(waves$day - model_start) + 1
waves <- waves[waves$day <= model_end,]
waves$wave2_sharp <- pmax(waves$wave2_sharp, waves$wave1_ons)
waves$wave2_flat <- pmax(waves$wave2_flat, waves$wave1_ons)

#-----
# plot
#=====

xl <- seq(model_start, model_end + 1, by = 'day')
xl <- xl[format(xl, '%d') == '01']
xx <- as.numeric(xl - model_start)
xlab <- paste0('1 ', format(xl, format = '%b %Y'))

cols <- brewer.pal(5, 'Set2')
cols[1] <- 'black'
ltys <- c(1, 3, 3, 3, 3)
ymax <- 100000

png('general_population_incidence.png', height = 6, width = 10, units = 'in', res = 300)

par(mar = c(4, 6, 2, 12), xpd = NA)

plot(1, type = 'n', xlim = c(0, nrow(waves)), ylim = c(0, ymax), axes = F, xlab = NA, ylab = NA)
rect(0, 0, nrow(waves), ymax, col = 'grey98')
axis(1, xx, xlab, las = 2, pos = 0)
segments(0, 0, 365)
axis(2, 0:floor(ymax/20000) * 20000, prettyNum(0:floor(ymax/20000) * 20000, big.mark = ','), las = 2, pos = 0)
lines(waves$day2, waves$wave1_mrc, col = cols[2], lty = ltys[2])
lines(waves$day2, waves$wave1_low, col = cols[3], lty = ltys[3])
lines(waves$day2, waves$wave2_sharp, col = cols[4], lty = ltys[4])
lines(waves$day2, waves$wave2_flat, col = cols[5], lty = ltys[5])
lines(waves$day2, waves$wave1_ons, col = ltys[1])

title(ylab = 'Number of new cases in England', line = 4)

ys <- seq(ymax * 0.4, ymax * 0.6, length.out = 5)
segments(365*1.05, ys, 365 * 1.1, ys, col = rev(cols), lty = rev(ltys))
text(365*1.12, ys, c('Second wave: flatter profile', 'Second wave: sharper profile', 'First wave: low profile', 'First wave: high profile', 'Base scenario'), adj = 0)

dev.off()

names(waves)[1] <- 'date'
names(waves)[7] <- 'day'
write.csv(waves, 'gp_cases31july2020.csv', row.names = F)
