
#===========
# status key
#===========

# 1 = community: susceptible and asymptomatic
# 2 = community: ili symptoms (not covid)
# 21 = community: exposed - day 1
# 22 = community: exposed - other days
# 3 = community: covid symptoms day 1
# 4 = community: covid, other days
# 5 = PROTECT: susceptible
# 6 = PROTECT: ili (all transferred to covid CARE next day)
# 25 = PROTECT: exposed - day 1
# 26 = PROTECT: exposed - other days
# 7 = PROTECT: new covid (all transferred to covid CARE next day)
# 19 = PROTECT: recovered: day 1
# 20 = PROTECT: recovered: other days
# 8 = CARE with ili (pre test): day 1
# 9 = CARE with ili (pre test): other days
# 10 = CARE with covid: day 1
# 11 = CARE with covid: other days
# 23 = CARE: exposed - day 1
# 24 = CARE: exposed - other days
# 18 = CARE with new COVID (acquired in intervention): day 1
# 12 = recovered in community
# 13 = hospital: day 1 - COULD RATIONALISE THESE AND JUST USE DURATION FROM FIRST ONSET OF COVID
# 14 = hospital: other days
# 15 = ITU: day 1
# 16 = ITU: other days
# 17 = died

#==================
# 1. model function
#==================

run_model <- function(

#----------------------------------
# inputs (values are base scenario)
#----------------------------------

# basic parameters
#-----------------

  seed = NA,
  testing = F, # testing (if false, 'time_to_results' defaults to duration of CARE, and after CARE vulnerable population is offered PROTECT rather than discharged to community)
  max_protect = NA, # NA for no maximum PROTECT (i.e. models demand)
  
  # population
  #-----------
  
  hostel_population = 34900,
  rough_sleeping_population = 10740, # based on official rough sleeper counts and 'case ascertainment' estimate using CHAIN and other assumptions
  proportion_vulnerable = 0.37, # age 55+ of 1+ LTC. Survey in London/Birmingham: https://bmjopen.bmj.com/content/9/4/e025192
  all_protect = F, # if T, everyone is offered protect regardless of vulnerability
  
  # disease and intervention
  #-------------------------
  
  # timings (in days)
  time_to_results = 2, # time to get result (after which negative cases are returned to community)
  self_discharge_day = 4,
  admission_day = 7, # to hospital or ITU
  duration_admission = 11,
  duration_PROTECT_recruitment = 28, # PROTECT population recruited steadily over this period (days)
  
  # intervention quality
  probability_identified = 0.7, # proportion of population identified
  accept = 0.8, # proportion accepting CARE or PROTECT when offered
  self_discharge_risk = 0.2,
  
  # case fatality and hospitalisation rates
  covid_severity_nv = c(0.4, 0.45, 0.1, 0.04, 0.01), # asymptomatic / mild / moderate / severe / critical. should sum to 1
  covid_severity_vul = c(0.1, 0.3, 0.3, 0.21, 0.09),
  cfr = c(0, 0.0001, 0.0025, 0.015, 0.5), # for non-vulnerable, by severity
  rr_CARE = 0.5, # risk ratio for mild and moderate cases in covid CARE

  # SEIR inputs
  e = 5, # duration of exposed (pre-symptomatic but infectious)
  B_h = 10,
  B_rs = 5,
  B_cc = 1,
  B_cp = 1,
  starting_cases = 10,

  # incidence of infection
  covid_attack_hostel = 0.8, # anything less than 1
  covid_attack_rough_sleepers = 0.5, # anything less than 1
  peak_day = 40,
  outbreak_duration = 90,
  B = 1.75, # parameter for 'shape' of curve (ADVANCED)
  ili_incidence = 6.3/700 # https://bmcinfectdis.biomedcentral.com/articles/10.1186/1471-2334-14-232

) {
  
  #-----------------------
  # calculate model inputs 
  #-----------------------
  
  # set peak day to <= outbreak duration
  peak_day <- min(peak_day, outbreak_duration)
  
  # model duration
  total_days <- outbreak_duration + 20
  
  # derived durations
  duration_covid <- e + admission_day + duration_admission # day of recovery or death
  duration_CARE <- admission_day + duration_admission
  
  # SEIR values
  disease_duration <- e + admission_day + duration_admission
  g <- 1 / disease_duration # gamma
  
  # seed
  if (!is.na(seed)) set.seed(seed)
  
  # change time to results if no testing
  time_to_results <- if (testing) time_to_results else duration_CARE
  
  # population size, vulnerability and eligibility for PROTECT
  n <- hostel_population + rough_sleeping_population # total number of people
  type <- c(rep(1, hostel_population), rep(2, rough_sleeping_population))
  vulnerable <- rbinom(n, 1, proportion_vulnerable)
  cpe <- vulnerable # covid PROTECT eligible
  cpe[all_protect] <- 1
  
  # covid severity
  severity_nv <- sample(1:length(covid_severity_nv), n, replace = T, prob = covid_severity_nv)
  severity_vul <- sample(1:length(covid_severity_vul), n, replace = T, prob = covid_severity_vul)
  severity <- ifelse(vulnerable == 0, severity_nv, severity_vul)
  
  # case fatality rates
  cfr_community <- cfr[severity]
  cfr_CARE  <- cfr_community * ifelse(severity %in% 2:3, rr_CARE, 1)

  # PROTECT referral day
  PROTECT_referral_day <- sample(1:duration_PROTECT_recruitment, n, replace = T)
  
  # likelihood of identification
  identified <- rbinom(n, 1, probability_identified) == 1
  
  # day 0
  day0 <- rep(1, n)
  day0[sample(1:hostel_population, starting_cases)] <- 21
  day0[sample((hostel_population+1):n, starting_cases)] <- 21
  dat <- as.matrix(day0)
  
  #-------------------------------------
  # function for modelling daily changes
  #-------------------------------------
  
  f <- function (day) {
    
    # previous statuses
    #------------------
    sy <- dat[, day] # status yesterday
    q.cov.dur <- if (day <= duration_covid) 0 else dat[, day - duration_covid]
    q.test <- if (day <= time_to_results) 0 else dat[, day - time_to_results]
    q.sd <- if (day <= self_discharge_day) 0 else dat[, day - self_discharge_day]
    q.admission <- if (day <= admission_day) 0 else dat[, day - admission_day]
    q.admission.start <- if (day <= duration_admission) 0 else dat[, day - duration_admission]
    q.symptom_start <- if (day <= e) 0 else dat[, day - e]

    # protect exceeded capacity yesterday (if so, then doesn't accept anyone today)
    #------------------------------------------------------------------------------
    protect_full <- if (is.na(max_protect)) F else sum(sy %in% c(5:7, 19:20)) >= max_protect

    # force of infection
    #-------------------
    foi <- function(hrs = NA, Beta, gam, i_statuses, n_statuses) {
      ty <- if (is.na(hrs)) T else type == hrs
      iN <- sum(sy %in% i_statuses & ty) / sum(sy %in% c(i_statuses, n_statuses) & ty)
      if (is.nan(iN)) 0 else min(iN * Beta * gam, 1)
    }
    foi_h <- foi(hrs = 1, Beta = B_h, i_statuses = c(3, 4, 21), n_statuses = c(1, 12), gam = g)
    foi_rs <- foi(hrs = 2, Beta = B_rs, i_statuses = c(3, 4, 21), n_statuses = c(1, 12), gam = g)
    foi_cp <- foi(Beta = B_cp, i_statuses = c(23, 7), n_statuses = c(5, 6, 19, 20), gam = g)
    foi_cc <- foi(Beta = B_cc, i_statuses = c(10, 11, 18, 22), n_statuses = c(8, 9), gam = g)
    
    # probabilities
    #--------------------
    p.died.community <- rbinom(n, 1, cfr_community) == 1
    p.died.CARE <- rbinom(n, 1, cfr_CARE) == 1
    p.self.discharge <- rbinom(n, 1, self_discharge_risk) == 1
    p.ref.CARE <- (rbinom(n, 1, accept) == 1) & (severity != 1)
    p.ref.PROTECT <- (rbinom(n, 1, accept) == 1) * !protect_full
    p.new.covid.community <- rbinom(n, 1, c(foi_h, foi_rs)[type]) == 1
    p.new.covid.CARE <- rbinom(n, 1, foi_cc) == 1
    p.new.covid.PROTECT <- rbinom(n, 1, foi_cp) == 1
    p.new.ili <- rbinom(n, 1, ili_incidence) == 1 & (!p.new.covid.community)

    # new status
    #-----------
    
    status <- sy
    
    # community covid
    status[(sy == 1) & p.new.covid.community] <- 21
    status[sy == 21] <- 22
    status[(sy == 22) & (q.symptom_start == 21)] <- 3
    status[sy == 3] <- 4
    status[(sy == 4) & (q.cov.dur %in% c(21, 23, 25))] <- 12
    
    # new ili in community
    status[(sy == 1) & p.new.ili] <- 2
    status[sy == 2] <- 1 # ILI only modelled to last 1 day (if not accepted COVID CARE)
    
    # PROTECT 
    status[(sy == 1) & (PROTECT_referral_day == day) & p.ref.PROTECT & (cpe == 1) & identified] <- 5
    status[sy == 5 & p.new.ili] <- 6 # transferred to CARE when ILI starts
    status[sy == 5 & p.new.covid.PROTECT] <- 25
    status[sy == 25] <- 26
    status[sy == 26 & q.symptom_start == 25] <- 7 # transferred to CARE when symptoms start
    status[sy == 5 & p.self.discharge & (PROTECT_referral_day + self_discharge_day == day)] <- 1
    status[sy == 20 & (q.sd == 19) & p.self.discharge] <- 12
    status[sy == 19] <- 20
    
    # CARE: covid
    status[(sy == 3) & p.ref.CARE & identified] <- 10
    status[sy == 7] <- 10
    status[sy == 10] <- 11
    status[(sy == 11) & (q.cov.dur %in% c(21, 23, 25))] <- 12
    status[(sy == 11) & (q.cov.dur %in% c(21, 23, 25)) & (cpe == 1) & (!testing) & p.ref.PROTECT] <- 19
    status[(sy == 11) & p.self.discharge & (q.sd %in% c(8, 10))] <- 4
    status[(sy %in% c(8, 9)) & p.new.covid.CARE] <- 23 # new covid (only those with ILI are susceptible)
    status[sy == 23] <- 24
    status[sy == 24 & q.symptom_start == 23] <- 10

    # CARE: ili
    status[(sy == 2) & p.ref.CARE & (day <= outbreak_duration)] <- 8
    status[(sy == 6) & (day <= outbreak_duration)] <- 8
    status[(sy == 8)] <- 9
    status[(sy == 9) & (q.test == 8)] <- 1
    status[(sy == 9) & (q.test == 8) & (cpe == 1) & p.ref.PROTECT] <- 5
    status[(sy == 9) & p.self.discharge & (q.sd == 8)] <- 1

    # hospital admissions
    status[(sy == 4) & (severity %in% 3:4) & (q.admission %in% c(3, 7, 18))] <- 13
    status[(sy == 11) & (severity == 4) & (q.admission %in% c(3, 7, 18))] <- 13
    status[sy == 13] <- 14
    status[(sy == 14) & q.admission.start == 13] <- 12
    status[(sy == 4) & (severity == 5) & (q.admission %in% c(3, 7, 18))] <- 15
    status[(sy == 11) & (severity == 5) & (q.admission %in% c(3, 7, 18))] <- 15
    status[sy == 15] <- 16
    status[(sy == 16) & q.admission.start == 15] <- 12
    
    # deaths
    day_of_death <- q.cov.dur %in% c(21, 23, 25)
    status[(sy == 4) & p.died.community & day_of_death] <- 17
    status[(sy %in% c(10, 11, 13:16)) & p.died.CARE & day_of_death] <- 17

    return(status)
    
  }
  
  #----------------
  # run daily model
  #----------------
  
  for(i in 1:total_days) {
    dat <- cbind(dat, f(i))
  }
  
  dat
  
}

#=====================================
# 2. FUNCTIONS FOR SUMMARISING RESULTS
#=====================================

# point estimates
#----------------

point_estimates <- function(fm, ae_prob = 1/18) { # fm = full matrix of individuals and statuses (ie. results of run model function)
  fs <- fm[, ncol(fm)] # final state
  bed_days <- colSums((fm == 13) | (fm == 14))
  itu_days <- colSums((fm == 15) | (fm == 16))
  covid_cases <- sum(fs %in% c(3, 4, 7, 10:18))
  covid_days_community <- sum(fm == 3 | fm == 4 | fm == 18)
  admissions = sum(fm == 13)
  itu <- sum(fm == 15)
  dth <- sum(fs == 17)
  c(
    peak_CARE = max(colSums((fm == 8) | (fm == 9) | (fm == 10) | (fm == 11) | (fm == 18))),
    peak_PROTECT = max(colSums((fm == 5) | (fm == 6) | (fm == 7) | (fm == 19) | (fm == 20))),
    cases = covid_cases,
    attack1000 = round(covid_cases / nrow(fm) * 1000, 0),
    deaths = dth,
    cfr1000 = round(dth / covid_cases * 1000, 0),
    hospital_admissions = admissions,
    hospital_bed_days = sum(bed_days),
    peak_bed_days = max(bed_days),
    hospital_peak_day = which.max(bed_days),
    itu_admission = itu,
    itu_bed_days = sum(itu_days),
    peak_itu_beds = max(itu_days),
    itu_peak_day = which.max(itu_days),
    ae_visits = round(covid_days_community * ae_prob, 0) + admissions + itu,
    ambulance = admissions + itu
  )
}

#====================================
# STOCHASTIC UNCERTAINTY IN BASE CASE
#====================================

#estimated duration on Dan's Mac for 1,000 runs with 45,000 individuals over 111 days: 5 hours

# t0 <- proc.time()
# su <- NULL
# seeds <- 1:2
# for (i in seeds) { # run for specific seeds, to allow replicability
#   dat <- run_model(seed = i, hostel_population = hostel_population, rough_sleeping_population = rough_sleeping_population)
#   su <- cbind(su, point_estimates(dat))
#   print(i)
# }
# t1 <- proc.time()
# t1 - t0

#=========
# RUN ONCE
#=========

dat <- run_model(hostel_population = 3490, 
                 rough_sleeping_population = 1074,
                 accept = 0,
                 seed = 913, 
                 B_h = 5, 
                 B_rs = 2, 
                 B_cp = 2, 
                 B_cc = 2,
                 outbreak_duration = 120)

# PLOTS
#======

library(RColorBrewer)

n <- ncol(dat)
r <- nrow(dat)

# number at each status by day
#-----------------------------

dsf <- function(fm) {
  ds <- t(sapply(1:26, function(x) colSums(fm == x)))
  ds2 <- rbind(`Community: susceptible` = colSums(ds[1:2,]),
               `Community: exposed` = colSums(ds[21:22,]),
               `COVID-PROTECT` = colSums(ds[c(5:7,19:20,25:26),]),
               `Community: recovered` = ds[12,],
               `Community: COVID-19` = colSums(ds[3:4,]),
               `COVID-CARE` = colSums(ds[c(8:11,18,23:24),]),
               `Admitted to hospital` = colSums(ds[13:14,]),
               `Admitted to ITU` = colSums(ds[15:16,]),
               Died = ds[17,])
  list(ds, ds2, rbind(0, apply(ds2, 2, cumsum)))
}
ds <- dsf(dat)

# y-axis function
#----------------

yaxt <- c(outer(c(1, 2.5, 5), 10^(0:6), '*'))
yaxt <- yaxt[yaxt != 2.5]
yxf <- function(ymax, tk = 5, type = 'next') { # function for making y-axes
  tm <- yaxt[which.min(abs(yaxt - ymax / tk))]
  nticks <- if (type == 'next') ceiling(ymax / tm) else floor(ymax / tm)
  seq(0, tm * nticks, tm)
}

# Number of people by current status
#-----------------------------------

cols <- brewer.pal(nrow(ds[[2]]), 'Paired')
par(mar = c(4, 5, 1, 15), xpd = NA)
plot(1, type = 'n', xlim = c(0, n), ylim = c(0, nrow(dat)), axes = F, xlab = NA, ylab = NA)
for(i in 1:nrow(ds[[2]])) {
  polygon(c(0:(n-1), (n-1):0), c(ds[[3]][i+1,], rev(ds[[3]][i,])), col = cols[i])
}
axis(1, seq(0, floor(n/14)*14, 14), 0:floor(n/14) * 2, pos = 0)
segments(0, 0, n)
axis(2, yxf(r, type = 'previous'), pos = 0, las = 2)
ys <- seq(r * 0.25, r * 0.75, length.out = length(cols) + 1)
rect(n * 1.07, ys[-length(ys)], n * 1.14, ys[-1], col = cols)
text(n * 1.19, ys[-length(ys)] + diff(ys) / 2, rownames(ds[[2]]), adj = 0)
title(xlab = 'Week', line = 2.5)
title(ylab = 'Population', line = 4)

# Hospital and ambulance use
#---------------------------

cols <- brewer.pal(4, 'Set1')

fpl <- function(d, yrange = 0.15) {
  ymax <- max(d) * 1.1
  plot(1, type = 'n', xlim = c(0, n), ylim = c(0, ymax), xlab = NA, ylab = NA, axes = F)
  for (i in 1:nrow(d)) lines(0:(n-1), d[i,], col = cols[i], lwd = 2)
  axis(1, seq(0, floor(n/14)*14, 14), 0:floor(n/14) * 2, pos = 0)
  segments(0, 0, n + 1)
  axis(2, yxf(ymax, type = 'previous'), pos = 0, las = 2)
  rect(0, 0, n, ymax)
  ys <- seq(ymax * (0.5-yrange), ymax * (0.5+yrange), length.out = nrow(d))
  segments(n * 1.1, ys, n * 1.2, ys, col = cols, lwd = 2)
  text(n * 1.25, ys, row.names(d), adj = 0)
  title(xlab = 'Week', line = 2.5)
  title(ylab = 'Number', line = 3.5)
}

admissions <- colSums(dat == 13 | dat == 15)
covid_days_community <- ds[[1]][5,]
ae_visits  <- round(covid_days_community * (1/18), 0) + admissions

par(mar = c(4, 5, 1, 15), xpd = NA)
fpl(rbind(ds[[2]][7:8,], `A&E visits` = ae_visits, `Ambulance journeys` = admissions))

# CARE and PROTECT use
#---------------------

par(mar = c(4, 5, 1, 15), xpd = NA)
fpl(ds[[2]][c(6, 3),], yrange = 0.05)

# check cases with most status changes
#-------------------------------------

changes <- rowSums(dat[,-1] != dat[,-ncol(dat)])
ids_max_changes <- which(max(changes) - changes < 7)
dat[ids_max_changes,]

# epi curve plot
#---------------

new_cases_total <- colSums((dat == 3) | (dat == 7)) # symptomatic
#type <- c(rep(1, hostel_population), rep(2, rough_sleeping_population))
#new_cases_rough_sleepers <- colSums(((dat == 3) | (dat == 7)) & type == 2)
cum_inc <- cumsum(new_cases_total) / r
ymax <- ceiling(max(new_cases_total)/50) * 50
par(xpd = NA, mar = c(4, 5, 1, 15))
plot(1, type = 'n', xlim = c(0, n + 1), ylim = c(0, ymax), axes = F, xlab = NA, ylab = 'New cases')
rect(0, 0, n + 1, ymax)
title(xlab = 'Week', line = 2.5)
rect(0:n, 0, 1:(n + 1), new_cases_total, border = NA, col = 'grey80')
#rect(0:n, 0, 1:(n + 1), new_ßcases_rough_sleepers, border = NA, col = 'grey60')
lines(0:(n-1) + 0.5, cum_inc * ymax, col = 'white', lwd = 5)
lines(0:(n-1) + 0.5, cum_inc * ymax, col = 'red')
axis(2, yxf(ymax, type = 'previous'), pos = 0, las = 2)
axis(4, 0:5/5 * ymax, paste0(0:5/5 * 100, '%'), las = 2, pos = n + 1, col = 'red', col.axis = 'red')
axis(1, seq(0, floor(n/14)*14, 14), 0:floor(n/14) * 2, pos = 0)
segments(0, 0, n + 1)
text(n * 1.35, ymax/2, 'Cumulative incidence', srt = 270, col = 'red')
text(n - n / 20, max(cum_inc + 0.05) * ymax, paste0(round(max(cum_inc) * 100, 0), '%'), col = 'red')
xs <- c(1.45, 1.9)
ys <- c(0.9, 0.95, 1)
rect(n * xs[1], ymax*ys[1:2], n * xs[2], ymax*ys[2:3], col = c('grey60', 'grey80'), border = NA)
text(n * mean(xs), ymax*(ys[1:2] + diff(ys)/2), c('Rough sleepers', 'Total'), cex = 0.8, col = c('white', 'black'))

# deaths
#--------

deaths <- (cbind(0, dat[,-ncol(dat)]) != 17) & (dat == 17)
deaths <- colSums(deaths)
ymax <- max(deaths)
par(xpd = NA, mar = c(4, 5, 1, 1))
plot(1, type = 'n', xlim = c(0, n + 1), ylim = c(0, ymax), axes = F, xlab = NA, ylab = 'Deaths')
rect(0:n, 0, 1:(n+1), deaths, col = "#8DA0CB", border = NA)
axis(1, seq(0, floor(n/14)*14, 14), 0:floor(n/14) * 2, pos = 0)
segments(0, 0, n + 1)
rect(0, 0, n+1, ymax)
axis(2, yxf(ymax, type = 'previous'), pos = 0, las = 2)
title(xlab = 'Week', line = 2.5)
text(n * 0.05, ymax * 0.95, paste0('Total = ', sum(deaths)), adj = 0)

