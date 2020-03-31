# server.R
library(shiny)
library(RColorBrewer)

#-----------
# status key
#-----------

# 1 = community: susceptible and asymptomatic
# 2 = community: ili symptoms (not covid)
# 3 = community: covid day 1
# 4 = community: covid, other days
# 5 = PROTECT: susceptible
# 6 = PROTECT: ili (all transferred to covid PROTECT)
# 7 = PROTECT: covid (all transferred to covid CARE)
# 8 = CARE with ili (pre test): day 1
# 9 = CARE with ili (pre test): other days
# 10 = CARE with covid: day 1
# 11 = CARE with covid: other days
# 12 = recovered in community
# 13 = admitted to hospital: day 1
# 14 = admitted to hospital: other days
# 15 = admitted to ITU: day 1
# 16 = admitted to ITU: other days
# 17 = died

set.seed(34) # this means that the results are replicable (i.e. random values come out the same every time)

#-------
# inputs
#-------

# basic parameters
#-----------------

#total_days <- 110

# population
#-----------

# individuals
hostel_population <- 8784
rough_sleeping_population <- 1136 # based on official rough sleeper counts and 'case ascertainment' estimate using CHAIN and other assumptions
proportion_vulnerable <- 0.50
all_protect <- F # if T, everyone is offered protect regardless of vulnerability
n <- hostel_population + rough_sleeping_population # total number of people
# disease and intervention
#-------------------------

# testing (if false, 'time_to_results' defaults to duration of CARE, and after CARE vulnerable population is offered PROTECT rather than discharged to community)
testing <- T

# PROTECT capacity
max_protect <- NA # NA for no maximum

# timings (in days)
time_to_results <- 2 # time to get result (after which negative cases are returned to community)
self_discharge_day <- 4
admission_day <- 7 # to hospital or ITU
died_covid_day <- 19
duration_covid <- 15 # day of recovery (for those not hospitalised)

# durations (days)
duration_CARE <- 14
duration_admission <- 12
duration_PROTECT_recruitment <- 28 # PROTECT population recruited steadily over this period (days)

# risks and rates
probability_identified <- 0.7 # proportion of population identified
accept_CARE <- 0.7 # proportion accepting CARE
accept_PROTECT <- 0.7 # proportion accepting PROTECT
self_discharge_risk <- 0.33
ili_incidence <- 6.3/700 # https://bmcinfectdis.biomedcentral.com/articles/10.1186/1471-2334-14-232
ae_prob <- 1/duration_covid # daily probability of A&E during COVID illness: average 1 visit per illness

# case fatality and hospitalisation rates
covid_severity <- c(0.65, 0.2, 0.1, 0.05) # mild / moderate / severe / critical. should sum to 1
cfr_community <- c(0.005, 0.01, 0.05, 0.2)
rr_vulnerable <- 4 # risk ratio for vulnerable people
rr_CARE <- 0.5 # risk ratio for mild and moderate cases in covid CARE

# covid community incidence
#peak_day <- 40
outbreak_duration <- 90 # should be shorter than model duration
covid_attack_hostel <- 0.8 # anything less than 1
covid_attack_rough_sleepers <- 0.5 # anything less than 1
PROTECT_incidence_fraction <- 1/4 # incidence of covid & ILI in PROTECT is x * hostel rate
B <- 1.75 # parameter for 'shape' of curve (not much value in changing)

type <- c(rep(1, hostel_population), rep(2, rough_sleeping_population))

main.function <- function(total_days,hostel_population,rough_sleeping_population,proportion_vulnerable,
                          all_protect,testing,max_protect,time_to_results,self_discharge_day,admission_day,
                          died_covid_day,duration_covid,duration_CARE,duration_admission,duration_PROTECT_recruitment,
                          probability_identified,accept_CARE,accept_PROTECT,self_discharge_risk,ili_incidence,ae_prob,
                          covid_severity,cfr_community,rr_vulnerable,rr_CARE,peak_day,outbreak_duration,covid_attack_hostel,
                          covid_attack_rough_sleepers,PROTECT_incidence_fraction,B){
  #-----------------------
  # calculate model inputs 
  #-----------------------
  
  # change time to results if no testing
  time_to_results <- if (testing) time_to_results else duration_CARE
  
  # population size, vulnerability and eligibility for PROTECT
  n <- hostel_population + rough_sleeping_population # total number of people
  type <- c(rep(1, hostel_population), rep(2, rough_sleeping_population))
  vulnerable <- rbinom(n, 1, proportion_vulnerable)
  cpe <- vulnerable # covid PROTECT eligible
  cpe[all_protect] <- 1
  
  # case fatality rates
  cfr_community <- rbind(cfr_community, cfr_community * rr_vulnerable)
  cfr_community <- pmin(cfr_community, 1)
  cfr_CARE <- c(cfr_community[1,1:2] * rr_CARE, cfr_community[1,3:4])
  cfr_CARE <- rbind(cfr_CARE, cfr_CARE * rr_vulnerable)
  cfr_CARE <- pmin(cfr_CARE, 1)
  
  # incidence of ili in PROTECT (same multiplier / fraction of community incidence as used in COVID)
  ili_incidence_PROTECT <- ili_incidence * PROTECT_incidence_fraction
  
  # draw covid incidence curve
  triangular_pattern <- c(seq(0, 1, length.out = peak_day), seq(1, 0, length.out = outbreak_duration - peak_day))
  # returns cumulative attack rate given incidence
  fi <- function(x) 1 - prod(1-(1 / ( 1 + (triangular_pattern / (1-triangular_pattern))^(-B)) * x))
  # returns curve given cumulative attack
  fi2 <- function(ca) { 
    incs <- 0:10000 / 10000 # incidences to try
    attacks <- sapply(incs, fi)
    peak_incidence <- incs[which.min(abs(attacks - ca))]
    inc <- 1 / ( 1 + (triangular_pattern / (1-triangular_pattern))^(-B)) * peak_incidence
    post_outbreak <- if (total_days > outbreak_duration) total_days - outbreak_duration else 0
    c(inc, rep(0, post_outbreak))
  }
  covid_incidence <- rbind(fi2(covid_attack_hostel), covid_incidence_rough_sleeper <- fi2(covid_attack_rough_sleepers))
  covid_incidence_PROTECT <- covid_incidence[1,] * PROTECT_incidence_fraction # placeholder
  
  # PROTECT referral day
  PROTECT_referral_day <- sample(1:duration_PROTECT_recruitment, n, replace = T)
  
  # covid severity
  severity <- sample(1:4, n, replace = T, prob = covid_severity)
  
  # select mortality risk
  mortality_community <- cfr_community[cbind(vulnerable + 1, severity)]
  mortality_CARE <- cfr_CARE[cbind(vulnerable + 1, severity)]
  
  # likelihood of identification
  identified <- rbinom(n, 1, probability_identified) == 1
  
  # days 0 and 1
  day0 <- rep(1, n)
  day1 <- rbinom(n, 1, ili_incidence) + 1
  day1[(PROTECT_referral_day == 1) & (rbinom(n, 1, accept_PROTECT) == 1) & (cpe == 1)] <- 5
  day1[rbinom(n, 1, covid_incidence[,1][type]) == 1] <- 3
  dat <- cbind(day0, day1)
  
  #-------------------------------------
  # function for modelling daily changes
  #-------------------------------------
  
  f <- function (day) {
    
    # previous statuses
    #------------------
    sy <- dat[, day] # status yesterday
    q.cov.dur <- if (day <= duration_covid) 0 else dat[, day - duration_covid]
    q.cov.died <- if(day <= died_covid_day) 0 else dat[, day - died_covid_day]
    q.test <- if (day <= time_to_results) 0 else dat[, day - time_to_results]
    q.CARE.dur <- if (day <= duration_CARE) 0 else dat[, day - duration_CARE]
    q.sd <- if (day <= self_discharge_day) 0 else dat[, day - self_discharge_day]
    q.admission <- if (day <= admission_day) 0 else dat[, day - admission_day]
    q.admission.start <- if(day <= duration_admission) 0 else dat[, day - duration_admission]
    q.covid.susceptible <- rowSums((dat == 3) | (dat == 7)) == 0
    
    # protect exceeded capacity yesterday
    protect_full <- if (is.na(max_protect)) F else sum(sy %in% 5:7) >= max_protect
    
    # probabilities
    #--------------------
    p.died.community <- rbinom(n, 1, mortality_community) == 1
    p.died.CARE <- rbinom(n, 1, mortality_CARE) == 1
    p.self.discharge <- rbinom(n, 1, self_discharge_risk) == 1
    p.ref.CARE <- rbinom(n, 1, accept_CARE) == 1
    p.ref.PROTECT <- (rbinom(n, 1, accept_PROTECT) == 1) * !protect_full
    p.new.covid <- rbinom(n, 1, covid_incidence[type, day]) == 1 # incidence depends on both rough sleeper/hostel status, and day
    p.new.ili <- rbinom(n, 1, ili_incidence) == 1 & (!p.new.covid)
    p.new.covid.PROTECT <- rbinom(n, 1, covid_incidence_PROTECT[day]) == 1
    p.new.ili.PROTECT <- rbinom(n, 1, ili_incidence_PROTECT) == 1 & (!p.new.covid.PROTECT)
    
    # new status
    #-----------
    
    status <- sy
    
    # community covid moves forward one day 
    status[sy == 3] <- 4
    status[(sy == 4) & (q.cov.dur %in% c(3, 7))] <- 12
    
    # PROTECT 
    status[(sy == 1) & (PROTECT_referral_day == day) & p.ref.PROTECT & (cpe == 1) & identified] <- 5
    status[sy == 5 & p.new.ili.PROTECT] <- 6
    status[sy == 5 & p.new.covid.PROTECT & q.covid.susceptible] <- 7
    status[sy == 5 & p.self.discharge & (PROTECT_referral_day + self_discharge_day == day)] <- 1
    
    # new diseases in community
    status[(sy == 1) & p.new.ili] <- 2
    status[sy == 2] <- 1
    status[(sy == 1) & p.new.covid & q.covid.susceptible] <- 3
    
    # CARE: covid from hostel or PROTECT (returns to hostel if from PROTECT as recovered)
    status[(sy == 3) & p.ref.CARE & identified] <- 10
    status[(sy == 7)] <- 10
    status[(sy == 10)] <- 11
    status[(sy == 11)] <- 11
    status[(sy == 11) & (q.CARE.dur == 10)] <- 12
    status[(sy == 11) & (q.CARE.dur == 10) & (cpe == 1) & (!testing) & p.ref.PROTECT] <- 5
    status[(sy == 11) & p.self.discharge & (q.sd == 10)] <- 4
    
    # CARE: ili from hostel or PROTECT (returns to PROTECT if from PROTECT)
    status[(sy == 2) & p.ref.CARE & day <= outbreak_duration] <- 8
    status[(sy == 6)] <- 8
    status[(sy == 8)] <- 9
    status[(sy == 9)] <- 9
    status[(sy == 9) & (q.test == 8)] <- 1
    status[(sy == 9) & (q.test == 8) & (cpe == 1) & p.ref.PROTECT] <- 5
    status[(sy == 9) & p.self.discharge & (q.sd == 8)] <- 1
    
    # hospital admissions
    status[(sy == 4) & (severity %in% c(2, 3)) & (q.admission %in% c(3, 7))] <- 13
    status[(sy == 11) & (severity == 3) & (q.admission %in% c(3, 7))] <- 13
    status[sy == 13] <- 14
    status[(sy == 14) & q.admission.start == 13] <- 12
    status[(sy == 4) & (severity == 4) & (q.admission %in% c(3, 7))] <- 15
    status[(sy == 11) & (severity == 4) & (q.admission %in% c(3, 7))] <- 15
    status[sy == 15] <- 16
    status[(sy == 16) & q.admission.start == 15] <- 12
    
    # deaths
    status[(sy == 4) & p.died.community & (q.cov.died %in% c(3, 7))] <- 17
    status[(sy == 11) & p.died.CARE & (q.cov.died %in% c(3, 7))] <- 17
    status[(sy == 14) & p.died.CARE & (q.cov.died %in% c(3, 7))] <- 17
    status[(sy == 16) & p.died.CARE & (q.cov.died %in% c(3, 7))] <- 17
    
    return(status)
    
  }
  
  #----------------
  # run daily model
  #----------------
  
  for(i in 2:total_days) {
    dat <- cbind(dat, f(i))
  }
  return(list(dat,covid_incidence))
}


# Define server logic 
shinyServer(function(input, output) {
  
  output$plot_EpiCurve <- renderPlot({
    # inputs
    peak_day <- input$peak_day
    total_days <- input$total_days
    
    # main code
    out <- main.function(total_days,hostel_population,rough_sleeping_population,proportion_vulnerable,
                         all_protect,testing,max_protect,time_to_results,self_discharge_day,admission_day,
                         died_covid_day,duration_covid,duration_CARE,duration_admission,duration_PROTECT_recruitment,
                         probability_identified,accept_CARE,accept_PROTECT,self_discharge_risk,ili_incidence,ae_prob,
                         covid_severity,cfr_community,rr_vulnerable,rr_CARE,peak_day,outbreak_duration,covid_attack_hostel,
                         covid_attack_rough_sleepers,PROTECT_incidence_fraction,B)
    
    dat <- out[[1]]
    covid_incidence<- out[[2]]
    
    #--------------------------------
    # derive A&E and ambulance visits
    #--------------------------------
    
    covid_in_community <- (dat == 3) | (dat == 4)
    ae_visits <- covid_in_community * matrix(rbinom(n * (total_days + 1), 1, ae_prob), nrow = n)
    admitted_via_ae <- (dat == 13) | (dat == 15)
    ae_visits <- ae_visits + admitted_via_ae
    ambulance_trip <- (dat == 13) | (dat == 15)
    
    # number at each status by day
    #-----------------------------
    ds <- factor(dat, 1:17) 
    #ds <- t(sapply(1:17, function(x) colSums(dat == x))) # daily summary
    ds <- split(ds, rep(seq_len(total_days + 1), each = n))
    ds <- sapply(ds, table)
    
    ds2 <- rbind(`Community: susceptible` = colSums(ds[1:2,]),
                 PROTECT = colSums(ds[5:7,]),
                 `Community: recovered` = ds[12,],
                 `Community: Covid-19` = colSums(ds[3:4,]),
                 CARE = colSums(ds[8:11,]),
                 `Admitted to hospital` = colSums(ds[13:14,]),
                 ITU = colSums(ds[15:16,]),
                 Died = ds[17,])
    
    cols <- brewer.pal(nrow(ds2), 'Paired')
    
    any_hospital <- colSums(ds[13:16,])
    
    # stacked plot of statuses
    #-------------------------
    
    ds3 <- apply(ds2, 2, cumsum)
    ds3 <- rbind(0, ds3)
    
    new_cases_total <- colSums((dat == 3) | (dat == 7))
    new_cases_rough_sleepers <- colSums(((dat == 3) | (dat == 7)) & type == 2)
    cum_inc <- cumsum(new_cases_total) / n
    ymax <- ceiling(max(new_cases_total)/50) * 50
    par(xpd = NA, mar = c(5, 5, 1, 5))
    plot(1, type = 'n', xlim = c(0, total_days + 1), ylim = c(0, ymax), axes = F, xlab = NA, ylab = 'New cases')
    rect(0, 0, total_days + 1, ymax)
    title(xlab = 'Week', line = 2.5)
    rect(0:total_days, 0, 1:(total_days + 1), new_cases_total, border = NA, col = 'grey80')
    rect(0:total_days, 0, 1:(total_days + 1), new_cases_rough_sleepers, border = NA, col = 'grey60')
    lines(0:total_days + 0.5, cum_inc * ymax, col = 'white', lwd = 5)
    lines(0:total_days + 0.5, cum_inc * ymax, col = 'red')
    axis(2, 0:(ymax/50) * 50, pos = 0, las = 2)
    axis(4, 0:5/5 * ymax, paste0(0:5/5 * 100, '%'), las = 2, pos = total_days + 1, col = 'red', col.axis = 'red')
    axis(1, seq(0, floor(total_days/14)*14, 14), 0:floor(total_days/14) * 2, pos = 0)
    segments(0, 0, total_days + 1)
    text(total_days + total_days/4, ymax/2, 'Cumulative incidence', srt = 270, col = 'red')
    text(total_days - total_days / 20, max(cum_inc + 0.05) * ymax, paste0(round(max(cum_inc) * 100, 0), '%'), col = 'red')
    xs <- c(0.7, 0.97)
    ys <- c(0.2, 0.25, 0.3)
    rect(total_days * xs[1], ymax*ys[1:2], total_days * xs[2], ymax*ys[2:3], col = c('grey60', 'grey80'), border = NA)
    text(total_days * mean(xs), ymax*(ys[1:2] + diff(ys)/2), c('Rough sleepers', 'Total'), cex = 0.8, col = c('white', 'black'))
    
  })
  
  output$plot_Stacked <- renderPlot({
    # inputs
    peak_day <- input$peak_day
    total_days <- input$total_days
    
    # main code
    out <- main.function(total_days,hostel_population,rough_sleeping_population,proportion_vulnerable,
                         all_protect,testing,max_protect,time_to_results,self_discharge_day,admission_day,
                         died_covid_day,duration_covid,duration_CARE,duration_admission,duration_PROTECT_recruitment,
                         probability_identified,accept_CARE,accept_PROTECT,self_discharge_risk,ili_incidence,ae_prob,
                         covid_severity,cfr_community,rr_vulnerable,rr_CARE,peak_day,outbreak_duration,covid_attack_hostel,
                         covid_attack_rough_sleepers,PROTECT_incidence_fraction,B)
    
    dat <- out[[1]]
    covid_incidence<- out[[2]]
    
    #--------------------------------
    # derive A&E and ambulance visits
    #--------------------------------
    
    covid_in_community <- (dat == 3) | (dat == 4)
    ae_visits <- covid_in_community * matrix(rbinom(n * (total_days + 1), 1, ae_prob), nrow = n)
    admitted_via_ae <- (dat == 13) | (dat == 15)
    ae_visits <- ae_visits + admitted_via_ae
    ambulance_trip <- (dat == 13) | (dat == 15)
    
    # number at each status by day
    #-----------------------------
    ds <- factor(dat, 1:17) 
    #ds <- t(sapply(1:17, function(x) colSums(dat == x))) # daily summary
    ds <- split(ds, rep(seq_len(total_days + 1), each = n))
    ds <- sapply(ds, table)
    
    ds2 <- rbind(`Community: susceptible` = colSums(ds[1:2,]),
                 PROTECT = colSums(ds[5:7,]),
                 `Community: recovered` = ds[12,],
                 `Community: Covid-19` = colSums(ds[3:4,]),
                 CARE = colSums(ds[8:11,]),
                 `Admitted to hospital` = colSums(ds[13:14,]),
                 ITU = colSums(ds[15:16,]),
                 Died = ds[17,])
    
    cols <- brewer.pal(nrow(ds2), 'Paired')
    
    any_hospital <- colSums(ds[13:16,])
    
    # stacked plot of statuses
    #-------------------------
    
    ds3 <- apply(ds2, 2, cumsum)
    ds3 <- rbind(0, ds3)
    
    #------------------
    # summarise results
    #------------------
    
    # Stacked plot
    
    par(mar = c(4, 5, 1, 12))
    plot(1, type = 'n', xlim = c(0, total_days), ylim = c(0, n), axes = F, xlab = NA, ylab = NA)
    for(i in 1:nrow(ds2)) {
      polygon(c(0:total_days, total_days:0), c(ds3[i+1,], rev(ds3[i,])), col = cols[i])
    }
    axis(1, seq(0, floor(total_days/14)*14, 14), 0:floor(total_days/14) * 2, pos = 0)
    segments(0, 0, total_days + 1)
    axis(2, pos = 0, las = 2)
    ys <- seq(n * 0.25, n * 0.75, length.out = length(cols) + 1)
    rect(total_days * 1.1, ys[-length(ys)], total_days * 1.2, ys[-1], col = cols)
    text(total_days * 1.25, ys[-length(ys)] + diff(ys) / 2, rownames(ds2), adj = 0)
    title(xlab = 'Week', line = 2.5)
    title(ylab = 'Population', line = 4)
  })
  
  output$plot_HealthcareUse <- renderPlot({
    # inputs
    peak_day <- input$peak_day
    total_days <- input$total_days
    
    # main code
    out <- main.function(total_days,hostel_population,rough_sleeping_population,proportion_vulnerable,
                         all_protect,testing,max_protect,time_to_results,self_discharge_day,admission_day,
                         died_covid_day,duration_covid,duration_CARE,duration_admission,duration_PROTECT_recruitment,
                         probability_identified,accept_CARE,accept_PROTECT,self_discharge_risk,ili_incidence,ae_prob,
                         covid_severity,cfr_community,rr_vulnerable,rr_CARE,peak_day,outbreak_duration,covid_attack_hostel,
                         covid_attack_rough_sleepers,PROTECT_incidence_fraction,B)
    
    dat <- out[[1]]
    covid_incidence<- out[[2]]
    
    #--------------------------------
    # derive A&E and ambulance visits
    #--------------------------------
    
    covid_in_community <- (dat == 3) | (dat == 4)
    ae_visits <- covid_in_community * matrix(rbinom(n * (total_days + 1), 1, ae_prob), nrow = n)
    admitted_via_ae <- (dat == 13) | (dat == 15)
    ae_visits <- ae_visits + admitted_via_ae
    ambulance_trip <- (dat == 13) | (dat == 15)
    
    # number at each status by day
    #-----------------------------
    ds <- factor(dat, 1:17) 
    #ds <- t(sapply(1:17, function(x) colSums(dat == x))) # daily summary
    ds <- split(ds, rep(seq_len(total_days + 1), each = n))
    ds <- sapply(ds, table)
    
    ds2 <- rbind(`Community: susceptible` = colSums(ds[1:2,]),
                 PROTECT = colSums(ds[5:7,]),
                 `Community: recovered` = ds[12,],
                 `Community: Covid-19` = colSums(ds[3:4,]),
                 CARE = colSums(ds[8:11,]),
                 `Admitted to hospital` = colSums(ds[13:14,]),
                 ITU = colSums(ds[15:16,]),
                 Died = ds[17,])
    
    cols <- brewer.pal(nrow(ds2), 'Paired')
    
    any_hospital <- colSums(ds[13:16,])
    
    # stacked plot of statuses
    #-------------------------
    
    ds3 <- apply(ds2, 2, cumsum)
    ds3 <- rbind(0, ds3)
    
    # line graph of healthcare use
    #-----------------------------
    
    # ambulance, A&E, hospital, ITU, CARE, PROTECT
    
    cols <- brewer.pal(6, 'Dark2')
    
    par(xpd = NA, mar = c(4, 4, 1, 8))
    
    fpl <- function(d) {
      plot(1, type = 'n', xlim = c(0, total_days), ylim = c(0, max(d)), xlab = NA, ylab = NA, axes = F)
      rect(0, 0, total_days, max(d))
      for (i in 1:nrow(d)) lines(0:total_days, d[i,], col = cols[i])
      axis(1, seq(0, floor(total_days/14)*14, 14), 0:floor(total_days/14) * 2, pos = 0)
      segments(0, 0, total_days + 1)
      axis(2, pos = 0, las = 2)
      ys <- seq(max(d) * 0.25, max(d) * 0.75, length.out = nrow(d))
      segments(total_days * 1.1, ys, total_days * 1.2, ys, col = cols)
      text(total_days * 1.25, ys, row.names(d), adj = 0)
    }
    
    par(mar = c(4, 4, 1, 12))
    fpl(rbind(ds2[6:7,], ae = colSums(ae_visits), amb = colSums(ambulance_trip)))
    
    })
  
  output$plot_CAREPROTECT <- renderPlot({
    # inputs
    peak_day <- input$peak_day
    total_days <- input$total_days
    
    # main code
    out <- main.function(total_days,hostel_population,rough_sleeping_population,proportion_vulnerable,
                         all_protect,testing,max_protect,time_to_results,self_discharge_day,admission_day,
                         died_covid_day,duration_covid,duration_CARE,duration_admission,duration_PROTECT_recruitment,
                         probability_identified,accept_CARE,accept_PROTECT,self_discharge_risk,ili_incidence,ae_prob,
                         covid_severity,cfr_community,rr_vulnerable,rr_CARE,peak_day,outbreak_duration,covid_attack_hostel,
                         covid_attack_rough_sleepers,PROTECT_incidence_fraction,B)
    
    dat <- out[[1]]
    covid_incidence<- out[[2]]
    
    #--------------------------------
    # derive A&E and ambulance visits
    #--------------------------------
    
    covid_in_community <- (dat == 3) | (dat == 4)
    ae_visits <- covid_in_community * matrix(rbinom(n * (total_days + 1), 1, ae_prob), nrow = n)
    admitted_via_ae <- (dat == 13) | (dat == 15)
    ae_visits <- ae_visits + admitted_via_ae
    ambulance_trip <- (dat == 13) | (dat == 15)
    
    # number at each status by day
    #-----------------------------
    ds <- factor(dat, 1:17) 
    #ds <- t(sapply(1:17, function(x) colSums(dat == x))) # daily summary
    ds <- split(ds, rep(seq_len(total_days + 1), each = n))
    ds <- sapply(ds, table)
    
    ds2 <- rbind(`Community: susceptible` = colSums(ds[1:2,]),
                 PROTECT = colSums(ds[5:7,]),
                 `Community: recovered` = ds[12,],
                 `Community: Covid-19` = colSums(ds[3:4,]),
                 CARE = colSums(ds[8:11,]),
                 `Admitted to hospital` = colSums(ds[13:14,]),
                 ITU = colSums(ds[15:16,]),
                 Died = ds[17,])
    
    cols <- brewer.pal(nrow(ds2), 'Paired')
    
    any_hospital <- colSums(ds[13:16,])
    
    # stacked plot of statuses
    #-------------------------
    
    ds3 <- apply(ds2, 2, cumsum)
    ds3 <- rbind(0, ds3)
    
    # line graph of healthcare use
    #-----------------------------
    
    # ambulance, A&E, hospital, ITU, CARE, PROTECT
    
    cols <- brewer.pal(6, 'Dark2')
    
    par(xpd = NA, mar = c(4, 4, 1, 8))
    
    fpl <- function(d) {
      plot(1, type = 'n', xlim = c(0, total_days), ylim = c(0, max(d)), xlab = NA, ylab = NA, axes = F)
      rect(0, 0, total_days, max(d))
      for (i in 1:nrow(d)) lines(0:total_days, d[i,], col = cols[i])
      axis(1, seq(0, floor(total_days/14)*14, 14), 0:floor(total_days/14) * 2, pos = 0)
      segments(0, 0, total_days + 1)
      axis(2, pos = 0, las = 2)
      ys <- seq(max(d) * 0.25, max(d) * 0.75, length.out = nrow(d))
      segments(total_days * 1.1, ys, total_days * 1.2, ys, col = cols)
      text(total_days * 1.25, ys, row.names(d), adj = 0)
    }
    
    par(mar = c(4, 4, 1, 12))
    fpl(ds2[c(2, 5),])
    
  })
  
  output$text_pointEstimates <- renderTable({
    # point estimates
    #----------------
    
    sum(ds[c(3, 4, 7, 10:17), total_days + 1]) # covid cases
    sum(ds[c(3, 4, 7, 10:17), total_days + 1]) / n # actual cumulative attack rate
    
    max(colSums((dat == 8) | (dat == 9) | (dat == 10) | (dat == 11))) # peak CARE
    max(colSums((dat == 5) | (dat == 6) | (dat == 7))) # peak PROTECT
    
    ds[17, total_days + 1] # died
    ds[17, total_days + 1] / sum(ds[c(3, 4, 7, 10:17), total_days + 1])  # CFR
    
    sum(dat %in% c(13, 14)) # hospital bed days
    sum(dat == 13) # hospital admissions
    max(colSums((dat == 13) | (dat == 14))) # peak beds
    which.max(colSums((dat == 13) | (dat == 14))) # bed days peak on day...
    
    sum(dat %in% c(15, 16)) # ITU days
    sum(dat == 15) # ITU admissions
    max(colSums((dat == 15) | (dat == 16))) # peak ITU beds
    which.max(colSums((dat == 15) | (dat == 16))) # bed days peak on day...
    
    sum(ae_visits) # A&E visits
    sum(ambulance_trip) # ambulance trips
  })
  
})

