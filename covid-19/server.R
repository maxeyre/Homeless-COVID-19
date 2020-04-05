# server.R

library(shiny)
library(RColorBrewer)

#===========
# STATUS KEY
#===========

# 1 = community: susceptible and asymptomatic
# 2 = community: ili symptoms (not covid)
# 3 = community: covid day 1
# 4 = community: covid, other days
# 5 = PROTECT: susceptible
# 6 = PROTECT: ili (all transferred to covid CARE next day)
# 7 = PROTECT: covid (all transferred to covid CARE next day)
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
# 18 = CARE with new COVID: day 1
# 19 = PROTECT: recovered: day 1
# 20 = PROTECT: recovered: other days

#============
# REGION DATA
#============

region.pop <- read.csv("https://raw.githubusercontent.com/maxeyre/COVID-19/master/homeless_pop.csv", stringsAsFactors = F)
region.pop <- rbind(region.pop, c("Custom", 3500, 1100))
list.regions <- as.list(region.pop$region)
names(list.regions) <- region.pop$region

#==============
# MAIN FUNCTION
#==============

main.function <- function(outbreak_duration,hostel_population,rough_sleeping_population,proportion_vulnerable,
                          all_protect,testing,max_protect,time_to_results,self_discharge_day,admission_day,duration_admission,duration_PROTECT_recruitment,
                          probability_identified,accept,self_discharge_risk,ili_incidence,ae_prob,
                          cfr,rr_CARE,peak_day,covid_attack_hostel,
                          covid_attack_rough_sleepers,PROTECT_incidence_fraction,B,covid_severity_nv,covid_severity_vul,seed){
  
  #-----------------------
  # calculate model inputs 
  #-----------------------
  
  # set peak day to <= outbreak duration
  peak_day <- min(peak_day, outbreak_duration)
  
  # model duration
  total_days <- outbreak_duration + 20
  
  # derived durations
  died_covid_day <- admission_day + duration_admission
  duration_covid <- died_covid_day # day of recovery (for those not hospitalised)
  duration_CARE <- duration_covid
  
  # covid community incidence
  triangular_pattern <- c(seq(0, 1, length.out = peak_day), seq(1, 0, length.out = outbreak_duration - peak_day))
  fi <- function(x) 1 - prod(1-(1 / ( 1 + (triangular_pattern / (1-triangular_pattern))^(-B)) * x))   # returns cumulative attack rate given incidence
  fi2 <- function(ca) { # returns curve given cumulative attack
    incs <- 0:10000 / 10000 # incidences to try
    attacks <- sapply(incs, fi)
    peak_incidence <- incs[which.min(abs(attacks - ca))]
    inc <- 1 / ( 1 + (triangular_pattern / (1-triangular_pattern))^(-B)) * peak_incidence
    post_outbreak <- if (total_days > outbreak_duration) total_days - outbreak_duration else 0
    c(inc, rep(0, post_outbreak))
  }
  covid_incidence <- rbind(fi2(covid_attack_hostel), covid_incidence_rough_sleeper <- fi2(covid_attack_rough_sleepers))
  
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
  
  # incidence in PROTECT (same multiplier / fraction of community incidence as used in COVID)
  ili_incidence_PROTECT <- ili_incidence * PROTECT_incidence_fraction
  covid_incidence_PROTECT <- covid_incidence[1,] * PROTECT_incidence_fraction
  
  # PROTECT referral day
  PROTECT_referral_day <- sample(1:duration_PROTECT_recruitment, n, replace = T)
  
  # likelihood of identification
  identified <- rbinom(n, 1, probability_identified) == 1
  
  # days 0 and 1
  day0 <- rep(1, n)
  day1 <- rbinom(n, 1, ili_incidence) + 1
  day1[(PROTECT_referral_day == 1) & (rbinom(n, 1, accept) == 1) & (cpe == 1)] <- 5
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
    
    # protect exceeded capacity yesterday (if so, then doesn't accept anyone today)
    protect_full <- if (is.na(max_protect)) F else sum(sy %in% 5:7) >= max_protect
    
    # probabilities
    #--------------------
    p.died.community <- rbinom(n, 1, cfr_community) == 1
    p.died.CARE <- rbinom(n, 1, cfr_CARE) == 1
    p.self.discharge <- rbinom(n, 1, self_discharge_risk) == 1
    p.ref.CARE <- (rbinom(n, 1, accept) == 1) & (severity != 1)
    p.ref.PROTECT <- (rbinom(n, 1, accept) == 1) * !protect_full
    p.new.covid <- rbinom(n, 1, covid_incidence[type, day]) == 1 # incidence depends on both rough sleeper/hostel status, and day
    p.new.covid.CARE <- rbinom(n, 1, covid_incidence[1, day]) == 1 # incidence in CARE is same as in hostels
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
    status[sy == 5 & p.new.covid.PROTECT] <- 7
    status[sy == 5 & p.self.discharge & (PROTECT_referral_day + self_discharge_day == day)] <- 1
    status[sy == 20 & (q.sd == 19) & p.self.discharge] <- 12
    status[sy == 19] <- 20
    
    # new diseases in community
    status[(sy == 1) & p.new.ili] <- 2
    status[sy == 2] <- 1 # ILI only modelled to last 1 day
    status[(sy == 1) & p.new.covid] <- 3
    
    # CARE: covid
    status[(sy == 3) & p.ref.CARE & identified] <- 10
    status[(sy == 7)] <- 10
    status[(sy == 10)] <- 11
    status[(sy == 11) & (q.CARE.dur == 10 | q.CARE.dur == 18)] <- 12
    status[(sy == 11) & (q.CARE.dur == 10) & (cpe == 1) & (!testing) & p.ref.PROTECT] <- 19
    status[(sy == 11) & p.self.discharge & (q.sd == 10)] <- 4
    status[(sy == 18)] <- 11
    
    # CARE: ili
    status[(sy == 2) & p.ref.CARE & (day <= outbreak_duration)] <- 8
    status[(sy == 6) & (day <= outbreak_duration)] <- 8
    status[(sy == 8)] <- 9
    status[(sy == 9) & (q.test == 8)] <- 1
    status[(sy == 9) & (q.test == 8) & (cpe == 1) & p.ref.PROTECT] <- 5
    status[(sy == 9) & p.self.discharge & (q.sd == 8)] <- 1
    status[(sy %in% c(8, 9)) & p.new.covid.CARE] <- 18 # new covid
    status[(sy == 18) & p.new.covid.CARE] <- 11
    
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
    day_of_death <- q.cov.died %in% c(3, 7, 18)
    status[(sy == 4) & p.died.community & day_of_death] <- 17
    status[(sy %in% c(10, 11, 13:16)) & p.died.CARE & day_of_death] <- 17
    
    return(status)
    
  }
  
  #----------------
  # run daily model
  #----------------
  
  for(i in 2:total_days) {
    dat <- cbind(dat, f(i))
  }
  
  return(list(dat, covid_incidence))
  
}

i <- 0 ## What does this do? ##

#====================
# DEFINE SERVER LOGIC
#====================

shinyServer(function(input, output, session) {
  
  observe({
    val <- input$outbreak_duration
    # Peak day maximum < outbreak duration
    updateSliderInput(session, "peak_day", value = 40,
                      min = 0, max = val)
  })
  
  observe({
    val2 <- as.character(input$region_choice)
    val_rough <- region.pop$rough_pop[region.pop$region==val2]
    val_hostel <- region.pop$hostel_pop[region.pop$region==val2]
    
    # Populate homeless/rough populations based on region choice
    updateNumericInput(session, "hostel_population", value = val_hostel,
                       min = 0, max = 50000)
    updateNumericInput(session, "rough_sleeping_population", value = val_rough,
                       min = 0, max = 50000)
  })
  
  # Add a link to a website
  url <- a("UCL CCIH", href="https://www.ucl.ac.uk/inclusion-health") # define the link and text
  output$url <- renderUI({
    tagList(url) # now outputted as output$url
  })
  
  
  # Code for all reactive calculations
  plots <- eventReactive(input$action, {
    
    #-------
    # Inputs
    #-------
    
    # epidemic parameters
    peak_day <- input$peak_day
    outbreak_duration <- input$outbreak_duration
    B <- 1.75 # LEAVING THIS AS FIXED; parameter for 'shape' of curve (not much value in changing)
    covid_attack_hostel <- input$covid_attack_hostel # anything less than 1
    covid_attack_rough_sleepers <- input$covid_attack_rough_sleepers # anything less than 1
    PROTECT_incidence_fraction <- 1 - input$PROTECT_incidence_fraction # incidence of covid & ILI in PROTECT is x * hostel rate
    ili_incidence <- 6.3/700 # LEAVING THIS AS FIXED; https://bmcinfectdis.biomedcentral.com/articles/10.1186/1471-2334-14-232
    
    # case fatality and hospitalisation rates
    covid_severity_nv <- c(0.4, 0.45, 0.1, 0.04, 0.01) # asymptomatic / mild / moderate / severe / critical. should sum to 1
    covid_severity_vul <- c(0.1, 0.3, 0.3, 0.21, 0.09)
    cfr <- c(0, 0.0001, 0.0025, 0.015, 0.5) # for non-vulnerable, by severity
    rr_CARE <- input$rr_CARE # risk ratio for mild and moderate cases in covid CARE
    
    # intervention quality
    probability_identified <- input$probability_identified # proportion of population identified
    accept <- input$accept_CARE # proportion accepting CARE
    self_discharge_risk <- input$self_discharge_risk
    
    # population
    hostel_population <- input$hostel_population
    rough_sleeping_population <- input$rough_sleeping_population
    proportion_vulnerable <- input$proportion_vulnerable
    
    # intervention parameters
    all_protect <- input$all_protect # F if T, everyone is offered protect regardless of vulnerability
    all_protect <- if (all_protect=="TRUE") TRUE else FALSE
    testing <- input$testing #T # testing (if false, 'time_to_results' defaults to duration of CARE, and after CARE vulnerable population is offered PROTECT rather than discharged to community)
    testing <- if (testing == 'TRUE') TRUE else FALSE

    # capacity limit in PROTECT
    max_protect <- if(input$max_protect_binary == 'FALE') NA else input$max_protect_value
    
    # timings (in days)
    time_to_results <- input$time_to_results # time to get result (after which negative cases are returned to community)
    self_discharge_day <- input$self_discharge_day
    admission_day <- input$admission_day # to hospital or ITU
    died_covid_day <- input$died_covid_day
    duration_covid <- input$duration_covid # day of recovery (for those not hospitalised)
    ae_prob <- 1/duration_covid # daily probability of A&E during COVID illness: average 1 visit per illness
    
    # durations (days)
    duration_CARE <- input$duration_CARE 
    duration_admission <- input$duration_admission
    duration_PROTECT_recruitment <- input$duration_PROTECT_recruitment # PROTECT population recruited steadily over this period (days)
    
    # seed
    seed <- 34
    
    #-----------
    # Run models
    #-----------
    
    # base
    #-----
    
    out <- main.function(outbreak_duration,hostel_population,rough_sleeping_population,proportion_vulnerable,
                         all_protect,testing,max_protect,time_to_results,self_discharge_day,admission_day,duration_admission,duration_PROTECT_recruitment,
                         probability_identified,accept,self_discharge_risk,ili_incidence,ae_prob,
                         cfr,rr_CARE,peak_day,covid_attack_hostel,
                         covid_attack_rough_sleepers,PROTECT_incidence_fraction,B,covid_severity_nv,covid_severity_vul,seed)
    
    dat_base <- out[[1]]
    covid_incidence_base <- out[[2]]
    
    # no intevention model
    #---------------------
    
    out_no_interv <- main.function(outbreak_duration,hostel_population,rough_sleeping_population,proportion_vulnerable,
                                   all_protect,testing,max_protect,time_to_results,self_discharge_day,admission_day,duration_admission,duration_PROTECT_recruitment,
                                   probability_identified,accept=0,self_discharge_risk,ili_incidence,ae_prob,
                                   cfr,rr_CARE,peak_day,covid_attack_hostel,
                                   covid_attack_rough_sleepers,PROTECT_incidence_fraction,B,covid_severity_nv,covid_severity_vul,seed)
    
    dat_noint <- out_no_interv[[1]]
    covid_incidence_noint <- out_no_interv[[2]]
    
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
  
    points_base <- point_estimates(dat_base)
    points_noint <- point_estimates(dat_noint)
    
    # daily summaries
    #----------------
    
    dsf <- function(fm) {
      ds <- t(sapply(1:20, function(x) colSums(fm == x)))
      ds2 <- rbind(`Community: susceptible` = colSums(ds[1:2,]),
                   `COVID-PROTECT` = colSums(ds[c(5:7,19:20),]),
                   `Community: recovered` = ds[12,],
                   `Community: COVID-19` = colSums(ds[3:4,]),
                   `COVID-CARE` = colSums(ds[c(8:11,18),]),
                   `Admitted to hospital` = colSums(ds[13:14,]),
                   `Admitted to ITU` = colSums(ds[15:16,]),
                   Died = ds[17,])
      list(ds, ds2, rbind(0, apply(ds2, 2, cumsum)))
    }
    
    ds_base <- dsf(dat_base)
    ds_noint <- dsf(dat_noint)
    
    #------
    # plots
    #------
    
    # recalculate parameters for graphs
    n <- hostel_population + rough_sleeping_population
    type <- c(rep(1, hostel_population), rep(2, rough_sleeping_population))
    
    # function for making y-axes
    yaxt <- c(outer(c(1, 2.5, 5), 10^(0:6), '*')) 
    yaxt <- yaxt[yaxt != 2.5]
    yxf <- function(ymax, tk = 5, type = 'next') { 
      tm <- yaxt[which.min(abs(yaxt - ymax / tk))]
      nticks <- if (type == 'next') ceiling(ymax / tm) else floor(ymax / tm)
      seq(0, tm * nticks, tm)
    }                  
    
    # Tab 1 - epidemic curve
    #-----------------------
    
    nd <- ncol(dat_base)
    total_days <- ncol(dat_base) -1
    
    new_cases_total <- colSums(ds_base[[1]][c(3, 7, 18),])
    new_cases_noint <- colSums(ds_noint[[1]][c(3, 7, 18),])
    new_cases_rough_sleepers <- colSums(((dat_base == 3) | (dat_base == 7) | dat_base == 18) & type == 2)
    cum_inc <- cumsum(new_cases_total) / n
    cum_inc_noint <- cumsum(new_cases_noint) / n
    ymax <- ceiling(max(new_cases_total)/50) * 50
    
    plot(1, type = 'n', xlim = c(0, nd * 1.5), ylim = c(0, ymax), axes = F, xlab = NA, ylab = NA)
    rect(0, 0, nd + 1, ymax)
    title(xlab = 'Week', line = 2.5)
    title(ylab = 'New cases', line = 2)
    rect(0:nd, 0, 1:(nd+1), new_cases_total, border = NA, col = 'grey80')
    rect(0:nd, 0, 1:(nd+1), new_cases_rough_sleepers, border = NA, col = 'grey60')
    lines(0:(nd-1) + 0.5, cum_inc * ymax, col = 'white', lwd = 5)
    lines(0:(nd-1) + 0.5, cum_inc_noint * ymax, col = 'white', lwd = 5)
    lines(0:(nd-1) + 0.5, cum_inc * ymax, col = 'red')
    lines(0:(nd-1) + 0.5, cum_inc_noint * ymax, col = 'red', lty = 3)
    axis(2, yxf(ymax, type = 'previous'), pos = 0, las = 2)
    axis(4, 0:5/5 * ymax, paste0(0:5/5 * 100, '%'), las = 2, pos = nd + 1, col = 'red', col.axis = 'red')
    axis(1, seq(0, floor(nd/14)*14, 14), 0:floor(nd/14) * 2, pos = 0)
    segments(0, 0, nd + 1)
    text(nd * 1.15, ymax/2, 'Cumulative incidence', srt = 270, col = 'red')
    ys <- ymax * seq(0.35, 0.65, length.out = 5)
    mid_ys <- ys[-length(ys)] + diff(ys)/2
    rect(nd * 1.2, ys[1:2], nd * 1.25, ys[2:3], col = c('grey60', 'grey80'), border = NA)
    segments(nd * 1.2, mid_ys[3:4], nd * 1.25, col = 'red', lty = c(1, 3))
    text(nd * 1.28, mid_ys, c('Rough sleepers', 'Total', 'With intervention', 'Without intervention'), adj = 0, col = c('black', 'black', 'red', 'red'))

    p1 <- recordPlot()
    
    # Tab 2 - stacked plot
    #---------------------
    
    cols <- brewer.pal(nrow(ds_base[[2]]), 'Paired')
    plot(1, type = 'n', xlim = c(0, nd * 1.5), ylim = c(0, n), axes = F, xlab = NA, ylab = NA)
    for(i in 1:nrow(ds_base[[2]])) {
      polygon(c(0:total_days, total_days:0), c(ds_base[[3]][i+1,], rev(ds_base[[3]][i,])), col = cols[i])
    }
    axis(1, seq(0, floor(total_days/14)*14, 14), 0:floor(total_days/14) * 2, pos = 0)
    segments(0, 0, total_days + 1)
    axis(2, yxf(n, type = 'previous'), pos = 0, las = 2)
    ys <- seq(n * 0.25, n * 0.75, length.out = length(cols) + 1)
    rect(total_days * 1.07, ys[-length(ys)], total_days * 1.14, ys[-1], col = cols)
    text(total_days * 1.19, ys[-length(ys)] + diff(ys) / 2, rownames(ds_base[[2]]), adj = 0)
    title(xlab = 'Week', line = 2.5)
    title(ylab = 'Population', line = 2)

    p2 <- recordPlot()
    
    # Tab 3 - healthcare use 
    #-----------------------
    
    cols <- brewer.pal(4, 'Set1')
    fpl <- function(d, yrange = 0.15) {
      ymax <- max(d) * 1.1
      plot(1, type = 'n', xlim = c(0, nd * 1.5), ylim = c(0, ymax), xlab = NA, ylab = NA, axes = F)
      for (i in 1:nrow(d)) lines(0:total_days-1, d[i,], col = cols[i], lwd = 2)
      axis(1, seq(0, floor(total_days/14)*14, 14), 0:floor(total_days/14) * 2, pos = 0)
      segments(0, 0, total_days + 1)
      axis(2, yxf(ymax, type = 'previous'), pos = 0, las = 2)
      rect(0, 0, total_days, ymax)
      ys <- seq(ymax * (0.5-yrange), ymax * (0.5+yrange), length.out = nrow(d))
      segments(total_days * 1.1, ys, total_days * 1.2, ys, col = cols, lwd = 2)
      text(total_days * 1.25, ys, row.names(d), adj = 0)
      title(xlab = 'Week', line = 2.5)
      title(ylab = 'Number', line = 2)
    }
    
    admissions <- colSums(ds_base[[1]][c(13, 15),])
    covid_days_community <- colSums(ds_base[[1]][c(3, 4),])
    ae_visits  <- round(covid_days_community * (1/18), 0) + admissions

    fpl(rbind(ds_base[[2]][6:7,], `A&E visits` = ae_visits, `Ambulance journeys` = admissions))

    p3 <- recordPlot()
    
    # Tab 4 - CARE & PROTECT use
    #---------------------------
    
    fpl(ds_base[[2]][c(5, 2),], yrange = 0.05)

    p4 <- recordPlot()
    
    # Tab 5 - deaths
    #---------------
    
    deaths <- (cbind(0, dat_base[,-ncol(dat_base)]) != 17) & (dat_base == 17)
    deaths_noint <- (cbind(0, dat_noint[,-ncol(dat_noint)]) != 17) & (dat_noint == 17)
    deaths <- colSums(deaths)
    cum_deaths <- cumsum(deaths)
    cum_deaths_noint <- cumsum(colSums(deaths_noint))
    max_deaths <- max(c(cum_deaths, cum_deaths_noint))
    ymax <- max(deaths)
    plot(1, type = 'n', xlim = c(0, nd * 1.5), ylim = c(0, ymax), axes = F, xlab = NA, ylab = NA)
    title(ylab = 'Daily deaths\n(with intervention)', line = 2)
    rect(0:total_days, 0, 1:nd, deaths, col = "#8DA0CB", border = NA)
    axis(1, seq(0, floor(total_days/14)*14, 14), 0:floor(total_days/14) * 2, pos = 0)
    segments(0, 0, nd)
    rect(0, 0, nd, ymax)
    axis(2, yxf(ymax, type = 'previous'), pos = 0, las = 2)
    title(xlab = 'Week', line = 2.5)
    text(total_days * 0.05, ymax * 0.95, paste0('Total = ', sum(deaths)), adj = 0)
    lines(0:(nd-1), (cum_deaths / max_deaths) * ymax, col = 'white', lwd = 5)
    lines(0:(nd-1), (cum_deaths_noint / max_deaths) * ymax, col = 'white', lwd = 5)
    lines(0:(nd-1), (cum_deaths / max_deaths) * ymax, col = 'red')
    lines(0:(nd-1), (cum_deaths_noint / max_deaths) * ymax, col = 'red', lty = 3)
    yx <- yxf(max_deaths, type = 'previous')
    axis(4, (yx/max_deaths) * ymax, yx, pos = nd, las = 2, col = 'red', col.axis = 'red')
    text(nd * 1.2, ymax/2, 'Cumulative deaths', srt = 90, col = 'red')
    
    p5 <- recordPlot()
    
    # Table
    #------
    
    estimates_out <- data.frame(Parameter=c("Peak CARE beds","Peak PROTECT beds","COVID-19 cases","Attack rate / 1000","Deaths","CFR / 1000",
                                            "Hospital admissions","Hospital bed days","Peak bed demand",
                                            "Day of bed day peak","ITU admissions","ITU bed days","Peak ITU bed days","Day of ITU peak",
                                            "A&E visits","Ambulance trips"))
    estimates_out$`With intervention` <- points_base
    estimates_out$`Without intervention` <- points_noint
    dif <- estimates_out$`With intervention` - estimates_out$`Without intervention`
    estimates_out$`% change` <- paste0(round(dif / estimates_out$`Without intervention` * 100, 1), '%')
    estimates_out$`% change`[c(1, 2, 4, 10, 14)] <- ''
    estimates_out$`With intervention` <- format(round(estimates_out$`With intervention`, 0), big.mark = ',')
    estimates_out$`Without intervention` <- format(round(estimates_out$`Without intervention`, 0), big.mark = ',')
    
    # Return outputs
    #---------------
    
    return(list(p1,p2,p3,p4,p5,estimates_out))
    
  })
  
  # Tab 1 plot
  output$plot_EpiCurve <- renderPlot({
    plots()[[1]]
  })
  
  # Tab 2 plot
  output$plot_Stacked <- renderPlot({
    plots()[[2]]
  })
  
  # Tab 3 plot
  output$plot_HealthcareUse <- renderPlot({
    plots()[[3]]
  })
  
  # Tab 4 plot
  output$plot_CAREPROTECT <- renderPlot({
    plots()[[4]]
  })
  
  # Tab 5 plot
  output$plot_deaths <- renderPlot({
    plots()[[5]]
  })
  
  # Reactive text for summaries
  output$table_epidemic <- renderTable({
    estimates_out <- plots()[[6]][1:6,]
    estimates_out
  })
  output$table_healthcare <- renderTable({
    estimates_out <- plots()[[6]][7:16,]
    estimates_out
  })
  
  
  # Point estimates
  output$text_pointEstimates <- renderDataTable({
    
  })
  
})