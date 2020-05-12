set.seed(20)

#-----------------
# fixed parameters
#-----------------

# hostels (actual hostel numbers and sizes)
#------------------------------------------

hl <- scan("https://raw.githubusercontent.com/maxeyre/Homeless-COVID-19/master/model/hl_hostel_beds.csv")
h_cl <- rep(seq_along(hl), hl)

# rough sleeping population (synthetic groups)
#---------------------------------------------

rs <- 10748 # overall population size

rs_mx_group <- 100
rs_cl <- sample(seq_len(rs_mx_group), rs, T, prob = 1 / seq_len(rs_mx_group))
rs_cl <- rep(seq_along(rs_cl), rs_cl)[seq_len(rs)]

# check cluster sizes
length(unique(rs_cl))
quantile(table(rs_cl))
tt <- table(table(rs_cl))
ttn <- as.numeric(names(tt))
sapply(split(tt, findInterval(ttn, c(0, 33, 66))), sum)

rs_cl <- rs_cl + max(h_cl)
  
# total population
#-----------------

cl <- c(h_cl, rs_cl) # cluster id
type <- c(rep(1, sum(hl)), rep(2, rs)) # 1 = hostel, 2 = rough sleeping
n <- length(cl) # total population size

# background incidence
#---------------------

bi <- scan("https://raw.githubusercontent.com/maxeyre/Homeless-COVID-19/master/model/lshtm_ld_1000.csv") # daily new cases in LSHTM "lockdown 1,000" scenario, ages 20-64, starting 29 Jan 2020
inc <- bi / 34339236 # ONS mid-year 2019 population estimate, ages 20-64

#---------------
# model function
#---------------

run_model <- function(

  #----------------------------------
  # inputs (values are base scenario)
  #----------------------------------

  # basic parameters
  #-----------------
  
  seed = NA,
  testing = T, # testing (if false, 'time_to_results' defaults to duration of CARE, and after CARE vulnerable population is offered PROTECT rather than discharged to community)
  max_protect = NA, # NA for no maximum PROTECT (i.e. models demand)
  model_duration = 196,
  interventions_start = 32,
  intervention_ends = NA,
  TEST = F, # test with one big cluster, no background incidence, and seeded with 10
  suppressDays = NA,
  suppressValue = 1,
  
  # population
  #-----------
  
  proportion_vulnerable = 0.37, 
  all_protect = F, # if T, everyone is offered protect regardless of vulnerability
  
  # disease and intervention
  #-------------------------
  
  # events happen on day ... (where day 0 = first day of COVID-19)
  day.inf = 2.5, # becomes infectious
  day.sym = 5, # becomes symptomatic
  day.nip = 9, # non-infectious symptomatic phase starts
  day.adm = 12, # admitted to hospital (or ITU)
  day.end = 23, # end of disease - recovers, discharged from CARE/hospital, or dies

  # other durations
  day.res = 2, # time to get result (after which negative cases are returned to community)
  dur.PROTECT_rec = 28, # PROTECT population recruited steadily over this period (days)
  
  # intervention quality
  probability_identified = 0.7, # proportion of population identified
  accept = 0.8, # proportion accepting CARE or PROTECT when offered
  self_discharge_fortnight = 0.08, # assuming 8% discharge risk over 2 weeks
  
  # case fatality and hospitalisation rates
  covid_severity_nv = c(0.4, 0.45, 0.1, 0.04, 0.01), # asymptomatic / mild / moderate / severe / critical. should sum to 1
  covid_severity_vul = c(0.1, 0.3, 0.3, 0.21, 0.09),
  cfr = c(0, 0.0001, 0.0025, 0.02, 0.6), # for non-vulnerable, by severity

  # r0
  r0_h = 1.4, # hostel
  r0_rs = 1.4, # rough sleeping
  r0_cc = 0.75, # covid care
  r0_cp = 0.75, # covid protect

  # incidence of influenza-like illness
  ili_incidence = 6.3/700

  ) {
  
  #-----------------------
  # calculate model inputs 
  #-----------------------
  
  # daily self discharge risk
  self_discharge_risk <- 1-(1-self_discharge_fortnight)^(1/14)
  
  # SEIR values
  g <- 1 / (day.nip - day.inf) # gamma
  r0c <- c(r0_h, r0_rs) # r0 community
  
  # seed
  if (!is.na(seed)) set.seed(seed)
  
  # population size, vulnerability and eligibility for PROTECT
  vulnerable <- rbinom(n, 1, proportion_vulnerable)
  cpe <- vulnerable == 1 # covid PROTECT eligible
  cpe[all_protect] <- T
  
  # covid severity
  severity_nv <- sample(seq_along(covid_severity_nv), n, replace = T, prob = covid_severity_nv)
  severity_vul <- sample(seq_along(covid_severity_vul), n, replace = T, prob = covid_severity_vul)
  severity <- ifelse(vulnerable == 0, severity_nv, severity_vul)
  
  # case fatality rates
  cfri <- cfr[severity]

  # PROTECT referral day
  PROTECT_referral_day <- sample(seq_len(dur.PROTECT_rec) + interventions_start - 1, n, replace = T)
  
  # likelihood of identification
  identified <- rbinom(n, 1, probability_identified) == 1
  
  # durations: mean
  dur.incubation <- day.inf
  dur.presymptom <- day.sym - day.inf
  dur.infSymptom <- day.nip - day.sym
  dur.nonInfect1 <- day.adm - day.nip
  dur.hospitaliz <- day.end - day.adm
  
  # durations: individual (in the published data we used 'rdgamma' from the package 'extraDistr')
  day.inf.i <- rpois(n, dur.incubation - 1) + 1
  day.sym.i <- rpois(n, dur.presymptom - 1) + 1 + day.inf.i
  day.nip.i <- rpois(n, dur.infSymptom - 1) + 1 + day.sym.i
  day.adm.i <- rpois(n, dur.nonInfect1 - 1) + 1 + day.nip.i
  day.end.i <- rpois(n, dur.hospitaliz - 1) + 1 + day.adm.i
  
  # duration people with ili in CARE
  day.ili <- if (testing) day.res else day.end - day.sym - 1
  
  # R0 supress
  r0suppress <- rep(F, model_duration)
  r0suppress[suppressDays] <- T
  
  # intervention ends
  ends <- min(model_duration, intervention_ends, na.rm = T)
  
  # TEST values
  cl <- if (TEST) type else cl
  inc <- if (TEST) rep(0, model_duration) else inc
  
  #-------------------------------------
  # function for modelling daily changes
  #-------------------------------------
  
  # status key
  #-----------
  
  # 1 = community: susceptible
  # 2 = community: ili
  # 3 = community: exposed
  # 4 = community: infectious pre-symptomatic
  # 5 = community: infectious symptomatic
  # 6 = community: not infectious symptomatic
  # 7 = community: recovered
  
  # 8 = PROTECT: susceptible
  # 9 = PROTECT: ili
  # 10 = PROTECT: exposed
  # 11 = PROTECT: infectious pre-symptomatic
  # 12 = PROTECT: infectious symptomatic (transferred to COVID-CARE next day)
  # 13 = PROTECT: recovered
  
  # 14 = CARE: ili / day 1
  # 15 = CARE: ili / other days	
  # 16 = CARE: exposed
  # 17 = CARE: infectious pre-symptomatic
  # 18 = CARE: infectious symptomatic
  # 19 = CARE: not infectious symptomatic
  
  # 20 = hospital
  # 21 = ITU
  # 22 = died
  
  nis <- c(3, 10, 16) # new infection status
  
  f <- function (day) {
    
    # previous statuses
    #------------------
    
    sy <- dat[, day] # status yesterday
    
    # duration after COVID19 infection (varying by individual)
    #---------------------------------------------------------
    
    q.inf <- (day - day.inf.i) == day_of_infection # becomes infectious today
    q.sym <- (day - day.sym.i) == day_of_infection # becomes symptomatic today
    q.CAR <- (day - day.sym.i) == (day_of_infection + 1) # offered COVID CARE
    q.nip <- (day - day.nip.i) == day_of_infection # becomes non-infectious
    q.adm <- (day - day.adm.i) == day_of_infection # admitted today
    q.end <- (day - day.end.i) == day_of_infection # COVID-19 ends today
    
    # end of CARE for those in ILI (longer if no testing)
    #----------------------------------------------------
    
    q.ild <- if (day <= day.ili) F else dat[, day - day.ili] == 14 # end of ILI in CARE

    # force of infection by individual
    #---------------------------------
    
    location <- findInterval(sy, c(0, 8, 14, 20)) # community, PROTECT, CARE, hospital/died
    iN <- sapply(split(sy, cl), function(x) sum(x == 4 | x == 5) / sum(x < 8))
    iN <- iN[cl]
    iN[location == 2] <- sum(sy == 11 | sy == 12) / sum(location == 2) # covid PROTECT
    iN[location == 3] <- sum(sy == 17 | sy == 18) / sum(location == 3) # covid CARE
    r0i <- r0c[type]
    r0i[r0suppress[day]] <- suppressValue
    r0i[location == 2] <- r0_cp
    r0i[location == 3] <- r0_cc
    foi <- 1 - exp(-iN * g * r0i)
    foi[is.na(foi)] <- 0 # is.na includes is.nan (changing those with zero denominator to zero)
    
    # protect exceeded capacity yesterday (if so, then doesn't accept anyone today)
    #------------------------------------------------------------------------------
    
    protect_full <- if (is.na(max_protect)) F else sum(location == 2) >= max_protect

    # intervention open
    #------------------
    
    started <- (day >= interventions_start) & (day <= ends)
    
    # probabilities
    #--------------
    
    p.cv1 <- rbinom(n, 1, foi) == 1 # FOI
    p.cv2 <- rbinom(n, 1, inc[day]) == 1 # background exposure
    p.cv3 <- p.cv1 | p.cv2 # FOI or background
    p.dth <- rbinom(n, 1, cfri) == 1
    p.sdg <- rbinom(n, 1, self_discharge_risk) == 1 # self-discharge risk
    p.rCC <- (rbinom(n, 1, accept) == 1) & (severity != 1) & started # accept COVID-CARE referral
    p.rCP <- (rbinom(n, 1, accept) == 1) & !protect_full & started # accept COVID-PROTECT referral
    p.ili <- rbinom(n, 1, ili_incidence) == 1 & (!p.cv3) # new ILI

    # new status
    #-----------
    
    status <- sy
    
    # covid in community
    status[(sy == 1) & p.cv3] <- 3
    status[location == 1 & q.inf] <- 4
    status[location == 1 & q.sym] <- 5
    status[location == 1 & q.nip] <- 6
    status[location == 1 & q.CAR & p.rCC & identified] <- 18
    status[(sy == 1) & (PROTECT_referral_day == day) & p.rCP & cpe & identified] <- 8
    status[(sy == 3 | sy == 4) & (PROTECT_referral_day == day) & p.rCP & cpe & identified] <- 10

    # new ili in community
    status[(sy == 1) & p.ili] <- 2
    status[sy == 2] <- 1
    status[(sy == 2) & p.rCC] <- 14
    
    # PROTECT 
    status[sy == 8 & p.ili] <- 9 
    status[sy == 9] <- 14 # transferred to COVID-CARE
    status[sy == 8 & p.cv3] <- 10
    status[location == 2 & q.inf] <- 11
    status[location == 2 & q.sym] <- 12
    status[sy == 12] <- 18 # transferred to COVID-CARE
    status[sy == 8 & p.sdg] <- 1 # self-discharge
    status[sy == 13 & p.sdg] <- 7 # self-discharge
    status[sy == 8 & !started] <- 1
    status[sy == 13 & !started] <- 7
    
    # CARE: covid
    status[location == 3 & q.inf] <- 17
    status[location == 3 & q.sym] <- 18
    status[location == 3 & q.nip] <- 19
    status <- ifelse(location == 3 & p.sdg, sy - 13, status) 
    status <- ifelse(location == 3 & (sy == 14 | sy == 15) & p.sdg, 1, status) 
    
    # CARE: ili
    status[sy == 14] <- 15
    status[sy == 15 & q.ild] <- 1
    status[sy == 15 & p.cv3] <- 10
    status[sy == 15 & p.sdg] <- 1
    status[sy == 15 & q.ild & cpe & p.rCP] <- 8

    # hospital admissions
    status[(location == 1) & q.adm & (severity == 3 | severity == 4)] <- 20
    status[(location == 3) & q.adm & severity == 4] <- 20
    status[q.adm & (severity == 5)] <- 21
    
    # end of covid
    status[q.end] <- 7
    status[(sy == 20 | sy == 21 | location == 3) & q.end & cpe & p.rCP] <- 13
    status[q.end & p.dth] <- 22
    
    return(status)
    
  }
  
  #----------------
  # run daily model
  #----------------
  
  dat <- matrix(rep(1, n)) # day 0
  day_of_infection <- rep(-Inf, n)
  
  # seed hostel and rough sleeping groups (for TEST scenario)
  if (TEST) {dat[,1][c(1:10, length(h_cl+1):(length(h_cl) + 11))] <- 3}

  for(i in seq_len(model_duration)) {
    dat <- cbind(dat, f(i))
    day_of_infection[(dat[,i+1] %in% c(3, 10, 16)) & (day_of_infection == -Inf)] <- i + 1
  }

  return(dat)
  
}
