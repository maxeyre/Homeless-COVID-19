set.seed(20)

#--------------
# 1. STATUS KEY
#==============

# The model assumes that individuals are in one of 16 discrete statuses:

# 1 = community: susceptible
# 2 = community: exposed
# 3 = community: infectious pre-symptomatic
# 4 = community: infectious symptomatic
# 5 = community: not infectious symptomatic
# 6 = community: recovered

# 7 = PROTECT: susceptible
# 8 = PROTECT: exposed
# 9 = PROTECT: infectious pre-symptomatic
# 10 = PROTECT: infectious symptomatic (transferred to COVID-CARE next day)
# 11 = PROTECT: recovered

# 12 = CARE: infectious symptomatic
# 13 = CARE: not infectious symptomatic

# 14 = hospital
# 15 = ITU
# 16 = died

#--------------------
# 2. FIXED PARAMETERS
#====================

# population size and clusters
#-----------------------------

# hostels (actual hostel numbers and sizes)
hl <- scan("https://raw.githubusercontent.com/maxeyre/Homeless-COVID-19/master/model/hl_hostel_beds.csv")
h_cl <- rep(seq_along(hl), hl)

# overall rough sleeping population size (street sleeping + night shelters)
rs <- 10748

# night shelters (based on night shelters in London)
number_in_ns <- round(963 / (1136/4266), 0)
nss <- scan("https://raw.githubusercontent.com/maxeyre/Homeless-COVID-19/master/model/nightshelter_sizes_london.csv")
ns_sample <- sample(nss, floor(number_in_ns / mean(nss) * 1.5), replace = T)
ns_sample <- ns_sample[cumsum(ns_sample) < number_in_ns]
ns_sample <- c(ns_sample, number_in_ns - sum(ns_sample))
ns_cl <- rep(seq_along(ns_sample), ns_sample)
ns_cl <- ns_cl + max(h_cl)

# street-sleeping groups (synthetic)
street_sleeping_pop <- rs - number_in_ns
ss_mx_group <- 100
ss_cl <- sample(seq_len(ss_mx_group), street_sleeping_pop, T, prob = 1 / seq_len(ss_mx_group))
ss_cl <- rep(seq_along(ss_cl), ss_cl)[seq_len(street_sleeping_pop)]
ss_cl <- ss_cl + max(ns_cl)

# check steet-sleeping cluster sizes
length(unique(ss_cl))
quantile(table(ss_cl))

# total population
cl <- c(h_cl, ns_cl, ss_cl) # cluster id
type <- c(rep(1, sum(hl)), rep(2, number_in_ns), rep(3, street_sleeping_pop)) # 1 = hostel, 2 = night shelters, 3 = street-sleeping
n <- length(cl) # total population size

# background incidence
#---------------------

background_incidence <- scan("https://raw.githubusercontent.com/maxeyre/Homeless-COVID-19/master/model/gp_cases_19june2020.csv")
background_incidence <- background_incidence / 56286961

#------------------
# 3. MODEL FUNCTION
#==================

run_model <- function(
  
  #----------------------------------
  # inputs (values are base scenario)
  #----------------------------------
  
  # basic parameters
  #-----------------
  
  seed = NA,
  model_duration = 123,
  interventions_start = 32,
  intervention_ends = NA,
  gen_pop_mix_start = NA,
  gen_pop_mix_end = NA,
  gen_pop_mix_para = 1,
  bi = background_incidence,
  TEST = F, # test with one big cluster, no background incidence, and seeded with 10 exposed on day 1

  # intervention
  #-------------
  
  dur.PROTECT_rec = 28, # PROTECT population recruited steadily over this period (days)
  accept_ss = 0.8, # proportion street sleepers PROTECT when offered
  accept_CARE = 0.8, # proportion accepting CARE when offered
  self_discharge_fortnight = 0.05, # assuming 5% discharge risk over 2 weeks
  
  # disease 
  #--------
  
  # disease durations
  dur.exposed = 4,
  dur.presymptom = 1.5,
  dur.infSymptom = 3.5,
  dur.hospital = 7,
  
  # case fatality and hospitalisation rates
  covid_severity = c(0.496, 0.46, 0.038, 0.006), # asymptomatic / mild / moderate / severe. should sum to 1
  cfr = c(0, 0.005, 0.3, 0.45), # by severity
  
  # r0
  r0_h = 2.7, # hostel
  r0_ns = 3.1, # night shelters
  r0_ss = 2.3, # street sleeping
  r0_cp = 0.5, # covid protect
  suppress_start = NA,
  suppress_end = NA,
  suppressValue = 1,
  k = 1 # distribution of R0 values. Positive value.ma Set to NA for uniform values of R0. High values approximate to poisson distribution
  
) {
  
  #-----------------------
  # calculate model inputs 
  #-----------------------
  
  # seed
  if (!is.na(seed)) set.seed(seed)
  
  # daily self discharge risk
  self_discharge_risk <- 1-(1-self_discharge_fortnight)^(1/14)
  
  # SEIR values
  g <- 1 / (dur.presymptom + dur.infSymptom) # gamma
  r0i_period2 <- c(r0_h, r0_ns, r0_ss) # r0 community
  r0i_period2 <- r0i_period2[type] # average r0 by individual
  if (!is.na(k)) {r0i_period2 <- rnbinom(n, size = k, mu = r0i_period2)}  # r0 by individual after k parameter
  r0i_period1 <- if (is.na(k)) rep(suppressValue, n) else rnbinom(n, size = k, mu = suppressValue)
  suppressDays <- if (is.na(suppress_start)) NA else suppress_start:suppress_end

  # covid severity
  severity <- sample(seq_along(covid_severity), n, replace = T, prob = covid_severity)

  # case fatality rates
  cfri <- cfr[severity]
  
  # PROTECT referral day
  PROTECT_referral_day <- sample(seq_len(dur.PROTECT_rec) + interventions_start - 1, n, replace = T)

  # durations: individual (in the published results we used extraDistr::rdgamma)
  day.inf.i <- rpois(n, dur.exposed - 1) + 1
  day.sym.i <- rpois(n, dur.presymptom - 1) + 1 + day.inf.i
  day.adm.i <- rpois(n, dur.infSymptom - 1) + 1 + day.sym.i # day infectiousness ends and/or hospitalised
  day.end.i <- rpois(n, dur.hospital - 1) + 1 + day.adm.i # day disease ends (recovers or dies)

  # intervention ends
  ends <- min(model_duration, intervention_ends, na.rm = T)
  
  # general population incidence parameter
  gpm <- rep(1, model_duration)
  if (!is.na(gen_pop_mix_start)) {
    gpmSuppressDays <- gen_pop_mix_start:gen_pop_mix_end
    gpm[gpmSuppressDays] <- gen_pop_mix_para
  }
  inc <- bi[seq_len(model_duration)] * gpm[seq_len(model_duration)]
  
  # TEST values
  cl <- if (TEST) rep(1, n) else cl
  inc <- if (TEST) rep(0, model_duration) else inc
  
  #-------------------------------------
  # function for modelling daily changes
  #-------------------------------------
  
  f <- function (day) {
    
    # previous statuses
    #------------------
    
    sy <- dat[, day] # status yesterday
    
    # duration after COVID19 infection (varying by individual)
    #---------------------------------------------------------
    
    q.inf <- (day - day.inf.i) == day_of_infection # becomes infectious today
    q.sym <- (day - day.sym.i) == day_of_infection # becomes symptomatic today
    q.CAR <- (day - day.sym.i) == (day_of_infection + 1) # offered COVID CARE
    q.adm <- (day - day.adm.i) == day_of_infection # becomes non-infections and/or admitted today
    q.end <- (day - day.end.i) == day_of_infection # recovers or dies today

    # location of individual yesterday
    #---------------------------------
    
    location <- findInterval(sy, c(0, 7, 12, 14)) # community, PROTECT, CARE, hospital/died
        
    # force of infection by individual
    #---------------------------------
    
    # likelihood of mixing with infectious person
    iN <- tapply(sy, cl, function(x) sum(x == 3 | x == 4) / sum(x < 7)) # proportion infectious in community by cluster
    iN <- iN[cl] # i/N by individual
    iN[location == 2] <- sum(sy == 9 | sy == 10) / sum(location == 2) # covid PROTECT
    
    # cluster-specific r0's
    r0i <- if (day %in% suppressDays) r0i_period1 else r0i_period2
    r0c <- r0i
    r0c[!(sy == 3 | sy == 4)] <- NA
    r0c <- tapply(r0c, cl, mean, na.rm = T) # community r0's by cluster
    r0p <- mean(r0i[sy == 9 | sy == 10]) # covid PROTECT r0
    r0f <- r0c[cl]
    r0f[location == 2] <- r0p
    
    # force of infection
    foi <- 1 - exp(-iN * g * r0f)
    foi[is.na(foi)] <- 0 # is.na includes is.nan (changing those with zero denominator to zero)
    
    # intervention open
    #------------------
    
    started <- (day >= interventions_start) & (day <= ends)
    
    # probabilities
    #--------------
    
    p.cv1 <- rbinom(n, 1, foi) == 1 # FOI
    p.cv2 <- rbinom(n, 1, inc[day]) == 1 # background exposure
    p.dth <- rbinom(n, 1, cfri) == 1
    p.sdg <- rbinom(n, 1, self_discharge_risk) == 1 # self-discharge risk
    p.rCC <- (rbinom(n, 1, accept_CARE) == 1) & started & (severity != 1) # accept_ss COVID-CARE referral
    p.rCP <- (rbinom(n, 1, accept_ss) == 1) & started # street sleepers accept COVID-PROTECT referral
    p.rCP[type == 2] <- T
    p.rCP[type == 1] <- F
    
    # new status
    #-----------
    
    status <- sy
    
    # community
    status[(sy == 1) & (p.cv1 | p.cv2)] <- 2
    status[location == 1 & q.inf] <- 3
    status[location == 1 & q.sym] <- 4
    status[location == 1 & q.adm] <- 5
    status[location == 1 & q.CAR & p.rCC] <- 12
    status <- ifelse((sy < 4) & (PROTECT_referral_day == day) & p.rCP, sy + 6, status)
    
    # PROTECT 
    status[sy == 7 & p.cv1] <- 8
    status[location == 2 & q.inf] <- 9
    status[location == 2 & q.sym] <- 10
    status[sy == 10] <- 12 # transferred to COVID-CARE (even if asymptomatic to reflect testing)
    status[sy == 7 & p.sdg] <- 1 # self-discharge
    status[sy == 11 & p.sdg] <- 6 # self-discharge
    status[sy == 7 & !started] <- 1
    status[sy == 11 & !started] <- 6
    
    # CARE
    status[location == 3 & q.adm] <- 13
    status <- ifelse(location == 3 & p.sdg, sy - 8, status) 

    # hospital admissions
    status[q.adm & (severity == 3)] <- 14
    status[q.adm & (severity == 4)] <- 15
    
    # end of covid
    status[q.end] <- 6
    status[location == 3 & q.end & p.rCP] <- 11
    status[q.end & p.dth] <- 16
    
    return(status)
    
  }
  
  #----------------
  # run daily model
  #----------------
  
  dat <- matrix(rep(1, n)) # day 0
  day_of_infection <- rep(-Inf, n)
  
  # seed (for TEST scenario)
  if (TEST) {dat[,1] <- rep(2:1, c(10, n - 10))}
  
  for(i in seq_len(model_duration)) {
    dat <- cbind(dat, f(i))
    day_of_infection[(dat[,i+1] %in% c(2, 8)) & (day_of_infection == -Inf)] <- i + 1
  }
  
  return(dat)
  
}
