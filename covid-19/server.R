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


region.pop <- read.csv("https://raw.githubusercontent.com/maxeyre/COVID-19/master/homeless_pop.csv")
region.pop$region <- as.character(region.pop$region)
add <- c("Custom", 10000, 10000)
region.pop <- rbind(region.pop,add)

list.regions <- as.list(region.pop$region)
names(list.regions) <- region.pop$region

is.odd <- function(x) x %% 2 != 0

main.function <- function(total_days,hostel_population,rough_sleeping_population,proportion_vulnerable,
                          all_protect,testing,max_protect,time_to_results,self_discharge_day,admission_day,
                          died_covid_day,duration_covid,duration_CARE,duration_admission,duration_PROTECT_recruitment,
                          probability_identified,accept_CARE,accept_PROTECT,self_discharge_risk,ili_incidence,ae_prob,
                          covid_severity,cfr_community,rr_vulnerable,cfr,rr_CARE,peak_day,outbreak_duration,covid_attack_hostel,
                          covid_attack_rough_sleepers,PROTECT_incidence_fraction,B){
  
  #-----------------------
  # calculate model inputs 
  #-----------------------
  
  # set peak day to <= outbreak duration
  peak_day <- min(peak_day, outbreak_duration)
  
  # change time to results if no testing
  time_to_results <- if (testing) time_to_results else duration_CARE
  
  # population size, vulnerability and eligibility for PROTECT
  n <- hostel_population + rough_sleeping_population # total number of people
  type <- c(rep(1, hostel_population), rep(2, rough_sleeping_population))
  vulnerable <- rbinom(n, 1, proportion_vulnerable)
  cpe <- vulnerable # covid PROTECT eligible
  cpe[all_protect] <- 1
  
  # case fatality rates
  cfr_community <- rbind(cfr, cfr * rr_vulnerable)
  cfr_community <- pmin(cfr_community, 1)
  cfr_CARE <- cfr_community
  cfr_CARE[,2:3] <- rr_CARE * cfr_CARE[,2:3]
  
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
  severity <- sample(1:(length(covid_severity)), n, replace = T, prob = covid_severity)
  
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
    
    # protect exceeded capacity yesterday (if so, then doesn't accept anyone today)
    protect_full <- if (is.na(max_protect)) F else sum(sy %in% 5:7) >= max_protect
    
    # probabilities
    #--------------------
    p.died.community <- rbinom(n, 1, mortality_community) == 1
    p.died.CARE <- rbinom(n, 1, mortality_CARE) == 1
    p.self.discharge <- rbinom(n, 1, self_discharge_risk) == 1
    p.ref.CARE <- (rbinom(n, 1, accept_CARE) == 1) & (severity != 1)
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
    status[(sy == 2) & p.ref.CARE & (day <= outbreak_duration)] <- 8
    status[(sy == 6) & (day <= outbreak_duration)] <- 8
    status[(sy == 8)] <- 9
    status[(sy == 9)] <- 9
    status[(sy == 9) & (q.test == 8)] <- 1
    status[(sy == 9) & (q.test == 8) & (cpe == 1) & p.ref.PROTECT] <- 5
    status[(sy == 9) & p.self.discharge & (q.sd == 8)] <- 1
    
    # hospital admissions
    status[(sy == 4) & (severity %in% 3:4) & (q.admission %in% c(3, 7))] <- 13
    status[(sy == 11) & (severity == 4) & (q.admission %in% c(3, 7))] <- 13
    status[sy == 13] <- 14
    status[(sy == 14) & q.admission.start == 13] <- 12
    status[(sy == 4) & (severity == 5) & (q.admission %in% c(3, 7))] <- 15
    status[(sy == 11) & (severity == 5) & (q.admission %in% c(3, 7))] <- 15
    status[sy == 15] <- 16
    status[(sy == 16) & q.admission.start == 15] <- 12
    
    # deaths
    day_of_death <- q.cov.died %in% c(3, 7)
    status[(sy == 4) & p.died.community & day_of_death] <- 17
    status[(sy %in% c(10, 11)) & p.died.CARE & day_of_death] <- 17
    status[(sy %in% (13:16)) & p.died.CARE & day_of_death] <- 17
    
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
    updateNumericInput(session, "hostel_population", value = val_rough,
                      min = 0, max = 50000)
    updateNumericInput(session, "rough_sleeping_population", value = val_hostel,
                       min = 0, max = 50000)
  })
  
  # Add a link to a website
  url <- a("UCL", href="https://www.ucl.ac.uk/") # define the link and text
  output$url <- renderUI({
    tagList(url) # now outputted as output$url
  })

  
  # Code for all reactive calculations
  plots <- reactive({
    
    if(is.odd(input$action)==FALSE){
      
      #### STANDARD GRAPHS
      
      #-----------------------
      # Inputs
      #-----------------------
      
      # epidemic parameters
      peak_day <- input$peak_day
      outbreak_duration <- input$outbreak_duration
      total_days <- outbreak_duration + 20
      covid_attack_hostel <- input$covid_attack_hostel # anything less than 1
      covid_attack_rough_sleepers <- input$covid_attack_rough_sleepers # anything less than 1
      PROTECT_incidence_fraction <- 1 - input$PROTECT_incidence_fraction # incidence of covid & ILI in PROTECT is x * hostel rate
      # case fatality and hospitalisation rates
      covid_severity <- c(0.2, 0.52, 0.125, 0.125, 0.03) # asymptomatic / mild / moderate / severe / critical. should sum to 1
      cfr <- c(0, 0.0001, 0.005, 0.025, 0.1) # for non-vulnerable, by severity
      rr_vulnerable <- input$rr_vulnerable #9 # risk ratio for vulnerable people
      rr_CARE <- input$rr_CARE # risk ratio for mild and moderate cases in covid CARE
      B <- 1.75 # LEAVING THIS AS FIXED; parameter for 'shape' of curve (not much value in changing)
      
      # risks and rates
      probability_identified <- input$probability_identified # proportion of population identified
      accept_CARE <- input$accept_CARE # proportion accepting CARE
      accept_PROTECT <- input$accept_CARE  # NOTE THAT THIS IS NOW JOINT FROM ACCEPT_CARE; proportion accepting PROTECT
      self_discharge_risk <- input$self_discharge_risk
      ili_incidence <- 6.3/700 # LEAVING THIS AS FIXED; https://bmcinfectdis.biomedcentral.com/articles/10.1186/1471-2334-14-232
      
      # population
      hostel_population <- input$hostel_population
      rough_sleeping_population <- input$rough_sleeping_population
      n <- hostel_population + rough_sleeping_population
      type <- c(rep(1, hostel_population), rep(2, rough_sleeping_population))
      proportion_vulnerable <- input$proportion_vulnerable
      
      # intervention parameters
      all_protect <- input$all_protect # F if T, everyone is offered protect regardless of vulnerability
      ifelse(all_protect=="TRUE", all_protect <- TRUE, all_protect <- FALSE)
      
      testing <- input$testing #T # testing (if false, 'time_to_results' defaults to duration of CARE, and after CARE vulnerable population is offered PROTECT rather than discharged to community)
      ifelse(testing=="TRUE", testing <- TRUE, testing <- FALSE)
      
      ifelse(input$max_protect_binary=="FALSE", max_protect <- NA, max_protect <- input$max_protect_value)
      #NA # UNSURE HOW TO INCLUDE THIS # NA for no maximum; # PROTECT capacity
      
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
      
      # main code
      out <- main.function(total_days,hostel_population,rough_sleeping_population,proportion_vulnerable,
                           all_protect,testing,max_protect,time_to_results,self_discharge_day,admission_day,
                           died_covid_day,duration_covid,duration_CARE,duration_admission,duration_PROTECT_recruitment,
                           probability_identified,accept_CARE,accept_PROTECT,self_discharge_risk,ili_incidence,ae_prob,
                           covid_severity,cfr_community,rr_vulnerable,cfr,rr_CARE,peak_day,outbreak_duration,covid_attack_hostel,
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
      
      ds <- t(sapply(1:17, function(x) colSums(dat == x))) # daily summary
      ds2 <- rbind(`Community: susceptible` = colSums(ds[1:2,]),
                   PROTECT = colSums(ds[5:7,]),
                   `Community: recovered` = ds[12,],
                   `Community: Covid-19` = colSums(ds[3:4,]),
                   CARE = colSums(ds[8:11,]),
                   `Admitted to hospital` = colSums(ds[13:14,]),
                   ITU = colSums(ds[15:16,]),
                   Died = ds[17,])
      ds3 <- apply(ds2, 2, cumsum)
      ds3 <- rbind(0, ds3)    
      any_hospital <- colSums(ds[13:16,])
      
      # plots
      #------
      
      # function for making y-axes
      yaxt <- c(outer(c(1, 2.5, 5), 10^(0:6), '*')) 
      yaxt <- yaxt[yaxt != 2.5]
      yxf <- function(ymax, tk = 5, type = 'next') { 
        tm <- yaxt[which.min(abs(yaxt - ymax / tk))]
        nticks <- if (type == 'next') ceiling(ymax / tm) else floor(ymax / tm)
        seq(0, tm * nticks, tm)
      }                  
      
      # Tab 1 - epidemic curve  
      new_cases_total <- colSums((dat == 3) | (dat == 7))
      new_cases_rough_sleepers <- colSums(((dat == 3) | (dat == 7)) & type == 2)
      cum_inc <- cumsum(new_cases_total) / n
      ymax <- ceiling(max(new_cases_total)/50) * 50
      par(xpd = NA, mar = c(4, 5, 1, 15))
      plot(1, type = 'n', xlim = c(0, total_days + 1), ylim = c(0, ymax), axes = F, xlab = NA, ylab = 'New cases')
      rect(0, 0, total_days + 1, ymax)
      title(xlab = 'Week', line = 2.5)
      rect(0:total_days, 0, 1:(total_days + 1), new_cases_total, border = NA, col = 'grey80')
      rect(0:total_days, 0, 1:(total_days + 1), new_cases_rough_sleepers, border = NA, col = 'grey60')
      lines(0:total_days + 0.5, cum_inc * ymax, col = 'white', lwd = 5)
      lines(0:total_days + 0.5, cum_inc * ymax, col = 'red')
      axis(2, yxf(ymax, type = 'previous'), pos = 0, las = 2)
      axis(4, 0:5/5 * ymax, paste0(0:5/5 * 100, '%'), las = 2, pos = total_days + 1, col = 'red', col.axis = 'red')
      axis(1, seq(0, floor(total_days/14)*14, 14), 0:floor(total_days/14) * 2, pos = 0)
      segments(0, 0, total_days + 1)
      text(total_days * 1.35, ymax/2, 'Cumulative incidence', srt = 270, col = 'red')
      text(total_days - total_days / 20, max(cum_inc + 0.05) * ymax, paste0(round(max(cum_inc) * 100, 0), '%'), col = 'red')
      xs <- c(1.45, 1.9)
      ys <- c(0.9, 0.95, 1)
      rect(total_days * xs[1], ymax*ys[1:2], total_days * xs[2], ymax*ys[2:3], col = c('grey60', 'grey80'), border = NA)
      text(total_days * mean(xs), ymax*(ys[1:2] + diff(ys)/2), c('Rough sleepers', 'Total'), cex = 0.8, col = c('white', 'black'))
      
      p1 <- recordPlot()
      
      # Tab 2 - stacked plot
      cols <- brewer.pal(nrow(ds2), 'Paired')
      par(mar = c(4, 5, 1, 15), xpd = NA)
      plot(1, type = 'n', xlim = c(0, total_days), ylim = c(0, n), axes = F, xlab = NA, ylab = NA)
      for(i in 1:nrow(ds2)) {
        polygon(c(0:total_days, total_days:0), c(ds3[i+1,], rev(ds3[i,])), col = cols[i])
      }
      axis(1, seq(0, floor(total_days/14)*14, 14), 0:floor(total_days/14) * 2, pos = 0)
      segments(0, 0, total_days + 1)
      axis(2, yxf(n, type = 'previous'), pos = 0, las = 2)
      ys <- seq(n * 0.25, n * 0.75, length.out = length(cols) + 1)
      rect(total_days * 1.07, ys[-length(ys)], total_days * 1.14, ys[-1], col = cols)
      text(total_days * 1.19, ys[-length(ys)] + diff(ys) / 2, rownames(ds2), adj = 0)
      title(xlab = 'Week', line = 2.5)
      title(ylab = 'Population', line = 4)
      
      p2 <- recordPlot()
      
      # Tab 3 - healthcare use    
      cols <- brewer.pal(4, 'Set1')
      fpl <- function(d, yrange = 0.15) {
        ymax <- max(d) * 1.1
        plot(1, type = 'n', xlim = c(0, total_days), ylim = c(0, ymax), xlab = NA, ylab = NA, axes = F)
        for (i in 1:nrow(d)) lines(0:total_days, d[i,], col = cols[i], lwd = 2)
        axis(1, seq(0, floor(total_days/14)*14, 14), 0:floor(total_days/14) * 2, pos = 0)
        segments(0, 0, total_days + 1)
        axis(2, yxf(ymax, type = 'previous'), pos = 0, las = 2)
        rect(0, 0, total_days, ymax)
        ys <- seq(ymax * (0.5-yrange), ymax * (0.5+yrange), length.out = nrow(d))
        segments(total_days * 1.1, ys, total_days * 1.2, ys, col = cols, lwd = 2)
        text(total_days * 1.25, ys, row.names(d), adj = 0)
        title(xlab = 'Week', line = 2.5)
        title(ylab = 'Number', line = 3.5)
      }
      
      par(mar = c(4, 5, 1, 15), xpd = NA)
      fpl(rbind(ds2[6:7,], `A&E visits` = colSums(ae_visits), `Ambulance journeys` = colSums(ambulance_trip)))
      
      p3 <- recordPlot()
      
      # Tab 4 - CARE & PROTECT use     
      par(mar = c(4, 5, 1, 15), xpd = NA)
      fpl(ds2[c(5, 2),], yrange = 0.05)
      
      p4 <- recordPlot()
      
      # Tab 5 - deaths
      deaths <- (cbind(0, dat[,-ncol(dat)]) != 17) & (dat == 17)
      deaths <- colSums(deaths)
      ymax <- max(deaths)
      par(xpd = NA, mar = c(4, 5, 1, 1))
      plot(1, type = 'n', xlim = c(0, total_days + 1), ylim = c(0, ymax), axes = F, xlab = NA, ylab = 'Deaths')
      rect(0:total_days, 0, 1:(total_days+1), deaths, col = "#8DA0CB", border = NA)
      axis(1, seq(0, floor(total_days/14)*14, 14), 0:floor(total_days/14) * 2, pos = 0)
      segments(0, 0, total_days + 1)
      rect(0, 0, total_days+1, ymax)
      axis(2, yxf(ymax, type = 'previous'), pos = 0, las = 2)
      title(xlab = 'Week', line = 2.5)
      text(total_days * 0.05, ymax * 0.95, paste0('Total = ', sum(deaths)), adj = 0)
      
      p5 <- recordPlot()
      
      #----------------
      # Outputs estimates for text
      #----------------
      
      estimates_out <- data.frame(Parameter=c("Total COVID-19 cases","Total COVID-19 deaths","Actual cum. attack rate","CFR","Peak no. in CARE","Peak no. in PROTECT",
                                              "Total hospital bed days","Total hospital admissions","Peak no. bed days",
                                              "Day of bed day peak","Total ITU days","Total ITU admissions","Peak no. ITU beds","Day of ITU peak",
                                              "Total A&E visits","Total ambulance trips"), Estimate = 0)
      
      estimates_out$Estimate[1] <- as.character(sum(ds[c(3, 4, 7, 10:17), total_days + 1])) # covid cases
      estimates_out$Estimate[2] <- ds[17, total_days + 1] # died
      estimates_out$Estimate[3] <- round(sum(ds[c(3, 4, 7, 10:17), total_days + 1]) / n, digits=3) # actual cumulative attack rate
      estimates_out$Estimate[4] <- round(ds[17, total_days + 1] / sum(ds[c(3, 4, 7, 10:17), total_days + 1]), digits=3)  # CFR
      estimates_out$Estimate[5] <- max(colSums((dat == 8) | (dat == 9) | (dat == 10) | (dat == 11))) # peak CARE
      estimates_out$Estimate[6] <- max(colSums((dat == 5) | (dat == 6) | (dat == 7))) # peak PROTECT
      
      
      
      estimates_out$Estimate[7] <- sum(dat %in% c(13, 14)) # hospital bed days
      estimates_out$Estimate[8] <- sum(dat == 13) # hospital admissions
      estimates_out$Estimate[9] <- max(colSums((dat == 13) | (dat == 14))) # peak beds
      estimates_out$Estimate[10] <- which.max(colSums((dat == 13) | (dat == 14))) # bed days peak on day...
      
      estimates_out$Estimate[11] <- sum(dat %in% c(15, 16)) # ITU days
      estimates_out$Estimate[12] <- sum(dat == 15) # ITU admissions
      estimates_out$Estimate[13] <- max(colSums((dat == 15) | (dat == 16))) # peak ITU beds
      estimates_out$Estimate[14]<- which.max(colSums((dat == 15) | (dat == 16))) # ITU beds peak on day...
      
      estimates_out$Estimate[15] <- sum(ae_visits) # A&E visits
      estimates_out$Estimate[16] <- sum(ambulance_trip) # ambulance trips
      
      p.list <- list(p1,p2,p3,p4,p5,estimates_out)
      
    }else{
      
      #### ADDING 'NO INTERVENTION' SCENARIO
      
      #-----------------------
      # Chosen inputs
      #-----------------------
      
      # epidemic parameters
      peak_day <- input$peak_day
      outbreak_duration <- input$outbreak_duration
      total_days <- outbreak_duration + 20
      covid_attack_hostel <- input$covid_attack_hostel # anything less than 1
      covid_attack_rough_sleepers <- input$covid_attack_rough_sleepers # anything less than 1
      PROTECT_incidence_fraction <- 1 - input$PROTECT_incidence_fraction # incidence of covid & ILI in PROTECT is x * hostel rate
      # case fatality and hospitalisation rates
      covid_severity <- c(0.2, 0.52, 0.125, 0.125, 0.03) # asymptomatic / mild / moderate / severe / critical. should sum to 1
      cfr <- c(0, 0.0001, 0.005, 0.025, 0.1) # for non-vulnerable, by severity
      rr_vulnerable <- input$rr_vulnerable #9 # risk ratio for vulnerable people
      rr_CARE <- input$rr_CARE # risk ratio for mild and moderate cases in covid CARE
      B <- 1.75 # LEAVING THIS AS FIXED; parameter for 'shape' of curve (not much value in changing)
      
      # risks and rates
      probability_identified <- input$probability_identified # proportion of population identified
      accept_CARE <- input$accept_CARE # proportion accepting CARE
      accept_PROTECT <- input$accept_CARE  # NOTE THAT THIS IS NOW JOINT FROM ACCEPT_CARE; proportion accepting PROTECT
      self_discharge_risk <- input$self_discharge_risk
      ili_incidence <- 6.3/700 # LEAVING THIS AS FIXED; https://bmcinfectdis.biomedcentral.com/articles/10.1186/1471-2334-14-232
      
      # population
      hostel_population <- input$hostel_population
      rough_sleeping_population <- input$rough_sleeping_population
      n <- hostel_population + rough_sleeping_population
      type <- c(rep(1, hostel_population), rep(2, rough_sleeping_population))
      proportion_vulnerable <- input$proportion_vulnerable
      
      # intervention parameters
      all_protect <- input$all_protect # F if T, everyone is offered protect regardless of vulnerability
      ifelse(all_protect=="TRUE", all_protect <- TRUE, all_protect <- FALSE)
      
      testing <- input$testing #T # testing (if false, 'time_to_results' defaults to duration of CARE, and after CARE vulnerable population is offered PROTECT rather than discharged to community)
      ifelse(testing=="TRUE", testing <- TRUE, testing <- FALSE)
      
      ifelse(input$max_protect_binary=="FALSE", max_protect <- NA, max_protect <- input$max_protect_value)
      #NA # UNSURE HOW TO INCLUDE THIS # NA for no maximum; # PROTECT capacity
      
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
      
      # main code
      out <- main.function(total_days,hostel_population,rough_sleeping_population,proportion_vulnerable,
                           all_protect,testing,max_protect,time_to_results,self_discharge_day,admission_day,
                           died_covid_day,duration_covid,duration_CARE,duration_admission,duration_PROTECT_recruitment,
                           probability_identified,accept_CARE,accept_PROTECT,self_discharge_risk,ili_incidence,ae_prob,
                           covid_severity,cfr_community,rr_vulnerable,cfr,rr_CARE,peak_day,outbreak_duration,covid_attack_hostel,
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
      
      ds <- t(sapply(1:17, function(x) colSums(dat == x))) # daily summary
      ds2 <- rbind(`Community: susceptible` = colSums(ds[1:2,]),
                   PROTECT = colSums(ds[5:7,]),
                   `Community: recovered` = ds[12,],
                   `Community: Covid-19` = colSums(ds[3:4,]),
                   CARE = colSums(ds[8:11,]),
                   `Admitted to hospital` = colSums(ds[13:14,]),
                   ITU = colSums(ds[15:16,]),
                   Died = ds[17,])
      ds3 <- apply(ds2, 2, cumsum)
      ds3 <- rbind(0, ds3)    
      any_hospital <- colSums(ds[13:16,])
      
      ##### ------------------------------------------------------ ######
      ### ADD IN FOR 'NO INTERVENTION'
      ##### ------------------------------------------------------ ######
      
      # main code
      out_no_interv <- main.function(total_days,hostel_population,rough_sleeping_population,proportion_vulnerable,
                           all_protect,testing,max_protect==0,time_to_results,self_discharge_day,admission_day,
                           died_covid_day,duration_covid,duration_CARE,duration_admission,duration_PROTECT_recruitment,
                           probability_identified==0,accept_CARE==0,accept_PROTECT==0,self_discharge_risk,ili_incidence,ae_prob,
                           covid_severity,cfr_community,rr_vulnerable,cfr,rr_CARE==1,peak_day,outbreak_duration,covid_attack_hostel,
                           covid_attack_rough_sleepers,PROTECT_incidence_fraction==1,B)
      
      dat_no_interv <- out_no_interv[[1]]
      covid_incidence_no_interv<- out_no_interv[[2]]
      
      #--------------------------------
      # derive A&E and ambulance visits
      #--------------------------------
      
      covid_in_community_no_interv <- (dat_no_interv == 3) | (dat_no_interv == 4)
      ae_visits_no_interv <- covid_in_community_no_interv * matrix(rbinom(n * (total_days + 1), 1, ae_prob), nrow = n)
      admitted_via_ae_no_interv <- (dat_no_interv == 13) | (dat_no_interv == 15)
      ae_visits <- ae_visits_no_interv + admitted_via_ae_no_interv
      ambulance_trip_no_interv <- (dat_no_interv == 13) | (dat_no_interv == 15)
      
      # number at each status by day
      #-----------------------------
      
      ds_no_interv <- t(sapply(1:17, function(x) colSums(dat_no_interv == x))) # daily summary
      ds2_no_interv <- rbind(`Community: susceptible` = colSums(ds_no_interv[1:2,]),
                   PROTECT = colSums(ds_no_interv[5:7,]),
                   `Community: recovered` = ds_no_interv[12,],
                   `Community: Covid-19` = colSums(ds_no_interv[3:4,]),
                   CARE = colSums(ds_no_interv[8:11,]),
                   `Admitted to hospital` = colSums(ds_no_interv[13:14,]),
                   ITU = colSums(ds_no_interv[15:16,]),
                   Died = ds_no_interv[17,])
      ds3_no_interv <- apply(ds2_no_interv, 2, cumsum)
      ds3_no_interv <- rbind(0, ds3_no_interv)    
      any_hospital_no_interv <- colSums(ds_no_interv[13:16,])
      
      ##### ------------------------------------------------------ ######
      ### END OF 'NO INTERVENTION' CALCULATIONS
      ##### ------------------------------------------------------ ######
      
      # plots
      #------
      
      # function for making y-axes
      yaxt <- c(outer(c(1, 2.5, 5), 10^(0:6), '*')) 
      yaxt <- yaxt[yaxt != 2.5]
      yxf <- function(ymax, tk = 5, type = 'next') { 
        tm <- yaxt[which.min(abs(yaxt - ymax / tk))]
        nticks <- if (type == 'next') ceiling(ymax / tm) else floor(ymax / tm)
        seq(0, tm * nticks, tm)
      }
      
      # Tab 1 - epidemic curve  
      new_cases_total <- colSums((dat == 3) | (dat == 7))
      new_cases_rough_sleepers <- colSums(((dat == 3) | (dat == 7)) & type == 2)
      cum_inc <- cumsum(new_cases_total) / n
      ymax <- ceiling(max(new_cases_total)/50) * 50
      par(xpd = NA, mar = c(4, 5, 1, 15))
      plot(1, type = 'n', xlim = c(0, total_days + 1), ylim = c(0, ymax), axes = F, xlab = NA, ylab = 'New cases')
      rect(0, 0, total_days + 1, ymax)
      title(xlab = 'Week', line = 2.5)
      rect(0:total_days, 0, 1:(total_days + 1), new_cases_total, border = NA, col = 'grey80')
      rect(0:total_days, 0, 1:(total_days + 1), new_cases_rough_sleepers, border = NA, col = 'grey60')
      lines(0:total_days + 0.5, cum_inc * ymax, col = 'white', lwd = 5)
      lines(0:total_days + 0.5, cum_inc * ymax, col = 'red')
      axis(2, yxf(ymax, type = 'previous'), pos = 0, las = 2)
      axis(4, 0:5/5 * ymax, paste0(0:5/5 * 100, '%'), las = 2, pos = total_days + 1, col = 'red', col.axis = 'red')
      axis(1, seq(0, floor(total_days/14)*14, 14), 0:floor(total_days/14) * 2, pos = 0)
      segments(0, 0, total_days + 1)
      text(total_days * 1.35, ymax/2, 'Cumulative incidence', srt = 270, col = 'red')
      text(total_days - total_days / 20, max(cum_inc + 0.05) * ymax, paste0(round(max(cum_inc) * 100, 0), '%'), col = 'red')
      xs <- c(1.45, 1.9)
      ys <- c(0.9, 0.95, 1)
      rect(total_days * xs[1], ymax*ys[1:2], total_days * xs[2], ymax*ys[2:3], col = c('grey60', 'grey80'), border = NA)
      text(total_days * mean(xs), ymax*(ys[1:2] + diff(ys)/2), c('Rough sleepers', 'Total'), cex = 0.8, col = c('white', 'black'))
      
      p1 <- recordPlot()
      
      # Tab 2 - stacked plot
      cols <- brewer.pal(nrow(ds2), 'Paired')
      par(mar = c(4, 5, 1, 15), xpd = NA)
      plot(1, type = 'n', xlim = c(0, total_days), ylim = c(0, n), axes = F, xlab = NA, ylab = NA)
      for(i in 1:nrow(ds2)) {
        polygon(c(0:total_days, total_days:0), c(ds3[i+1,], rev(ds3[i,])), col = cols[i])
      }
      axis(1, seq(0, floor(total_days/14)*14, 14), 0:floor(total_days/14) * 2, pos = 0)
      segments(0, 0, total_days + 1)
      axis(2, yxf(n, type = 'previous'), pos = 0, las = 2)
      ys <- seq(n * 0.25, n * 0.75, length.out = length(cols) + 1)
      rect(total_days * 1.07, ys[-length(ys)], total_days * 1.14, ys[-1], col = cols)
      text(total_days * 1.19, ys[-length(ys)] + diff(ys) / 2, rownames(ds2), adj = 0)
      title(xlab = 'Week', line = 2.5)
      title(ylab = 'Population', line = 4)
      
      p2 <- recordPlot()
      
      # Tab 3 - healthcare use    
      cols <- brewer.pal(4, 'Set1')
      fpl <- function(d, yrange = 0.15) {
        ymax <- max(d) * 1.1
        plot(1, type = 'n', xlim = c(0, total_days), ylim = c(0, ymax), xlab = NA, ylab = NA, axes = F)
        for (i in 1:nrow(d)) lines(0:total_days, d[i,], col = cols[i], lwd = 2)
        axis(1, seq(0, floor(total_days/14)*14, 14), 0:floor(total_days/14) * 2, pos = 0)
        segments(0, 0, total_days + 1)
        axis(2, yxf(ymax, type = 'previous'), pos = 0, las = 2)
        rect(0, 0, total_days, ymax)
        ys <- seq(ymax * (0.5-yrange), ymax * (0.5+yrange), length.out = nrow(d))
        segments(total_days * 1.1, ys, total_days * 1.2, ys, col = cols, lwd = 2)
        text(total_days * 1.25, ys, row.names(d), adj = 0)
        title(xlab = 'Week', line = 2.5)
        title(ylab = 'Number', line = 3.5)
      }
      
      par(mar = c(4, 5, 1, 15), xpd = NA)
      fpl(rbind(ds2[6:7,], `A&E visits` = colSums(ae_visits), `Ambulance journeys` = colSums(ambulance_trip)))
      
      p3 <- recordPlot()
      
      # Tab 4 - CARE & PROTECT use     
      par(mar = c(4, 5, 1, 15), xpd = NA)
      fpl(ds2[c(5, 2),], yrange = 0.05)
      
      p4 <- recordPlot()
      
      # Tab 5 - deaths
      deaths <- (cbind(0, dat[,-ncol(dat)]) != 17) & (dat == 17)
      deaths <- colSums(deaths)
      ymax <- max(deaths)
      par(xpd = NA, mar = c(4, 5, 1, 1))
      plot(1, type = 'n', xlim = c(0, total_days + 1), ylim = c(0, ymax), axes = F, xlab = NA, ylab = 'Deaths')
      rect(0:total_days, 0, 1:(total_days+1), deaths, col = "#8DA0CB", border = NA)
      axis(1, seq(0, floor(total_days/14)*14, 14), 0:floor(total_days/14) * 2, pos = 0)
      segments(0, 0, total_days + 1)
      rect(0, 0, total_days+1, ymax)
      axis(2, yxf(ymax, type = 'previous'), pos = 0, las = 2)
      title(xlab = 'Week', line = 2.5)
      text(total_days * 0.05, ymax * 0.95, paste0('Total = ', sum(deaths)), adj = 0)
      
      p5 <- recordPlot()
      
      #----------------
      # Outputs estimates for text (with added comparison without intervention)
      #----------------
      
      estimates_out <- data.frame(Parameter=c("Total COVID-19 cases","Total COVID-19 deaths","Actual cum. attack rate","CFR","Peak no. in CARE","Peak no. in PROTECT",
                                              "Total hospital bed days","Total hospital admissions","Peak no. bed days",
                                              "Day of bed day peak","Total ITU days","Total ITU admissions","Peak no. ITU beds","Day of ITU peak",
                                              "Total A&E visits","Total ambulance trips"),`With intervention`=0,`No intervention`=0)
      
      # Without intervention
      ## Epidemic overview
      estimates_out$`With intervention`[1] <- sum(ds[c(3, 4, 7, 10:17), total_days + 1]) # covid cases
      estimates_out$`With intervention`[2] <- ds[17, total_days + 1] # died
      estimates_out$`With intervention`[3] <- sum(ds[c(3, 4, 7, 10:17), total_days + 1]) / n # actual cumulative attack rate
      estimates_out$`With intervention`[4] <- ds[17, total_days + 1] / sum(ds[c(3, 4, 7, 10:17), total_days + 1])  # CFR
      estimates_out$`With intervention`[5] <- max(colSums((dat == 8) | (dat == 9) | (dat == 10) | (dat == 11))) # peak CARE
      estimates_out$`With intervention`[6] <- max(colSums((dat == 5) | (dat == 6) | (dat == 7))) # peak PROTECT
      ## Healthcare use
      estimates_out$`With intervention`[7] <- sum(dat %in% c(13, 14)) # hospital bed days
      estimates_out$`With intervention`[8] <- sum(dat == 13) # hospital admissions
      estimates_out$`With intervention`[9] <- max(colSums((dat == 13) | (dat == 14))) # peak beds
      estimates_out$`With intervention`[10] <- which.max(colSums((dat == 13) | (dat == 14))) # bed days peak on day...
      estimates_out$`With intervention`[11] <- sum(dat %in% c(15, 16)) # ITU days
      estimates_out$`With intervention`[12] <- sum(dat == 15) # ITU admissions
      estimates_out$`With intervention`[13] <- max(colSums((dat == 15) | (dat == 16))) # peak ITU beds
      estimates_out$`With intervention`[14]<- which.max(colSums((dat == 15) | (dat == 16))) # ITU beds peak on day...
      estimates_out$`With intervention`[15] <- sum(ae_visits) # A&E visits
      estimates_out$`With intervention`[16] <- sum(ambulance_trip) # ambulance trips
      
      # No intervention
      ## Epidemic overview
      estimates_out$`No intervention`[1] <- sum(ds_no_interv[c(3, 4, 7, 10:17), total_days + 1]) # covid cases
      estimates_out$`No intervention`[2] <- ds_no_interv[17, total_days + 1] # died
      estimates_out$`No intervention`[3] <- sum(ds_no_interv[c(3, 4, 7, 10:17), total_days + 1]) / n # actual cumulative attack rate
      estimates_out$`No intervention`[4] <- ds_no_interv[17, total_days + 1] / sum(ds_no_interv[c(3, 4, 7, 10:17), total_days + 1])  # CFR
      estimates_out$`No intervention`[5] <- max(colSums((dat_no_interv == 8) | (dat_no_interv == 9) | (dat_no_interv == 10) | (dat_no_interv == 11))) # peak CARE
      estimates_out$`No intervention`[6] <- max(colSums((dat_no_interv == 5) | (dat_no_interv == 6) | (dat_no_interv == 7))) # peak PROTECT
      ## Healthcare use
      estimates_out$`No intervention`[7] <- sum(dat_no_interv %in% c(13, 14)) # hospital bed days
      estimates_out$`No intervention`[8] <- sum(dat_no_interv == 13) # hospital admissions
      estimates_out$`No intervention`[9] <- max(colSums((dat_no_interv == 13) | (dat_no_interv == 14))) # peak beds
      estimates_out$`No intervention`[10] <- which.max(colSums((dat_no_interv == 13) | (dat_no_interv == 14))) # bed days peak on day...
      estimates_out$`No intervention`[11] <- sum(dat_no_interv %in% c(15, 16)) # ITU days
      estimates_out$`No intervention`[12] <- sum(dat_no_interv == 15) # ITU admissions
      estimates_out$`No intervention`[13] <- max(colSums((dat_no_interv == 15) | (dat_no_interv == 16))) # peak ITU beds
      estimates_out$`No intervention`[14]<- which.max(colSums((dat_no_interv == 15) | (dat_no_interv == 16))) # ITU beds peak on day...
      estimates_out$`No intervention`[15] <- sum(ae_visits_no_interv) # A&E visits
      estimates_out$`No intervention`[16] <- sum(ambulance_trip_no_interv) # ambulance trips
      
      # % reduction
      estimates_out$`% reduction` <- 0
      estimates_out$`% reduction`[1:2] <- 100*(estimates_out$`No intervention`[1:2]-estimates_out$`With intervention`[1:2])/estimates_out$`No intervention`[1:2]
      estimates_out$`% reduction`[3:6] <-c(rep("-",4))
      estimates_out$`% reduction`[7:16] <- 100*(estimates_out$`No intervention`[7:16]-estimates_out$`With intervention`[7:16])/estimates_out$`No intervention`[7:16]
      
      # Round figures to 0 and 2 decimal places depending
      estimates_out$`With intervention`[1:2] <- round(estimates_out$`With intervention`[1:2],0)
      estimates_out$`With intervention`[3:4] <- round(estimates_out$`With intervention`[3:4],3)
      estimates_out$`With intervention`[5:16] <- round(estimates_out$`With intervention`[5:16],0)
      
      estimates_out$`No intervention`[1:2] <- round(estimates_out$`No intervention`[1:2],0)
      estimates_out$`No intervention`[3:4] <- round(estimates_out$`No intervention`[3:4],3)
      estimates_out$`No intervention`[5:16] <- round(estimates_out$`No intervention`[5:16],0)
      
      estimates_out$`% reduction`[1:2] <- round(estimates_out$`% reduction`[1:2],1)
      estimates_out$`% reduction`[7:16] <- round(estimates_out$`% reduction`[7:16],1)
      
      p.list <- list(p1,p2,p3,p4,p5,estimates_out)
      
    }
    
               
    return(p.list)
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
