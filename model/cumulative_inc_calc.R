t <- c(seq(0,4,1)) # time unit = 2 weeks as that's roughly what our 0.08 incidence is for.

max_inc <- 0.08 # this is 2-weekly incidence
time_max_inc <- 4 #  incidence peaks on 8th week after epidemic starts
k <- 2 # exponential shape factor this is IR = exp(alpha*x^k)
alpha <- log(1 + max_inc)/(time_max_inc^k)

IR <- exp(alpha*t^k) -1 # Incidence rate
plot(t, IR)

CI <- c(0) # Cumulative incidence
# To account for shrinking susceptible pop.
for (i in 2:length(IR)){
  CI[i] <- CI[i-1] + (1-sum(IR[1:i-1]))*IR[i] # i.e. CI = proportion of total pop infected + susceptible proportion * incidence rate
}

plot(t, CI)

# appreciate that our 0.08 incidence rate in hostels is out of total population, not susceptibles. could adjust for this, but probably ok?