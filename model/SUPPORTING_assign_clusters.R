# METADATA
# This script is associated with the article 'COVID-19 among people experiencing homelessness in England: a modelling study'
# [ Add publication details ]
# This script assigns individuals to specific clusters / sub-groups, which are modelled as closed groups within which transmission occurs
# The script produces the file 'gp_cases31july2020.csv' that is already included in the repository. This file is a model input# There is no need to run this script prior to running the model; it is only provided for completeness
# There is no need to run this script prior to running the model; it is only provided for completeness and to ensure reproducability of results (as the implementation of set.seed varies between different versions of R)
# The script is written in R4.0.0 but is compatible with most recent versions of R

set.seed(20)

# hostels (actual hostel numbers and sizes)
hl <- scan("https://raw.githubusercontent.com/maxeyre/Homeless-COVID-19/master/model/SUPPORTING_hl_hostel_beds.csv")
h_cl <- rep(seq_along(hl), hl) # hotel cluster ID's

# overall rough sleeping population size (street sleeping + night shelters)
rs <- 10748L

# night shelters (based on night shelters in London)
number_in_ns <- round(963L / (1136L/4266L), 0L)
nss <- scan("https://raw.githubusercontent.com/maxeyre/Homeless-COVID-19/master/model/SUPPORTING_nightshelter_sizes_london.csv")
ns_sample <- sample(nss, floor(number_in_ns / mean(nss) * 1.5), replace = T)
ns_sample <- ns_sample[cumsum(ns_sample) < number_in_ns]
ns_sample <- c(ns_sample, number_in_ns - sum(ns_sample))
ns_cl <- rep(seq_along(ns_sample), ns_sample)
ns_cl <- ns_cl + max(h_cl) # night shelter cluster ID's

# street-sleeping groups (synthetic)
street_sleeping_pop <- rs - number_in_ns
ss_mx_group <- 100L
ss_cl <- sample(seq_len(ss_mx_group), street_sleeping_pop, T, prob = 1 / seq_len(ss_mx_group))
ss_cl <- rep(seq_along(ss_cl), ss_cl)[seq_len(street_sleeping_pop)]
ss_cl <- ss_cl + max(ns_cl) # street-sleeping cluster ID's

# check steet-sleeping cluster sizes
length(unique(ss_cl))
quantile(table(ss_cl))

# total population
cl <- c(h_cl, ns_cl, ss_cl) # cluster id
type <- c(rep(1L, sum(hl)), rep(2L, number_in_ns), rep(3L, street_sleeping_pop)) # 1 = hostel, 2 = night shelters, 3 = street-sleeping
n <- length(cl) # total population size

# COVID-PROTECT hotels (people sleeping rough only)
hotels <- scan("https://raw.githubusercontent.com/maxeyre/Homeless-COVID-19/master/model/SUPPORTING_hotel_sizes.csv")
hotel_sample <- sample(hotels, floor(rs / mean(hotels) * 1.5), replace = T)
hotel_sample <- hotel_sample[cumsum(hotel_sample) < rs]
hotel_sample <- c(hotel_sample, rs - sum(hotel_sample))
hotel_cl <- rep(seq_along(hotel_sample), hotel_sample)
hotel_cl <- hotel_cl + max(cl)
hotel_cl <- c(rep(0L, length(h_cl)), hotel_cl)

# -----------------
# create RData file
# =================

save(list = c('cl', 'type', 'n', 'hotel_cl'), file = 'cluster_ids.RData')
