#  -----------------
#  general functions
#  =================

pe <- function (fm, start_day = 1) { # extract key results from model
  fm <- fm[,start_day:ncol(fm)]
  c(
    n = nrow(fm),
    peak_PROTECT = max(colSums((fm > 6) & (fm < 12))),
    peak_CARE = max(colSums((fm == 12) | (fm == 13))),
    cases = sum(rowSums(fm == 2 | fm == 8) > 0),
    deaths = sum(fm[, ncol(fm)] == 16) - sum(fm[,1] == 16),
    admissions = sum(rowSums(fm == 14) > 0),
    itu = sum(rowSums(fm == 15) > 0)
  )
}

run_model2 <- function(...) pe(run_model(...)) # run model and return key results

sf <- function(m) { # summarise matrix of key results from different runs
  r <- apply(m, 1, quantile, probs = c(0.025, 0.5, 0.975))
  r <- formatC(r, digit = 0, format = 'd', big.mark = ',')
  apply(r, 2, function(x) paste0(x[2], ' (', x[1], '-', x[3], ')'))
}

# 1 = 29 January
# 32 = 1 March
# 123 = 31 May
# 168 = 15 July
# 337 = 31 December

#  ----------
#  scenario A
#  ==========

t0 <- proc.time()
sA <- sapply(1:200, function(x)
  run_model2(model_duration = 123,
             gen_pop_mix_para = 0.5,
             gen_pop_mix_start = 1,
             gen_pop_mix_end = 124,
             suppressValue = 0.75,
             suppress_start = 1,
             suppress_end = 124,
             seed = x))
t1 <- proc.time()
t1 - t0

sf(sA)

#  ----------
#  scenario B
#  ==========

t0 <- proc.time()
sB <- sapply(1:200, function(x)
  run_model2(model_duration = 123,
             interventions_start = 124,
             seed = x))
t1 <- proc.time()
t1 - t0

sf(sB)

#  ------------------------------------
#  functions / parameters for scenarios
#  ====================================

new_cases <- function(fm) { # count number of new cases per day
  nd <- ncol(fm)
  x <- fm == 2 | fm == 8
  case_day <- max.col(x, ties.method = 'first')
  anyCase <- rowSums(x) == 0
  case_day[anyCase] <- NA
  sapply(seq_len(nd), function(x) sum(case_day == x, na.rm = T))
}

# background incidence with no second wave

inc2 <- background_incidence
inc2[150:length(inc2)] <- inc2[length(inc2)] 

#  ----------
#  scenario C
#  ==========

sC <- NULL
sC_124 <-NULL
sC_cases <- NULL
t0 <- proc.time()
for(i in 1:200) {
  m <- run_model(model_duration = 337,
                 gen_pop_mix_start = 1,
                 gen_pop_mix_end = 168,
                 gen_pop_mix_para = 0.5,
                 suppress_start = 1,
                 suppress_end = 168,
                 suppressValue = 0.75,
                 intervention_ends = 168,
                 bi = inc2)
  sC <- rbind(sC, pe(m))
  sC_124 <- rbind(sC_124, pe(m, start_day = 124))
  sC_cases <- rbind(sC_cases, new_cases(m))
  print(i)
}
t1 <- proc.time()
t1 - t0

sf(t(sC))
sf(t(sC_124))

#  ----------
#  scenario D
#  ==========

sD <- NULL
sD_124 <-NULL
sD_cases <- NULL
t0 <- proc.time()
for(i in 1:200) {
  m <- run_model(model_duration = 337,
                 gen_pop_mix_start = 1,
                 gen_pop_mix_end = 337,
                 gen_pop_mix_para = 0.5,
                 suppress_start = 1,
                 suppress_end = 337,
                 suppressValue = 0.75,
                 intervention_ends = 337,
                 bi = background_incidence)
  sD <- rbind(sD, pe(m))
  sD_124 <- rbind(sD_124, pe(m, start_day = 124))
  sD_cases <- rbind(sD_cases, new_cases(m))
  print(i)
}
t1 <- proc.time()
t1 - t0

sf(t(sD))

#  ----------
#  scenario E
#  ==========

sE <- NULL
sE_124 <- NULL
sE_cases <- NULL
t0 <- proc.time()
for(i in 1:200) {
  m <- run_model(model_duration = 337,
                 gen_pop_mix_start = 1,
                 gen_pop_mix_end = 337,
                 gen_pop_mix_para = 0.5,
                 suppress_start = 1,
                 suppress_end = 168,
                 suppressValue = 0.75,
                 intervention_ends = 168,
                 bi = background_incidence)
  sE <- rbind(sE, pe(m))
  sE_124 <- rbind(sE_124, pe(m, start_day = 124))
  sE_cases <- rbind(sE_cases, new_cases(m))
  print(i)
}
t1 <- proc.time()
t1 - t0

sf(t(sE))

#  ----------
#  scenario F
#  ==========

sF <- NULL
sF_124 <- NULL
sF_cases <- NULL
t0 <- proc.time()
for(i in 1:200) {
  m <- run_model(model_duration = 337,
                 gen_pop_mix_start = 1,
                 gen_pop_mix_end = 337,
                 gen_pop_mix_para = 0.5,
                 suppress_start = 1,
                 suppress_end = 337,
                 suppressValue = 0.75,
                 intervention_ends = 337,
                 bi = inc2)
  sF <- rbind(sF, pe(m))
  sF_124 <- rbind(sF_124, pe(m, start_day = 124))
  sF_cases <- rbind(sF_cases, new_cases(m))
  print(i)
}
t1 <- proc.time()
t1 - t0

sf(t(sF))

#  -----------------
#  table for article
#  =================

table3 <- rbind(sf(sA), sf(sB), sf(t(sF_124)), sf(t(sC_124)), sf(t(sD_124)), sf(t(sE_124)))
write.csv(table3, 'table3_22june2020.csv')

str_ends <- sapply(gregexpr(" ", table3), function(x) x[1])
fig <- mapply(substr, x = table3, start = 0, stop = str_ends)
fig <- gsub('[[:punct:] ]+','', fig)
fig <- as.numeric(fig)
fig <- matrix(fig, ncol = ncol(table3), dimnames = list(rownames(table3), colnames(table3)))
fig[2,] - fig[1,]

#  --------------
#  difference A/B
#  ==============

difBA <- sB - sA
sf(difBA)

#  -----------------------------
#  plot for cases - scenario C:F
#  =============================

nd <- ncol(sF_cases)

library(RColorBrewer)
cols <- brewer.pal(9, 'Blues')[c(3, 8)]

day0 <- as.Date('2020-01-29', origin = '1970-01-01')
xpts <- as.Date(paste0('2020', '-', 2:12, '-', 1))
xax <- as.numeric(xpts - day0)
xlab <- format(xpts, "%d %b")

pf <- function(cases, ymax = 300, xoff = 0, yoff = 0) {
  tot <- rowSums(cases)
  med <- cases[which.min(abs(tot - median(tot))),]
  xp <- seq_along(med)
  apply(replace(cases, cases > ymax, ymax), 1, function(y) lines(xp + xoff, y + yoff, lwd = 0.5, col = cols[1]))
  lines(xp + xoff, med + yoff, col = cols[2])
}

wave1end <- as.numeric(as.Date('2020-05-31') - day0)

#png('new_cases_scenarios_C_F_19june2020.png', height = 10, width = 10, units = 'in', res = 300)

plot(1, type = 'n', xlim = c(0, nd * 2), ylim = c(0, 600), axes = F, xlab = NA, ylab = NA)

rect(0, 0, nd * 2, 600, col = 'grey99')

pf(sF_cases, yoff = 300)
pf(sC_cases)
pf(sD_cases, xoff = nd, yoff = 300)
pf(sE_cases, xoff = nd)

rect(0, 0, nd * 2, 600)
rect(0, 0, nd * 2, 300)
rect(0, 0, nd, 600)

axis(1, xax, xlab, las = 2, pos = 0)
axis(1, xax + nd, xlab, las = 2, pos = 0)

axis(2, 0:5 * 50, las = 2, pos = 0)
axis(2, 0:5 * 50 + 300, 0:5 * 50, las = 2, pos = 0)

mtext('Number of new cases per day', 2, line = 3)
text(rep(c(nd/2 + wave1end/2, nd + nd/2 + wave1end/2), each = 2), c(590, 290, 590, 290),
     c('Scenario C\n(No second wave;\nall measures continued)',
       'Scenario D\n(No second wave;\nall measures lifted)',
       'Scenario E\n(Second wave;\nall measures continued)',
       'Scenario F\n(Second wave;\nMeasures only continued\nin general population)'),
     adj = c(0.5, 1))

text(rep(c(wave1end/2, nd + wave1end/2), each = 2), c(590, 290, 590, 290),
     rep('Scenario A', 4),
     adj = c(0.5, 1))

segments(c(wave1end, wave1end + nd), 0, y1 = 600, lty = 3)

#dev.off()
