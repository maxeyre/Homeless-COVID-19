library(RColorBrewer)

# ---------------------
# load 1,000 model runs
# =====================

# sA:sH = results with stochastic variation only. Each object is a list of 2 items; first item is key model results; second item is daily new cases
# mcA:mcH = key results with probabilistic variation in inputs
# mcA_cases:mcH_cases = daily new cases with probabilistic variation in inputs

load("homeless_model_results_25aug2020.RData")

sf <- function(x) {
  y <- apply(x, 2, quantile, probs = c(0.025, 0.5, 0.975))
  y <- formatC(round(y, 0), big.mark = ',', format = 'd')
  apply(y, 2, function(x) paste0(x[2], ' (', x[1], '-', x[3], ')'))
}

# ----------------------------------------
# results in main article (first 200 runs)
# ========================================

# table 3

table3 <- rbind(sf(sA[[1]][1:200,4:7]),
                sf(sB[[1]][1:200,4:7]),
                sf(sB[[1]][1:200,4:7] - sA[[1]][1:200,4:7]),
                sf(sC[[1]][1:200,4:7]),
                sf(sD[[1]][1:200,4:7]),
                sf(sD[[1]][1:200,4:7] - sC[[1]][1:200,4:7]),
                sf(sE[[1]][1:200,4:7]),
                sf(sF[[1]][1:200,4:7]),
                sf(sG[[1]][1:200,4:7]),
                sf(sH[[1]][1:200,4:7]))
write.csv(table3, 'table3_25aug2020.csv')

# table S4

tableS4 <- rbind(sf(mcA[1:200,]),
                 sf(mcB[1:200,]),
                 sf(mcB[1:200,] - mcA[1:200,]),
                 sf(mcC[1:200,]),
                 sf(mcD[1:200,]),
                 sf(mcD[1:200,] - mcC[1:200,]),
                 sf(mcE[1:200,]),
                 sf(mcF[1:200,]),
                 sf(mcG[1:200,]),
                 sf(mcH[1:200,]))
write.csv(tableS4, 'tableS4_25aug2020.csv')

#  --------------------------------
#  plot of new cases - scenario C:H
#  ================================

nd <- ncol(sC[[2]])
model_start <- as.Date('2020-02-01')

xl <- seq(model_start, model_start + 366, by = 'day')
xl <- xl[format(xl, '%d') == '01']
xl <- tail(head(xl, -1), -1)
xx <- as.numeric(xl - model_start)
xlab <- paste0('1 ', format(xl, format = '%b %Y'))

pf <- function(cases, ymax = 300, xoff = 0, yoff = 0, cols = brewer.pal(9, 'Blues')[c(3, 8)]) {
  tot <- rowSums(cases)
  med <- cases[which.min(abs(tot - median(tot))),]
  xp <- seq_along(med)
  apply(replace(cases, cases > ymax, ymax), 1, function(y) lines(xp + xoff, y + yoff, lwd = 0.5, col = cols[1]))
  lines(xp + xoff, med + yoff, col = cols[2])
}

wave1end <- as.numeric(as.Date('2020-05-31') - model_start)

# stochastic variation only
# -------------------------

pdf('fig2.pdf', height = 10, width = 15)

plot(1, type = 'n', xlim = c(0, nd * 3), ylim = c(0, 600), axes = F, xlab = NA, ylab = NA)

rect(0, 0, nd * 3, 600, col = 'grey98')

pf(sC[[2]][1:200,], yoff = 300)
pf(sD[[2]][1:200,])
pf(sE[[2]][1:200,], yoff = 300, xoff = nd)
pf(sF[[2]][1:200,], xoff = nd)
pf(sG[[2]][1:200,], yoff = 300, xoff = nd * 2)
pf(sH[[2]][1:200,], xoff = nd * 2)

rect(0, 0, nd * 3, 300)
rect(nd, 0, nd*2, 600)

axis(1, xx, xlab, las = 2, pos = 0)
axis(1, xx + nd, xlab, las = 2, pos = 0)
axis(1, xx + nd * 2, xlab, las = 2, pos = 0)

axis(2, 0:5 * 50, las = 2, pos = 0)
axis(2, 0:5 * 50 + 300, 0:5 * 50, las = 2, pos = 0)

mtext('Number of new cases per day', 2, line = 3)
text(rep(c(nd/2 + wave1end/2, nd * 1.5 + wave1end/2, nd * 2.5 + wave1end/2), each = 2), c(590, 290, 590, 290, 590, 290),
     c('Scenario C\n(No second wave;\nall measures continued)',
       'Scenario D\n(No second wave;\nall measures lifted)',
       'Scenario E\n(Sharp second wave;\nall measures continued)',
       'Scenario F\n(Sharp second wave;\nMeasures only continued\nin general population)',
       'Scenario G\n(as per scenario D,\nbut with COVID-CARE\nand COVID-PROTECT)',
       'Scenario H\n(Flat second wave;\nMeasures only continued\nin general population)'),
     adj = c(0.5, 1))

text(rep(c(wave1end/2, nd + wave1end/2, nd * 2 + wave1end/2), each = 2), c(590, 290, 590, 290, 590, 290),
     rep('Scenario A', 4),
     adj = c(0.5, 1))

segments(c(wave1end, wave1end + nd, wave1end + nd*2), 0, y1 = 600, lty = 3)

dev.off()

# with probabilistic variation in inputs
# --------------------------------------

pdf('fig2_with_probabilistic_variation.pdf', height = 10, width = 15)

plot(1, type = 'n', xlim = c(0, nd * 3), ylim = c(0, 600), axes = F, xlab = NA, ylab = NA)

rect(0, 0, nd * 3, 600, col = 'grey98')

pf(mcC_cases[1:200,], yoff = 300)
pf(mcD_cases[1:200,])
pf(mcE_cases[1:200,], yoff = 300, xoff = nd)
pf(mcF_cases[1:200,], xoff = nd)
pf(mcG_cases[1:200,], yoff = 300, xoff = nd * 2)
pf(mcH_cases[1:200,], xoff = nd * 2)

rect(0, 0, nd * 3, 300)
rect(nd, 0, nd*2, 600)

axis(1, xx, xlab, las = 2, pos = 0)
axis(1, xx + nd, xlab, las = 2, pos = 0)
axis(1, xx + nd * 2, xlab, las = 2, pos = 0)

axis(2, 0:5 * 50, las = 2, pos = 0)
axis(2, 0:5 * 50 + 300, 0:5 * 50, las = 2, pos = 0)

mtext('Number of new cases per day', 2, line = 3)
text(rep(c(nd/2 + wave1end/2, nd * 1.5 + wave1end/2, nd * 2.5 + wave1end/2), each = 2), c(590, 290, 590, 290, 590, 290),
     c('Scenario C\n(No second wave;\nall measures continued)',
       'Scenario D\n(No second wave;\nall measures lifted)',
       'Scenario E\n(Sharp second wave;\nall measures continued)',
       'Scenario F\n(Sharp second wave;\nMeasures only continued\nin general population)',
       'Scenario G\n(as per scenario D,\nbut with COVID-CARE\nand COVID-PROTECT)',
       'Scenario H\n(Flat second wave;\nMeasures only continued\nin general population)'),
     adj = c(0.5, 1))

text(rep(c(wave1end/2, nd + wave1end/2, nd * 2 + wave1end/2), each = 2), c(590, 290, 590, 290, 590, 290),
     rep('Scenario A', 4),
     adj = c(0.5, 1))

segments(c(wave1end, wave1end + nd, wave1end + nd*2), 0, y1 = 600, lty = 3)

dev.off()

# ---------------
# full 1,000 runs
# ===============

# table 3

rbind(sf(sA[[1]][,4:7]),
      sf(sB[[1]][,4:7]),
      sf(sB[[1]][,4:7] - sA[[1]][,4:7]),
      sf(sC[[1]][,4:7]),
      sf(sD[[1]][,4:7]),
      sf(sD[[1]][,4:7] - sC[[1]][,4:7]),
      sf(sE[[1]][,4:7]),
      sf(sF[[1]][,4:7]),
      sf(sG[[1]][,4:7]),
      sf(sH[[1]][,4:7]))

# table S4

rbind(sf(mcA),
      sf(mcB),
      sf(mcB - mcA),
      sf(mcC),
      sf(mcD),
      sf(mcD - mcC),
      sf(mcE),
      sf(mcF),
      sf(mcG),
      sf(mcH))

# -------------------------
# compare 200 to 1,000 runs
# =========================

cf <- function(x, ind1 = 1, ind2 = 2) {
  plot(density(x[,ind1]), axes = F, main = NA, xlab = NA, ylab = NA)
  lines(density(x[1:200,ind1]), col = 'red')
  box()
  plot(density(x[,ind2]), axes = F, main = NA, xlab = NA, ylab = NA)
  lines(density(x[1:200,ind2]), col = 'red')
  box()
}

par(mfrow = c(8, 2), mar = c(0, 0, 0, 0))
cf(mcA)
cf(mcB)
cf(mcC)
cf(mcD)
cf(mcE)
cf(mcF)
cf(mcG)
cf(mcH)
