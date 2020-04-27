mc <- run_model()

# PLOTS
#------

library(RColorBrewer)

nc <- ncol(mc)
r <- nrow(mc)
day0 <- as.Date('2020-01-29', origin = '1970-01-01')
xax <- seq(0, nc, 7)
weeks <- seq(day0, day0 + nc, by = 'week')
xlab <- format(weeks, "%d %b")
xlab <- paste0('Wk', seq_along(xax) -1, ': ', xlab)

# number at each status by day
#-----------------------------

dsf <- function(fm) {
  ds <- t(sapply(1:22, function(x) colSums(fm == x)))
  ds2 <- rbind(`Community: susceptible` = colSums(ds[1:2,]),
               `Community: recovered` = ds[7,],
               `COVID-PROTECT` = colSums(ds[8:13,]),
               `Community: exposed` = ds[3,],
               `Community: COVID-19` = colSums(ds[4:6,]),
               `COVID-CARE` = colSums(ds[14:19,]),
               `Admitted to hospital` = ds[20,],
               `Admitted to ITU` = ds[21,],
               Died = ds[22,])
  list(ds, ds2, rbind(0, apply(ds2, 2, cumsum)))
}
ds <- dsf(mc)

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
plot(1, type = 'n', xlim = c(0, nc), ylim = c(0, nrow(mc)), axes = F, xlab = NA, ylab = NA)
for(i in 1:nrow(ds[[2]])) {
  polygon(c(0:(nc - 1), (nc - 1):0), c(ds[[3]][i+1,], rev(ds[[3]][i,])), col = cols[i])
}
axis(1, xax, xlab, las = 2, pos = 0)
segments(0, 0, nc)
axis(2, yxf(r, type = 'previous'), pos = 0, las = 2)
ys <- seq(r * 0.25, r * 0.75, length.out = length(cols) + 1)
rect(nc * 1.07, ys[-length(ys)], nc * 1.14, ys[-1], col = cols)
text(nc * 1.19, ys[-length(ys)] + diff(ys) / 2, rownames(ds[[2]]), adj = 0)
title(ylab = 'Population', line = 4)