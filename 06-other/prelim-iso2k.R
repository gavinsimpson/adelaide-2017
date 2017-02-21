## iso2K prelim using a small subset of data

## First, work on a function to do analysis os a single site,
## where uncertainty in the age model is incorporated into the
## GAM for a trend

## Data
chrons <- readRDS("../data/chrons.rds")
cores  <- readRDS("../data/cores.rds")
## Something seems messed up as `year` is character
cores <- transform(cores, year = as.numeric(year))
## and this introduces NAs so we need to explore this more

## Packages
library("mgcv")
library("ggplot2")
theme_set(theme_bw())

## Work with site == LESA
lesa <- subset(cores, site == "LESA")
lesaChron <- subset(chrons, site == "LESA")
## seems to have calendar year in data and years BP in age models...
lesa <- transform(lesa, ageBP = -(year - 1950), ageBP2 = year - 1950)

## plot
ggplot(lesa, aes(x = ageBP, y = d18O)) +
    geom_point() +
    scale_x_reverse() +
    labs(y = expression(delta^{18}*O ~ "[‰ VSMOW]"))

yearLims <- range(lesaChron[, 1:1500])
obsLims <- with(lesa, range(ageBP))

## Set up knots
k <- 40                                 # basis dimension
m <- c(3,1)                             # cubic b-splines, penalty on first order deriv
xk = list(ageBP2 = -yearLims)

mod <- gam(d18O ~ s(ageBP2, bs = "bs", m = m, k = k),
           data = lesa, method = "REML", knots = xk)

plot(mod, residuals = TRUE, pch = 16, n = 500)
summary(mod)
gam.check(mod)

fitGam <- function(time, y, knots, k, bs = "bs", m = c(3,1), newt) {
    df <- data.frame(time = -time, y = y)
    mod <- gam(y ~ s(time, bs = bs, m = m), data = df, method = "REML",
               knots = list(time = knots))
    newdf <- data.frame(time = newt)
    predict(mod, newdata = newdf)
}

## Set up knots
k <- 40                                 # basis dimension
m <- c(3,1)                             # cubic b-splines, penalty on first order deriv
xk = list(ageBP2 = -yearLims)
N <- 500
newt <- seq(xk[[1]][1], xk[[1]][2], length = N)
f <- fitGam(lesaChron[,1], lesa$d18O, knots = xk, k = k, newt = newt)
plot(newt, f, type = "l")

fits <- sapply(lesaChron[, -NCOL(lesaChron)],
               fitGam, y = lesa$d18O, k = k, m = m, knots = xk, newt = newt)

summ <- t(apply(fits, 1L, quantile, probs = c(0.025, 0.5, 0.975)))

matplot(newt, fits, type = "l", col = "#00000022")
matlines(newt, summ, lwd = c(3,5,3), col = "red", lty = 1)

####
m1 <- gam(d18O ~ site + s(year, by = site, k = 30), data = cores,
          method = "REML", subset = year > 1000)
op <- par(mar = c(4,4,1,1) + 0.1)
plot(m1, pages = 1, scheme = 1, scale = 0)
par(op)

m2 <- gam(d18O ~ site + s(year, k = 40) + s(year, by = site, k = 10, m = 1), data = cores,
          method = "REML", subset = year > 1000, select = TRUE)
op <- par(mar = c(4,4,1,1) + 0.1)
plot(m2, pages = 1, scheme = 1)
par(op)

m3 <- gam(d18O ~ s(year, site, k = 30, bs = "fs"), data = cores,
          method = "REML", subset = year > 1000)
op <- par(mar = c(4,4,1,1) + 0.1)
plot(m3, pages = 1, scheme = 1, scale = 0)
par(op)

m4 <- gam(d18O ~ s(year, k = 50) + s(year, site, k = 10, m = 1, bs = "fs"), data = cores,
          method = "REML", subset = year > 1000, select = TRUE)
op <- par(mar = c(4,4,1,1) + 0.1)
plot(m4, pages = 1, scheme = 1)
par(op)

library("analogue")
foo <- function(df) {
    res <- tran(~ d18O, data = df, method = "range")
    transform(df, norm = res[, 1])
}
spl <- split(cores, f = cores$site)     # split
spl <- lapply(spl, foo)                 # apply
dat <- do.call("rbind", spl)            # combine

yearlim <- with(subset(cores, year > 1000), range(year))
newdf <- with(cores, expand.grid(year = seq(yearlim[1], yearlim[2], length = 250),
                                 site = levels(site)))
newdf <- transform(newdf, fit = predict(m1, newdf))
stacked <- do.call("rbind", lapply(split(newdf, newdf$site), `[[`, "fit"))

clust <- hclust(dist(stacked))
plot(clust)
plot(m3)
