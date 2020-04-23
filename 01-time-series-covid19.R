## Ming Khan 
# April 23, 2020 
# Analytical Paleobiology HW on Covid-19

library(mgcv) # for gam models
library(caret) # for workflow in machine learning
library('forecast') # for Auto-ARIMA



inf <- read.csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv",
                header = T) # reported infected
rec <- read.csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_recovered_global.csv",
                header = T) # reported recovered
dea <- read.csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv", 
                header=T) # reported death

# Data for Germany (All dates)

infe <- inf[,-c(1:4)]
dead <- dea[,-c(1:4)]
reco <- rec[,-c(1:4)]
i.country <- aggregate(infe, by=list(inf$Country.Region), FUN=sum)
r.country <- aggregate(reco, by=list(rec$Country.Region), FUN=sum)
d.country <- aggregate(dead, by=list(dea$Country.Region), FUN=sum)
i <- i.country[66,-1] # Germany
r <- r.country[66,-1]
d <- d.country[66,-1]

# which(d.country$Group.1=="Germany")

i <- as.numeric(i.country[66,-1])
r <- as.numeric(r.country[66,-1])
d <- as.numeric(d.country[66,-1])
inds <- seq(as.Date("2020-01-22"), (Sys.Date()-1), by = "day")

# When first infection reported?
st <- which(i>0)[1]
inds <- inds[-(1:st)]
i <- i[-(1:st)]
r <- r[-(1:st)]
d <- d[-(1:st)]

quartz(w = 8, h = 5)
op <- par(mfrow=c(1,2))
plot(inds, i, col="red", type="l", xlab = "Date", ylab = "Number of cases", 
     main="Covid-19 in Germany")
lines(inds, r, col="green")
lines(inds, d, col="brown")
legend("topleft",legend = c("Infections", "Recoveries","Fatalities"), 
       col= c("red", "green", "brown"), lty = 1)

# First differences and gam smoothing
di <- diff(i)
dr <- diff(r)
dd <- diff(d)

d.inds <- inds[-1]

plot(d.inds, di, col="red", type="l", xlab="Date", ylab = "Number of cases", 
     main = "Daily Rate of Increase")
lines(d.inds, dr, col="green")
lines(d.inds, dd, col="brown")

x <- c(1:length(d.inds))
fit.gi <- gam(di~s(x))
fit.gr <- gam(dr~s(x))
fit.gd <- gam(dd~s(x))
lines(d.inds, predict(fit.gi), col="red")
lines(d.inds, predict(fit.gr), col="green")
lines(d.inds, predict(fit.gd), col="brown")
legend("topleft",legend = c("Infections", "Recoveries","Fatalities"), 
       col= c("red", "green", "brown"), lty = 1)


# Spring forecast  

# forecast for infection rates (classical gam extrapolation)

quartz(w = 8, h = 5)
op <- par(mfrow=c(1,2))
fore <- 100
d.pre <- predict(fit.gi, data.frame(x=seq(1,fore)), link="log", type="response")
inds <- seq(as.Date(d.inds[1]), as.Date(d.inds[1])+fore-1, by=1)

plot(inds, d.pre, type="l", xlab = "Date", ylab = "Number of cases", 
     main = "Predicted (GAM) Infection Rates")

# forecast for infection rates (Arima)
fit <- auto.arima(di)
fo <- forecast(fit,h=20)
plot(fo) # Why cannot I add axes labels?? 

## FORECASTING CHANGES BY DATE SELECTION 

# Case 1: Start date from March 1, 2020 

inds0103 <- seq(as.Date("2020-01-22"), (Sys.Date()-1), by = "day")

