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

infe <- inf[,-c(1:4)] #infection
dead <- dea[,-c(1:4)] #death
reco <- rec[,-c(1:4)] #fatality
i.country <- aggregate(infe, by=list(inf$Country.Region), FUN=sum)
r.country <- aggregate(reco, by=list(rec$Country.Region), FUN=sum)
d.country <- aggregate(dead, by=list(dea$Country.Region), FUN=sum)
i <- i.country[66,-1] # Germany
r <- r.country[66,-1]
d <- d.country[66,-1]

# which(d.country$Group.1=="Germany")

iN <- as.numeric(i.country[66,-1]) #adding N for "as.numeric" to make my life easier later
rN <- as.numeric(r.country[66,-1])
dN <- as.numeric(d.country[66,-1])
inds <- seq(as.Date("2020-01-22"), (Sys.Date()-1), by = "day")

# When first infection reported?
st <- which(iN>0)[1]
inds <- inds[-(1:st)]
i <- iN[-(1:st)]
r <- rN[-(1:st)]
d <- dN[-(1:st)]

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

## PLOTTING INFECTIONS, FITTING MODELS

quartz()
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

## PLOTTING FORECASTS 

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

startD <- which(date=="X3.1.20") #to find col number of March 1, 2020
inf0103 <- iN[startD:length(iN)] #infection rate per day since March 1, 2020
rec0103 <- rN[startD:length(iN)] #recovery rate
dea0103 <- dN[startD:length(iN)] #death rate
inds0103 <- seq(as.Date("2020-03-01"), (Sys.Date()-1), by = "day")

# First differences and gam smoothing
di0103 <- diff(inf0103)
dr0103 <- diff(rec0103)
dd0103 <- diff(dea0103)

d.inds0103 <- inds0103[-1]

## PLOTTING INFECTIONS, FITTING MODELS

quartz(w=7,h=5)
plot(d.inds0103, di0103, col="red", type="l", xlab="Date", ylab = "Number of cases", 
     main = "Daily Rate of Increase Since March 1, 2020", ylim=c(0,10000))
lines(d.inds0103, dr0103, col="green")
lines(d.inds0103, dd0103, col="brown")

y <- c(1:length(d.inds0103))
fit.gi0103 <- gam(di0103~s(y))
fit.gr0103 <- gam(dr0103~s(y))
fit.gd0103 <- gam(dd0103~s(y))
lines(d.inds0103, predict(fit.gi0103), col="red")
lines(d.inds0103, predict(fit.gr0103), col="green")
lines(d.inds0103, predict(fit.gd0103), col="brown")
legend("topleft",legend = c("Infections", "Recoveries","Fatalities"), 
       col= c("red", "green", "brown"), lty = 1)

## PLOTTING FORECASTS SINCE MARCH 1, 2020

# forecast for infection rates (classical gam extrapolation)

quartz(w = 8, h = 5)
op <- par(mfrow=c(1,2))

fore <- 100
d.pre0103 <- predict(fit.gi0103, data.frame(y=seq(1,fore)), link="log", type="response")
inds0103 <- seq(as.Date(d.inds0103[1]), as.Date(d.inds0103[1])+fore-1, by=1)

plot(inds0103, d.pre0103, type="l", xlab = "Date", ylab = "Number of cases", 
     main = "GAM Infection: start 01.03.2020")

# forecast for infection rates (Arima)
fit0103 <- auto.arima(di0103)
fo0103 <- forecast(fit0103,h=20)
plot(fo0103) # Why cannot I add axes labels?? 


