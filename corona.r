### Global variables ###
startDate = as.Date("2020-03-01")

### GitHub global dataset - currently unused (updated after 24 hours) ###

#corona <- read.csv(url("https://raw.githubusercontent.com/datasets/covid-19/master/data/time-series-19-covid-combined.csv"))
#corona$Date <- as.Date(corona$Date)

### EU ECDC data (updated every morning 06:00-10:00 CET) ###
ecdc <- read.csv(url("http://opendata.ecdc.europa.eu/covid19/casedistribution/csv"))
ecdc$dateRep <- as.Date(ecdc$dateRep, format = "%d/%m/%Y")

ecdc <- ecdc[order(ecdc$'countriesAndTerritories', as.Date(ecdc$dateRep)),]
ecdc$cumulDeaths <- ave(ecdc$deaths, ecdc$geoId, FUN=cumsum)

ecdc_nl <- subset(ecdc, geoId == 'NL')
ecdc_nl <- subset(ecdc_nl, dateRep >= startDate)

ecdc_uk <- subset(ecdc, geoId == 'UK')
ecdc_uk <- subset(ecdc_uk, dateRep >= startDate)

ecdc_de <- subset(ecdc, geoId == 'DE')
ecdc_de <- subset(ecdc_de, dateRep >= startDate)

ecdc_fr <- subset(ecdc, geoId == 'FR')
ecdc_fr <- subset(ecdc_fr, dateRep >= startDate)

ecdc_be <- subset(ecdc, geoId == 'BE')
ecdc_be <- subset(ecdc_be, dateRep >= startDate)


### Stichting Nice Intensive Care statistics for the Netherlands (updated continuously) ###

#install.packages('jsonlite', 'curl')
library(jsonlite)

ic_new_admissions <- fromJSON("https://www.stichting-nice.nl/covid-19/public/new-intake/")
ic_new_admissions <- subset(ic_new_admissions, select = c("date", "newIntake"))

ic_occupancy <- fromJSON("https://www.stichting-nice.nl/covid-19/public/intake-count/")
ic_occupancy <- subset(ic_occupancy, select = c("date", "intakeCount"))

ic_affected_hospitals <- fromJSON("https://www.stichting-nice.nl/covid-19/public/ic-count")
ic_affected_hospitals <- subset(ic_affected_hospitals, select = c("date", "icCount"))

ic_cumulative <- fromJSON("https://www.stichting-nice.nl/covid-19/public/ic-cumulative")
ic_cumulative <- subset(ic_cumulative, select = c("date", "intakeCumulative"))

ic_deaths <- fromJSON("https://www.stichting-nice.nl/covid-19/public/died-cumulative/")
ic_deaths <- subset(ic_deaths, select = c("date", "diedCumulative"))

ic_stats_nl <- merge(ic_new_admissions, ic_occupancy, by="date", all=TRUE)
ic_stats_nl <- merge(ic_stats_nl, ic_affected_hospitals, by="date", all=TRUE)
ic_stats_nl <- merge(ic_stats_nl, ic_cumulative, by="date", all=TRUE)
ic_stats_nl <- merge(ic_stats_nl, ic_deaths, by="date", all=TRUE)
ic_stats_nl$date <- as.Date(ic_stats_nl$date)
ic_stats_nl <- subset(ic_stats_nl, ic_stats_nl$date >= startDate)
ic_stats_nl <- subset(ic_stats_nl, ic_stats_nl$date < Sys.Date()) # Exclude current day's data

rm(ic_new_admissions, ic_occupancy, ic_affected_hospitals, ic_cumulative, ic_deaths)

### Plot the data we collected ###

# Helper function for our repetitive plot() calls
covPlot <- function(x, y, ylim, main, ylab, hor_line) {
  if (missing(ylab)) {
    ylab = ""
  }
  
  sub = paste(sep = "", "Generated: ", Sys.Date(), ", data as at: ", (Sys.Date() - 1))
  
  if (missing(ylim)) {
    plot(x = x, y = y, xlab = "", ylab = ylab, main = main, sub = sub, type = 'b')
  } else {
    plot(x = x, y = y, ylim = ylim, xlab = "", ylab = ylab, main = main, sub = sub, type = 'b')
  }
  
  grid()
  text(x = x, y = y, labels = y, pos = 3, offset = 0.5, cex = 0.55)
  
  if (!missing(hor_line)) {
    hor_line();
  }
}

# Function to plot all data
plot_all <- function () {
  # Draw a horizontal line showing the average daily deaths in the Netherlands in 2018
  nl_daily_avg_deaths <- function() {
    abline(h=153363/365, col="orange")
  }
  
  covPlot(x = ecdc_nl$dateRep,
          y = ecdc_nl$cumulDeaths,
          ylim = c(0, 1000),
          ylab = "Cumulative deaths",
          main = "COVID-19 cumulative deaths\nNetherlands",
          hor_line = nl_daily_avg_deaths)
  
  covPlot(x = ecdc_nl$dateRep,
          y = ecdc_nl$deaths,
          ylim = c(0, 150),
          ylab = "Deaths",
          main = "COVID-19 daily deaths\nNetherlands",
          hor_line = nl_daily_avg_deaths)
  
  covPlot(x = ecdc_uk$dateRep,
          y = ecdc_uk$cumulDeaths,
          ylim = c(0, 1000),
          ylab = "Cumulative deaths",
          main = "COVID-19 cumulative deaths\nUnited Kingdom")
  
  covPlot(x = ecdc_uk$dateRep,
          y = ecdc_uk$deaths,
          ylim = c(0, 250),
          ylab = "Deaths",
          main = "COVID-19 daily deaths\nUnited Kingdom")
  
  covPlot(x = ecdc_de$dateRep,
          y = ecdc_de$cumulDeaths,
          ylim = c(0, 1000),
          ylab = "Cumulative deaths",
          main = "COVID-19 cumulative deaths\nGermany")
  
  covPlot(x = ecdc_de$dateRep,
          y = ecdc_de$deaths,
          ylim = c(0, 150),
          ylab = "Deaths",
          main = "COVID-19 daily deaths\nGermany")
  
  covPlot(x = ecdc_fr$dateRep,
          y = ecdc_fr$cumulDeaths,
          ylim = c(0, 2000),
          ylab = "Cumulative deaths",
          main = "COVID-19 cumulative deaths\nFrance")
  
  covPlot(x = ecdc_fr$dateRep,
          y = ecdc_fr$deaths,
          ylim = c(0, 500),
          ylab = "Deaths",
          main = "COVID-19 daily deaths\nFrance")
  
  covPlot(x = ecdc_be$dateRep,
          y = ecdc_be$cumulDeaths,
          ylim = c(0, 1000),
          ylab = "Cumulative deaths",
          main = "COVID-19 cumulative deaths\nBelgium")
  
  covPlot(x = ecdc_be$dateRep,
          y = ecdc_be$deaths,
          ylim = c(0, 150),
          ylab = "Deaths",
          main = "COVID-19 daily deaths\nBelgium")
  
  covPlot(x = ic_stats_nl$date,
          y = ic_stats_nl$newIntake,
          ylab = "Admissions",
          main = "Daily COVID-19 intensive care admissions\nNetherlands")
  
  # Draw three horizontal lines depiciting anticipated intensive care capacity in the Netherlands
  ic_capacity_lines <- function() {
    abline(h=1150, col="green")
    abline(h=1400, col="orange")
    abline(h=1600, col="red")
  }
  
  covPlot(x = ic_stats_nl$date,
          y = ic_stats_nl$intakeCount,
          ylim = c(0, 1600),
          main = "COVID-19 patients in intensive care\nNetherlands",
          ylab = "Patients",
          hor_line = ic_capacity_lines)
}

### Generate plots on screen ###

plot_all()

#### Save plots as PDF ###

# Prepare PDF
pdf(file = paste(format(Sys.Date(), format = "%y%m%d"), "covplots.pdf", sep = "-"), paper = "a4", width = 10, height = 15)

#Set up two plots per image
par(mfrow = c(2, 1))

# Generate plots
plot_all()

# Write plots to PDF
dev.off()