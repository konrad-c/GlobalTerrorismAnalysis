library(ggplot2)
library(dplyr)
library(reshape)
library(gridExtra)
library(lubridate)

#setwd("/UNI/Y2S1/2083/Assignment3/globalTerrorismAnalysis")

# --Task---
#  

# read the data
sf <- read.csv2("Data/gtd/globalterrorism.csv", sep=",")
# Clean bad data
sf <- sf[sf$imonth != "0", ]

# graoh the number of deads in the last 45 years i neach of the countries
sub_countries <- sf$country_txt
sub_years <- sf$iyear
sub_months <- sf$imonth
sub_deaths <- sf$nkill
sub_attacktype <- sf$attacktype1_txt
# Casting of variables
sub_countries <- as.character(sub_countries)
sub_deaths <- as.numeric(as.character(unlist(sub_deaths)))
sub_years <- as.numeric(as.character(unlist(sub_years)))
sub_months <- as.character(unlist(sub_months))
sub_attacktype <- as.character(unlist(sub_attacktype))
sub_months[as.numeric(sub_months) < 10] <- paste("0", sub_months[as.numeric(sub_months) < 10], sep="")

# Create Date columns:
sub_date <- paste("01", sub_months, paste0(sub_years," 00:00:00"), sep="/")
sub_date <- as.Date(as.character(as.POSIXct(strptime(sub_date, format="%d/%m/%Y %H:%M:%S"))))

# Create reduced data.frame
reduced_sf <- data.frame(sub_years, sub_date, sub_countries, sub_deaths, sub_attacktype)
reduced_sf <- na.omit(reduced_sf)

# Create data from reduced set for unique list of countries

countries <- as.character(unique(sf$country_txt))
deaths <- vector(length(countries), mode="numeric")
for(country in countries){
  death_set <- subset(reduced_sf, sub_countries == country)
  deaths[[country]] <- round(sum(death_set$sub_deaths))
}
deaths_country <- data.frame(countries, deaths) 
deaths_country <- deaths_country[order(deaths_country$deaths, decreasing = T), ]
n <- 6
plot_data <- deaths_country[1:n, ]
plot_data$countries <- factor(plot_data$countries, levels = as.character(plot_data$countries))

ggplot(plot_data, aes(x=countries, weight=deaths)) +
  labs(x="Country", y="Deaths", title="Top 25 countries with most deaths resulting from terrorist attacks between 1970-2015") +
  geom_bar() + 
  theme_bw() +
  geom_text(aes(y=deaths,label=deaths), position=position_dodge(width=0.9), vjust=0.25, hjust=-0.15)+
  coord_flip() +
  scale_y_continuous(limits = c(0,63000), expand = c(0.01,0))


# ---- Country specific deaths over YEARS:
countries <- as.character(unique(sf$country_txt))
years <- unique(sf$iyear)
count <- 1
country_vec <- vector(mode="character")
year_vec <- vector(mode="numeric")
death_vec <- vector(mode="numeric")
attack_vec <- vector(mode="numeric")
for(country in countries){
  reduced_sf_set <- reduced_sf[reduced_sf$sub_countries == country, ]
  sf_set <- sf[sf$country_txt == country, ]
  for(year in years){
    country_vec[[count]] <- country
    year_vec[[count]] <- year
    death_set <- subset(reduced_sf_set, sub_years == year)
    death_vec[[count]] <- round(sum(death_set$sub_deaths))
    attack_vec[[count]] <- length(which(sf_set$iyear == year))
    count <- count+1
  }
}
deaths_country_time <- data.frame(Country=country_vec, Year=year_vec, Deaths=death_vec, Attacks=attack_vec)
# Look at only top N countries:
deaths_time <- deaths_country_time[grep(paste(plot_data$countries, collapse='|'), deaths_country_time$Country, ignore.case=TRUE),]
deaths_plot <- ggplot(data=deaths_time, aes(x=Year, y=Deaths, fill=Country, colour=Country)) +
  geom_line() +
  scale_y_continuous(limits=c(0,15000))
attacks_plot <- ggplot(data=deaths_time, aes(x=Year, y=Attacks, fill=Country, colour=Country)) +
  geom_line() +
  scale_y_continuous(limits=c(0,15000))
grid.arrange(deaths_plot, attacks_plot, ncol=2)

# Look at western countries
western_countries <- c(
  "Canada",
  "United States",
  "Australia",
  "Russia",
  "Finland",
  "France",
  "United Kingdom",
  "Germany",
  "Netherlands",
  "Japan",
  "New Zealand",
  "Sweden",
  "Switzerland"
)
top_western_countries <- deaths_country[grep(paste(western_countries, collapse='|'), deaths_country$countries, ignore.case=TRUE),]
top_western_countries <- top_western_countries[order(top_western_countries$deaths),]
top_western_countries <- as.character(top_western_countries$countries)
deaths_time <- deaths_country_time[grep(paste(top_western_countries, collapse='|'), deaths_country_time$Country, ignore.case=TRUE),]
deaths_western_plot <- ggplot(deaths_time, aes(x=Year, y=Deaths, colour=Country)) +
  geom_line() 
attacks_western_plot <- ggplot(deaths_time, aes(x=Year, y=Attacks, colour=Country)) +
  geom_line()
grid.arrange(deaths_western_plot, attacks_western_plot, ncol=1)
# ---- Plot All Deaths and Attacks in all countries:
deaths_vec <- vector(mode="numeric")
attacks_vec <- vector(mode="numeric")
year_vec <- vector(mode="numeric")
count <- 1
for(year in unique(deaths_time$Year)){
  year_vec[[count]] <- year
  deaths_vec[[count]] <- sum(deaths_time[deaths_time$Year == year, ]$Deaths)
  attacks_vec[[count]] <- sum(deaths_time[deaths_time$Year == year, ]$Attacks)
  count <- count + 1
}
deaths_time_total <- data.frame(
  Year=year_vec,
  Deaths=deaths_vec,
  Attacks=attacks_vec
)
deaths_western_total_plot <- ggplot(deaths_time_total, aes(x=Year, y=Deaths, colour="red")) +
  geom_line() +
  scale_y_continuous(limits=c(0,700))
attacks_western_total_plot <- ggplot(deaths_time_total, aes(x=Year, y=Attacks, colour="blue")) +
  geom_line() +
  scale_y_continuous(limits=c(0,700))
grid.arrange(deaths_western_total_plot, attacks_western_total_plot, ncol=1)



# ---- Country specific deaths over MONTHS:
countries <- unique(reduced_sf$sub_countries)
dates <- unique(reduced_sf$sub_date)
count <- 1
country_vec <- vector(mode="character")
dates_vec <- vector(mode="numeric")
year_vec <- vector(mode="numeric")
month_vec <- vector(mode="numeric")
death_vec <- vector(mode="numeric")
attack_vec <- vector(mode="numeric")
for(country in countries){
  reduced_sf_set <- reduced_sf[reduced_sf$sub_countries == country, ]
  for(i in 1:length(dates)){
    date <- dates[[i]]
    country_vec[[count]] <- country
    dates_vec[[count]] <- date
    death_set <- subset(reduced_sf_set, sub_date == date)
    death_vec[[count]] <- round(sum(death_set$sub_deaths))
    attack_vec[[count]] <- nrow(death_set)
    year_vec[count] <- year(date)
    month_vec[count] <- month(date)
    count <- count+1
  }
}
deaths_country_time <- data.frame(
  Country=country_vec,
  Date=dates_vec,
  Year=year_vec,
  Month=month_vec,
  Deaths=death_vec,
  Attacks=attack_vec
)

top_n_countries <- plot_data$countries
graph_vec <- list()
for(country in top_n_countries){
  deaths_time <- deaths_country_time[deaths_country_time$Country == country, ]
  graph_vec[[country]] <- ggplot(data=deaths_time, aes(x=Month, y=Deaths, colour=factor(Year))) +
    geom_line() +
    labs(x="Date", y="Deaths", title=country) +
    theme(legend.title = element_blank())
}
grid.arrange(grobs = graph_vec, ncol=ceiling(sqrt(n)))

# ---- Looking at attack type over time:

