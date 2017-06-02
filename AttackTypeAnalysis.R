library(ggplot2)
library(dplyr)
library(reshape)
library(gridExtra)
library(lubridate)

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

# Look at Attack Type over time:
attacktypes <- as.character(unique(reduced_sf$sub_attacktype))
# ---- Country specific deaths over YEARS:
countries <- as.character(unique(reduced_sf$sub_countries))
years <- unique(reduced_sf$sub_years)
count <- 1
country_vec <- vector(mode="character")
year_vec <- vector(mode="numeric")
death_vec <- vector(mode="numeric")
attack_vec <- vector(mode="numeric")
attacktype_vec <- vector(mode="character")
for(attacktype in attacktypes){
  reduced_sf_set <- subset(reduced_sf, sub_attacktype == attacktype)
  for(year in years){
    #country_vec[[count]] <- country
    year_vec[[count]] <- year
    death_set <- subset(reduced_sf_set, sub_years == year)
    death_vec[[count]] <- round(sum(death_set$sub_deaths))
    attack_vec[[count]] <- nrow(death_set)
    attacktype_vec[[count]] <- attacktype
    count <- count+1
  }
}
attacktype_frame <- data.frame(
  Year=year_vec,
  AttackType=attacktype_vec,
  Deaths=death_vec,
  Attacks=attack_vec
)
deaths_plot <- ggplot(attacktype_frame, aes(x=Year, y=Deaths, colour=AttackType)) +
  geom_line() +
  theme_bw() +
  scale_colour_discrete(name="Attack Type")
attacks_plot <- ggplot(attacktype_frame, aes(x=Year, y=Attacks, colour=AttackType)) +
  geom_line() + 
  theme_bw() +
  scale_colour_discrete(name="Attack Type")
grid.arrange(deaths_plot, attacks_plot, ncol=1)

# ---- Looking only at Western Countries: ----
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
countries <- as.character(unique(reduced_sf$sub_countries))
years <- unique(reduced_sf$sub_years)
count <- 1
country_vec <- vector(mode="character")
year_vec <- vector(mode="numeric")
death_vec <- vector(mode="numeric")
attack_vec <- vector(mode="numeric")
attacktype_vec <- vector(mode="character")
r_sf <- reduced_sf[grep(paste(western_countries, collapse='|'), reduced_sf$sub_countries, ignore.case=TRUE),]
for(attacktype in attacktypes){
  reduced_sf_set <- subset(r_sf, sub_attacktype == attacktype)
  for(year in years){
    #country_vec[[count]] <- country
    year_vec[[count]] <- year
    death_set <- subset(reduced_sf_set, sub_years == year)
    death_vec[[count]] <- round(sum(death_set$sub_deaths))
    attack_vec[[count]] <- nrow(death_set)
    attacktype_vec[[count]] <- attacktype
    count <- count+1
  }
}
western_attacktype_frame <- data.frame(
  Year=year_vec,
  AttackType=attacktype_vec,
  Deaths=death_vec,
  Attacks=attack_vec
)
deaths_plot <- ggplot(western_attacktype_frame, aes(x=Year, y=Deaths, colour=AttackType)) +
  geom_line() +
  labs(title="Western Terror Attacks by Attack Type") +
  theme_bw() +
  scale_y_continuous(limits=c(0,500)) +
  scale_colour_discrete(name="Attack Type")
attacks_plot <- ggplot(western_attacktype_frame, aes(x=Year, y=Attacks, colour=AttackType)) +
  geom_line() + 
  theme_bw() +
  scale_colour_discrete(name="Attack Type")
grid.arrange(deaths_plot, attacks_plot, ncol=1)
