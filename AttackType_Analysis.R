library(ggplot2)
library(dplyr)
library(reshape)
library(gridExtra)
library(lubridate)
library(egg)
setwd("~/Uni/FIT2083/GlobalTerrorismAnalysis")

#extract legend
#https://github.com/hadley/ggplot2/wiki/Share-a-legend-between-two-ggplot2-graphs
g_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)}

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
  labs(title="Global Terror Attacks by Attack Type") +
  scale_colour_discrete(name="Attack Type")
attacks_plot <- ggplot(attacktype_frame, aes(x=Year, y=Attacks, colour=AttackType)) +
  geom_line() + 
  theme_bw() +
  scale_colour_discrete(name="Attack Type")
global_legend <- g_legend(deaths_plot)
grid.arrange(arrangeGrob(deaths_plot + theme(legend.position="none"),
                         attacks_plot + theme(legend.position="none"),
                         nrow=2),
             global_legend, ncol=2, widths = c(3,1))

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
#r_sf <- reduced_sf[grep(paste(western_countries, collapse='|'), reduced_sf$sub_countries, ignore.case=TRUE),]
# ---- United States only ----
r_sf <- reduced_sf[reduced_sf$sub_countries == "United States", ]
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
  labs(title="United States Terror Attack Deaths by Attack Type") +
  theme_bw() +
  scale_colour_discrete(name="Attack Type")
deaths_zoom_plot <- ggplot(western_attacktype_frame, aes(x=Year, y=Deaths, colour=AttackType)) +
  geom_line() +
  labs(title="Reduced Y-axis range") +
  theme_bw() +
  scale_y_continuous(limits=c(0,175))
deaths_zoom_further_plot <- ggplot(western_attacktype_frame, aes(x=Year, y=Deaths, colour=AttackType)) +
  geom_line() +
  labs(title="Further reduced Y-axis range") +
  theme_bw() +
  scale_y_continuous(limits=c(0,40))
death_legend <- g_legend(deaths_plot)
grid.arrange(arrangeGrob(deaths_plot + theme(legend.position="none"),
                         deaths_zoom_plot + theme(legend.position="none"),
                         deaths_zoom_further_plot + theme(legend.position = "none"),
                         nrow=3),
             death_legend, ncol=2, widths = c(3,1))


attacks_plot <- ggplot(western_attacktype_frame, aes(x=Year, y=Attacks, colour=AttackType)) +
  geom_line() + 
  theme_bw() +
  labs(title="United States Terror Attacks by Attack Type") +
  scale_colour_discrete(name="Attack Type")
attacks_zoom_plot <- ggplot(western_attacktype_frame, aes(x=Year, y=Attacks, colour=AttackType)) +
  geom_line() + 
  theme_bw() +
  scale_colour_discrete(name="Attack Type") +
  scale_y_continuous(limits=c(0,50)) +
  labs(title="Reduced Y-axis range")
attack_legend <- g_legend(attacks_plot)
grid.arrange(arrangeGrob(attacks_plot + theme(legend.position="none"),
                         attacks_zoom_plot + theme(legend.position="none"),
                         nrow=2),
             attack_legend, ncol=2, widths = c(3,1))

# ---- Looking only at Top 6 ----
top6countries <- as.character(plot_data$countries)
countries <- as.character(unique(reduced_sf$sub_countries))
years <- unique(reduced_sf$sub_years)
count <- 1
country_vec <- vector(mode="character")
year_vec <- vector(mode="numeric")
death_vec <- vector(mode="numeric")
attack_vec <- vector(mode="numeric")
attacktype_vec <- vector(mode="character")
r_sf <- reduced_sf[grep(paste(top6countries, collapse='|'), reduced_sf$sub_countries, ignore.case=TRUE),]
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
top6_attacktype_frame <- data.frame(
  Year=year_vec,
  AttackType=attacktype_vec,
  Deaths=death_vec,
  Attacks=attack_vec
)
deaths_plot <- ggplot(top6_attacktype_frame, aes(x=Year, y=Deaths, colour=AttackType)) +
  geom_line() +
  theme_bw() +
  labs(title=
"Terror Attacks by Attack Type in Iraq, Afghanistan, Pakistan, Nigeria,
India & Sri Lanka (Top 6 countries by deaths from terrorist attacks)") +
  scale_colour_discrete(name="Attack Type")
attacks_plot <- ggplot(top6_attacktype_frame, aes(x=Year, y=Attacks, colour=AttackType)) +
  geom_line() + 
  theme_bw() +
  scale_colour_discrete(name="Attack Type")
top6_legend <- g_legend(deaths_plot)
grid.arrange(arrangeGrob(deaths_plot + theme(legend.position="none"),
                         attacks_plot + theme(legend.position="none"),
                         nrow=2),
             top6_legend, ncol=2, widths = c(3,1))


