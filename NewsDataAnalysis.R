library(ggplot2)
library(dplyr)
library(reshape)
library(gridExtra)
library(lubridate)
install.packages(devtools)
devtools::install_github("baptiste/egg")
library(egg)

# --Task---
#  
sf <- read.csv2("Data/gtd/globalterrorism.csv", sep=",")

# read the data
articles_sf <- read.csv2("Crawler/articleCount.csv", sep=",")
articles_sf$ArticleType <- "Total"
terror_articles_sf <- read.csv2("Crawler/TerrorArticleCount.csv", sep=",")
terror_articles_sf$ArticleType <- "Terror Article"
sf_news <- rbind(articles_sf, terror_articles_sf)
sf_news$Year <- as.numeric(sf_news$Year)
sf_news$NumArticles <- as.numeric(sf_news$NumArticles)
# ---- Plot Data ----
numarticle <- ggplot(sf_news, aes(x=Year, y=NumArticles, colour=ArticleType)) +
  geom_point() +
  geom_line() + 
  theme_bw() +
  scale_color_discrete(name="Article Type") +
  labs(x="Year", y="# of Articles")
sf_prop <- data.frame(
  Year=articles_sf$Year, 
  Proportion=terror_articles_sf$NumArticles/articles_sf$NumArticles
)
proparticle <- ggplot(sf_prop, aes(x=Year, y=Proportion)) +
  geom_point() +
  geom_line() + 
  theme_bw() +
  labs(x="Year", y="Ratio of Terrorism Articles to Total Articles") +
  geom_smooth(method="lm", alpha=0.0)
grid.arrange(numarticle, proparticle, ncol=1)

# ---- Terror Articles vs Attacks & Deaths
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
reduced_sf <- reduced_sf[reduced_sf$sub_countries == "United States", ]
reduced_sf <- na.omit(reduced_sf)

count <- 1
year_vec <- vector(mode="numeric")
death_vec <- vector(mode="numeric")
attack_vec <- vector(mode="numeric")
for(year in years){
  year_vec[[count]] <- year
  death_set <- subset(reduced_sf, sub_years == year)
  death_vec[[count]] <- round(sum(death_set$sub_deaths))
  attack_vec[[count]] <- nrow(death_set)
  count <- count+1
}
us_sf <- data.frame(
  Year=year_vec,
  Deaths=death_vec,
  Attacks=attack_vec
  #TerrorArticles=sf_news[sf_news$ArticleType=="Terror Article", ]$NumArticles[1:45]
)
us_sf <- melt(data = us_sf, id.vars = "Year")
us_plot <- ggplot(us_sf, aes(x=Year,y=value, colour=variable)) +
  geom_line() +
  theme_bw() +
  scale_colour_discrete(name="") +
  theme(legend.position = "left") +
  labs(x="", y="Number of", title="United States Terror & News Statistics")
us_plot_zoomed <- ggplot(us_sf, aes(x=Year,y=value, colour=variable)) +
  geom_line() +
  theme_bw() +
  scale_y_continuous(limits=c(0,180))+
  scale_colour_discrete(name="") +
  theme(legend.position = "left") +
  labs(x="Year", y="Number of", title="Reduced Y-axis range")
ggarrange(us_plot, us_plot_zoomed, proparticle, ncol=1)
  #geom_line(data=us_sf, aes(x=Year, y=Deaths)) +
  #geom_line(data=us_sf, aes(x=Year, y=Attacks)) +
  #geom_line(data=sf_prop, aes(x=Year, y=Proportion))
