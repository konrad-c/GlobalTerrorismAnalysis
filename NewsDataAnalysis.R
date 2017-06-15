library(ggplot2)
library(dplyr)
library(reshape)
library(gridExtra)
library(MASS)
library(lubridate)
install.packages(devtools)
devtools::install_github("baptiste/egg")
library(egg)

rlm_eqn <- function(df){
  x <- df[, 1]
  y <- df[, 2]
  m <- rlm(y ~ x, df);
  eq <- substitute(italic(y) == a + b %.% italic(x), 
                   list(a = format(coef(m)[1], digits = 2), 
                        b = format(coef(m)[2], digits = 2)))
  as.character(as.expression(eq));                 
}

# --Task---
#  
sf <- read.csv2("Data/gtd/globalterrorism.csv", sep=",")

# read the data
articles_sf <- read.csv2("Crawlers/articleCount.csv", sep=",")
articles_sf$Year <- as.numeric(as.character(articles_sf$Year))
articles_sf$month <- as.character(articles_sf$month)
articles_sf$NumArticles <- as.numeric(as.character(articles_sf$NumArticles))
articles_sf$ArticleType <- "Total"
articles_sf$month[as.numeric(articles_sf$month) < 10] <- paste("0", articles_sf$month[as.numeric(articles_sf$month) < 10], sep="")

terror_articles_sf <- read.csv2("Crawlers/TerrorismArticleCount.csv", sep=",")
terror_articles_sf$Year <- as.numeric(as.character(terror_articles_sf$Year))
terror_articles_sf$month <- as.character(terror_articles_sf$month)
terror_articles_sf$NumArticles <- as.numeric(as.character(terror_articles_sf$NumArticles))
terror_articles_sf$ArticleType <- "Terror Article"
terror_articles_sf$month[as.numeric(terror_articles_sf$month) < 10] <- paste("0", terror_articles_sf$month[as.numeric(terror_articles_sf$month) < 10], sep="")

# Create Date columns:
terror_articles_sf$Date <- paste("01", terror_articles_sf$month, paste0(terror_articles_sf$Year," 00:00:00"), sep="/")
terror_articles_sf$Date <- as.Date(as.character(as.POSIXct(strptime(terror_articles_sf$Date, format="%d/%m/%Y %H:%M:%S"))))
articles_sf$Date <- paste("01", articles_sf$month, paste0(articles_sf$Year," 00:00:00"), sep="/")
articles_sf$Date <- as.Date(as.character(as.POSIXct(strptime(articles_sf$Date, format="%d/%m/%Y %H:%M:%S"))))

# Bind 
sf_news <- rbind(articles_sf, terror_articles_sf)
sf_news$Year <- as.numeric(sf_news$Year)
sf_news <- na.omit(sf_news)
sf_news$NumArticles <- as.numeric(sf_news$NumArticles)
# ---- Plot Data ----
numarticle <- ggplot(sf_news, aes(x=Date, y=NumArticles, colour=ArticleType)) +
  #geom_point() +
  geom_line() + 
  theme_bw() +
  scale_color_discrete(name="Article Type") +
  labs(x="Year", y="# of Articles", title="New York Times Articles Mentioning Terrorism by Month")
sf_prop <- data.frame(
  Date=articles_sf$Date, 
  Proportion=terror_articles_sf$NumArticles/articles_sf$NumArticles
)
proparticle_rlm <- ggplot(sf_prop, aes(x=Date, y=Proportion)) +
  #geom_point() +
  geom_line() + 
  theme_bw() +
  labs(x="Year", y="Proportion of Terrorism Articles to Total Articles") +
  geom_smooth(method="rlm", alpha=0.0) +
  geom_text(x = 1970, y = 0.20, label = rlm_eqn(sf_prop), parse = TRUE) #+
  #geom_vline(xintercept = as.numeric(as.Date("2001/09/11 00:00:00")), colour="red")
ggarrange(numarticle, proparticle_rlm, ncol=1)

# ---- Articles by Year: ----
count <- 1
year_vec <- vector(mode="numeric")
article_total <- vector(mode="numeric")
article_terror <- vector(mode="numeric")
for(year in unique(sf$iyear)){
  year_vec[[count]] <- year
  sub_sf_news <- subset(sf_news, Year == year)
  article_total[[count]] <- round(sum(sub_sf_news[sub_sf_news$ArticleType == "Total",]$NumArticles))
  article_terror[[count]] <- round(sum(sub_sf_news[sub_sf_news$ArticleType == "Terror Article",]$NumArticles))
  count <- count+1
}
sf_news_year <- data.frame(
  Year=year_vec,
  Total=article_total, 
  'Terror Article'=article_terror
)
sf_prop_year <- data.frame(
  Year=year_vec,
  Proportion=article_terror/article_total
)
sf_news_year <- melt(data = sf_news_year, id.vars="Year", variable_name = "ArticleType")
numarticle_year <- ggplot(sf_news_year, aes(x=Year, y=value, colour=ArticleType)) +
  #geom_point() +
  geom_line() + 
  theme_bw() +
  scale_color_discrete(name="Article Type") +
  labs(x="Year", y="# of Articles", title="New York Times Articles Mentioning Terrorism by Year")
proparticle_year_rlm <- ggplot(sf_prop_year, aes(x=Year, y=Proportion)) +
  #geom_point() +
  geom_line() + 
  theme_bw() +
  labs(x="Year", y="Ratio of Terrorism Articles to Total Articles") +
  geom_smooth(method="rlm", alpha=0.0) +
  geom_smooth(method="rlm", data=sfsubset, aes(x=Year, y=Proportion)) +
  geom_text(x = 1975, y = 0.050, label = rlm_eqn(sf_prop_year), parse = TRUE) +
  geom_text(x = 1975, y = 0.030, label = rlm_eqn(sfsubset), parse = TRUE)
ggarrange(numarticle_year, proparticle_year_rlm, ncol=1)

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
date_vec <- list()#vector(mode="numeric")
for(date in unique(sub_date)){
  #date_vec[[count]] <- as.Date(as.character(as.POSIXct(strptime(date, format="%d/%m/%Y %H:%M:%S"))))
  death_set <- subset(reduced_sf, sub_date == date)
  death_vec[[count]] <- round(sum(death_set$sub_deaths))
  attack_vec[[count]] <- nrow(death_set)
  count <- count+1
}
us_sf <- data.frame(
  Date=unique(sub_date),#unlist(date_vec),
  Deaths=death_vec,
  Attacks=attack_vec
  #TerrorArticles=sf_news[sf_news$ArticleType=="Terror Article", ]$NumArticles[1:45]
)
us_sf_melted <- melt(data = us_sf, id.vars = "Year")
us_plot <- ggplot(us_sf_melted, aes(x=Year,y=value, colour=variable)) +
  geom_line() +
  theme_bw() +
  scale_colour_discrete(name="") +
  theme(legend.position = "left") +
  labs(x="", y="Number of", title="United States Terror & News Statistics")
us_plot_zoomed <- ggplot(us_sf_melted, aes(x=Year,y=value, colour=variable)) +
  geom_line() +
  theme_bw() +
  scale_y_continuous(limits=c(0,180))+
  scale_colour_discrete(name="") +
  geom_smooth(method='lm') +
  theme(legend.position = "left") +
  labs(x="Year", y="Number of", title="Reduced Y-axis range")
ggarrange(us_plot, us_plot_zoomed, proparticle_year_rlm, ncol=1)
  #geom_line(data=us_sf, aes(x=Year, y=Deaths)) +
  #geom_line(data=us_sf, aes(x=Year, y=Attacks)) +
  #geom_line(data=sf_prop, aes(x=Year, y=Proportion))

r2 <- function(x){  
        SSe <- sum((x$resid)^2);  
        observed <- x$resid+x$fitted;  
        SSt <- sum((observed-mean(observed))^2);  
        value <- 1-SSe/SSt;  
        return(value);  
        } 
