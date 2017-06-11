library(ggplot2)
library(dplyr)
library(reshape)
library(grid)
library(gridExtra)

setwd("~/Uni/FIT2083/GlobalTerrorismAnalysis")

# ---- TODO: ----
# Look at successes by the number of deaths per act. To do this, 
# group by nkill, e.g. nkill < 5, < 10, < 20, < 50, < 100, etc...
# Can be done with a for loop and breaking if statement:

sf <- read.csv("Data/gtd/globalterrorism.csv", sep=",")

sf_known <- sf[sf$nkill != "", ]
#sf[sf$nkill == "", ]$nkill <- mean(as.numeric(as.character(unlist(sf[sf$nkill != "", ]$nkill))))
sf_known$nkill <- as.numeric(as.character(unlist(sf_known$nkill)))
head(sf)
successes_group <- list(mode="vector")
attempts_group <- list(mode="vector")
deaths_group <- list(mode="vector")
groups <- c(1, 2, 5, 10, 20, 30, 50, 100,150,200,250,300,400,500,1000,2000)
for(year in unique(sf_known$iyear)){
  sf_known_year <- sf_known[sf_known$iyear == year, ]
  for(i in 1:length(groups)){
    if(i == 1){
      deaths_group[[as.character(year)]][[i]] <- length(which(sf_known_year$nkill < groups[[i]]))
    }else{
      deaths_group[[as.character(year)]][[i]] <- length(which(sf_known_year$nkill < groups[[i]] & sf_known_year$nkill >= groups[[i-1]]))
    }
  }
}

death_frame <- data.frame(t(data.frame(deaths_group))[2:46, ])
death_frame$Year <- unique(sf$iyear)
colnames(death_frame)[1] <- "  0 deaths"
colnames(death_frame)[2:(ncol(death_frame)-1)] <- paste(" < ", as.character(groups[2:length(groups)]), " deaths", sep = "")
death_frame <- melt(death_frame, id.vars = "Year", variable_name = "series")

death_frame$Year <- as.numeric(death_frame$Year)
death_frame$value <- as.numeric(as.character(unlist(death_frame$value)))
high_death_frame <- death_frame[death_frame$series != c("  0 deaths", " < 2 deaths", " < 5 deaths"), ]
p <- ggplot(high_death_frame, aes(x=Year, y=value, colour=series)) +
  geom_text(data = subset(death_frame, Year == max(death_frame$Year)), aes(label = series, colour = series, x = Inf, y = value), hjust = -.1) +
  geom_line() +
  #geom_point() +
  scale_colour_discrete(guide = 'none')  +  
  theme_bw() + 
  theme(plot.margin = unit(c(1,6,1,1), "lines")) +
  scale_y_continuous(limits = c(0,1100), expand=c(0,0)) +
  scale_x_continuous(limits = c(1970, 2015), expand=c(0,0))+
  labs(x="Year", y="# of Attacks")
#scale_y_continuous(limits = c(0, 2000))
gt <- ggplotGrob(p)
gt$layout$clip[gt$layout$name == "panel"] <- "off"
grid.draw(gt)

# ---- Successes vs Attempts
count <- 1
successes <- vector(mode="numeric")
attempts <- vector(mode="numeric")
country_vec <- vector(mode="character")
year_vec <- vector(mode="numeric")
# Top6
sf_sub <- sf[grep(paste(plot_data$countries, collapse='|'), sf$country_txt, ignore.case=TRUE),]
# Western Countries
sf_sub <- sf[grep(paste(western_countries[1:6], collapse='|'), sf$country_txt, ignore.case=TRUE),]
# United states
sf_sub <- sf[sf$country_txt == "United States", ]

countries <- unique(as.character(sf_sub$country_txt))
for(country in countries){
  sf_subset <- sf_sub[sf_sub$country_txt == country, ]
  for(year in unique(sf$iyear)){
    #country_vec[[count]] <- country
    year_vec[[count]] <- year
    successes[count] <- sum(sf_subset[sf_subset$iyear == year, ]$success)
    attempts[count] <- nrow(sf_subset[sf_subset$iyear == year, ])
    count <- count + 1
  }
}

success_time <- data.frame(
  #Country=country_vec, 
  Year=year_vec,
  Successes=successes,
  Attempts=attempts,
  Proportion=successes/attempts
)
success_time$Proportion[which(success_time$Proportion == "NaN")] <- 0
# Summary
for(country in unique(success_time$Country)){
  print(country)
  print(summary(success_time[success_time$Country==country, ]$Attempts))
}

plots <- list()
legend_cols <- c("Successes"="#E41A1C", "Attempts"="#377EB8", "Proportion"="black")
for(country in unique(success_time$Country)){
  success_time_sub <- success_time[success_time$Country == country, ]
  plots[[country]] <- ggplot() + 
    geom_line(data=success_time_sub, aes(x=Year, y=Proportion, colour="Proportion")) +
    geom_point(data=success_time_sub, aes(x=Year, y=Proportion, colour="Proportion")) +
    #geom_line(data=success_time_sub, aes(x=Year, y=Attempts, colour="Attempts")) +
    labs(x="Year", y="Successes/Attempts", title=country) +
    theme_bw() +
    #scale_y_continuous(limits=c(0,1)) +
    scale_colour_manual(name="",values=legend_cols)
}
grid.arrange(grobs = plots, ncol=ceiling(sqrt(length(plots))))

  #scale_y_continuous(limits=c(0,1))
