library(ggplot2)
library(dplyr)
library(reshape)
library(grid)

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
for(year in unique(sf$iyear)){
  successes[count] <- sum(sf[sf$iyear == year, ]$success)
  attempts[count] <- nrow(sf[sf$iyear == year, ])
  count <- count + 1
}
success_time <- data.frame(unique(sf$iyear), successes, attempts, deaths)
colnames(success_time) <- c("Year", "Successes", "Attempts", "Deaths")
success_time$Proportion <- success_time$Successes/success_time$Attempts

ggplot(success_time, aes(x=Year, y=Proportion)) + 
  labs(x="Year", y="Successes/Attempts") +
  geom_point() + 
  geom_line() + 
  theme_bw()+
  scale_y_continuous(limits=c(0,1))
