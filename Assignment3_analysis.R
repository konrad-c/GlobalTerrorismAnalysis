library(xlsx)
library(ggplot2)
library(dplyr)
library(reshape)
library(grid)

setwd("~/Uni/FIT2083/Assignment3")

sf <- read.csv2("Data/vic_crime_2012_2016.csv", sep=",")
for(i in 1:ncol(sf)){
  for(j in 1:nrow(sf)){
    if(sf[j, i] == "null")
      sf[j, i] <- NA
  }
}
for(i in 1:ncol(sf)){
  if(i != 27){
    sf[i] <- as.numeric(unlist(sf[i]))
  }
}
summary(sf)

# ---- Fill NA values ----
#install.packages("mice")
library(mice)
sf <- sf[order(sf$ref_period), ]
imputedData <- list()
count <- 1
for(i in unique(sf$lga_code_2011)){
  t_data <- sf[sf$lga_code_2011 == i, -27]
  imputedData[[count]] <- mice(data = t_data, m = 5, method = "pmm", maxit = 50, seed = 500)
  sf[sf$lga_code_2011 == i, -27] <- complete(imputedData[[count]], 1:ncol(sf))
  count <- count+1
}

# ---- TODO: ----
# Look at successes by the number of deaths per act. To do this, 
# group by nkill, e.g. nkill < 5, < 10, < 20, < 50, < 100, etc...
# Can be done with a for loop and breaking if statement:
which(sf$nkill < 1)
group_vector <- vector(mode="vector")
greaterthan_vector <- vector(mode="numeric")
for(j in 1:nrow(sf)){
  row <- sf[j, ]
  year <- as.character(row$iyear)
  for(i in groups){
    if(row$nkill < i){
      if(is.null(group_vector[[year]][[as.character(i)]])){
        group_vector[[year]][[as.character(i)]] <- 1
      }else{
        group_vector[[year]][[as.character(i)]] <- group_vector[[year]][[as.character(i)]] + 1
      }
      break
    }else{ # Greater than max in groups
      if(is.null(group_vector[[year]][[as.character(i)]])){
        greaterthan_vector[[year]] <- 1
      }else{
        greaterthan_vector[[year]] <- group_vector[[year]] + 1
      }
    }
  }
}

sf <- read.csv2("Data/gtd/globalterrorism.csv", sep=",")

sf_known <- sf[sf$nkill != "", ]
#sf[sf$nkill == "", ]$nkill <- mean(as.numeric(as.character(unlist(sf[sf$nkill != "", ]$nkill))))
sf_known$nkill <- as.numeric(as.character(unlist(sf_known$nkill)))
head(sf)
successes <- vector(length = length(unique(sf$iyear)), mode="numeric")
attempts <- vector(length = length(unique(sf$iyear)), mode="numeric")
deaths <- vector(length = length(unique(sf$iyear)), mode="numeric")
deaths_group <- list(mode="vector")
groups <- c(1, 2, 5, 10, 20, 30, 50, 100,150,200,250,300,400,500,1000,2000)
for(year in unique(sf_known$iyear)){
  sf_known_year <- sf_known[sf_known$iyear == year, ]
  for(i in 1:length(groups)){
    if(i == 1){
      deaths_group[[as.character(year)]][[i]] <- length(which(sf_known_year$nkill < groups[[i]]))
    }else{
      deaths_group[[as.character(year)]][[i]] <- length(which(sf_known_year$nkill < groups[[i]] & sf_known_year$nkill >= groups[[i-1]]))# - sum(deaths_group[[as.character(year)]][1:i-1])
    }
  }
}

death_frame <- data.frame(t(data.frame(deaths_group))[2:46, ])
death_frame$Year <- unique(sf$iyear)
colnames(death_frame)[1:(ncol(death_frame)-1)] <- paste(" < ", as.character(groups), sep = "")
death_frame <- melt(death_frame, id.vars = "Year", variable_name = "series")

death_frame$Year <- as.numeric(death_frame$Year)
death_frame$value <- as.numeric(as.character(unlist(death_frame$value)))

p <- ggplot(death_frame, aes(x=Year, y=value, colour=series)) +
  #geom_smooth(method='lm', alpha=0.07) +
  #geom_point() + 
  geom_text(data = subset(death_frame, Year == max(death_frame$Year)), aes(label = series, colour = series, x = Inf, y = value), hjust = -.1) +
  geom_line() +
  #geom_point() +
  scale_colour_discrete(guide = 'none')  +    
  theme(plot.margin = unit(c(1,3,1,1), "lines")) 
  #scale_y_continuous(limits = c(0, 2000))
gt <- ggplotGrob(p)
gt$layout$clip[gt$layout$name == "panel"] <- "off"
grid.draw(gt)

 # ---- Successes vs Attempts
count <- 1
for(year in unique(sf$iyear)){
  successes[count] <- sum(sf[sf$iyear == year, ]$success)
  attempts[count] <- nrow(sf[sf$iyear == year, ])
  deaths[count] <- sum(sf[sf$iyear == year, ]$nkill)
  count <- count + 1
}
success_time <- data.frame(unique(sf$iyear), successes, attempts, deaths)
colnames(success_time) <- c("Year", "Successes", "Attempts", "Deaths")
success_time$Proportion <- success_time$Successes/success_time$Attempts

ggplot(success_time, aes(x=Year, y=Deaths)) + 
  geom_point() + 
  geom_line() + 
  theme_bw()
  #scale_y_continuous(limits=c(0,1))
cor(sf)
