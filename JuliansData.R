library(xlsx)
library(ggplot2)
library(dplyr)
library(reshape)
library(grid)

setwd("/UNI/Y2S1/2083/Assignment3/globalTerrorismAnalysis")

# --Task---
#  

# read the data
sf <- read.csv2("Data/gtd/globalterrorism.csv", sep=",")

# graph the number of deaths dut to terrism for each country
sf_known <- sf[sf$nkill != "", ]
sf_known$nkill <- as.numeric(as.character(unlist(sf_known$nkill)))

countriesTest <- sf_known$country_txt
countriesNumber <- sf_known$country
deaths <- sf$nkill



for (deaths in sf_known$nkill)
  sf_deaths <- data.frame(subset(sf, ))


