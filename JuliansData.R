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

# graoh the number of deads in the last 45 years i neach of the countries
