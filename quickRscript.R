
# Quick R Script
# A. Hart


# Setup ----------------

# Load packages
  library(tidyverse)

# Set directory (change path to match your directory)
  setwd("~/SIS600")
  
# Load data
  wrep = read_csv("womenparl.csv")


  
# Inspect data ---------
  
  head(wrep) # frist few rows
  
  names(wrep) # variable names
  
  summary(wrep) # basic summary stats
  
  
  
# Analysis -------------  

# PR systems?
  wrep %>%
    count(PRsystem) %>%
    mutate(Perc = n/sum(n) * 100)

  
# Graph
  boxplot(wrep$WomenParliament, horizontal = TRUE)

    
# Summary stats
  summary(wrep)
  
  wrep %>%
    summarize(
      Avg = mean(WomenParliament),
      SD = sd(WomenParliament)
    )

  
# Bit more info
  wrep %>%
    filter(WomenParliament >= 50) %>%
    select(Country, WomenParliament)
  

  