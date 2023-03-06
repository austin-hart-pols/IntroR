# -------------------
# First analysis
# A. Hart

# Setup (2.4 - 2.5)
  library(tidyverse)
  library(stargazer)
  setwd("your directory goes here")
  
# Load data (4.1)
  load('womenparl.Rdata')

  
# Explore your data --------------
## Survival Guide 4.2
 
  head(wdata) # first few rows
  
  names(wdata) # variable names
  
  summary(wdata) # basic summary stats
  

# Summary stats ------------------
## Survival guide Ch. 5-5.2
  
# Describe data on women in parliament (women.in.parl)
  summary(wdata$women.in.parl)
    # syntax: summary(data.name$variable.name)
    # try it for the whole frame: summary(wdata)

  summarise(wdata, # data
    WIP.avg = mean(women.in.parl, na.rm = T), # always add `na.rm=T`
    WIP.sd = sd(women.in.parl, na.rm = T)
  )
    # a customizable approach using the pipe `%>%`
  
# Visualize the dist
  hist(wdata$women.in.parl, xlab = '% seats held', main = 'Women in parliament, 2023')
  boxplot(wdata$women.in.parl, horizontal = TRUE)  
  
  
# Diff of means ------------------
## Survival Guide 5.3
  
# Compare group means
  group_by(wdata, pr.system) %>%
    summarise(
      n = n(),
      wipAvg = mean(women.in.parl, na.rm = T),
      wipMed = median(women.in.parl, na.rm = T)
    )
  
# Visualize
  boxplot(women.in.parl ~ pr.system, data = wdata)  
  
# Test Rule's hypothesis
  t.test(women.in.parl ~ pr.system, data = wdata, alternative='greater')
  
 
# Regression ---------------------
## Survival Guide 7
  
# Scatter enviro protection over women in parliament
  plot(enviro.index ~ women.in.parl, data = wdata)
    # syntax: plot(outcome.var ~ exposure.var, data.name)
  
# Estimate linear association  
  est1 = lm(enviro.index ~ women.in.parl, data = wdata)
  est1  
  stargazer(est1, type = 'text', keep.stat = 'n')
  
# Add the line to the plot
  plot(enviro.index ~ women.in.parl, data = wdata)
  abline(est1, col = 'red', lw = 3)
  
  
# Counts/Freq --------------------
## Survival guide 6
  
# Describe use of proportional representation
  tab1 = 
    count(wdata, pr.system) %>%  # creates freq table
    mutate(Percent = 100 * n/sum(n))
  # last line calculates n as relative freq
  
  tab1
  
# visualize  
  barplot(Percent ~ pr.system, tab1)
  # syntax: barplot(bar.height ~ group, data.name)
  



  