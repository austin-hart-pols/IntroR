#+ First analysis
#+ A. Hart

# Beginning a script -------------
## Load pacakges
  library(tidyverse)
  library(stargazer)

## Set directory
  setwd("your directory goes here")
  
## Load data (4.1)
  load('womenparl.Rdata')

  
# Explore your data --------------
## Survival Guide 4.2
  head(wdata) # see first few rows
  names(wdata) # variable names
  summary(wdata) # basic summary stats
  

# Summary stats ------------------
## Survival guide Ch. 5-5.2
  
# Describe data on women in parliament (women.in.parl)
  summary(wdata$women.in.parl)
    # syntax: summary(data$variable)

  summarize(wdata, # data
    Wavg = mean(women.in.parl, na.rm = T), # always add `na.rm=T`
    Wsd = sd(women.in.parl, na.rm = T)
  )
  
# Visualize the dist
  hist(
    wdata$women.in.parl, 
    xlab = '% seats held', 
    main = 'Women in parliament, 2023'
  )
  
  boxplot(wdata$women.in.parl, horizontal = TRUE)  
  
  
# Diff of means ------------------
## Survival Guide 5.3
  
# Compare group means
  group_by(wdata, pr.system) |>
    summarise(
      n = n(),
      wip.avg = mean(women.in.parl, na.rm = T),
      wip.med = median(women.in.parl, na.rm = T)
    )
  
# Visualize
  boxplot(women.in.parl ~ pr.system, data = wdata)  
  
# Test Rule's hypothesis
  t.test(
    women.in.parl ~ pr.system, 
    data = wdata, 
    alternative='greater'
  )
  
 
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
    wdata |>
    count(pr.system) |>  # creates freq table
    mutate(Percent = 100 * n/sum(n))
  # last line calculates n as relative freq
  
  tab1
  
# visualize  
  barplot(Percent ~ pr.system, tab1)
  # syntax: barplot(bar.height ~ group, data.name)
  



  