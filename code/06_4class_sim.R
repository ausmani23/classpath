#########################################################
#########################################################

#clear workspace
rm(list=ls())

#load packages
require(stringr)
require(plyr)
require(dplyr)
require(zoo)
require(tidyr)
require(rprojroot)
require(data.table)
require(ggplot2)

#set dirs
homedir<-find_root(
  criterion=has_file('_notesonclass.Rproj')
)
codedir<-file.path(
  homedir,"code"
)
outputdir<-file.path(
  homedir,"output"
)
datadir<-file.path(
  homedir,"data"
)

#########################################################
#########################################################

#extra packs
require(Rlab)
require(scales)

#########################################################
#########################################################

#stick in a loop
scenarios = c(
  'normal', 
  'indirect_off',
  'direct_off',
  'both_off', 
  'class_off'
)

### DATA
#these parameters are set from without
#(mostly given by census data)

p_black = 0.15 # proportion of black population
p_white = 1 - p_black

p_class_black = c(18.95, 66.30, 12.63, 2.11)/100
p_class_white = c(12.13, 59.46, 23.75, 4.65)/100

#marginalized probabilities
p_class = (p_class_black * p_black) + (p_class_white * p_white)

#median income, by race and class
y_class_black_median = c(3500, 33000, 60000, 35950)
y_class_white_median = c(8300, 49000, 80000, 66000)

#mean income, by race and class
y_class_black_mean = c(13144, 41843, 76287, 62900)
y_class_white_mean = c(25293, 63433, 112858, 119976)

ratio_median2mean_class_black = y_class_black_median/y_class_black_mean
ratio_median2mean_class_white = y_class_white_median/y_class_white_mean

#marginalized race variables
y_black_mean = sum(y_class_black_mean * p_class_black)
y_white_mean = sum(y_class_white_mean * p_class_white)

#marignalized class variables
y_class_mean = y_class_black_mean * p_black + y_class_white_mean * p_white
y_mean = sum(y_class_mean * p_class)
ratio_median2mean_class = ratio_median2mean_class_black*p_black + ratio_median2mean_class_white*p_white

#class-convergence parameter, ranges 0-1
#when 0, class distributions do not converge at all
#when 1, classes converge fully
#we set this parameter to create some kind of desired gini coefficient
alpha <- 1

returndf <- lapply(scenarios,function(this_scenario) {
  
  
  #this_scenario <- 'class_off'
  
  # Race -> Class
  #----------------------
  if ( this_scenario %in% c('indirect_off','both_off') ) { 
    #eliminate race->class
    p_class_black <- p_class_white <- p_class
  }
  
  # Race --> Outcome
  #----------------------
  
  if (this_scenario %in% c('direct_off', 'both_off') ) {
    #eliminate race->outcome
    y_class_black_mean <- y_class_white_mean <- y_class_mean
    y_class_black_median <- y_class_white_median <- y_class_mean * ratio_median2mean_class
  }
  
  # Class --> Outcome
  #----------------------
  
  if (this_scenario == 'class_off') {
    #eliminate class->outcome
    
    #converge means
    y_class_black_mean   = alpha*y_black_mean + (1-alpha)*y_class_black_mean
    y_class_white_mean   = alpha*y_white_mean + (1-alpha)*y_class_white_mean
    
    #converge dispersions
    y_class_black_median = y_class_black_mean * (
      alpha*max(ratio_median2mean_class) + 
        (1-alpha)*ratio_median2mean_class_black
    ) 
    y_class_white_median = y_class_white_mean * (
      alpha*max(ratio_median2mean_class) + 
        (1-alpha)*ratio_median2mean_class_white
    ) 
    
  }
  
  # Fixed parameters
  #----------------------
  n       = 1 * 10^5 # number of draws
  n_black = as.integer( round(p_black * n) )
  n_white = n - n_black
  
  mu_class_black = log( y_class_black_median ) #anchored in median?
  mu_class_white = log( y_class_white_median )
  
  s_class_black = sqrt(  2 * (y_class_black_mean/y_class_black_median) ) #CHECK
  s_class_white = sqrt(  2 * (y_class_white_mean/y_class_white_median) )
  
  # Simulate
  #---------------------
  
  # Black pop
  y_black = rep(NA, length = n_black)
  c_black = sample( 1:4, size = n_black, replace=T, prob = p_class_black )
  for ( k in 1:length(c_black) ) {
    y_black[k] = rlnorm( 
      n=1, 
      meanlog = mu_class_black[c_black[k]], 
      sdlog = s_class_black[c_black[k]]
    )
  }
  
  #  White pop
  y_white = rep(NA, length = n_white)
  c_white = sample( 1:4, size = n_white, replace=T, prob = p_class_white )
  for ( k in 1:length(c_white) ) {
    y_white[k] = rlnorm( 
      n=1, 
      meanlog = mu_class_white[c_white[k]], 
      sdlog = s_class_white[c_white[k]]
    )
  }
  
  #output df
  returndf <- rbind.fill(
    data.frame(race='White',income=y_white,class=c_white),
    data.frame(race='Black',income=y_black,class=c_black)
  )
  returndf$scenario <- this_scenario
  
  
  #calculate quantiles
  
  #add quantile in overall distribution of income
  tmpf<-ecdf(returndf$income)
  returndf$income_q <- 100 * tmpf(returndf$income)
  
  #get quantile in race-specific distribution of income
  returndf<-by(returndf,returndf$race,function(df) {
    tmpf<-ecdf(df$income)
    df$income_q_race<-100 * tmpf(df$income)
    df
  }) %>% rbind.fill %>% data.table
  
  returndf
  
}) %>% rbind.fill


#########################################################
#########################################################

setwd(datadir)
write.csv(
  returndf,
  'sim_4class.csv',
  row.names=F
)






