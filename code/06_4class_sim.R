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

set.seed(23)

#########################################################
#########################################################

#define the different scenarios

scenarios = c(
  'normal', 
  'indirect_off',
  'direct_off',
  'race_off', 
  'class_off'
)

demographies = c(
  'usa',
  'southafrica'
)

loopdf <- expand.grid(
  scenario=scenarios,
  demography=demographies,
  stringsAsFactors=F
)
loopdf$i<-1:nrow(loopdf)

#########################################################
#########################################################

#load the info from siminfo
setwd(outputdir); dir()
p_white_usa <- readLines('siminfo_whitepop.txt') %>% as.numeric

infodf<-fread('siminfo.csv')

#########################################################
#########################################################

returndf <- lapply(loopdf$i,function(i) {
  
  
  #i<-1
  #loop through
  
  thisrow<-loopdf[i,]
  this_scenario<-thisrow$scenario
  this_demography<-thisrow$demography
  
  #########################################################
  
  #set demography
  if(this_demography=='usa') {
    #p_white = 0.605 / (0.605 + 9.121)
    p_white = p_white_usa/100
    p_black = 1 - p_white
  } else if(this_demography=='southafrica') {
    p_white = 0.102 / (0.784 + 0.102)
    p_black = 1 - p_white
  }
  
  
  #########################################################
  
  #these parameters are set from without
  #(in usa case, given by census data and demography)
  
  
  tmp<-infodf$race_m=='Black'
  p_class_black = c(
    infodf$proportion_race[infodf$class_f=='Reserve Army' & tmp], 
    infodf$proportion_race[infodf$class_f=='Working-Class' & tmp],
    infodf$proportion_race[infodf$class_f=='Professionals' & tmp], 
    infodf$proportion_race[infodf$class_f=='Capitalists' & tmp]
  )/100
  tmp<-infodf$race_m=='White'
  p_class_white = c(
    infodf$proportion_race[infodf$class_f=='Reserve Army' & tmp], 
    infodf$proportion_race[infodf$class_f=='Working-Class' & tmp],
    infodf$proportion_race[infodf$class_f=='Professionals' & tmp], 
    infodf$proportion_race[infodf$class_f=='Capitalists' & tmp]
  )/100
  
  # if(this_demography=='southafrica') {
  #   p_class_white = c(5,30,40,25)/100
  # }
  
  #marginalized probabilities
  p_class = (p_class_black * p_black) + (p_class_white * p_white)
  
  #median income, by race and class
  tmp<-infodf$race_m=='Black'
  y_class_black_median = c(
    infodf$median_income[infodf$class_f=='Reserve Army' & tmp], 
    infodf$median_income[infodf$class_f=='Working-Class' & tmp],
    infodf$median_income[infodf$class_f=='Professionals' & tmp], 
    infodf$median_income[infodf$class_f=='Capitalists' & tmp]
  )
  tmp<-infodf$race_m=='White'
  y_class_white_median = c(
    infodf$median_income[infodf$class_f=='Reserve Army' & tmp], 
    infodf$median_income[infodf$class_f=='Working-Class' & tmp],
    infodf$median_income[infodf$class_f=='Professionals' & tmp], 
    infodf$median_income[infodf$class_f=='Capitalists' & tmp]
  )
  
  #mean income, by race and class
  tmp<-infodf$race_m=='Black'
  y_class_black_mean = c(
    infodf$mean_income[infodf$class_f=='Reserve Army' & tmp], 
    infodf$mean_income[infodf$class_f=='Working-Class' & tmp],
    infodf$mean_income[infodf$class_f=='Professionals' & tmp], 
    infodf$mean_income[infodf$class_f=='Capitalists' & tmp]
  )
  tmp<-infodf$race_m=='White'
  y_class_white_mean = c(
    infodf$mean_income[infodf$class_f=='Reserve Army' & tmp], 
    infodf$mean_income[infodf$class_f=='Working-Class' & tmp],
    infodf$mean_income[infodf$class_f=='Professionals' & tmp], 
    infodf$mean_income[infodf$class_f=='Capitalists' & tmp]
  )
  
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
  
  #########################################################
  #########################################################
  
  # Race -> Class
  #----------------------
  if ( this_scenario %in% c('indirect_off','race_off') ) { 
    #eliminate race->class
    p_class_black <- p_class_white <- p_class
  }
  
  # Race --> Outcome
  #----------------------
  
  if (this_scenario %in% c('direct_off', 'race_off') ) {
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
  
  mu_class_black = log( y_class_black_median ) 
  mu_class_white = log( y_class_white_median )
  
  s_class_black = sqrt(  2 * (y_class_black_mean/y_class_black_median) ) 
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
  returndf$i <- i
  
  
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

#add meta info back in
returndf<-merge(
  loopdf,
  returndf,
  by='i'
)

#########################################################
#########################################################

setwd(datadir)
write.csv(
  returndf,
  'sim_4class.csv',
  row.names=F
)

#########################################################
#########################################################

#troublehsooting

#why is the black working class above mean income? 
# 
# returndf<-data.table(returndf)
# tmpdf<-returndf[scenario=='normal' & demography=='usa']
# mean_inc <- tapply(tmpdf$income,tmpdf$class,mean)
# tmpdf[race=='Black' & class==4 & income>mean_inc,.N]
# tmpdf[race=='Black' & class==2 & income<mean_inc,.N]
# 
