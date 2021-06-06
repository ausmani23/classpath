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

#########################################################
#########################################################

# Variables for counterfactuals
#----------------------
p_black                           = 0.15 # proportion of black population
ratio_median_black2white_worker   = 0.70 # race->outcome: racial differential within working class
ratio_odds_capitalist_white2black = 4 # race->class: odds ratio of white vs. black in capitalist class
ratio_median_capitalist2worker    = 2 # class->outcome: capitalist to white-worker median

# Fixed parameters
#----------------------
n                   = 10^3 # number of draws 
p_capitalist        = 0.30 # proportion of capitalist class locations
median_white_worker = 1000 # median income for workers
med2mean_worker     = 0.70 # dispersion of worker incomes (median to mean < 1)
med2mean_capitalist = 0.50 # dispersion capitalist incomes (median to mean < 1)

#########################################################
#########################################################

# Derived parameters
#----------------------
# Log-normal (workers)
mu_white_worker = log( median_white_worker )
mu_black_worker = log( median_white_worker * ratio_median_black2white_worker ) 
s_worker = sqrt(  2 * log(1/med2mean_worker) ); # scale parameter

# Log-normal (capitalists)
mu_capitalist = log( median_white_worker * ratio_median_capitalist2worker ) # > 0
s_capitalist = sqrt(  2 * log(1/med2mean_capitalist) ); # scale parameter

# Proportions of capitalists
p_property_black = p_capitalist / ( p_black + ratio_odds_capitalist_white2black*(1-p_black) )
p_property_white = p_property_black * ratio_odds_capitalist_white2black

# Population sizes
n_black = round(p_black * n) 
n_white = n - n_black

#########################################################
#########################################################

#  Sample white population

y_white = rep(NA, length = n_white)
c_white = rbern( n = n_white, prob = p_property_white)
for ( k in 1:length(c_white) ) {
  if (c_white[k] == 0) {
    y_white[k] = rlnorm( n=1, meanlog = mu_white_worker, sdlog = s_worker) 
  } else {
    y_white[k] = rlnorm(n=1, meanlog = mu_capitalist, sdlog = s_capitalist)
  }
} 

# Sample black population 

y_black = rep(NA, length = n_black)
c_black = rbern( n = n_black, prob = p_property_black)
for ( k in 1:length(c_black) ) {
  if (c_black[k] == 0) {
    y_black[k] = rlnorm( n=1, meanlog = mu_black_worker, sdlog = s_worker) 
  } else {
    y_black[k] = rlnorm(n=1, meanlog = mu_capitalist, sdlog = s_capitalist)
  }
} 

#########################################################
#########################################################

quantile_black_white_50_50 = quantile(y_black, 0.50)/quantile(y_white, 0.50)
quantile_black_white_20_20 = quantile(y_black, 0.20)/quantile(y_white, 0.20)
quantile_black_white_80_80 = quantile(y_black, 0.80)/quantile(y_white, 0.80)

quantile_black_black_80_20 = quantile(y_black, 0.80)/quantile(y_black, 0.20)
quantile_white_white_80_20 = quantile(y_white, 0.80)/quantile(y_white, 0.20)

#########################################################
#########################################################

#put these together in a dataframe
incomedf<-rbind.fill(
  data.frame(race='White',income=y_white),
  data.frame(race='Black',income=y_black)
)

#add quantile in overall distribution of income
tmpf<-ecdf(incomedf$income)
incomedf$income_q <- 100 * tmpf(incomedf$income)

#get quantile in race-specific distribution of income
incomedf<-by(incomedf,incomedf$race,function(df) {
  tmpf<-ecdf(df$income)
  df$income_q_race<-100 * tmpf(df$income)
  df
}) %>% rbind.fill %>% data.table

incomedf$race<-factor(incomedf$race)
tmpcolors<-c('blue','red')
names(tmpcolors)<-levels(incomedf$race)

#########################################################
#########################################################

#make a plot of the PDF
g.tmp<-ggplot(
  incomedf,
  aes(
    x=log(income),
    group=race,
    color=race
  )
) +
  geom_density(
    size=2,
  ) +
  scale_color_manual(
    name="",
    values=tmpcolors
  ) +
  xlab("\nIncome (log)") +
  ylab("Density\n") + 
  theme_bw() +
  theme(
    legend.position='top',
    legend.direction='horizontal'
  )

setwd(outputdir)
ggsave(
  "fig_pdfs.png",
  plot=g.tmp,
  width=8,
  height=6
)


#########################################################
#########################################################

#make a plot of the CDF

#illustrate the 50th percentile
tmpdf<-by(incomedf,incomedf$race,function(tmpdf) {
  # tmpdf$tmp<-tmpdf$outcome_i_q_race - 25
  # tmpdf$q25<-abs(tmpdf$tmp)==min(abs(tmpdf$tmp))
  tmpdf$tmp<-tmpdf$income_q_race - 50
  tmpdf$q50<-abs(tmpdf$tmp)==min(abs(tmpdf$tmp))
  # tmpdf$tmp<-tmpdf$outcome_i_q_race - 75
  # tmpdf$q75<-abs(tmpdf$tmp)==min(abs(tmpdf$tmp))
  tmpdf<-tmpdf[q50==T]
  tmpdf<-gather(
    tmpdf,
    var,
    val,
    q50
  ) %>% data.table
  tmpdf[val==T]
}) %>% rbind.fill

g.tmp <- ggplot(
  incomedf,
  aes(
    x=log(income),
    y=income_q_race,
    color=race
  )
) +
  geom_point(
    size=2,
    alpha=0.5
  ) +
  geom_point(
    data=tmpdf,
    color='black',
    size=2
  ) +
  geom_line(
    data=tmpdf,
    linetype='dashed',
    color='black',
    size=0.5
  ) +
  scale_color_manual(
    name="",
    values=tmpcolors
  ) +
  theme_bw() +
  xlab("\nIncome (log)") +
  ylab("Within-Race Quantile\n") + 
  theme(
    legend.position='top',
    legend.direction='horizontal'
  )

setwd(outputdir)
ggsave(
  "fig_cdfs.png",
  plot=g.tmp,
  width=8,
  height=6
)







