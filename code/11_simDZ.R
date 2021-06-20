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

# Variables for counterfactuals
#----------------------
p_black                           = 0.13 # proportion of black population
ratio_median_black2white_worker   = 0.94 # race->outcome: Med[worker income|black]  / Med[worker income|white]
ratio_odds_capitalist_white2black = 16    # race->class: odds ratio P(capitalist|white) / P(capitalist|black) = [P(white|capitalist) / P(black|capitalist)]*[P(black)/P(white)]
ratio_wages2totalincome           = 0.58 # class->outcome: wage share E[worker income]*n_workers/( total income )
med2mean_worker                   = 0.65 # class->outcome: dispersion of wage income (0 < median/mean < 1)

# Fixed parameters
#----------------------
n                   = 10^5 # number of draws 
p_capitalist        = 0.10 # proportion of capitalist class locations
median_white_worker = 36000 # median income for white workers
med2mean_capitalist = 0.50 # dispersion capitalist incomes (0 < median/mean < 1)

#########################################################
#########################################################

# Derived parameters
#----------------------
# Log-normal (workers|race)
s_worker = sqrt(  2 * log(1/med2mean_worker) ); # scale parameter
mu_white_worker = log( median_white_worker )
mu_black_worker = log( median_white_worker * ratio_median_black2white_worker ) 

# Proportions of capitalists conditioned on race
p_capitalist_cond_black = p_capitalist / 
  ( p_black + ratio_odds_capitalist_white2black*(1-p_black) ) #p(capitalist|black)
p_capitalist_cond_white = p_capitalist_cond_black * 
  ratio_odds_capitalist_white2black #p(capitalist|white)

# Parameters linking wage share to capitalist mean income
mean_white_worker = exp( mu_white_worker + s_worker^2/2 ) #E[income | worker, white]
mean_black_worker = exp( mu_black_worker + s_worker^2/2 ) #E[income | worker, black]
mean_worker       = mean_white_worker*(1-p_black) +  mean_black_worker*p_black #E[income | worker]

ratio_odds_capitalist = p_capitalist/(1-p_capitalist)
mean_capitalist       = (1-ratio_wages2totalincome) * mean_worker /
  ( (ratio_wages2totalincome) * ratio_odds_capitalist  )  #E[income | capitalist]

# Log-normal (capitalists|race)
s_capitalist = sqrt(  2 * log(1/med2mean_capitalist) ); # scale parameter
mu_capitalist = log( mean_capitalist ) - s_capitalist^2/2 # > 0


# Population sizes
n_black = round(p_black * n) 
n_white = n - n_black

#########################################################
#########################################################

#  Sample white population

y_white = rep(NA, length = n_white)
c_white = rbern( n = n_white, prob = p_capitalist_cond_white)
for ( k in 1:length(c_white) ) {
  if (c_white[k] == 0) {
    y_white[k] = rlnorm( n=1, meanlog = mu_white_worker, sdlog = s_worker) 
  } else {
    y_white[k] = rlnorm(n=1, meanlog = mu_capitalist, sdlog = s_capitalist)
  }
} 

# Sample black population 

y_black = rep(NA, length = n_black)
c_black = rbern( n = n_black, prob = p_capitalist_cond_black)
for ( k in 1:length(c_black) ) {
  if (c_black[k] == 0) {
    y_black[k] = rlnorm( n=1, meanlog = mu_black_worker, sdlog = s_worker) 
  } else {
    y_black[k] = rlnorm(n=1, meanlog = mu_capitalist, sdlog = s_capitalist)
  }
} 

#########################################################
# STATS
#########################################################

quantile_black_white_10_10 = quantile(y_black, 0.10)/quantile(y_white, 0.10)
quantile_black_white_20_20 = quantile(y_black, 0.20)/quantile(y_white, 0.20)
quantile_black_white_50_50 = quantile(y_black, 0.50)/quantile(y_white, 0.50)
quantile_black_white_80_80 = quantile(y_black, 0.80)/quantile(y_white, 0.80)
quantile_black_white_90_90 = quantile(y_black, 0.90)/quantile(y_white, 0.90)

quantile_black_black_80_20 = quantile(y_black, 0.80)/quantile(y_black, 0.20)
quantile_white_white_80_20 = quantile(y_white, 0.80)/quantile(y_white, 0.20)

total_income = sum(y_white) + sum(y_black);

total_worker = 0;
for ( k in 1:length(c_white) ) {
  if (c_white[k] == 0) {
    total_worker = total_worker + y_white[k]; 
  } 
}

for ( k in 1:length(c_black) ) {
  if (c_black[k] == 0) {
    total_worker = total_worker + y_black[k];
  } 
}



#########################################################
#########################################################

#put these together in a dataframe
incomedf<-rbind.fill(
  data.frame(race='White',income=y_white,capitalist=c_white),
  data.frame(race='Black',income=y_black,capitalist=c_black)
)


tapply(
  incomedf$income,
  list(incomedf$capitalist,incomedf$race),
  quantile,0.1
)

#########################################################
#########################################################

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
    x=income,
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
  xlim(0, 60000) +
  xlab("\nIncome") +
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
    x=income,
    y=income_q_race,
    color=race
  )
) +
  geom_line(
    size=2
  ) +
  geom_point(
    data=tmpdf,
    color='black',
    size=2
  ) +
  geom_line(
    data=tmpdf,
    linetype='solid',
    color='black',
    size=0.5
  ) +
  #this governs the tick marks
  scale_x_log10(
    breaks = 10^(1:8),
    labels = scales::comma
  ) +
  ## this governs limits
  # coord_cartesian(
  #   xlim = c(10^3,10^7)
  # ) +
  scale_color_manual(
    name="",
    values=tmpcolors
  ) +
  theme_bw() +
  xlab("\nIncome") +
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

########################
# Print
########################

print('Inter-racial gaps:')
print(quantile_black_white_10_10)
print(quantile_black_white_20_20)
print(quantile_black_white_50_50)
print(quantile_black_white_80_80)
print(quantile_black_white_90_90)

print('Intra-racial:')
print(quantile_black_black_80_20)
print(quantile_white_white_80_20)

print(total_worker/total_income)


