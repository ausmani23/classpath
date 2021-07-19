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

#for weighted ecdf
require(spatstat)

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

setwd(datadir); dir()
incomedf<-fread(
  'censusdf.csv'
)
bottomcode<-min(incomedf$inctot_f)


#########################################################
#########################################################

#REPARATIONS AND REDISTRIBUTION

#play around w/ race-based and class-based simulation
tmpdf<-incomedf[year==2019,]

#we use weight_f to simulate.. 
tmpdf<-data.frame(
  income=rep(tmpdf$inctot_f,tmpdf$weight_f),
  race=rep(tmpdf$race_f,tmpdf$weight_f),
  class=rep(tmpdf$class_f,tmpdf$weight_f)
) %>% data.table

#this gets percentile
tmpf<-ecdf(tmpdf$income)
tmpdf$income_q<-round(100 * tmpf(tmpdf$income))

#also generate race-specific percentile
tmpfunction<-function(x) {
  tmpf<-ecdf(x)
  100 * tmpf(x)
}
tmpdf[
  ,
  income_q_race := tmpfunction(income)
  ,
  by=c('race')
]

#########################################################
#########################################################

#REPARATIONS

#median gap between blacks/whites
mwage_w <- tmpdf[race==1,median(income)]
mwage_b <- tmpdf[race==2,median(income)]

#we need to close this gap
mediangap <- mwage_w - mwage_b
wbratio <- nrow(tmpdf[race==1])/nrow(tmpdf[race==2])

#we take x/wbratio from whites to give x to blacks
#x + x/wbratio = mediangap
#solve for x
reparations_gain <- (mediangap * wbratio)/(1 + wbratio)
reparations_loss <- -1 * reparations_gain / wbratio
tmpdf[race==1, reparations_tax:=reparations_loss]
tmpdf[race==2, reparations_tax:=reparations_gain]
tmpdf$reparations_tax[tmpdf$race==1] <- reparations_loss
tmpdf$reparations_tax[tmpdf$race==2] <- reparations_gain
#transfer the money 
#(except, if you don't have very much, we will only take as much
#as will leave you with bottomcode amount)
tmpdf[
  income + reparations_tax<bottomcode,
  reparations_tax := bottomcode - income
]
tmpdf[,income_reparations := income + reparations_tax]

#check that the median gap is closed (yup)
tmpdf[,median(income_reparations),by='race']

#rank everyone by interest in this proposal
#(i.e., what they gain in welfare) 
tmpdf[,welfare_reparations:=log(income_reparations)-log(income)]

#########################################################
#########################################################

#REDISTRIBUTION

#split the pie more equally
#we do this by shrinking everyone towards the mean
#and do it so that the gini is not so dramatic.. 
DescTools::Gini(tmpdf$income) #initial gini

avgincome <- mean(tmpdf$income)
shrinkage <- 0.5 #shrinkage factor, used to calibrate how much redistribution
tmpdf[,redistribution_tax := (avgincome - income) * shrinkage]
tmpdf[,income_redistribution := income + redistribution_tax]
tmpdf[,welfare_redistribution := log(income_redistribution) - log(income)]

#gini before, after
# require(DescTools)
# Gini(tmpdf$income) #0.51
# Gini(tmpdf$income_reparations) #0.50
# Gini(tmpdf$income_redistribution) #0.25

#########################################################
#########################################################

#REPARATIONS VS. REDISTRIBUTION
tmpdf[,welfare_comparison := welfare_reparations - welfare_redistribution]

#we will want to summarize this in a series of ways

#BY RACE-SPECIFIC PERCENTILE

sumdf<-tmpdf[
  ,
  .(
    welfare_redistribution=median(welfare_redistribution),
    welfare_reparations=median(welfare_reparations),
    welfare_comparison=median(welfare_comparison)
  )
  ,
  by=c(
    'race',
    'income_q_race'
  )
]

sumdf<-gather(
  sumdf,
  scenario,
  welfare,
  welfare_redistribution:welfare_comparison
)
sumdf$scenario<-str_replace(sumdf$scenario,"welfare\\_","")

sumdf$scenario <- factor(
  sumdf$scenario,
  levels=c('reparations','redistribution','comparison'),
  labels=c('Reparations','Redistribution','Net Comparison')
)

sumdf$race<-factor(
  sumdf$race,
  levels=c(1,2),
  labels=c('Whites','Blacks')
)
tmpcolors<-c(
  'Whites'='Red',
  'Blacks'='Blue'
)

g.tmp<-ggplot(
  sumdf,
  aes(
    x=income_q_race,
    y=welfare,
    color=race,
    group=race
  )
) +
  geom_line(
    size=1
  ) +
  geom_hline(
    yintercept=0,
    linetype='dashed'
  ) +
  scale_color_manual(
    name="",
    values=tmpcolors
  ) +
  xlab("\nWithin-Race Income Rank (Percentile)") +
  ylab("Welfare Gain (+ implies support, - implies oppose)\n") +
  facet_wrap(
    ~ scenario,
    ncol=1
  ) + 
  theme_bw() + 
  theme(
    legend.position = 'top',
    legend.direction = 'horizontal'
  )

setwd(outputdir)
ggsave(
  plot=g.tmp,
  filename="fig_welfarecomparison.png",
  width=5,
  height=10
)

#########################################################
#########################################################

#BY RACE AND CLASS


#show preferences across the three scenarios

sumdf<-tmpdf[
  ,
  .(
    welfare_redistribution=median(welfare_redistribution),
    welfare_reparations=median(welfare_reparations),
    welfare_comparison=median(welfare_comparison)
  )
  ,
  by=c(
    'race',
    'class'
  )
]

sumdf<-gather(
  sumdf,
  scenario,
  welfare,
  welfare_redistribution:welfare_comparison
)
sumdf$scenario<-str_replace(sumdf$scenario,"welfare\\_","")

sumdf$scenario <- factor(
  sumdf$scenario,
  levels=c('reparations','redistribution','comparison'),
  labels=c('Reparations','Redistribution','Net Comparison')
)


sumdf$race<-factor(
  sumdf$race,
  levels=c(2,1),
  labels=c('Black','White')
)

sumdf$class<-factor(
  sumdf$class,
  levels=c(1,2,3,4),
  labels=c(
    'Reserve Army',
    'Working-Class',
    'Professionals',
    'Capitalists'
  )
)

g.tmp<-ggplot(
  sumdf,
  aes(
    x=race,
    y=class,
    fill=welfare,
    label=format(round(welfare,2),nsmall=2)
  )
) +
  geom_tile(
    width=0.9,
    height=0.9,
    color='black',
    size=1.5
  ) +
  geom_text(
    color='black'
  ) +
  scale_fill_gradient2(
    guide=F,
    low = "red",
    mid = "white",
    high = "darkgreen",
    midpoint = 0
  ) +
  scale_x_discrete(position='top') +
  xlab("") +
  ylab("") +
  facet_wrap(
    ~ scenario
  ) +
  theme_bw() +
  theme(
    panel.grid.major = element_blank(),
    panel.border = element_blank(),
    axis.ticks = element_blank()
  )

setwd(outputdir)
ggsave(
  plot=g.tmp,
  filename="fig_welfarecomparison_table.png",
  width=8,
  height=3
)


#########################################################
#########################################################
