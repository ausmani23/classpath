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

#read in sim data
setwd(datadir); dir()
simdf<-fread(
  'sim_4class.csv'
)

#########################################################
#########################################################


#summarize the sim data
tmpseq<-c(1:10000)/10000
sumdf <- simdf[
  ,
  .(
    income = quantile(income,tmpseq) %>% unname,
    income_q = 100 * tmpseq
  )
  ,
  by=c('race','class','scenario')
]

#spread scenarios
sumdf <- spread(
  sumdf,
  scenario,
  income
)

#calculate welfare cgain

sumdf$welfare_gain <- log(sumdf$both_off) - log(sumdf$class_off)


plotdf <- sumdf

plotdf$class <- factor(
  plotdf$class,
  levels=c(1,2,3,4),
  labels=c(
    'Reserve Army',
    'Working-Class',
    'Professionals',
    'Capitalists'
  )
)

plotdf$race<-factor(
  plotdf$race,
  levels=c('Black','White')
)
tmpcolors<-c(
  'White'='Red',
  'Black'='Blue'
)


g.tmp <- ggplot(
  plotdf,
  aes(
    x=income_q,
    y=welfare_gain,
    group=race,
    color=race
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
  facet_wrap(
    ~ class
  ) +
  ylab(
    'Welfare Gain (Race Paths Off vs. Class Path Off)\n'
  ) +
  xlab(
    '\nWithin-Race, Within-Class Income Percentile'
  ) +
  theme_bw() 

setwd(outputdir)
ggsave(
  plot=g.tmp, 
  filename="fig_4class_counterfactual.png",
  width=8,
  height=6
)