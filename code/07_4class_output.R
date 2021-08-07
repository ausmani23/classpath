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

#gini's are not realistic
require(DescTools)
simdf[
  ,
  .(gini = Gini(income))
  ,
  by=c(
    'scenario'
  )
]

# #truncate top and bottom incomes?
# sum(simdf$income>10^6)/nrow(simdf) #3% of people
# simdf$income[simdf$income>10^6]<-10^6
# sum(simdf$income<10^3)/nrow(simdf) #also 3% of people
# simdf$income[simdf$income/10^3]<-10^3

#we can reduce it a little.. 
simdf[
  ,
  .(gini = Gini(income))
  ,
  by=c(
    'scenario'
  )
]

#########################################################
#########################################################

#MAKE A RACE x CLASS PLOT

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
plotdf <- spread(
  sumdf,
  scenario,
  income
)

#calculate welfare cgain
plotdf$welfare_gain <- log(plotdf$both_off) - log(plotdf$class_off)

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
    'Welfare(Race-Based Strategy) - Welfare(Class-Based Strategy)\n'
  ) +
  xlab(
    '\nWithin-Race, Within-Class Income Percentile'
  ) +
  theme_bw() 

setwd(outputdir)
ggsave(
  plot=g.tmp, 
  filename="fig_4class_counterfactual_byclass.png",
  width=8,
  height=6
)

#########################################################
#########################################################

#MAKE A SINGLE PLOT FOR RACE


#summarize the sim data
tmpseq<-c(1:10000)/10000
sumdf <- simdf[
  ,
  .(
    income = quantile(income,tmpseq) %>% unname,
    income_q = 100 * tmpseq
  )
  ,
  by=c('race','scenario')
]

#spread scenarios
plotdf <- spread(
  sumdf,
  scenario,
  income
)

#calculate welfare cgain
plotdf$net_chg <- log(plotdf$both_off) - log(plotdf$class_off)
plotdf$class_chg <- log(plotdf$class_off) - log(plotdf$normal)
plotdf$race_chg <- log(plotdf$both_off) - log(plotdf$normal)
# plotdf$class <- factor(
#   plotdf$class,
#   levels=c(1,2,3,4),
#   labels=c(
#     'Reserve Army',
#     'Working-Class',
#     'Professionals',
#     'Capitalists'
#   )
# )
tmpvars<-c(
  'race',
  'income_q',
  'net_chg',
  'class_chg',
  'race_chg'
)

plotdf<-plotdf[,tmpvars,with=F]

plotdf <- gather(
  plotdf,
  counterfactual,
  gain,
  net_chg:race_chg
) %>% data.table

plotdf$race<-factor(
  plotdf$race,
  levels=c('Black','White')
)
tmpcolors<-c(
  'White'='Red',
  'Black'='Blue'
)

tmplevels<-c(
  'race_chg',
  'class_chg',
  'net_chg'
)
tmplabels<-c(
  "Race-Based Interventions",
  "Class-Based Intervention",
  "Comparison of the Two"
)
plotdf$counterfactual <- factor(
  plotdf$counterfactual,
  tmplevels,
  tmplabels
)

g.tmp <- ggplot(
  plotdf,
  aes(
    x=income_q,
    y=gain,
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
    ~ counterfactual
  ) +
  ylab(
    'Welfare Gain\n'
  ) +
  xlab(
    '\nWithin-Race Income Percentile'
  ) +
  theme_bw() 

setwd(outputdir)
ggsave(
  plot=g.tmp, 
  filename="fig_4class_counterfactuals.png",
  width=8,
  height=6
)

#another way of representing this

plotdf<-sumdf[scenario%in%c('normal','both_off','class_off')]

plotdf$race<-factor(
  plotdf$race,
  levels=c('Black','White')
)

plotdf$scenario<-factor(
  plotdf$scenario,
  levels=c('normal','both_off','class_off'),
  labels=c('Baseline','Race-Based Counterfactual','Class-Based Counterfactual')
)
tmpcolors<-c(
  'Baseline' = 'Black',
  'Race-Based Counterfactual'='Blue',
  'Class-Based Counterfactual'='Red'
)

g.tmp<- ggplot(
  plotdf,
  aes(
    x=income_q,
    y=log(income),
    color=scenario
  )
) +
  geom_line() +
  scale_color_manual(
    name="",
    values=tmpcolors
  ) +
  ylab(
    'Income (Log)\n'
  ) +
  xlab(
    '\nWithin-Race Income Percentile'
  ) +
  facet_wrap(
    ~ race
  ) +
  theme_bw() 


setwd(outputdir)
ggsave(
  plot=g.tmp, 
  filename="fig_4class_counterfactual_incomes.png",
  width=8,
  height=6
)

