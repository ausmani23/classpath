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

# #gini's are not realistic
# require(DescTools)
# simdf[
#   ,
#   .(gini = Gini(income))
#   ,
#   by=c(
#     'scenario'
#   )
# ]
# 
# # #truncate top and bottom incomes
# # sum(simdf$income>10^6)/nrow(simdf) #3% of people
# # simdf$income[simdf$income>10^6]<-10^6
# # sum(simdf$income<10^3)/nrow(simdf) #also 3% of people
# # simdf$income[simdf$income/10^3]<-10^3
# 
# #we can reduce it a little.. 
# simdf[
#   ,
#   .(gini = Gini(income))
#   ,
#   by=c(
#     'scenario'
#   )
# ]

#########################################################
#########################################################

#MAKE A SINGLE PLOT FOR RACE

#summarize the sim data
#b/c the tails are a little wonky
#we only plot till the 99th percentile
tmpseq<-c(1:10000)/10000
tmpseq<-tmpseq[tmpseq>=0.01 & tmpseq<=0.99]
sumdf <- simdf[
  ,
  .(
    income = quantile(income,tmpseq) %>% unname,
    income_q = 100 * tmpseq
  )
  ,
  by=c('race','scenario','demography')
]

#spread scenarios
plotdf <- spread(
  sumdf,
  scenario,
  income
)

#calculate welfare cgain
plotdf$net_chg <- log(plotdf$race_off) - log(plotdf$class_off)
plotdf$class_chg <- log(plotdf$class_off) - log(plotdf$normal)
plotdf$race_chg <- log(plotdf$race_off) - log(plotdf$normal)

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
  'demography',
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
  "Net Preference"
)
plotdf$counterfactual <- factor(
  plotdf$counterfactual,
  tmplevels,
  tmplabels
)

g.tmp <- ggplot(
  plotdf[demography=='usa',],
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
    ~ counterfactual,
    ncol=1
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
  width=6,
  height=10
)

#make an rsa version of the graph 
g.tmp <- ggplot(
  plotdf[demography=='southafrica',],
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
    ~ counterfactual,
    ncol=1
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
  filename="fig_4class_counterfactuals_southafrica.png",
  width=6,
  height=10
)


#########################################################
#########################################################

#MAKE A BY CLASS SUMMARY PLOT
#WITH RACE INDICATED

#this is a grid we will we use to visualize
popdf<-simdf[scenario=='normal',.(num=.N),by=c('demography','race','class')]
tmpdf<-simdf[scenario=='normal',.(denom=.N),by=c('demography','class')]
popdf<-merge(popdf,tmpdf)
popdf$pct <- round(100 * popdf$num/popdf$denom)
popdf$num <- popdf$denom <- NULL
popdf <- by(popdf,list(popdf$class,popdf$demography),function(df) {
  #df<-popdf[class==1,]
  tmpdf<-data.frame(
    race=c(rep("Black",df$pct[1]),rep("White",df$pct[2]))
  )
  tmpdf$row<-1:nrow(tmpdf)
  tmpdf$class<-unique(df$class)
  tmpdf$demography<-unique(df$demography)
  tmpdf
}) %>% rbind.fill %>% data.table
tmpdf<-expand.grid(
  x=1:10,
  y=1:10
)
tmpdf$row <- 1:nrow(tmpdf)
popdf<-merge(popdf,tmpdf) %>% data.table


#loop through this and summarize this in
#X bits, where X is the share of the class in this race
sumdf <- by(simdf,list(simdf$race,simdf$class,simdf$demography),function(df) {
  
  #df <- simdf[simdf$race=='Black' & simdf$class==1 & simdf$demography=='southafrica' ,]
  
  #how many numbers do i have to summarize this?
  tmp<-popdf$race==unique(df$race) & 
    popdf$class==unique(df$class) &
    popdf$demography==unique(df$demography)
  members_class<-sum(tmp)
  tmpseq<-c(1:100)/100
  tmpseq[tmpseq==1]<-0.99 #b/c of crazy tails
  rows<-popdf$row[tmp]
  #use the median of every group to summarize
  tmpseq<-sapply(split(tmpseq,cut(tmpseq,members_class)),median)
  tmpdf<-simdf[
    race==unique(df$race) & class==unique(df$class)
    ,
    .(
      race=unique(df$race),
      class=unique(df$class),
      demography=unique(df$demography),
      income = quantile(income,tmpseq) %>% unname,
      income_q = 100 * tmpseq,
      row = rows
    )
    ,
    by='scenario'
  ]
  

}) %>% rbind.fill %>% data.table

plotdf <- merge(
  sumdf,
  popdf,
  by=c('demography','race','class','row')
)

plotdf <- spread(
  plotdf,
  scenario,
  income
)
plotdf$net_chg <- log(plotdf$race_off) - log(plotdf$class_off)
plotdf$class_chg <- log(plotdf$class_off) - log(plotdf$normal)
plotdf$race_chg <- log(plotdf$race_off) - log(plotdf$normal)

tmpvars<-c(
  'x',
  'y',
  'demography',
  'race',
  'class',
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

plotdf$support <- plotdf$gain>0
plotdf$shape[plotdf$race=='Black']<-"b"
plotdf$shape[!plotdf$race=='Black']<-"w"


plotdf$support<-factor(
  plotdf$support,
  levels=c(T,F),
  labels=c('Support','Oppose')
)
tmpcolors<-c(
  'Support'='darkgreen',
  'Oppose'='white'
)


tmplevels<-c(
  'race_chg',
  'class_chg',
  'net_chg'
)
tmplabels<-c(
  "Race-Based Interventions",
  "Class-Based Intervention",
  "Net Preference"
)
plotdf$counterfactual <- factor(
  plotdf$counterfactual,
  tmplevels,
  tmplabels
)

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


g.tmp <- ggplot(
  plotdf[demography=='usa',],
  aes(
    x=x,
    y=y,
    fill=support,
    label=shape
  )
) +
  geom_tile() +
  geom_text(
    size=2
  ) +
  scale_fill_manual(
    name="",
    values=tmpcolors
  ) +
  facet_grid(
    class ~ counterfactual
  ) +
  xlab("") +
  ylab("") +
  theme_bw() +
  theme(
    panel.grid = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank()
  )

setwd(outputdir)
ggsave(
  plot=g.tmp, 
  filename="fig_4class_counterfactual_support_byclass.png",
  width=8,
  height=6
)


g.tmp <- ggplot(
  plotdf[demography=='southafrica',],
  aes(
    x=x,
    y=y,
    fill=support,
    label=shape
  )
) +
  geom_tile() +
  geom_text(
    size=2
  ) +
  scale_fill_manual(
    name="",
    values=tmpcolors
  ) +
  facet_grid(
    class ~ counterfactual
  ) +
  xlab("") +
  ylab("") +
  theme_bw() +
  theme(
    panel.grid = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank()
  )

setwd(outputdir)
ggsave(
  plot=g.tmp, 
  filename="fig_4class_counterfactual_support_byclass_southafrica.png",
  width=8,
  height=6
)

# #########################################################
# #########################################################

#make one plot w/o class, to contrast the two cases

#this is a grid we will we use to visualize
popdf<-simdf[scenario=='normal',.(num=.N),by=c('demography','race')]
tmpdf<-simdf[scenario=='normal',.(denom=.N),by=c('demography')]
popdf<-merge(popdf,tmpdf)
popdf$pct <- round(100 * popdf$num/popdf$denom)
popdf$num <- popdf$denom <- NULL
popdf <- by(popdf,list(popdf$demography),function(df) {
  #df<-popdf[class==1,]
  tmpdf<-data.frame(
    race=c(rep("Black",df$pct[1]),rep("White",df$pct[2]))
  )
  tmpdf$row<-1:nrow(tmpdf)
  tmpdf$demography<-unique(df$demography)
  tmpdf
}) %>% rbind.fill %>% data.table
tmpdf<-expand.grid(
  x=1:10,
  y=1:10
)
tmpdf$row <- 1:nrow(tmpdf)
popdf<-merge(popdf,tmpdf) %>% data.table


#loop through this and summarize this in
#X bits, where X is the share of the class in this race
sumdf <- by(simdf,list(simdf$race,simdf$demography),function(df) {
  
  #df <- simdf[simdf$race=='White' & simdf$demography=='southafrica' ,]
  
  #how many numbers do i have to summarize this?
  tmp<-popdf$race==unique(df$race) & 
    popdf$demography==unique(df$demography)
  members_class<-sum(tmp)
  tmpseq<-c(1:100)/100
  tmpseq[tmpseq==1]<-0.99 #b/c of crazy tails
  rows<-popdf$row[tmp]
  #use the median of every group to summarize
  tmpseq<-sapply(split(tmpseq,cut(tmpseq,members_class)),median)
  tmpdf<-simdf[
    race==unique(df$race)
    ,
    .(
      race=unique(df$race),
      demography=unique(df$demography),
      income = quantile(income,tmpseq) %>% unname,
      income_q = 100 * tmpseq,
      row = rows
    )
    ,
    by='scenario'
  ]
  
  
}) %>% rbind.fill %>% data.table

plotdf <- merge(
  sumdf,
  popdf,
  by=c('demography','race','row')
)

plotdf <- spread(
  plotdf,
  scenario,
  income
)
plotdf$net_chg <- log(plotdf$race_off) - log(plotdf$class_off)
plotdf$class_chg <- log(plotdf$class_off) - log(plotdf$normal)
plotdf$race_chg <- log(plotdf$race_off) - log(plotdf$normal)

tmpvars<-c(
  'x',
  'y',
  'demography',
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

plotdf$support <- plotdf$gain>0
plotdf$shape[plotdf$race=='Black']<-"b"
plotdf$shape[!plotdf$race=='Black']<-"w"


plotdf$support<-factor(
  plotdf$support,
  levels=c(T,F),
  labels=c('Support','Oppose')
)
tmpcolors<-c(
  'Support'='darkgreen',
  'Oppose'='white'
)


tmplevels<-c(
  'race_chg',
  'class_chg',
  'net_chg'
)
tmplabels<-c(
  "Race-Based Interventions",
  "Class-Based Intervention",
  "Net Preference"
)
plotdf$counterfactual <- factor(
  plotdf$counterfactual,
  tmplevels,
  tmplabels
)

tmplevels<-c(
  'usa',
  'southafrica'
)
tmplabels<-c(
  'USA',
  'South Africa'
)
plotdf$demography <- 
  factor(
    plotdf$demography,
    tmplevels,
    tmplabels
  )

g.tmp <- ggplot(
  plotdf,
  aes(
    x=x,
    y=y,
    fill=support,
    label=shape
  )
) +
  geom_tile() +
  geom_text(
    size=2
  ) +
  scale_fill_manual(
    name="",
    values=tmpcolors
  ) +
  facet_grid(
    demography ~ counterfactual
  ) +
  xlab("") +
  ylab("") +
  theme_bw() +
  theme(
    panel.grid = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank()
  )

setwd(outputdir)
ggsave(
  plot=g.tmp, 
  filename="fig_4class_counterfactual_support_bothdemographies.png",
  width=8,
  height=5
)
