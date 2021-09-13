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
incdf<-incomedf[year==2019,]

#we use weight_f to simulate.. 
incdf<-data.frame(
  income=rep(incdf$inctot_f,incdf$weight_f),
  race=rep(incdf$race_m,incdf$weight_f),
  class=rep(incdf$class_f,incdf$weight_f)
) %>% data.table

#calculate overall, and white/black gini
Gini(incdf$income)
Gini(incdf$income[incdf$race==1])
Gini(incdf$income[incdf$race==2])

#########################################################
#########################################################

#get quantiles

#this gets percentile
tmpf<-ecdf(incdf$income)
incdf$income_q<-round(100 * tmpf(incdf$income))
tmpf<-NULL

#also generate race-specific percentile
tmpfunction<-function(x) {
  tmpf<-ecdf(x)
  100 * tmpf(x)
}
incdf[
  ,
  income_q_race := tmpfunction(income)
  ,
  by=c('race')
]


#########################################################
#########################################################

#REPARATIONS

# #median gap between blacks/whites
# mwage_w <- incdf[race==1,median(income)]
# mwage_b <- incdf[race==2,median(income)]
# 
# #we need to close this gap
# mediangap <- mwage_w - mwage_b
# wbratio <- nrow(incdf[race==1])/nrow(incdf[race==2])
# 
# #we take x/wbratio from whites to give x to blacks
# #x + x/wbratio = mediangap
# #solve for x
# reparations_gain <- (mediangap * wbratio)/(1 + wbratio)
# reparations_loss <- -1 * reparations_gain / wbratio
# print(reparations_gain); print(reparations_loss)
# incdf[race==1, reparations_tax:=reparations_loss]
# incdf[race==2, reparations_tax:=reparations_gain]
# # incdf$reparations_tax[incdf$race==1] <- reparations_loss
# # incdf$reparations_tax[incdf$race==2] <- reparations_gain
# #transfer the money 
# #(except, if you don't have very much, we will only take as much
# #as will leave you with bottomcode amount)
# incdf[
#   income + reparations_tax<bottomcode,
#   reparations_tax := bottomcode - income
# ]
# incdf[,income_reparations := income + reparations_tax]
# 
# #check that the median gap is closed (yup)
# incdf[,median(income_reparations),by='race']
# 
# #rank everyone by interest in this proposal
# #(i.e., what they gain in welfare) 
# incdf[,welfare_reparations:=log(income_reparations)-log(income)]

#########################################################
#########################################################

#REDISTRIBUTIVE REPARATIONS

#some people say reparations itself would be redistributive
#so let's implement a redistributive tax (take x% from everyone)
#and redistribute this to black people, specifically.. 

#implement progressive tax
#we calibrate a taxrate so that this makes median incomes equal.. 
incdf[, reparations2_taxrate := 1/100 * (income_q/16)] 
incdf[, reparations2_tax := -1 * (income * reparations2_taxrate)]
reparations2_gain <- sum(-1 * incdf$reparations2_tax)/sum(incdf$race==2)
incdf[race==1, reparations2_transfer := 0] 
incdf[race==2, reparations2_transfer := reparations2_gain]
incdf[,income_reparations2 := income + reparations2_tax + reparations2_transfer]
incdf[,welfare_reparations2 := log(income_reparations2) - log(income)]

#check that the median gap is closed?
median(incdf$income_reparations2[incdf$race==1])
median(incdf$income_reparations2[incdf$race==2])

#use redistributive version of reparations
incdf[,income_reparations := income_reparations2]
incdf[,welfare_reparations := welfare_reparations2]

#########################################################
#########################################################

#REDISTRIBUTION

#split the pie more equally
#we do this by shrinking everyone towards the mean
#and do it so that the gini is not so dramatic..

avgincome <- mean(incdf$income)
shrinkage <- 0.5 #shrinkage factor, used to calibrate how much redistribution
incdf[,redistribution_tax := (avgincome - income) * shrinkage]
incdf[,income_redistribution := income + redistribution_tax]
incdf[,welfare_redistribution := log(income_redistribution) - log(income)]

#gini before, after
#require(DescTools)
Gini(incdf$income) #0.51
Gini(incdf$income_reparations) #0.48
Gini(incdf$income_redistribution) #0.26

#########################################################
#########################################################

#REPARATIONS VS. REDISTRIBUTION
incdf[,welfare_comparison := welfare_reparations - welfare_redistribution]

#########################################################
#########################################################

#BY RACE-SPECIFIC PERCENTILE

sumdf<-incdf[
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
  ylab("+ implies support, - implies oppose") +
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
  filename="fig_policy_counterfactuals.png",
  width=5,
  height=10
)

#########################################################
#########################################################

#ADD RACE/CLASS

popdf<-merge(
  incdf[,.(num=.N),by=c('race','class')],
  incdf[,.(denom=.N),by=c('class')]
  )
popdf$pct <- round(100 * popdf$num/popdf$denom)
popdf$num <- popdf$denom <- NULL
popdf <- by(popdf,popdf$class,function(df) {
  #df<-popdf[class==1,]
  tmpdf<-data.frame(
    race=c(rep(2,df$pct[df$race==2]),rep(1,df$pct[df$race==1]))
  )
  tmpdf$row<-1:nrow(tmpdf)
  tmpdf$class<-unique(df$class)
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
sumdf <- by(incdf,list(incdf$race,incdf$class),function(df) {
  
  #df <- incdf[incdf$race==1 & incdf$class==1,]
  print(unique(df$race))
  print(unique(df$class))
  
  #how many numbers do i have to summarize this?
  tmp<-popdf$race==unique(df$race) & popdf$class==unique(df$class)
  members_class<-sum(tmp)
  tmpseq<-c(1:100)/100
  #tmpseq[tmpseq==1]<-0.99 #b/c of crazy tails
  rows<-popdf$row[tmp]
  #use the median of every group to summarize
  tmpseq<-sapply(split(tmpseq,cut(tmpseq,members_class)),median)
  tmpdf<-incdf[
    race==unique(df$race) & class==unique(df$class)
    ,
    .(
      race=unique(df$race),
      class=unique(df$class),
      income_q = 100 * tmpseq,
      income = quantile(income,tmpseq) %>% unname,
      income_redistribution = quantile(income_redistribution,tmpseq) %>% unname,
      income_reparations = quantile(income_reparations,tmpseq) %>% unname,
      row = rows
    )
  ]
  
}) %>% rbind.fill %>% data.table

plotdf <- merge(
  sumdf,
  popdf,
  by=c('race','class','row')
)

# plotdf <- spread(
#   plotdf,
#   scenario,
#   income
# )
plotdf$net_chg <- log(plotdf$income_reparations) - log(plotdf$income_redistribution)
plotdf$redistribution_chg <- log(plotdf$income_redistribution) - log(plotdf$income)
plotdf$reparations_chg <- log(plotdf$income_reparations) - log(plotdf$income)

tmpvars<-c(
  'x',
  'y',
  'race',
  'class',
  'income_q',
  'net_chg',
  'redistribution_chg',
  'reparations_chg'
)
plotdf<-plotdf[,tmpvars,with=F]

plotdf <- gather(
  plotdf,
  counterfactual,
  gain,
  net_chg:reparations_chg
) %>% data.table

plotdf$support <- plotdf$gain>0
plotdf$shape[plotdf$race==1]<-"w"
plotdf$shape[!plotdf$race==1]<-"b"


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
  'reparations_chg',
  'redistribution_chg',
  'net_chg'
)
tmplabels<-c(
  "Reparations",
  "Redistribution",
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
  filename="fig_policy_counterfactual_support_byclass.png",
  width=8,
  height=6
)




# #########################################################
# #########################################################
# 
# #BY RACE AND CLASS
# 
# 
# #show preferences across the three scenarios
# 
# sumdf<-tmpdf[
#   ,
#   .(
#     welfare_redistribution=median(welfare_redistribution),
#     welfare_reparations=median(welfare_reparations),
#     welfare_comparison=median(welfare_comparison)
#   )
#   ,
#   by=c(
#     'race',
#     'class'
#   )
# ]
# 
# sumdf<-gather(
#   sumdf,
#   scenario,
#   welfare,
#   welfare_redistribution:welfare_comparison
# )
# sumdf$scenario<-str_replace(sumdf$scenario,"welfare\\_","")
# 
# sumdf$scenario <- factor(
#   sumdf$scenario,
#   levels=c('reparations','redistribution','comparison'),
#   labels=c('Reparations','Redistribution','Net Comparison')
# )
# 
# 
# sumdf$race<-factor(
#   sumdf$race,
#   levels=c(2,1),
#   labels=c('Black','White')
# )
# 
# sumdf$class<-factor(
#   sumdf$class,
#   levels=c(1,2,3,4),
#   labels=c(
#     'Reserve Army',
#     'Working-Class',
#     'Professionals',
#     'Capitalists'
#   )
# )
# 
# g.tmp<-ggplot(
#   sumdf,
#   aes(
#     x=race,
#     y=class,
#     fill=welfare,
#     label=format(round(welfare,2),nsmall=2)
#   )
# ) +
#   geom_tile(
#     width=0.9,
#     height=0.9,
#     color='black',
#     size=1.5
#   ) +
#   geom_text(
#     color='black'
#   ) +
#   scale_fill_gradient2(
#     guide=F,
#     low = "red",
#     mid = "white",
#     high = "darkgreen",
#     midpoint = 0
#   ) +
#   scale_x_discrete(position='top') +
#   xlab("") +
#   ylab("") +
#   facet_wrap(
#     ~ scenario
#   ) +
#   theme_bw() +
#   theme(
#     panel.grid.major = element_blank(),
#     panel.border = element_blank(),
#     axis.ticks = element_blank()
#   )
# 
# setwd(outputdir)
# ggsave(
#   plot=g.tmp,
#   filename="fig_welfarecomparison_table.png",
#   width=8,
#   height=3
# )
# 
# 
# #########################################################
# #########################################################
