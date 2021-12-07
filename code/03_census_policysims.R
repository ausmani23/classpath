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
require(DescTools)

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

#quick function to outputdfs
output <- function(df,tmpname) {
  setwd(outputdir)
  if( str_detect(tmpname,"\\.pdf$|\\.png$") ) 
    tmpname<-str_replace(tmpname,"\\.pdf$|\\.png$",".csv")
  write.csv(
    df,
    tmpname,
    row.names=F
  )
}


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

#REPARATIONS (MEDIAN)

#in this, we close the median gap

#median gap between blacks/whites
mwage_w <- incdf[race==1,median(income)]
mwage_b <- incdf[race==2,median(income)]

#we need to close this gap
mediangap <- mwage_w - mwage_b
wbratio <- nrow(incdf[race==1])/nrow(incdf[race==2])

#we take x/wbratio from whites to give x to blacks
#x + x/wbratio = mediangap
#solve for x
reparations_gain <- (mediangap * wbratio)/(1 + wbratio)
reparations_loss <- -1 * reparations_gain / wbratio
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

# #REPARATIONS (VARIABLE ACROSS GAP)
# 
# #in this, we close each gap
# #to do this, we round within-race to the nearest 2.5th percentile
# incdf[,income_q_race2 := floor(income_q_race/2.5)*2.5]
# 
# #put the white people in bottom groups
# #in the same gruop as bottom-most black people
# minrank <- min(incdf[,income_q_race2[race==2]])
# incdf[race==1 & income_q_race2<=minrank,income_q_race2:=minrank]
# 
# #we calculate the racial gap in each group
# incdf[
#   ,
#   meangap := mean(income[race==1]) - mean(income[race==2])
#   ,
#   by=c('income_q_race2')
# ]
# 
# #now, we redistribute within each decile
# #the amount sufficient to close that gap
# incdf[
#   ,
#   wbratio := sum(race==1)/sum(race==2)
#   ,
#   by=c('income_q_race2')
# ]
# 
# incdf[,reparations_tax := (meangap * wbratio)/(1 + wbratio)]
# incdf[race==1,reparations_tax:= -1 * reparations_tax / wbratio]
# # 
# # #groups with no black people
# # incdf[is.nan(reparations_tax),reparations_tax:=0] 
# 
# #check to make sure we are not taxing people 
# #beyond amount that takes them to bottomcode
# incdf[
#   income + reparations_tax<bottomcode,
#   reparations_tax := bottomcode - income
# ]
# incdf[,income_reparations := income + reparations_tax]
# 
# #check that the gaps are closed
# gapdf<-incdf[,.(gap=mean(income_reparations)),by=c('race','income_q_race2')]
# gapdf<-spread(gapdf,race,gap) 
# gapdf$gap<-round(gapdf$`1`-gapdf$`2`)
# gapdf
# 
# #check that the overall gap is closed
# round(
#   incdf[,mean(income_reparations[race==1])] -
#   incdf[,mean(income_reparations[race==2])]
# )
# #yes, basically
# 
# #and is overall amount of money preserved?
# round(
#   incdf[,mean(income)] -
#   incdf[,mean(income_reparations)]
# )
# #yes, basically
# 
# # #rank everyone by interest in this proposal
# # #(i.e., what they gain in welfare) 
# incdf[,welfare_reparations:=log(income_reparations)-log(income)]

#########################################################
#########################################################

#REDISTRIBUTIVE REPARATIONS TO CLOSE MEDIAN

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

#what about gap at bottom/top?
quantile(incdf$income_reparations2[incdf$race==1],0.15)
quantile(incdf$income_reparations2[incdf$race==2],0.15)
quantile(incdf$income_reparations2[incdf$race==1],0.85)
quantile(incdf$income_reparations2[incdf$race==2],0.85)

#what about mean gap?
mean(incdf$income_reparations2[incdf$race==1]) -
  mean(incdf$income_reparations2[incdf$race==2])
mean(incdf$income[incdf$race==1]) -
  mean(incdf$income[incdf$race==2])
#went from about 30k to 10k
#D+W propose to close this (see loc 5359), but this has some v strange implications
#either it is distributed to blacks according to current income distribution,
#inw hich case rich blacks are the big beneficiaries of reparations
#or it is distributed in a more egalitarian wy, in which case
#poor blacks would become substantially richer than poor whites,
#transforming the black income distribution..
#one seems implausbiel, the other is laughbly inegalitarian
#we don't discuss this in the piece, but it raises an important point:
#there is no one black-white gap which you might minimize..

#these look funky; b/c gap varies,
#flat amount does different things at different points

#how is redistribution done
quantile(
  incdf$reparations2_transfer[incdf$race==2] +
    incdf$reparations2_tax[incdf$race==2],
  c(0,0.15,0.5,0.85,1)
)

#how poor is the poorest black person, post reparations
minblackincome <- min(incdf[,income_reparations2[race==2]])
min(incdf[race==1 & income_reparations2>minblackincome,income_q_race])
min(incdf[race==1 & income_reparations2>minblackincome,income_q_race])
#this is about the 20th percentile black person


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
Gini(incdf$income) #0.516
Gini(incdf$income_reparations) #0.48
Gini(incdf$income_redistribution) #0.258 (exactly half)

#racial gap before, after
median(incdf[race==1,income]) - median(incdf[race==2,income]) #$20,000
median(incdf[race==1,income_reparations]) - median(incdf[race==2,income_reparations]) #-$111
median(incdf[race==1,income_redistribution]) - median(incdf[race==2,income_redistribution]) #$10,000

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
) %>% data.table
sumdf$scenario<-str_replace(sumdf$scenario,"welfare\\_","")

#########################################################
#########################################################

#PLOT PREP

#b/c of bottom-coding in data and manually, 
#the bottom X% are shown in one point at the plot (at X)
#for visual reasons, we will want a horizontal line..
extradf<-expand.grid(
  income_q_race=0,
  scenario=unique(sumdf$scenario),
  race=unique(sumdf$race),
  stringsAsFactors=F
)
sumdf<-rbind.fill(
  extradf,
  sumdf
) %>% data.table
setorder(sumdf,scenario,race,-income_q_race)
sumdf$welfare<-na.locf(sumdf$welfare)

#generate factors
sumdf$scenario <- factor(
  sumdf$scenario,
  levels=c('reparations','redistribution','comparison'),
  labels=c('Reparations','Redistribution','Reparations vs. Redistribution')
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


#########################################################
#########################################################

#THREE SEPARATE PLOTS

for (mylevel in levels(sumdf$scenario)) {
  
  #mylevel <- levels(sumdf$scenario)[3]
  myfilename <- paste0(str_extract_all(mylevel,"[A-z]+")[[1]],collapse="") %>% 
    tolower
  
  if(mylevel=="Reparations vs. Redistribution") {
    mylabels<-c(
      "Strongly Prefer Redistribution",
      "Prefer Redistribution",
      "Neutral",
      "Prefer Reparations",
      "Strongly Prefer Reparations"
    )
  } else {
    mylabels<-c(
      "Strongly Oppose",
      "Oppose",
      "Neutral",
      "Support",
      "Strongly Support"
    )
  }
  
  plotdf<-sumdf[scenario==mylevel]
  
  g.tmp<-ggplot(
    plotdf,
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
    xlab("\nWithin-Race Income Rank") +
    ylab("") +
    #bound it by max/min of welfare
    scale_y_continuous(
      breaks=c(-3.55,-1.775,0,1.775,3.55),
      labels=mylabels,
      limits=c(-3.55,3.55)
    ) + 
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
  tmpname<-paste0("fig_",myfilename,".pdf")
  output(plotdf,tmpname)
  ggsave(
    plot=g.tmp,
    filename=tmpname,
    width=6,
    height=4
  )
  
  
}

#########################################################
#########################################################

#DEPRECATED

# #SINGLE PLOT
# 
# g.tmp<-ggplot(
#   sumdf,
#   aes(
#     x=income_q_race,
#     y=welfare,
#     color=race,
#     group=race
#   )
# ) +
#   geom_line(
#     size=1
#   ) +
#   geom_hline(
#     yintercept=0,
#     linetype='dashed'
#   ) +
#   scale_color_manual(
#     name="",
#     values=tmpcolors
#   ) +
#   xlab("\nWithin-Race Income Rank (Percentile)") +
#   ylab("+ implies support, - implies oppose") +
#   facet_wrap(
#     ~ scenario,
#     ncol=1
#   ) + 
#   theme_bw() + 
#   theme(
#     legend.position = 'top',
#     legend.direction = 'horizontal'
#   )
# 
# setwd(outputdir)
# ggsave(
#   plot=g.tmp,
#   filename="fig_policy_counterfactuals.pdf",
#   width=5,
#   height=10
# )

#########################################################
#########################################################

#DEPRECATED
# #ADD RACE/CLASS
# 
# popdf<-merge(
#   incdf[,.(num=.N),by=c('race','class')],
#   incdf[,.(denom=.N),by=c('class')]
#   )
# popdf$pct <- round(100 * popdf$num/popdf$denom)
# popdf$num <- popdf$denom <- NULL
# popdf <- by(popdf,popdf$class,function(df) {
#   #df<-popdf[class==1,]
#   tmpdf<-data.frame(
#     race=c(rep(2,df$pct[df$race==2]),rep(1,df$pct[df$race==1]))
#   )
#   tmpdf$row<-1:nrow(tmpdf)
#   tmpdf$class<-unique(df$class)
#   tmpdf
# }) %>% rbind.fill %>% data.table
# tmpdf<-expand.grid(
#   x=1:10,
#   y=1:10
# )
# tmpdf$row <- 1:nrow(tmpdf)
# popdf<-merge(popdf,tmpdf) %>% data.table
# 
# #loop through this and summarize this in
# #X bits, where X is the share of the class in this race
# sumdf <- by(incdf,list(incdf$race,incdf$class),function(df) {
#   
#   #df <- incdf[incdf$race==1 & incdf$class==1,]
#   print(unique(df$race))
#   print(unique(df$class))
#   
#   #how many numbers do i have to summarize this?
#   tmp<-popdf$race==unique(df$race) & popdf$class==unique(df$class)
#   members_class<-sum(tmp)
#   tmpseq<-c(1:100)/100
#   #tmpseq[tmpseq==1]<-0.99 #b/c of crazy tails
#   rows<-popdf$row[tmp]
#   #use the median of every group to summarize
#   tmpseq<-sapply(split(tmpseq,cut(tmpseq,members_class)),median)
#   tmpdf<-incdf[
#     race==unique(df$race) & class==unique(df$class)
#     ,
#     .(
#       race=unique(df$race),
#       class=unique(df$class),
#       income_q = 100 * tmpseq,
#       income = quantile(income,tmpseq) %>% unname,
#       income_redistribution = quantile(income_redistribution,tmpseq) %>% unname,
#       income_reparations = quantile(income_reparations,tmpseq) %>% unname,
#       row = rows
#     )
#   ]
#   
# }) %>% rbind.fill %>% data.table
# 
# plotdf <- merge(
#   sumdf,
#   popdf,
#   by=c('race','class','row')
# )
# 
# # plotdf <- spread(
# #   plotdf,
# #   scenario,
# #   income
# # )
# plotdf$net_chg <- log(plotdf$income_reparations) - log(plotdf$income_redistribution)
# plotdf$redistribution_chg <- log(plotdf$income_redistribution) - log(plotdf$income)
# plotdf$reparations_chg <- log(plotdf$income_reparations) - log(plotdf$income)
# 
# tmpvars<-c(
#   'x',
#   'y',
#   'race',
#   'class',
#   'income_q',
#   'net_chg',
#   'redistribution_chg',
#   'reparations_chg'
# )
# plotdf<-plotdf[,tmpvars,with=F]
# 
# plotdf <- gather(
#   plotdf,
#   counterfactual,
#   gain,
#   net_chg:reparations_chg
# ) %>% data.table
# 
# plotdf$support <- plotdf$gain>0
# plotdf$shape[plotdf$race==1]<-"w"
# plotdf$shape[!plotdf$race==1]<-"b"
# 
# 
# plotdf$support<-factor(
#   plotdf$support,
#   levels=c(T,F),
#   labels=c('Support','Oppose')
# )
# tmpcolors<-c(
#   'Support'='darkgreen',
#   'Oppose'='white'
# )
# 
# 
# tmplevels<-c(
#   'reparations_chg',
#   'redistribution_chg',
#   'net_chg'
# )
# tmplabels<-c(
#   "Reparations",
#   "Redistribution",
#   "Net Preference"
# )
# plotdf$counterfactual <- factor(
#   plotdf$counterfactual,
#   tmplevels,
#   tmplabels
# )
# 
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
# 
# 
# g.tmp <- ggplot(
#   plotdf,
#   aes(
#     x=x,
#     y=y,
#     fill=support,
#     label=shape
#   )
# ) +
#   geom_tile() +
#   geom_text(
#     size=2
#   ) +
#   scale_fill_manual(
#     name="",
#     values=tmpcolors
#   ) +
#   facet_grid(
#     class ~ counterfactual
#   ) +
#   xlab("") +
#   ylab("") +
#   theme_bw() +
#   theme(
#     panel.grid = element_blank(),
#     axis.text = element_blank(),
#     axis.ticks = element_blank()
#   )
# 
# setwd(outputdir)
# ggsave(
#   plot=g.tmp, 
#   filename="fig_policy_counterfactual_support_byclass.pdf",
#   width=8,
#   height=6
# )




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
#   filename="fig_welfarecomparison_table.pdf",
#   width=8,
#   height=3
# )
# 
# 
# #########################################################
# #########################################################
