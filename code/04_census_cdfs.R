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

#########################################################
#########################################################

# CDFs

#we can't plot 4 * 10^6 obs.
#but we want to plot the distribution

#to do this, I want to get race-specific quantiles
#and the income that corresponds. 
tmpseq<-c(1:10000)/10000
plotdf <- incomedf[
  ,
  .(
    income = quantile(inctot_f,tmpseq) %>% unname,
    income_q = 100 * tmpseq
  )
  ,
  by=c('race_f','class_f')
]

#only plot the unique lowest income in each raceXclass grouping
plotdf<-by(plotdf,list(plotdf$race_f,plotdf$class_f),function(df) {
  
  #df<-plotdf[plotdf$race_f==1,]
  tmp<-df$income==min(df$income)
  dropthreshold <- max(df$income_q[tmp])
  df[df$income_q>=dropthreshold,]
  
}) %>% rbind.fill %>% data.table

#plotdf<-plotdf[income>0,]
plotdf$race_f<-factor(
  plotdf$race_f,
  levels=c(2,1),
  labels=c('Black','White')
)
tmpcolors<-c('blue','red')
names(tmpcolors)<-levels(plotdf$race_f)

plotdf$class_f<-factor(
  plotdf$class_f,
  levels=c(1,2,3,4),
  labels=c(
    'Reserve Army',
    'Working-Class',
    'Professionals',
    'Capitalists'
  )
)

#illustrate the 50th percentile
tmpdf<-by(plotdf,list(plotdf$race_f,plotdf$class_f),function(tmpdf) {
  tmpdf$tmp<-tmpdf$income_q - 50
  tmpdf$q50<-abs(tmpdf$tmp)==min(abs(tmpdf$tmp))
  tmpdf<-tmpdf[q50==T]
  tmpdf<-gather(
    tmpdf,
    var,
    val,
    q50
  ) %>% data.table
  tmpdf[val==T]
}) %>% rbind.fill
gapdf<-tmpdf

#make the graph
g.tmp<-ggplot(
  plotdf,
  aes(
    x=income,
    y=income_q,
    color=race_f
  )
) + 
  geom_line(
    size=2
  ) +
  scale_color_manual(
    name="",
    values=tmpcolors
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
  facet_wrap(
    ~ class_f
  ) +
  #this governs the tick marks
  scale_x_log10(
    breaks = 10^(3:6),
    labels = c("$1,000 (and below)","$10,000","$100,000","$1,000,000")
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
  filename="fig_cdfs_raceXclass.png",
  plot=g.tmp,
  width=8,
  height=6
)

#########################################################
#########################################################

#PDFs

plotdf <- incomedf[
  ,
  .(
    income=density(inctot_f,weights=weight_f/sum(weight_f))$x,
    density=density(inctot_f,weights=weight_f/sum(weight_f))$y
  )
  ,
  by=c(
    'race_f',
    'class_f'
  )
]

plotdf<-plotdf[income>=bottomcode,]


plotdf$race_f<-factor(
  plotdf$race_f,
  levels=c(2,1),
  labels=c('Black','White')
)
tmpcolors<-c('blue','red')
names(tmpcolors)<-levels(plotdf$race_f)

plotdf$class_f<-factor(
  plotdf$class_f,
  levels=c(1,2,3,4),
  labels=c(
    'Reserve Army',
    'Working-Class',
    'Professionals',
    'Capitalists'
  )
)

g.tmp<-ggplot(
  plotdf,
  aes(
    x=income,
    y=density,
    color=race_f,
    group=race_f
  )
) +
  geom_line() +
  facet_wrap(
    ~ class_f,
    ncol=1
  ) +
  geom_line(
    size=1
  ) +
  scale_color_manual(
    name="",
    values=tmpcolors
  ) +   
  scale_x_log10(
    breaks = 10^(3:6),
    labels = c("$1,000 (and below)","$10,000","$100,000","$1,000,000")
  ) +
  theme_bw()


setwd(outputdir)
ggsave(
  filename="fig_pdfs_raceXclass.png",
  plot=g.tmp,
  width=8,
  height=6
)

#########################################################
#########################################################

#output useful info for the simulation

#two-way table
tmptable<-questionr::wtd.table(
  incomedf$class_f,
  incomedf$race_f,
  weights=incomedf$weight_f
)

#proportions of races in each class
whites_classprops <- 100 * tmptable[,1]/apply(tmptable,2,sum)[1]
blacks_classprops <- 100 * tmptable[,2]/apply(tmptable,2,sum)[2]

#add this info to gapdf
#and save out to send to Dave
tmpnames<-c(
  'Reserve Army',
  'Working-Class',
  'Professionals',
  'Capitalists'
)
tmpdf<-rbind.fill(
  data.frame(
    class_f=tmpnames,
    race_f='White',
    proportion_race=whites_classprops
  ),
  data.frame(
    class_f=tmpnames,
    race_f='Black',
    proportion_race=blacks_classprops
  )
)

#get other useufl info
tmpdf2<-incomedf[
  ,
  .(
    mean_income=mean(inctot_f),
    median_income=quantile(inctot_f,0.5)
  )
  ,
  by=c(
    'race_f',
    'class_f'
  )
]
tmpdf2$race_f<-factor(
  tmpdf2$race_f,
  levels=c(2,1),
  labels=c('Black','White')
)
tmpdf2$class_f<-factor(
  tmpdf2$class_f,
  levels=c(1,2,3,4),
  labels=c(
    'Reserve Army',
    'Working-Class',
    'Professionals',
    'Capitalists'
  )
)

infodf<-merge(
  tmpdf,
  tmpdf2
)
setwd(outputdir)
write.csv(
  infodf,
  "siminfo.csv",
  row.names=F
)

#########################################################
#########################################################

