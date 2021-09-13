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

#save out
setwd(datadir); dir()
incomedf<-fread(
  "censusdf.csv"
)

#set a bottomcode
bottomcode<-1000

#########################################################
#########################################################

require(DescTools)
# Gini(
#   incomedf$inctot_f,
#   incomedf$weight_f
# )
# Gini(
#   incomedf$inctot_f[incomedf$race_m==2],
#   incomedf$weight_f[incomedf$race_m==2]
# )
# Gini(
#   incomedf$inctot_f[incomedf$race_m==1],
#   incomedf$weight_f[incomedf$race_m==1]
# )

#########################################################
#########################################################

# PDF/PMF/BINNED PROOPORTIONS

incomedf[,income_bin := (20000 * floor(inctot_f/20000))/1000]
incomedf[income_bin >= 140, income_bin:= 140]

plotdf<-incomedf[
  ,
  .(
    white=sum(weight_f[race_m==1]),
    black=sum(weight_f[race_m==2])
  )
  ,
  by=c('income_bin')
]
plotdf<-gather(
  plotdf,
  race_m,
  total,
  white:black
) %>% data.table
tmpdf<-plotdf[,.(overall=sum(total)),by='race_m']
plotdf<-merge(plotdf,tmpdf)
plotdf$percent <- plotdf$total/plotdf$overall

plotdf$race_m<-factor(
  plotdf$race_m,
  levels=c('black','white'),
  labels=c('Black','White')
)
tmpcolors<-c('blue','red')
names(tmpcolors)<-levels(plotdf$race_m)


tmplevels<-c(plotdf$income_bin) %>% unique %>% sort
tmp<-sprintf("$%.0fk",tmplevels)
tmp2<-c(tmp[-1],"+")
tmplabels<-paste0(tmp," to ",tmp2)

plotdf$income_bin<-factor(
  plotdf$income_bin,
  tmplevels,
  tmplabels
)

g.tmp <- ggplot(
  plotdf,
  aes(
    x=income_bin,
    y=percent,
    fill=race_m
  )
) +
  geom_bar(
    width=0.5,
    stat='identity',
    position='dodge'
  ) +
  scale_fill_manual(
    name="",
    values=tmpcolors
  ) + 
  theme_bw() +
  xlab("\nIncome Bin") +
  ylab("Proportion\n")

setwd(outputdir)
ggsave(
  filename="fig_pmf.png",
  plot=g.tmp,
  width=10,
  height=5
)


#########################################################
#########################################################

#QUANTILE PLOT BY RACE
tmpseq<-c(1:10000)/10000
plotdf <- incomedf[
  ,
  .(
    income = quantile(inctot_f,tmpseq) %>% unname,
    income_q = 100 * tmpseq
  )
  ,
  by=c('race_m')
]

#only plot the unique lowest income in each raceXclass grouping
plotdf<-by(plotdf,plotdf$race_m,function(df) {
  
  #df<-plotdf[plotdf$race_m==1,]
  tmp<-df$income==min(df$income)
  dropthreshold <- max(df$income_q[tmp])
  df[df$income_q>=dropthreshold,]
  
}) %>% rbind.fill %>% data.table

plotdf$race_m<-factor(
  plotdf$race_m,
  levels=c(2,1),
  labels=c('Black','White')
)
tmpcolors<-c('blue','red')
names(tmpcolors)<-levels(plotdf$race_m)

#illustrate a percentile
tmpdf<-by(plotdf,plotdf$race_m,function(tmpdf) {
  tmpdf$tmp<-tmpdf$income_q - 15
  tmpdf$qh<-abs(tmpdf$tmp)==min(abs(tmpdf$tmp))
  tmpdf<-tmpdf[qh==T]
  tmpdf<-gather(
    tmpdf,
    var,
    val,
    qh
  ) %>% data.table
  tmpdf[val==T]
}) %>% rbind.fill
gapdf<-tmpdf

#gap in % terms
(gapdf$income[gapdf$race_m=="White"] - gapdf$income[gapdf$race_m=="Black"])/
  gapdf$income[gapdf$race_m=="White"]

#illustrate rich and poor blacks
tmpdf2<-plotdf[race_m=='Black' & income_q%in%c(15,85)]

#gap in % terms
(tmpdf2$income[tmpdf2$income_q==85] - tmpdf2$income[tmpdf2$income_q==15])/
  tmpdf2$income[tmpdf2$income_q==85]


#make the graph
g.tmp <- ggplot(
  plotdf,
  aes(
    x=income_q,
    y=income,
    color=race_m
  )
) + 
  geom_line(
    size=2
  ) +
  scale_color_manual(
    name="",
    values=tmpcolors
  ) + 
  #highlight median gap
  geom_point(
    data=tmpdf,
    color='grey',
    size=4
  ) +
  geom_line(
    data=tmpdf,
    linetype='solid',
    color='black',
    size=1
  ) +
  #highlight 10th,90th percentile black
  geom_point(
    data=tmpdf2,
    color='grey',
    size=4
  ) +
  geom_hline(
    yintercept=tmpdf2$income,
    linetype='dashed'
  ) +
  #this governs the tick marks
  scale_y_log10(
    breaks = 10^(3:6),
    labels = c("$1,000 (and below)","$10,000","$100,000","$1,000,000")
  ) +
  theme_bw() +
  ylab("Income\n") +
  xlab("\nWithin-Race Quantile") + 
  theme(
    legend.position='top',
    legend.direction='horizontal'
  )

setwd(outputdir)
ggsave(
  filename="fig_quantileplot.png",
  plot=g.tmp,
  width=8,
  height=6
)

#########################################################
#########################################################

#PLOT PROPORTIONS OF EACH BY CLASS

plotdf<-incomedf[
  ,
  .(
    pop = sum(weight_f)
  )
  ,
  by=c('race_m','class_f')
]
tmpdf<-plotdf[,sum(pop),by=c('class_f')]
plotdf<-merge(plotdf,tmpdf)
plotdf$pct_class<-round(100 * plotdf$pop/plotdf$V1)
plotdf$V1<-NULL
plotdf$labelpos<-plotdf$pct_class
plotdf$labelpos[plotdf$race_m==2]<-100

plotdf$race_m<-factor(
  plotdf$race_m,
  levels=c(2,1),
  labels=c('Black','White')
)
tmpcolors<-c('blue','red')
names(tmpcolors)<-levels(plotdf$race_m)

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

#generate a visualization
g.tmp<-ggplot(
  plotdf,
  aes(
    x=class_f,
    y=pct_class,
    fill=race_m
  )
) +
  geom_bar(
    stat='identity',
    color='black',
    width=1
  ) +
  geom_text(
    aes(
      y=labelpos-5,
      label=paste0(pct_class,"%")
    ),
    vjust=1,
    color='white',
    size=5
  ) +
  scale_fill_manual(
    name="",
    values=tmpcolors
  ) +
  coord_flip() +
  xlab("") +
  ylab("\n% of Class of Given Race") +
  theme_bw() +
  theme(
    panel.grid = element_blank()
  )

setwd(outputdir)
ggsave(
  filename="fig_proportions.png",
  plot=g.tmp,
  width=8,
  height=4
)

#########################################################
#########################################################

#QUANTILE PLOT BY CLASS
tmpseq<-c(1:10000)/10000
plotdf <- incomedf[
  ,
  .(
    income = quantile(inctot_f,tmpseq) %>% unname,
    income_q = 100 * tmpseq
  )
  ,
  by=c('class_f')
]

#only plot the unique lowest income in each raceXclass grouping
plotdf<-by(plotdf,plotdf$class_f,function(df) {
  
  #df<-plotdf[plotdf$race_m==1,]
  tmp<-df$income==min(df$income)
  dropthreshold <- max(df$income_q[tmp])
  df[df$income_q>=dropthreshold,]
  
}) %>% rbind.fill %>% data.table

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

# #illustrate a percentile
# tmpdf<-by(plotdf,plotdf$race_m,function(tmpdf) {
#   tmpdf$tmp<-tmpdf$income_q - 10
#   tmpdf$qh<-abs(tmpdf$tmp)==min(abs(tmpdf$tmp))
#   tmpdf<-tmpdf[qh==T]
#   tmpdf<-gather(
#     tmpdf,
#     var,
#     val,
#     qh
#   ) %>% data.table
#   tmpdf[val==T]
# }) %>% rbind.fill
# gapdf<-tmpdf

# #illustrate rich and poor blacks
# tmpdf2<-plotdf[race_m=='Black' & income_q%in%c(10,90)]


#make the graph
g.tmp <- ggplot(
  plotdf,
  aes(
    x=income_q,
    y=income,
    color=class_f
  )
) + 
  geom_line(
    size=2
  ) +
  scale_color_discrete(
    name=""
  ) +
  #highlight median gap
  # geom_point(
  #   data=tmpdf,
  #   color='black',
  #   size=2
  # ) +
  # geom_line(
  #   data=tmpdf,
  #   linetype='solid',
  #   color='black',
  #   size=0.5
  # ) +
  # #highlight 10th,90th percentile black
  # geom_point(
  #   data=tmpdf2,
  #   color='black',
  #   size=2
  # ) +
  # geom_hline(
  #   yintercept=tmpdf2$income,
  #   linetype='dashed'
  # ) +
  #this governs the tick marks
  scale_y_log10(
    breaks = 10^(3:6),
    labels = c("$1,000 (and below)","$10,000","$100,000","$1,000,000")
  ) +
  theme_bw() +
  ylab("Income\n") +
  xlab("\nWithin-Class Quantile") + 
  theme(
    legend.position='top',
    legend.direction='horizontal'
  )

setwd(outputdir)
ggsave(
  filename="fig_quantileplot_class.png",
  plot=g.tmp,
  width=8,
  height=6
)

#########################################################
#########################################################

# # QUANTILE PLOT BY CLASS AND RACE
# 
# #we can't plot 4 * 10^6 obs.
# #but we want to plot the distribution
# 
# #to do this, I want to get race-specific quantiles
# #and the income that corresponds. 
# tmpseq<-c(1:10000)/10000
# plotdf <- incomedf[
#   ,
#   .(
#     income = quantile(inctot_f,tmpseq) %>% unname,
#     income_q = 100 * tmpseq
#   )
#   ,
#   by=c('race_m','class_f')
# ]
# 
# #only plot the unique lowest income in each raceXclass grouping
# plotdf<-by(plotdf,list(plotdf$race_m,plotdf$class_f),function(df) {
#   
#   #df<-plotdf[plotdf$race_m==1,]
#   tmp<-df$income==min(df$income)
#   dropthreshold <- max(df$income_q[tmp])
#   df[df$income_q>=dropthreshold,]
#   
# }) %>% rbind.fill %>% data.table
# 
# #plotdf<-plotdf[income>0,]
# plotdf$race_m<-factor(
#   plotdf$race_m,
#   levels=c(2,1),
#   labels=c('Black','White')
# )
# tmpcolors<-c('blue','red')
# names(tmpcolors)<-levels(plotdf$race_m)
# 
# plotdf$class_f<-factor(
#   plotdf$class_f,
#   levels=c(1,2,3,4),
#   labels=c(
#     'Reserve Army',
#     'Working-Class',
#     'Professionals',
#     'Capitalists'
#   )
# )
# 
# #illustrate the 50th percentile
# tmpdf<-by(plotdf,list(plotdf$race_m,plotdf$class_f),function(tmpdf) {
#   tmpdf$tmp<-tmpdf$income_q - 50
#   tmpdf$q50<-abs(tmpdf$tmp)==min(abs(tmpdf$tmp))
#   tmpdf<-tmpdf[q50==T]
#   tmpdf<-gather(
#     tmpdf,
#     var,
#     val,
#     q50
#   ) %>% data.table
#   tmpdf[val==T]
# }) %>% rbind.fill
# gapdf<-tmpdf
# 
# #make the graph
# g.tmp<-ggplot(
#   plotdf,
#   aes(
#     x=income,
#     y=income_q,
#     color=race_m
#   )
# ) + 
#   geom_line(
#     size=2
#   ) +
#   scale_color_manual(
#     name="",
#     values=tmpcolors
#   ) + 
#   geom_point(
#     data=tmpdf,
#     color='black',
#     size=2
#   ) +
#   geom_line(
#     data=tmpdf,
#     linetype='dashed',
#     color='black',
#     size=0.5
#   ) +
#   facet_wrap(
#     ~ class_f
#   ) +
#   #this governs the tick marks
#   scale_x_log10(
#     breaks = 10^(3:6),
#     labels = c("$1,000 (and below)","$10,000","$100,000","$1,000,000")
#   ) +
#   theme_bw() +
#   xlab("\nIncome") +
#   ylab("Within-Race Quantile\n") + 
#   theme(
#     legend.position='top',
#     legend.direction='horizontal'
#   )
# 
# setwd(outputdir)
# ggsave(
#   filename="fig_cdfs_raceXclass.png",
#   plot=g.tmp,
#   width=8,
#   height=6
# )

#########################################################
#########################################################

#PDFs

# plotdf <- incomedf[
#   ,
#   .(
#     income=density(inctot_f,weights=weight_f/sum(weight_f))$x,
#     density=density(inctot_f,weights=weight_f/sum(weight_f))$y
#   )
#   ,
#   by=c(
#     'race_m',
#     'class_f'
#   )
# ]
# 
# plotdf<-plotdf[income>=bottomcode,]
# 
# 
# plotdf$race_m<-factor(
#   plotdf$race_m,
#   levels=c(2,1),
#   labels=c('Black','White')
# )
# tmpcolors<-c('blue','red')
# names(tmpcolors)<-levels(plotdf$race_m)
# 
# plotdf$class_f<-factor(
#   plotdf$class_f,
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
#   plotdf,
#   aes(
#     x=income,
#     y=density,
#     color=race_m,
#     group=race_m
#   )
# ) +
#   geom_line() +
#   facet_wrap(
#     ~ class_f,
#     ncol=1
#   ) +
#   geom_line(
#     size=1
#   ) +
#   scale_color_manual(
#     name="",
#     values=tmpcolors
#   ) +   
#   scale_x_log10(
#     breaks = 10^(3:6),
#     labels = c("$1,000 (and below)","$10,000","$100,000","$1,000,000")
#   ) +
#   theme_bw()
# 
# 
# setwd(outputdir)
# ggsave(
#   filename="fig_pdfs_raceXclass.png",
#   plot=g.tmp,
#   width=8,
#   height=6
# )

#########################################################
#########################################################

#output useful info for the simulation

#two-way table
tmptable<-questionr::wtd.table(
  incomedf$class_f,
  incomedf$race_m,
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
    race_m='White',
    proportion_race=whites_classprops
  ),
  data.frame(
    class_f=tmpnames,
    race_m='Black',
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
    'race_m',
    'class_f'
  )
]
tmpdf2$race_m<-factor(
  tmpdf2$race_m,
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

