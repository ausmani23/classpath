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

#helper funcitons
isodd<-function(x) x%%2!=0
iseven<-function(x) x%%2==0
genfw<-function(x) { #takes columns you want, and generates fwf request  
  #format has to be startcol,endcol,startcol,endcol,startcol,endcol...  
  fwf<-c()  
  for (i in 1:length(x)) {
    if (i==1) 
      fwf<-append(fwf,-(x[1]-1)) 
    if (isodd(i) & i>1) {
      if ((x[i]-x[i-1])>1) 
        fwf<-append(fwf,-(x[i]-x[i-1])+1)
    }
    if (iseven(i)) 
      fwf<-append(fwf,x[i]-x[i-1]+1)
  }
  if (fwf[1]==0) fwf<-fwf[-1] #if we're getting first col, need to do this 
  return(fwf)
}
genfw(c(1,4))

#########################################################
#########################################################

#load codebook
setwd(datadir); dir()
tmpfname<-"usa_00036.cbk"
tmp<-readLines(tmpfname)

stline <- str_detect(tmp,"Variable") %>% which %>% min + 1
endline <- str_detect(tmp,"Variable Availability Key") %>% which - 2
if(!is.finite(stline) | !is.finite(endline))
  stop('var missing')

tmp<-tmp[stline:endline]
tmp<-str_extract(tmp,"(.*?)\\sX\\s")
colnames<-str_extract(tmp,"[A-Z0-9]+") %>% 
  tolower
locs<-str_extract(tmp,"(H|P)\\s+[0-9]+(\\-[0-9]+)?")
locs<-str_replace(locs,"(H|P)\\s+","")
stloc<-str_extract(locs,"^[0-9]+") %>%
  as.numeric
endloc<-str_extract(locs,"[0-9]+$") %>%
  as.numeric

#order stloc
neworder<-order(stloc)
colnames<-colnames[neworder]
stloc<-stloc[neworder]
endloc<-endloc[neworder]

#need to generate fwf format for cols
tmpmatrix<-matrix(
  c(stloc,endloc),
  ncol=2
) 
tmpfwf<-c(t(tmpmatrix))
widths<-genfw(tmpfwf)
types<-rep("i",length(colnames)) %>%
  paste0(collapse="")
types<-rep("integer",length(colnames))


#########################################################
#########################################################

#get the .dat
tmpfname <- str_replace_all(tmpfname,"\\.cbk|\\.txt",".dat")

#this is data to construct
setwd(datadir); dir()
require(LaF)
raw<-laf_open_fwf(
  filename=tmpfname,
  column_types=types,
  column_names=colnames,
  column_widths=widths
)

#get the dataset
thisdf<-data.table(raw[1:nrow(raw),colnames])

#recode variables

#weights (combines geographic weight w/ person weight)
thisdf[,weight_f := perwt/100]
thisdf[,hhweight_f := hhwt/100]

#agegroups 
thisdf[ age< 15, ageg_f := 1] 
thisdf[ age>=15 & age<18 , ageg_f := 2]
thisdf[ age>=18 & age<24 , ageg_f := 3] 
thisdf[ age>=24 & age<35 , ageg_f := 4]
thisdf[ age>=35 & age<50 , ageg_f := 5]
thisdf[ age>=50 & age<65 , ageg_f := 6]
thisdf[ age>=65 , ageg_f :=7]

#sex
thisdf[ sex==1 , sex_f := 1] 
thisdf[ sex==2 , sex_f := 2] 

#race/hispanic
#whites, non-hispanic
#blacks, non-hispanic
#hispanics
#other
#also, make those who choose black AND.. := black
blackcodes<-c(801,830:845) 
#also, make sure that hispanic is always reported
# tmp<-sum(df$hispan==9)
# if(tmp>0)
#   stop()
thisdf[ , race_f := 4] #make everyone other
thisdf[ race==1 & hispan==0, race_f := 1] #whites
thisdf[ (race==2 | raced%in%blackcodes) & hispan==0, race_f := 2] #blacks
thisdf[ hispan != 0, race_f := 3] #hispanics
#tmptab<-table( df$race_f, useNA="always" )
#100 * tmptab/sum(tmptab) 

#income
thisdf[ inctot==9999999 , inctot := NA]
thisdf[ incwage==9999999, incwage := NA]
#bottom-code..
thisdf[, inctot_f := inctot]
thisdf[, incwage_f := incwage]
bottomcode<-1000
thisdf[ inctot_f<bottomcode, inctot_f := bottomcode ]
thisdf[ incwage_f<bottomcode, incwage_f := bottomcode]

#rank occupations by education closure

#give everyone educational_q
thisdf[ educd%in%c(0,1), educd_f := NA_integer_]
thisdf[ !educd%in%c(0,1), educd_f := educd]
tmpf<-ecdf(thisdf$educd_f)
thisdf$educ_q<-100 * tmpf(thisdf$educd_f)

#get college grads
thisdf[educ<7 & educ!=0, collegegrad:=0] #0 is NA
thisdf[educ>=7, collegegrad:=1]

#rank occupations by educaiton
tmpdf<-thisdf[
  occ!=0
  ,
  .(
    #median_educ=weighted.median(educ_q,weight_f),
    pct_collegegrad = 100 * weighted.mean(
      collegegrad,weight=
        weight_f,
      na.rm=T
    )
  )
  ,
  by=c(
    'occ'
  )
]
tmpdf<-tmpdf[order(-tmpdf$pct_collegegrad),]
tmpdf$rank<-1:nrow(tmpdf)
tmpdf$skillclass<-as.numeric(cut(tmpdf$rank,4))
#split the ranking into four classes
#take the bottom 75% for working class
#take the top 25% for professionals
#this is kind of arbitrary, but it suffices

thisdf<-merge(
  thisdf,
  tmpdf[,c('occ','skillclass')],
  by='occ'
)

#nilf
#nilf, w-age or unemployed
#unskilled worker
#skilled worker/professional
#capitalist

#not of w-age or those not in the labor force
thisdf[ ageg_f%in%c(1,2,7) | empstat%in%c(0,3) , class_f := NA_integer_ ]
#of w-age and not in the labor force or just unemployed
thisdf[ ageg_f%in%c(3,4,5,6) & empstat%in%c(0,2,3), class_f := 1]
#unskilled worker (bottom of skilldistribution)
thisdf[ empstat==1 & classwkr==2 & skillclass%in%c(2,3,4), class_f :=2 ] 
#skilled worker or self-employed
thisdf[ empstat==1 & classwkr==2 & skillclass==1 , class_f:= 3]
thisdf[ empstat==1 & classwkr==1, class_f :=3 ]
#capitalist (anyone who is incorporated self-employed)
thisdf[ classwkrd==14, class_f:= 4 ]

tmptable<-questionr::wtd.table(
  thisdf$class_f,
  weights=thisdf$perwt
)
100 * tmptable/sum(tmptable)
#13% reserve-army/unemployed
#54% w-class
#30% professionals/self-employed
#4% capitalists

#########################################################
#########################################################

# CDFs

#subset to working-age men
incomedf <-  thisdf[
  ageg_f%in%c(3,4,5,6) & 
    race_f%in%c(1,2) & 
    sex_f==1,
]

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
infodf<-merge(
  gapdf[,c('race_f','class_f','income')],
  tmpdf
)
names(infodf)[names(infodf)=="income"]<-"medianincome"
setwd(outputdir)
write.csv(
  infodf,
  "siminfo.csv",
  row.names=F
)

#########################################################
#########################################################

#REPARATIONS AND REDISTRIBUTION

#play around w/ race-based and class-based simulation
tmpdf<-incomedf[year==2019,]

#we use weight_f to simulate.. 
tmpdf<-data.frame(
  income=rep(tmpdf$inctot_f,tmpdf$weight_f),
  race=rep(tmpdf$race_f,tmpdf$weight_f)
) %>% data.table

#REPARATIONS

#median gap between blacks/whites
mwage_w <- median(tmpdf$income[tmpdf$race==1])
mwage_b <- median(tmpdf$income[tmpdf$race==2])

#we need to close this gap
mediangap <- mwage_w - mwage_b
wbratio<-sum(tmpdf$race==1)/sum(tmpdf$race==2)
#we take x/wbratio from whites to give x to blacks
#x + x/wbratio = mediangap
#solve for x

reparations_gain <- (mediangap * wbratio)/(1 + wbratio)
reparations_loss <- -1 * reparations_gain / wbratio

tmpdf$reparations_tax[tmpdf$race==1] <- reparations_loss
tmpdf$reparations_tax[tmpdf$race==2] <- reparations_gain

#transfer the money 
#(except, if you don't have very much, we will only take as much
#as will leave you with bottomcode amount)
tmp<-(tmpdf$income + tmpdf$reparations_tax)<bottomcode
tmpdf$reparations_tax[tmp] <- bottomcode - tmpdf$income[tmp]
tmpdf$income_reparations <- (tmpdf$income + tmpdf$reparations_tax)

#check that the median gap is closed (yup)
mwage_w_new <- median(tmpdf$income_reparations[tmpdf$race==1])
mwage_b_new <- median(tmpdf$income_reparations[tmpdf$race==2])

#rank everyone by interest in this proposal
#(i.e., what they gain in welfare) 
tmpdf$welfare_reparations <- log(tmpdf$income_reparations) - log(tmpdf$income)

#REDISTRIBUTION

#split the pie equally
tmpdf$income_redistribution <- mean(tmpdf$income)
tmpdf$welfare_redistribution <- log(tmpdf$income_redistribution) - log(tmpdf$income)

#REPARATIONS VS. REDISTRIBUTION

tmpdf$welfare_comparison <- tmpdf$welfare_reparations - tmpdf$welfare_redistribution

#summarize welfare comparison by race X percentile income
tmpf<-ecdf(tmpdf$income)
tmpdf$percentile<-round(100 * tmpf(tmpdf$income))

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
    'percentile'
  )
]

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
    x=percentile,
    y=welfare_comparison,
    color=race,
    group=race
  )
) +
  geom_line(
    size=2
  ) +
  geom_hline(
    yintercept=0,
    linetype='dashed'
  ) +
  scale_color_manual(
    name="",
    values=tmpcolors
  ) +
  xlab("\nIncome Rank (Percentile)") +
  ylab("Welfare Gain (Reparations) - Weflare Gain (Redistribution)\n") +
  theme_bw() + 
  theme(
    legend.position = 'top',
    legend.direction = 'horizontal'
  )

setwd(outputdir)
ggsave(
  plot=g.tmp,
  filename="fig_welfarecomparison.png",
  width=8,
  height=6
)


#########################################################
#########################################################
