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

setwd(datadir); dir()
mydf<-fread('notesonclass.csv')

#########################################################
#########################################################

#idvars
tmpnames<-c(
  "R0000100",
  "R0536401",
  "R0536402",
  "R1482600"
)
newnames<-c(
  "pubid",
  "bdatem",
  "bdatey",
  "race"
)
names(mydf)[names(mydf)%in%tmpnames]<-newnames

#incvars
tmpnames<-c(
  "R1204500", 
  "R2563300",
  "R3884900",
  "R5464100",
  "R7227800",
  "S1541700",
  "S2011500",
  ###
  "U0008900",
  "U1845500"
)
newnames<-c(
  "hhinc1997",
  "hhinc1998",
  "hhinc1999",
  "hhinc2000",
  "hhinc2001",
  "hhinc2002",
  "hhinc2003",
  "faminc2015",
  "faminc2017"
)
names(mydf)[names(mydf)%in%tmpnames]<-newnames

#recode of these vars
for(v in newnames) {
  #fix NAs
  mydf[[v]][mydf[[v]]<0]<-NA 
}

#########################################################
#########################################################

#reshape to long
gathervars<-c(
  newnames
)

mydf<-gather(
  mydf,
  var,
  val,
  all_of(gathervars)
) %>% data.table

mydf$year<-str_extract(mydf$var,"[0-9]{4}") %>% as.numeric
mydf$var<-str_replace(mydf$var,"[0-9]{4}","")
#add age at time of obs
mydf$age<-mydf$year - mydf$bdatey

#add note of which vars these are
mydf$type<-"raw"
mydf$type[str_detect(mydf$var,"\\_q$")]<-"q"
mydf$type[str_detect(mydf$var,"\\_race_q$")]<-"raceq"
mydf$var<-str_extract(mydf$var,"hhinc|faminc")


#collapse to pubid-level, w/ childhood income and adult income
incdf<-mydf[
  ,
  .(
    race=unique(race),
    childinc=mean(val[age<=18],na.rm=T),
    adultinc=mean(val[age>30],na.rm=T)
  )
  ,
  by=c(
    'pubid',
    'type'
  )
  ]

for(v in c("childinc","adultinc")) {
  #get quantiles
  newv<-paste0(v,"_q")
  tmpf<-ecdf(incdf[[v]])
  incdf[[newv]]<-100 * tmpf(incdf[[v]])
  #get w/in race quantile
  incdf<-by(incdf,incdf$race,function(df) {
    newv_race<-paste0(v,"_race_q")
    tmpf<-ecdf(df[[v]])
    df[[newv_race]]<-100 * tmpf(df[[v]])
    df
  }) %>% rbind.fill %>% data.table
}


#########################################################
#########################################################

#plot CDFs of childhood household income
plotdf<-incdf[is.finite(childinc_race_q),]

#plot the quantiles on this graph
tmpdf<-by(plotdf,plotdf$race,function(tmpdf) {
  #tmpdf<-plotdf[race==1,]
  qs<-c(10,25,50,75,90)
  for(q in qs) {
    tmpdf$tmp<-tmpdf$childinc_race_q - q
    tmpdf[[paste0("q",q)]]<-abs(tmpdf$tmp)==min(abs(tmpdf$tmp))
  }
  qvars<-names(tmpdf)[str_detect(names(tmpdf),"q[0-9]{2}")]
  tmpdf<-tmpdf[apply(tmpdf[,qvars,with=F],1,sum)>0,]
  tmpdf<-gather_(
    tmpdf,
    "var",
    "val",
    "qvars"
  ) %>% data.table
  tmpdf<-tmpdf[val==T]
  #pick just one 
  tmpdf$rank<-tapply(tmpdf$pubid,tmpdf$var,function(x) 1:length(x)) %>% unlist
  tmpdf[rank==1]
}) %>% rbind.fill %>% data.table

ggplot(
  plotdf[race%in%c(1,4)],
  aes(
    x=log(childinc),
    y=childinc_race_q,
    color=factor(race),
    group=factor(race)
  )
) + 
  geom_point() + 
  geom_point(
    data=tmpdf[race%in%c(1,4)],
    aes(
      shape=var
    ),
    color='black',
    size=2
  ) +
  theme_bw()

#########################################################
#########################################################

#plot CDFs of adult household income

plotdf<-incdf



ggplot(
  plotdf[race%in%c(1,4)],
  aes(
    x=log(adultinc),
    y=adultinc_race_q,
    color=factor(race),
    group=factor(race)
  )
) + 
  geom_line()

#########################################################
#########################################################

#show racial inequality exists, 
#even when conditioning on childhood income


#round to nearest 5th quantile
incdf$childinc_q5 <- floor(incdf$childinc_q/5)*5

#get adult income quantile, averages, at this point
sumdf<-incdf[
  is.finite(childinc_q5)
  ,
  .(
    adultinc_q=mean(adultinc_q,na.rm=T)
  )
  ,
  by=c(
    'childinc_q5',
    'race'
  )
]

require(ggplot2)

ggplot(
  sumdf[race%in%c(1,4)],
  aes(
    x=childinc_q5,
    y=adultinc_q,
    color=factor(race)
  )
) +
  geom_point() +
  theme_bw()

#clear racial gap, even conditioning on childinc


#########################################################
#########################################################



# #overall quantile
# tmpf<-ecdf(mydf$faminc)
# mydf$inc_q<-100 * tmpf(mydf$faminc)
# 
# #within-race quantile
# mydf<-by(mydf,mydf$race,function(df) {
#   tmpf<-ecdf(df$faminc)
#   df$inc_race_q<-100 * tmpf(df$faminc)
#   df 
# }) %>% rbind.fill %>% data.table
# mydf$inc_log<-log(mydf$faminc+1)

ggplot(
  mydf[race%in%c(1,4)],
  aes(
    x=inc_log,
    y=inc_race_q,
    color=factor(race)
  )
) +
  geom_point(
    size=2,
    alpha=0.5
  ) +
  theme_bw() +
  xlab(
    "\nLog Income"
  ) +
  ylab(
    "Within-Race Quantile"
  )

exp(10)


##use quanitle
##organize by age (take pre-18 income, adult income)
##figure out dropouts

