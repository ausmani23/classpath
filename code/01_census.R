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
tmpfname<-"usa_00038.cbk"
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
thisdf[ race==1 & hispan==0, race_f := 1] #whites, non-hispanics
thisdf[ (race==2 | raced%in%blackcodes) & hispan==0, race_f := 2] #blacks, non-hisapnics
thisdf[ hispan != 0, race_f := 3] #hispanics, all races
#tmptab<-table( df$race_f, useNA="always" )
#100 * tmptab/sum(tmptab) 

#add a majority-minority measure of race
thisdf[ , race_m := 1]
thisdf[ race_f%in%c(2,3,4), race_m:= 2]


#income
thisdf[ inctot==9999999 , inctot := NA]
thisdf[ incwage==9999999, incwage := NA]
thisdf[ incbus00==9999999, incbus00 :=NA]
thisdf[ incinvst==9999999, incinvst :=NA]
#bottom-code..
thisdf[, inctot_f := inctot]
thisdf[, incwage_f := incwage]
thisdf[, incinvest_f := incinvst]

bottomcode<-1000
thisdf[ inctot_f<bottomcode, inctot_f := bottomcode ]
thisdf[ incwage_f<bottomcode, incwage_f := bottomcode]
thisdf[ incinvest_f<bottomcode, incinvest_f := bottomcode]

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
#capitalist (anyone who is incorporated self-employed
#or who makes maj of income from investment and significant income)
thisdf[ classwkrd==14, class_f:= 4 ]
thisdf[ incinvest_f/inctot_f > 0.5 & incinvest_f>10^4, class_f := 4]

#strategically positioned workers? 
#https://usa.ipums.org/usa/volii/ind2017.shtml
#construction (770)
#manufacturing (1070-3990)
#transportation and warehousing (6070-6390)
thisdf$strategic_f <- 0
thisdf[ ind==770, strategic_f := 1] #construction
thisdf[ ind%in%c(1070:3990), strategic_f := 2] #manufacturing
thisdf[ ind%in%c(6070:6390), strategic_f := 3] #trucking and transport

#########################################################
#########################################################

#subset to black and white working-age men
incomedf <-  thisdf[
  ageg_f%in%c(3,4,5,6) & 
    race_f%in%c(1,2) & 
    sex_f==1,
]

overall_median <- weighted.median(incomedf$inctot_f,incomedf$weight_f)
white_median <- weighted.median(
  incomedf$inctot_f[incomedf$race_m==1],
  incomedf$weight_f[incomedf$race_m==1]
) #wh
black_median <- weighted.median(
  incomedf$inctot_f[incomedf$race_m==2],
  incomedf$weight_f[incomedf$race_m==2]
) #black incomes
(white_median - black_median)/white_median


tmptable<-questionr::wtd.table(
  incomedf$class_f,
  weights=incomedf$weight_f
)
100 * tmptable/sum(tmptable)

tmptable<-questionr::wtd.table(
  incomedf$class_f,
  incomedf$race_f,
  weights=incomedf$weight_f
)
100 * tmptable/apply(tmptable,1,sum)
100 * tmptable[,1]/sum(tmptable[,1]) #whites by class
100 * tmptable[,2]/sum(tmptable[,2]) #nonwhites by class

#output siminfo
tmpdf<-incomedf[,sum(weight_f),by=c('race_f')]
whitepop <- 100 - 100 * tmpdf$V1[tmpdf$race_f==2]/sum(tmpdf$V1)
setwd(outputdir)
write(whitepop,"siminfo_whitepop.txt")

#weighted race table
tmptable<-questionr::wtd.table(
  incomedf$race_f,
  weights=incomedf$weight_f
)
print(100 * tmptable/sum(tmptable))

#race by class
tmptable<-questionr::wtd.table(
  x=incomedf$class_f,
  y=incomedf$race_f,
  weights=incomedf$weight_f
)
print(100 * tmptable/apply(tmptable,1,sum))

#race by strategic sector, workers only
tmpdf<-incomedf[class_f==2]
tmptable<-questionr::wtd.table(
  x=tmpdf$strategic_f==0,
  y=tmpdf$race_f,
  weights=tmpdf$weight_f
)
print(100 * tmptable/apply(tmptable,1,sum))
#as a whole

tmptable<-questionr::wtd.table(
  x=tmpdf$strategic_f,
  y=tmpdf$race_f,
  weights=tmpdf$weight_f
)
print(100 * tmptable/apply(tmptable,1,sum))
#broken down

#what is up with transport?
transportdf<-tmpdf[
  ind%in%c(6070:6390),
  .(
    black=sum(weight_f[race_f==2]),
    white=sum(weight_f[race_f==1])
  ),
  by=c('ind')
]
transportdf$black<-transportdf$black/apply(transportdf,1,sum)
transportdf$white<-transportdf$white/apply(transportdf,1,sum)
transportdf[order(transportdf$black),]
#overrepresented, within transprot, in least strateig industries
#(though we are probably strating to stretch limits of ACS sample)

#########################################################
#########################################################

#save out
setwd(datadir); dir()
write.csv(
  incomedf,
  "censusdf.csv",
  row.names=F
)

#########################################################
#########################################################
