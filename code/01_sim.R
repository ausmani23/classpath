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

#scenarios  

#race, class, outcome
loopdf<-expand.grid(
  seed=1,
  N_agents=1000,
  majshare = 0.3,
  #fixed feature of classXrace stratification
  beta_race_class = c(1), 
  #direct effect of race
  beta_race_outcome = c(0,1),
  #direct effect of class
  beta_class_outcome = c(0,1),
  #extent of determinism in this world
  cause_luck_ratio = c(1,5),
  stringsAsFactors=F
)

#disagreement is both about:
# (1) which causal pathways are in operation
# (2) how important these pathways are relative to luck/other causes

#incidrect path: race doesn't directly affect outcome, but class doe
tmp<-loopdf$beta_race_outcome==0 & 
  loopdf$beta_class_outcome==1 

#direct path: race directly affects outcome, and class does not
tmp<-tmp | 
  loopdf$beta_class_outcome==0 & 
  loopdf$beta_race_outcome==1
loopdf<-loopdf[tmp,]

#describe scenarios
loopdf$scenario<-"direct path"
tmp<-loopdf$beta_race_outcome==0
loopdf$scenario[tmp]<-"indirect path"

tmp<-loopdf$cause_luck_ratio==5
loopdf$scenario[tmp] <-
  paste0(loopdf$scenario[tmp],", deterministic")

#########################################################
#########################################################

#simulate

#index
loopdf$i<-1:nrow(loopdf);
fulloutput<-lapply(loopdf$i,function(i) {
  
  
  #i<-4
  
  #tracker
  pct_done <- round(i/nrow(loopdf) * 100)
  if(i==1)
    print(paste0(nrow(loopdf),' iterations'))
  print(paste0("Iteration ",i))
  if(pct_done%%10==0)
    print(paste0(pct_done,"% done"))
  
  #this sim
  thisrow<-loopdf[loopdf$i==i,]
  
  #params
  set.seed(thisrow$seed)
  
  #generate agents, w/ assigned race
  agentsdf<-data.frame(
    agentid=1:thisrow$N_agents,
    race_i=as.numeric(pnorm(rnorm(thisrow$N_agents))>thisrow$majshare)
  ) %>% data.table
  
  #generate class position,
  #which we take to be a continous measure
  #something like log(earnings) 
  agentsdf$class_i <- 
    #effect of race on inherited income
    thisrow$cause_luck_ratio * thisrow$beta_race_class * agentsdf$race_i +
    #brute luck
    rnorm(thisrow$N_agents,sd=1)
  
  #standardize class
  agentsdf$class_i <- scale(agentsdf$class_i)[,1]
  
  #now, generate the outcome
  agentsdf$outcome_i <- 
    #effect of race on outcome
    thisrow$cause_luck_ratio * thisrow$beta_race_outcome * agentsdf$race_i +
    #effect of class on outcome
    thisrow$cause_luck_ratio * thisrow$beta_class_outcome * agentsdf$class_i + 
    #brute luck
    rnorm(thisrow$N_agents,sd=1)
  
  #standardize the outcome
  agentsdf$outcome_i <- scale(agentsdf$outcome_i)[,1]
  
  #cdfs will help illustrate what is going on
  
  #get quantile in overall distribution of initial class position 
  tmpf<-ecdf(agentsdf$class_i)
  agentsdf$class_i_q<-100 * tmpf(agentsdf$class_i)
  
  #get quantile in overall distribution of final outcome
  tmpf<-ecdf(agentsdf$outcome_i)
  agentsdf$outcome_i_q<-100 * tmpf(agentsdf$outcome_i)
  
  #get quantiles in race-specific distribution
  #which is a useful way to illustrate racial inequality
  #across the different worlds
  agentsdf<-by(agentsdf,agentsdf$race_i,function(df) {
    #df<-agentsdf[agentsdf$race_i==1,]
    tmpf<-ecdf(df$class_i)
    df$class_i_q_race<-100 * tmpf(df$class_i)
    tmpf<-ecdf(df$outcome_i)
    df$outcome_i_q_race<-100 * tmpf(df$outcome_i)
    df
  }) %>% rbind.fill %>% data.table
  
  #return
  agentsdf$i<-i
  agentsdf
  
  
}) 

#########################################################
#########################################################

#put these dfs together
fulldf<-rbind.fill(fulloutput)

#identify the scenarios
fulldf<-merge(
  fulldf,
  loopdf
) %>% data.table


#########################################################
#########################################################

#illustrate

require(ggplot2)

#CDF

#for illustrating given ptiles in each race
tmpdf<-by(fulldf,fulldf$race_i,function(tmpdf) {
  tmpdf$tmp<-tmpdf$outcome_i_q_race - 25
  tmpdf$q25<-abs(tmpdf$tmp)==min(abs(tmpdf$tmp))
  tmpdf$tmp<-tmpdf$outcome_i_q_race - 75
  tmpdf$q75<-abs(tmpdf$tmp)==min(abs(tmpdf$tmp))
  tmpdf<-tmpdf[q25==T | q75==T]
  tmpdf<-gather(
    tmpdf,
    var,
    val,
    q25:q75
  ) %>% data.table
  tmpdf[val==T]
}) %>% rbind.fill


g.tmp <- ggplot(
  fulldf,
  aes(
    x=outcome_i,
    y=outcome_i_q_race,
    color=factor(race_i)
  )
) +
  geom_point(
    size=2,
    alpha=0.5
  ) +
  geom_point(
    data=tmpdf,
    aes(
      shape=var
    ),
    color='black',
    size=2
  ) +
  facet_wrap(
    ~ scenario
  ) +
  xlab("\nOutcome in SDs Above/Below Mean") +
  ylab("Within-Race Quantile\n") +
  theme_bw() 

setwd(outputdir)
ggsave(
  "fig_race_cdfs.pdf",
  plot=g.tmp,
  width=8,
  height=4
)


g.tmp<-ggplot(
  fulldf,
  aes(
    x=class_i_q,
    y=outcome_i,
    color=factor(race_i)
  ) 
) +
  geom_point(
    alpha=0.5
  ) +
  facet_wrap(
    ~ scenario
  ) +
  xlab("\nClass Quantile") +
  ylab("Outcome in SDs Above/Below Mean\n") +
  theme_bw()

setwd(outputdir)
ggsave(
  "fig_class_cdfs.pdf",
  plot=g.tmp,
  width=8,
  height=4
)


#########################################################
#########################################################
