library(ggplot2)
library(data.table)
library(dplyr)
library(langcog)
library(readr)
library(tidyr)
library(stringr)
library(magrittr)
library(directlabels)
library(lubridate)
library(lme4)
library(rstan)

rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())
#options(mc.cores = 1)

## New

df <- data.table(pmean=vector(mode="numeric"),
                 pa=vector(mode="numeric"),
                 alpha=vector(mode="numeric"),
                 ninteractions=vector(mode="numeric"),
                 method=vector(mode="character"),
                 value=vector(mode="numeric"))
mod <- stan_model('alignment.vectorized.stan')
pmean <- 5
nspeakers <- 50
ninteractions <- 100
pa <- .1
alpha <- 1
cutoff <- 0
maxtrials <- 20000
for (pmean in c(5,10)) {
for (pa in c(.05,.1,.25,.5)) {
for (ninteractions in c(100,500)) {
for (alpha in c(.9,1,1.05,1.1,1.2)) {
for (i in 1:2) {
nmsgs <- rgeom(maxtrials,1/pmean)+1
na <- rbinom(length(nmsgs),nmsgs,pa)
nna <- nmsgs-na
#ma <- matrix(0,ncol=nspeakers,nrow=nspeakers)
#mna <- matrix(0,ncol=nspeakers,nrow=nspeakers)
goodones <- sample.int(maxtrials,maxtrials,F)
goodones <- goodones[na[goodones]>cutoff&nna[goodones]>cutoff]
#goodones <- goodones[goodones%%nspeakers!=ceiling(goodones/nspeakers)]
#numbers[i] <- length(goodones)

while (ninteractions>length(goodones)) {
  nmsgs2 <- rgeom(maxtrials,1/pmean)+1
  nmsgs <- c(nmsgs,nmsgs2)
  na <- rbinom(length(nmsgs),nmsgs,pa)
  nna <- nmsgs-na
  goodones <- sample.int(length(nmsgs),length(nmsgs),F)
  goodones <- goodones[na[goodones]>cutoff&nna[goodones]>cutoff]
}

goodones <- goodones[1:ninteractions]
ma <- na[goodones]
mna <- nna[goodones]

pba <- alpha*pa
mba <- rbinom(length(ma),ma,pba)
mnba <- ma-mba
mbna <- rbinom(length(mna),mna,pa)
mnbna <- mna-mbna

dnm <- mba/ma - (mba+mbna)/(ma+mna)
#dnm <- mba/ma - (mbna)/(mna)
df <- bind_rows(df,list(pmean=pmean,pa=pa,alpha=alpha,ninteractions=ninteractions,method='DNM',value=mean(dnm)))

dnm10 <- dnm[(na[goodones]+nna[goodones])>=10]
df <- bind_rows(df,list(pmean=pmean,pa=pa,alpha=alpha,ninteractions=ninteractions,method='DNM10',value=mean(dnm10),iters=500))


#dnm <- log((mba+pa)/(ma+1)) - log((mbna+pa)/(mna+1))
#df <- bind_rows(df,list(pmean=pmean,pa=pa,alpha=alpha,ninteractions=ninteractions,method='log',value=mean(dnm)))

alignment_data <- list(NumMarkers=1,
                       NumSubPops = 1,
                       NumObservations = length(mba),
                       SpeakerSubPop = rep(1,length(mba)),
                       MarkerType = rep(1,length(mba)),
                       NumUtterancesAB = ma,
                       NumUtterancesNotAB = mna,
                       CountsAB = mba,
                       CountsNotAB = mbna)
fit <- sampling(mod, data = alignment_data, 
            iter = 500, chains =1 )

df <- bind_rows(df,list(pmean=pmean,pa=pa,alpha=alpha,ninteractions=ninteractions,method='model',value=mean(colMeans(rstan:::extract(fit,"eta_ab_observation")$eta_ab_observation)),iters=500))

}}}}}

ggplot(aes(x=alpha,y=value,color=as.factor(pa)),data=df) + geom_point() + geom_smooth(method='loess') + facet_grid(method~ninteractions,scales='free')

df2 <- df %>% filter(ninteractions==500) %>%
  group_by(alpha,pa,pmean,ninteractions,method) %>%
  multi_boot_standard("value", na.rm = T)

ggplot(aes(x=alpha,y=mean,color=as.factor(pmean)),data=df2) + geom_pointrange(aes(ymin=ci_lower,ymax=ci_upper),position="dodge") + facet_grid(method~pa,scales='free')

##################
### Second try
##################

df <- data.table(pmean=vector(mode="numeric"),
                 pa=vector(mode="numeric"),
                 alpha=vector(mode="numeric"),
                 ninteractions=vector(mode="numeric"),
                 method=vector(mode="character"),
                 value=vector(mode="numeric"))
mod <- stan_model('alignment.vectorized.stan')
pmean <- 5
nspeakers <- 50
ninteractions <- 100
pa <- .1
alpha <- 1
cutoff <- 0
maxtrials <- 20000
for (pmean in c(5)) {
  for (pa in c(.05,.25,.5,.75)) {
    for (ninteractions in c(500)) {
      for (alpha in c(.9,1,1.1)) {
        for (i in 1:10) {
          nmsgs <- rgeom(maxtrials,1/pmean)+1
          na <- rbinom(length(nmsgs),nmsgs,pa)
          nna <- nmsgs-na
          #ma <- matrix(0,ncol=nspeakers,nrow=nspeakers)
          #mna <- matrix(0,ncol=nspeakers,nrow=nspeakers)
          goodones <- sample.int(maxtrials,maxtrials,F)
          goodones <- goodones[na[goodones]>cutoff&nna[goodones]>cutoff]
          #goodones <- goodones[goodones%%nspeakers!=ceiling(goodones/nspeakers)]
          #numbers[i] <- length(goodones)
          
          while (ninteractions>length(goodones)) {
            nmsgs2 <- rgeom(maxtrials,1/pmean)+1
            nmsgs <- c(nmsgs,nmsgs2)
            na <- rbinom(length(nmsgs),nmsgs,pa)
            nna <- nmsgs-na
            goodones <- sample.int(length(nmsgs),length(nmsgs),F)
            goodones <- goodones[na[goodones]>cutoff&nna[goodones]>cutoff]
          }
          
          goodones <- goodones[1:ninteractions]
          ma <- na[goodones]
          mna <- nna[goodones]
          
          pba <- alpha*pa
          mba <- rbinom(length(ma),ma,pba)
          mnba <- ma-mba
          mbna <- rbinom(length(mna),mna,pa)
          mnbna <- mna-mbna
          
          dnm <- mba/ma - (mba+mbna)/(ma+mna)
          df <- bind_rows(df,list(pmean=pmean,pa=pa,alpha=alpha,ninteractions=ninteractions,method='DNM',value=mean(dnm)))
          
          dnm2 <- mba/ma - (mbna)/(mna)
          df <- bind_rows(df,list(pmean=pmean,pa=pa,alpha=alpha,ninteractions=ninteractions,method='DNM-NOTA',value=mean(dnm2)))

          dnm10 <- dnm[(na[goodones]+nna[goodones])>=10]
          df <- bind_rows(df,list(pmean=pmean,pa=pa,alpha=alpha,ninteractions=ninteractions,method='DNM10',value=mean(dnm10),iters=500))
          
          
          #dnm <- log((mba+pa)/(ma+1)) - log((mbna+pa)/(mna+1))
          #df <- bind_rows(df,list(pmean=pmean,pa=pa,alpha=alpha,ninteractions=ninteractions,method='log',value=mean(dnm)))
          
          alignment_data <- list(NumMarkers=1,
                                 NumSubPops = 1,
                                 NumObservations = length(mba),
                                 SpeakerSubPop = rep(1,length(mba)),
                                 MarkerType = rep(1,length(mba)),
                                 NumUtterancesAB = ma,
                                 NumUtterancesNotAB = mna,
                                 CountsAB = mba,
                                 CountsNotAB = mbna)
          fit <- sampling(mod, data = alignment_data, 
                          iter = 250, chains =1 )
          
          df <- bind_rows(df,list(pmean=pmean,pa=pa,alpha=alpha,ninteractions=ninteractions,method='model_eta',value=mean(colMeans(rstan:::extract(fit,"eta_ab_observation")$eta_ab_observation))))
          df <- bind_rows(df,list(pmean=pmean,pa=pa,alpha=alpha,ninteractions=ninteractions,method='model_mu',value=mean(log(colMeans(rstan:::extract(fit,"mu_ab")$mu_ab)) - 
                                                                                                                           log(colMeans(rstan:::extract(fit,"mu_notab")$mu_notab)))))
          df <- bind_rows(df,list(pmean=pmean,pa=pa,alpha=alpha,ninteractions=ninteractions,method='model_sub',value=mean(colMeans(rstan:::extract(fit,"mu_ab")$mu_ab) - 
                                                                                                                            colMeans(rstan:::extract(fit,"mu_notab")$mu_notab))))
          
        }}}}}

ggplot(aes(x=alpha,y=value,color=as.factor(pa)),data=df) + geom_point() + geom_smooth(method='loess') + facet_grid(method~ninteractions,scales='free')

df2 <- df %>% filter(ninteractions==500) %>%
  group_by(alpha,pa,pmean,ninteractions,method) %>%
  multi_boot_standard("value", na.rm = T)

ggplot(aes(x=alpha,y=mean,color=as.factor(pmean)),data=df2) + geom_pointrange(aes(ymin=ci_lower,ymax=ci_upper),position="dodge") + facet_grid(method~pa,scales='free')

save(df,file='results/www2016_simulations_2.RData')

#########################################
### Third try
#########################################

df <- data.table(pmean=vector(mode="numeric"),
                 pa=vector(mode="numeric"),
                 alpha=vector(mode="numeric"),
                 ninteractions=vector(mode="numeric"),
                 method=vector(mode="character"),
                 value=vector(mode="numeric"))
mod <- stan_model('alignment.vectorized2.stan')
pmean <- 5
nspeakers <- 50
ninteractions <- 100
pa <- .1
alpha <- 1
cutoff <- 0
maxtrials <- 20000
for (pmean in c(5)) {
  for (pa in c(.05,.25,.5,.75)) {
    for (ninteractions in c(1500)) {
      for (alpha in c(.9,1,1.1)) {
        for (i in 1:10) {
          nmsgs <- rgeom(maxtrials,1/pmean)+1
          na <- rbinom(length(nmsgs),nmsgs,pa)
          nna <- nmsgs-na
          #ma <- matrix(0,ncol=nspeakers,nrow=nspeakers)
          #mna <- matrix(0,ncol=nspeakers,nrow=nspeakers)
          goodones <- sample.int(maxtrials,maxtrials,F)
          goodones <- goodones[na[goodones]>cutoff&nna[goodones]>cutoff]
          #goodones <- goodones[goodones%%nspeakers!=ceiling(goodones/nspeakers)]
          #numbers[i] <- length(goodones)
          
          while (ninteractions>length(goodones)) {
            nmsgs2 <- rgeom(maxtrials,1/pmean)+1
            nmsgs <- c(nmsgs,nmsgs2)
            na <- rbinom(length(nmsgs),nmsgs,pa)
            nna <- nmsgs-na
            goodones <- sample.int(length(nmsgs),length(nmsgs),F)
            goodones <- goodones[na[goodones]>cutoff&nna[goodones]>cutoff]
          }
          
          goodones <- goodones[1:ninteractions]
          ma <- na[goodones]
          mna <- nna[goodones]
          
          pba <- alpha*pa
          mba <- rbinom(length(ma),ma,pba)
          mnba <- ma-mba
          mbna <- rbinom(length(mna),mna,pa)
          mnbna <- mna-mbna
          
          dnm <- mba/ma - (mba+mbna)/(ma+mna)
          df <- bind_rows(df,list(pmean=pmean,pa=pa,alpha=alpha,ninteractions=ninteractions,method='DNM',value=mean(dnm)))
          
#           dnm2 <- mba/ma - (mbna)/(mna)
#           df <- bind_rows(df,list(pmean=pmean,pa=pa,alpha=alpha,ninteractions=ninteractions,method='DNM-NOTA',value=mean(dnm2)))
#           
#           dnm10 <- dnm[(na[goodones]+nna[goodones])>=10]
#           df <- bind_rows(df,list(pmean=pmean,pa=pa,alpha=alpha,ninteractions=ninteractions,method='DNM10',value=mean(dnm10),iters=500))
          
          
          #dnm <- log((mba+pa)/(ma+1)) - log((mbna+pa)/(mna+1))
          #df <- bind_rows(df,list(pmean=pmean,pa=pa,alpha=alpha,ninteractions=ninteractions,method='log',value=mean(dnm)))
          
          alignment_data <- list(NumMarkers=1,
                                 NumSubPops = 1,
                                 NumObservations = length(mba),
                                 SpeakerSubPop = rep(1,length(mba)),
                                 MarkerType = rep(1,length(mba)),
                                 NumUtterancesAB = ma,
                                 NumUtterancesNotAB = mna,
                                 CountsAB = mba,
                                 CountsNotAB = mbna,
                                 StdDev = 10)
          fit <- sampling(mod, data = alignment_data, 
                          iter = 500, chains =1 )
          
          df <- bind_rows(df,list(pmean=pmean,pa=pa,alpha=alpha,ninteractions=ninteractions,method='model_m_pop',value=mean(rstan:::extract(fit,"m_ab_pop")$m_ab_pop)))
          df <- bind_rows(df,list(pmean=pmean,pa=pa,alpha=alpha,ninteractions=ninteractions,method='model_eta_pop',value=mean(colMeans(rstan:::extract(fit,"eta_ab_pop")$eta_ab_pop))))
          df <- bind_rows(df,list(pmean=pmean,pa=pa,alpha=alpha,ninteractions=ninteractions,method='model_eta_subpop',value=mean(colMeans(rstan:::extract(fit,"eta_ab_subpop")$eta_ab_subpop))))
          df <- bind_rows(df,list(pmean=pmean,pa=pa,alpha=alpha,ninteractions=ninteractions,method='model_mu',value=mean(log(colMeans(rstan:::extract(fit,"mu_ab")$mu_ab)) - 
                                                                                                                           log(colMeans(rstan:::extract(fit,"mu_notab")$mu_notab)))))
          df <- bind_rows(df,list(pmean=pmean,pa=pa,alpha=alpha,ninteractions=ninteractions,method='model_sub',value=mean(colMeans(rstan:::extract(fit,"mu_ab")$mu_ab) - 
                                                                                                                            colMeans(rstan:::extract(fit,"mu_notab")$mu_notab))))
          
        }}}}}

ggplot(aes(x=alpha,y=value,color=as.factor(pa)),data=df) + geom_hline(yintercept=0) + geom_point() + geom_smooth(method='loess') + facet_grid(method~pa,scales='free')

df2 <- df %>%
  group_by(alpha,pa,pmean,method) %>%
  multi_boot_standard("value", na.rm = T)

ggplot(aes(x=alpha,y=mean,color=as.factor(pmean)),data=df2) + geom_hline(yintercept=0) + geom_pointrange(aes(ymin=ci_lower,ymax=ci_upper),position="dodge") + facet_grid(method~pa,scales='free')

save(df,file='results/www2016_simulations_3.RData')

################
# Investigation of alpha=1 problems
################

invlogit <- function(p) { return(-log(1/p-1))}
logit <- function(x) { return(exp(x)/(1+exp(x)))}

dt <- data.table(eta_ab=colMeans(rstan:::extract(fit,"eta_ab_observation")$eta_ab_observation),
                 eta_notab=colMeans(rstan:::extract(fit,"eta_observation")$eta_observation),
                 mu_ab=colMeans(rstan:::extract(fit,"mu_ab")$mu_ab),
                 mu_notab=colMeans(rstan:::extract(fit,"mu_notab")$mu_notab),
                 pba=mba/ma,
                 pbna=mbna/mna,
                 pa=ma/(ma+mna),
                 pb=(mba+mbna)/(ma+mna),
                 count=ma+mna)

ggplot(aes(x=mu_notab,y=mu_ab,color=pa),data=dt) +geom_point() + geom_abline(yintercept=0,slope=1)

ggplot(aes(x=mu_notab,y=mu_ab,color=pba),data=dt) +geom_point() + geom_abline(yintercept=0,slope=1) + geom_smooth(method='lm')

ggplot(aes(x=eta_notab,y=eta_ab,color=pba),data=dt) +geom_point() + geom_abline(yintercept=0,slope=1) + geom_smooth(method='lm')

ggplot(aes(x=eta_notab,y=eta_ab,color=pba),data=dt) +geom_point() + geom_abline(yintercept=0,slope=1) + geom_smooth(method='lm')


ggplot(aes(x=logit(eta_notab),y=logit(eta_ab+eta_notab),color=count),data=dt) +geom_point() + geom_abline(yintercept=0,slope=1) + geom_point(aes(x=mean(logit(eta_notab)),y=mean(logit(eta_ab+eta_notab))),color='red') + geom_point(aes(x=weighted.mean(logit(eta_notab),count),y=weighted.mean(logit(eta_ab+eta_notab),count)),color='green')

ggplot(aes(x=(mu_ab-mu_notab),y=(pba-pbna),color=count),data=dt) +geom_point() + geom_abline(yintercept=0,slope=1)

####################
# Fourth try
####################

df <- data.table(sd=vector(mode="numeric"),
                 pa=vector(mode="numeric"),
                 alpha=vector(mode="numeric"),
                 ninteractions=vector(mode="numeric"),
                 method=vector(mode="character"),
                 value=vector(mode="numeric"))
mod <- stan_model('alignment.vectorized2.stan')
pmean <- 5
nspeakers <- 50
ninteractions <- 100
pa <- .1
alpha <- 1
cutoff <- 0
maxtrials <- 20000
for (sd in c(.1,1,10)) {
  for (pa in c(.05,.75)) {
    for (ninteractions in c(1500)) {
      for (alpha in c(.9,1,1.1)) {
        for (i in 1:10) {
          nmsgs <- rgeom(maxtrials,1/pmean)+1
          na <- rbinom(length(nmsgs),nmsgs,pa)
          nna <- nmsgs-na
          #ma <- matrix(0,ncol=nspeakers,nrow=nspeakers)
          #mna <- matrix(0,ncol=nspeakers,nrow=nspeakers)
          goodones <- sample.int(maxtrials,maxtrials,F)
          goodones <- goodones[na[goodones]>cutoff&nna[goodones]>cutoff]
          #goodones <- goodones[goodones%%nspeakers!=ceiling(goodones/nspeakers)]
          #numbers[i] <- length(goodones)
          
          while (ninteractions>length(goodones)) {
            nmsgs2 <- rgeom(maxtrials,1/pmean)+1
            nmsgs <- c(nmsgs,nmsgs2)
            na <- rbinom(length(nmsgs),nmsgs,pa)
            nna <- nmsgs-na
            goodones <- sample.int(length(nmsgs),length(nmsgs),F)
            goodones <- goodones[na[goodones]>cutoff&nna[goodones]>cutoff]
          }
          
          goodones <- goodones[1:ninteractions]
          ma <- na[goodones]
          mna <- nna[goodones]
          
          pba <- alpha*pa
          mba <- rbinom(length(ma),ma,pba)
          mnba <- ma-mba
          mbna <- rbinom(length(mna),mna,pa)
          mnbna <- mna-mbna
          
          dnm <- mba/ma - (mba+mbna)/(ma+mna)
          df <- bind_rows(df,list(pmean=pmean,pa=pa,alpha=alpha,ninteractions=ninteractions,method='DNM',value=mean(dnm)))
          
          #           dnm2 <- mba/ma - (mbna)/(mna)
          #           df <- bind_rows(df,list(pmean=pmean,pa=pa,alpha=alpha,ninteractions=ninteractions,method='DNM-NOTA',value=mean(dnm2)))
          #           
          #           dnm10 <- dnm[(na[goodones]+nna[goodones])>=10]
          #           df <- bind_rows(df,list(pmean=pmean,pa=pa,alpha=alpha,ninteractions=ninteractions,method='DNM10',value=mean(dnm10),iters=500))
          
          
          #dnm <- log((mba+pa)/(ma+1)) - log((mbna+pa)/(mna+1))
          #df <- bind_rows(df,list(pmean=pmean,pa=pa,alpha=alpha,ninteractions=ninteractions,method='log',value=mean(dnm)))
          
          alignment_data <- list(NumMarkers=1,
                                 NumSubPops = 1,
                                 NumObservations = length(mba),
                                 SpeakerSubPop = rep(1,length(mba)),
                                 MarkerType = rep(1,length(mba)),
                                 NumUtterancesAB = ma,
                                 NumUtterancesNotAB = mna,
                                 CountsAB = mba,
                                 CountsNotAB = mbna,
                                 StdDev = sd)
          fit <- sampling(mod, data = alignment_data, 
                          iter = 250, chains =1 )
          
          df <- bind_rows(df,list(sd=sd,pa=pa,alpha=alpha,ninteractions=ninteractions,method='model_m_pop',value=mean(rstan:::extract(fit,"m_ab_pop")$m_ab_pop)))
          df <- bind_rows(df,list(sd=sd,pa=pa,alpha=alpha,ninteractions=ninteractions,method='model_eta_pop',value=mean(colMeans(rstan:::extract(fit,"eta_ab_pop")$eta_ab_pop))))
          df <- bind_rows(df,list(sd=sd,pa=pa,alpha=alpha,ninteractions=ninteractions,method='model_eta_subpop',value=mean(colMeans(rstan:::extract(fit,"eta_ab_subpop")$eta_ab_subpop))))
          df <- bind_rows(df,list(sd=sd,pa=pa,alpha=alpha,ninteractions=ninteractions,method='model_mu',value=mean(log(colMeans(rstan:::extract(fit,"mu_ab")$mu_ab)) - 
                                                                                                                           log(colMeans(rstan:::extract(fit,"mu_notab")$mu_notab)))))
          df <- bind_rows(df,list(sd=sd,pa=pa,alpha=alpha,ninteractions=ninteractions,method='model_sub',value=mean(colMeans(rstan:::extract(fit,"mu_ab")$mu_ab) - 
                                                                                                                            colMeans(rstan:::extract(fit,"mu_notab")$mu_notab))))
          
        }}}}}

ggplot(aes(x=alpha,y=value,color=as.factor(sd)),data=df) + geom_hline(yintercept=0) + geom_point() + geom_smooth(method='loess') + facet_grid(method~pa,scales='free')

df2 <- df %>%
  group_by(alpha,pa,sd,method) %>%
  multi_boot_standard("value", na.rm = T)

ggplot(aes(x=alpha,y=mean,color=as.factor(sd)),data=df2) + geom_hline(yintercept=0) + geom_pointrange(aes(ymin=ci_lower,ymax=ci_upper),position="dodge") + facet_grid(method~pa,scales='free')

save(df,file='results/www2016_simulations_4.RData')

####################
# Fifth try
####################

df <- data.table(sd=vector(mode="numeric"),
                 pa=vector(mode="numeric"),
                 alpha=vector(mode="numeric"),
                 ninteractions=vector(mode="numeric"),
                 method=vector(mode="character"),
                 value=vector(mode="numeric"))
mod <- stan_model('alignment.vectorized2.stan')
pmean <- 5
nspeakers <- 50
cutoff <- 0
maxtrials <- 20000
for (sd in c(1)) {
  for (pa in c(.05,.1,.5,.75,.9)) {
    for (ninteractions in c(25,100)) {
      for (alpha in c(-1,-.5,0,.5,1)) {
        for (i in 1:10) {
          nmsgs <- rgeom(maxtrials,1/pmean)+1
          na <- rbinom(length(nmsgs),nmsgs,pa)
          nna <- nmsgs-na
          goodones <- sample.int(maxtrials,maxtrials,F)
          goodones <- goodones[na[goodones]>cutoff&nna[goodones]>cutoff]

          while (ninteractions>length(goodones)) {
            nmsgs2 <- rgeom(maxtrials,1/pmean)+1
            nmsgs <- c(nmsgs,nmsgs2)
            na <- rbinom(length(nmsgs),nmsgs,pa)
            nna <- nmsgs-na
            goodones <- sample.int(length(nmsgs),length(nmsgs),F)
            goodones <- goodones[na[goodones]>cutoff&nna[goodones]>cutoff]
          }
          
          goodones <- goodones[1:ninteractions]
          ma <- na[goodones]
          mna <- nna[goodones]
          
          pba <- logit(invlogit(pa)+alpha)
          mba <- rbinom(length(ma),ma,pba)
          mnba <- ma-mba
          mbna <- rbinom(length(mna),mna,pa)
          mnbna <- mna-mbna
          
          dnm <- mba/ma - (mba+mbna)/(ma+mna)
          df <- bind_rows(df,list(pa=pa,alpha=alpha,ninteractions=ninteractions,method='DNM',value=mean(dnm)))
          
          alignment_data <- list(NumMarkers=1,
                                 NumSubPops = 1,
                                 NumObservations = length(mba),
                                 SpeakerSubPop = rep(1,length(mba)),
                                 MarkerType = rep(1,length(mba)),
                                 NumUtterancesAB = ma,
                                 NumUtterancesNotAB = mna,
                                 CountsAB = mba,
                                 CountsNotAB = mbna,
                                 StdDev = sd)
          fit <- sampling(mod, data = alignment_data, 
                          iter = 200, chains =1 )
          
          df <- bind_rows(df,list(sd=sd,pa=pa,alpha=alpha,ninteractions=ninteractions,method='model_m_pop',value=mean(rstan:::extract(fit,"m_ab_pop")$m_ab_pop)))
          df <- bind_rows(df,list(sd=sd,pa=pa,alpha=alpha,ninteractions=ninteractions,method='model_eta_pop',value=mean(colMeans(rstan:::extract(fit,"eta_ab_pop")$eta_ab_pop))))
          df <- bind_rows(df,list(sd=sd,pa=pa,alpha=alpha,ninteractions=ninteractions,method='model_eta_subpop',value=mean(colMeans(rstan:::extract(fit,"eta_ab_subpop")$eta_ab_subpop))))
          df <- bind_rows(df,list(sd=sd,pa=pa,alpha=alpha,ninteractions=ninteractions,method='model_mu',value=mean(log(colMeans(rstan:::extract(fit,"mu_ab")$mu_ab)) - 
                                                                                                                     log(colMeans(rstan:::extract(fit,"mu_notab")$mu_notab)))))
          df <- bind_rows(df,list(sd=sd,pa=pa,alpha=alpha,ninteractions=ninteractions,method='model_sub',value=mean(colMeans(rstan:::extract(fit,"mu_ab")$mu_ab) - 
                                                                                                                      colMeans(rstan:::extract(fit,"mu_notab")$mu_notab))))
          
        }}}}}

ggplot(aes(x=alpha,y=value,color=as.factor(ninteractions)),data=df) + geom_hline(yintercept=0) + geom_point() + geom_smooth(method='lm') + facet_grid(method~pa,scales='free')

df2 <- df %>%
  group_by(alpha,pa,sd,ninteractions,method) %>%
  multi_boot_standard("value", na.rm = T) %>%
  mutate(method=factor(method,c('DNM','model_sub','model_mu','model_eta_subpop','model_eta_pop','model_m_pop')))

ggplot(aes(x=as.factor(alpha),y=mean,color=as.factor(ninteractions)),data=df2) + geom_hline(yintercept=0) + geom_pointrange(aes(ymin=ci_lower,ymax=ci_upper),position = position_dodge(width = 0.3)) + facet_grid(method~pa,scales='free') +theme_bw()

df3 <- df2 %>% filter(method!='model_m_pop'&method!='model_eta_pop') %>%
  mutate(significant=ifelse(xor(sign(ci_lower)==sign(ci_upper)&sign(mean)==sign(alpha),alpha==0),0,1))

ggplot(aes(x=as.factor(alpha),y=mean,color=as.factor(ninteractions),alpha=significant),data=df3) +
  geom_hline(yintercept=0) + 
  geom_pointrange(aes(ymin=ci_lower,ymax=ci_upper),position = position_dodge(width = 0.3)) + 
  facet_grid(method~pa,scales='free') +
  scale_color_brewer(palette = "Set1") +
  scale_alpha(range=c(.5,.9),guide=FALSE) +
  labs(x="true alignment (in logit space)",y="estimated alignment (in method's space)",
       title="Simulated data -- alignment specified in logit space\nmean 5 interactions between each pair, 200 Stan iters, 10 runs",color="# pairs\nobserved") +
  theme_bw()

save(df,file='results/www2016_simulations_6.RData')

