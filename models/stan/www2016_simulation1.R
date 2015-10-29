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

invlogit <- function(p) { return(-log(1/p-1))}
logit <- function(x) { return(exp(x)/(1+exp(x)))}


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
  for (pa in c(.05,.1,.9)) {
    for (ninteractions in c(25)) {
      for (alpha in c(-.5,0,.5)) {
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
  mutate(significant=ifelse(xor(sign(ci_lower)==sign(ci_upper),alpha==0),0,1))

df3$significant[sign(df3$mean)!=sign(df3$alpha)&df3$alpha!=0] <- 1

ggplot(aes(x=as.factor(alpha),y=mean,color=as.factor(ninteractions),alpha=significant),data=df3) +
  geom_hline(yintercept=0) + 
  geom_pointrange(aes(ymin=ci_lower,ymax=ci_upper),position = position_dodge(width = 0.3)) + 
  facet_grid(method~pa,scales='free') +
  scale_color_brewer(palette = "Set1") +
  scale_alpha(range=c(.4,1),guide=FALSE) +
  labs(x="true alignment (in logit space)",y="estimated alignment (in method's space)",
       title="Simulated data -- alignment specified in logit space\nmean 5 interactions between each pair, 200 Stan iters, 10 runs",color="# pairs\nobserved") +
  theme_bw()


#############
# Trying again with explicitly separated eta_ & eta_ab
# (In case the learner added too much correlation)

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
  for (pa in c(.05,.1,.9)) {
    for (ninteractions in c(100)) {
      for (alpha in c(-.5,0,.5)) {
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
          df <- bind_rows(df,list(sd=sd,pa=pa,alpha=alpha,ninteractions=ninteractions,method='model_eta_subpop',value=mean(colMeans(rstan:::extract(fit,"eta_ab_subpop")$eta_ab_subpop-rstan:::extract(fit,"eta_subpop")$eta_subpop))))
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
  mutate(significant=ifelse(xor(sign(ci_lower)==sign(ci_upper),alpha==0),0,1))

df3$significant[sign(df3$mean)!=sign(df3$alpha)&df3$alpha!=0] <- 1

ggplot(aes(x=as.factor(alpha),y=mean,color=as.factor(ninteractions),alpha=significant),data=df3) +
  geom_hline(yintercept=0) + 
  geom_pointrange(aes(ymin=ci_lower,ymax=ci_upper),position = position_dodge(width = 0.3)) + 
  facet_grid(method~pa,scales='free') +
  scale_color_brewer(palette = "Set1") +
  scale_alpha(range=c(.4,1),guide=FALSE) +
  labs(x="true alignment (in logit space)",y="estimated alignment (in method's space)",
       title="Simulated data -- alignment specified in logit space\nmean 5 interactions between each pair, 200 Stan iters, 10 runs",color="# pairs\nobserved") +
  theme_bw()
