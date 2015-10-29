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

addmodelvalues <- function(df,fit) {
  df <- bind_rows(df,list(sd=sd,pa=pa,alpha=alpha,npairs=npairs,method='model_m_pop',value=mean(rstan:::extract(fit,"m_ab_pop")$m_ab_pop)))
  df <- bind_rows(df,list(sd=sd,pa=pa,alpha=alpha,npairs=npairs,method='model_eta_pop',value=mean(colMeans(rstan:::extract(fit,"eta_ab_pop")$eta_ab_pop))))
  df <- bind_rows(df,list(sd=sd,pa=pa,alpha=alpha,npairs=npairs,method='model_eta_subpop',value=mean(colMeans(rstan:::extract(fit,"eta_ab_subpop")$eta_ab_subpop))))
  df <- bind_rows(df,list(sd=sd,pa=pa,alpha=alpha,npairs=npairs,method='model_mu',value=mean(log(colMeans(rstan:::extract(fit,"mu_ab")$mu_ab)) - 
                                                                                                             log(colMeans(rstan:::extract(fit,"mu_notab")$mu_notab)))))
  df <- bind_rows(df,list(sd=sd,pa=pa,alpha=alpha,npairs=npairs,method='model_sub',value=mean(colMeans(rstan:::extract(fit,"mu_ab")$mu_ab) - 
                                                                                                              colMeans(rstan:::extract(fit,"mu_notab")$mu_notab))))
  return(df)
}

###################
# End shared functions
genmsgs <- function(pmean,pa,npairs,maxtrials=20000,cutoff=0) {
  nmsgs <- rgeom(maxtrials,1/pmean)+1
  na <- rbinom(length(nmsgs),nmsgs,pa)
  nna <- nmsgs-na
  goodones <- sample.int(maxtrials,maxtrials,F)
  goodones <- goodones[na[goodones]>cutoff&nna[goodones]>cutoff]
  
  while (npairs>length(goodones)) {
    nmsgs2 <- rgeom(maxtrials,1/pmean)+1
    nmsgs <- c(nmsgs,nmsgs2)
    na <- rbinom(length(nmsgs),nmsgs,pa)
    nna <- nmsgs-na
    goodones <- sample.int(length(nmsgs),length(nmsgs),F)
    goodones <- goodones[na[goodones]>cutoff&nna[goodones]>cutoff]
  }
  
  goodones <- goodones[1:npairs]
  ma <- na[goodones]
  mna <- nna[goodones]
  
  return(list(ma=ma,mna=mna))
}

runsim1 <- function(df,pmean,pa,npairs,alpha,sd,maxtrials=20000,cutoff=0,cdf=FALSE) {
  temp <- genmsgs(pmean,pa,npairs)
  ma <- temp$ma
  mna <- temp$mna
  
  pba <- logit(invlogit(pa)+alpha)
  mba <- rbinom(length(ma),ma,pba)
  mnba <- ma-mba
  mbna <- rbinom(length(mna),mna,pa)
  mnbna <- mna-mbna
  
  dnm <- mba/ma - (mba+mbna)/(ma+mna)
  df <- bind_rows(df,list(sd=sd,pa=pa,alpha=alpha,npairs=npairs,method='DNM',value=mean(dnm)))
  
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
  df <- addmodelvalues(df,fit)
  return(df)
  if (is.data.frame(cdf)) { cdf <- bind_rows(cdf,list(sd=sd,pa=pa,alpha=alpha,npairs=npairs,truecorr=cor(mba/ma,mbna/mna),modelcorr=cor(colMeans(rstan:::extract(fit,"mu_ab")$mu_ab),colMeans(rstan:::extract(fit,"mu_notab")$mu_notab)))) }
  
  if (is.data.frame(cdf)) { return(list(df=df,cdf=cdf)) }
}

df <- data.table(sd=vector(mode="numeric"),
                 pa=vector(mode="numeric"),
                 alpha=vector(mode="numeric"),
                 npairs=vector(mode="numeric"),
                 method=vector(mode="character"),
                 value=vector(mode="numeric"))
cdf <- df
cdf$truecorr <- cdf$value
cdf$modelcorr <- cdf$value
cdf$value <- NULL
mod <- stan_model('alignment.vectorized2.stan')
pmean <- 5
nspeakers <- 50
#cutoff <- 0
#maxtrials <- 20000
for (sd in c(1)) {
  for (pa in c(.05)) {
    for (npairs in c(25)) {
      for (alpha in c(-.5)) {
        for (i in 1:2) {
          df  <- runsim1(df,pmean,pa,npairs,alpha,sd)
        }}}}}

ggplot(aes(x=alpha,y=value,color=as.factor(npairs)),data=df) + geom_hline(yintercept=0) + geom_point() + geom_smooth(method='lm') + facet_grid(method~pa,scales='free')

df2 <- df %>%
  group_by(alpha,pa,sd,npairs,method) %>%
  multi_boot_standard("value", na.rm = T) %>%
  mutate(method=factor(method,c('DNM','model_sub','model_mu','model_eta_subpop','model_eta_pop','model_m_pop')))

ggplot(aes(x=as.factor(alpha),y=mean,color=as.factor(npairs)),data=df2) + geom_hline(yintercept=0) + geom_pointrange(aes(ymin=ci_lower,ymax=ci_upper),position = position_dodge(width = 0.3)) + facet_grid(method~pa,scales='free') +theme_bw()

df3 <- df2 %>% filter(method!='model_m_pop'&method!='model_eta_pop') %>%
  mutate(significant=ifelse(xor(sign(ci_lower)==sign(ci_upper),alpha==0),0,1))

df3$significant[sign(df3$mean)!=sign(df3$alpha)&df3$alpha!=0] <- 1

ggplot(aes(x=as.factor(alpha),y=mean,color=as.factor(npairs),alpha=significant),data=df3) +
  geom_hline(yintercept=0) + 
  geom_pointrange(aes(ymin=ci_lower,ymax=ci_upper),position = position_dodge(width = 0.3)) + 
  facet_grid(method~pa,scales='free') +
  scale_color_brewer(palette = "Set1") +
  scale_alpha(range=c(.4,1),guide=FALSE) +
  labs(x="true alignment (in logit space)",y="estimated alignment (in method's space)",
       title="Simulated data -- alignment specified in logit space\nmean 5 interactions between each pair, 200 Stan iters, 10 runs",color="# pairs\nobserved") +
  theme_bw()

###################
# Sim 1b: Adding user-specific random noise

runsim1b <- function(df,pmean,pa,npairs,alpha,sd,usersd,maxtrials=20000,cutoff=0,cdf=FALSE) {
  pa2 <- logit(rnorm(maxtrials,invlogit(pa),usersd))
  temp <- genmsgs(pmean,pa2,npairs)
  ma <- temp$ma
  mna <- temp$mna
  
  ipb <- rnorm(npairs,invlogit(pa2),sd)
  pba <- logit(ipb+alpha)
  mba <- rbinom(length(ma),ma,pba)
  mnba <- ma-mba
  mbna <- rbinom(length(mna),mna,logit(ipb))
  mnbna <- mna-mbna
  
  dnm <- mba/ma - (mba+mbna)/(ma+mna)
  df <- bind_rows(df,list(sd=sd,pa=pa,alpha=alpha,npairs=npairs,method='DNM',value=mean(dnm)))
  
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
  df <- addmodelvalues(df,fit)
  if (is.data.frame(cdf)) { cdf <- bind_rows(cdf,list(sd=sd,pa=pa,alpha=alpha,npairs=npairs,truecorr=cor(mba/ma,mbna/mna),modelcorr=cor(colMeans(rstan:::extract(fit,"mu_ab")$mu_ab),colMeans(rstan:::extract(fit,"mu_notab")$mu_notab)))) }

  if (is.data.frame(cdf)) { return(list(df=df,cdf=cdf)) }
  else { return(df) }
}


df <- data.table(sd=vector(mode="numeric"),
                 pa=vector(mode="numeric"),
                 alpha=vector(mode="numeric"),
                 npairs=vector(mode="numeric"),
                 method=vector(mode="character"),
                 value=vector(mode="numeric"))
cdf <- df
cdf$truecorr <- cdf$value
cdf$modelcorr <- cdf$value
cdf$value <- NULL
mod <- stan_model('alignment.vectorized3.stan')
pmean <- 5
nspeakers <- 50
#cutoff <- 0
#maxtrials <- 20000
for (sd in c(1)) {
  for (pa in c(.05,.75)) {
    for (npairs in c(100)) {
      for (alpha in c(-.5,.5)) {
        for (i in 1:10) {
          print(c(pmean,pa,npairs,alpha,sd,i))
          temp  <- runsim1b(df,pmean,pa,npairs,alpha,sd,sd,cdf=cdf)
          df <- temp$df
          cdf <- temp$cdf
        }}}}}

save(df,cdf,file = 'results/www2016_results1b.RData')

df3 <- df %>%
  group_by(alpha,pa,sd,npairs,method) %>%
  multi_boot_standard("value", na.rm = T) %>%
  mutate(method=factor(method,c('DNM','model_sub','model_mu','model_eta_subpop','model_eta_pop','model_m_pop'))) %>% 
  filter(method!='model_m_pop'&method!='model_eta_pop') %>%
  mutate(significant=ifelse(xor(sign(ci_lower)==sign(ci_upper),alpha==0),0,1))

df3$significant[sign(df3$mean)!=sign(df3$alpha)&df3$alpha!=0] <- 1

ggplot(aes(x=as.factor(alpha),y=mean,color=as.factor(npairs),alpha=significant),data=df3) +
  geom_hline(yintercept=0) + 
  geom_pointrange(aes(ymin=ci_lower,ymax=ci_upper),position = position_dodge(width = 0.3)) + 
  facet_grid(method~pa,scales='free') +
  scale_color_brewer(palette = "Set1") +
  scale_alpha(range=c(.4,1),guide=FALSE) +
  labs(x="true alignment (in logit space)",y="estimated alignment (in method's space)",color="# pairs\nobserved",
       title="Simulated data -- alignment specified in logit space\nmean 5 interactions between each pair, 200 Stan iters, 10 runs\nmodel with no explicit link between p(B|A) & p(B|notA)") +
  theme_bw()

#Correlation in the model is a big problem:
qplot(x=truecorr,y=modelcorr,data=cdf) + geom_abline(xintercept=0,slope=1)
#Notice even when there *is* correlation between p(B|A) and p(B|notA), the model's still massively overestimating it

##########################################
# Simulation 2: building actual messages


# meanwords is desired mean # of words per tweet, rate is an inverse variance term (scale in a gamma dist)
genmsgs2 <- function(pmean,pa,npairs,meanwords,rate,maxtrials=20000,cutoff=0) {
  nmsgs <- NULL
  goodones <- NULL
  while (npairs>length(goodones)) {
    nmsgs2 <- rgeom(maxtrials,1/pmean)+1
    nmsgs <- c(nmsgs,rgeom(maxtrials,1/pmean)+1)
    nwords <- rgamma(npairs,meanwords*rate,rate)
    nna <- rbinom(length(nmsgs),nmsgs,(1-pa)**nwords)
    na <- nmsgs-nna
    goodones <- sample.int(maxtrials,maxtrials,F)
    goodones <- goodones[na[goodones]>cutoff&nna[goodones]>cutoff]
  }

  goodones <- goodones[1:npairs]
  ma <- na[goodones]
  mna <- nna[goodones]
  
  return(list(ma=ma,mna=mna))
}

runsim2 <- function(df,pmean,pa,npairs,alpha,sd,meanwords,rate,maxtrials=20000,cutoff=0,cdf=FALSE) {
  #pa2 <- logit(rnorm(maxtrials,invlogit(pa),usersd))
  temp <- genmsgs2(pmean,pa,npairs,meanwords,rate)
  ma <- temp$ma
  mna <- temp$mna
  
  nwordsb <- rgamma(npairs,meanwords*rate,rate)
  pba <- logit(invlogit(pa)+alpha)
  mnba <- rbinom(length(ma),ma,(1-pba)**nwordsb)
  mba <- ma-mnba
  mnbna <- rbinom(length(mna),mna,(1-pa)**nwordsb)
  mbna <- mna-mnbna
  
  dnm <- mba/ma - (mba+mbna)/(ma+mna)
  df <- bind_rows(df,list(sd=sd,pa=pa,alpha=alpha,npairs=npairs,method='DNM',value=mean(dnm)))
  
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
  df <- addmodelvalues(df,fit)
  if (is.data.frame(cdf)) { cdf <- bind_rows(cdf,list(sd=sd,pa=pa,alpha=alpha,npairs=npairs,meanwords=meanwords,rate=rate,truecorr=cor(mba/ma,mbna/mna),modelcorr=cor(colMeans(rstan:::extract(fit,"mu_ab")$mu_ab),colMeans(rstan:::extract(fit,"mu_notab")$mu_notab)))) }
  
  if (is.data.frame(cdf)) { return(list(df=df,cdf=cdf)) }
  else { return(df) }
}

df <- data.table(sd=vector(mode="numeric"),
                 pa=vector(mode="numeric"),
                 alpha=vector(mode="numeric"),
                 npairs=vector(mode="numeric"),
                 method=vector(mode="character"),
                 value=vector(mode="numeric"))
cdf <- df
cdf$truecorr <- cdf$value
cdf$modelcorr <- cdf$value
cdf$value <- NULL
mod <- stan_model('alignment.vectorized2.stan')
pmean <- 5
nspeakers <- 50
#cutoff <- 0
#maxtrials <- 20000
for (sd in c(1)) {
  for (pa in c(.005,.05,.2)) {
    for (npairs in c(25,100)) {
      for (alpha in c(-.5,0,.5)) {
        for (meanwords in c(8)) {
          for (rate in c(2)) {
            for (i in 1:10) {
              print(c(pmean,pa,npairs,alpha,sd,i))
              temp  <- runsim2(df,pmean,pa,npairs,alpha,sd,meanwords,rate,cdf=cdf)
              df <- temp$df
              cdf <- temp$cdf
            }}}}}}}

save(df,cdf,file = 'results/www2016_results2.RData')

df3 <- df %>%
  group_by(alpha,pa,sd,npairs,method) %>%
  multi_boot_standard("value", na.rm = T) %>%
  mutate(method=factor(method,c('DNM','model_sub','model_mu','model_eta_subpop','model_eta_pop','model_m_pop'))) %>% 
  filter(method!='model_m_pop'&method!='model_eta_pop') %>%
  mutate(significant=ifelse(xor(sign(ci_lower)==sign(ci_upper),alpha==0),0,1))

df3$significant[sign(df3$mean)!=sign(df3$alpha)&df3$alpha!=0] <- 1

ggplot(aes(x=as.factor(alpha),y=mean,color=as.factor(npairs),alpha=significant),data=df3) +
  geom_hline(yintercept=0) + 
  geom_pointrange(aes(ymin=ci_lower,ymax=ci_upper),position = position_dodge(width = 0.3)) + 
  facet_grid(method~pa,scales='free') +
  scale_color_brewer(palette = "Set1") +
  scale_alpha(range=c(.4,1),guide=FALSE) +
  labs(x="true alignment (in logit space)",y="estimated alignment (in method's space)",color="# pairs\nobserved\n(and mean\nwords)",
       title="Simulated data -- by word\nmean 5 interactions between each pair, 200 Stan iters, 10 runs\nmodel explicitly linking p(B|A) & p(B|notA)") +
  theme_bw()

qplot(x=truecorr,y=modelcorr,data=cdf) + geom_abline(xintercept=0,slope=1) + geom_smooth(method='loess')

#####################
# Simulation 1 results & graph

load('results/www2016_simulations_6.RData')

df3 <- df %>%
  rename(npairs = ninteractions) %>%
  group_by(alpha,pa,sd,npairs,method) %>%
  multi_boot_standard("value", na.rm = T) %>%
  mutate(method=factor(method,c('DNM','model_sub','model_mu','model_eta_subpop','model_eta_pop','model_m_pop'))) %>% 
  filter(method!='model_m_pop'&method!='model_eta_pop'&method!='model_eta_subpop'&method!='model_sub') %>%
  mutate(method=ifelse(method=='model_mu','model','DNM')) %>%
  mutate(significant=ifelse(xor(sign(ci_lower)==sign(ci_upper),alpha==0),0,1))

df3$significant[sign(df3$mean)!=sign(df3$alpha)&df3$alpha!=0] <- 1

dev.off()

pdf('results/www2016_simulation1_simple.pdf',width=10,height=6)
ggplot(aes(x=as.factor(alpha),y=mean,color=as.factor(npairs),alpha=significant),data=df3) +
  geom_hline(yintercept=0) + 
  geom_pointrange(aes(ymin=ci_lower,ymax=ci_upper),position = position_dodge(width = 0.3)) + 
  facet_grid(method~pa,scales='free') +
  scale_color_brewer(palette = "Set1") +
  scale_alpha(range=c(.4,1),guide=FALSE) +
  labs(x="true alignment (in logit space)",y="estimated alignment (in method's space)",color="# pairs\nobserved",
       title="Estimated alignments for tweet-based simulation\n(with person-specific baselines)") +
  theme_bw()
dev.off()


######################
# Simulation 1b results & graph

load('results/www2016_results1b.RData') #actually, this is an incomplete sample of the results

df3 <- df %>%
  rename(npairs = ninteractions) %>%
  group_by(alpha,pa,sd,npairs,method) %>%
  multi_boot_standard("value", na.rm = T) %>%
  mutate(method=factor(method,c('DNM','model_sub','model_mu','model_eta_subpop','model_eta_pop','model_m_pop'))) %>% 
  filter(method!='model_m_pop'&method!='model_eta_pop'&method!='model_eta_subpop'&method!='model_sub') %>%
  mutate(method=ifelse(method=='model_mu','model','DNM')) %>%
  mutate(significant=ifelse(xor(sign(ci_lower)==sign(ci_upper),alpha==0),0,1))

df3$significant[sign(df3$mean)!=sign(df3$alpha)&df3$alpha!=0] <- 1

dev.off()

pdf('results/www2016_simulation1b_simple.pdf')
ggplot(aes(x=as.factor(alpha),y=mean,color=as.factor(npairs),alpha=significant),data=df3) +
  geom_hline(yintercept=0) + 
  geom_pointrange(aes(ymin=ci_lower,ymax=ci_upper),position = position_dodge(width = 0.3)) + 
  facet_grid(method~pa,scales='free') +
  scale_color_brewer(palette = "Set1") +
  scale_alpha(range=c(.4,1),guide=FALSE) +
  labs(x="true alignment (in logit space)",y="estimated alignment (in method's space)",color="# pairs\nobserved",
       title="Estimated alignments for tweet-based simulation\n(with person-specific baselines)") +
  theme_bw()
dev.off()

######################
# Simulation 2 results & graph

load('results/www2016_results2.RData')

df3 <- df %>%
  group_by(alpha,pa,sd,npairs,method) %>%
  multi_boot_standard("value", na.rm = T) %>%
  mutate(method=factor(method,c('DNM','model_sub','model_mu','model_eta_subpop','model_eta_pop','model_m_pop'))) %>% 
  filter(method!='model_m_pop'&method!='model_eta_pop'&method!='model_eta_subpop'&method!='model_sub') %>%
  mutate(method=ifelse(method=='model_mu','model','DNM')) %>%
  mutate(significant=ifelse(xor(sign(ci_lower)==sign(ci_upper),alpha==0),0,1))

df3$significant[sign(df3$mean)!=sign(df3$alpha)&df3$alpha!=0] <- 1

dev.off()

pdf('results/www2016_simulation2_simple.pdf',width=10,height=6)
ggplot(aes(x=as.factor(alpha),y=mean,color=as.factor(npairs),alpha=significant),data=df3) +
  geom_hline(yintercept=0) + 
  geom_pointrange(aes(ymin=ci_lower,ymax=ci_upper),position = position_dodge(width = 0.3)) + 
  facet_grid(method~pa,scales='free') +
  scale_color_brewer(palette = "Set1") +
  scale_alpha(range=c(.4,1),guide=FALSE) +
  labs(x="true alignment (in logit space)",y="estimated alignment (in method's space)",color="# pairs\nobserved",
       title="Estimated alignments for word-based simulation") +
  theme_bw()
dev.off()


##########
# Code graveyard
