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

addmodelvalues_notab <- function(df,fit) {
  #df <- bind_rows(df,list(sd=sd,pa=pa,alpha=alpha,npairs=npairs,method='model_m_pop',value=mean(rstan:::extract(fit,"m_ab_pop")$m_ab_pop)))
  #df <- bind_rows(df,list(sd=sd,pa=pa,alpha=alpha,npairs=npairs,method='model_eta_pop',value=mean(colMeans(rstan:::extract(fit,"eta_ab_pop")$eta_ab_pop))))
  df <- bind_rows(df,list(sd=sd,pa=pa,alpha=alpha,npairs=npairs,method='model_eta',value=mean(colMeans(rstan:::extract(fit,"eta_subpop")$eta_subpop))))
  df <- bind_rows(df,list(sd=sd,pa=pa,alpha=alpha,npairs=npairs,method='model_eta_ab',value=mean(colMeans(rstan:::extract(fit,"eta_ab_subpop")$eta_ab_subpop))))
  df <- bind_rows(df,list(sd=sd,pa=pa,alpha=alpha,npairs=npairs,method='model_mu',value=mean(log(colMeans(rstan:::extract(fit,"mu_ab")$mu_ab)) - 
                                                                                               log(colMeans(rstan:::extract(fit,"mu_notab")$mu_notab)))))
  df <- bind_rows(df,list(sd=sd,pa=pa,alpha=alpha,npairs=npairs,method='model_sub',value=mean(colMeans(rstan:::extract(fit,"mu_ab")$mu_ab) - 
                                                                                                colMeans(rstan:::extract(fit,"mu_notab")$mu_notab))))
  return(df)
}

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

runsim1_variance <- function(pmean,pa,npairs,alpha,sd,maxtrials=20000,cutoff=0,cdf=FALSE) {
  temp <- genmsgs(pmean,pa,npairs)
  ma <- temp$ma
  mna <- temp$mna
  
  pba <- logit(invlogit(pa)+alpha)
  mba <- rbinom(length(ma),ma,pba)
  mnba <- ma-mba
  mbna <- rbinom(length(mna),mna,pa)
  mnbna <- mna-mbna
  
  dnm <- mba/ma - (mba+mbna)/(ma+mna)
  mean <- mean(dnm)
  df <- as.data.frame(mean) %>%
    mutate(pmean=pmean,sd=sd,pa=pa,alpha=alpha,npairs=npairs,method='DNM0')
  
  mean <- mean(dnm[(ma+mna)>=10])
  df <- as.data.frame(mean) %>%
    mutate(pmean=pmean,sd=sd,pa=pa,alpha=alpha,npairs=npairs,method='DNM10') %>%
    bind_rows(df)
  
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
                  iter = 200, chains = 1)
  
  mean <- mean(rstan:::extract(fit,"eta_ab_subpop")$eta_ab_subpop)
  #colnames(a) <- c("iteration","subpop","marker","model_eta")
  
  markeraligns <- as.data.frame(mean) %>%
    mutate(pmean=pmean,sd=sd,pa=pa,alpha=alpha,npairs=npairs,method="model_eta") %>%
    bind_rows(df)
  
  return(markeraligns)
}

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

runsim2_crossiter <- function(pmean,pa,npairs,alpha,sd,meanwords,rate,maxtrials=20000,cutoff=0,cdf=FALSE) {
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
  df <- as.data.frame(dnm) %>%
    mutate(sd=sd,pa=pa,alpha=alpha,npairs=npairs,meanwords=meanwords,rate=rate,method='DNM') %>%
    group_by(sd,pa,alpha,npairs,method) %>%
    multi_boot_standard("dnm")
  
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
  
  a <- melt(rstan:::extract(fit,"eta_ab_subpop")$eta_ab_subpop)
  colnames(a) <- c("iteration","subpop","marker","model_eta")
  
  markeraligns <- a %>% group_by(marker) %>% multi_boot_standard("model_eta",nboot=100)
  markeraligns %<>% mutate(sd=sd,pa=pa,alpha=alpha,npairs=npairs,meanwords=meanwords,rate=rate,method="model_eta") %>%
    bind_rows(df)
  
  return(markeraligns)
}

#######




###############
# End shared fns

##############
# Simulation 1: per-tweet probability (with uniform eta_notab distribution)
# Using cross-iteration bootstrapping

df <- data.table(sd=vector(mode="numeric"),
                 pa=vector(mode="numeric"),
                 alpha=vector(mode="numeric"),
                 npairs=vector(mode="numeric"),
                 method=vector(mode="character"),
                 mean=vector(mode="numeric"),
                 ci_lower=vector(mode="numeric"),
                 ci_upper=vector(mode="numeric"))
mod <- stan_model('alignment.vectorized5.stan')
sd <- .5
for (npairs in c(250)) {
  for (pa in c(.5)) {
    for (pmean in c(5,10,20)) {
      for (alpha in c(.5)) {
        for (i in c(1:5)) {
          print(c(npairs,pa,pmean,alpha,i))
          df <- bind_rows(df,runsim1_variance(pmean,pa,npairs,alpha,sd))
        }}}}}

save(df,file="results/www2016_simulation1_variance.RData")

df$method[df$method=='model_eta'] <- 'model'

ggplot(aes(x=log10(npairs),y=mean,color=as.factor(alpha)),data=df) + geom_point() + geom_smooth(method='lm') + facet_grid(pmean~method)

df2 <- df %>% filter(!is.na(mean)) %>% group_by(pmean,pa,alpha,npairs,method) %>%
  summarize(sd=sd(mean),mean=mean(mean))

df3 <- df2 %>% group_by(pmean,pa,alpha,method) %>%
  summarize(min=min(sd)) %>%
  left_join(df2) %>%
  filter(alpha==0.5)

pdf('results/www2016_varianceplot_test_bynpairs.pdf')
ggplot(aes(x=npairs,y=sd/min,color=as.factor(pmean)),data=df3) + geom_point() +
  geom_smooth(method='lm',formula=y ~ log10(x),se=F) +
  facet_grid(~method) + 
  labs(x="Number of pairs",color="Mean messages\nper pair")
dev.off()

df3 <- df2 %>% group_by(npairs,pa,alpha,method) %>%
  summarize(min=min(sd)) %>%
  left_join(df2) %>%
  filter(alpha==0.5)

pdf('results/www2016_varianceplot_test_bynmsgs.pdf')
ggplot(aes(x=pmean,y=sd/min,color=as.factor(npairs)),data=df3) + geom_point() +
  geom_smooth(method='lm',formula=y ~ log10(x),se=F) +
  facet_grid(~method) + 
  labs(color="Number of pairs",x="Mean messages per pair")
dev.off()

pdf('results/www2016_simulation1_crossiter.pdf',width=5,height=5)
ggplot(aes(x=alpha,y=mean,color=as.factor(pa)),data=df) +
  geom_pointrange(aes(ymin=ci_lower,ymax=ci_upper),position=position_dodge(width=0.1)) + 
  geom_smooth(method='loess',se=FALSE) +
  theme_bw(base_size = 25) +
  facet_grid(method~npairs,scales='free') +
  #scale_color_brewer(palette="BrBG") +
  scale_color_manual(values=c('#fec44f','#fe9929','#ec7014','#cc4c02','#8c2d04')) +
  #scale_color_gradient(low="#003333",high="#BBEEEE") +
  #scale_alpha_discrete(range=c(.4,1),guide=FALSE) +
  labs(x="true alignment (in logit space)",y="estimated alignment (in method's space)",color="  marker\nfrequency") +
  theme_bw()
dev.off()

#######################
# Simulation 2: per-word simulations (with uniform eta_notab distribution)

load("results/www2016_simulation2_uniform.RData")


df <- data.table(sd=vector(mode="numeric"),
                 pa=vector(mode="numeric"),
                 alpha=vector(mode="numeric"),
                 npairs=vector(mode="numeric"),
                 method=vector(mode="character"),
                 mean=vector(mode="numeric"),
                 ci_lower=vector(mode="numeric"),
                 ci_upper=vector(mode="numeric"),
                 meanwords=vector(mode="numeric"),
                 rate=vector(mode="numeric"))
mod <- stan_model('alignment.vectorized5.stan')
pmean <- 5
meanwords <- 8
rate <- 2
for (sd in c(.5)) {
  for (pa in c(.2,.1,.05,.02,.01)) {
    for (npairs in c(100)) {
      for (alpha in seq(-1.5,1.5,.25)) {
        for (i in c(1)) {
          print(c(pmean,sd,pa,npairs,alpha,i))
          df <- bind_rows(df,runsim2_crossiter(pmean,pa,npairs,alpha,sd,meanwords,rate))
        }}}}}

save(df,file="results/www2016_simulation2_crossiter.RData")

df3 <- df %>%
  mutate(significant=ifelse(xor(sign(ci_lower)==sign(ci_upper),alpha==0),0,1))

df3$significant[sign(df3$mean)!=sign(df3$alpha)&df3$alpha!=0] <- 1

pdf('results/www2016_simulation2_crossiter.pdf')
ggplot(aes(x=alpha,y=mean,color=as.factor(pa)),data=df3) +
  geom_pointrange(aes(ymin=ci_lower,ymax=ci_upper),position=position_dodge(width=0.1)) + 
  geom_smooth(method='loess',se=FALSE) +
  facet_grid(method~.,scales='free') +
  scale_color_brewer(palette="BrBG") +
  #scale_color_gradient(low="#003333",high="#BBEEEE") +
  #scale_alpha_discrete(range=c(.4,1),guide=FALSE) +
  labs(x="true alignment (in logit space)",y="estimated alignment (in method's space)",color="  marker\nfrequency",
       title="Simulated data by-word -- alignment specified in logit space\nmean 5 interactions between 100 pairs, 200 Stan iters") +
  theme_bw()
dev.off()

###########################
# Simulation 2b: actually doing word-by-word generation

runsim2b_crossiter <- function(pmean,pa,npairs,alpha,sd,minwords,maxwords,maxtrials=2000,cutoff=0,cdf=FALSE) {
  #pa2 <- logit(rnorm(maxtrials,invlogit(pa),usersd))
  nmsgs <- NULL
  goodones <- NULL
  tdf <- NULL
  repeat {
    nmsgs2 <- rgeom(maxtrials,1/pmean)+1
    ba <- rep(0,maxtrials)
    bna <- rep(0,maxtrials)
    nba <- rep(0,maxtrials)
    nbna <- rep(0,maxtrials)
    lsm <- rep(0,maxtrials)
    lla <- rep(0,maxtrials)
    for (pairnum in seq(1,length(nmsgs2))) {
      nmsg <- nmsgs2[pairnum]
      if (nmsg>1) {
        nwordsa <- round(runif(nmsg,minwords,maxwords))
        nwordsb <- round(runif(nmsg,minwords,maxwords))
        ca <- rbinom(length(nwordsa),nwordsa,pa)
        if (sum(ca>0)&sum(ca==0)) {
          cb <- rbinom(length(nwordsb),nwordsb,logit((ca>0)*alpha+invlogit(pa)))
          ba[pairnum] <- sum(cb>0&ca>0)
          bna[pairnum] <- sum(cb>0&ca==0)
          nba[pairnum] <- sum(cb==0&ca>0)
          nbna[pairnum] <- sum(cb==0&ca==0)
          pa1 <- sum(ca)/sum(nwordsa)
          pb1 <- sum(cb)/sum(nwordsb)
          lsm[pairnum] <- 1 - abs(pa1-pb1)/(pa1+pb1)
          lla[pairnum] <- sum((cb*(ca>0))/(nwordsa*nwordsb))
        }
      }
    }
    tdf <- data.frame(ba,nba,bna,nbna,lsm,lla) %>%
      mutate(na=ba+nba,nna=bna+nbna,dnm=(ba/(ba+nba)-(ba+bna)/(ba+bna+nba+nbna))) %>%
      filter(na>0&nna>0) %>%
      bind_rows(tdf)
    if (nrow(tdf)>=npairs) {
      tdf %<>% sample_n(npairs)
      break
    }
  }  
  tdf$temp <- 1
  
  dnmdf <- tdf %>% group_by(temp) %>%
    multi_boot_standard("dnm",nboot=100) %>%
    select(-temp) %>%
    mutate(method='DNM')
  
  lsmdf <- tdf %>% group_by(temp) %>%
    multi_boot_standard("lsm",nboot=100) %>%
    select(-temp) %>%
    mutate(method='LSM')
  
  lladf <- tdf %>% group_by(temp) %>%
    multi_boot_standard("lla",nboot=100) %>%
    select(-temp) %>%
    mutate(method='LLA')
  
  df <- bind_rows(dnmdf,lsmdf) %>%
    bind_rows(lladf) %>%
    mutate(sd=sd,pa=pa,alpha=alpha,npairs=npairs,minwords=minwords,maxwords=maxwords)
  
  alignment_data <- list(NumMarkers=1,
                         NumSubPops = 1,
                         NumObservations = npairs,
                         SpeakerSubPop = rep(1,npairs),
                         MarkerType = rep(1,npairs),
                         NumUtterancesAB = tdf$na,
                         NumUtterancesNotAB = tdf$nna,
                         CountsAB = tdf$ba,
                         CountsNotAB = tdf$bna,
                         StdDev = sd)
  fit <- sampling(mod, data = alignment_data, 
                  iter = 200, chains =1 )
  
  a <- melt(rstan:::extract(fit,"eta_ab_subpop")$eta_ab_subpop)
  colnames(a) <- c("iteration","subpop","marker","model_eta")
  
  markeraligns <- a %>% group_by(marker) %>% multi_boot_standard("model_eta",nboot=100)
  markeraligns %<>% mutate(sd=sd,pa=pa,alpha=alpha,npairs=npairs,minwords=minwords,maxwords=maxwords,method="model") %>%
    bind_rows(df)
  
  return(markeraligns)
}

df <- data.table(sd=vector(mode="numeric"),
                 pa=vector(mode="numeric"),
                 alpha=vector(mode="numeric"),
                 npairs=vector(mode="numeric"),
                 method=vector(mode="character"),
                 mean=vector(mode="numeric"),
                 ci_lower=vector(mode="numeric"),
                 ci_upper=vector(mode="numeric"))
mod <- stan_model('alignment.vectorized5.stan')
pmean <- 2
minwords <- 1
maxwords <- 25
for (sd in c(.5)) {
  for (pa in c(.005,.01,.05,.1,.2)) {
    for (npairs in c(500)) {
      for (alpha in seq(-1.25,1.25,.5)) {
        for (i in c(1:5)) {
          print(c(pmean,sd,pa,npairs,alpha,i))
          df <- bind_rows(df,runsim2b_crossiter(pmean,pa,npairs,alpha,sd,minwords,maxwords))
        }}}}}

save(df,file="results/www2016_simulation2b_crossiter.RData")

pdf('results/www2016_simulation2b_crossiter.pdf',width=5,height=6)
ggplot(aes(x=alpha,y=mean,color=as.factor(pa)),data=df) +
  geom_pointrange(aes(ymin=ci_lower,ymax=ci_upper),position=position_dodge(width=0.1)) + 
  geom_smooth(method='loess',se=FALSE) +
  theme_bw(base_size = 25) +
  facet_grid(method~.,scales='free') +
  #scale_color_brewer(palette="BrBG") +
  scale_color_manual(values=c('#fec44f','#fe9929','#ec7014','#cc4c02','#8c2d04')) +
  #scale_color_gradient(low="#003333",high="#BBEEEE") +
  #scale_alpha_discrete(range=c(.4,1),guide=FALSE) +
  labs(x="true alignment (in log-odds space)",y="estimated alignment (in method's space)",color="  marker\nfrequency") +
  theme_bw()
dev.off()

#######################
# Re-running Simulation 1 to investigate the effect of the cutoff

runsim1_cutoff <- function(pmean,pa,npairs,alpha,sd,maxtrials=20000,cutoff=0,cdf=FALSE) {
  temp <- genmsgs(pmean,pa,npairs)
  ma <- temp$ma
  mna <- temp$mna
  
  pba <- logit(invlogit(pa)+alpha)
  mba <- rbinom(length(ma),ma,pba)
  mnba <- ma-mba
  mbna <- rbinom(length(mna),mna,pa)
  mnbna <- mna-mbna
  
  dnm <- mba/ma - (mba+mbna)/(ma+mna)
  df <- as.data.frame(dnm) %>%
    mutate(sd=sd,pa=pa,alpha=alpha,npairs=npairs,method='DNM0') %>%
    group_by(sd,pa,alpha,npairs,method) %>%
    multi_boot_standard("dnm",nboot=100)
  
  df <- as.data.frame(dnm) %>%
    mutate(sd=sd,pa=pa,alpha=alpha,npairs=npairs,method='DNM10',n=ma+mna) %>%
    filter(n>=10) %>%
    group_by(sd,pa,alpha,npairs,method) %>%
    multi_boot_standard("dnm",nboot=100) %>%
    bind_rows(df)
  
  return(df)
}

df <- data.table(sd=vector(mode="numeric"),
                 pa=vector(mode="numeric"),
                 alpha=vector(mode="numeric"),
                 npairs=vector(mode="numeric"),
                 method=vector(mode="character"),
                 mean=vector(mode="numeric"),
                 ci_lower=vector(mode="numeric"),
                 ci_upper=vector(mode="numeric"))
mod <- stan_model('alignment.vectorized5.stan')
pmean <- 5
for (sd in c(.5)) {
  for (pa in c(.1,.3,.5,.7,.9)) {
    for (npairs in c(500)) {
      for (alpha in seq(-1.5,1.5,.25)) {
        for (i in c(1:5)) {
          print(c(pmean,sd,pa,npairs,alpha,i))
          df <- bind_rows(df,runsim1_cutoff(pmean,pa,npairs,alpha,sd))
        }}}}}

save(df,file="results/www2016_simulation1_cutoff.RData")

#df$method[df$method=='model_eta'] <- 'model'

pdf('results/www2016_simulation1_cutoff_bias.pdf',width=5,height=5)
df2 <- df %>% mutate(method=as.factor(method),pa = as.factor(pa))
ggplot(aes(x=alpha,y=mean,color=method),data=df2) +
  geom_pointrange(aes(ymin=ci_lower,ymax=ci_upper),position=position_dodge(width=0.1)) + 
  geom_smooth(method='loess',se=FALSE) +
  theme_bw(base_size = 25) +
  facet_wrap(~pa) +
  #scale_color_brewer(palette="BrBG") +
  scale_color_manual(values=c('#fe9929','#cc4c02')) +
  #scale_color_gradient(low="#003333",high="#BBEEEE") +
  #scale_alpha_discrete(range=c(.4,1),guide=FALSE) +
  labs(x="true alignment (in logit space)",y="estimated alignment (in method's space)",color="  marker\nfrequency") +
  theme_bw()
dev.off()

pdf('results/www2016_simulation1_cutoff_variance.pdf',width=5,height=5)
df2 <- df %>% mutate(method=as.factor(method),pa = as.factor(pa))
ggplot(aes(x=alpha,y=(ci_upper-ci_lower),color=method),data=df2) +
  geom_point(position=position_dodge(width=0.1)) +
  #geom_pointrange(aes(ymin=ci_lower,ymax=ci_upper),position=position_dodge(width=0.1)) + 
  geom_smooth(method='loess',se=FALSE) +
  theme_bw(base_size = 25) +
  facet_wrap(~pa) +
  #scale_color_brewer(palette="BrBG") +
  scale_color_manual(values=c('#fe9929','#cc4c02')) +
  #scale_color_gradient(low="#003333",high="#BBEEEE") +
  #scale_alpha_discrete(range=c(.4,1),guide=FALSE) +
  labs(x="true alignment (in logit space)",y="estimated alignment (in method's space)",color="  marker\nfrequency") +
  theme_bw()
dev.off()

# Sim 2

runsim2b_cutoff <- function(pmean,pa,npairs,alpha,sd,minwords,maxwords,maxtrials=2000,cutoff=0,cdf=FALSE) {
  #pa2 <- logit(rnorm(maxtrials,invlogit(pa),usersd))
  nmsgs <- NULL
  goodones <- NULL
  tdf <- NULL
  repeat {
    nmsgs2 <- rgeom(maxtrials,1/pmean)+1
    ba <- rep(0,maxtrials)
    bna <- rep(0,maxtrials)
    nba <- rep(0,maxtrials)
    nbna <- rep(0,maxtrials)
    lsm <- rep(0,maxtrials)
    lla <- rep(0,maxtrials)
    for (pairnum in seq(1,length(nmsgs2))) {
      nmsg <- nmsgs2[pairnum]
      if (nmsg>1) {
        nwordsa <- round(runif(nmsg,minwords,maxwords))
        nwordsb <- round(runif(nmsg,minwords,maxwords))
        ca <- rbinom(length(nwordsa),nwordsa,pa)
        if (sum(ca>0)&sum(ca==0)) {
          cb <- rbinom(length(nwordsb),nwordsb,logit((ca>0)*alpha+invlogit(pa)))
          ba[pairnum] <- sum(cb>0&ca>0)
          bna[pairnum] <- sum(cb>0&ca==0)
          nba[pairnum] <- sum(cb==0&ca>0)
          nbna[pairnum] <- sum(cb==0&ca==0)
          pa1 <- sum(ca)/sum(nwordsa)
          pb1 <- sum(cb)/sum(nwordsb)
          lsm[pairnum] <- 1 - abs(pa1-pb1)/(pa1+pb1)
          lla[pairnum] <- sum((cb*(ca>0))/(nwordsa*nwordsb))
        }
      }
    }
    tdf <- data.frame(ba,nba,bna,nbna,lsm,lla) %>%
      mutate(na=ba+nba,nna=bna+nbna,dnm=(ba/(ba+nba)-(ba+bna)/(ba+bna+nba+nbna))) %>%
      filter(na>0&nna>0) %>%
      bind_rows(tdf)
    if (nrow(tdf)>=npairs) {
      tdf %<>% sample_n(npairs)
      break
    }
  }  
  tdf$temp <- 1
  
  dnmdf <- tdf %>% group_by(temp) %>%
    multi_boot_standard("dnm",nboot=100) %>%
    select(-temp) %>%
    mutate(method='DNM0')
  
  dnm10df <- tdf %>% group_by(temp) %>%
    filter((na+nna)>=10) %>%
    multi_boot_standard("dnm",nboot=100) %>%
    select(-temp) %>%
    mutate(method='DNM10')
  
  lsmdf <- tdf %>% group_by(temp) %>%
    multi_boot_standard("lsm",nboot=100) %>%
    select(-temp) %>%
    mutate(method='LSM')
  
  lladf <- tdf %>% group_by(temp) %>%
    multi_boot_standard("lla",nboot=100) %>%
    select(-temp) %>%
    mutate(method='LLA')
  
  df <- bind_rows(dnmdf,dnm10df) %>%
    bind_rows(lsmdf) %>%
    bind_rows(lladf) %>%
    mutate(sd=sd,pa=pa,alpha=alpha,npairs=npairs,minwords=minwords,maxwords=maxwords)
  
  return(df)
  
#   alignment_data <- list(NumMarkers=1,
#                          NumSubPops = 1,
#                          NumObservations = npairs,
#                          SpeakerSubPop = rep(1,npairs),
#                          MarkerType = rep(1,npairs),
#                          NumUtterancesAB = tdf$na,
#                          NumUtterancesNotAB = tdf$nna,
#                          CountsAB = tdf$ba,
#                          CountsNotAB = tdf$bna,
#                          StdDev = sd)
#   fit <- sampling(mod, data = alignment_data, 
#                   iter = 200, chains =1 )
#   
#   a <- melt(rstan:::extract(fit,"eta_ab_subpop")$eta_ab_subpop)
#   colnames(a) <- c("iteration","subpop","marker","model_eta")
#   
#   markeraligns <- a %>% group_by(marker) %>% multi_boot_standard("model_eta",nboot=100)
#   markeraligns %<>% mutate(sd=sd,pa=pa,alpha=alpha,npairs=npairs,minwords=minwords,maxwords=maxwords,method="model") %>%
#     bind_rows(df)
#   
#   return(markeraligns)
}

df <- data.table(sd=vector(mode="numeric"),
                 pa=vector(mode="numeric"),
                 alpha=vector(mode="numeric"),
                 npairs=vector(mode="numeric"),
                 method=vector(mode="character"),
                 mean=vector(mode="numeric"),
                 ci_lower=vector(mode="numeric"),
                 ci_upper=vector(mode="numeric"))
mod <- stan_model('alignment.vectorized5.stan')
pmean <- 2
minwords <- 1
maxwords <- 25
for (sd in c(.5)) {
  for (pa in c(.005,.01,.05,.1,.2)) {
    for (npairs in c(500)) {
      for (alpha in seq(-1.25,1.25,.5)) {
        for (i in c(1:5)) {
          print(c(pmean,sd,pa,npairs,alpha,i))
          df <- bind_rows(df,runsim2b_cutoff(pmean,pa,npairs,alpha,sd,minwords,maxwords))
        }}}}}

save(df,file="results/www2016_simulation2b_cutoff.RData")

pdf('results/www2016_simulation2b_cutoff_bias.pdf',width=5,height=5)
df2 <- df %>% mutate(method=as.factor(method),pa = as.factor(pa))
ggplot(aes(x=alpha,y=mean,color=method),data=df2) +
  geom_pointrange(aes(ymin=ci_lower,ymax=ci_upper),position=position_dodge(width=0.1)) + 
  geom_smooth(method='loess',se=FALSE) +
  theme_bw(base_size = 25) +
  facet_wrap(~pa) +
  scale_color_brewer(palette="Set1") +
  #scale_color_manual(values=c('#fe9929','#cc4c02')) +
  #scale_color_gradient(low="#003333",high="#BBEEEE") +
  #scale_alpha_discrete(range=c(.4,1),guide=FALSE) +
  labs(x="true alignment (in logit space)",y="estimated alignment (in method's space)",color="  marker\nfrequency") +
  theme_bw()
dev.off()

pdf('results/www2016_simulation2b_cutoff_variance.pdf',width=5,height=5)
df2 <- df %>% mutate(method=as.factor(method),pa = as.factor(pa))
ggplot(aes(x=alpha,y=(ci_upper-ci_lower),color=method),data=df2) +
  geom_point(position=position_dodge(width=0.1)) +
  #geom_pointrange(aes(ymin=ci_lower,ymax=ci_upper),position=position_dodge(width=0.1)) + 
  geom_smooth(method='loess',se=FALSE) +
  theme_bw(base_size = 25) +
  facet_wrap(~pa) +
  scale_color_brewer(palette="Set1") +
  #scale_color_manual(values=c('#fe9929','#cc4c02')) +
  #scale_color_gradient(low="#003333",high="#BBEEEE") +
  #scale_alpha_discrete(range=c(.4,1),guide=FALSE) +
  labs(x="true alignment (in logit space)",y="estimated alignment (in method's space)",color="  marker\nfrequency") +
  theme_bw()
dev.off()

