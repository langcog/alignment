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
  df <- addmodelvalues_notab(df,fit)
  return(df)
  if (is.data.frame(cdf)) { cdf <- bind_rows(cdf,list(sd=sd,pa=pa,alpha=alpha,npairs=npairs,truecorr=cor(mba/ma,mbna/mna),modelcorr=cor(colMeans(rstan:::extract(fit,"mu_ab")$mu_ab),colMeans(rstan:::extract(fit,"mu_notab")$mu_notab)))) }
  
  if (is.data.frame(cdf)) { return(list(df=df,cdf=cdf)) }
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
  df <- addmodelvalues_notab(df,fit)
  if (is.data.frame(cdf)) { cdf <- bind_rows(cdf,list(sd=sd,pa=pa,alpha=alpha,npairs=npairs,meanwords=meanwords,rate=rate,truecorr=cor(mba/ma,mbna/mna),modelcorr=cor(colMeans(rstan:::extract(fit,"mu_ab")$mu_ab),colMeans(rstan:::extract(fit,"mu_notab")$mu_notab)))) }
  
  if (is.data.frame(cdf)) { return(list(df=df,cdf=cdf)) }
  else { return(df) }
}

###############
# End shared fns

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
for (sd in c(.1,.2,.3,.5,.7,1)) {
  for (pa in c(.05,.1,.9)) {
    for (npairs in c(100)) {
      for (alpha in c(-1,0,1)) {
        for (i in 1:10) {
          df  <- runsim1(df,pmean,pa,npairs,alpha,sd)
        }}}}}

save(df,file="results/www2016_simulation1_etas.RData")

#ggplot(aes(x=alpha,y=value,color=as.factor(npairs)),data=df) + geom_hline(yintercept=0) + geom_point() + geom_smooth(method='lm') + facet_grid(method~pa,scales='free')

#converting the model_eta value to an error-from-baseline value 
df2 <- df
df2$value[df2$method=='model_eta'] <- logit(df2$value[df2$method=='model_eta'])-df2$pa[df2$method=='model_eta']

df2 %<>%
  group_by(alpha,pa,sd,npairs,method) %>%
  multi_boot_standard("value", na.rm = T) %>%
  mutate(method=factor(method,c('DNM','model_sub','model_mu','model_eta_ab','model_eta')))

#ggplot(aes(x=as.factor(alpha),y=mean,color=as.factor(sd)),data=df2) + geom_hline(yintercept=0) + geom_pointrange(aes(ymin=ci_lower,ymax=ci_upper),position = position_dodge(width = 0.3)) + facet_grid(method~pa,scales='free') +theme_bw()

df3 <- df2 %>% filter(method!='model_m_pop'&method!='model_eta_pop') %>%
  mutate(significant=ifelse(xor(sign(ci_lower)==sign(ci_upper),alpha==0),0,1))

df3$significant[sign(df3$mean)!=sign(df3$alpha)&df3$alpha!=0] <- 1

pdf('results/www2016_simulation1_etas.pdf')
ggplot(aes(x=as.factor(alpha),y=mean,color=as.factor(sd),alpha=significant),data=df3) +
  geom_hline(yintercept=0) + 
  geom_pointrange(aes(ymin=ci_lower,ymax=ci_upper),position = position_dodge(width = 0.3)) + 
  facet_grid(method~pa,scales='free') +
  scale_color_brewer(palette = "Dark2") +
  scale_alpha(range=c(.4,1),guide=FALSE) +
  labs(x="true alignment (in logit space)",y="estimated alignment (in method's space)",color="model SD",
       title="Simulated data -- alignment specified in logit space\nmean 5 interactions between each pair, 200 Stan iters, 10 runs") +
  theme_bw()
dev.off()


##############
# Simulation 1: per-tweet probability (with uniform eta_notab distribution)

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
mod <- stan_model('alignment.vectorized4.stan')
pmean <- 5
nspeakers <- 50
#cutoff <- 0
#maxtrials <- 20000
for (sd in c(.5)) {
  for (pa in c(.1,.3,.5,.7,.9)) {
    for (npairs in c(100)) {
      for (alpha in c(-1.5,-1.25,1.25,1.5)) {
        for (i in 1:5) {
          print(c(pmean,sd,pa,npairs,alpha,i))
          df  <- runsim1(df,pmean,pa,npairs,alpha,sd)
        }}}}}

save(df,file="results/www2016_simulation1_etas_uniform.RData")

#ggplot(aes(x=alpha,y=value,color=as.factor(npairs)),data=df) + geom_hline(yintercept=0) + geom_point() + geom_smooth(method='lm') + facet_grid(method~pa,scales='free')

#converting the model_eta value to an error-from-baseline value 
df2 <- df
df2$value[df2$method=='model_eta'] <- logit(df2$value[df2$method=='model_eta'])-df2$pa[df2$method=='model_eta']

df2 %<>%
  group_by(alpha,pa,sd,npairs,method) %>%
  filter(sd==0.5) %>%
  multi_boot_standard("value", na.rm = T) %>%
  mutate(method=factor(method,c('DNM','model_sub','model_mu','model_eta_ab','model_eta')))

#ggplot(aes(x=as.factor(alpha),y=mean,color=as.factor(sd)),data=df2) + geom_hline(yintercept=0) + geom_pointrange(aes(ymin=ci_lower,ymax=ci_upper),position = position_dodge(width = 0.3)) + facet_grid(method~pa,scales='free') +theme_bw()

df3 <- df2 %>% filter(method!='model_m_pop'&method!='model_eta_pop') %>%
  mutate(significant=ifelse(xor(sign(ci_lower)==sign(ci_upper),alpha==0),0,1))

df3$significant[sign(df3$mean)!=sign(df3$alpha)&df3$alpha!=0] <- 1

pdf('results/www2016_simulation1_etas_uniform.pdf')
ggplot(aes(x=as.factor(alpha),y=mean,color=as.factor(sd),alpha=significant),data=df3) +
  geom_hline(yintercept=0) + 
  geom_pointrange(aes(ymin=ci_lower,ymax=ci_upper),position = position_dodge(width = 0.3)) + 
  facet_grid(method~pa,scales='free') +
  scale_color_brewer(palette = "Dark2") +
  scale_alpha(range=c(.4,1),guide=FALSE) +
  labs(x="true alignment (in logit space)",y="estimated alignment (in method's space)",color="model SD",
       title="Simulated data -- alignment specified in logit space\nmean 5 interactions between each pair, 200 Stan iters, 10 runs") +
  theme_bw()
dev.off()

pdf('results/www2016_simulation1_dnm_uniform.pdf')
df3 %<>% filter(method=='DNM'|method=='model_eta_ab')
ggplot(aes(x=alpha,y=mean,color=as.factor(pa)),data=df3) +
  geom_pointrange(aes(ymin=ci_lower,ymax=ci_upper)) + 
  geom_smooth(method='loess',se=FALSE) +
  facet_grid(method~.,scales='free') +
  scale_color_brewer(palette = "BrBG") +
  #scale_alpha_discrete(range=c(.4,1),guide=FALSE) +
  labs(x="true alignment (in logit space)",y="estimated alignment (in method's space)",color="  marker\nfrequency",
       title="Simulated data -- alignment specified in logit space\nmean 5 interactions between 100 pairs, 200 Stan iters, 5 runs") +
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
                 value=vector(mode="numeric"))
mod <- stan_model('alignment.vectorized4.stan')
pmean <- 5
meanwords <- 8
rate <- 2
for (sd in c(.5)) {
  for (pa in c(.02)) {
    for (npairs in c(100)) {
      for (alpha in seq(-1.5,1.5,.25)) {
        for (i in 1:5) {
          print(c(pmean,sd,pa,npairs,alpha,i))
          df  <- runsim2(df,pmean,pa,npairs,alpha,sd,meanwords,rate)
        }}}}}

save(df,file="results/www2016_simulation2_uniform.RData")

#ggplot(aes(x=alpha,y=value,color=as.factor(npairs)),data=df) + geom_hline(yintercept=0) + geom_point() + geom_smooth(method='lm') + facet_grid(method~pa,scales='free')

#converting the model_eta value to an error-from-baseline value 
df2 <- df
df2$value[df2$method=='model_eta'] <- logit(df2$value[df2$method=='model_eta'])-df2$pa[df2$method=='model_eta']

df2 %<>%
  group_by(alpha,pa,sd,npairs,method) %>%
  filter(sd==0.5,pa>.005) %>%
  multi_boot_standard("value", na.rm = T) %>%
  mutate(method=factor(method,c('DNM','model_sub','model_mu','model_eta_ab','model_eta')))

#ggplot(aes(x=as.factor(alpha),y=mean,color=as.factor(sd)),data=df2) + geom_hline(yintercept=0) + geom_pointrange(aes(ymin=ci_lower,ymax=ci_upper),position = position_dodge(width = 0.3)) + facet_grid(method~pa,scales='free') +theme_bw()

df3 <- df2 %>% filter(method!='model_m_pop'&method!='model_eta_pop') %>%
  mutate(significant=ifelse(xor(sign(ci_lower)==sign(ci_upper),alpha==0),0,1))

df3$significant[sign(df3$mean)!=sign(df3$alpha)&df3$alpha!=0] <- 1

pdf('results/www2016_simulation2_uniform.pdf')
df3 %<>% filter(method=='DNM'|method=='model_eta_ab')
ggplot(aes(x=alpha,y=mean,color=as.factor(pa)),data=df3) +
  geom_pointrange(aes(ymin=ci_lower,ymax=ci_upper)) + 
  geom_smooth(method='loess',se=FALSE) +
  facet_grid(method~.,scales='free') +
  scale_color_brewer(palette = "BrBG") +
  #scale_alpha_discrete(range=c(.4,1),guide=FALSE) +
  labs(x="true alignment (in logit space)",y="estimated alignment (in method's space)",color="  marker\nfrequency",
       title="Simulated data -- alignment specified in logit space\nmean 5 interactions between 100 pairs, 200 Stan iters, 5 runs") +
  theme_bw()
dev.off()

