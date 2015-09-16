library(data.table)
library(bit64)
library(Matrix)
library(rstan)
library(dplyr)
library(tidyr)
library(ggplot2)
library(langcog)

ci_32 <- function(x){quantile(x,.32)}
ci_68 <- function(x){quantile(x,.68)}

rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

################
# Twitter data

d2 <- fread('../../../data/liwc_wildcards.csv') %>%
#d2 <- fread('../test50cats.csv') %>%
  rename(sid=speakerId,rid=replierId,vspeak=verifiedSpeaker,vreply=verifiedReplier) %>%
  group_by(vspeak,rid,vreply,category) %>%
  summarise_each(funs(sum), ba, nba, bna, nbna) %>%
  ungroup() %>%
  mutate(pa = log(ba/(ba+nba)) - log(bna/(bna+nbna))) %>%
  filter(vreply == FALSE)

num_subpops <- length(unique(d2$vspeak))
num_markers <- length(unique(d2$category))
#num_repliers <- length(unique(d2$rid))

alignment_data <- list(NumMarkers=num_markers,
                       NumSubPops = num_subpops,
                       NumObservations = nrow(d2),
                       SpeakerSubPop = as.numeric(d2$vspeak)+1,
                       MarkerType = as.numeric(as.factor(d2$category)),
                       NumUtterancesAB = d2$ba+d2$nba,
                       NumUtterancesNotAB = d2$bna+ d2$nbna,
                       CountsAB = d2$ba,
                       CountsNotAB = d2$bna)


###
# CHILDES (Brown)

d2 <- fread('../../Childes_Results/Brown300StemResults.csv') %>%
  rename(sid=speakerId,rid=replierId) %>%
  filter(sid=='CHI'|sid=='MOT',rid=='CHI'|rid=='MOT') %>%
  group_by(sid,docId) %>%
  summarise_each(funs(sum), ba, nba, bna, nbna) %>%
  ungroup() %>%
  mutate(pa = log(ba/(ba+nba)) - log(bna/(bna+nbna)))

num_subpops <- length(unique(d2$sid))
num_markers <- length(unique(d2$docId))
#num_repliers <- length(unique(d2$rid))

alignment_data <- list(NumMarkers=num_markers,
                       NumSubPops = num_subpops,
                       NumObservations = nrow(d2),
                       SpeakerSubPop = as.numeric(as.factor(d2$sid)),
                       MarkerType = as.numeric(as.factor(d2$docId)),
                       NumUtterancesAB = d2$ba+d2$nba,
                       NumUtterancesNotAB = d2$bna+ d2$nbna,
                       CountsAB = d2$ba,
                       CountsNotAB = d2$bna)


fit <- stan(file = 'alignment.vectorized.stan', data = alignment_data, 
            iter = 1000, chains =1 )

mu_notab <- colMeans(rstan:::extract(fit,"mu_notab")$mu_notab)
mu_ab <- colMeans(rstan:::extract(fit,"mu_ab")$mu_ab)

#If the model works, its mu_ values should be "smoothed" estimates of p(B|A) & p(B|notA)
plot(mu_ab,d2$ba/(d2$ba+d2$nba))
plot(mu_notab,d2$bna/(d2$bna+d2$nbna))

#checking in on the learned subpopulation values
eta_ab_subpop <- colMeans(rstan:::extract(fit,"eta_ab_subpop")$eta_ab_subpop)
eta_subpop <- colMeans(rstan:::extract(fit,"eta_subpop")$eta_subpop)

#eta_ab_subpop is similar to a smoothed log-space alignment measure
#we can estimate this directly from the data
base_ab <- d2 %>% group_by(vspeak,category) %>%
  summarize(pa = log(sum(ba)/(sum(ba)+sum(nba)))-log(sum(bna)/(sum(bna)+sum(nbna)))) %>%
  spread(category,pa) %>%
  arrange(vspeak)

#CHILDES version
base_ab <- d2 %>% group_by(sid,docId) %>%
  summarize(pa = log(sum(ba)/(sum(ba)+sum(nba)))-log(sum(bna)/(sum(bna)+sum(nbna)))) %>%
  spread(docId,pa) %>%
  arrange(sid)

select(base_ab,-vspeak)-eta_ab_subpop

#Moving the mean mu_observations into the dataframe as a smoothed alignment estimate
d3 <- d2
d3$model_eta <- colMeans(rstan:::extract(fit,"eta_ab_observation")$eta_ab_observation)
d3$model_mu <- log(colMeans(rstan:::extract(fit,"mu_ab")$mu_ab))-log(colMeans(rstan:::extract(fit,"mu_notab")$mu_notab))

d4 <- d3 %>% group_by(vspeak,category) %>%
  summarize(dnm = (sum(ba)/(sum(ba)+sum(nba)))-(sum(bna)/(sum(bna)+sum(nbna))),
            pa = log(sum(ba)/(sum(ba)+sum(nba)))-log(sum(bna)/(sum(bna)+sum(nbna))),
            model_eta=mean(model_eta),
            model_mu=mean(model_mu)) %>%
  gather(measure,alignment,dnm,pa,model_eta,model_mu)

ggplot(aes(x=category,y=alignment,fill=vspeak),data=d4) +
  geom_bar(stat="identity",position="dodge") +
  facet_grid(measure~.,scales='free') +
  theme_bw()

#CHILDES version
d4 <- d3 %>% group_by(sid,docId) %>%
  summarize(dnm = (sum(ba)/(sum(ba)+sum(nba)))-(sum(bna)/(sum(bna)+sum(nbna))),
            pa = log(sum(ba)/(sum(ba)+sum(nba)))-log(sum(bna)/(sum(bna)+sum(nbna))),
            model_eta=mean(model_eta),
            model_mu=mean(model_mu)) %>%
  gather(measure,alignment,dnm,pa,model_eta,model_mu) %>%
  spread(sid,alignment) %>%
  mutate(diff=CHI-MOT) %>%
  mutate(name=substr(docId,0,3))

ggplot(aes(x=docId,y=diff,fill=name),data=d4) +
  geom_bar(stat="identity",position="dodge") +
  facet_grid(measure~.,scales='free') +
  theme_bw()


bootd3df <- d3df %>%
  group_by(vspeak,category) %>%
  multi_boot_standard(column = "model_eta")

ggplot(aes(x=category,y=mean,fill=vspeak),data=bootd3df) +
  geom_bar(stat="identity",position="dodge") +
  geom_errorbar(limits,position="dodge") +
  theme_bw()

bootd3df <- d3df %>%
  group_by(vspeak,category) %>%
  multi_boot_standard(column = "model_mu")

ggplot(aes(x=category,y=mean,fill=vspeak),data=bootd3df) +
  geom_bar(stat="identity",position="dodge") +
  geom_errorbar(limits,position="dodge") +
  theme_bw()


##########################################################
