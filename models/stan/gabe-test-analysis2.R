library(data.table)
library(bit64)
library(rstan)
library(dplyr)
library(tidyr)
library(ggplot2)
library(langcog)

ci_32 <- function(x){quantile(x,.32)}
ci_68 <- function(x){quantile(x,.68)}

d <- fread('test50cats.csv')

speakers <- as.numeric(d2$sid)
repliers <- as.numeric(d2$rid)

counts_ab <- d2$ba
counts_notab <- d2$bna

num_utterances_ab <-counts_ab + d2$nba
num_utterances_notab <- counts_notab + d2$nbna

num_people <- length(levels(d2$sid))


alignment_data <- list(NumPeople = num_people,
                       NumPairs = length(counts_ab),
                       NumUtterancesAB = num_utterances_ab,
                       NumUtterancesNotAB = num_utterances_notab,
                       CountsAB = counts_ab,
                       CountsNotAB = counts_notab,
                       Speaker = speakers,
                       Replier = repliers)


#fit <- stan(file = 'alignment.stan', data = alignment_data, 
#            iter = 1000, chains =1 )

eta_ab <- colMeans(rstan:::extract(fit,"eta_ab")$eta_ab)
eta_ab_pop <- rstan:::extract(fit,"eta_ab_pop")$eta_ab_pop
n_pop <- mean(rstan:::extract(fit,"n_pop")$n_pop)
n_person <- colMeans(rstan:::extract(fit,"n_person")$n_person)

eta_pop <- mean(rstan:::extract(fit,"eta_pop")$eta_pop)
eta_person <- colMeans(rstan:::extract(fit,"eta_person")$eta_person)

theta_a <- colMeans(rstan:::extract(fit,"thetaA")$thetaA)
theta_nota <- colMeans(rstan:::extract(fit,"thetaNotA")$thetaNotA)
theta_diffs <- log(theta_a) - log(theta_nota)

mu_ab <- colMeans(rstan:::extract(fit,"mu_ab")$mu_ab)
mu_person <- colMeans(rstan:::extract(fit,"mu_person")$mu_person)

cor.matrix <- data.frame(pa = d2$pa,
                         counts = num_utterances_ab,
                         unsmoothed = d2$unsmoothed,
                         theta_diff = theta_diffs,
                         eta_ab = eta_ab,
                         mu_ab = mu_ab,
                         vspeak = d2$vspeak,
                         vreply = d2$vreply) %>%
  group_by(vspeak,vreply)

ggplot(aes(x = theta_diff, y = pa, color = log(counts)), data = cor.matrix) +
  geom_point(size=2) + 
  theme_bw(base_size = 14) +
  theme(panel.grid = element_blank()) +
  geom_smooth(method='loess')

cor.matrix %>%
  multi_boot_standard(., "eta_ab", 
                      statistics_functions = c("ci_32", "ci_68"))

cor.matrix %>%
  multi_boot_standard(., "theta_diff", 
                      statistics_functions = c("ci_32", "ci_68"))

cor.matrix %>%
  multi_boot_standard(., "pa", 
                      statistics_functions = c("ci_32", "ci_68"))

#Are we actually getting convergence?
dt <- data.frame(eta_sd=apply(rstan:::extract(fit,"eta_ab")$eta_ab,2,sd),
                utterances=num_utterances_ab,
                eta_mean=eta_ab)

cor(dt$eta_sd,dt$eta_mean)
cor(dt$eta_sd,dt$utterances)


ggplot(aes(x=utterances,y=eta_sd,color=eta_mean),data=dt) + geom_point() + theme_bw()

#Is there a difference between early and late variance that suggests convergence still ongoing?
etas <- data.table(rstan:::extract(fit,"eta_ab")$eta_ab)
early_sd <- etas[1:100,]
late_sd <- etas[3901:4000,]

dt <- data.frame(early=apply(early_sd,2,sd),
                 late=apply(late_sd,2,sd)) %>%
      gather(time,sd,early,late)

ggplot(aes(x=time,y=sd),data=dt) + geom_violin()

