library(data.table)
library(bit64)
library(Matrix)
library(rstan)
library(dplyr)
library(tidyr)
library(ggplot2)
library(langcog)

ci_32 <- function(x){quantile(x,.32)}
ci_68 <- function(x){quantile(x,.32)}

rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())
# 
# make_sparse_matrix <- function(d, col1, col2 = NULL) {
# 
#   if(is.null(col2))
#     as.matrix(sparseMatrix(i = as.numeric(d$sid),
#                            j = as.numeric(d$rid),
#                            x = d[[col1]]))
#   else 
#     as.matrix(sparseMatrix(i = as.numeric(d$sid),
#                            j = as.numeric(d$rid),
#                            x = d[[col1]] + d[[col2]]))
# }



# num_utterances_ab <- make_sparse_matrix(d2,"ba","nba")
# num_utterances_notab <- make_sparse_matrix(d2,"bna","nbna")
# 
# counts_ab <- make_sparse_matrix(d2,"ba")
# counts_notab <- make_sparse_matrix(d2,"bna")

d2 <- fread('pros.csv') %>%
  mutate(sid = as.character(sid),
         rid = as.character(rid)) %>%
  mutate(sid = factor(sid, levels = union(sid,rid)),
         rid = factor(rid, levels = union(sid,rid))) %>%
  mutate(unsmoothed = ba/(ba + nba) - bna/(bna + nbna))

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


fit <- stan(file = 'alignment.stan', data = alignment_data, 
            iter = 1000, chains =1 )

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
  group_by(vspeak,vreply) %>%
  multi_boot_standard(., "eta_ab", 
                      statistics_functions = c("ci_32", "ci_68"))

ggplot(aes(x = inf, y = unsmoothed, color = log(counts)), data = cor.matrix) +
  geom_point(size=2) + 
  theme_bw(base_size = 14) +
  theme(panel.grid = element_blank())
