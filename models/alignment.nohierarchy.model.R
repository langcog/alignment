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
  group_by(vspeak,vreply) %>%
  summarise_each(funs(sum), ba, nba, bna, nbna) %>%
  mutate(pa = log(ba/(ba+nba)) - log(bna/(bna+nbna)))

counts_ab <- t(matrix(d2$ba,nrow = 2))
counts_notab <- t(matrix(d2$bna,nrow=2))

num_utterances_ab <-counts_ab + t(matrix(d2$nba,nrow=2))
num_utterances_notab <- counts_notab + t(matrix(d2$nbna,nrow=2))

num_subpops <- 2


alignment_data <- list(NumSubPops = num_subpops,
                       NumUtterancesAB = num_utterances_ab,
                       NumUtterancesNotAB = num_utterances_notab,
                       CountsAB = counts_ab,
                       CountsNotAB = counts_notab)


fit <- stan(file = 'alignment.notheta.stan', data = alignment_data, 
            iter = 1000, chains =1 )

eta_ab_subpops <- colMeans(rstan:::extract(fit,"eta_ab_subpops")$eta_ab_subpops)
eta_ab_subpops <- apply(rstan:::extract(fit,"eta_ab_subpops")$eta_ab_subpops,c(2,3),sd)

eta_ab_pop <- rstan:::extract(fit,"eta_ab_pop")$eta_ab_pop
eta_pop <- rstan:::extract(fit,"eta_pop")$eta_pop
n_pop <- mean(rstan:::extract(fit,"n_pop")$n_pop)
n_person <- colMeans(rstan:::extract(fit,"n_person")$n_person)

eta_pop <- mean(rstan:::extract(fit,"eta_pop")$eta_pop)
eta_person <- colMeans(rstan:::extract(fit,"eta_person")$eta_person)

theta_a <- colMeans(rstan:::extract(fit,"thetaA")$thetaA)
theta_nota <- colMeans(rstan:::extract(fit,"thetaNotA")$thetaNotA)
theta_diffs <- log(theta_a) - log(theta_nota)

theta_a <- rstan:::extract(fit,"thetaA")$thetaA
theta_nota <- rstan:::extract(fit,"thetaNotA")$thetaNotA
theta_diffs <- rstan:::extract(fit,"thetaA")$thetaA - rstan:::extract(fit,"thetaNotA")$thetaNotA



diff_ff <- theta_diffs[,1,1]
diff_ft <- theta_diffs[,1,2]
diff_tf <- theta_diffs[,2,1]
diff_tt <- theta_diffs[,2,2]

diff_ff <- theta_diffs[,1,1]
diff_ft <- theta_diffs[,1,2]
diff_tf <- theta_diffs[,2,1]
diff_tt <- theta_diffs[,2,2]

qplot(1:500,ff,geom ="smooth")
qplot(1:500,tf,geom = "smooth")

mu_ab <- rstan:::extract(fit,"mu_ab")$mu_ab

mu_ff <- mu_ab[,1,1]
mu_ft <- mu_ab[,1,2]
mu_tf <- mu_ab[,2,1]
mu_tt <- mu_ab[,2,2]

mu_subpop <- rstan:::extract(fit,"mu_subpop")$mu_subpop


mu_ab <- colMeans(rstan:::extract(fit,"mu_ab")$mu_ab)
mu_person <- colMeans(rstan:::extract(fit,"mu_person")$mu_person)

cor.matrix <- data.frame(pa = d2$pa,
                         counts = num_utterances_ab,
                         unsmoothed = d2$unsmoothed,
                         theta_diff = theta_diffs,
                         mu_ab = mu_ab,
                         vspeak = d2$vspeak,
                         vreply = d2$vreply) %>%
  group_by(vspeak,vreply) %>%
  multi_boot_standard(., "theta_diff", 
                      statistics_functions = c("ci_32", "ci_68"))

ggplot(aes(x = inf, y = unsmoothed, color = log(counts)), data = cor.matrix) +
  geom_point(size=2) + 
  theme_bw(base_size = 14) +
  theme(panel.grid = element_blank())
