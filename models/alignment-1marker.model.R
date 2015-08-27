library(data.table)
library(bit64)
library(Matrix)
library(rstan)
library(dplyr)
library(tidyr)
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


fit <- stan(file = 'alignment-1marker.stan', data = alignment_data, 
            iter = 1000, chains = 1 )

pos_infs <- colMeans(rstan:::extract(fit,"pos_inf")$pos_inf)
neg_infs <- colMeans(rstan:::extract(fit,"neg_inf")$neg_inf)
infs <- pos_infs - neg_infs


theta_a <- colMeans(rstan:::extract(fit,"thetaA")$thetaA)
theta_nota <- colMeans(rstan:::extract(fit,"thetaNotA")$thetaNotA)
theta_diffs <- log(theta_a) - log(theta_nota)

cor.matrix <- data.frame(pa = d2$pa,
                         pos_inf = pos_infs, 
                         neg_inf = neg_infs, 
                         inf = infs,
                         counts = num_utterances_ab,
                         unsmoothed = d2$unsmoothed,
                         theta_diff = theta_diffs,
                         vspeak = d2$vspeak,
                         vreply = d2$vreply) %>%
  group_by(vspeak,vreply) %>%
  multi_boot_standard(., "theta_diff", 
                      statistics_functions = c("ci_32", "ci_68"))

ggplot(aes(x = inf, y = unsmoothed, color = log(counts)), data = cor.matrix) +
  geom_point(size=2) + 
  theme_bw(base_size = 14) +
  theme(panel.grid = element_blank())
