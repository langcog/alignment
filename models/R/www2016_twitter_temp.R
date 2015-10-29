library(data.table)
library(bit64)
library(Matrix)
library(rstan)
library(dplyr)
library(tidyr)
library(ggplot2)
library(langcog)

ci_025 <- function(x){quantile(x,.025)}
ci_05 <- function(x){quantile(x,.05)}
ci_32 <- function(x){quantile(x,.32)}
ci_68 <- function(x){quantile(x,.68)}
ci_95 <- function(x){quantile(x,.95)}
ci_975 <- function(x){quantile(x,.975)}

rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

convertcategories2 <- function(d) {
  return (d %>%
            mutate(category = factor(category,levels=rev(levels(as.factor(category))))) %>%
            mutate(category = factor(category,levels=c("ipron",
                                                       "you",
                                                       "we",
                                                       "i",
                                                       "quant",
                                                       "preps",
                                                       "conj",
                                                       "article",
                                                       "tentat",
                                                       "negate",
                                                       "incl",
                                                       "excl",
                                                       "discrep",
                                                       "certain"))) %>%
            mutate(group = ifelse(category %in% c("i","we","you","ipron"),"pronominal",ifelse(category %in% c("article","quant","conj","preps"),"syntactic","conceptual"))) %>%
            mutate(group = factor(group,levels=c("conceptual","syntactic","pronominal"))) %>%
            mutate(category = plyr:::revalue(as.factor(category),
                                             c("certain"="certainty",
                                               "discrep"="discrepancy",
                                               "excl"="exclusion",
                                               "incl"="inclusion",
                                               "negate"="negation",
                                               "quant"="quantifier",
                                               "tentat"="tentative",
                                               "conj"="conjunction",
                                               "preps"="preposition",
                                               "ipron"="indefinite",
                                               "i"="1st singular",
                                               "we"="1st plural",
                                               "you"="2nd person"))))
}

convertcategoriesnum <- function(d) {
  return (d %>%
            mutate(category = factor(marker,levels=rev(levels(as.factor(marker))))) %>%
            mutate(category = plyr:::revalue(category,
                                             c("1"="article",
                                               "2"="certain",
                                               "3"="conj",
                                               "4"="discrep",
                                               "5"="excl",
                                               "6"="i",
                                               "7"="incl",
                                               "8"="ipron",
                                               "9"="negate",
                                               "10"="preps",
                                               "11"="quant",
                                               "12"="tentat",
                                               "13"="we",
                                               "14"="you"))))
}

addcategories <- function(d) {
  d$category <- c("article","certain","conj","discrep","excl","i","incl","ipron","negate","preps","quant","tentat","we","you")
  return(d)
}


################
# Twitter data

sd <- .5

d2 <- fread('../../data/liwc_wildcards.csv') %>%
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
                       CountsNotAB = d2$bna,
                       StdDev = sd)

fit <- stan(file = 'alignment.vectorized5.stan', data = alignment_data, 
            iter = 250, chains =1 )

d3 <- d2
d3$model_eta <- colMeans(rstan:::extract(fit,"eta_ab_observation")$eta_ab_observation)
d3$model_mu <- log(colMeans(rstan:::extract(fit,"mu_ab")$mu_ab))-log(colMeans(rstan:::extract(fit,"mu_notab")$mu_notab))

etas <- rstan:::extract(fit,"eta_ab_subpop")$eta_ab_subpop

save(d3,fit,file='results/www2016_twitter_verif_final.RData')

#reloading the results
load(file='results/www2016_twitter_verif_final.RData')

#extract the by group eta values
etas <- rstan:::extract(fit,"eta_ab_subpop")$eta_ab_subpop
meandiffs <- colMeans(etas[,2,]-etas[,1,])  #mean difference between the TRUE (powerful) and FALSE (nonpowerful) alignments
ci_upper <- apply(etas[,2,]-etas[,1,],2,'ci_68')
ci_lower <- apply(etas[,2,]-etas[,1,],2,'ci_32')

df <- as.data.frame(list(mean=meandiffs,ci_lower=ci_lower,ci_upper=ci_upper))
df <- convertcategories2(addcategories(df)) %>%
  mutate(color = ifelse(ci_lower<0&ci_upper<0,"less alignment",ifelse(ci_lower>0,"more alignment","n.s."))) %>%
  mutate(color = factor(color,c("more alignment","n.s.","less alignment")))

pdf(file="results/www2016_ourpowerdiff_verif_final1sd.pdf",height=6,width=6)
ggplot(aes(y=category,x=mean,color=color),data=df) +
  geom_point() +
  geom_errorbarh(aes(xmin=ci_lower,xmax=ci_upper,height=0)) +
  geom_vline(xintercept=0) +
  theme_bw(base_size = 20) +
  theme(panel.grid = element_blank(),axis.title.y= element_blank()) +
  guides(color=FALSE) +
  scale_color_manual(values=c('#1f78b4','#33a02c','#e31a1c')) +
  facet_grid(group~.,space='free',scale='free') +
  labs(y="marker category",x="alignment to power")
dev.off()

ci_upper <- apply(etas[,2,]-etas[,1,],2,'ci_upper')
ci_lower <- apply(etas[,2,]-etas[,1,],2,'ci_lower')

df <- as.data.frame(list(mean=meandiffs,ci_lower=ci_lower,ci_upper=ci_upper))
df <- convertcategories2(addcategories(df)) %>%
  mutate(color = ifelse(ci_lower<0&ci_upper<0,"less alignment",ifelse(ci_lower>0,"more alignment","n.s."))) %>%
  mutate(color = factor(color,c("more alignment","n.s.","less alignment")))

pdf(file="results/www2016_ourpowerdiff_verif_final95.pdf",height=6,width=6)
ggplot(aes(y=category,x=mean,color=color),data=df) +
  geom_point() +
  geom_errorbarh(aes(xmin=ci_lower,xmax=ci_upper,height=0)) +
  geom_vline(xintercept=0) +
  theme_bw(base_size = 20) +
  theme(panel.grid = element_blank(),axis.title.y= element_blank()) +
  guides(color=FALSE) +
  scale_color_manual(values=c('#1f78b4','#33a02c','#e31a1c')) +
#  scale_color_manual(values=c('#33a02c')) +
  facet_grid(group~.,space='free',scale='free') +
  labs(y="marker category",x="alignment to power")
dev.off()

#Calculating bootstrapped CIs for each category for DNM (verification status)
df2 <- as.data.frame(d3) %>%
  group_by(vspeak,category) %>%
  filter((ba+bna+nba+nbna)>=10) %>%
  mutate(dnm=(ba/(ba+nba))-((ba+bna)/(ba+bna+nba+nbna))) %>%
  multi_boot_standard("dnm", na.rm = T)

df3me <- df2 %>%
  select(vspeak,category,mean) %>%
  spread(vspeak,mean) %>%
  transmute(category=category,mean = `TRUE` - `FALSE`)
df3mi <- df2 %>%                    #minimum is TRUE lower - FALSE upper
  mutate(min = ifelse(vspeak,ci_lower,ci_upper)) %>%
  select(vspeak,category,min) %>%
  spread(vspeak,min) %>%
  transmute(category=category,min = `TRUE` - `FALSE`)
df3ma <- df2 %>%                    #maximum is TRUE upper - FALSE lower
  mutate(max = ifelse(vspeak,ci_upper,ci_lower)) %>%
  select(vspeak,category,max) %>%
  spread(vspeak,max) %>%
  transmute(category=category,max = `TRUE` - `FALSE`)

df3 <- left_join(df3me,df3mi) %>%
  left_join(df3ma) %>%
  mutate(color = ifelse(min<0&max<0,"less alignment",ifelse(min>0,"more alignment","n.s."))) %>%
  mutate(color = factor(color,c("more alignment","n.s.","less alignment")))

df3 <- convertcategories2(df3)

pdf(file="results/www2016_dnmpowerdiff_verif_final.pdf",height=6,width=6)
ggplot(aes(y=category,x=mean,color=color),data=df3) +
  geom_point() +
  geom_errorbarh(aes(xmin=min,xmax=max,height=0)) +
  geom_vline(xintercept=0) +
  theme_bw(base_size = 20) +
  theme(panel.grid = element_blank(),axis.title.y= element_blank()) +
  guides(color=FALSE) +
#  scale_color_manual(values=c('#1f78b4','#33a02c','#e31a1c')) +
  scale_color_manual(values=c('#33a02c')) +
  facet_grid(group~.,space='free',scale='free') +
  labs(y="marker category",x="alignment to power")
dev.off()

#Calculating bootstrapped CIs for each category by 
etas2 <- apply(rstan:::extract(fit,"eta_ab_subpop")$eta_ab_subpop,c(1,3),mean)
meandiffs <- apply(etas2,2,'mean')
ci_upper <- apply(etas2,2,'ci_68')
ci_lower <- apply(etas2,2,'ci_32')

df <- as.data.frame(list(mean=meandiffs,ci_lower=ci_lower,ci_upper=ci_upper))
df <- convertcategories2(addcategories(df)) %>%
  mutate(color = ifelse(ci_lower<0&ci_upper<0,"less alignment",ifelse(ci_lower>0,"more alignment","n.s."))) %>%
  mutate(color = factor(color,c("more alignment","n.s.","less alignment"))) %>%
  mutate(method='HAM')

df3 <- as.data.frame(d3) %>%
  convertcategories2() %>%
  group_by(category,group) %>%
  filter((ba+bna+nba+nbna)>=10) %>%
  mutate(dnm=(ba/(ba+nba))-((ba+bna)/(ba+bna+nba+nbna))) %>%
  multi_boot_standard("dnm", na.rm = T) %>%
  mutate(method="DNM") %>%
  bind_rows(df)  %>%
  mutate(color = ifelse(ci_lower<0&ci_upper<0,"less alignment",ifelse(ci_lower>0,"more alignment","n.s."))) %>%
  mutate(color = factor(color,c("more alignment","n.s.","less alignment")))

pdf(file="results/www2016_alignmentdnmour_final1sd.pdf",height=6,width=8)
ggplot(aes(y=category,x=mean,color=color),data=df3) +
  geom_point() +
  geom_errorbarh(aes(xmin=ci_lower,xmax=ci_upper,height=0)) +
  geom_vline(xintercept=0) +
  theme_bw(base_size = 20) +
  theme(panel.grid = element_blank(),axis.title.y= element_blank()) +
  guides(color=FALSE) +
  scale_color_manual(values=c('#1f78b4','#33a02c','#e31a1c')) +
  facet_grid(group~method,space='free_y',scale='free') +
  labs(y="marker category",x="alignment")
dev.off()

etas2 <- apply(rstan:::extract(fit,"eta_ab_subpop")$eta_ab_subpop,c(1,3),mean)
meandiffs <- apply(etas2,2,'mean')
ci_upper <- apply(etas2,2,'ci_upper')
ci_lower <- apply(etas2,2,'ci_lower')

df <- as.data.frame(list(mean=meandiffs,ci_lower=ci_lower,ci_upper=ci_upper))
df <- convertcategories2(addcategories(df)) %>%
  mutate(color = ifelse(ci_lower<0&ci_upper<0,"less alignment",ifelse(ci_lower>0,"more alignment","n.s."))) %>%
  mutate(color = factor(color,c("more alignment","n.s.","less alignment"))) %>%
  mutate(method='HAM')

df3 <- as.data.frame(d3) %>%
  convertcategories2() %>%
  group_by(category,group) %>%
  filter((ba+bna+nba+nbna)>=10) %>%
  mutate(dnm=(ba/(ba+nba))-((ba+bna)/(ba+bna+nba+nbna))) %>%
  multi_boot_standard("dnm", na.rm = T) %>%
  mutate(method="DNM") %>%
  bind_rows(df)  %>%
  mutate(color = ifelse(ci_lower<0&ci_upper<0,"less alignment",ifelse(ci_lower>0,"more alignment","n.s."))) %>%
  mutate(color = factor(color,c("more alignment","n.s.","less alignment")))

pdf(file="results/www2016_alignmentdnmour_final95.pdf",height=6,width=8)
ggplot(aes(y=category,x=mean,color=color),data=df3) +
  geom_point() +
  geom_errorbarh(aes(xmin=ci_lower,xmax=ci_upper,height=0)) +
  geom_vline(xintercept=0) +
  theme_bw(base_size = 20) +
  theme(panel.grid = element_blank(),axis.title.y= element_blank()) +
  guides(color=FALSE) +
  scale_color_manual(values=c('#1f78b4','#33a02c','#e31a1c')) +
  facet_grid(group~method,space='free_y',scale='free') +
  labs(y="marker category",x="alignment")
dev.off()

##################
# Twitter with follower ratios

sd <- .5

d2 <- fread('../../data/liwc_wildcards.csv') %>%
  rename(sid=speakerId,rid=replierId,vspeak=verifiedSpeaker,vreply=verifiedReplier) %>%
  na.omit() %>%
  mutate(fratio=(speakerFollowers/(speakerFollowers+replierFollowers))>=.9) %>%
  group_by(fratio,rid,category) %>%
  summarise_each(funs(sum), ba, nba, bna, nbna) %>%
  ungroup() %>%
  mutate(pa = log(ba/(ba+nba)) - log(bna/(bna+nbna)))

num_subpops <- length(unique(d2$fratio))
num_markers <- length(unique(d2$category))

alignment_data <- list(NumMarkers=num_markers,
                       NumSubPops = num_subpops,
                       NumObservations = nrow(d2),
                       SpeakerSubPop = as.numeric(d2$fratio)+1,
                       MarkerType = as.numeric(as.factor(d2$category)),
                       NumUtterancesAB = d2$ba+d2$nba,
                       NumUtterancesNotAB = d2$bna+ d2$nbna,
                       CountsAB = d2$ba,
                       CountsNotAB = d2$bna,
                       StdDev = sd)

fit <- stan(file = 'alignment.vectorized5.stan', data = alignment_data, 
            iter = 200, chains =1 )

d3 <- d2
d3$model_eta <- colMeans(rstan:::extract(fit,"eta_ab_observation")$eta_ab_observation)
d3$model_mu <- log(colMeans(rstan:::extract(fit,"mu_ab")$mu_ab))-log(colMeans(rstan:::extract(fit,"mu_notab")$mu_notab))

save(d3,fit,file='results/www2016_twitter_fratio90.RData')

load('results/www2016_twitter_fratio90.RData')

etas <- rstan:::extract(fit,"eta_ab_subpop")$eta_ab_subpop
meandiffs <- colMeans(etas[,2,]-etas[,1,])  #mean difference between the TRUE (powerful) and FALSE (nonpowerful) alignments
ci_upper <- apply(etas[,2,]-etas[,1,],2,'ci_68')
ci_lower <- apply(etas[,2,]-etas[,1,],2,'ci_32')

df <- as.data.frame(list(mean=meandiffs,ci_lower=ci_lower,ci_upper=ci_upper))
df <- convertcategories2(addcategories(df)) %>%
  mutate(color = ifelse(ci_lower<0&ci_upper<0,"less alignment",ifelse(ci_lower>0,"more alignment","n.s."))) %>%
  mutate(color = factor(color,c("more alignment","n.s.","less alignment")))

ci_upper <- apply(etas[,2,]-etas[,1,],2,'ci_upper')
ci_lower <- apply(etas[,2,]-etas[,1,],2,'ci_lower')

df <- as.data.frame(list(mean=meandiffs,ci_lower=ci_lower,ci_upper=ci_upper))
df <- convertcategories2(addcategories(df)) %>%
  mutate(color = ifelse(ci_lower<0&ci_upper<0,"less alignment",ifelse(ci_lower>0,"more alignment","n.s."))) %>%
  mutate(color = factor(color,c("more alignment","n.s.","less alignment")))

pdf(file="results/www2016_ourpowerdiff_fratio90_95.pdf",height=6,width=6)
ggplot(aes(y=category,x=mean,color=color),data=df) +
  geom_point(size=2) +
  geom_errorbarh(aes(xmin=ci_lower,xmax=ci_upper,height=0),size=2) +
  geom_vline(xintercept=0) +
  theme_bw(base_size = 20) +
  theme(panel.grid = element_blank(),axis.title.y= element_blank()) +
  guides(color=FALSE) +
  scale_color_manual(values=c('#1f78b4','#33a02c','#e31a1c')) +
  facet_grid(group~.,space='free',scale='free') +
  labs(y="marker category",x="alignment to power")
dev.off()

#Calculating bootstrapped CIs for each category for DNM (verification status)
df2 <- as.data.frame(d3) %>%
  group_by(fratio,category) %>%
  filter((ba+bna+nba+nbna)>=10) %>%
  mutate(dnm=(ba/(ba+nba))-((ba+bna)/(ba+bna+nba+nbna))) %>%
  multi_boot_standard("dnm", na.rm = T)

df3me <- df2 %>%
  select(fratio,category,mean) %>%
  spread(fratio,mean) %>%
  transmute(category=category,mean = `TRUE` - `FALSE`)
df3mi <- df2 %>%                    #minimum is TRUE lower - FALSE upper
  mutate(min = ifelse(fratio,ci_lower,ci_upper)) %>%
  select(fratio,category,min) %>%
  spread(fratio,min) %>%
  transmute(category=category,min = `TRUE` - `FALSE`)
df3ma <- df2 %>%                    #maximum is TRUE upper - FALSE lower
  mutate(max = ifelse(fratio,ci_upper,ci_lower)) %>%
  select(fratio,category,max) %>%
  spread(fratio,max) %>%
  transmute(category=category,max = `TRUE` - `FALSE`)

df3 <- left_join(df3me,df3mi) %>%
  left_join(df3ma) %>%
  mutate(color = ifelse(min<0&max<0,"less alignment",ifelse(min>0,"more alignment","n.s."))) %>%
  mutate(color = factor(color,c("more alignment","n.s.","less alignment")))

df3 <- convertcategories2(df3)

pdf(file="results/www2016_dnmpowerdiff_fratio90_final.pdf",height=6,width=6)
ggplot(aes(y=category,x=mean,color=color),data=df3) +
  geom_point(size=3) +
  geom_errorbarh(aes(xmin=min,xmax=max,height=0),size=1.25) +
  geom_vline(xintercept=0) +
  theme_bw(base_size = 20) +
  theme(panel.grid = element_blank(),axis.title.y= element_blank()) +
  guides(color=FALSE) +
#  scale_color_manual(values=c('#1f78b4','#33a02c','#e31a1c')) +
  scale_color_manual(values=c('#33a02c','#e31a1c')) +
  facet_grid(group~.,space='free',scale='free') +
  labs(y="marker category",x="alignment to power")
dev.off()

###########################
# Data stats

d2 <- fread('../../data/liwc_wildcards.csv') %>%
  rename(sid=speakerId,rid=replierId,vspeak=verifiedSpeaker,vreply=verifiedReplier) %>%
  na.omit() %>%
  mutate(fratio=(speakerFollowers/(speakerFollowers+replierFollowers))) %>%
  mutate(pa=(ba/(ba+nba)-((ba+bna)/(ba+bna+nba+nbna))))

ggplot(aes(x=fratio,y=pa),data=d2) + geom_smooth()

median(d2$fratio)

d2 %>% group_by(sid,rid,category) %>%
  mutate(n=ba+bna+nba+nbna) %>%
  ungroup() %>%
  summarize(mean=mean(n))

d2 %>% mutate(pid=paste(sid,rid)) %>%
  select(pid) %>%
  distinct() %>%
  summarize(pairs=n())