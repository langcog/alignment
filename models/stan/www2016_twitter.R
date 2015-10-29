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

convertcategories <- function(d) {
  return (d %>%
            mutate(category = factor(category,levels=rev(levels(as.factor(category))))) %>%
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
                                               "ipron"="indefinites",
                                               "i"="1st singular",
                                               "we"="1st plural",
                                               "you"="2nd person"))))
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

fit <- stan(file = 'alignment.vectorized4.stan', data = alignment_data, 
            iter = 250, chains =1 )

#mu_notab <- colMeans(rstan:::extract(fit,"mu_notab")$mu_notab)
#mu_ab <- colMeans(rstan:::extract(fit,"mu_ab")$mu_ab)

#If the model works, its mu_ values should be "smoothed" estimates of p(B|A) & p(B|notA)
#plot(mu_ab,d2$ba/(d2$ba+d2$nba))
#plot(mu_notab,d2$bna/(d2$bna+d2$nbna))

#Recently I've found the model overestimates correlation of p(B|A) & p(B|notA)
# so check to see if that's still the case
#cor(mu_ab,mu_notab)
#cor(d2$ba/(d2$ba+d2$nba),d2$bna/(d2$bna+d2$nbna))

#Moving the mean mu_observations into the dataframe as a smoothed alignment estimate
d3 <- d2
d3$model_eta <- colMeans(rstan:::extract(fit,"eta_ab_observation")$eta_ab_observation)
d3$model_mu <- log(colMeans(rstan:::extract(fit,"mu_ab")$mu_ab))-log(colMeans(rstan:::extract(fit,"mu_notab")$mu_notab))
#d3$mu_ab <- colMeans(rstan:::extract(fit,"mu_ab")$mu_ab)
#d3$mu_notab <- colMeans(rstan:::extract(fit,"mu_notab")$mu_notab)

save(d3,fit,file='results/www2016_twitter_verif.RData')

#Calculating the p(A) probability for each marker category (in Table tab:LIWC [1])
# d3 %>% group_by(category) %>% summarize_each("sum") %>% mutate(p=(ba+nba)/(ba+bna+nba+nbna)) %>% select(category,p)
# 
# #Calculating bootstrapped CIs for each category by 
# df2 <- as.data.frame(d3) %>%
#   group_by(vspeak,category) %>%
#   multi_boot_standard("model_eta", na.rm = T)
# 
# ggplot(aes(x=category,y=mean,color=vspeak),data=df2) +
#   geom_pointrange(aes(ymin=ci_lower,ymax=ci_upper),position = position_dodge(width = 0.3)) +
# #  facet_grid(measure~.,scales='free') +
#   theme_bw()
# 
# df3me <- df2 %>%
#   select(vspeak,category,mean) %>%
#   spread(vspeak,mean) %>%
#   transmute(category=category,mean = `TRUE` - `FALSE`)
# df3mi <- df2 %>%                    #minimum is TRUE lower - FALSE upper
#   mutate(min = ifelse(vspeak,ci_lower,ci_upper)) %>%
#   select(vspeak,category,min) %>%
#   spread(vspeak,min) %>%
#   transmute(category=category,min = `TRUE` - `FALSE`)
# df3ma <- df2 %>%                    #maximum is TRUE upper - FALSE lower
#   mutate(max = ifelse(vspeak,ci_upper,ci_lower)) %>%
#   select(vspeak,category,max) %>%
#   spread(vspeak,max) %>%
#   transmute(category=category,max = `TRUE` - `FALSE`)
# 
# df3 <- left_join(df3me,df3mi) %>%
#   left_join(df3ma) %>%
#   mutate(color = ifelse(min<0&max<0,"less alignment",ifelse(min>0,"more alignment","n.s."))) %>%
#   mutate(color = factor(color,c("more alignment","n.s.","less alignment")))
# 
# ggplot(aes(x=category,y=mean,color=color),data=df3) +
#   geom_pointrange(aes(ymin=min,ymax=max)) +
#   geom_hline(yintercept=0) +
#   theme_bw() +
#   labs(x="marker category",y="alignment to power",color="More/less alignment\nto power?",
#        title="Difference in alignment to power (verification), by marker") +
#   coord_flip()

#Calculating bootstrapped CIs for each category by 
df2 <- as.data.frame(d3) %>%
  #filter((ba+bna+nba+nbna)>=10) %>%
  group_by(vspeak,category) %>%
  multi_boot_standard("model_eta", na.rm = T)

#ggplot(aes(x=category,y=mean,color=vspeak),data=df2) +
#  geom_pointrange(aes(ymin=ci_lower,ymax=ci_upper),position = position_dodge(width = 0.3)) +
#  #  facet_grid(measure~.,scales='free') +
#  theme_bw()

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

df3 <- convertcategories(df3)

pdf(file="results/www2016_ourpowerdiff_verif.pdf",height=7.5,width=10)
ggplot(aes(x=category,y=mean,color=color),data=df3) +
  geom_pointrange(aes(ymin=min,ymax=max)) +
  geom_hline(yintercept=0) +
  theme_bw(base_size = 20) +
  theme(panel.grid = element_blank()) +
  guides(color=FALSE) +
  labs(x="marker category",y="alignment to power",color="More/less alignment\nto power?",
       title="Model-based alignment to power\n(power=verification)") +
  coord_flip()
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

df3 <- convertcategories(df3)

pdf(file="results/www2016_dnmpowerdiff_verif.pdf",height=7.5,width=10)
ggplot(aes(x=category,y=mean,color=color),data=df3) +
  geom_pointrange(aes(ymin=min,ymax=max)) +
  geom_hline(yintercept=0) +
  theme_bw(base_size = 20) +
  theme(panel.grid = element_blank()) +
  guides(color=FALSE) +
  labs(x="marker category",y="alignment to power",color="More/less alignment\nto power?",
       title="DNM et al. alignment to power\n(power=verification)") +
  coord_flip()
dev.off()

#Calculating bootstrapped CIs for each category by 
df2 <- as.data.frame(d3) %>%
  group_by(category) %>%
  multi_boot_standard("model_eta", na.rm = T) %>%
  mutate(method="model")

df3 <- as.data.frame(d3) %>%
  group_by(category) %>%
  filter((ba+bna+nba+nbna)>=10) %>%
  mutate(dnm=(ba/(ba+nba))-((ba+bna)/(ba+bna+nba+nbna))) %>%
  multi_boot_standard("dnm", na.rm = T) %>%
  mutate(method="DNM") %>%
  bind_rows(df2)

df3 <- convertcategories(df3)


pdf(file="results/www2016_alignmentdnmour_verif.pdf",height=7.5,width=10)
ggplot(aes(x=category,y=mean),data=df3) +
  geom_pointrange(aes(ymin=ci_upper,ymax=ci_lower)) +
  geom_hline(yintercept=0) +
  theme_bw(base_size = 20) +
  theme(panel.grid = element_blank()) +
  labs(x="marker category",y="alignment to power",
       title="Overall alignment by marker") +
  facet_wrap(~method) +
  coord_flip()
dev.off()


########
# Failed attempt at bootstrapping true-false difference
# model_mu_diff <- function(df) {
# df %<>% summarize(model_mu = mean(model_mu)) %>% spread(vspeak,model_mu) %>% mutate(model_mu = `TRUE` - `FALSE`)
# return(df$model_mu) }
# 
# df2 <- as.data.frame(d3) %>%
#   group_by(vspeak,category) %>%
#   multi_boot_standard("model_mu", empirical_function = "model_mu_diff")


##################
# Twitter with follower ratios

sd <- .5

d2 <- fread('../../data/liwc_wildcards.csv') %>%
  rename(sid=speakerId,rid=replierId,vspeak=verifiedSpeaker,vreply=verifiedReplier) %>%
  na.omit() %>%
  mutate(fratio=(speakerFollowers/(speakerFollowers+replierFollowers))>=(100/101)) %>%
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

fit <- stan(file = 'alignment.vectorized4.stan', data = alignment_data, 
            iter = 250, chains =1 )

d3 <- d2
d3$model_eta <- colMeans(rstan:::extract(fit,"eta_ab_observation")$eta_ab_observation)
d3$model_mu <- log(colMeans(rstan:::extract(fit,"mu_ab")$mu_ab))-log(colMeans(rstan:::extract(fit,"mu_notab")$mu_notab))

save(d3,fit,file='results/www2016_twitter_fratio.RData')

#fratio alignment effects (DNM)
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

df3 <- convertcategories(df3)

pdf(file="results/www2016_dnmpowerdiff_fratio.pdf",height=7.5,width=10)
ggplot(aes(x=category,y=mean,color=color),data=df3) +
  geom_pointrange(aes(ymin=min,ymax=max)) +
  geom_hline(yintercept=0) +
  theme_bw(base_size = 20) +
  theme(panel.grid = element_blank()) +
  guides(color=FALSE) +
  labs(x="marker category",y="alignment to power",color="More/less alignment\nto power?",
       title="DNM et al. alignment to power\n(power=follower ratio)") +
  coord_flip()
dev.off()

# Twitter with follower counts (model-based)
df2 <- as.data.frame(d3) %>%
  group_by(fratio,category) %>%
  #filter((ba+bna+nba+nbna)>=10) %>%
  multi_boot_standard("model_eta", na.rm = T)

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

df3 <- convertcategories(df3)

pdf(file="results/www2016_ourpowerdiff_fratio.pdf",height=7.5,width=10)
ggplot(aes(x=category,y=mean,color=color),data=df3) +
  geom_pointrange(aes(ymin=min,ymax=max)) +
  geom_hline(yintercept=0) +
  theme_bw(base_size = 20) +
  theme(panel.grid = element_blank()) +
  guides(color=FALSE) +
  labs(x="marker category",y="alignment to power",color="More/less alignment\nto power?",
       title="Model-based alignment to power\n(power=follower ratio)") +
  coord_flip()
dev.off()

#######################
# Graveyard

ggplot(aes(x=category,))

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

save(d3,file="results/twitter_14cats_d3.RData")

load("results/twitter_14cats_d3.RData")

#Plotting & computing twitter_verifunverif_14cats.pdf
bootd3df <- as.data.frame(d3) %>%
  group_by(vspeak) %>%
  multi_boot_standard(column = "model_eta")

bootd3df <- as.data.frame(d3) %>%
  group_by(vspeak,category) %>%
  multi_boot_standard(column = "model_eta")

bootd3df %<>% ungroup() %>%
  mutate(vspeak = plyr:::revalue(as.factor(vspeak),c("TRUE"="verified","FALSE"="unverified")))

ggplot(aes(x=vspeak,y=mean,fill=vspeak),data=bootd3df) +
  geom_bar(stat="identity",position="dodge") +
  geom_linerange(aes(ymin=ci_lower,ymax=ci_upper),position="dodge",size=1.3) +
  theme_bw() +
  scale_color_brewer(palette = "Set1") +
  theme_bw(base_size = 30) +
  geom_hline(yintercept=0,linetype='dotted',size=2) +
  theme(panel.grid = element_blank(),axis.title.x=element_blank()) +
  labs(#title='Model-estimated Alignment to Verified/Unverified Speakers',
       #x='Child\'s Age (months)',
       y='Alignment Score') + #',
       #fill='Aligning to') +
  theme(legend.position = "none")

bootd3df <- as.data.frame(d3) %>%
  group_by(vspeak,category) %>%
  multi_boot_standard(column = "model_eta")


bootd3df %<>% ungroup() %>%
  mutate(vspeak = plyr:::revalue(as.factor(vspeak),c("TRUE"="verified","FALSE"="unverified"))) %>%
#   mutate(category = plyr:::revalue(as.factor(category),
#                                    c("certain"="\ncertainty",
#                                      "conj"="conjunction",
#                                      "discrep"="\ndiscrepancy",
#                                      "excl"="exclusion",
#                                      "i"="\nI",
#                                      "incl"="inclusion",
#                                      "ipron"="\nindef. pro.",
#                                      "negate"="negation",
#                                      "preps"="\npreposition",
#                                      "quant"="quantifier",
#                                      "tentat"="\ntentative",
#                                      "you"="\nyou")))
  mutate(category = plyr:::revalue(as.factor(category),
                                 c("certain"="certainty",
                                   "conj"="conjunction",
                                   "discrep"="discrepancy",
                                   "excl"="exclusion",
                                   "i"="I",
                                   "incl"="inclusion",
                                   "ipron"="indef. pro.",
                                   "negate"="negation",
                                   "preps"="preposition",
                                   "quant"="quantifier",
                                   "tentat"="tentative",
                                   "you"="you")))

temp <- as.data.frame(d3) %>%
  group_by(category) %>%
  summarize(n=n()) %>%
  arrange(desc(n)) %>%
  mutate(category = plyr:::revalue(as.factor(category),
                                   c("certain"="certainty",
                                     "conj"="conjunction",
                                     "discrep"="discrepancy",
                                     "excl"="exclusion",
                                     "i"="I",
                                     "incl"="inclusion",
                                     "ipron"="indef. pro.",
                                     "negate"="negation",
                                     "preps"="preposition",
                                     "quant"="quantifier",
                                     "tentat"="tentative",
                                     "you"="you")))

bootd3df$category <- factor(bootd3df$category,levels=bootd4df$category) 

ggplot(aes(x=category,y=mean,fill=vspeak),data=bootd3df) +
  geom_bar(stat="identity",position="dodge",width=.7) +
  geom_linerange(aes(ymin=ci_lower,ymax=ci_upper),position=position_dodge(width=.7),size=1.3) +
  theme_bw() +
  scale_color_brewer(palette = "Set1") +
  theme_bw(base_size = 20) +
  theme(panel.grid = element_blank(),axis.title.x=element_blank()) +
  labs(#title='Model-estimated Alignment to Verified/Unverified Speakers',
    #x='Child\'s Age (months)',
    y='Alignment (delta log-odds)') +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 90, hjust = 1,vjust=0.5))


#Plotting & computing twitter_diff_14cats.pdf
bootd4df <- as.data.frame(d3) %>%
  group_by(vspeak,category) %>%
  select(vspeak,category,rid,model_eta) %>%
  spread(vspeak,model_eta) %>%
  na.omit() %>%
  transmute(category,rid,diff=`TRUE`-`FALSE`) %>%
  group_by(category) %>%
  multi_boot_standard(column = "diff") 

bootd4df %<>% ungroup() %>%
  mutate(category = plyr:::revalue(as.factor(category),
                                   c("certain"="certainty",
                                     "conj"="conjunction",
                                     "discrep"="discrepancy",
                                     "excl"="exclusion",
                                     "i"="I",
                                     "incl"="inclusion",
                                     "ipron"="indef. pro.",
                                     "negate"="negation",
                                     "preps"="preposition",
                                     "quant"="quantifier",
                                     "tentat"="tentative",
                                     "you"="you"))) %>%
  arrange(desc(ci_lower))

bootd4df$category <- factor(bootd4df$category,levels=bootd4df$category) 


ggplot(aes(x=category,y=mean),data=bootd4df) +
  #geom_bar(stat="identity",position="dodge") +
  geom_pointrange(aes(ymin=ci_lower,ymax=ci_upper),position="dodge") +
  geom_hline(yintercept=0,linetype='dotted') +
  scale_color_brewer(palette = "Set1") +
  theme_bw(base_size = 20) +
  theme(panel.grid = element_blank(),axis.title.x=element_blank()) +
  labs(#title='Model-estimated Alignment to Verified/Unverified Speakers',
    #x='Child\'s Age (months)',
    y='Alignment (delta log-odds)') +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 90, hjust = 1,vjust=0.5))