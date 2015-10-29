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

read.data.file <- function(file) {
  read_csv(paste0('../results/', file)) %>%
    rename(marker=category)
}

userinfo.files <- list.files(path = "../results/", 
                             pattern = 'SCOTUSRoles.csv', all.files = FALSE)

result.files <- list.files(path = "../results/", 
                           pattern = 'SCOTUSResults_2011_2014_14cats.csv', all.files = FALSE)

scotus.userinfo <- bind_rows(lapply(userinfo.files, function(file) {
  read_csv(paste0('../results/',
                  file))})) %>%
  select(docId,id,role,sex)

scotus.results <- bind_rows(lapply(result.files, read.data.file))

scotus.data <- left_join(scotus.results,scotus.userinfo,by=c("docId"="docId","replierId"="id")) %>%
   rename(replierRole=role,replierSex=sex) %>%
   left_join(scotus.userinfo,by=c("docId"="docId","speakerId"="id")) %>%
   rename(speakerRole=role,speakerSex=sex)

###########################
# Stop here if reloading d3 from saved files!

d2 <- scotus.data %>%
  group_by(speakerId,replierId,speakerRole,replierRole,marker) %>%
  summarise_each(funs(sum), ba, nba, bna, nbna) %>%
  unite(sid, speakerRole,replierRole)

d2 <- scotus.data %>%
  group_by(speakerId,replierId,speakerRole,speakerSex,replierRole,replierSex,marker) %>%
  summarise_each(funs(sum), ba, nba, bna, nbna) %>%
  unite(sid, speakerRole,speakerSex,replierRole,replierSex)

num_subpops <- length(unique(d2$sid))
num_markers <- length(unique(d2$marker))
#num_repliers <- length(unique(d2$rid))

alignment_data <- list(NumMarkers=num_markers,
                       NumSubPops = num_subpops,
                       NumObservations = nrow(d2),
                       SpeakerSubPop = as.numeric(as.factor(d2$sid)),
                       MarkerType = as.numeric(as.factor(d2$marker)),
                       NumUtterancesAB = d2$ba+d2$nba,
                       NumUtterancesNotAB = d2$bna+d2$nbna,
                       CountsAB = d2$ba,
                       CountsNotAB = d2$bna)


fit <- stan(file = 'alignment.vectorized.stan', data = alignment_data, 
            iter = 500, chains =1 )

mu_notab <- colMeans(rstan:::extract(fit,"mu_notab")$mu_notab)
mu_ab <- colMeans(rstan:::extract(fit,"mu_ab")$mu_ab)

#If the model works, its mu_ values should be "smoothed" estimates of p(B|A) & p(B|notA)
plot(mu_ab,d2$ba/(d2$ba+d2$nba))
plot(mu_notab,d2$bna/(d2$bna+d2$nbna))

#checking in on the learned subpopulation values
eta_ab_subpop <- colMeans(rstan:::extract(fit,"eta_ab_subpop")$eta_ab_subpop)
eta_subpop <- colMeans(rstan:::extract(fit,"eta_subpop")$eta_subpop)

d3 <- d2 %>%
  mutate(model_eta = colMeans(rstan:::extract(fit,"eta_ab_observation")$eta_ab_observation))

d3$model_mu <- log(colMeans(rstan:::extract(fit,"mu_ab")$mu_ab)) - 
    log(colMeans(rstan:::extract(fit,"mu_notab")$mu_notab))

d3$model_dnm <- colMeans(rstan:::extract(fit,"mu_ab")$mu_ab) - 
  colMeans(rstan:::extract(fit,"mu_notab")$mu_notab)

save(d3,file=paste0('results/SCOTUS_liwc14_rolesex_d3.RData'))

}

#################################################
# Resume here if loading d3 from saved files!

if (exists('d3all')) {
  rm('d3all')
}
  load(paste0('results/SCOTUS_liwc14_rolesex_d3.RData'))
  if (exists('d3all')) {
    d3all %<>% bind_rows(d3)    
  } else {
    d3all <- d3
  }
 
d4 <- as.data.frame(d3) %>%
  #mutate(dnm = (ba/(ba+nba))-((ba+bna)/(ba+bna+nba+nbna))) %>%
  #separate(sid, c("Speaker", "Age"), sep = "_") %>%
  #mutate(Age = as.numeric(Age)) %>%
  group_by(sid) %>%
  multi_boot_standard("model_eta", na.rm = T)

d4 %<>% ungroup() %>%
  mutate(Speaker = plyr:::revalue(as.factor(Speaker),c("CHI"="Child","MOT"="Mother")))


d4 %<>% separate(sid,c("speakerRole","speakerSex","replierRole","replierSex"),sep="_") %>%
  filter(speakerRole!=replierRole) %>%
  unite(roles,speakerRole,replierRole) %>%
  unite(sexes,speakerSex,replierSex)

ggplot(aes(x = sexes, y = mean#, colour = sid,
           #group = interaction(Child, sid)), data = d4) +
           ), data=d4) + 
  facet_wrap(~roles) + 
  geom_pointrange(aes(ymax = ci_upper, ymin = ci_lower)) +
  #geom_smooth(method = "loess") +
  scale_color_brewer(palette = "Set1") +
  theme_bw(base_size = 14) +
  theme(panel.grid = element_blank()) +
  labs(title='Model-estimated Alignment to Mother and Child by Age\n(LIWC Categories)',
       x='Child\'s Age (months)',
       y='Alignment (delta log-odds)',
       colour='Alignment to')

d4 <- d3all %>%
  separate(sid, c("Speaker", "Age"), sep = "_") %>%
  mutate(Age = as.numeric(Age)) %>%
  group_by(Speaker,Age) %>%
  multi_boot_standard("model_eta", na.rm = T)

d4 %<>% ungroup() %>%
  mutate(Speaker = plyr:::revalue(as.factor(Speaker),c("CHI"="to Child","MOT"="to Mother"))) %>%
  
d4sm <- d4 %>% 
  mutate(Age=as.numeric(Age)) %>%
  filter(Age>=25,Age<=60)

#This is the plot for results/children_eta_bywords.pdf
ggplot(aes(x = as.numeric(Age), y = mean, colour = Speaker,
           #group = interaction(Child, Speaker)), data = d4) +
           ), data=d4) + 
  #facet_wrap(~Child) + 
  geom_pointrange(aes(ymax = ci_upper, ymin = ci_lower)) +
  geom_smooth(method = "loess") +
  scale_color_brewer(palette = "Set1") +
  theme_bw(base_size = 14) +
  theme(panel.grid = element_blank()) +
  labs(title='Model-estimated Alignment to Mother and Child by Age\n(LIWC Categories)',
       x='Child\'s Age (months)',
       y='Alignment (delta log-odds)',
       colour='Alignment to')
  
#############################
# TEmporary catgeorical analysis

bootd3df <- as.data.frame(d3all) %>%
  #separate(sid, c("Speaker", "Age"), sep = "_") %>%
  #mutate(Age = as.numeric(Age)) %>%
  group_by(sid,marker) %>%
  select(sid,marker,model_eta) %>%
  summarize(model_eta=mean(model_eta)) %>%
  spread(sid,model_eta) %>%
  na.omit() %>%
  transmute(marker,diff=Justice_Other-Other_Justice) %>%
  group_by(marker) %>%
  multi_boot_standard(column = "diff") 

# bootd3df %<>% ungroup() %>%
#   mutate(marker = plyr:::revalue(as.factor(marker),
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

arrange(bootd3df,desc(mean))

ggplot(aes(x=marker,y=mean),data=bootd3df) +
  #geom_bar(stat="identity",position="dodge") +
  geom_pointrange(aes(ymin=ci_lower,ymax=ci_upper),position="dodge") +
  geom_hline(yintercept=0) +
  scale_color_brewer(palette = "Set1") +
  theme_bw(base_size = 14) +
  theme(panel.grid = element_blank(),axis.title.x=element_blank()) +
  labs(title='Model-estimated Increase in Alignment to Verified Speakers',
       #x='Child\'s Age (months)',
       y='Alignment (delta log-odds)')

#############

bootd3df <- as.data.frame(d3) %>%
  filter(sid!='Justice_Justice') %>%
  filter((ba+bna+nba+nbna)>=10) %>%
  group_by(sid,marker) %>%
  #summarise_each(funs(sum), ba, nba, bna, nbna) %>%
  #ungroup() %>%
  mutate(dnm = (ba/(ba+nba))-((ba+bna)/(ba+bna+nba+nbna))) %>%
  filter(marker!='i',marker!='we',marker!='you') %>%
  multi_boot_standard(column = "model_eta",nboot=500)

ggplot(aes(x=marker,y=mean,fill=sid),data=bootd3df) +
  geom_bar(stat="identity",position="dodge") +
  #geom_errorbar(aes(ymin=ci_lower,ymax=ci_upper),position="dodge") +
  theme_bw() +
  scale_color_brewer(palette = "Set1") +
  theme_bw(base_size = 14) +
  theme(panel.grid = element_blank(),axis.title.x=element_blank()) +
  labs(title='Model-estimated Alignment to Verified/Unverified Speakers',
       #x='Child\'s Age (months)',
       y='Alignment (delta log-odds)',
       fill='Aligning to')

#########

da <- as.data.frame(d3) %>%
  filter(sid=='Justice_Attorney') %>%
  mutate(justice=speakerId,type='Alignment TO Justice') %>%
  group_by(justice,type) %>%
  multi_boot_standard("model_eta", na.rm = T,nboot=500)

db <- as.data.frame(d3) %>%
  filter(sid=='Attorney_Justice') %>%
  mutate(justice=replierId,type='Alignment BY Justice') %>%
  group_by(justice,type) %>%
  multi_boot_standard("model_eta", na.rm = T,nboot=500)

d4 <- bind_rows(da,db)

tomean <- mean(da$mean)
bymean <- mean(db$mean)

ggplot(aes(x = justice, y = mean, colour = type,
  #group = interaction(Child, sid)), data = d4) +
  ), data=d4) +
  #facet_wrap(~type) +
  geom_pointrange(aes(ymax = ci_upper, ymin = ci_lower)) +
  geom_hline(yintercept=bymean,lty='dashed',color="#E41A1C") +
  geom_hline(yintercept=tomean,lty='dashed',color="#377EB8") +
  #geom_smooth(method = "loess") +
  scale_color_brewer(palette = "Set1") +
  theme_bw(base_size = 14) +
  theme(panel.grid = element_blank(),
        axis.title.x = element_blank()) +
  labs(title='Model-estimated Alignment by/to SCOTUS Justices\n(LIWC Categories)',
  #x='Child\'s Age (months)',
  y='Alignment (delta log-odds)',
  colour='Justice')

