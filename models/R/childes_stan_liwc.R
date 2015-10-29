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


convert.age <- function(childes.age) {
  age <- as.numeric(unlist(strsplit(childes.age, "[PYMD]"))[2:4])
  age[2] <- ifelse(is.na(age[2]), 0, age[2])
  age[3] <- ifelse(is.na(age[3]), 0, age[3])
  
  return((age[1]*365+age[2]*30.5+age[3])/30.5)
}

read.data.file <- function(file) {
  read_csv(paste0('../Childes_results/', file)) %>%
    mutate(type = ifelse(str_count(file,"Stem") > 0, "Stemmed", "Unstemmed")) %>%
    rename(marker = category)
}

read.marker.file <- function(file) {
  read_csv(paste0('../Marker_Lists/', file)) %>%
    mutate(type = ifelse(str_count(file,"Stem") > 0, "Stemmed", "Unstemmed"),
           corpus = sub("Marker.*","",file),"") %>%
    select(Word,Frequency,type,corpus) %>%
    rename(marker = Word)
}

userinfo.files <- list.files(path = "../Childes_userinfo", 
                             pattern = '*.csv', all.files = FALSE)

result.files <- list.files(path = "../Childes_results", 
                           pattern = '*Results.liwc2007_converted.csv', all.files = FALSE)

marker.files <- list.files(path = "../Marker_Lists/", 
                           pattern = '*Freq.csv', all.files = FALSE)


childes.userinfo <- bind_rows(lapply(userinfo.files, function(file) {
  read_csv(paste0('../Childes_userinfo/',
                  file))})) %>%
  rename(Child = role) %>%
  rowwise() %>%
  mutate(Age = convert.age(Age))

childes.markers <- bind_rows(lapply(marker.files, read.marker.file))

childes.results <- bind_rows(lapply(result.files, read.data.file)) %>%
  rename(DocId = docId)

childes.data <- left_join(childes.results,childes.userinfo) %>%
  left_join(childes.markers) %>%
  rename(Speaker = speakerId, Replier = replierId) %>%
  filter(Speaker %in% c("CHI", "MOT"),Replier %in% c("CHI", "MOT")) 

###########################
# Stop here if reloading d3 from saved files!

for (currcorpus in unique(childes.data$corpus)) {
d2 <- childes.data %>%
  filter(corpus==currcorpus,type == "Unstemmed") %>%
  mutate(Age = floor(Age)) %>%
  group_by(Child,Age,Speaker,corpus,marker) %>%
  summarise_each(funs(sum), ba, nba, bna, nbna) %>%
  unite(sid, Speaker, Age)

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

save(d3,file=paste0('results/',currcorpus,'_liwcdnm_d3.RData'))

}

#################################################
# Resume here if loading d3 from saved files!

if (exists('d3all')) {
  rm('d3all')
}
for (currcorpus in unique(childes.data$corpus)) {
  load(paste0('results/',currcorpus,'_liwcdnm_d3.RData'))
  if (exists('d3all')) {
    d3all %<>% bind_rows(d3)    
  } else {
    d3all <- d3
  }
}
 
d4 <- d3all %>%
  separate(sid, c("Speaker", "Age"), sep = "_") %>%
  mutate(Age = as.numeric(Age)) %>%
  group_by(Child, Speaker, Age, corpus) %>%
  multi_boot_standard("model_eta", na.rm = T)

d4 %<>% ungroup() %>%
  mutate(Speaker = plyr:::revalue(as.factor(Speaker),c("CHI"="Child","MOT"="Mother")))

ggplot(aes(x = as.numeric(Age), y = mean, colour = Speaker,
           group = interaction(Child, Speaker)), data = d4) +
           #), data=d4) + 
  facet_wrap(~Child) + 
  geom_pointrange(aes(ymax = ci_upper, ymin = ci_lower)) +
  geom_smooth(method = "loess") +
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

bootd3df <- d3all %>%
  separate(sid, c("Speaker", "Age"), sep = "_") %>%
  mutate(Age = as.numeric(Age)) %>%
  group_by(Speaker,marker,Age) %>%
  select(Speaker,marker,Age,model_eta) %>%
  summarize(model_eta=mean(model_eta)) %>%
  spread(Speaker,model_eta) %>%
  na.omit() %>%
  transmute(marker,Age,diff=CHI-MOT) %>%
  group_by(marker) %>%
  multi_boot_standard(column = "diff") 

bootd3df %<>% ungroup() %>%
  mutate(marker = plyr:::revalue(as.factor(marker),
                                   c("certain"="\ncertainty",
                                     "conj"="conjunction",
                                     "discrep"="\ndiscrepancy",
                                     "excl"="exclusion",
                                     "i"="\nI",
                                     "incl"="inclusion",
                                     "ipron"="\nindef. pro.",
                                     "negate"="negation",
                                     "preps"="\npreposition",
                                     "quant"="quantifier",
                                     "tentat"="\ntentative",
                                     "you"="\nyou")))

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
