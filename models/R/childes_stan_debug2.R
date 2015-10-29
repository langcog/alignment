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
                           pattern = '*1.csv', all.files = FALSE)

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
  #left_join(childes.markers) %>%
  rename(Speaker = speakerId, Replier = replierId) %>%
  filter(Speaker %in% c("CHI", "MOT", "FAT", "INV"), Replier %in% c("CHI","MOT","FAT","INV")) 

###########################
# Stop here if reloading d3 from saved files!

for (currcorpus in unique(childes.data$corpus)) {
#for (currcorpus in c("Suppes","Thomas","Manchester"))
d2 <- childes.data %>%
  filter(corpus==currcorpus) %>%
  mutate(Age = floor(Age)) %>%
  group_by(Child,Age,Speaker,Replier,corpus,marker) %>%
  summarise_each(funs(sum), ba, nba, bna, nbna) %>%
  unite(sid, Speaker,Replier, Age)

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

#checking in on the learned subpopulation values
eta_ab_subpop <- colMeans(rstan:::extract(fit,"eta_ab_subpop")$eta_ab_subpop)
eta_subpop <- colMeans(rstan:::extract(fit,"eta_subpop")$eta_subpop)

d3 <- d2 %>%
  mutate(model_eta = colMeans(rstan:::extract(fit,"eta_ab_observation")$eta_ab_observation))

d3$model_mu <- log(colMeans(rstan:::extract(fit,"mu_ab")$mu_ab)) - 
    log(colMeans(rstan:::extract(fit,"mu_notab")$mu_notab))

d3$model_dnm <- colMeans(rstan:::extract(fit,"mu_ab")$mu_ab) - 
  colMeans(rstan:::extract(fit,"mu_notab")$mu_notab)

save(d3,file=paste0('results/',currcorpus,'_liwc2007_d3.RData'))

}

#################################################
# Resume here if loading d3 from saved files!

rm(d3all)
for (currcorpus in unique(childes.data$corpus)) {
  load(paste0('results/',currcorpus,'_liwc2007_d3.RData'))
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
  labs(title='Model-estimated Alignment to Mother and Child by Age',
       x='Child\'s Age (months)',
       y='Alignment (delta log-odds)',
       colour='Alignment to')

d4 <- d3all %>%
  separate(sid, c("Speaker", "Age"), sep = "_") %>%
  mutate(Age = as.numeric(Age)) %>%
  group_by(Speaker,Age) %>%
  multi_boot_standard("model_eta", na.rm = T)

d4 %<>% ungroup() %>%
  mutate(Speaker = plyr:::revalue(as.factor(Speaker),c("CHI"="Child","MOT"="Mother")))

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
  labs(title='Model-estimated Alignment to Mother and Child by Age',
       x='Child\'s Age (months)',
       y='Alignment (delta log-odds)',
       colour='Alignment to')
  

