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
  read_csv(paste0('../Childes_Results/', file)) %>%
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
                           pattern = '*.csv', all.files = FALSE)

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
  filter(Speaker %in% c("CHI", "MOT")) %>%
  filter(type == "Unstemmed") %>%
  mutate(Age = floor(Age)) %>%
  group_by(Child,Age,Speaker,corpus,marker) %>%
  summarise_each(funs(sum), ba, nba, bna, nbna) %>%
  unite(sid, Speaker, Age)

d2 <- childes.data

# 
# d2 <- fread('../Childes_Results/Brown300StemResults.csv') %>%
#   rename(sid=speakerId,rid=replierId) %>%
#   filter(sid=='CHI'|sid=='MOT',rid=='CHI'|rid=='MOT') %>%
#   group_by(sid,docId) %>%
#   summarise_each(funs(sum), ba, nba, bna, nbna) %>%
#   ungroup() %>%
#   mutate(pa = log(ba/(ba+nba)) - log(bna/(bna+nbna)))

num_subpops <- length(unique(childes.data$sid))
num_markers <- length(unique(childes.data$marker))
#num_repliers <- length(unique(d2$rid))

alignment_data <- list(NumMarkers=num_markers,
                       NumSubPops = num_subpops,
                       NumObservations = nrow(childes.data),
                       SpeakerSubPop = as.numeric(as.factor(childes.data$sid)),
                       MarkerType = as.numeric(as.factor(childes.data$marker)),
                       NumUtterancesAB = childes.data$ba+childes.data$nba,
                       NumUtterancesNotAB = childes.data$bna+ childes.data$nbna,
                       CountsAB = childes.data$ba,
                       CountsNotAB = childes.data$bna)


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

save(d3,file='results/children_bywords_d3.RData')
 
d4 <- d3 %>%
  separate(sid, c("Speaker", "Age"), sep = "_") %>%
  mutate(Age = as.numeric(Age)) %>%
  group_by(Child, Speaker, Age,corpus) %>%
  multi_boot_standard("model_eta", na.rm = T)

ggplot(aes(x = as.numeric(Age), y = mean, colour = Speaker,
           group = interaction(Child, Speaker)), data = d4) +
           #), data=d4) + 
  facet_wrap(~Child) + 
  geom_pointrange(aes(ymax = ci_upper, ymin = ci_lower)) +
  geom_smooth(method = "loess") +
  scale_color_brewer(palette = "Set1") +
  theme_bw(base_size = 14) +
  theme(panel.grid = element_blank())

d4 <- d3 %>%
  separate(sid, c("Speaker", "Age"), sep = "_") %>%
  mutate(Age = as.numeric(Age)) %>%
  group_by(Speaker,Age) %>%
  multi_boot_standard("model_eta", na.rm = T)

d4 %<>% ungroup() %>%
  mutate(Speaker = plyr:::revalue(as.factor(Speaker),c("CHI"="to Child","MOT"="to Mother")))

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
       colour='Alignment')
  

