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

ci_32 <- function(x) { return(quantile(x,.32))}
ci_68 <- function(x) { return(quantile(x,.32))}

convertcategories2 <- function(d) {
  nocats <- FALSE
  if(is.null(d$category)) {
    d$category <- d$marker
    nocats <- TRUE
  }
  d %<>%
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
                                               "you"="2nd person")))
  if(nocats) { return(d %>% mutate(marker=category) %>% select(-category)) } 
  return(d)
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


resultsdir <- "../../results/"

read.data.file <- function(file) {
  read_csv(paste0(resultsdir, file)) %>%
    rename(marker=category)
}

userinfo.files <- list.files(path = resultsdir, 
                             pattern = 'SCOTUS_1979-2014_Roles.csv', all.files = FALSE)

result.files <- list.files(path = resultsdir, 
                           pattern = 'SCOTUS_1979-2014_Results.csv', all.files = FALSE)

scotus.userinfo <- bind_rows(lapply(userinfo.files, function(file) {
  read_csv(paste0(resultsdir,
                  file))})) %>%
  mutate(year=substr(rootdir,nchar(rootdir)-3,nchar(rootdir))) %>%
  #mutate(docId=paste(year,docId,sep="/")) %>%
  select(year,docId,id,role,sex)

#Temporary fix for the files that appear in multiple years - remove once they are handled correctly
scotus.userinfo %<>% 
  select(year,docId) %>%
  group_by(docId) %>%
  summarize(numyears=n_distinct(year)) %>%
  filter(numyears==1) %>%
  select(-numyears) %>%
  inner_join(scotus.userinfo,by="docId")
  
scotus.results <- bind_rows(lapply(result.files, read.data.file))

scotus.data <- left_join(scotus.results,scotus.userinfo,by=c("docId"="docId","replierId"="id")) %>%
   rename(replierRole=role,replierSex=sex) %>%
   left_join(scotus.userinfo,by=c("docId"="docId","speakerId"="id","year"="year")) %>%
   rename(speakerRole=role,speakerSex=sex) %>%
   filter(speakerRole!='Unidentified',replierRole!='Unidentified')

#Something weird is happening in the pre-1998 data, so removing it for now.
# (to see: ggplot(aes(x=year),data=scotus.data) + geom_histogram() )
# note: this isn't a result of removing the files that have names in multiple folders.
scotus.data %<>% filter(as.numeric(year)>=1998)

###########################
# Stop here if reloading d3 from saved files!

sd <- 0.5

d2 <- scotus.data %>%
  filter(speakerRole!=replierRole) %>%
  group_by(speakerId,replierId,speakerRole,replierRole,marker,year) %>%
  summarise_each(funs(sum), ba, nba, bna, nbna) %>%
  unite(sid,speakerRole,replierRole)

# d2 <- scotus.data %>%
#   group_by(speakerId,replierId,speakerRole,speakerSex,replierRole,replierSex,marker) %>%
#   summarise_each(funs(sum), ba, nba, bna, nbna) %>%
#   unite(sid, speakerRole,speakerSex,replierRole,replierSex)

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
                       CountsNotAB = d2$bna,
                       StdDev=sd)


fit <- stan(file = '../stan/alignment.www2016.stan', data = alignment_data, 
            iter = 30, chains =1, 
            include=T, pars=c("eta_subpop","eta_ab_subpop","eta_ab_observation"))

#mu_notab <- colMeans(rstan:::extract(fit,"mu_notab")$mu_notab)
#mu_ab <- colMeans(rstan:::extract(fit,"mu_ab")$mu_ab)

#If the model works, its mu_ values should be "smoothed" estimates of p(B|A) & p(B|notA)
#plot(mu_ab,d2$ba/(d2$ba+d2$nba))
#plot(mu_notab,d2$bna/(d2$bna+d2$nbna))

#checking in on the learned subpopulation values
eta_ab_subpop <- colMeans(rstan:::extract(fit,"eta_ab_subpop")$eta_ab_subpop)
eta_subpop <- colMeans(rstan:::extract(fit,"eta_subpop")$eta_subpop)

#Calculating 95% 
etas2 <- rstan:::extract(fit,"eta_ab_subpop")$eta_ab_subpop
meanetas <- apply(etas2,c(2,3),'mean')
ci_upper <- apply(etas2,c(2,3),'ci_upper')
ci_lower <- apply(etas2,c(2,3),'ci_lower')


table(alignment_data$SpeakerSubPop,d2$sid)

d <- bind_rows(data.frame(sid='Attorney_Justice',marker=levels(as.factor(d2$marker)),
                          model_subpop_mean=meanetas[1,],
                          model_subpop_ciu=ci_upper[1,],
                          model_subpop_cil=ci_lower[1,]),
               data.frame(sid='Justice_Attorney',marker=levels(as.factor(d2$marker)),
                          model_subpop_mean=meanetas[2,],
                          model_subpop_ciu=ci_upper[2,],
                          model_subpop_cil=ci_lower[2,]))

d3 <- d2 %>%
  mutate(model_eta = colMeans(rstan:::extract(fit,"eta_ab_observation")$eta_ab_observation)) %>%
  left_join(d,by=c('sid','marker'))

d <- convertcategories2(d)

d3 <- convertcategories2(d3)

save(d3,d,file=paste0('../results/SCOTUS_1979-2014_rolesasgroup.RData'))

#################################################
# Resume here if loading d3 & d from saved files!

load('../results/SCOTUS_1979-2014_rolesasgroup.RData')

ggplot(aes(y = marker, x = model_subpop_mean, colour = sid), data=d) + 
  geom_errorbarh(aes(xmax = model_subpop_ciu, xmin = model_subpop_cil,height=0)) +
  geom_point(size=3) +
  geom_vline(xintercept=0,lty=2) +
  theme_bw(base_size = 20) +
  theme(panel.grid = element_blank(),axis.title.y= element_blank()) +
  guides(color=FALSE) +
  scale_color_manual(values=c('#1f78b4','#33a02c','#e31a1c')) +
  facet_grid(group~sid,space='free_y',scale='free_y') +
  labs(y="marker category",x="alignment to first by second")

d4 <- d3 %>%
  mutate(justice=ifelse(sid=='Justice_Attorney',speakerId,replierId)) %>%
  mutate(speakerId=as.factor(speakerId),replierId=as.factor(replierId),sid=as.factor(sid),justice=as.factor(justice),year=as.numeric(year))

ggplot(aes(x = as.numeric(year), y = model_eta, colour = paste(group,sid)), data=d4) + 
  #geom_errorbarh(aes(xmax = model_subpop_ciu, xmin = model_subpop_cil,height=0)) +
  geom_smooth(method='loess') +
  theme_bw(base_size = 20) +
  theme(panel.grid = element_blank(),axis.title.y= element_blank()) +
  #guides(color=FALSE) +
  #scale_color_manual(values=c('#1f78b4','#33a02c','#e31a1c')) +
  scale_color_brewer(palette = 'Paired') +
  #facet_wrap(~justice) +
  labs(y="marker category",x="year")


ggplot(aes(y = marker, x = model_subpop_mean, colour = sid), data=d) + 
  geom_errorbarh(aes(xmax = model_subpop_ciu, xmin = model_subpop_cil,height=0)) +
  geom_point(size=3) +
  geom_vline(xintercept=0,lty=2) +
  theme_bw(base_size = 20) +
  theme(panel.grid = element_blank(),axis.title.y= element_blank()) +
  guides(color=FALSE) +
  scale_color_manual(values=c('#1f78b4','#33a02c','#e31a1c')) +
  facet_grid(group~sid,space='free_y',scale='free_y') +
  labs(y="marker category",x="alignment to first by second")

###################################
# Experimental model with different values for each justice

sd <- 0.5

d2 <- scotus.data %>%
  filter(speakerRole!=replierRole) %>%
  mutate(justice=ifelse(speakerRole=='Justice',speakerId,replierId)) %>%
  group_by(justice,speakerRole,replierRole,marker,year) %>%
  summarise_each(funs(sum), ba, nba, bna, nbna) %>%
  unite(sid,justice,speakerRole,replierRole,year)

# d2 <- scotus.data %>%
#   group_by(speakerId,replierId,speakerRole,speakerSex,replierRole,replierSex,marker) %>%
#   summarise_each(funs(sum), ba, nba, bna, nbna) %>%
#   unite(sid, speakerRole,speakerSex,replierRole,replierSex)

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
                       CountsNotAB = d2$bna,
                       StdDev=sd)


fit <- stan(file = '../stan/alignment.www2016.stan', data = alignment_data, 
            iter = 500, chains =1, 
            include=T, pars=c("eta_ab_subpop"))

etas2 <- rstan:::extract(fit,"eta_ab_subpop")$eta_ab_subpop
meanetas <- apply(etas2,c(2,3),'mean')
ci_uppers <- apply(etas2,c(2,3),'ci_68')
ci_lowers <- apply(etas2,c(2,3),'ci_32')

convertetas <- function(d) {
  dm <- data.frame(d)
  colnames(dm) <- levels(as.factor(d2$marker))
  dm$sid <- levels(as.factor(d2$sid))
  return(dm %>% gather(marker,value,-sid))
}

dm <- convertetas(meanetas) %>%
  mutate(mean=value) %>% select(-value) %>%
  left_join(convertetas(ci_uppers),by=c('sid','marker')) %>%
  mutate(ciu=value) %>% select(-value) %>%
  left_join(convertetas(ci_lowers),by=c('sid','marker')) %>%
  mutate(cil=value) %>% select(-value) %>%
  separate(sid,c('justice','speakerRole','replierRole','year'),sep='_') %>%
  unite(sid,speakerRole,replierRole)

dm %<>% filter(justice!='THOM')  #removing the near-silent Clarence Thomas

dm %<>% convertcategories2()

ggplot(aes(x = as.numeric(year), y = mean, colour = sid), data=dm) + 
  geom_pointrange(aes(ymax = ciu, ymin = cil,height=0),alpha=0.5) +
  geom_smooth(method='loess',size=1.1) +
  theme_bw(base_size = 20) +
  theme(panel.grid = element_blank(),axis.title.y= element_blank()) +
  guides(alpha=FALSE) +
  scale_color_brewer(palette = "Set1") +
  #scale_color_manual(values=c('#1f78b4','#33a02c','#e31a1c')) +
  #scale_color_brewer(palette = 'Paired') +
  facet_wrap(~justice) +
  labs(y="marker category",x="year")

ggplot(aes(x = as.numeric(year), y = mean, colour = sid), data=dm) + 
  geom_pointrange(aes(ymax = ciu, ymin = cil,height=0),alpha=0.5) +
  geom_smooth(method='loess',size=1.1) +
  theme_bw(base_size = 20) +
  theme(panel.grid = element_blank(),axis.title.y= element_blank()) +
  guides(alpha=FALSE) +
  scale_color_brewer(palette = "Set1") +
  #scale_color_manual(values=c('#1f78b4','#33a02c','#e31a1c')) +
  #scale_color_brewer(palette = 'Paired') +
  facet_wrap(~marker) +
  labs(y="marker category",x="year")

ggplot(aes(x = as.numeric(year), y = mean, colour = sid
           #group = interaction(Child, Speaker)), data = d4) +
  ), data=dm) + 
  #facet_wrap(~Child) + 
  geom_pointrange(aes(ymax = ciu, ymin = cil),alpha=.5) +
  geom_smooth(method = "loess",size=1.1) +
  guides(alpha=FALSE) +
  scale_color_brewer(palette = "Set1") +
  theme_bw() + 
  #theme_bw(base_size = 20) +
  geom_hline(yintercept=0,linetype='dotted',size=1) +
  theme(panel.grid = element_blank()) +
  facet_wrap(~group) + #,scale='free_y') + 
  labs(title='Alignment by Marker Group over Time',
    x='Year',
    y='HAM Alignment',
    colour='Interaction Type')


#################################

 
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

