liwcs <- read_tsv('../../wordlists/liwc2007_converted.tsv',
                  col_names = FALSE) %>%
  rename(word = X1, category = X2) 


shuffled_liwcs <- liwcs %>% 
  mutate(word = sample(word))

write_tsv(shuffled_liwcs, "../../wordlists/liwc2007_shuffled1.tsv",
          col_names = FALSE)

