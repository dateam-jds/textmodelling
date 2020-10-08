library(tidyverse) ###data wrangling
library(rtweet) ###data fetching
library(textmineR) ###data modeling / preprocessing
library(katadasaR) ###lemmatization bahasa indonesia
library(SnowballC) ###Steaming


# create token named "twitter_token"
twitter_token <- create_token(
  app = appname,
  consumer_key = key,
  consumer_secret = secret,
  access_token = access_token,
  access_secret = access_secret)


###fetching data & stopword

rstats_tweets <- search_tweets(q = "Jawa Barat",
                               n = 3000)

stopword <- read_csv("./df/stopword_list.csv", col_names = FALSE) %>% 
  rename(word = X1)
new_stop <- data.frame(word = c("jawa", "barat", "jabar"))
stopword <- rbind(stopword, new_stop)

#################### cleansing data
text <- rstats_tweets %>% 
  transmute(id = status_id,
            text = text %>% str_to_lower() %>%
              str_remove_all("([:punct:]|\\d*)") %>% 
              str_remove_all("(\n)") %>% 
              str_remove_all("(https.*)\\s|(https.*)$") %>% 
              str_replace_all('([[:alpha:]])\\1+', '\\1')) %>% 
  distinct(text, .keep_all = TRUE) %>% 
  na.omit


####tokenazation

tokens <- text %>% 
  tidytext::unnest_tokens(word, text) %>% 
  filter(word %>% nchar>3) %>% 
  mutate(word = word %>% wordStem("id"),
         word = word %>% sapply(katadasar)) %>% 
  anti_join(stopword, by = c("word")) %>% 
  group_by(id) %>%
  mutate(ind = row_number()) %>%
  ungroup %>% 
  spread(key = ind, value = word) %>% 
  mutate_all(., ~replace_na(., "")) %>% 
  unite(text, -id, sep =" ") %>% 
  mutate(text = text %>% trimws()) %>% 
  ungroup()

####document term matrix
dtm <- CreateDtm(tokens$text, 
                 doc_names = tokens$id, 
                 # stem_lemma_function = function (x) SnowballC::wordStem(x, "id"),
                 ngram_window = c(1, 2))
### Term frequencies matrix
tf <- TermDocFreq(dtm = dtm) %>% tibble()

# original_tf <- tf %>% select(term, term_freq,doc_freq) %>% tibble()
# 
# rownames(original_tf) <- 1:nrow(original_tf)
# 
# vocabulary <- tf$term[ tf$term_freq > 1 & tf$doc_freq < nrow(dtm) / 2 ]

