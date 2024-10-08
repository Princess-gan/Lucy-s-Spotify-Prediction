# # load required packages
# library(tidytext)
# library(dplyr)
# library(quanteda)
# 
# song_data <- read_csv("random data/song_data.csv") %>% 
#   janitor::clean_names() %>% 
#   view()
# 
# # assuming "song_name" is the column name in "song_data" that contains the song names
# corpus <- corpus(song_data, text_field = "song_name")
# 
# # remove stop words from the corpus
# tidy_corpus <- corpus %>%
#   tokens() %>%
#   tokens_remove(stopwords("english")) %>%
#   dfm()
# # find frequencey
# tidy_corpus %>%
#   count(word, sort = TRUE)

library(quanteda)

song_data <- read_csv("random data/song_data.csv") %>% 
  janitor::clean_names() 

# assuming "song_name" is the column name in "song_data" that contains the song names
corpus <- corpus(song_data, text_field = "song_name")

# remove stop words from the corpus
tidy_corpus <- corpus %>%
  tokens() %>%
  tokens_remove(stopwords("english"))

# count word frequency using quanteda
freq <- dfm(tidy_corpus) %>%
  topfeatures(decreasing = TRUE, n = 10)

freq
