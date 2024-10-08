### load packages

library(tidyverse)
library(tidymodels)
library(ggplot2)
library(patchwork)
#library(tidyr)

### Handle common conflicts
tidymodels_prefer()

### load cleaned data
song_data_raw <- read_csv("data/raw data/song_data_genres_processed .csv") %>% 
  janitor::clean_names() 

# set seed
set.seed(2023)

#check the NA values
song_data_raw %>% 
  skimr::skim_without_charts() 

### Variable inspection
#check target variables
p1 <- ggplot(song_data_raw, aes(popularity)) +
  geom_density() +
  theme_minimal()

p2 <- ggplot(song_data_raw, aes(popularity)) +
  geom_boxplot() +
  theme_void()

initial <- p2 / p1 +
  plot_layout(heights = c(1, 5))

ggsave("popularity1.png", plot = initial)

### Make modifications for variables
## Modify outcome variables
# Filter out all rows where the column "popularity" <10
song_data <- 
  song_data_raw %>% 
  filter(popularity > 10) 

p1 <- ggplot(song_data, aes(popularity)) +
  geom_density() +
  theme_minimal()

p2 <- ggplot(song_data, aes(popularity)) +
  geom_boxplot() +
  theme_void()

popularity2 <- p2 / p1 +
  plot_layout(heights = c(1, 5)) +
  ggtitle("Song popularity distribution")

popularity2

ggplot(song_data, aes(x = popularity)) +
  geom_histogram() +
  labs(x = "Popularity", y = "Count")

ggsave("popularity2.png", plot = popularity2)

# Looks like a normal distribution! 

summary(song_data)
#view(song_data)

## Cleaning "genres" variables
#skip NA values
song_data_no_NA <- song_data %>%
  filter(!is.na(genres)) 

dim(song_data_no_NA)

song_data_genre <- song_data_no_NA %>% 
  mutate(genre_words = str_split(genres, pattern = ", ' |', |'")) %>%
  unnest(genre_words) %>% 
  filter(genre_words != "") %>%
  ungroup() %>% 
  view()

song_data_genres <- song_data_genre %>%
  select(-genres) %>%
  distinct() %>% view()
# now "artists" is the primary key

### class imbalance
table(song_data_genres$genre_words)

song_data_genres %>% 
ggplot(aes(x = genre_words)) +
  geom_bar() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
# class imbalance detected 

song_data_cleaned <- song_data_no_NA %>% 
  mutate(duration = duration_ms/1000) %>% 
  select(-c(genres, artists, acousticness, energy, valence, key, duration_ms))
  
dim(song_data_cleaned)

song_data_genres <- song_data_genres %>% 
  mutate(duration = duration_ms/1000) 
  

#save cleaned dataset
write_csv(song_data_cleaned, file = "data/clean data/song_data_cleaned.csv")
write_csv(song_data_genres, file = "data/clean data/song_data_genres.csv")

