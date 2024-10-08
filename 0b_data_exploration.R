# load packages
library(tidyverse)
library(tidymodels)
library(ggplot2)
library(patchwork)
library(corrplot)
library(ggpubr)
library(outliers)

# Handle common conflicts
tidymodels_prefer()

# load cleaned data
song_data <- read_csv("data/clean data/song_data_EDA.csv") %>% view()
names(song_data)
# set seed
set.seed(2023)

# counting the top 10 frequent genres 
genre_counts <- song_data %>%
  # remove NA values
  # split by column and comma
  # mutate(genre_words = str_split(genres, pattern = ", ' |', |'")) %>%
  # unnest(genre_words) %>% 
  # # remove empty genre words
  # filter(genre_words != "") %>%
  # ungroup() %>%   
  # count the frequency of each genre word
  group_by(genre_words) %>%
  count() %>%
  arrange(desc(n)) %>%
  head(15)

genre_counts
# create a bar plot of the top 10 genre word frequency
counts <- genre_counts %>% 
  arrange(desc(n)) %>%
  ggplot(aes(x = reorder(genre_words, n), y = n)) +
  geom_col(
    color = "Black",
    fill = "#BCABCB"
  ) + 
  xlab("10 Top Genre Words") +
  ylab("Frequency") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  ggtitle("Genre words frequency")

counts 

ggsave("count.png", plot = counts)

# Create a boxplot of danceability by top genres
top_genres <- song_data %>%
  count(genre_words) %>%
  top_n(15) %>%
  pull(genre_words)

song_data_top_genres <- song_data %>%
  filter(genre_words %in% top_genres) %>% 
  group_by(genre_words) %>%
  summarize(mean_danceability = mean(danceability), .groups = "drop") %>%
  arrange(desc(mean_danceability)) %>%
  slice(1:15)

song_data_top_genres$genre_words <- 
  reorder(song_data_top_genres$genre_words, 
          -song_data_top_genres$mean_danceability)

genre_danceability <- 
  song_data_top_genres %>% 
  ggplot(aes(x = genre_words, y = mean_danceability)) +
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  labs(x = "Genres", y = "Danceability")

ggsave("genre_danceability.png", plot = genre_danceability)

# # Create a scatterplot matrix of several variables
# ggplot(song_data, aes(x = danceability, y = energy)) +
#   geom_point() +
#   facet_grid(~ genre_words) +
#   labs(x = "Danceability", y = "Energy")

# corr plot between variables
song_data_cor <- song_data %>%
  select(acousticness, danceability, 
         energy, instrumentalness, liveness, 
         loudness, speechiness, tempo, valence, 
         key, mode, count, duration) %>%
  cor()

corrplot1 <- corrplot(
         song_data_cor, method = "circle", 
         type = "upper", order = "hclust", 
         tl.col = "black", tl.srt = 45, tl.cex = 0.7,
         ) # can only consider loudness, replace "acousticness" and "energy"

# correlation with target variable 
## example 1: 
ggplot(song_data, aes(x = instrumentalness, y = popularity)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  xlab("Instrumentalness") +
  ylab("Popularity") +
  ggtitle("Scatterplot of Popularity vs Instrumentalness")

## aggregated graph: 
correlation2<- song_data %>%
  #delete 'valence' because it's similar to 'mode'- mode easier
  #delete 'speechness' because it's kind of determined by genre
  #delete 'count' cause it counts the time?
  select(danceability, instrumentalness, liveness, 
         loudness, speechiness, tempo, valence
         key, mode, count, duration, popularity) %>%
  gather(variable, value, -popularity) %>%
  ggplot(aes(x = value, y = popularity)) +
  geom_point(alpha = 0.5, size = 0.5) +
  geom_smooth(method = "lm", se = FALSE) +
  facet_wrap(~variable, scales = "free") +
  labs(x = "Variable", y = "Popularity") 
  #stat_cor(method = "pearson", label.y = 0.9, label.x = 0.9)

song_data_cor <- song_data %>%
  select(count, popularity) %>%
  cor()

count_popularity_cor <- 
  song_data_cor["count", "popularity"]

count_popularity_cor

# outliers <- 
#   identify_outliers(song_data$count, 
#                     song_data$popularity, plot = TRUE)


# Identify outliers of Loudness using a boxplot
song_data %>% 
  ggplot(aes(x = "Loudness", y = loudness)) +
  geom_boxplot() +
  labs(x = "", y = "Loudness")

song_data %>% 
  ggplot(aes(x = "duration_ms", y = duration_ms)) +
  geom_boxplot() +
  labs(x = "", y = "duration_ms")

sapply(song_data, is.numeric)
numeric_cols <- sapply(song_data, is.numeric)
# stats <- song_data[, numeric_cols] %>% 
#   summarize_all(list(~list(quantile(., c(0.25, 0.75), na.rm = TRUE))))
# stats
names(song_data)


summary(song_data$duration_ms)
song_data <- song_data %>% 
  filter(loudness > -20)

