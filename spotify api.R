library(spotifyr)
library(dplyr)
library(purrr)
library(tidyr)

# set up OAuth access_token
token <- spotifyr::get_spotify_access_token(client_id = "9b913334c36344199697255d00334abf", 
                                            client_secret = "4b477b64f9554bb4bc9a97156ad64446")

# function to retrieve track information
get_track <- function(track_id) {
  track_info <- spotifyr::get_track(track_id,  access_token= token)
  track_features <- spotifyr::get_track_audio_features(track_id, access_token = access_token)
  artist_ids <- track_info$artists$id
  artist_names <- track_info$artists$name
  album_name <- track_info$album$name
  album_release_date <- track_info$album$release_date
  album_type <- track_info$album$type
  track_name <- track_info$name
  track_duration_ms <- track_info$duration_ms
  track_popularity <- track_info$popularity
  danceability <- track_features$danceability
  energy <- track_features$energy
  key <- track_features$key
  loudness <- track_features$loudness
  mode <- track_features$mode
  speechiness <- track_features$speechiness
  acousticness <- track_features$acousticness
  instrumentalness <- track_features$instrumentalness
  liveness <- track_features$liveness
  valence <- track_features$valence
  tempo <- track_features$tempo
  time_signature <- track_features$time_signature
  df <- data.frame(track_id = track_id, artist_id = artist_ids, artist_name = artist_names, album = album_name,
                   album_release_date = album_release_date, album_type = album_type, track_name = track_name,
                   track_duration_ms = track_duration_ms, track_popularity = track_popularity,
                   danceability = danceability, energy = energy, key = key, loudness = loudness,
                   mode = mode, speechiness = speechiness, acousticness = acousticness,
                   instrumentalness = instrumentalness, liveness = liveness, valence = valence, tempo = tempo,
                   time_signature = time_signature, stringsAsFactors = FALSE)
  return(df)
}


# function to retrieve artist genres
get_artist_genres <- function(artist_id, token) {
  artist_info <- spotifyr::get_artist(artist_id, token = token)
  genres <- artist_info$genres
  return(genres)
}

# define list of track IDs
track_ids <- c("4uLU6hMCjMI75M1A2tKUQC", "7xGfFoTpQ2E7fRF5lN10tr", "2XU0oxnq2qxCpomAAuJY8K")

# # Get track information for each track ID
# tracks <- data.frame()
# for (i in seq_along(track_ids)) {
#   track_info <- get_track(track_ids[i], access_token = token)
#   tracks <- rbind(tracks, track_info)
#   Sys.sleep(0.5) # add a pause of 0.5 seconds between requests to avoid overloading the API
# }
# 

# Get track information
tracks <- data.frame()
for (i in 1:length(track_ids)) {
  track_info <- get_track(track_ids[i], token = token)
  track_df <- data.frame(track_id = track_ids[i],
                         track_name = track_info$name,
                         album = track_info$album$name,
                         artists = toString(track_info$artists$name))
  tracks <- rbind(tracks, track_df)
}

# Get genre information for each artist
get_artist_genres <- function(artist_id) {
  artist_info <- get_artist(artist_id, access_token = access_token)
  genres <- artist_info$genres
  return(genres)
}

tracks$artist_id <- NA
for (i in 1:nrow(tracks)) {
  track_info <- get_track(tracks[i, "track_id"], access_token = access_token)
  artist_id <- track_info$artists$id[1]
  tracks[i, "artist_id"] <- artist_id
}

tracks$genres <- NA
for (i in 1:nrow(tracks)) {
  artist_id <- tracks[i, "artist_id"]
  if (!is.na(artist_id)) {
    genres <- get_artist_genres(artist_id)
    tracks[i, "genres"] <- toString(genres)
  }
}

# View the resulting data frame
View(tracks)
