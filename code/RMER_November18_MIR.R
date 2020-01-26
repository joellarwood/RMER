# Data Processing November 2018 Prolific Data 
library(tidyverse)
library(spotifyr)
library(genius)
library(tidytext)
source("~/Library/Mobile Documents/com~apple~CloudDocs/Research/SpotifyCredentials.R")

## Load in Data

Nov18BasicClean <-read_rds(here::here("data", "RMER_November2018_ProcessedWide.rds"))

# get spotify data

## Script to access songs 

## Taken from https://mirr.netlify.com/audio-features-spotify.html 

track_audio_features <- function(artist, title, type = "track") {
  search_results <- spotifyr::search_spotify(paste(artist, title), type = type)
  
  spotify_artist <- dplyr::rename(
    tibble::enframe(search_results[[1]][[1]][[3]]), 
    artist_rtn = value)
  
  spotify_song <- dplyr::rename(
    tibble::enframe(search_results$name),
    song_rtn = value)
  
  spotify_meta_rtn <- dplyr::select(dplyr::inner_join(spotify_artist, spotify_song), -name) 
  
  track_audio_feats <- spotifyr::get_track_audio_features(search_results$id[[1]]) %>%
    dplyr::select(-uri, -track_href, -analysis_url)

  track_audio_feats <- cbind(spotify_meta_rtn, track_audio_feats)
  return(track_audio_feats)
}

possible_feats <- possibly(track_audio_features, otherwise = tibble())

#features <- dat %>%
#  mutate(audio_features = map2(artist, title, possible_feats)) %>%
#  unnest() %>% 
#  as_tibble()


Nov18Features <- Nov18BasicClean %>% 
  mutate(audio_features = map2(sad.song.artist, sad.song, possible_feats)) %>% 
  unnest(cols = c(audio_features)) %>% 
  as_tibble() 


Nov18Features <- Nov18Features %>% 
  mutate(song_artist = glue::glue("{song_rtn}_{artist_rtn}"), 
         spotify_row = row_number())

# Some participants had multiple songs returned. I need to filter this out manually. I also used this to remove mismatched songs (incl. live versions)

row_remove <- c(3, 5, 12, 16, 20, 22, 29, 31, 35, 37, 39, 53, 54:56, 74, 79, 97, 98, 100, 102, 112, 113, 116, 117, 120, 126, 128, 129, 154, 163, 173, 176, 177, 182, 191, 192, 202, 204, 209, 212, 217, 222, 224, 226:228, 239, 249, 257, 258, 264, 277, 278, 285, 293, 304, 310, 319, 322, 323, 327, 328, 334, 338, 343, 350, 353, 354, 358, 359, 364, 367, 387, 391, 392, 394, 396, 397, 403)

length(row_remove)

Nov18Features_Filtered <- Nov18Features %>% 
  filter(!spotify_row %in% row_remove)

#Filter to just important spotify variables
Nov18Features_Filtered_Select <- Nov18Features_Filtered %>% 
  select(ID, song_artist, valence, energy, artist_rtn, song_rtn)


#Write CSV
Nov18Features_Filtered_Select %>% 
  write_csv(here::here("data", "RMER_November2018_Spotify.csv"))

#Write Rdata 
Nov18Features_Filtered_Select %>% 
  write_rds(here::here("data", "RMER_November2018_Spotify.rds"))

Nov18Lyrics <- Nov18Features_Filtered_Select %>% 
  genius::add_genius(artist = artist_rtn, 
                     title = song_rtn, 
                      type = "lyrics") %>% 
  select(song_artist, lyric)

Nov18_Lyrics_Features <- Nov18Features_Filtered_Select %>% 
  left_join(Nov18Lyrics, by = "song_artist")

# Create sentiment scores 

sad_words <- tidytext::get_sentiments("nrc") %>% 
  filter(sentiment == "sadness") %>% 
  select(word) %>% 
  mutate(sad = T)

pos_words <- tidytext::get_sentiments("nrc") %>% 
  filter(sentiment == "positive") %>% 
  select(word) %>% 
  mutate(positive = T)

neg_words <- tidytext::get_sentiments("nrc") %>% 
  filter(sentiment == "negative") %>% 
  select(word) %>% 
  mutate(negative = T)

Nov18_Sent <- Nov18_Lyrics_Features %>% 
  unnest_tokens(word, lyric) %>%
  select(word, song_artist) %>% 
  anti_join(stop_words, by = 'word') %>%
  left_join(sad_words, by = 'word') %>% 
  left_join(pos_words, by = "word") %>% 
  left_join(neg_words, by = "word")  %>%  
  group_by(song_artist) %>% 
  summarise(word_count = n(),
            sad_count = sum(sad, na.rm = T), 
            pos_count = sum(positive, na.rm = T), 
            neg_count = sum(negative, na.rm = TRUE), 
            sad_total_ratio = round(exp(sad_count)/ exp(word_count), 4),
            neg_pos_ratio = round(exp(neg_count)/exp(pos_count), 4), 
            sad_pos_ratio = round(exp(sad_count)/exp(pos_count), 4)) %>% 
  mutate(bias = if_else(neg_pos_ratio > 1, 
                        "Negative", 
                        if_else (neg_pos_ratio< 1,
                                 "Positive",
                                 if_else(neg_pos_ratio == 1, 
                                         "Equal", "NA")))) 

Nov18_Features_Sent <- Nov18_Sent %>% 
  right_join(Nov18Features_Filtered_Select, by = "song_artist")

write_csv(Nov18_Features_Sent, here::here("data", "RMER_November2018_Features_Sentiment.csv"))

write_rds(Nov18_Features_Sent, here::here("data", "RMER_November2018_Features_Sentiment.rds"))





