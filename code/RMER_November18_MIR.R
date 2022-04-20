# Data Processing November 2018 Prolific Data 
library(tidyverse)
library(spotifyr)
library(genius)
library(tidytext)
library(stringdist)
source("~/Library/Mobile Documents/com~apple~CloudDocs/Research/SpotifyCredentials.R")

## Load in Data

Nov18BasicClean <-read_rds(here::here("data", "RMER_November2018_ProcessedWide.rds"))

# get spotify data

## Script to access songs 

## Taken from https://mirr.netlify.com/audio-features-spotify.html 

track_audio_features <- function(artist, title, type = "track") {
  search_results <- spotifyr::search_spotify(paste(artist, title), type = track)
  
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

Nov18Features %>% 
  mutate(participant_nom = glue::glue("{sad.song}_{sad.song.artist}")) %>% 
  select(song_artist, spotify_row, ID, participant_nom) %>%
  mutate(dist=stringdist(participant_nom, song_artist)) %>% 
  filter(dist>7) %>% 
  view()

# Note section below will not be reporducable given dynamic nature of spotify search algorothms
row_remove <- c(3, 5, 12, 20, 22, 29, 31, 35, 37, 39, 53, 54:56, 79, 86, 99, 98, 101, 103, 113, 114, 118, 121, 127, 129, 134, 141, 155, 159, 164, 173, 174, 177, 178, 183, 201, 203, 208, 211, 216, 221, 223, 225:227, 238, 242, 248, 250, 256, 257, 263, 276, 277, 284, 286, 292, 303, 309, 318, 321, 322, 331, 335, 340, 341, 348, 351, 352, 356, 357, 362, 385, 389, 390, 394, 395, 401, 402)

length(row_remove)

Nov18Features_Filtered <- Nov18Features %>% 
  filter(!spotify_row %in% row_remove)


#Write CSV
Nov18Features_Filtered %>% 
  write_csv(here::here("data", "RMER_November2018_Spotify.csv"))

#Write Rdata 
Nov18Features_Filtered %>% 
  write_rds(here::here("data", "RMER_November2018_Spotify.rds"))

Nov18Lyrics <- Nov18Features_Filtered %>% 
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





