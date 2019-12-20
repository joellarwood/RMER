# Data Processing November 2018 Prolific Data 
library(tidyverse)
library(spotifyr)
library(genius)


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
  mutate(song_artist = glue::glue("{song_rtn}_{artist_rtn}"))

#Write CSV
Nov18Features %>% 
  write_csv(here::here("data", "RMER_November2018_Spotify.csv"))

#Write Rdata 
Nov18Features %>% 
  write_rds(here::here("data", "RMER_November2018_Spotify.rds"))

Nov18Lyrics <- Nov18Features %>% 
  distinct(song_artist, .keep_all = TRUE) %>% 
  genius::add_genius(artist = artist_rtn, 
                     title = song_rtn, 
                     type = "lyrics") %>% 
  select(song_rtn, artist_rtn, song_artist,  lyrics)

Nov18Features_Lyrics <- Nov18Lyrics %>% 
  select(song_artist, lyric) %>% 
  right_join(Nov18Features, by = "song_artist")


# Write features and lyrics 
Nov18Features_Lyrics %>% 
  write_csv(here::here("data", "RMER_November2018_Spotify_Lyrics.csv"))

#Write Rdata 
Nov18Features_Lyrics %>% 
  write_rds(here::here("data", "RMER_November2018_Spotify_Lyrics.rds"))





