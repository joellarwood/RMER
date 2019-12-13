# Data Processing November 2018 Prolific Data 
library(tidyverse)
library(genius)
library(genius)


## Load in Data

Nov18BasicClean <-   read_rds(here::here("data", "RMER_November2018_ProcessedWide.rds"))

# get spotify data

## Script to access songs 

## Taken from https://mirr.netlify.com/audio-features-spotify.html 


track_audio_features <- function(artist, title, type = "track") {
  search_results <- spotifyr::search_spotify(paste(artist, title), type = type)
  
  track_audio_feats <- spotifyr::get_track_audio_features(search_results$id[[1]]) %>%
    dplyr::select(-id, -uri, -track_href, -analysis_url)
  
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

#Write CSV
Nov18Features %>% 
  write_csv(here::here("data", "RMER_November2018_Spotify.csv"))

#Write Rdata 
Nov18Features %>% 
  write_rds(here::here("data", "RMER_November2018_Spotify.rds"))






