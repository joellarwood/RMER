# Data Processing November 2018 Prolific Data 
  
library(naniar)
library(tidyverse)

## Load in Data

Nov18Raw <- qualtRics::read_survey(here::here("data", "RMER_November2018.csv")) %>% 
  replace_with_na_all(condition = ~.x == -99)

testvalues <- c("test", "joel test")

# keep only full responses
Nov18BasicClean <- Nov18Raw %>%  
  distinct(ProlificID, .keep_all = TRUE) %>%  
  filter(Finished == 1) %>%  
  filter(DistributionChannel == "anonymous") %>%  
  filter(sad.song != testvalues)

# Reverse code
Nov18BasicClean <- Nov18BasicClean %>% 
  mutate(rrq_6_Orig = rrq_6, 
         rrq_9_Orig = rrq_9, 
         rrq_10_Orig = rrq_10, 
         rrq_6 = 6 - rrq_6, 
         rrq_9 = 6 - rrq_9, 
         rrq_10 = 6 - rrq_10)

#score items
Nov18BasicClean <- Nov18BasicClean %>% 
  mutate(musebaq = rowSums(dplyr::select(., musebaq_1:musebaq_9)),
         Baseline = rowSums(dplyr::select(., deq_1_1:deq_1_4)),
         PostInduction = rowSums(dplyr::select(., deq_2_1: deq_2_4)),
         PostListening = rowSums(dplyr::select(., deq_3_1: deq_3_4)),
         rumination = rowMeans(dplyr::select(., rrq_1:rrq_12), na.rm = TRUE), 
         ResponseId = factor(ResponseId), 
         age = age + 17
  )

#export 
Nov18BasicClean %>% 
  write_rds(here::here("data", "RMER_November2018_ProcessedWide.rds"))

Nov18BasicClean %>% 
  write_csv(here::here("data", "RMER_November2018_ProcessedWide.csv"))

