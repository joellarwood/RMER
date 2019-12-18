# Data Processing November 2018 Prolific Data 
  
library(visdat)
library(tidyverse)

## Load in Data
testvalues <- c("test", "joel test")

Nov18Raw <- qualtRics::read_survey(here::here("data", "RMER_November2018.csv")) %>% 
  replace_with_na_all(condition = ~.x == -99) %>% 
  distinct(ProlificID, .keep_all = TRUE) %>%  
  filter(Finished == 1) %>%  
  filter(DistributionChannel == "anonymous") %>%  
  filter(sad.song != testvalues)
  
visdat::vis_dat(Nov18Raw)

reguseupper <- as.numeric(quantile(Nov18Raw$reg.use_1, .66, na.rm = TRUE))
reguselower <-as.numeric(quantile(Nov18Raw$reg.use_1, .33, na.rm = TRUE))

# keep only full responses
Nov18BasicClean <- Nov18Raw %>%  
  dplyr::mutate(musebaq = rowSums(dplyr::select(., musebaq_1:musebaq_9)),
                Baseline = rowSums(dplyr::select(., deq_1_1:deq_1_4)),
                PostInduction = rowSums(dplyr::select(., deq_2_1: deq_2_4)),
                PostListening = rowSums(dplyr::select(., deq_3_1: deq_3_4)),
                rumination = rowMeans(dplyr::select(., rrq_1:rrq_12), na.rm = TRUE),
                musebaq = rowSums(dplyr::select(., musebaq_1:musebaq_9)),
                ResponseId = factor(ResponseId), 
                age = age + 17, 
                reg_use_factor = as_factor(if_else(reg.use_1 >= reguseupper, 
                                                   "Use", 
                                                   if_else(reg.use_1 <= reguselower,
                                                           "Not Use", 
                                                           "Middle")))) %>%  
  dplyr::mutate(BSR = mecscale_1 - 1, 
                Entrainment = mecscale_2 - 1, 
                Conditioning = mecscale_3 - 1, 
                Contagion = mecscale_4 - 1, 
                Imagery = mecscale_5 - 1, 
                Memory = mecscale_6 - 1, 
                Expectancy = mecscale_7 - 1, 
                Appraisal = mecscale_8 - 1) %>% 
  dplyr::distinct(ProlificID, .keep_all = TRUE)

#export 
Nov18BasicClean %>% 
  write_rds(here::here("data", "RMER_November2018_ProcessedWide.rds"))

Nov18BasicClean %>% 
  write_csv(here::here("data", "RMER_November2018_ProcessedWide.csv"))

