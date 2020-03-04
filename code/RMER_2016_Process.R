# Data Processing 2016 Data 

library(naniar)
library(tidyverse)
library(qualtRics)
library(here)
library(janitor)

RMER2016 <- qualtRics::read_survey(here::here("data", "RMER_2016.csv")) %>% 
  clean_names()

#filter out bad data 
RMER2016Filter <-  RMER2016 %>% 
  filter(music_access == 1, # 1 indicated they could do the study
         mip_timing_page_submit > 300) %>%  # Make sure participants were on page for the full time
  mutate(condition_time = rowSums(dplyr::select(., 
                                                control_timing_page_submit, 
                                                cond_ss_timer_page_submit, 
                                                chet_timing_page_submit), 
                                  na.rm = TRUE), 
    condition = as_factor(rowSums(dplyr::select(., 
                                                cond_control, 
                                                cond_chet,
                                                cond_self_selected), 
                                  na.rm = TRUE)),
    condition = fct_recode(condition, 
                           control = "1", 
                           sad = "2", 
                           self = "3"),
    age = as.integer(age)) %>% 
  filter(condition_time > 280, 
         age < 26,
         condition != "0") 

# select relevant variables 
RMER2016Clean <- RMER2016Filter %>%  
  select(response_id, 
         condition, 
         age:sample, 
         contains("poms"),
         muisc_info4_1, 
         lpq1_1:lpq3_1,
         rrq1_1:rrq12_1, 
         contains("mars"),
         self_selected_song1) %>% 
  mutate(rrq6_orig = rrq6_1, # reverse coding
         rrq9_orig = rrq9_1, 
         rrq10_orig = rrq10_1,
         rrq6_1 = 6 - rrq6_orig, 
         rrq9_1 = 6 -rrq9_orig, 
         rrq10_1 = 6 - rrq10_orig) 

# score variables 

RMER2016Clean <- RMER2016Clean %>% 
  mutate(sad_baseline = rowSums(select(., pomsdsf1_1: pomsdsf8_1)), 
         sad_postinduction  = rowSums(select(., pomsdsf1_2: pomsdsf8_2)), 
         sad_postlistening = rowSums(select(., pomsdsf1_3: pomsdsf8_3)), 
         pos_baseline = rowSums(select(., pomsvsf1_1: pomsvsf5_1)), 
         pos_postinduction = rowSums(select(., pomsvsf1_2: pomsvsf5_2)), 
         pos_postlistening = rowSums(select(., pomsvsf1_3: pomsvsf5_3)), 
         rum = rowMeans(select(., rrq1_1:rrq12_1), na.rm = TRUE))
                                          

# export wide 
RMER2016Clean %>% 
  write_rds(here::here("data", "RMER_2016_ProcessedWide.rds"))

RMER2016Clean %>% 
  write_csv(here::here("data", "RMER_2016_ProcessedWide.csv"))


# make long 
RMER2016Long <- RMER2016Clean %>% 
  select(response_id, contains("sad"), contains("pos")) %>% 
  pivot_longer(
    cols = -response_id,
    names_to = c(".value", "timepoint"), 
    names_sep = "_"
    ) %>% 
  left_join(select(RMER2016Clean, rum, condition, response_id), by = "response_id")

# export long 
RMER2016Long %>% 
  write_rds(here::here("data", "RMER_2016_ProcessedLong.rds"))

RMER2016Long %>% 
  write_csv(here::here("data", "RMER_2016_ProcessedLong.csv"))
