library(tidyverse)
library(stringr)
options(tibble.print_min = 100)
data = read_csv("savant_data_stealing.csv",guess_max = 10000) %>% janitor::clean_names()

# stolethird =
# data %>% 
#   mutate(pitchID = row_number()) %>%
#   # select(pitchID, events,des,pitch_type,release_speed,batter,at_bat_number,on_1b,on_2b,on_3b) %>%
#   select(pitchID, batter, pitcher,balls, strikes,at_bat_number,on_2b,on_3b) %>%
#   mutate_at(c("on_2b","on_3b"), as.numeric) %>%
#   mutate(on_2b = na_if(on_2b,"null"), on_3b = na_if(on_3b,"null")) %>%
#   group_by(batter,pitcher,at_bat_number) %>%
#   arrange(batter) %>%
#   # filter(sum(is.na(on_1b))>0&&sum(!is.na(on_1b))>0)
#   filter(sum(is.na(on_2b))>0) %>%
#   filter(sum(!is.na(on_2b))>0) %>%
#   # group_by(on_2b,on_3b) %>%
#   ungroup() %>%
#   mutate(groupnumber = row_number()) %>%
#   group_by(batter,pitcher,at_bat_number) %>%
#   mutate(help = sum(!is.na(on_3b))) %>%
#   filter(help!=0) %>%
#   filter(sum(is.na(on_3b))!=0) %>%
#   group_by(batter,pitcher,at_bat_number,on_3b) %>%
#   filter(is.na(on_2b)|is.na(on_3b)) %>%
#   group_by(batter,pitcher,at_bat_number) %>%
#   mutate(row_number()) %>%
#   ungroup() %>%
#   filter(!is.na(on_2b))




  
##  
stolesecond =
data %>% 
  mutate(pitchID = row_number()) %>%
  # select(pitchID, events,des,pitch_type,release_speed,batter,at_bat_number,on_1b,on_2b,on_3b) %>%
  select(pitchID, batter, pitcher,balls, strikes,at_bat_number,on_1b,on_2b,on_3b) %>%
  mutate_at(c("on_1b","on_2b","on_3b"), as.numeric) %>%
  mutate(on_1b = na_if(on_1b,"null"), on_2b = na_if(on_2b,"null"), on_3b = na_if(on_3b,"null")) %>%
  group_by(batter,pitcher,at_bat_number) %>%
  arrange(batter) %>%
  filter(sum(is.na(on_2b))>1&&sum(!is.na(on_2b))>1) %>%
  group_by(batter,pitcher,at_bat_number,on_2b) %>%
  mutate(groupID = row_number())  %>%
  ungroup() %>%
  filter(!is.na(on_1b)|!is.na(on_2b)) %>%
  filter(!is.na(on_2b)) %>%
  group_by(batter,pitcher,at_bat_number,on_2b) %>%
  filter(groupID == max(groupID)) %>% ## this is the pitch he stole from 1st to second 
  ungroup() %>%
  select(pitchID,batter) %>%
  rename(stole2nd = batter) 

secondseal =
data %>%
  # select(game_date,batter, des,pitcher,balls, strikes,outs_when_up,at_bat_number,on_1b,on_2b,on_3b, release_speed,pitch_type) %>%
  mutate(pitchID = row_number(), des = na_if(des,"null")) %>%
  left_join(stolesecond) %>%
  mutate(stole2nd = ifelse(is.na(stole2nd), "No","Yes")) %>% 
  mutate(stole2ndcaught = ifelse(str_detect(des,"caught")&str_detect(des,"2nd base"), "Yes", "No")) %>% 
  mutate(stealattempt = ifelse(stole2nd=="Yes"|stole2ndcaught=="Yes","Yes","No")) %>%
  mutate(release_speed = as.numeric(release_speed)) %>% filter(stealattempt == "Yes")
  select(pitchID,des, stole2nd,stole2ndcaught,stealattempt) 


secondseal %>% names
  # select(pitch_type,release_speed,stole2nd) %>%
  select(release_speed,stole2nd, outs_when_up) %>%
  gather(key, other,-release_speed,-stole2nd) %>%
  ggplot(aes(x=release_speed, fill = stole2nd)) + geom_density(, alpha = .5) +
  facet_wrap(~other)

secondseal %>% 
  select(pitch_type,release_speed,pitch_type,stole2nd) %>%
  # gather(key, other,-release_speed,-stole2nd) %>%
  ggplot(aes(x=release_speed, fill = stole2nd)) + geom_density(, alpha = .5) 
  # facet_wrap(~other)


data %>% 
  select(on_1b,on_2b,on_3b) %>%
  mutate(on_1b = na_if(on_1b,"null"), on_2b = na_if(on_2b,"null"), on_3b = na_if(on_3b,"null")) %>%
  mutate(onbase = if_else(!is.na(on_1b)|!is.na(on_2b)|!is.na(on_3b),"Yes","No"))
  select(stole2nd,balls, strikes, release_speed)
         