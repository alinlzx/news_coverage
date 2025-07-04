source("header.R")

# reading in merged news/event df with sims --------- 
event_news_sim <- read_csv("data/mst/01_event_news_similarity_build.csv")

# checking out high similarity events/news -----------
event_news_sim %>% filter(cosine_sim > 0.5)  %>% View

# checking out nyt raw ------------
event_news_sim %>% filter(grepl("Myanm", nyt_abstract) | grepl("Myanma", nyt_title) ) %>% 
  select(nyt_title, nyt_abstract) %>% distinct() %>% View
