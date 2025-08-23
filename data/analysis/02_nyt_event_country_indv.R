######################

# This script does 

#####################

source("header.R")


# importing cosine similarities ----------------

nyt_events_match <- read_csv("data/mst/01_event_news_similarity_build.csv") %>% 
  filter(cosine_sim > 0.35)

# importing nyt articles --> country build ---------------

nyt_country_cleaned <- read_csv("data/mst/05_nyt_to_countries.csv")

# joining the two builds --------------------

nyt_events_check <- nyt_events_match %>% 
  transform(country = tolower(country)) %>% 
  left_join(nyt_country_cleaned %>% select(news_index, nyt_country=country), by = c("news_index")) %>% 
  
  # filter out articles not mapped to a country
  filter(!is.na(nyt_country)) %>% 
  
  # if none of the articles are equivalent to the acled event country field, then probably not relevant
  group_by(event_index, news_index) %>% 
  mutate(max_country_equivalence = max(country == nyt_country)) %>% 
  filter(max_country_equivalence == 1)



