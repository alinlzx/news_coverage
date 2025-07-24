source("header.R")

# function to clean strings
clean_str_custom <- function (str) {
  str %>% str_replace_all("\\-", " ") %>% 
    str_replace_all("\\s*\\([^\\)]+\\)", "") %>% 
    str_replace_all("[^[:alnum:]]", " ") %>% 
    stri_trans_general(id = "Latin-ASCII") %>% str_squish %>% 
    tolower
}

# reading in event data and grouping --------------------

acled_events <- read_csv("data/mst/00b_acled_events_filter.csv") %>% 
  select(-1)

acled_events_grouped <- acled_events %>% 
  group_by(year, country, region) %>% 
  summarise(
    total_fat = sum(fatalities),
    across(c(notes, actor1, actor2), ~paste(., collapse = "; "))
  ) %>% ungroup %>% 
  transform(
    # combining & deduping actors
    actors = paste(actor1, actor2, sep = "; ")
    # actors = str_split(actors, pattern = "; ") %>% unlist() %>% unique()
  ) %>% 
  select(-c(actor1, actor2)) 

actors_concat <- map(pull(acled_events_grouped, actors), 
        ~str_split(., pattern = "; ") %>% unlist() %>% unique() %>% paste(collapse="; ")) %>% 
  unlist() 

acled_events_w_actors <- acled_events_grouped %>% 
  select(-actors) %>% 
  cbind(actors = actors_concat)

# making an intermediate export
# acled_events_w_actors %>% write_csv("data/mst/00c_acled_events_grouped.csv")

# named entities manually extracted, then we import it again
acled_events_grouped_ner <- read_csv("data/mst/00d_acled_events_grouped_ner_manual.csv")


# making a crosswalk between ner and countries ---------------------

# split manual ner column
country_ner_xwalk <- acled_events_grouped_ner %>% 
  select(country, ner) %>% 
  transform(ner = str_replace_all(ner, ", ", "; ")) %>% 
  separate_longer_delim(ner, delim = "; ") %>% 
  na.omit() %>% 
  transform(ner = ner %>% clean_str_custom())

# all ner patterns in one place
ner_extract <- country_ner_xwalk %>% 
  summarise(ner = paste(ner, collapse = "|")) %>% 
  pull(ner) %>% 
  str_replace("|", " | ")

# then, read in nyt data --------------------------------

nyt_articles <- read_csv("data/mst/00_nyt_articles.csv") %>% select(-1)

# doing country named entity recognition on title and abstracts -------------------

# getting a list of all possible country codes:
countries <- unique(na.omit(c(
  countrycode::codelist$country.name.en,
  countrycode::codelist$cldr.short.en,
  countrycode::codelist$cldr.territory.en,
  countrycode::codelist$iso.name.en
))) %>% clean_str_custom

# flattening nyt article abstract and titles
nyt_articles_cleaned <- nyt_articles %>% 
  transform(
    nyt_txt = paste(nyt_title, nyt_abstract, sep = ". ") %>% clean_str_custom()
  ) 

# identifying countries based on nyt text -------------------

# poli/confli lexicon regex
poli_con_pattern <- poli_conflict_lexicon %>% paste(collapse = "|")

# first map text to manual named entities
nyt_articles_ner <- nyt_articles_cleaned %>% 
  select(news_index, nyt_txt) %>% 
  filter(grepl(poli_con_pattern, nyt_txt)) %>% 
  cbind(
    nyt_ner = map(.x = pull(., nyt_txt),
                      .f = ~str_extract_all(., ner_extract, simplify = T) %>% 
                        str_trim() %>% stri_remove_empty_na %>% paste(collapse = "; ")) %>%
                        unlist()
                      )




  
