######################

# This script picks out NER (entities, countries, capitals) from 
# nyt article texts, and imputes the country/countries that article
# references. 

# essentially a lexicon based classifier

#####################

source("header.R")

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
  summarise(ner = paste(ner, collapse = "\\b|\\b")) %>% 
  pull(ner) 

# then, read in nyt data --------------------------------

nyt_articles <- read_csv("data/mst/00_nyt_articles.csv") %>% select(-1)

# doing country named entity recognition on title and abstracts -------------------

# getting a list of all possible country codes
countries <- unique(na.omit(c(
  countrycode::codelist$country.name.en,
  countrycode::codelist$cldr.short.en,
  countrycode::codelist$cldr.territory.en,
  countrycode::codelist$iso.name.en
))) %>% clean_str_custom %>% 
  discard(~ . %in% c("us", "uk")) %>% 
  append(c("u s", "u k"))

# getting xwalk of country capitals to countries
country_capitals <- r2country::country_capital %>% 
  left_join(r2country::country_names, by = "ID") %>% 
  rename(country = name) %>% 
  mutate(across(c(country, capital), ~clean_str_custom(.)))

# concat country into regex pattern
country_extract <- countries %>% 
  paste(collapse = "|") 

# concat country capitals into regex pattern
country_capitals_extract <- country_capitals %>% 
  pull(capital) %>% 
  paste(collapse = "\\b|\\b")

# flattening nyt article abstract and titles
nyt_articles_cleaned <- nyt_articles %>% 
  transform(
    nyt_txt = paste(nyt_title, nyt_abstract, sep = ". ") %>% clean_str_custom()
  ) 



# identifying countries based on nyt text -------------------

# poli/confli lexicon regex
poli_con_pattern <- poli_conflict_lexicon_short %>% paste(collapse = "|")

# first map text to manual named entities
nyt_articles_ner <- nyt_articles_cleaned %>% 
  select(news_index, nyt_txt) %>% 
  filter(grepl(poli_con_pattern, nyt_txt)) %>% 
  cbind(
    nyt_ner = map(.x = pull(., nyt_txt),
                  .f = ~str_extract_all(., ner_extract, simplify = T) %>% 
                    str_trim() %>% stri_remove_empty_na %>% unique %>% 
                    paste(collapse = "; ")) %>%
      unlist()
  ) %>% 
  separate_longer_delim(nyt_ner, delim = "; ") %>% 
  # next use entities to identify countries
  left_join(country_ner_xwalk, by = c("nyt_ner" = "ner"))

# exporting a copy of nyt ner 
nyt_articles_ner %>% write_csv("data/mst/03_news_articles_ner.csv")


# also map texts directly to country names:
nyt_articles_countries <- nyt_articles_cleaned %>% 
  select(news_index, nyt_txt) %>% 
  filter(grepl(poli_con_pattern, nyt_txt)) %>% 
  cbind(
    country = map(.x = pull(., nyt_txt),
                  .f = ~str_extract_all(., country_extract, simplify = T) %>% 
                    str_trim() %>% stri_remove_empty_na %>% paste(collapse = "; ")) %>%
      unlist()
  )  %>%  separate_longer_delim(country, delim = "; ") 

# also map texts to country capitals:
nyt_articles_capitals <- nyt_articles_cleaned %>% 
  select(news_index, nyt_txt) %>% 
  filter(grepl(poli_con_pattern, nyt_txt)) %>% 
  cbind(
    capital = map(.x = pull(., nyt_txt),
                  .f = ~str_extract_all(., country_capitals_extract, simplify = T) %>% 
                    str_trim() %>% stri_remove_empty_na %>% paste(collapse = "; ")) %>%
      unlist()
  )  %>%  separate_longer_delim(capital, delim = "; ") %>% 
  left_join(country_capitals, by = c("capital"))


# cleaning and binding the two dfs:

nyt_country_cleaned <- nyt_articles_ner %>% 
  filter(!is.na(country)) %>% 
  filter(nyt_ner != "hamas" | country != "Lebanon") %>%  # limited relevance to lebanon here
  select(-nyt_ner) %>% 
  bind_rows(
    nyt_articles_countries 
  ) %>% 
  bind_rows(
    nyt_articles_capitals %>% select(news_index, nyt_txt, country)
  ) %>% 
  transform(country = clean_str_custom(country)) %>% 
  filter(country != "u s" & country != "" & country != "united states") %>%  # we do not look at US (i.e. domestic news) in this analysis
  distinct() %>% 
  # taking out unlikely matches
  filter(!(country %in% c('jersey', 'niue', 'iceland', "unknown", "nassau",
                          "netherlands", "marshall islands", "anguilla", "seychelles", 
                          "georgia" # refers to state of georgia
  ))) %>% 
  filter(!(!grepl("\\boman\\b", nyt_txt) & country == "oman"))

# looking at countries mentioned in relation to confli in another country
nyt_country_cleaned %>% group_by(news_index) %>% 
  filter(n_distinct(country) > 1) %>% 
  View()

# exportin nyt --> country build
nyt_country_cleaned %>% write_csv("data/mst/05_nyt_to_countries.csv")


# evaluating lexicon classifier accuracy --------------

nyt_class <- read_csv("data/mst/00a2_nyt_articles_sample_annotated.csv") %>% 
  left_join(nyt_country_cleaned %>% select(news_index) %>% 
              transform(lexicon_class = 1)) %>% 
  transform(lexicon_class = replace_na(lexicon_class, 0))

TP <- nrow(nyt_class %>% filter(poli_con_class == 1 & lexicon_class == poli_con_class))
FP <- nrow(nyt_class %>% filter(poli_con_class == 0 & lexicon_class != poli_con_class))
FN <- nrow(nyt_class %>% filter(poli_con_class == 1 & lexicon_class != poli_con_class))

lexicon_precision <- TP/(TP + FP)
lexicon_recall <- TP/(TP + FN)