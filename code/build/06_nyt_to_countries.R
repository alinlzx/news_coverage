######################

# This script maps nyt articles to countries. 

#####################

source("header.R")

# reading in master xwalk of country to entities ---------
master_xwalk <- read_csv("data/mst/04_master_xwalk_nyt_ner.csv")

# creating regex patterns:

# concat country into regex pattern
country_extract <- master_xwalk %>% 
  filter(type == "country_name") %>% pull(ner) %>% 
  paste(collapse = "|") 

# concat country capitals into regex pattern
country_ner_extract <- master_xwalk %>% 
  filter(type %in% c("other_ner", "capital")) %>% 
  pull(ner) %>% 
  paste(collapse = "\\b|\\b")

# master_extract
master_extract <- paste(country_extract, country_ner_extract, sep = "\\b")


# read in nyt data --------------------------------

nyt_articles <- read_csv("data/mst/00_nyt_articles.csv") %>% select(-1) %>% 
  # what if we only cared about US, World, Opinion, Briefing sections
  filter(grepl("\\/us\\/|\\/world\\/|\\/briefing\\/|\\/opinion\\/", nyt_url))

# flattening nyt article abstract and titles
nyt_articles_cleaned <- nyt_articles %>% 
  transform(
    nyt_txt = paste(nyt_title, nyt_abstract, sep = ". ") %>% clean_str_custom()
  ) 


# a helper that can map text to country names, capitals, and other named entities
# takes a few sec to run ------------------------------------

# takes in a set of articles of poli class 1, spits out articles + corresponding country/countries
map_art_to_c <- function (arts) {
  
  a_to_c <- arts %>% 
    cbind(
      nyt_ner = map(.x = pull(., nyt_txt),
                    .f = ~str_extract_all(., master_extract, simplify = T) %>% 
                      str_trim() %>% stri_remove_empty_na %>% unique %>% 
                      paste(collapse = "; ")) %>%
        unlist()
    ) %>% 
    separate_longer_delim(nyt_ner, delim = "; ") %>% 
    # next use entities to identify countries
    left_join(master_xwalk, by = c("nyt_ner" = "ner"))
  
}


# another helper to clean up country names -----------------

clean_up_map_countries <- function (arts_and_country) {
  arts_and_country %>% 
    filter(!is.na(country)) %>% 
    filter(nyt_ner != "hamas" | country != "Lebanon") %>%  # limited relevance to lebanon here
    filter(country != "u s" & country != "" & country != "united states") %>%  # we do not look at US (i.e. domestic news) in this analysis
    select(-nyt_ner,-type) %>% 
    distinct() %>% 
    # taking out unlikely matches
    filter(!(country %in% c('jersey', 'niue', 'iceland', "unknown", "nassau", "brunswick",
                            "netherlands", "marshall islands", "anguilla", "seychelles", 
                            "georgia" # refers to state of georgia
    ))) %>% 
    filter(!(!grepl("\\boman\\b", nyt_txt) & country == "oman"))
}


# -------------------
# building lexicon classifier
# -------------------

# poli/confli lexicon regex
poli_con_pattern <- poli_conflict_lexicon_short %>% paste(collapse = "|")

nyt_lexicon_class <- nyt_articles_cleaned %>% 
  select(nyt_txt, news_index) %>% 
  filter(grepl(poli_con_pattern, nyt_txt)) %>% 
  map_art_to_c() # many-to-many

nyt_lexicon_class_clean <- nyt_lexicon_class %>% 
  clean_up_map_countries

# exporting a copy of nyt ner 
# nyt_articles_ner %>% write_csv("data/mst/03_news_articles_ner.csv")

# looking at countries mentioned in relation to confli in another country
# nyt_country_cleaned %>% group_by(news_index) %>% 
  # filter(n_distinct(country) > 1) %>% 
  # View()



# -------------------
# adding countries to ml classifier
# -------------------

nyt_ml_class_raw <- read_csv("data/mst/05_nyt_ml_predict_classes.csv")
nyt_ml_class <- nyt_ml_class_raw %>% 
  filter(ml_predict_classes == 1 & grepl("\\/us\\/|\\/world\\/|\\/briefing\\/|\\/opinion\\/", nyt_url)) %>% 
  select(news_index) %>% 
  left_join(nyt_articles_cleaned %>% select(news_index, nyt_txt)) %>% 
  map_art_to_c()

nyt_ml_class_clean <- nyt_ml_class %>% 
  clean_up_map_countries

# exportin nyt --> country build
nyt_ml_class_clean %>% write_csv("data/mst/06_nyt_to_countries.csv")

#  -------------------------
# evaluating classifier performance 
#  -------------------------

nyt_train_class <- read_csv("data/mst/00a2_nyt_articles_sample_annotated.csv")

# function to evaluate classifiers:
eval_classifier <- function (build) {
  
  nyt_class <- nyt_train_class %>% 
    left_join(build %>% select(news_index) %>% 
                transform(predict_class = 1)) %>% 
    transform(predict_class = replace_na(predict_class, 0)) %>% 
    select(news_index, contains("class")) %>% 
    distinct()
  
  TP <- nrow(nyt_class %>% filter(poli_con_class == 1 & predict_class == poli_con_class))
  FP <- nrow(nyt_class %>% filter(poli_con_class == 0 & predict_class != poli_con_class))
  FN <- nrow(nyt_class %>% filter(poli_con_class == 1 & predict_class != poli_con_class))
  
  accuracy <- nrow(nyt_class %>% filter(poli_con_class == predict_class))/nrow(nyt_class)
  precision <- TP/(TP + FP)
  recall <- TP/(TP + FN)
  
  return(c(accuracy, precision, recall))
}

# evaluating ML classifier

nyt_ml_class_clean %>% eval_classifier

# evaluating lexicon classifier accuracy 

nyt_lexicon_class_clean %>% eval_classifier


nyt_class %>% left_join(nyt_articles) %>% 
  filter(poli_con_class != predict_class)






