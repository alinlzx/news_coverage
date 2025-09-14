######################

# This script picks out NER (entities, countries, capitals) from 
# nyt article texts.

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

# creating a master xwalk for NER (country, capital, other entities) to country

master_xwalk <- bind_rows(
  country_capitals %>% select(country, ner = capital) %>% transform(type = "capital"),
  country_ner_xwalk %>% transform(type = "other_ner"),
  tibble(country = countries, ner = countries, type = "country_name") 
) %>% 
  # recoding inconsistent names
  mutate(
    country = ifelse(grepl("brazzaville", ner), "republic of congo", country),
    across(c(country, ner), ~ifelse(. == "congo brazzaville", "republic of congo", .)),
    ner = ifelse(ner == "palestinian territories", "palestinian", ner),
    country = case_match(
      country,
      "dr congo" ~ "democratic republic of congo",
      "congo" ~ "democratic republic of congo",
      "congo kinshasa" ~ "democratic republic of congo",
      "korea" ~ "south korea",
      "palestine state of" ~ "palestine",
      "bosnia" ~ "bosnia and herzegovina",
      "bosnia herzegovina" ~ "bosnia and herzegovina",
      "cabo verde" ~ "cape verde",
      "united kingdom" ~ "u k",
      "hong kong sar china" ~ "hong kong",
      "macao sar china" ~ "macau",
      "macao" ~ "macau",
      "palestinian territories" ~ "palestine",
      .default = country
    ),
    country = clean_str_custom(country)
  ) %>% 
  arrange(country) %>% distinct()

# exporting a copy of the master xwalk
master_xwalk %>% write_csv("data/mst/04_master_xwalk_nyt_ner.csv")