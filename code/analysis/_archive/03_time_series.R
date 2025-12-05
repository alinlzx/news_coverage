######################

# This script creates time series of conflicts and coverage over time

#####################

source("header.R")

# importing events data -----------------------------------------

acled_events <- read_csv("data/mst/00b_acled_events_filter.csv") %>% 
  select(-1)



# importing all nyt coverage classified as poli con -----------

nyt_country_cleaned <- read_csv("data/mst/06_nyt_to_countries.csv")
nyt_articles <- read_csv("data/mst/00_nyt_articles.csv")

# creating time series of event / coverage over time, by region ---------------------

country_region_xwalk <- acled_events %>% select(country, region) %>% 
  distinct() %>% 
  # categorizing geographic region of event
  transform(country_clean = clean_str_custom(country), 
            region_for_plt = ifelse(country %in% c("Palestine", "Israel"), "Palestine/Israel",
                                    ifelse(country %in% c("Ukraine", "Russia"), "Ukraine/Russia", region))) %>% 
  transform(is_palestine = (region_for_plt == "Palestine/Israel"),
            is_ukraine = (region_for_plt == "Ukraine/Russia"),
            region_for_plt = ifelse(region_for_plt %in% c("Europe", "Middle East"), paste("Other", region_for_plt), region_for_plt)) %>% 
  transform(region_for_plt = case_match(region_for_plt,
                                        "Eastern Africa" ~ "Sub-Saharan Africa",
                                        "Southeast Africa" ~ "Sub-Saharan Africa",
                                        "Middle Africa" ~ "Sub-Saharan Africa",
                                        "Western Africa" ~ "Sub-Saharan Africa",
                                        "Southern Africa" ~ "Sub-Saharan Africa",
                                        "Caribbean" ~ "Americas",
                                        "South America" ~ "Americas",
                                        "North America" ~ "Americas",
                                        "Other Middle East" ~ "Other Middle East & N. Africa",
                                        "Northern Africa" ~ "Other Middle East & N. Africa",
                                        "South Asia" ~ "South and Southeast Asia",
                                        "Southeast Asia" ~ "South and Southeast Asia",
                                        .default = region_for_plt)) %>% 
  select(country, region_for_plt, country_clean) 

nyt_coverage_over_time <- nyt_country_cleaned %>% 
  left_join(nyt_articles %>% select(news_index, nyt_date), by = "news_index") %>% 
  
  # joining on country --> region from ACLED. For countries not in ACLED, this field will be missing. 
  # that's ok because better to compare coverage and events for same set of countries. 
  left_join(country_region_xwalk, by = c("country" = "country_clean")) %>% 
  select(-country) %>% 
  rename(country = country.y) %>% 
  filter(!is.na(country))

acled_region <- acled_events %>% left_join(country_region_xwalk %>% select(country, region_for_plt))


# a function that takes in region of interest as input and outputs time series 

make_ts <- function (region) {
  
  for_ts <- country_region_xwalk %>% 
    filter(region_for_plt == region) %>% 
    group_by(nyt_date) %>% 
    summarise(n_art = n()) %>% 
    full_join(acled_region %>% filter(region_for_plt == region) %>% 
                group_by(event_date) %>% summarise(n_fat = sum(fatalities), n_event = n()),
              by = c("nyt_date" = "event_date")) %>% 
    mutate(across(contains("n_"), ~replace_na(., 0)))
  
  
  # getting scale factor for dual axes
  scale_factor <- max(for_ts$n_fat)/max(for_ts$n_art)
  
  for_ts_long <- for_ts %>% 
    pivot_longer(cols = c(n_art, n_fat), names_to = "stat", values_to = "n") 
  
  
  # plotting dual axes time series
  
  
    
  
  
}