######################

# This script does 

#####################

source("header.R")

# reading in events data ---------------------
acled_events <- read_csv("data/mst/00b_acled_events_filter.csv") %>% 
  select(-1)

# importing cosine similarities ----------------

nyt_events_match <- read_csv("data/mst/01_event_news_similarity_build.csv") %>% 
  filter(cosine_sim > 0.5 & date_diff <= 1)

# importing nyt articles --> country build ---------------

nyt_country_cleaned <- read_csv("data/mst/06_nyt_to_countries.csv")

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

# how many of the articles identified are actually news articles per ml classifier? 
(nyt_events_check$news_index %>% unique %>% length())/(nyt_events_match$news_index %>% unique %>% length())


# creating a build at the event level
nyt_events_match_grouped <- nyt_events_check %>% 
  select(event_index, news_index, cosine_sim) %>% distinct() %>% # select(notes, nyt_title) %>% 
  full_join(acled_events) %>% 
  group_by(event_index, event_date, actor1, actor2, event_type, interaction, region, country, fatalities, notes) %>% 
  summarise(
    sum_cosine = replace_na(sum(cosine_sim, na.rm = T), 0),
    n_art = n_distinct(news_index, na.rm = T)
  ) %>% 
  # categorizing geographic region of event
  transform(region_for_plt = ifelse(country %in% c("Palestine", "Israel"), "Palestine/Israel",
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
                                        .default = region_for_plt))



# visualizing coverage vs. fat by event ---------------
n_regions <- nyt_events_match_grouped$region_for_plt %>% unique() %>% length

# defining colors
pal <- c(brewer.pal(n = n_regions, name = "Set1"), 
         "gray")
names(pal) <- c(nyt_events_match_grouped$region_for_plt %>% unique(), "Other")

alpha_pal <- c(rep(1, n_regions), 0.25)


# a function to color one region and label everything else as "other" 

plot_events <- function (color_col, cat_pick) {
  
  for_plt <- nyt_events_match_grouped %>% 
    transform(for_plt_color = ifelse(get(color_col) == cat_pick, cat_pick, "Other"))
  
  ggplot(for_plt, aes(x = fatalities, y = n_art)) + 
    geom_point(aes(color = for_plt_color), size = 3, alpha = 0.5) + 
    labs(
      y = "Number of Relevant Articles",
      x = "Number of Fatalities"
    ) + 
    scale_color_manual(values = pal) + 
    theme_minimal() + 
    theme(legend.position = "bottom",
          legend.title = element_blank())
  
}


  






