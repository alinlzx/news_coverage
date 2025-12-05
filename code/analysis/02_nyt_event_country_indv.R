######################

# This script applies filters to the cosine similarity event-article level build
# and then outputs a few time series/point plots of coverage vs. casualty

#####################

source("header.R")

# reading in events data ---------------------
acled_events <- read_csv("data/mst/00b_acled_events_filter.csv") %>% 
  select(-1)

# state to region xwalk
country_region_xwalk <- read_csv("data/mst/06a_country_to_region_xwalk.csv")


# importing cosine similarities ----------------

nyt_events_match <- read_csv("data/mst/01_event_news_similarity_build.csv") %>% 
  filter(cosine_sim > 0.5 & date_diff <= 1)

nyt_events_match_filt <- nyt_events_match %>% 
  filter(grepl("\\/us\\/|\\/world\\/|\\/briefing\\/|\\/opinion\\/", nyt_url))

# importing nyt articles --> country build ---------------

nyt_country_cleaned <- read_csv("data/mst/06_nyt_to_countries.csv")

nyt_country_cleaned_date <- nyt_country_cleaned %>% 
  left_join(read_csv("data/mst/00_nyt_articles.csv") %>% select(nyt_date, news_index))

# joining the two builds --------------------

nyt_events_check <- nyt_events_match_filt %>% 
  transform(country = tolower(country)) %>% 
  # many to many join
  left_join(nyt_country_cleaned %>% select(news_index, nyt_country=country), by = c("news_index")) %>% 
  
  # filter out articles not mapped to a country
  filter(!is.na(nyt_country)) %>% 
  
  # if none of the articles are equivalent to the acled event country field, then probably not relevant
  group_by(event_index, news_index) %>% 
  mutate(max_country_equivalence = max(country == nyt_country)) %>% 
  filter(max_country_equivalence == 1) %>% 
  transform(country = clean_str_custom(country)) %>% 
  # categorizing geographic region of event
  left_join(country_region_xwalk, by = c("country")) %>% 
  code_region

# how many of the articles identified are actually news articles per ml classifier? 
(nyt_events_check$news_index %>% unique %>% length())/(nyt_events_match$news_index %>% unique %>% length())



# creating a build at the event level
nyt_events_match_grouped <- nyt_events_check %>% 
  select(event_index, news_index, cosine_sim) %>% distinct() %>% # select(notes, nyt_title) %>% 
  full_join(acled_events) %>% 
  transform(country = clean_str_custom(country)) %>% 
  # categorizing geographic region of event
  left_join(country_region_xwalk, by = c("country")) %>% 
  code_region %>% 
  group_by(event_index, event_date, actor1, actor2, event_type, interaction, region_un, subregion, country, fatalities, notes) %>% 
  summarise(
    sum_cosine = replace_na(sum(cosine_sim, na.rm = T), 0),
    n_art = n_distinct(news_index, na.rm = T)
  ) 



# visualizing coverage vs. fat by event ---------------

all_regions <- c(nyt_events_match_grouped$region_un, country_region_xwalk$region_un) %>% unique %>% sort()
n_regions <- all_regions %>% unique() %>% length

# defining colors
pal <- c(brewer.pal(n = n_regions, name = "Set1"), 
         "gray")
names(pal) <- c(all_regions %>% unique(), "Other")
# alpha_pal <- c(rep(1, n_regions), 0.25)

# getting scale factor for dual axes
scale_factor <- max(nyt_events_match_grouped$fatalities)/max(nyt_events_match_grouped$n_art)/2

# a function to color one region over time and label everything else as "other" 
# region_pick <- "Ukraine/Russia"
plot_events <- function (region_pick, just_arts = F) {
  
  # get number of distinct articles over time
  ts_event_news <- nyt_events_check %>% 
    filter(subregion == region_pick) %>% 
    group_by(event_date) %>% 
    summarise(n_art = n_distinct(news_index))
  
  # get number of distinct articles at country level: 
  ts_country <- nyt_country_cleaned_date %>% 
    left_join(country_region_xwalk, by = c("country")) %>% 
    code_region %>% 
    filter(subregion == region_pick) %>% 
    group_by(event_date = nyt_date) %>% 
    summarise(n_art_country = n_distinct(news_index)) 
  
  ts <- nyt_events_match_grouped %>% 
    filter(subregion == region_pick) %>% 
    group_by(event_date) %>% 
    summarise(n_fat = sum(fatalities)) %>% 
    left_join(ts_event_news, by = "event_date") %>%  
    left_join(ts_country, by = "event_date") %>% 
    transform(n_art_country = replace_na(n_art_country, 0) * scale_factor,
              n_art = replace_na(n_art, 0) * scale_factor) %>% 
    # rename(n_fat = fatalities) %>% 
    # transform(n_art = n_art * scale_factor) %>% 
    transform(# rolling 3 day window mean
              n_fat_smooth = rollmean(n_fat, 3, align = "right", na.pad = T),
              n_art_smooth = rollmean(n_art, 3, align = "right", na.pad = T),
              n_art_country_smooth = rollmean(n_art_country, 3, align = "right", na.pad = T)) 
    # transform(region_label = ifelse(region_un == region_pick, region_pick, "Other"))

  
  # ggplot(for_plt, aes(x = fatalities, y = n_art)) + 
  #   geom_point(aes(color = for_plt_color), size = 3, alpha = 0.5) + 
  #   labs(
  #     y = "Number of Relevant Articles",
  #     x = "Number of Fatalities"
  #   ) + 
  #   scale_color_manual(values = pal) + 
  #   theme_minimal() + 
  #   theme(legend.position = "bottom",
  #         legend.title = element_blank())
  
  # setting color pal and linetypes
  ts_pal <- c("Total Article Count (Country-Level)" = "#000038", 
              "Total Fatalities" = "#953551", 
              "Total Article Count (Event-Level)" = "#000038")
  
  ts_lines <- c("Total Article Count (Country-Level)" = 1, 
              "Total Fatalities" = 1, 
              "Total Article Count (Event-Level)" = 81)
    
    ts_long <- ts %>%
      pivot_longer(cols = c(n_art_smooth, n_fat_smooth, n_art_country_smooth), names_to = "stat", values_to = "n") %>% 
      filter(!is.na(n)) %>% 
      transform(stat = ifelse(stat == "n_art_smooth", "Total Article Count (Event-Level)",
                              ifelse(stat == "n_art_country_smooth", "Total Article Count (Country-Level)", "Total Fatalities")))  # %>% 
      # filter(stat == "Total Article Count (Country-Level)")
    
    text_size <- 10
    
    if (just_arts == T) {
      
      ts_long <- ts_long %>% 
        transform(n_art = n_art / scale_factor,
                  n_art_country = n_art_country/scale_factor) %>% 
        filter(grepl("Article Count", stat))
      
      # plotting dual axes time series
      ggplot(ts_long, aes(x = event_date, y = n)) + 
        geom_line(aes(color = stat, linetype = stat), linewidth = 0.5, alpha = 0.5) +
        geom_smooth(aes(color = stat, linetype = stat)) + 
        scale_y_continuous(name = "Number of Articles") + 
        scale_color_manual(values = ts_pal) + 
        scale_linetype_manual(values = ts_lines) + 
        theme_linedraw() + 
        theme(legend.position = "bottom",
              legend.title = element_blank(), 
              title = element_text(size = text_size),
              legend.text = element_text(size = text_size),
              axis.title.y = element_text(size = text_size),
              axis.text.y = element_text(size = text_size),
              axis.title.x = element_blank(),
              axis.text.x = element_text(size = text_size)) + 
        labs(title = region_pick) + 
        guides(colour = guide_legend("stat"), linetype = guide_legend("stat")) 
      
    }
    
    else {
  
    # plotting dual axes time series
    ggplot(ts_long, aes(x = event_date, y = n)) + 
      geom_line(aes(color = stat, linetype = stat), linewidth = 1) +
      scale_y_continuous(name = "Number of Fatalities",
                         sec.axis = sec_axis(~ . / scale_factor, name = "Number of Articles")) + 
      scale_color_manual(values = ts_pal) + 
      scale_linetype_manual(values = ts_lines) + 
      theme_linedraw() + 
      theme(legend.position = "none",
            legend.title = element_blank(), 
            title = element_text(size = text_size),
            legend.text = element_text(size = text_size),
            axis.title.y = element_text(size = text_size),
            axis.text.y = element_text(size = text_size),
            axis.title.x = element_blank(),
            axis.text.x = element_text(size = text_size)) + 
      labs(title = region_pick) + 
      guides(colour = guide_legend("stat"), linetype = guide_legend("stat"))
      
    }
  
}

ts_list <- map(c("Africa", "Ukraine/Russia", "Palestine/Israel", "Greater ME", "Myanmar"), 
                      ~plot_events(.))

grid.arrange(grobs = ts_list[c(1:6)], nrow = 3)

plot_events("Palestine/Israel", T)

# plotting article against fatality, by region/country: 
plot_art_vs_fat_point_event <- function (region_pick) {
  
  for_point_plt <- nyt_events_match_grouped %>% 
    # left_join(world_data %>% select(country, region_un)) %>% 
    mutate(# across(c(n_art, total_fat), ~ifelse(. == 0, . + 0.9, .)), # for log scale
           # region_un = ifelse(country %in% c("palestine", "israel"), "Palestine/Israel",
                              # ifelse(country %in% c("ukraine", "russia"), "Ukraine/Russia", region_un)),
           region_label = ifelse(region_un == region_pick, region_un, "Other")) %>% 
    as_tibble %>% 
    filter(!is.na(region_label)) %>% 
    group_by(country, region_un, region_label) %>% 
    mutate(
      total_fat = sum(fatalities),
    )
  
  ggplot(data = for_point_plt,
         aes(x = fatalities, y = n_art)) + 
    geom_point(aes(color = region_label), size = 3, alpha = 0.35) + 
    scale_color_manual(values = pal) + 
    theme_bw() + 
    # scale_x_log10() + 
    # scale_y_log10() + 
    labs(x = "Event Fatalities",
         y = "Article Count",
         colour = "Region",
         title = region_pick) +
    theme(legend.position = "none",
          axis.title.x = element_text(size = 12),
          axis.title.y = element_text(size = 12),
          title = element_text(size = 12)) 
  
}

event_point_plt_list <- map(c("Africa", "Ukraine/Russia", "Palestine/Israel", "Asia"), 
               ~plot_art_vs_fat_point_event(.))

grid.arrange(grobs = event_point_plt_list[c(1:4)], nrow = 2)

  






