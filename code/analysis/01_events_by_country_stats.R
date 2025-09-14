######################

# This script does three things:

# 1. identify relevant political conflict events in nyt art, and tag their countries
# 2. join nyt art to acled events
# 3. make visualizations of joined data

#####################

source("header.R")

# importing nyt --> country build -----------------------------
nyt_country_cleaned <- read_csv("data/mst/06_nyt_to_countries.csv")

  
# importing events data -----------------------------------------

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

# mapping nyt countries to acled countries -----------------------
  
nyt_country_grouped <- nyt_country_cleaned %>% 
    
    # some countries have multiple names. fix. 
    mutate(country = case_match(
      country,
      "bosnia" ~ "bosnia and herzegovina",
      "palestinian territories" ~ "palestine",
      "u k" ~ "united kingdom",
      "hamburg" ~ "germany",
      .default = country
    )) %>% 
    
    group_by(country) %>% 
    summarise(n_art = n()) %>% 
    full_join(acled_events_grouped %>% transform(country = clean_str_custom(country)), by = "country") %>% 
    mutate(across(c(n_art, total_fat), ~replace_na(.,0))) 

# exporting a copy
nyt_country_grouped %>% write_csv("data/analysis/01_nyt_event_country_grouped.csv")
  
# function to plot maps of coverage and fat -------------

# nyt_country_grouped <- read_csv("data/analysis/01_nyt_event_country_grouped.csv")
  
# df <- nyt_country_grouped
# stat_col <- "n_art"

# prepping data to be plotted:
# Load world map shapefile
world <- ne_countries(scale = "medium", returnclass = "sf")

# Ensure matching format: lowercase country names
world <- world %>% mutate(name_long_lower = tolower(name_long)) %>% 
  mutate(country = case_match(
    name_long_lower,
    "falkland islands / malvinas" ~ "falkland islands",
    "the gambia" ~ "gambia",
    "dem. rep. korea" ~ "north korea",
    "republic of korea" ~ "south korea",
    "russian federation" ~ "russia",
    "democratic republic of the congo" ~ "democratic republic of congo",
    "guinea-bissau" ~ "guinea bissau",
    .default= name_long_lower
  ))

# Merge user data with map
world_data <- full_join(world, nyt_country_grouped, by = c("country" = "country")) %>% 
  mutate(across(c("n_art", "total_fat"), ~ifelse(name_long_lower == "united states", NA, replace_na(., 0)))) %>% 
  transform(is_us = ifelse(name_long_lower == "united states", 1, 0))

# getting centroid coords
centroids <- st_centroid(world_data) %>%
  select(name_long_lower, n_art, total_fat) %>% 
  mutate(lon = st_coordinates(.)[,1],
         lat = st_coordinates(.)[,2],
         across(c(n_art, total_fat), ~ifelse(. == 0, NA, .)))

# checking join
# world_data %>% filter(is.na(featurecla) | is.na(n_art)) %>% select(country, featurecla, n_art) %>% View

# xwalk for plot labels
map_var_xwalk <- tibble(
  varname = c("n_art", "total_fat"),
  varlab = c("Number of Articles", "Total Fatalities")
)

  plot_world_statistic <- function(stat_col) {

    
    centroids_nona <- centroids %>% 
      filter(!is.na(get(stat_col)))
    
    # plotting symbol map --------------------
    stat_max <- max(world_data[[stat_col]], na.rm = TRUE)
    
    ggplot(data = world_data) +
      geom_sf(aes(fill = as.factor(is_us)), color = "black") +
      scale_fill_manual(values = c("white", "gray")) +
      geom_point(data = centroids_nona, 
                 aes(x = lon, 
                     y = lat, 
                     size = get(stat_col)), color = "#00008B",
                 alpha = 0.25)+ 
      scale_size_continuous(range = c(1, 25), breaks = c(20, 100, stat_max)) + 
      theme_map() +
      labs(size = glue("{pull(map_var_xwalk %>% filter(varname == stat_col), varlab)}"))
    
    # plotting cropped map -------------
    # Define bounding box for Europe + Africa + Middle East
    if (region_pick == "East") {
      bbox <- c(xmin = 65, xmax = 150, ymin = -20, ymax = 60)
    } else if (region_pick == "West") {
      bbox <- c(xmin = -25, xmax = 80, ymin = -50, ymax = 70)
    }
    
    
    # Crop map
    region <- st_crop(world_data, bbox)
    
    # Plot
    ggplot() +
      geom_sf(data = region, aes(fill = !!sym(stat_col)), color = "black") +
      scale_fill_gradient(low = "white", high = "#000038") +
      theme_minimal() +
      coord_sf(xlim = c(bbox["xmin"], bbox["xmax"]), ylim = c(bbox["ymin"], bbox["ymax"])) +
      labs(fill = glue("{pull(map_var_xwalk %>% filter(varname == stat_col), varlab)}"))
    
    
    # dual axis bars -------------------
    
 
    # getting scale factor for dual axes
    scale_factor <- max(nyt_country_grouped$total_fat)/max(nyt_country_grouped$n_art)
    
    # reshape long
    nyt_country_grouped_long <- nyt_country_grouped %>% arrange(get(stat_col)) %>% slice_max(get(stat_col), n = 30) %>%
      mutate(n_art = n_art * scale_factor, index_by_stat = row_number(),
             country = str_to_title(country)) %>% 
    pivot_longer(cols = c(n_art, total_fat), names_to = "stat", values_to = "n") %>% 
      transform(stat = as.factor(stat)) %>% 
      transform(stat = factor(stat, levels = c("total_fat", "n_art")))
    
    
    ggplot(nyt_country_grouped_long) + 
      geom_bar(stat = "identity", aes(x = reorder(country, index_by_stat), y = n, fill = stat), position = "dodge") + 
      scale_y_continuous(name = "Number of Fatalities",
                         sec.axis = sec_axis(~ . / scale_factor, name = "Number of Articles")) + 
      scale_fill_manual(values = c("#953552", "#000038"), labels = c("Total Fatalities", "Total Article Count")) + 
      labs(x = "Country") +
      theme_minimal() + 
      theme(legend.position = "bottom",
            legend.title = element_blank(),
            axis.text.x = element_text(angle = 75, vjust = 1, hjust = 1))
      
    
    
  }
  
# applying the function to fat:

plot_world_statistic( "n_art")


