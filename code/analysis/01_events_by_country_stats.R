######################

# This script does three things:

# 1. identify relevant political conflict events in nyt art, and tag their countries
# 2. join nyt art to acled events
# 3. make visualizations of joined data, including maps, bars, & points

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
  transform(is_us = ifelse(name_long_lower == "united states", 1, 0)) %>% 
  filter(sovereignt != "Antarctica")

# -----------------------------------------------------

# exporting a copy of map data
world_data %>% select(country, iso_a2_eh, region_un, subregion,
                      n_art, total_fat, is_us) %>% 
  filter(!iso_a2_eh %in% c("VA", "MP", "VI", "GU", "AS", "PR", "GS", "IO", "SH", "PN",
                           "AI", "FK", "KY", "BM", "VC", "TC", "MS", "JE", "GG", "IM", 
                           "-99", "NU", "CK", "AW", "CW", "PM", "WF", "MF", "BL", "PF", 
                           "NC", "TF", "AX", "FO", "HM", "NF", "SX")) %>% 
  as_tibble() %>% select(-geometry) %>% 
  write_csv("data/analysis/01a_nyt_event_country_grouped_with_geo.csv")

# exporting a copy as country - region xwalk
world_data %>% select(country, region_un, subregion) %>% as_tibble() %>% 
  write_csv("data/mst/06a_country_to_region_xwalk.csv")

# ------------------------------------------------------

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

  plot_world_statistic <- function(stat_col, region_pick = "all") {

    heat_color = ifelse(stat_col == "n_art", "#000038", "maroon")
    
    centroids_nona <- centroids %>% 
      filter(!is.na(get(stat_col)))
    
    # plotting symbol map --------------------
    stat_max <- max(world_data[[stat_col]], na.rm = TRUE)
    
    interval = ifelse(stat_max > 2000, 1000, 100)
    stat_max_round <- ceiling(stat_max/interval)*interval
    
    if (region_pick == "all") {
      
      # plotting the entire world
      
      map <- ggplot(data = world_data) +
        geom_sf(aes(fill = as.factor(is_us)), color = "gray") +
        scale_fill_manual(values = c("white", "gray"), guide = "none") +
        geom_point(data = centroids_nona, 
                   aes(x = lon, 
                       y = lat, 
                       size = get(stat_col)), color = heat_color,
                   alpha = 0.5)+ 
        scale_size_continuous(range = c(1, 25), breaks = c(20, stat_max_round/5, stat_max_round/2, floor(stat_max/interval)*interval)) + 
        theme_map() +
        # theme(legend.position = "bottom") +
        labs(size = glue("{pull(map_var_xwalk %>% filter(varname == stat_col), varlab)}"))
      
      return(map)
    }
    
    else {
      
      # plotting cropped map -------------
      # Define bounding box for Europe + Africa + Middle East
      if (region_pick == "East") {
        bbox <- c(xmin = 55, xmax = 150, ymin = -8, ymax = 60)
        world_data <- world_data %>% filter(sovereignt != "Russia")
      } else if (region_pick == "West") {
        bbox <- c(xmin = -20, xmax = 60, ymin = -10, ymax = 70)
        
      }
      
      
      # Crop map
      region <- st_crop(world_data, bbox)
      region_centroids <- st_crop(centroids_nona, bbox)
      
      # Plot
      ggplot() +
        geom_sf(data = region, aes(fill = !!sym(stat_col)), color = "black") +
        scale_fill_gradient(low = "white", high = heat_color) +
        # geom_point(data = region_centroids,
        #            aes(x = lon,
        #                y = lat,
        #                size = get(stat_col)), color = heat_color,
        #            alpha = 0.5) +
        scale_size_continuous(guide = "none", range = c(1, 50)) +
        theme_minimal() +
        theme(legend.text = element_text(size = 15),
              legend.title = element_text(size = 20)) + 
        coord_sf(xlim = c(bbox["xmin"], bbox["xmax"]), ylim = c(bbox["ymin"], bbox["ymax"])) +
        labs(fill = glue("{pull(map_var_xwalk %>% filter(varname == stat_col), varlab)}")) + 
          theme(legend.position = "bottom",
                axis.title.x = element_blank(),
                axis.title.y = element_blank()) + 
        guides(fill = guide_colorbar(barwidth = 10, barheight = 1.5))
      
      
      
      
    }
   
    
    
  }
  
# applying the function to fat:
# and saving exhibits
  
world_fat <- plot_world_statistic( "total_fat")
world_art <- plot_world_statistic( "n_art")
ggsave("exhibits/01_events_by_country_stats/ex1_world_fat.png", world_fat,
       width = 15, height = 7, units = "in")
ggsave("exhibits/01_events_by_country_stats/ex2_world_art.png", world_art,
       width = 15, height = 7, units = "in")

east_art <- plot_world_statistic( "n_art", "East")
east_fat <- plot_world_statistic( "total_fat", "East")
east_compare <- grid.arrange(east_art, east_fat, ncol = 1)
ggsave("exhibits/01_events_by_country_stats/ex3_east_compare.png", east_compare,
       width = 8, height = 15, units = "in")

west_art <- plot_world_statistic( "n_art", "West")
west_fat <- plot_world_statistic( "total_fat", "West")
west_compare <- grid.arrange(west_art, west_fat, ncol = 2)
ggsave("exhibits/01_events_by_country_stats/ex3_west_compare.png", west_compare,
       width =15, height = 8, units = "in")

# dual axis bars -------------------

stat_col <- "total_fat"

# getting scale factor for dual axes
scale_factor <- max(nyt_country_grouped$total_fat)/max(nyt_country_grouped$n_art)

# reshape long
nyt_country_grouped_long <- nyt_country_grouped %>% arrange(get(stat_col)) %>% slice_max(get(stat_col), n = 15) %>%
  mutate(n_art = n_art * scale_factor, index_by_stat = row_number(),
         country = str_to_title(country)) %>% 
  pivot_longer(cols = c(n_art, total_fat), names_to = "stat", values_to = "n") %>% 
  transform(stat = as.factor(stat)) %>% 
  transform(stat = factor(stat, levels = c("total_fat", "n_art")),
            country = ifelse(country == "Democratic Republic Of Congo", "DRC", country))


dual_bar <- ggplot(nyt_country_grouped_long) + 
  geom_bar(stat = "identity", aes(x = reorder(country, index_by_stat), y = n, fill = stat), position = "dodge") + 
  scale_y_continuous(name = "Number of Fatalities",
                     sec.axis = sec_axis(~ . / scale_factor, name = "Number of Articles")) + 
  scale_fill_manual(values = c("#953552", "#000038"), labels = c("Total Fatalities", "Total Article Count")) + 
  # labs(x = "Country") +
  theme_minimal() + 
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        legend.text = element_text(size = 15),
        axis.title.y = element_text(size = 15),
        axis.text.y = element_text(size = 15),
        axis.title.x = element_blank(),
        axis.text.x = element_text(angle = 75, vjust = 1, hjust = 1, size = 20))

ggsave(glue("exhibits/01_events_by_country_stats/ex4_dual_bar_order_by_{stat_col}.png"), dual_bar,
       width =8, height = 6, units = "in")

# point plot of casualty on coverage --------------------

# highlight one region, and then make all other regions gray: 
# region_pick <- "Palestine"

plt_art_vs_fat_point <- function (region_pick) {
  
  for_point_plt <- nyt_country_grouped %>% 
    left_join(world_data %>% select(country, region_un, subregion)) %>% 
    code_region %>% 
    mutate(across(c(n_art, total_fat), ~ifelse(. == 0, . + 0.9, .)), # for log scale
           region_label = ifelse(region_un == region_pick, region_un, "Other")) %>% 
    as_tibble %>% 
    filter(!is.na(region_label))
  
  clabs <- for_point_plt %>% 
    filter(total_fat > 1000) %>% 
    filter(region_un == region_pick)
  
  pal <- c(brewer.pal(n = length(for_point_plt %>% pull(region_un) %>% unique), name = "Set1"), 
           "gray")
  names(pal) <- c(for_point_plt$region_un %>% unique() %>% sort, "Other")
  
  point_plt <- ggplot(data = for_point_plt,
         aes(x = total_fat, y = n_art)) + 
    geom_point(aes(color = region_label), size = 3) + 
    scale_color_manual(values = pal) + 
    theme_bw() + 
    scale_x_log10() + 
    # scale_y_log10() + 
    labs(x = "Total Fatalities",
         y = "Article Count",
         colour = "Region",
         title = region_pick) +
    theme(legend.position = "none",
          axis.title.x = element_text(size = 12),
          axis.title.y = element_text(size = 12),
          title = element_text(size = 12))  + 
    geom_label_repel(data = clabs, aes(y = n_art, x = total_fat, label = country),
                     size = 3,
                     box.padding = unit(0.5, "lines"),
                     point.padding = unit(0.5, "lines"),
                     segment.colour = "grey50")
  
  file_region_name <- str_remove_all(region_pick, '\\/')
  ggsave(glue("exhibits/01_events_by_country_stats/ex5_point_plt_{file_region_name}.png"), point_plt,
         width =6, height = 4, units = "in")
  
  
}

point_plt_list <- map(c("Africa", "Ukraine/Russia", "Palestine/Israel", "Asia"), 
    ~plt_art_vs_fat_point(.))

grid.arrange(grobs = point_plt_list, nrow = 2)



