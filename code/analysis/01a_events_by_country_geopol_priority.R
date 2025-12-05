######################

# This script:

# 1. Takes 01 analysis and adds geopolitical contingencies from CFR and
# adds it as another dimension of data viz

#####################

# coding up conflict contingencies ------------------

geopol_priorities <- tibble(
  country = c(# tier i
    "china", "taiwan", "philippines", "iran", "russia", "ukraine",
              "lebanon", "palestine", "israel", "haiti", "mexico",
              
    # tier ii
    "north korea", "india", "pakistan", "turkey", "iraq", "syria",
    "yemen", "somalia", "sudan", 
    
    # tier iii
    "myanmar", "bangladesh", "ethiopia", "kosovo", "serbia", "libya", 
    "burkina faso", "mali", "niger", "nigeria", "democratic republic of congo",
    "mozambique"),
  geo_contingency = c(rep("Tier 1 Contingency", 11),
               rep("Tier 2 Contingency", 9),
               rep("Tier 3 Contingency", 12))
)

# importing grouped country data ---------

nyt_country_grouped <- read_csv("data/analysis/01_nyt_event_country_grouped.csv")

# joining on conflict contingencies
nyt_country_grouped_geopol <- nyt_country_grouped %>% 
  left_join(geopol_priorities, by = "country") %>% 
  transform(geo_contingency = replace_na(geo_contingency, "none"))

# plotting scatter of art vs fat, colored by contingency ------

# tier_pick <- "Tier 3 Contingency"
geo_pal <- c("Tier 1 Contingency" = "#8c0000", 
             "Tier 2 Contingency" = "#ff9800",
             "Tier 3 Contingency" = "#ffc100",
             "none" = "lightgray")

make_geopol_points <- function (tier_pick) {
  
  for_geopol_point_plt <- nyt_country_grouped_geopol %>% 
    mutate(
      across(c(n_art, total_fat), ~ifelse(. == 0, . + 0.9, .)),
      tier_label = ifelse(geo_contingency == tier_pick, geo_contingency, "other")
    )
  
  geo_clabs <- for_geopol_point_plt %>% filter(grepl("Tier ", geo_contingency) & geo_contingency == tier_label) %>% 
    filter(n_art > 100 | total_fat > 1000)
  
  ggplot(data = for_geopol_point_plt,
         aes(x = total_fat, y = n_art)) + 
    geom_point(aes(color = tier_label), size = 3, alpha = 0.75) + 
    scale_color_manual(values = geo_pal) + 
    theme_bw() + 
    scale_x_log10() + 
    scale_y_log10() + 
    labs(x = "Total Fatalities",
         y = "Article Count",
         colour = "Contingency",
         title = tier_pick) +
    theme(legend.position = "none",
          axis.title.x = element_text(size = 12),
          axis.title.y = element_text(size = 12),
          title = element_text(size = 12))  + 
    geom_label_repel(data = geo_clabs, aes(y = n_art, x = total_fat, label = country),
                     size = 3.5,
                     box.padding = unit(0.5, "lines"),
                     point.padding = unit(0.5, "lines"),
                     segment.colour = "grey50")
  
  
}

make_geopol_points("Tier 3 Contingency")

geopol_point_plts <- map(geopol_priorities %>% pull(geo_contingency) %>% unique,
                         ~make_geopol_points(.))

grid.arrange(grobs = geopol_point_plts, nrow = 2)
