######################

# This script investigates distribution of actors 
# in conflicts of given country

#####################

source("header.R")

# importin --------------------------
acled_events <- read_csv("data/mst/00b_acled_events_filter.csv") %>% 
  select(-1)

# groupin --------------------------
all_actors <- rbind(
  acled_events %>% select(actor = actor1, country, fatalities),
  acled_events %>% select(actor = actor2, country, fatalities)
) %>% 
  # recoding some categories to be more general:
  transform(
    actor = ifelse(grepl("Military Forces of", actor), str_extract(actor, "^[^()]*"), actor) %>% 
      str_squish()
  ) %>% 
  group_by(country, actor) %>% 
  summarise(fat_responsible = sum(fatalities),
            conflicts_involved = n()) %>% 
  # civilians are not militant actors
  filter(!grepl("Civilian", actor))


# graphin -----------------------

country_pick <- "Ukraine"

responsible_actors_bars <- function (country_pick, actor_dist) {
  
  for_bar_plt <- all_actors %>% filter(country == country_pick) %>% 
    arrange(desc(conflicts_involved)) %>%
    transform(actor =  str_extract(actor, "^[^:]*") %>% str_squish()) %>% 
    transform(
      label = if_else(fat_responsible > 300, actor, "")  # only label top 3
    ) 
  
  for_bar_labs <- for_bar_plt %>% 
    filter(label != "") 
  
  # ggplot(for_bar_plt, aes(x = "", y = fat_responsible, fill = actor)) +
  #   geom_col(width = 1, color = "white") +
  #   coord_polar(theta = "y") +
  #   geom_text(
  #     aes(label = label),
  #     position = position_stack(vjust = 0.5),
  #     size = 5
  #   ) +
  #   theme_void() +
  #   theme(legend.position = "none") +
  #   scale_fill_viridis_d(option = "turbo")
  
  barcol <- ifelse(country_pick == "Palestine" | country_pick == "Ukraine", "#d5b60a", "#8b0000")
  
  base_bar <- ggplot(for_bar_plt, aes(x = reorder(actor, -conflicts_involved), y = conflicts_involved)) + 
    geom_col(fill = barcol) + 
    theme_minimal() 
  
  if (actor_dist == "few") {
    base_bar + theme(axis.text.x = element_text(angle = 60, vjust = 1, hjust = 1, size = 10)) +
      labs(x = "Actor",
           y = "Total Conflicts Involved",
           title = country_pick) + 
      scale_y_continuous(trans = pseudo_log_trans(base = 10)) + 
      scale_x_discrete(labels = for_bar_plt$label)
  } else {
    base_bar +  theme(axis.text.x = element_blank()) + 
      geom_label_repel(data = for_bar_labs, aes(y = conflicts_involved, label = label),
                       size = 3,
                       box.padding = unit(0.3, "lines"),
                       point.padding = unit(0.5, "lines"),
                       segment.colour = "grey50") + 
      labs(x = "Actor",
           y = "Total Conflicts Involved",
           title = country_pick) + 
      scale_y_continuous(trans = pseudo_log_trans(base = 10))
  }
   
  
}

actors_by_c_few <- map(c("Ukraine", "Palestine"),
    ~responsible_actors_bars(., actor_dist = "few"))

actors_by_c_few_grid <- grid.arrange(grobs = actors_by_c_few, nrow = 1)


ggsave("exhibits/07_actors/ex9_few_actors_bars.png", actors_by_c_few_grid,
       width = 8, height = 6)

actors_by_c_many <- map(c("Myanmar", "Sudan", "Democratic Republic of Congo", "Ethiopia" , "Nigeria"),
                       ~responsible_actors_bars(., actor_dist = "many"))

actors_by_c_many_grid <- grid.arrange(grobs = actors_by_c_many, nrow = 3)

ggsave("exhibits/07_actors/ex9_many_actors_bars.png", actors_by_c_many_grid,
       width = 7, height = 6)


