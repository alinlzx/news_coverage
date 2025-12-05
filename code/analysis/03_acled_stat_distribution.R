######################

# This script checks distribution of fatality stat in ACLED by region

#####################

source("header.R")

# reading in events data ---------------------
acled_events <- read_csv("data/mst/00b_acled_events_filter.csv") %>% 
  select(-1)

# state to region xwalk
country_region_xwalk <- read_csv("data/mst/06a_country_to_region_xwalk.csv")

acled_region <- acled_events %>%
  transform(country = clean_str_custom(country),
            civilians_flag = ifelse(grepl("civilian", event_type, ignore.case = T)|grepl("civilian", interaction, ignore.case = T),
                                    "Violence Against Civilians", "Violence Between Combatants")) %>% 
  left_join(country_region_xwalk, by = c("country")) %>% 
  code_region

acled_region_type <- acled_region %>% 
  group_by(subregion, civilians_flag) %>% 
  summarise(total_fat = sum(fatalities)) %>% 
  filter(subregion %in% c("Africa", "Greater ME", "Myanmar", "Palestine/Israel", "Ukraine/Russia"))

# bar plots by region and event type

ggplot(acled_region_type, aes(x = subregion, y = total_fat)) + 
  geom_bar(stat = "identity", position = "dodge", aes(fill = civilians_flag)) + 
  scale_fill_manual(values = c("#953551", "#c39969")) + 
  theme_bw() + 
  labs(x = "Region",
       y = "Total Fatalities") + 
  theme(legend.position = "bottom",
        legend.text = element_text(size = 15),
        axis.title.y = element_text(size = 15),
        axis.text.y = element_text(size = 15),
        axis.title.x = element_blank(),
        axis.text.x = element_text(angle = 75, vjust = 1, hjust = 1, size = 20))

plot_fat_dist <- function (region_pick) {
  
  for_dist <- acled_region %>% 
    filter(region_un == region_pick)
  
}
