library(dplyr)
library(tidyverse)
library(fuzzyjoin)
library(ggthemes)
library(fmsb)
library(ggradar)
library(openxlsx)
library(janitor)
library(lubridate)
library(data.table)
library(glue)
library(stargazer)
library(countrycode)
library(rjson)
library(stringi)
library(r2country)
library(sf)
library(rnaturalearthdata)
library(rnaturalearth)
library(gridExtra)
library(RColorBrewer)
library(ggrepel)
library(zoo)

# function to clean strings
clean_str_custom <- function (str) {
  str %>% str_replace_all("\\-", " ") %>% 
    str_replace_all("\\s*\\([^\\)]+\\)", "") %>% 
    str_replace_all("[^[:alnum:]]", " ") %>% 
    stri_trans_general(id = "Latin-ASCII") %>% str_squish %>% 
    tolower
}

# a "comprehensive" lexicon for nyt articles on politics/conflict

poli_conflict_lexicon <- c("defense", "ambush", "ammunition", "armed", "arms", "army", "armistice", "battle", "blockade", "bomb", "cannon", "artillery", "civil", 
                           "colonel", "corporal", "conflict", "ceasefire", "corps", "destroy", "destruct", "draft", "fight", "fleet", "fort", "general", 
                           "grenade", "damage", "guerrilla", "gang", "gun", "infantry", "intervention", "intervene", "legion", "lieutenant", 
                           "military", "militant", "militia", "missile", "munition", "naval", "navy", "patrol", "pentagon", "radar", "rebel", 
                           "regiment", "rifle", "rocket", "sergeant", "shell", "soldier", "submarine", "surrender", "tnt", "troop", "war", "weapon",
                           
                           
                           "adversary", "alliance", "allied", "ally",  "autocrat",
                           "border", "campaign", "combat", "communist", "force", "conspiracy", "dictat",
                           "fascist", "humanitarian", "internat", "invade", "invasion", 
                           "junta", "jurisdiction", "liberate", "liberation", "radical", "reactionary", 
                           "revolu", "revolt", "secede", "secession", "treason",
                           
                           c("territory", "friction", "brigade", "target", "retaliat", "attack", "casualt", "assault", 
                             "drone", "escalat", "p o w", "explod", "explos", "blast", "kill", "injure", "hostage", "junta", "violen", 
                             "occupation", "occupy", "deploy", "fight", "clash", "crisis", "unrest"))


poli_conflict_lexicon_short <- c("defense",  "ammunition", "armed", "army", "armistice", "battle",  "bomb", "artillery",  
                                 "conflict", "cease fire", "destroy", "destruct", "fight",  
                                  "guerrilla", "gang",   
                                 "military", "militant", "militia", "missile", "munition", "rebel", 
                                  "rocket",  "soldier", "troop", "war", "weapon","combat",  "forces", 
                                  "humanitarian",  "invade", "invasion", 
                                 "junta", "territory", "retaliat", "attack", "casualt", "assault", 
                                   "drone", "escalat", "p o w", "explod", "explos",  "kill", "injure", "hostage", 
                                   "violen", 
                                 "deploy", "fight")


# a quick helper function to code up regions -----------

code_region <- function (df) {
  
  df %>% 
    transform(region_un = ifelse(country %in% c("palestine", "israel"), "Palestine/Israel",
                                 ifelse(country %in% c("ukraine", "russia"), "Ukraine/Russia", region_un)))  %>% 
    transform(subregion = ifelse((subregion == "Western Asia" & region_un != "Palestine/Israel") | country %in% c("Iran", "Afghanistan"), "Greater ME", 
                                 ifelse(country == "myanmar", "Myanmar", region_un)))
}
