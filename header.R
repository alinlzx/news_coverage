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

# a "comprehensive" lexicon for nyt articles on politics/conflict

poli_conflict_lexicon <- c("defense", "ambush", "ammunition", "armed", "arms", "army", "armistice", "battle", "blockade", "bomb", "cannon", "artillery", "civil", 
                           "colonel", "corporal", "conflict", "corps", "destroy", "destruct", "draft", "fight", "fleet", "fort", "general", 
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
                             "occupation", "occupy", "deploy", "fight", "clash", "crisis"))
