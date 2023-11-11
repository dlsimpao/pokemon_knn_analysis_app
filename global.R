# packages
library(shiny)
library(shinythemes)
library(shinycssloaders)
library(shinyWidgets)
library(tidyverse)
library(knitr)
library(ggfortify)
library(plotly)
library(FNN)
library(jsonlite)
library(lubridate)
library(httr)
library(rvest)
library(shinyjs)
library(gtrendsR)

# To install
packages <- c(
  "shiny", "shinythemes", "shinycssloaders", "shinyWidgets", "tidyverse", "knitr", "ggfortify",
  "plotly", "FNN", "jsonlite", "lubridate", "httr", "rvest", "shinyjs", "gtrendsR"
)

# Inspired by https://github.com/ThiagoValentimMarques/The-ten-most-similar-players-Pro-Evolution-Soccer-2019



# package.check <- lapply(packages, FUN = function(x) {
#   if (!require(x, character.only = TRUE)) {
#     install.packages(x, dependencies = TRUE)
#   }
# })



links <- data.frame(
  api_mon = "https://pokeapi.co/api/v2/pokemon",
  api_nat = "https://pokeapi.co/api/v2/nature",
  db_stats = "https://pokemondb.net/pokedex/all",
  db_spr = "https://pokemondb.net/pokedex/national",
  db_item = "https://pokemondb.net/item/all",
  db_ab = "https://pokemondb.net/ability",
  db_move = "https://pokemondb.net/move/all",
  stringsAsFactors = FALSE
)

# API Requests #
rname <- GET(links$api_mon, query = list(limit = 897))
rnat <- GET(links$api_nat, query = list(limit = 60))

stop_for_status(rname)
stop_for_status(rnat)

jname <- content(rname, as = "text") %>% fromJSON()
jnat <- content(rnat, as = "text") %>% fromJSON()

# df of names and url
api_mons <- as.data.frame(jname$results)

# all mons with hypenated names
hype_names <- api_mons[grepl("-", api_mons$name), ]

# list of natures
natures <- jnat$results$name

# Web Scraping #

# `html`: sprites
html <- read_html(links$db_spr)

# list of sprite pngs
sprites <- html %>%
  html_nodes("span.infocard-lg-img") %>%
  html_nodes(".img-fixed.img-sprite") %>%
  html_attr("data-src")

# names available in the PokeDB
dbnames <- html %>%
  html_nodes("a.ent-name") %>%
  html_text() %>%
  as.data.frame()

# rename columns to 'name'
colnames(dbnames) <- "name"

dbmon_sprites <- tibble(dbnames) %>% mutate(name = str_to_lower(name), sprite = sprites)

# add new sprites png (Nidoran, Mr. Mime, other Megas not matched)
dbsprites <- left_join(api_mons, dbmon_sprites, by = "name") %>%
  select(name, sprite) %>%
  filter(!is.na(sprite))


# More Web Scraping #
html2 <- read_html(links$db_item) # items
html3 <- read_html(links$db_ab) # abilities
html4 <- read_html(links$db_move) # moves
html5 <- read_html(links$db_stats) # stats

# `html2`: items
items <- html2 %>%
  html_nodes("table.data-table.block-wide") %>%
  html_table() %>%
  as.data.frame() %>%
  tibble()

# Table of item information - filtered on usable held items
usable_items <- items %>%
  filter(Category %in% c("Hold items", "Berries")) %>%
  arrange(Category)


# `html3`: abilities
ab_info <- html3 %>%
  html_nodes("table#abilities") %>%
  html_table() %>%
  as.data.frame() %>%
  tibble()

# Table of ability information
ab_info <- ab_info %>%
  select(Name, Description) %>%
  mutate(Name = sub("\\s", "-", .$Name))

# `html4`: attacks
atk_info <- html4 %>%
  html_nodes("table#moves") %>%
  html_table() %>%
  as.data.frame() %>%
  tibble()

# Table of attack information
atk_info <- atk_info %>%
  select(Name, Type, Power, `Acc.`, Effect)
# %>%  mutate(Name = sub(' +','-',.$Name))

# `html5`: stats

dbstats <- html5 %>%
  html_nodes("table#pokedex") %>%
  html_table() %>%
  as.data.frame() %>%
  tibble()

dbstats <- dbstats %>%
  select(`X.`, Name, Total, HP, Attack, Defense, `Sp..Atk`, `Sp..Def`, Speed) %>%
  mutate(Name = str_to_lower(Name))


# Modify dbstats names to match api_mons names
temp <- dbstats %>% mutate(Name = case_when(
  grepl("<U+2640>", Name) ~ gsub("<U+2640>", "-f", Name),
  grepl("<U+2642>", Name) ~ gsub("<U+2642>", "-m", Name),
  grepl("é", Name) ~ gsub("é", "e", Name),
  grepl("(meganium|yanmega)", Name) ~ Name, # <- important potential bug
  grepl("mimikyu", Name) ~ "mimikyu-disguised",
  grepl("(mega charizard )", Name) ~ gsub("(mega charizard )", "-mega-", Name),
  grepl("(mega mewtwo )", Name) ~ gsub("(mega mewtwo )", "-mega-", Name),
  grepl("(mega)", Name) ~ gsub("(mega).*", "-mega", Name),
  grepl("(\\. )", Name) ~ gsub("(. )", "-", Name),
  grepl("(\\: )", Name) ~ gsub("(. )", "-", Name),
  grepl("\\.$", Name) ~ gsub("\\.$", "", Name),
  grepl("( form).*", Name) ~ gsub("( form).*", "", Name),
  grepl(".(mode|cloak|kyogre|groudon|rotom|style|kyurem|size)$", Name) ~
  gsub(".(mode|cloak|kyogre|groudon|rotom|style|kyurem|size)$", "", Name),
  grepl("^hoopa", Name) ~ gsub("^hoopa", "", Name),
  TRUE ~ Name
))

regexMons <- "(castform|kyogre|groudon|deoxys|wormadam|rotom|giratina|basculin|darmanitan|tornadus|landorus|thundurus|
kyurem|meloetta|aegislash|oricorio|shaymin|keldeo|lycanroc|wishiwashi|gourgeist|pumpkaboo|meowstic|indeedee)"

temp <- temp %>% mutate(Name = case_when(
  grepl(paste0(regexMons, "[^-]"), Name) ~ gsub(regexMons, "\\1-", Name),
  grepl("(minior)[^-]", Name) ~ gsub("(minior)", "\\1-red-", Name),
  grepl(".(confined)", Name) ~ gsub(".(confined)", "", Name),
  grepl("(complete|ultra-necrozma)", Name) ~ gsub("(complete|ultra-necrozma)", "", Name),
  grepl("\\s", Name) ~ gsub("\\s", "-", Name),
  grepl(".{2}%", Name) ~ gsub(".{2}%", "", Name),
  grepl("'", Name) ~ gsub("'", "", Name),
  TRUE ~ Name
))

# LEFT JOIN with api_mons; Table of stats info
stat_info <- left_join(api_mons, temp, by = c("name" = "Name")) %>%
  select(name, Total, HP, Attack, Defense, `Sp..Atk`, `Sp..Def`, Speed) %>%
  drop_na()

# Pokemon Roles based on stats

# Physical Sweeper
ps <- stat_info[[4]] + stat_info[[8]]
# Special Sweeper
ss <- stat_info[[6]] + stat_info[[8]]
# Wall
w <- stat_info[[3]] + stat_info[[5]] + stat_info[[7]]
# Physical Tank
pt <- stat_info[[4]] + stat_info[[5]]
# Special Tank
st <- stat_info[[6]] + stat_info[[7]]


stat_role <- tibble(
  name = stat_info[1],
  `Physical Sweeper` = ps,
  `Special Sweeper` = ss,
  # Wall = w,
  `Physical Tank` = pt,
  `Special Tank` = st
)

role <- colnames(stat_role)[max.col(stat_role[2:5], ties.method = "first") + 1]

set.seed(151)

stat_role <- stat_role %>%
  mutate(role = role)


############### Role Desc ###################
roleDesc <- tibble(
  ps = "This Pokémon has good Attack and Speed stats. To do well, it needs to outspeed the opponent's team and deal heavy damage.
    Here is an example of Physical Sweeper:<br><br>
      Lucario @ Life Orb
      <br>Ability: Inner Focus
      <br>EVs: 4 HP / 252 Atk / 252 Spe
      <br>Adamant nature 
      <br>- Swords Dance
      <br>- Close Combat
      <br>- ExtremeSpeed
      <br>- Crunch",
  ss = "This Pokémon has good Special Attack and Speed stats. To do well, it needs to outspeed the opponent's team and deal heavy damage.
    Here is an example of Special Sweeper:<br><br>
      Starmie @ Leftovers
      <br>Ability: Natural Cure
      <br>EVs: 42 HP / 216 Spe / 252 SpA
      <br>Timid Nature 
      <br>- Surf
      <br>- Ice Beam
      <br>- Thunderbolt
      <br>- Recover",
  pt = "This Pokémon has good Attack and Defense stats. To do well, it must take the brunt of the opposing team's damage and wear them down.
      Here is an example of Physical Tank:<br><br>
      Steelix @ Leftovers 
      <br>Ability: Sturdy 
      <br>EVs: 252 HP / 80 Atk / 176 Def
      <br>Impish Nature
      <br>- Earthquake
      <br>- Rock Slide
      <br>- Explosion
      <br>- Toxic",
  st = "This Pokémon has good Special Attack and Special Defense stats. To do well, it must take the brunt of the opposing team's damage and wear them down.
    Here is an example of Special Tank:<br><br>
      Blissey @ Leftovers
      <br>Ability: Natural Cure
      <br>EVs: 252 HP / 252 Def / 4 SpD
      <br>Bold Nature
      <br>- Seismic Toss
      <br>- Toxic
      <br>- Soft-Boiled
      <br>- Heal Bell"
)
