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
  grepl(paste0(regexMons,"[^-]"),Name) ~ gsub(regexMons, "\\1-", Name),
  grepl("(minior)[^-]", Name) ~ gsub("(minior)", "\\1-red-", Name),
  grepl(".(confined)", Name) ~ gsub(".(confined)", "", Name),
  grepl("(complete|ultra-necrozma)", Name) ~ gsub("(complete|ultra-necrozma)", "", Name),
  grepl("\\s", Name) ~ gsub("\\s", "-", Name),
  grepl(".{2}%", Name) ~ gsub(".{2}%", "", Name),
  grepl("'", Name) ~ gsub("'", "", Name),
  TRUE ~ Name
))
temp$Name %>% str_subset('minior')
temp$Name %>% str_subset('wormadam')
