#create a signle data frame from all API requests

l = api_mons$name


#would take too long, need to create a database

#sprites
j2 <- str_glue("https://pokeapi.co/api/v2/pokemon/{specie}/", specie = myMon()) %>%
  fromJSON(flatten = TRUE)
j2$sprites$front_default

sprite_list <- sapply(l, function(x) {
  j <- str_glue("https://pokeapi.co/api/v2/pokemon/{specie}/", specie = x) %>%
    fromJSON(flatten = TRUE)
  j$sprites$front_default
})

