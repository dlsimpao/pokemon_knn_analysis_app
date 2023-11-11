# Table of stats information
stat_info <- stat_info %>% 
  select(`X.`,Name,Total,HP,Attack,Defense,`Sp..Atk`,`Sp..Def`,Speed) %>% 
  mutate(Name = str_to_lower(Name))

# Filter problem names
pnames <- stat_info[grepl('\\s',stat_info$Name),2] %>% pull() 
pnames <- pnames[!grepl('(alolan|galarian|partner|size|face|tempo)',pnames)]

# First name change
temp <- case_when(
  grepl('(mega charizard )',pnames) ~ gsub('(mega charizard )','-mega-',pnames),
  grepl('(mega mewtwo )',pnames) ~ gsub('(mega mewtwo )','-mega-',pnames),
  grepl('(mega)',pnames) ~ gsub('(mega).*','-mega',pnames),
  grepl('(\\. )', pnames) ~ gsub('(. )','-',pnames),
  grepl('(\\: )', pnames) ~ gsub('(. )','-',pnames),
  grepl('\\.$',pnames) ~ gsub('\\.$','',pnames),
  grepl('( form).*',pnames) ~ gsub('( form).*','',pnames),
  grepl('.(mode|cloak|kyogre|groudon|rotom|style|kyurem)$',pnames) ~ 
    gsub('.(mode|cloak|kyogre|groudon|rotom|style|kyurem)$','',pnames),
  grepl('^hoopa',pnames) ~ gsub('^hoopa','',pnames),
  TRUE ~ pnames
)

# Second name change
temp <- case_when(
  grepl('(castform)[^-]',temp) ~ gsub('(castform)','castform-',temp),
  grepl('(kyogre)[^-]',temp) ~ gsub('(kyogre)','kyogre-',temp),
  grepl('(groudon)[^-]',temp) ~ gsub('(groudon)','groudon-',temp),
  grepl('(deoxys)[^-]',temp) ~ gsub('(deoxys)','deoxys-',temp),
  grepl('(wormadam)[^-]',temp) ~ gsub('(wormadam)','wormadam-',temp),
  grepl('(rotom)[^-]',temp) ~ gsub('(rotom)','rotom-',temp),
  grepl('(giratina)[^-]',temp) ~ gsub('(giratina)','giratina-',temp),
  grepl('(basculin)[^-]',temp) ~ gsub('(basculin)','basculin-',temp),
  grepl('(darmanitan)[^-]',temp) ~ gsub('(darmanitan)','darmanitan-',temp),
  grepl('(tornadus)[^-]',temp) ~ gsub('(tornadus)','tornadus-',temp),
  grepl('(landorus)[^-]',temp) ~ gsub('(landorus)','landorus-',temp),
  grepl('(thundurus)[^-]',temp) ~ gsub('(thundurus)','thundurus-',temp),
  grepl('(kyurem)[^-]',temp) ~ gsub('(kyurem)','kyurem-',temp),
  grepl('(meloetta)[^-]',temp) ~ gsub('(meloetta)','meloetta-',temp),
  grepl('(aegislash)[^-]',temp) ~ gsub('(aegislash)','aegislash-',temp),
  grepl('(oricorio)[^-]',temp) ~ gsub('(oricorio)','oricorio-',temp),
  grepl('(shaymin)[^-]',temp) ~ gsub('(shaymin)','shaymin-',temp), 
  grepl('(keldeo)[^-]',temp) ~ gsub('(keldeo)','keldeo-',temp),
  grepl('(lycanroc)[^-]',temp) ~ gsub('(lycanroc)','lycanroc-',temp),
  grepl('(wishiwashi)[^-]',temp) ~ gsub('(wishiwashi)','wishiwashi-',temp),
  grepl('[^d](-meteor)',temp) ~ gsub('(-meteor)','-red-meteor',temp),
  grepl('(complete|-confined|ultra-necrozma)',temp) ~ gsub('(complete|-confined|ultra-necrozma)','',temp),
  grepl('\\s',temp) ~ gsub('\\s','-',temp),
  grepl('.{2}%',temp) ~ gsub('.{2}%','',temp),
  TRUE ~ temp
)


#################################################################################

# Modify stat info with better names
temp <- dbstats %>% mutate(Name = case_when(
  grepl('♀',Name) ~ gsub('♀','-f',Name),
  grepl('♂',Name) ~ gsub('♂','-m',Name),
  grepl('é',Name) ~ gsub('é','e',Name),
  grepl('(meganium|yanmega)',Name) ~ Name, # <- important potential bug
  grepl('mimikyu',Name) ~ 'mimikyu-disguised',
  grepl('(mega charizard )',Name) ~ gsub('(mega charizard )','-mega-',Name),
  grepl('(mega mewtwo )',Name) ~ gsub('(mega mewtwo )','-mega-',Name),
  grepl('(mega)',Name) ~ gsub('(mega).*','-mega',Name),
  grepl('(\\. )', Name) ~ gsub('(. )','-',Name),
  grepl('(\\: )', Name) ~ gsub('(. )','-',Name),
  grepl('\\.$',Name) ~ gsub('\\.$','',Name),
  grepl('( form).*',Name) ~ gsub('( form).*','',Name),
  grepl('.(mode|cloak|kyogre|groudon|rotom|style|kyurem|size)$',Name) ~ 
    gsub('.(mode|cloak|kyogre|groudon|rotom|style|kyurem|size)$','',Name),
  grepl('^hoopa',Name) ~ gsub('^hoopa','',Name),
  TRUE ~ Name)
)

temp <- temp %>% mutate(Name = case_when(
  grepl('(castform)[^-]',Name) ~ gsub('(castform)','castform-',Name),
  grepl('(kyogre)[^-]',Name) ~ gsub('(kyogre)','kyogre-',Name),
  grepl('(groudon)[^-]',Name) ~ gsub('(groudon)','groudon-',Name),
  grepl('(deoxys)[^-]',Name) ~ gsub('(deoxys)','deoxys-',Name),
  grepl('(wormadam)[^-]',Name) ~ gsub('(wormadam)','wormadam-',Name),
  grepl('(rotom)[^-]',Name) ~ gsub('(rotom)','rotom-',Name),
  grepl('(giratina)[^-]',Name) ~ gsub('(giratina)','giratina-',Name),
  grepl('(basculin)[^-]',Name) ~ gsub('(basculin)','basculin-',Name),
  grepl('(darmanitan)[^-]',Name) ~ gsub('(darmanitan)','darmanitan-',Name),
  grepl('(tornadus)[^-]',Name) ~ gsub('(tornadus)','tornadus-',Name),
  grepl('(landorus)[^-]',Name) ~ gsub('(landorus)','landorus-',Name),
  grepl('(thundurus)[^-]',Name) ~ gsub('(thundurus)','thundurus-',Name),
  grepl('(kyurem)[^-]',Name) ~ gsub('(kyurem)','kyurem-',Name),
  grepl('(meloetta)[^-]',Name) ~ gsub('(meloetta)','meloetta-',Name),
  grepl('(aegislash)[^-]',Name) ~ gsub('(aegislash)','aegislash-',Name),
  grepl('(oricorio)[^-]',Name) ~ gsub('(oricorio)','oricorio-',Name),
  grepl('(shaymin)[^-]',Name) ~ gsub('(shaymin)','shaymin-',Name), 
  grepl('(keldeo)[^-]',Name) ~ gsub('(keldeo)','keldeo-',Name),
  grepl('(lycanroc)[^-]',Name) ~ gsub('(lycanroc)','lycanroc-',Name),
  grepl('(wishiwashi)[^-]',Name) ~ gsub('(wishiwashi)','wishiwashi-',Name),
  grepl('(gourgeist)[^-]',Name) ~ gsub('(gourgeist)','gourgeist-',Name),
  grepl('(pumpkaboo)[^-]',Name) ~ gsub('(pumpkaboo)','pumpkaboo-',Name),
  grepl('(meowstic)[^-]',Name) ~ gsub('(meowstic)','meowstic-',Name),
  grepl('(indeedee)[^-]',Name) ~ gsub('(indeedee)','indeedee-',Name),
  grepl('(minior)[^-]',Name) ~ gsub('(minior)','minior-red-',Name),
  grepl('.(confined)',Name) ~ gsub('.(confined)','',Name),
  grepl('(complete|ultra-necrozma)',Name) ~ gsub('(complete|ultra-necrozma)','',Name),
  grepl('\\s',Name) ~ gsub('\\s','-',Name),
  grepl('.{2}%',Name) ~ gsub('.{2}%','',Name),
  grepl("'",Name) ~ gsub("'",'',Name),
  TRUE ~ Name)
)

#stat_info[is.na(stat_info$Total),]

temp$Name[grepl('minior',temp$Name)]
dbstats$Name[grepl('tapu',dbstats$Name)]
api_mons$name[grepl('farfetch',api_mons$name)]