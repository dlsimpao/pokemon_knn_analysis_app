server <- function(input, output, session) {

  # reactive variables

  # Sets default values
  myMon <- reactive({
    if (input$mongo == 0) "pikachu-phd" else x()
  })

  oppMon <- reactive({
    if (input$mongo2 == 0) "blissey" else x2()
  })

  # for gtrends
  selected <- reactive({
    p1 <- myMon() %>% gsub("-", " ", .)
    p2 <- oppMon() %>% gsub("-", " ", .)
    c(p1, p2)
  })

  # updates default values
  x <- eventReactive(input$mongo, {
    str_to_lower(input$mon) %>% gsub("\\s", "-", .)
  })

  x2 <- eventReactive(input$mongo2, {
    str_to_lower(input$mon2) %>% gsub("\\s", "-", .)
  })






  ###################################################################################


  # REQUESTS #
  apimon <- reactive({
    str_glue("https://pokeapi.co/api/v2/pokemon/{specie}/", specie = myMon()) %>%
      fromJSON(flatten = TRUE)
  })

  apimon2 <- reactive({
    str_glue("https://pokeapi.co/api/v2/pokemon/{specie}/", specie = oppMon()) %>%
      fromJSON(flatten = TRUE)
  })

  ### Learnset ###
  learnset <- reactive({
    j <- str_glue("https://pokeapi.co/api/v2/pokemon/{specie}/", specie = myMon()) %>%
      fromJSON(flatten = TRUE)

    j$moves$move.name %>%
      str_to_title() %>%
      gsub("-", " ", .)
  })

  learnset2 <- reactive({
    j <- str_glue("https://pokeapi.co/api/v2/pokemon/{specie}/", specie = oppMon()) %>%
      fromJSON(flatten = TRUE)

    j$moves$move.name %>%
      str_to_title() %>%
      gsub("-", " ", .)
  })

  ### Types ###
  types1 <- reactive({
    j <- str_glue("https://pokeapi.co/api/v2/pokemon/{specie}/", specie = myMon()) %>%
      fromJSON(flatten = TRUE)
    paste0("[", j$types$type.name %>% str_to_upper(), "]")
  })

  types2 <- reactive({
    j <- str_glue("https://pokeapi.co/api/v2/pokemon/{specie}/", specie = oppMon()) %>%
      fromJSON(flatten = TRUE)
    paste0("[", j$types$type.name %>% str_to_upper(), "]")
  })


  ### Abilities ###
  ability1 <- reactive({
    j <- str_glue("https://pokeapi.co/api/v2/pokemon/{specie}/", specie = myMon()) %>%
      fromJSON(flatten = TRUE)
    j$abilities$ability.name %>% str_to_title()
  })

  ability2 <- reactive({
    j <- str_glue("https://pokeapi.co/api/v2/pokemon/{specie}/", specie = oppMon()) %>%
      fromJSON(flatten = TRUE)
    j$abilities$ability.name %>% str_to_title()
  })


  ### Sprites ###

  mysprite <- reactive({
    if (myMon() %in% dbsprites$name) {
      dbsprites %>%
        filter(name == myMon()) %>%
        select(sprite) %>%
        pull()
    } else {
      j2 <- str_glue("https://pokeapi.co/api/v2/pokemon/{specie}/", specie = myMon()) %>%
        fromJSON(flatten = TRUE)
      j2$sprites$front_default
    }
  })

  oppsprite <- reactive({
    if (oppMon() %in% dbsprites$name) {
      dbsprites %>%
        filter(name == oppMon()) %>%
        select(sprite) %>%
        pull()
    } else {
      j2 <- str_glue("https://pokeapi.co/api/v2/pokemon/{specie}/", specie = oppMon()) %>%
        fromJSON(flatten = TRUE)
      j2$sprites$front_default
    }
  })

  ############################ gtrends ############################

  trend <- reactive({
    t <- lapply(selected(), function(x) {
      gtrends(x)$interest_over_time %>%
        select(keyword, date, hits) %>%
        filter(as.Date(date) > Sys.Date() - 31 * 3) %>%
        mutate(hits = as.numeric(hits))
    })
    rbind(t[[1]], t[[2]])
  })

  new_trend <- eventReactive(input$popplot, {
    trend()
  })


  ############################################### stats for KNN #############################################

  kdata <- reactive({
    stat_info %>%
      filter(name != myMon())
  })

  kstat <- reactive({
    a <- data.frame(
      name = myMon(),
      HP = apimon()$stats$base_stat[[1]],
      Attack = apimon()$stats$base_stat[[2]],
      Defense = apimon()$stats$base_stat[[3]],
      `Sp..Atk` = apimon()$stats$base_stat[[4]],
      `Sp..Def` = apimon()$stats$base_stat[[5]],
      Speed = apimon()$stats$base_stat[[6]]
    )

    a <- a %>% mutate(Total = as.integer(rowSums(.[2:7])))
    a <- a %>% select(1, 8, 2:7)
  })

  kdata3 <- reactive({
    rbind(kstat(), kdata()) # puts the chosen mon in the first row
  })


  kdata4 <- reactive({
    kdata3() %>%
      select(2:8)
  })

  # KNN #
  kdata5 <- reactive({
    as.numeric(knnx.index(kdata4(), kdata4()[1, , drop = FALSE], k = 11))
  })

  kdata6 <- reactive({
    df <- (kdata3()[kdata5(), 1]) %>% str_to_title()
    df[2:11] %>% gsub("-", " ", .)
  })

  # Rival #

  k2data <- reactive({
    stat_info %>%
      filter(name != oppMon())
  })

  k2stat <- reactive({
    a <- data.frame(
      name = oppMon(),
      HP = apimon2()$stats$base_stat[[1]],
      Attack = apimon2()$stats$base_stat[[2]],
      Defense = apimon2()$stats$base_stat[[3]],
      `Sp..Atk` = apimon2()$stats$base_stat[[4]],
      `Sp..Def` = apimon2()$stats$base_stat[[5]],
      Speed = apimon2()$stats$base_stat[[6]]
    )

    a <- a %>% mutate(Total = as.integer(rowSums(.[2:7])))
    a <- a %>% select(1, 8, 2:7)
  })

  k2data3 <- reactive({
    rbind(k2stat(), k2data()) # puts the chosen mon in the first row
  })


  k2data4 <- reactive({
    k2data3() %>%
      select(2:8)
  })

  # KNN #
  k2data5 <- reactive({
    as.numeric(knnx.index(k2data4(), k2data4()[1, , drop = FALSE], k = 11))
  })

  k2data6 <- reactive({
    df <- (k2data3()[k2data5(), 1]) %>% str_to_title()
    df[2:11] %>% gsub("-", " ", .)
  })

  # Combine Tables

  ktable <- reactive({
    t <- cbind(kdata6(), k2data6())
    tnames <- c(myMon(), oppMon()) %>%
      str_to_title() %>%
      gsub("-", " ", .)
    colnames(t) <- tnames
    t
  })


  ##########################################################################################################


  # Base Stats at 100

  # Include Nature influence

  bstat <- reactive({
    temp <- data.frame(
      HP = apimon()$stats$base_stat[[1]] * 2,
      ATK = apimon()$stats$base_stat[[2]] * 2,
      DEF = apimon()$stats$base_stat[[3]] * 2,
      SPATK = apimon()$stats$base_stat[[4]] * 2,
      SPDEF = apimon()$stats$base_stat[[5]] * 2,
      SPD = apimon()$stats$base_stat[[6]] * 2
    )
  })

  bstat2 <- reactive({
    temp <- data.frame(
      HP = apimon2()$stats$base_stat[[1]] * 2,
      ATK = apimon2()$stats$base_stat[[2]] * 2,
      DEF = apimon2()$stats$base_stat[[3]] * 2,
      SPATK = apimon2()$stats$base_stat[[4]] * 2,
      SPDEF = apimon2()$stats$base_stat[[5]] * 2,
      SPD = apimon2()$stats$base_stat[[6]] * 2
    )
  })

  #################################### EVS #############################################
  totev <- reactive({
    sum(input$hp + input$atk + input$def + input$spatk + input$spdef + input$spd)
  })

  totev2 <- reactive({
    sum(input$hp2 + input$atk2 + input$def2 + input$spatk2 + input$spdef2 + input$spd2)
  })

  # Works with observe # Resets EVs if they go over 508
  evlist <- reactive({
    a <- data.frame(
      HP = 0, Atk = 0,
      Def = 0, SpA = 0,
      SpD = 0, Spe = 0
    )

    if (totev() <= 508) {
      a <- data.frame(
        HP = input$hp, Atk = input$atk,
        Def = input$def, SpA = input$spatk,
        SpD = input$spdef, Spe = input$spd
      )
    }
    a
  })

  evlist2 <- reactive({
    a <- data.frame(
      HP = 0, Atk = 0,
      Def = 0, SpA = 0,
      SpD = 0, Spe = 0
    )

    if (totev2() <= 508) {
      a <- data.frame(
        HP = input$hp2, Atk = input$atk2,
        Def = input$def2, SpA = input$spatk2,
        SpD = input$spdef2, Spe = input$spd2
      )
    }
    a
  })

  # Impact of EVs on base stats
  evhp <- reactive(input$hp / 4)
  evstk <- reactive(input$atk / 4)
  evdf <- reactive(input$def / 4)
  evsatk <- reactive(input$spatk / 4)
  evsdf <- reactive(input$spdef / 4)
  evspd <- reactive(input$spd / 4)

  evhp2 <- reactive(input$hp2 / 4)
  evstk2 <- reactive(input$atk2 / 4)
  evdf2 <- reactive(input$def2 / 4)
  evsatk2 <- reactive(input$spatk2 / 4)
  evsdf2 <- reactive(input$spdef2 / 4)
  evspd2 <- reactive(input$spd2 / 4)


  # stats chart
  stats_chart <- reactive({
    # add in IVs values later - just add 31
    a <- tibble(
      HP = c(bstat()$HP + 204, 0, bstat()$HP + 110 + evhp()),
      Attack = c(bstat()$ATK + 99, 0, bstat()$ATK + 5 + evstk()),
      Defense = c(bstat()$DEF + 99, 0, bstat()$DEF + 5 + evdf()),
      `Sp. Attack` = c(bstat()$SPATK + 99, 0, bstat()$SPATK + 5 + evsatk()),
      `Sp. Defense` = c(bstat()$SPDEF + 99, 0, bstat()$SPDEF + 5 + evsdf()),
      Speed = c(bstat()$SPD + 99, 0, bstat()$SPD + 5 + evspd())
    )

    names(a) <- c(
      paste("HP", bstat()$HP + 110 + evhp()),
      paste("Attack", bstat()$ATK + 5 + evstk()),
      paste("Defense", bstat()$DEF + 5 + evdf()),
      paste("Sp. Attack", bstat()$SPATK + 5 + evsatk()),
      paste("Sp. Defense", bstat()$SPDEF + 5 + evsdf()),
      paste("Speed", bstat()$SPD + 5 + evspd())
    )
    a
  })

  stats_chart2 <- reactive({
    # add in IVs values later - just add 31
    a <- tibble(
      HP = c(bstat2()$HP + 204, 0, bstat2()$HP + 110 + evhp2()),
      Attack = c(bstat2()$ATK + 99, 0, bstat2()$ATK + 5 + evstk2()),
      Defense = c(bstat2()$DEF + 99, 0, bstat2()$DEF + 5 + evdf2()),
      `Sp. Attack` = c(bstat2()$SPATK + 99, 0, bstat2()$SPATK + 5 + evsatk2()),
      `Sp. Defense` = c(bstat2()$SPDEF + 99, 0, bstat2()$SPDEF + 5 + evsdf2()),
      Speed = c(bstat2()$SPD + 99, 0, bstat2()$SPD + 5 + evspd2())
    )

    names(a) <- c(
      paste("HP", "(", bstat2()$HP + 110 + evhp2(), ")"),
      paste("Attack", "(", bstat2()$ATK + 5 + evstk2(), ")"),
      paste("Defense", "(", bstat2()$DEF + 5 + evdf2(), ")"),
      paste("Sp. Attack", "(", bstat2()$SPATK + 5 + evsatk2(), ")"),
      paste("Sp. Defense", "(", bstat2()$SPDEF + 5 + evsdf2(), ")"),
      paste("Speed", "(", bstat2()$SPD + 5 + evspd2(), ")")
    )
    a
  })


  ########## ITEMS ##############

  item1 <- reactive({
    usable_items %>%
      filter(Name == input$item) %>%
      select(Effect) %>%
      pull()
  })

  item2 <- reactive({
    usable_items %>%
      filter(Name == input$item2) %>%
      select(Effect) %>%
      pull()
  })

  ########### ABILITY #############

  ab1 <- reactive({
    ab_info %>%
      filter(Name == input$ability %>%
        gsub(" ", "-", .)) %>%
      select(Description) %>%
      pull()
  })

  ab2 <- reactive({
    ab_info %>%
      filter(Name == input$ability2 %>%
        gsub(" ", "-", .)) %>%
      select(Description) %>%
      pull()
  })

  ################ EV ###############
  ev1 <- reactive({
    if (totev() <= 508) {
      paste0("Remaining: ", 508 - totev())
    } else {
      paste0("Exceeded!")
    }
  })

  ev2 <- reactive({
    if (totev2() <= 508) {
      paste0("Remaining: ", 508 - totev2())
    } else {
      paste0("Exceeded!")
    }
  })


  myMove <- reactive({
    req(input$moves != "")
    atk_info %>%
      filter(Name %in% input$moves) %>%
      select(Name, Type, Effect)
  })

  oppMove <- reactive({
    req(input$moves2 != "")
    atk_info %>%
      filter(Name %in% input$moves2) %>%
      select(Name, Type, Effect)
  })

  ######################## OUTPUTS ########################

  output$sprite1 <- renderUI({
    tags$img(
      src = mysprite(), width = "250", height = "250",
      style = "display: block; margin-left: auto; margin-right: auto"
    )
  })

  output$sprite2 <- renderUI({
    tags$img(
      src = oppsprite(), width = "250", height = "250",
      style = "display: block; margin-left: auto; margin-right: auto"
    )
  })



  output$`table-title` <- renderText("Top 10 Pokémon with Similar Stats to:")

  output$knn_info <- renderText("Table generated from KNN")

  output$like <- renderTable(ktable(),
    width = "75%",
    align = "c",
    border = TRUE,
    hover = TRUE,
    colnames = TRUE,
    caption = "The table above is generated from a KNN algorithm."
  )



  ############################ RADAR PLOT #########################

  output$fullstats <- renderPlotly({
    validate(
      need(myMon() %in% api_mons & oppMon() %in% api_mons, "Please enter a valid Pokemon.")
    )

    plot_ly(
      type = "scatterpolar",
      mode = "closest",
      fill = "toself"
    ) %>%
      add_trace(
        r = as.matrix(stats_chart()[3, ]),
        theta = c("HP", "ATK", "DEF", "SP.ATK", "SP.DEF", "SPD"),
        showlegend = TRUE,
        mode = "markers",
        name = str_to_title(myMon())
      ) %>%
      add_trace(
        r = as.matrix(stats_chart2()[3, ]),
        theta = c("HP", "ATK", "DEF", "SP.ATK", "SP.DEF", "SPD"),
        showlegend = TRUE,
        mode = "markers",
        name = str_to_title(oppMon())
      ) %>%
      layout(
        title = list(
          text = "Base Stats at Lv. 100",
          x = 0.46,
          font = list(
            size = 30
          )
        ),

        margin = list(t = 100),
        polar = list(
          radialaxis = list(
            visible = T,
            range = c(0, 700)
          )
        ),
        showlegend = TRUE
      )
  })

  # roles
  myRole <- reactive({
    if (grepl("pikachu", myMon())) {
      stat_role %>%
        filter(name == "pikachu") %>%
        pull(role)
    } else {
      stat_role %>%
        filter(name == myMon()) %>%
        pull(role)
    }
  })

  oppRole <- reactive({
    if (grepl("pikachu", oppMon())) {
      stat_role %>%
        filter(name == "pikachu") %>%
        pull(role)
    } else {
      stat_role %>%
        filter(name == oppMon()) %>%
        pull(role)
    }
  })


  myRoleDesc <- reactive({
    if (myRole() == "Physical Sweeper") {
      s <- roleDesc$ps
    } else if (myRole() == "Special Sweeper") {
      s <- roleDesc$ss
    } else if (myRole() == "Physical Tank") {
      s <- roleDesc$pt
    } else {
      s <- roleDesc$st
    }
  })

  oppRoleDesc <- reactive({
    if (oppRole() == "Physical Sweeper") {
      s <- roleDesc$ps
    } else if (oppRole() == "Special Sweeper") {
      s <- roleDesc$ss
    } else if (oppRole() == "Physical Tank") {
      s <- roleDesc$pt
    } else {
      s <- roleDesc$st
    }
  })


  #################################################################

  output$mytypes <- renderText(types1())
  output$opptypes <- renderText(types2())

  output$`ability-info` <- renderText(ab1())
  output$`ability-info2` <- renderText(ab2())


  output$`item-info` <- renderText(item1())
  output$`item-info2` <- renderText(item2())

  output$`move-info` <- renderTable(myMove(),
    width = "auto"
  )
  output$`move-info2` <- renderTable(oppMove(),
    width = "auto"
  )

  output$ev <- renderText(ev1())
  output$ev2 <- renderText(ev2())

  output$caption <- renderText("*Toggle the EVs to see how it affects base stats.")

  # gtrend

  output$popularity <- renderPlot({
    ggplot() +
      geom_line(aes(x = date, y = hits, color = keyword), data = new_trend(), size = 2) +
      xlab("Date") +
      ylab("Hits") +
      ggtitle("Popularity in the Past Three Months") +
      theme_bw()
  })

  # roles
  output$role1 <- renderText({
    aname <- myMon() %>%
      gsub("-", " ", .) %>%
      str_to_title()
    paste0(aname, " - ", myRole())
  })

  output$role2 <- renderText({
    aname <- oppMon() %>%
      gsub("-", " ", .) %>%
      str_to_title()
    paste0(aname, " - ", oppRole())
  })

  output$desc1 <- renderUI(HTML(myRoleDesc()))
  output$desc2 <- renderUI(HTML(oppRoleDesc()))
  ###################### OBSERVE #########################



  ####################### MOVES ############################

  observe({
    updateSelectInput(session, "moves",
      choices = c(learnset()),
      selected = ""
    )
  })

  observe({
    updateSelectInput(session, "moves2",
      choices = c(learnset2()),
      selected = ""
    )
  })

  ######################## ABILITIY #######################
  observe({
    updateSelectInput(session, "ability",
      choices = c(ability1() %>%
        gsub("-", " ", .))
    )
  })

  observe({
    updateSelectInput(session, "ability2",
      choices = c(ability2() %>%
        gsub("-", " ", .))
    )
  })


  ############################ EVS ##########################

  observe({
    updateSliderInput(session, "hp", value = evlist()$HP)
  })

  observe({
    updateSliderInput(session, "atk", value = evlist()$Atk)
  })

  observe({
    updateSliderInput(session, "def", value = evlist()$Def)
  })

  observe({
    updateSliderInput(session, "spatk", value = evlist()$SpA)
  })

  observe({
    updateSliderInput(session, "spdef", value = evlist()$SpD)
  })

  observe({
    updateSliderInput(session, "spd", value = evlist()$Spe)
  })



  observe({
    updateSliderInput(session, "hp2", value = evlist2()$HP)
  })

  observe({
    updateSliderInput(session, "atk2", value = evlist2()$Atk)
  })

  observe({
    updateSliderInput(session, "def2", value = evlist2()$Def)
  })

  observe({
    updateSliderInput(session, "spatk2", value = evlist2()$SpA)
  })

  observe({
    updateSliderInput(session, "spdef2", value = evlist2()$SpD)
  })

  observe({
    updateSliderInput(session, "spd2", value = evlist2()$Spe)
  })


  ######################################################################

  # Pokemon Script

  observeEvent(input$script, {
    # items
    i <- ifelse(input$item == "-", "", paste0(" @ ", input$item))
    # nature
    n <- ifelse(input$nature == "-", "", paste0(input$nature, " Nature<br>"))
    # evs
    e <- if (sum(evlist()) != 0) {
      e1 <- names(evlist())
      e2 <- lapply(seq(evlist()), function(x) paste(evlist()[x], e1[x], "/"))
      e3 <- paste0("EVs: ", paste(unlist(e2), collapse = " "), "<br>")
    } else {
      ""
    }
    # moves
    m <- if (length(input$moves) > 0) {
      m1 <- lapply(seq(input$moves), function(x) paste0("- ", input$moves[x], "<br>"))
      m2 <- paste0(unlist(m1), collapse = " ")
    } else {
      ""
    }

    showModal(modalDialog(
      title = "Copy/Paste: Pokémon Text File",
      easyClose = TRUE,
      p(HTML(
        input$mon, i, "<br>",
        "Ability:", input$ability, "<br>",
        e, n, m
      ))
    ))
  })

  observeEvent(input$script2, {
    # items
    i <- ifelse(input$item2 == "-", "", paste0(" @ ", input$item2))
    # nature
    n <- ifelse(input$nature2 == "-", "", paste0(input$nature2, " Nature<br>"))
    # evs
    e <- if (sum(evlist2()) != 0) {
      e1 <- names(evlist2())
      e2 <- lapply(seq(evlist2()), function(x) paste(evlist2()[x], e1[x], "/"))
      e3 <- paste0("EVs: ", paste(unlist(e2), collapse = " "), "<br>")
    } else {
      ""
    }
    # moves
    m <- if (length(input$moves2) > 0) {
      m1 <- lapply(seq(input$moves2), function(x) paste0("- ", input$moves2[x], "<br>"))
      m2 <- paste0(unlist(m1), collapse = " ")
    } else {
      ""
    }
    showModal(modalDialog(
      title = "Copy/Paste: Pokémon Text File",
      easyClose = TRUE,
      p(HTML(
        input$mon2, i, "<br>",
        "Ability:", input$ability2, "<br>",
        e, n, m
      ))
    ))
  })
}
