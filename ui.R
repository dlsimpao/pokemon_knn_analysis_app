ui <-
  fluidPage(
    theme = shinytheme("united"),

    # test
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "card.css"),
      tags$link(rel = "stylesheet", type = "text/css", href = "polaroid.css")
    ),

    headerPanel("Poké Battle Matchup"),
    sidebarPanel(
      navbarPage("Trainers:",
        id = "tabs",
        tabPanel(
          "  You  ",
          helpText("Generation VIII not included."),
          textOutput("mytypes"),
          fluidRow(
            column(
              9,
              selectInput(
                "mon", "Pokemon",
                str_to_title(api_mons$name) %>%
                  gsub("-", " ", .), "Pikachu Phd"
              )
            ),
            column(3,
              style = "margin-top: 24px; width: auto",
              actionButton("mongo", "I choose you!")
            )
          ),
          br(),
          br(),

          selectInput("ability", "Ability", ""),
          textOutput("ability-info"),
          br(),

          selectInput("nature", "Nature", c("-", str_to_title(natures)), "-"),
          selectInput("item", "Held Item", c("-", str_to_title(usable_items$Name)), "-"),
          textOutput("item-info"),
          br(),

          helpText("Pokémon can learn at most 4 moves for battle."),
          selectizeInput("moves", "See Learnable Moves", "-",
            multiple = TRUE, options = list(maxItems = 4)
          ),
          actionButton("script", "Generate Pokémon File"),
          helpText(HTML("Formats the Pokémon's information. <br>
                                                   Attributes included: Item, Ability, EVs, Nature, Moves <br>
                                                   (Websites like <a>Pokemon Showdown</a> can import this file.)")),

          br(),
          br(),
          br(),
          br(),

          h2("Effort Values (EVs)", align = "center"),
          helpText("Effort Values help raise your total base stats.", textOutput("ev")),

          sliderInput("hp", "HP", 0, 252, 0, step = 4),
          sliderInput("atk", "Attack", 0, 252, 0, step = 4),
          sliderInput("spatk", "Sp. Attack", 0, 252, 0, step = 4),
          sliderInput("def", "Defense", 0, 252, 0, step = 4),
          sliderInput("spdef", "Sp. Defense", 0, 252, 0, step = 4),
          sliderInput("spd", "Speed", 0, 252, 0, step = 4)
        ),
        tabPanel(
          "Rival",

          helpText("Generation VIII not included."),
          textOutput("opptypes"),
          fluidRow(
            column(
              9,
              selectInput(
                "mon2", "Pokemon",
                str_to_title(api_mons$name) %>%
                  gsub("-", " ", .), "Blissey"
              )
            ),
            column(3,
              style = "margin-top: 24px; width: auto",
              actionButton("mongo2", "I choose you!")
            )
          ),
          br(),
          br(),

          selectInput("ability2", "Ability", ""),
          textOutput("ability-info2"),
          br(),

          selectInput("nature2", "Nature", c("-", str_to_title(natures)), selected = "-"),
          selectInput("item2", "Held Item", c("-", str_to_title(usable_items$Name)), selected = "-"),
          textOutput("item-info2"),
          br(),

          helpText("Pokémon can learn at most 4 moves for battle."),
          selectizeInput("moves2", "See Learnable Moves", "-",
            multiple = TRUE, options = list(maxItems = 4)
          ),
          actionButton("script2", "Generate Pokémon File"),
          helpText(HTML("Generates a text file that stores Pokémon info in a specified format. <br>
                                                   Attributes included: Item, Ability, EVs, Nature, Moves <br>
                                                   (Websites like <a>Pokemon Showdown</a> can import this file.)")),

          br(),
          br(),
          br(),
          br(),


          h2("Effort Values (EVs)", align = "center"),
          helpText("Effort Values help raise your total base stats.", textOutput("ev2")),

          sliderInput("hp2", "HP", 0, 252, 0, step = 4),
          sliderInput("atk2", "Attack", 0, 252, 0, step = 4),
          sliderInput("spatk2", "Sp. Attack", 0, 252, 0, step = 4),
          sliderInput("def2", "Defense", 0, 252, 0, step = 4),
          sliderInput("spdef2", "Sp. Defense", 0, 252, 0, step = 4),
          sliderInput("spd2", "Speed", 0, 252, 0, step = 4)
        )
      )
    ),
    mainPanel(
      navbarPage(
        "",
        tabPanel(
          "Home",
          fluidRow(
            column(
              6,
              div(
                class = "card",
                tags$img(uiOutput("sprite1"), width = "auto")
              )
            ),
            column(
              6,
              div(
                class = "card",
                tags$img(uiOutput("sprite2"), width = "auto")
              )
            )
          ),
          br(),

          div(
            h2(textOutput("table-title"), align = "center"),

            tableOutput("like"),
            align = "center"
          ),
          br(),
          br(),
          br(),



          div(plotlyOutput("fullstats", width = 600, height = 600),
            align = "center",
            tags$i(textOutput("caption"), style = "font-size:25px", align = "center")
          ) # full stats at Lv. 100
        ),
        tabPanel(
          "Trends",
          h1("Popularity Plot"),
          helpText("Gauged by search hits. (Proof of Concept)"),
          actionButton("popplot", "Generate Plot"),
          br(),
          br(),
          div(
            class = "polaroid",
            withSpinner(plotOutput("popularity"), type = 6, color = "red")
          ),
          br(),
          br(),

          h1("Popular Roles"),
          h3(strong(textOutput("role1"))),
          p(htmlOutput("desc1")),
          tags$head(tags$style("#desc1{
                                 font-size: 20px;
                                 }")),
          h3(strong(textOutput("role2"))),
          p(htmlOutput("desc2")),
          tags$head(tags$style("#desc2{
                                 font-size: 20px;
                                 }"))
        ),
        tabPanel(
          "About",
          h1("What are Pokémon?"),
          helpText("Taken from the Official Pokémon Website."),
          p("Pokémon are creatures of all shapes and sizes who live in the wild or alongside humans. 
                                      For the most part, Pokémon do not speak except to utter their names. Pokémon are raised and 
                                      commanded by their owners (called “Trainers”). During their adventures, Pokémon grow and become 
                                      more experienced and even, on occasion, evolve into stronger Pokémon. There are currently more 
                                      than 700 creatures that inhabit the Pokémon universe.", style = "font-size:20px"),
          br(),
          h2("Concepts"),
          p(strong("Pokémon Battle"), ": Pokémon use moves to cause the opposing Pokémon to faint (when their HP reaches zero).
            In a single battle turn, the Pokémon make moves sequentially, according to their speed and move priorities.", style = "font-size: 20px"),
          br(),
          p(strong("HP"), ": Hit points; the amount of damage the Pokemon can take", style = "font-size:20px"),
          p(strong("ATK"), ": base attack; accounts for physical damage it can give", style = "font-size:20px"),
          p(strong("DEF"), ": base defense; accounts for physical damage it can take", style = "font-size:20px"),
          p(strong("SPATK"), ": base special attack; accounts for special damage it can give", style = "font-size:20px"),
          p(strong("SPDEF"), ": base special defense; accounts for special damage it can take", style = "font-size:20px"),
          p(strong("SPD"), ": base speed; determines the move order in a battle", style = "font-size:20px"),
          br(),
          p(strong("EVs"), ": Effort Values; determines the allocation of stat increases as a Pokémon levels up.", style = "font-size: 20px"),
          p(strong("Max EVs per stat"), ": 252", style = "font-size:20px"),
          p(strong("Max Total EVs"), ": 510", style = "font-size: 20px"),
          p(strong("EV training"), ": In game, upon defeat of an opponent, certain EV stats are raised.", style = "font-size: 20px"),
          p(strong("IVs"), ": Individual Values; increases base stats by one. Max: 31", style = "font-size: 20px"),
          p("Different Natures influence base stats differently. (e.g. Adamant Nature boosts the ATK stat by 10% and 
            lowers SPATK stat by 10%.)", style = "font-size: 20px"),
          helpText("For practicality, EVs are capped at 508 and IVs are automatically included. The influence of Natures are assumed."),

          br(),
          h2("About the App"),
          p("The app serves as a statistical tool for players in the Pokémon Video Game Competition (VGC). The app has two main components: 'Home' and 'Trends'.
          The first page of the app displays the sprites and statistics of the two chosen Pokémon. To change the selected Pokémon, simply scroll through the select box to choose your Pokémon and your rival's Pokémon.
          The table titled 'Top 10 Pokémon with Similar Stats' is generated through KNN algorithm, which finds the 10 closest Pokémon
          in terms of stats. Utility-wise, this provides useful alternatives for the players. Furthermore, the radar plot is gerenated through 'plotly'
          and gives reference to strengths and weakness of the two Pokémon. The last feature of the first page is the 'Generate Pokémon File' button on the 
          side panel, which gathers the user inputs, formats them, and provides a pop-out for copying. The next page, 'Trends', portrays the Pokémon's popularity based on search hits over time. This provides some
          insight into the Pokémon's prevalence in Pokémon VGC community. Moreover, the most natural battle roles are assigned to the chosen
          Pokémon with some descriptions.", style = "font-size:20px")
        )
      )
    )
  )
