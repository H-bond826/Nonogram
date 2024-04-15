ui <- fluidPage(
  titlePanel("Nonograms"),
  tags$style(type="text/css", "#controls { width: 60%; margin: 0 auto; }"),
  fluidRow(
    column(4,
           div(id = "controls",
               wellPanel(
                 style = "background-color: #FFFFFF;",
                 tags$head(
                   tags$style(HTML(".js-irs-0 .irs-grid { display: none; }"))
                 ),
                 sliderInput("gridSize", "Grid Size", min = 5, max = 20, value = 6),
                 selectInput("difficulty", "difficulty", choices = c("Easy", "Medium", "Expert"), selected = "Medium"),
                 actionButton("update", "Generate", style = "background-color: #3399FF; color: white;"),  
                 actionButton("verify", "Check", style = "background-color: green; color: white;")  
               )
           )
    ),
    column(8,
           conditionalPanel(
             condition = "output.gridExists",
             plotOutput("grid", click = "grid_click", height = "800px")  
           ),
           conditionalPanel(
             condition = "!output.gridExists",
             tags$div(
               style = "text-align: center; padding: 50px;",
               tags$h1("Nonograms ", style = "color: #3399FF;"),
               tags$p("Le but de ce jeu est de découvrir une planche de cellules bleues et de cellules libres. 
                        En exemple 1 5 2 représente successivement: 1 cellule, 5 cellules et 2 cellules bleues séparées entre elles par une ou plusieurs cellules vides.
                        Cliquez les cellules une fois pour les marquer occupées. Cliquez encore une fois afin de les marquer avec un X.", style = "font-size: 15px;")
             )
           )
           
    )
  )
)