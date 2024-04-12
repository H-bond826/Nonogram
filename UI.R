ui <- fluidPage(
  titlePanel("Nonograms"),
  tags$style(type="text/css", "#controls { width: 60%; margin: 0 auto; }"),
  fluidRow(
    column(4,
           div(id = "controls",
               wellPanel(
                 sliderInput("gridSize", "nxn", min = 5, max = 20, value = 6),
                 selectInput("difficulty", "difficulty", choices = c("Easy", "Medium", "Hard"), selected = "Medium"),
                 actionButton("update", "Generate", style = "background-color: #3399FF; color: white;"),  
                 actionButton("verify", "Check", style = "background-color: green; color: white;")  
               )
           )
    ),
    column(8,
           wellPanel(
             conditionalPanel(
               condition = "output.gridExists",
               plotOutput("grid", click = "grid_click", height = "800px")  # Increase the height of the grid
             ),
             conditionalPanel(
               condition = "!output.gridExists",
               tags$div(
                 style = "text-align: center; padding: 50px;",
                 tags$h1("Nonograms ", style = "color: #3399FF;"),
                 tags$p("Le but de ce jeu est de découvrir une planche de cellules bleues et de cellules libres. 
                        Vous pouvez faire ceci en suivant les définitions des lignes et des colonnes – des séquences de nombres qui décrivent les groupes de cellules bleues apparaissant sur ces lignes et colonnes. 
                        En exemple 1 5 2 représente successivement: 1 cellule, 5 cellules et 2 cellules bleues séparées entre elles par une ou plusieurs cellules vides.
                        Cliquez les cellules une fois pour les marquer occupées. Cliquez encore une fois afin de les marquer avec un X.", style = "font-size: 15px;")
               )
             )
           )
    )
  )
)



# Server
server <- function(input, output, session) {
  # Reactive values for the grid size, grid state, indications and counters
  grid_size <- reactiveVal()
  grid_state <- reactiveVal()
  indications <- reactiveVal()
  solution <- reactiveVal()
  
  observeEvent(input$update, {  # Listen for clicks on the update button
    grid_size(input$gridSize)  # Update grid size
    result <- gameCreate(grid_size(), input$difficulty)
    solution(result$X)  # Store the solution
    #print(solution)
    indications(list(horizontal = result$matindichori, vertical = result$matindicvert))  # Update indications
    grid_state(matrix(0, nrow = grid_size(), ncol = grid_size()))  # Initialize grid_state with a blank grid
  })
  
  observeEvent(input$verify, {  # Listen for clicks on the verify button
    print(rot(grid_state()))
    #print(solution()== rot(grid_state()))
    #transform all cells that are 2 to 0
    test<-rot(grid_state())
    test[test == 2] <- 0
    if(all(test==solution())) {
      showNotification("Congratulations! You have solved the game.", type = "message")
    } else {
      # If not all cells are filled, show a warning message
      showNotification("The game is not yet solved. Keep trying!", type = "warning")
    }
  })
  
  output$gridExists <- reactive({
    !is.null(grid_state()) && !is.null(indications())
  })
  
  outputOptions(output, "gridExists", suspendWhenHidden = FALSE)
  
  output$grid <- renderPlot({
    # Draw the grid based on the state of each cell
    if (!is.null(grid_state()) && !is.null(grid_size()) && !is.null(indications())) {
      draw_grid(grid_state(), grid_size(), indications())
    }
  }, res = 100)
  
  observeEvent(input$grid_click, {
    # Update the state of the clicked cell
    if (!is.null(grid_state()) && !is.null(grid_size())) {
      x <- floor(input$grid_click$x)
      y <- floor(input$grid_click$y)
      if(x < 1 || x > grid_size() || y < 1 || y > grid_size()) {
        return()
      }
      current_state <- grid_state()
      if(current_state[x,y]==2){
        current_state[x,y]=-1
      }
      current_state[x, y] <- current_state[x, y]+1
      grid_state(current_state)
      
      # Redraw the grid
      draw_grid(grid_state(), grid_size(), indications())
    }
  })
}

# Run the app
shinyApp(ui = ui, server = server)