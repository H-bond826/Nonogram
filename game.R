library(shiny)
gameCreate <- function(n, difficulty) {
  #Initialisation des variables
  p <- 0.666  # la probabilité que chaque cellule soit remplie aléatoirement
  y <- 0
  ind1 <- c() #destinés à stocker respectivement les indices 1(lignes) et 2(colonnes).
  ind2 <- c()
  compteurH <- rep(0, n) #enregistrent le nombre d'indices pour chaque ligne et chaque colonne
  compteurV <- rep(0, n)
  matindichori <- matrix(data = 0, nrow = n, ncol = floor(n/2) + 1)
  matindicvert <- matrix(data = 0, nrow = floor(n/2) + 1, ncol = n)
  matrice <- rbinom(n * n, 1, p)
  X <- matrix(matrice, n, byrow = TRUE)
  
  if (difficulty == "Easy") {
    random_integer1 <- sample(1:n, 1)
    random_integer2 <- sample(1:n, 1)
    
    X[random_integer1, ] <- 1
    X[, random_integer2] <- 1
    
  }
  
  if (difficulty == "Expert") {
    a <- n + 1
    while(a == n + 1) {
      a <- 0
      matrice <- rbinom(n * n, 1, p)
      X <- matrix(matrice, n, byrow = TRUE)
      
      # Pour les lignes horizontales
      for (i in 1:n) {
        for(j in 1:n) {
          if (X[i, j] == 1) {
            y <- y + 1
          }
          if (X[i, j] == 0 & y != 0) {
            ind1 <- append(ind1, y)
            y <- 0
            compteurH[i] <- compteurH[i] + 1
          }
        }
        if (y != 0) {
          ind1 <- append(ind1, y)
          y <- 0
          compteurH[i] <- compteurH[i] + 1
        }
        while (length(ind1) < floor(n/2) + 1) {
          ind1 <- append(ind1, NA)
        }
        
        print(a)
        if (a < sum(ind1, na.rm = TRUE) + compteurH[i]) {
          print("hello")
          print(compteurH)
          print(ind1)
          a <- sum(ind1, na.rm = TRUE) + compteurH[i]
        }
        
        matindichori[i, ] <- ind1
        matindichori[is.na(matindichori)] <- ""
        ind1 <- c()
      }
      # Pour les colonnes verticales
      X_t = t(X) # Transposer la matrice
      
      for (i in 1:n) {
        for(j in 1:n)
        {
          if (X_t[i,j]==1)
          {
            y=y+1
          }
          if (X_t[i,j]==0 & y!=0)
          {
            ind2=append(ind2,y)
            y=0
            compteurV[i]=compteurV[i]+1
          }
        }
        if (y!=0)
        {
          ind2=append(ind2,y)
          y=0
          compteurV[i]=compteurV[i]+1
        }
        while (length(ind2)<floor(n/2)+1) 
        {
          ind2=append(ind2,NA)
          
        }
        
        matindicvert[,i]=ind2
        matindicvert[is.na(matindicvert)] <- ""
        ind2=c()
        if (a < sum(ind2, na.rm = TRUE) + compteurV[i]) {
          a <- sum(ind2, na.rm = TRUE) + compteurV[i]
        }
      }
    }
  }
  
  else{       
    for (i in 1:n) {
      for(j in 1:n) {
        if (X[i, j] == 1) {
          y <- y + 1
        }
        if (X[i, j] == 0 & y != 0) {
          ind1 <- append(ind1, y)
          y <- 0
          compteurH[i] <- compteurH[i] + 1
        }
      }
      if (y != 0) {
        ind1 <- append(ind1, y)
        y <- 0
        compteurH[i] <- compteurH[i] + 1
      }
      while (length(ind1) < floor(n/2) + 1) {
        ind1 <- append(ind1, NA)
      }
      
      matindichori[i, ] <- ind1
      matindichori[is.na(matindichori)] <- ""
      ind1 <- c()
    }   
    
    X_t = t(X) # Transposer la matrice
    for (i in 1:n) {
      for(j in 1:n)
      {
        if (X_t[i,j]==1)
        {
          y=y+1
        }
        if (X_t[i,j]==0 & y!=0)
        {
          ind2=append(ind2,y)
          y=0
          compteurV[i]=compteurV[i]+1
        }
      }
      if (y!=0)
      {
        ind2=append(ind2,y)
        y=0
        compteurV[i]=compteurV[i]+1
      }
      while (length(ind2)<floor(n/2)+1) 
      {
        ind2=append(ind2,NA)
        
      }
      
      matindicvert[,i]=ind2
      matindicvert[is.na(matindicvert)] <- ""
      ind2=c()
      
    }
  }
  
  return(list(matindichori = matindichori, matindicvert = matindicvert, X = X, compteurH = compteurH, compteurV = compteurV))
}

# Fonction draw the grid
draw_grid <- function(grid_state, gridSize, indications) {
  
  plot(1:gridSize, 1:gridSize, type = "n", xlab = "", ylab = "", xaxt = 'n', yaxt = 'n', xlim = c(-gridSize*0.10, gridSize+1), ylim = c(1, gridSize*1.2), asp = 1, bty = "n")
  for(x in 1:gridSize) {
    for(y in 1:gridSize) {
      rect(x, y, x + 1, y + 1, col = ifelse(grid_state[x, y] == 1, "blue", "white"), border = "black", lwd = 1)
      
      if(grid_state[x, y] == 2) {
        offset = 0.1  # Define an offset
        lines(c(x + offset, x + 1 - offset), c(y + offset, y + 1 - offset), col = "red", lwd = 2)
        lines(c(x + 1 - offset, x + offset), c(y + offset, y + 1 - offset), col = "red", lwd = 2)
      }
    }
  }
  # Define margins to accommodate the indications
  par(mar = c(0, gridSize, 0, 0))
  for (i in 1:gridSize) {
    for (j in floor(gridSize/2):1) {
      text(x = i+0.5, y = gridSize+1.2+j*(gridSize/28+0.1), paste(indications$vertical[(gridSize/2)-j+1, i], collapse = " "), adj = c(1, 1), font = 2, cex=1.7/log(gridSize))
      
    }
    text(x = 1, y = gridSize-i+1.7, paste(indications$horizontal[i,], collapse = " "), adj = c(1, 1), font = 2, cex=1.7/log(gridSize))
  }
}
rotate90 <- function(mat) {
  return(t(mat[, ncol(mat):1]))
}

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



# Server
server <- function(input, output, session) {
  # Reactive values for the grid size, grid state, indications and counters
  grid_size <- reactiveVal()
  grid_state <- reactiveVal()
  indications <- reactiveVal()
  solution <- reactiveVal()
  
  observeEvent(input$update, { #generate
    grid_size(input$gridSize)  #ajuste la taille de la grille du jeu en fonction de la sélection de l'utilisateur via un curseur.
    result <- gameCreate(grid_size(), input$difficulty) #crée une nouvelle instance du jeu avec la taille de grille et la difficulté spécifiées. Cette fonction retourne probablement une liste contenant l'état initial du jeu et des indices.
    solution(result$X)  #obtenir X
    indications(list(horizontal = result$matindichori, vertical = result$matindicvert))  # Update indications
    grid_state(matrix(0, nrow = grid_size(), ncol = grid_size()))  # Initialization 
  })
  
  observeEvent(input$verify, {  #vérifie si la grille modifiée correspond à la solution stockée
    test<-rotate90(grid_state())
    test[test == 2] <- 0
    if(all(test==solution())) { 
      showNotification("Congratulations!", type = "message")
    } else {
      showNotification("There are mistakes in the puzzle solution.", type = "warning")
    }
  })
  
  output$gridExists <- reactive({
    !is.null(grid_state()) && !is.null(indications())
  })
  
  outputOptions(output, "gridExists", suspendWhenHidden = FALSE)
  
  output$grid <- renderPlot({
    if (!is.null(grid_state()) && !is.null(grid_size()) && !is.null(indications())) {
      draw_grid(grid_state(), grid_size(), indications())
    }
  }, res = 100)
  
  observeEvent(input$grid_click, {
    if (!is.null(grid_state()) && !is.null(grid_size())) {#vérifier que grid_state() et grid_size() ont été correctement initialisés 
      x <- floor(input$grid_click$x)
      y <- floor(input$grid_click$y)
      if(x < 1 || x > grid_size() || y < 1 || y > grid_size()) {#Si les coordonnées sont hors des dimensions réelles de la grille, la fct ne fait rien et se termine.
        return()
      }
      current_state <- grid_state()
      if(current_state[x,y]==2){
        current_state[x,y]=-1
      }
      current_state[x, y] <- current_state[x, y]+1
      grid_state(current_state)
      draw_grid(grid_state(), grid_size(), indications())
    }
  })
}

shinyApp(ui, server )