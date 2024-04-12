library(shiny)
rotate90 <- function(mat) {
  return(t(mat[, ncol(mat):1]))
}

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
  
  if (difficulty == "Hard") {
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
      
    }
  }
  
  
  return(list(matindichori = matindichori, matindicvert = matindicvert, X = X, compteurH = compteurH, compteurV = compteurV))
}

# Function to draw the grid
draw_grid <- function(grid_state, gridSize, indications) {
  
  plot(1:gridSize, 1:gridSize, type = "n", xlab = "", ylab = "", xaxt = 'n', yaxt = 'n', xlim = c(-gridSize*0.10, gridSize+1), ylim = c(1, gridSize*1.5))
  
}


ui <- fluidPage(
  titlePanel("Picross"),
  tags$style(type="text/css", "#controls { width: 60%; margin: 0 auto; }"),
  fluidRow(
    column(4,
           div(id = "controls",
               wellPanel(
                 sliderInput("gridSize", "Grid size", min = 5, max = 15, value = 5),
                 selectInput("difficulty", "difficultyiculty", choices = c("Easy", "Medium", "Hard"), selected = "Medium"),
                 actionButton("update", "Create game", style = "background-color: red; color: white;"),  # Add the update button with red background
                 actionButton("verify", "Verify", style = "background-color: green; color: white;")  # Add the solve button with green background
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
                 tags$h1("Welcome to Picross!", style = "color: #3399FF;"),
                 tags$p("Please select the parameters and create the game.", style = "font-size: 20px;")
               )
             )
           )
    )
  )
)





# Run the app
shinyApp(ui = ui, server = server)