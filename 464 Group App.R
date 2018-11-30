library(shiny)

# Define UI for data upload app ----
ui <- fluidPage(
  
  # App title ----
  titlePanel("KS and Wilcoxon Test for 2 Groups"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      # Horizontal line ----
      tags$hr(),
      
      # Input: Select separator ----
      radioButtons("samp1", "Sample 1 Type",
                   choices = c(Normal = "norm",
                               Expoential = "exp",
                               Binoimial = "binom"),
                   selected = "norm"),
      
      # Input: Select quotes ----
      radioButtons("samp2", "Sample 2 Type",
                   choices = c(Normal = "norm",
                               Exponential = "exp",
                               Binomial = "binom"),
                   selected = "norm"),
      
      # Horizontal line ----
      tags$hr(),
      
      # Input: Select number of rows to display ----
      radioButtons("nsize", "Sample Size",
                   choices = c(Head = "head",
                               All = "all"),
                   selected = "head")
      
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: Data file ----
      tableOutput("contents"),
      
      # Output: Boxplots ----
      plotOutput("boxplots"),
      
      # Output: Histogram Permutation Test ----
      plotOutput("perm_test_plot"),
      
      # Output: P-value Permutation Test ----
      textOutput("p_value")
      
    )
    
  )
)

# Define server logic to read selected file ----
server <- function(input, output) {
  
  # input$file1 will be NULL initially. After the user selects
  # and uploads a file, head of that data file by default,
  # or all rows if selected, will be shown.
  
  data <- reactive({
    
    req(input$file1)
    
    df <- read.csv(input$file1$datapath,
                   header = input$header,
                   sep = input$sep,
                   quote = input$quote)[,-1]
    
    if(input$disp == "head") {
      
      return(head(df))
      
    } else {
      
      return(df)
      
    }
    
  })
  
  output$contents <- renderTable({
    
    data()
    
  })
  
  output$boxplots <- renderPlot({
    
    boxplot(data(), col = "grey80")
    
  })
  
  
  # Make Permutation Test for the Median

  
  output$perm_test_plot <- renderPlot({

    var1 <- data()[!is.na(data()[,1]), 1]
    var2 <- data()[!is.na(data()[,2]), 2]
    delta_obs <- median(var2) - median(var1)
    
    n1 <- length(var1)
    combined_results <- c(var1, var2)
    permutations_1 <- t(combn(combined_results, n1))
    permutations_2 <- t(apply(permutations_1, 1, function(x, a) a[!a %in% x], a = combined_results))
    
    delta_perm <-  apply(permutations_1, 1, median) - apply(permutations_2, 1, median)
    
    hist(delta_perm, col = "grey80")
    abline(v = delta_obs, col = "red")
    
  })
  
  output$p_value <- renderText({
    
    var1 <- data()[!is.na(data()[,1]), 1]
    var2 <- data()[!is.na(data()[,2]), 2]
    delta_obs <- median(var2) - median(var1)
    
    n1 <- length(var1)
    combined_results <- c(var1, var2)
    permutations_1 <- t(combn(combined_results, n1))
    permutations_2 <- t(apply(permutations_1, 1, function(x, a) a[!a %in% x], a = combined_results))
    
    delta_perm <-  apply(permutations_1, 1, median) - apply(permutations_2, 1, median)
    
    pval_perm <- round(sum(delta_perm >= delta_obs) / length(delta_perm), 3)
    
    paste("The p-value for the permutation test is", pval_perm)
    
  })
  
}
# Run the app ----
shinyApp(ui, server)