
# Define UI
ui <- fluidPage(
  titlePanel("Simple Shiny App with mtcars Dataset"),
  sidebarLayout(
    sidebarPanel(
      # Feature 1: Dropdown menu to select a variable
      # This feature allows users to interactively choose which variable from the dataset they want to analyze.
      selectInput("variable", "Choose a variable:",
                  choices = colnames(mtcars))
    ),
    mainPanel(
      # Feature 2: Display basic statistics of the selected variable
      # This feature provides a quick summary of the selected variable, giving insights like mean and median, which are useful for basic data analysis.
      verbatimTextOutput("summaryStat"),
      
      # Feature 3: Simple plot of the selected variable
      # A histogram is a fundamental visualization tool that helps in understanding the distribution of data points for the selected variable.
      plotOutput("histPlot")
    )
  )
)

# Define server logic
server <- function(input, output) {
  
  output$summaryStat <- renderText({
    var <- input$variable
    data <- mtcars[[var]]
    summaryStats <- paste("Mean:", mean(data, na.rm = TRUE),
                          "Median:", median(data, na.rm = TRUE))
    summaryStats
  })
  
  output$histPlot <- renderPlot({
    var <- input$variable
    data <- mtcars[[var]]
    hist(data, main = paste("Histogram of", var),
         xlab = var, border = "blue", col = "green")
  })
}

# Run the application
shinyApp(ui = ui, server = server)
