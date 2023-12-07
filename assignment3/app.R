# Define UI
ui <- fluidPage(
  titlePanel("Enhanced Shiny App with mtcars Dataset"),
  sidebarLayout(
    sidebarPanel(
      selectInput("variable", "Choose a variable:",
                  choices = colnames(mtcars)),
      hr(),
      checkboxGroupInput("cylFilter", "Filter by Cylinders:",
                         choices = unique(mtcars$cyl)),
      sliderInput("gearFilter", "Select Gear Range:",
                  min = min(mtcars$gear), max = max(mtcars$gear),
                  value = c(min(mtcars$gear), max(mtcars$gear)),
                  step = 1)
    ),
    mainPanel(
      verbatimTextOutput("summaryStat"),
      selectInput("plotType", "Select Plot Type:",
                  choices = c("Histogram" = "hist", "Boxplot" = "box")),
      plotOutput("customPlot")
    )
  )
)

# Define server logic
server <- function(input, output) {
  
  filteredData <- reactive({
    data <- mtcars
    if (!is.null(input$cylFilter) && !length(input$cylFilter) == 0) {
      data <- data[data$cyl %in% input$cylFilter, ]
    }
    data <- data[data$gear >= input$gearFilter[1] & data$gear <= input$gearFilter[2], ]
    data
  })
  
  output$summaryStat <- renderText({
    var <- input$variable
    data <- filteredData()[[var]]
    summaryStats <- paste("Mean:", mean(data, na.rm = TRUE),
                          "Median:", median(data, na.rm = TRUE),
                          "Standard Deviation:", sd(data, na.rm = TRUE),
                          "Variance:", var(data, na.rm = TRUE),
                          "Range:", paste(range(data, na.rm = TRUE), collapse = "-"))
    summaryStats
  })
  
  output$customPlot <- renderPlot({
    var <- input$variable
    data <- filteredData()[[var]]
    if(input$plotType == "hist") {
      hist(data, main = paste("Histogram of", var),
           xlab = var, border = "blue", col = "green")
    } else if(input$plotType == "box") {
      boxplot(data, main = paste("Boxplot of", var),
              ylab = var, col = "orange")
    }
  })
}

# Run the application
shinyApp(ui = ui, server = server)
