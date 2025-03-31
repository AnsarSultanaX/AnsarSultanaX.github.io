library(shiny)
library(ggplot2)
library(dplyr)
library(shinyjs)
library(DT)

# Define UI for application
ui <- fluidPage(
  titlePanel(
    div(
      style = "display: flex; justify-content: center; align-items: center;",
      icon("beer"),
      strong("Beer Data Analysis"),
      icon("beer")
    )
  ),
  
  helpText("This app lets you explore beer data with histograms, boxplots, scatterplots, and more."),
  
  sidebarLayout(
    sidebarPanel(
      fileInput("Beers", "Choose the CSV File with your beers", accept = ".csv"),
      fileInput("Breweries", "Choose the CSV File with your breweries", accept = ".csv"),
      
      uiOutput("stateUI"),
      radioButtons("plotType", "Choose Plot Type:",
                   choices = c("Histogram" = "hist", "Boxplot" = "box")),
      checkboxInput("regressionLine", "Show Regression Line", value = FALSE),
      
      # Social media and GitHub share buttons (use images for the logos)
      div(
        style = "display: flex; justify-content: space-between; margin-top: 20px;",
        actionButton("githubBtn", label = "GitHub", icon = icon("github")),
        actionButton("twitterBtn", label = "Twitter", icon = icon("twitter")),
        actionButton("facebookBtn", label = "Facebook", icon = icon("facebook"))
      ),
      
      # Help button
      shinyjs::useShinyjs(),
      actionButton("helpBtn", "Help", icon = icon("question-circle"))
    ),
    
    mainPanel(
      plotOutput("mainPlot"),
      plotOutput("scatterPlot"),
      DTOutput("dataTable")
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  
  # Reactive to read the data
  beerData <- reactive ({ 
    req(input$Beers)
    read.csv(input$Beers$datapath)
  })
  
  BreweriesData <- reactive ({ 
    req(input$Breweries)
    read.csv(input$Breweries$datapath)
  })
  
  CombinedData <- reactive({
    merge(beerData(), BreweriesData(), by.x="Breweries_id", by.y="Brew_id")
  })
  
  CombinedStateData <- reactive({
    data <- CombinedData()
    if (!is.null(input$FilterByState)) {
      data <- data[data$State == input$FilterByState, ]
    }
    data
  })
  
  output$stateUI<- renderUI({
    req(CombinedData())
    selectInput("FilterByState","Select State", choices = unique(CombinedData()$State), selected = unique(CombinedData()$State)[1])
  })
  
  # Plot for Histogram or Boxplot
  output$mainPlot <- renderPlot({
    CombinedStateData <- CombinedData()
    if (!is.null(input$FilterByState)){
      CombinedStateData <-CombinedStateData[CombinedStateData$State == input$FilterByState,]
    }
    
    # Choose between Histogram and Boxplot
    if (input$plotType == "hist") {
      ggplot(CombinedStateData, aes(x = IBU)) +
        geom_histogram(binwidth = 1, fill = "blue", color = "black") +
        labs(title = "IBU Histogram", x = "IBU", y = "Frequency")
    } else {
      ggplot(CombinedStateData, aes(x = "", y = IBU)) +
        geom_boxplot(fill = "blue", color = "black") +
        labs(title = "IBU Boxplot", y = "IBU")
    }
  })
  
  # Scatter plot of IBU vs ABV with regression line
  output$scatterPlot <- renderPlot({
    req(CombinedStateData())
    p <- ggplot(CombinedStateData(), aes(x = IBU, y = ABV)) +
      geom_point(aes(color = state)) +
      labs(title = "IBU vs ABV Scatterplot", x = "IBU", y = "ABV")
    
    # Add regression line if checkbox is selected
    if (input$regressionLine) {
      p <- p + geom_smooth(method = "lm", color = "red")
    }
    
    print(p)
  })
  
  # Render Data Table
  output$dataTable <- renderDT({
    req(CombinedStateData())
    datatable(CombinedStateData())
  })
  
  # Help button functionality
  observeEvent(input$helpBtn, {
    showModal(modalDialog(
      title = "Help Information",
      "To use this app:\n\n- Upload a CSV file with beer data.\n- Choose between histograms or boxplots for IBU.\n- Filter data by state.\n- Visualize IBU vs. ABV with or without a regression line."
    ))
  })
  
  # GitHub, Twitter, and Facebook buttons (these would be placeholders)
  observeEvent(input$githubBtn, {
    showModal(modalDialog(title = "GitHub", "Redirecting to GitHub..."))
  })
  observeEvent(input$twitterBtn, {
    showModal(modalDialog(title = "Twitter", "Redirecting to Twitter..."))
  })
  observeEvent(input$facebookBtn, {
    showModal(modalDialog(title = "Facebook", "Redirecting to Facebook..."))
  })
}

# Run the application
shinyApp(ui = ui, server = server)

install.packages("rsconnect")

rsconnect::setAccountInfo(name='ansarsultanacamara',
                          token='AB374C3FC37F0369F02454F1794B815A',
                          secret='DhNfmbgLIaOfZWHw1THX4plEE870YmTdcNzMDH1g')

library(rsconnect)
rsconnect::deployApp()
