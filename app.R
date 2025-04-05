library(shiny)
library(ggplot2)
library(dplyr)
library(DT)
library(shinyjs)
library(fontawesome)

# UI
ui <- fluidPage(
  useShinyjs(),
  
  titlePanel(
    div(
      style = "display: flex; justify-content: center; align-items: center;",
      icon("beer"),
      strong("Beer Data Analysis"),
      icon("beer")
    )
  ),
  
  helpText("Explore beer IBU and ABV by state using histograms, boxplots, scatterplots, and more."),
  
  sidebarLayout(
    sidebarPanel(
      # file inputs
      fileInput("Beers", "Choose Beers CSV", accept = ".csv"),
      fileInput("Breweries", "Choose Breweries CSV", accept = ".csv"),
      
      # filter by state (populated after upload)
      uiOutput("stateUI"),
      
      # choose plot variable
      selectInput("yvar", "Select Variable to Plot:", choices = c("IBU", "ABV")),
      
      # plot type
      radioButtons("plotType", "Choose Plot Type:",
                   choices = c("Histogram" = "hist", "Boxplot" = "box")),
      
      # toggle regression line
      checkboxInput("showReg", "Add Regression Line (IBU vs ABV)", FALSE),
      
      # help and socials
      actionButton("helpBtn", "Help", icon = icon("question-circle")),
      br(), br(),
      actionButton("githubBtn", label = "GitHub", icon = icon("github")),
      actionButton("twitterBtn", label = "Twitter", icon = icon("twitter")),
      actionButton("facebookBtn", label = "Facebook", icon = icon("facebook"))
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Main Plots",
                 plotOutput("histBoxPlot"),
                 plotOutput("scatterPlot")),
        tabPanel("Data Table",
                 DTOutput("dataTable"))
      )
    )
  )
)

# Server
server <- function(input, output, session) {
  
  # read in Beers
  beerData <- reactive({
    req(input$Beers)
    read.csv(input$Beers$datapath, stringsAsFactors = FALSE)
  })
  
  # read in Breweries
  breweryData <- reactive({
    req(input$Breweries)
    read.csv(input$Breweries$datapath, stringsAsFactors = FALSE)
  })
  
  # merge on Brewery_id and Brew_ID
  combinedData <- reactive({
    df <- merge(beerData(), breweryData(), by.x = "Brewery_id", by.y = "Brew_ID")
    df
  })
  
  # update state choices based on uploaded data
  output$stateUI <- renderUI({
    req(combinedData())
    selectInput("stateInput", "Select State:",
                choices = c("All States", sort(unique(combinedData()$State))),
                selected = "All States")
  })
  
  #filter based on selected state
  filteredData <- reactive({
    df <- combinedData()
    if (!is.null(input$stateInput) && input$stateInput != "All States") {
      df <- df %>% filter(State == input$stateInput)
    }
    df
  })
  
  #plot histogram or boxplot
  output$histBoxPlot <- renderPlot({
    req(filteredData())
    df <- filteredData()
    yvar <- input$yvar
    
    if (input$plotType == "hist") {
      ggplot(df, aes_string(x = yvar)) +
        geom_histogram(binwidth = ifelse(yvar == "ABV", 0.01, 1),
                       fill = "blue", color = "black") +
        labs(title = paste("Histogram of", yvar), x = yvar, y = "Count") +
        theme_minimal()
    } else {
      ggplot(df, aes_string(y = yvar)) +
        geom_boxplot(fill = "blue") +
        labs(title = paste("Boxplot of", yvar), y = yvar, x = "") +
        theme_minimal()
    }
  })
  
  #plot scatterplot of IBU vs ABV
  output$scatterPlot <- renderPlot({
    req(filteredData())
    df <- filteredData()
    
    p <- ggplot(df, aes(x = IBU, y = ABV)) +
      geom_point(alpha = 0.6, color = "blue") +
      labs(title = "Scatter Plot of IBU vs ABV", x = "IBU", y = "ABV") +
      theme_minimal()
    
    if (input$showReg) {
      p <- p + geom_smooth(method = "lm", se = FALSE, color = "red")
    }
    
    p
  })
  
  
  # render data table
  output$dataTable <- renderDT({
    req(filteredData())
    datatable(filteredData(), options = list(pageLength = 10))
  })
  
  # help button
  observeEvent(input$helpBtn, {
    showModal(modalDialog(
      title = "Help Information",
      "To use this app:\n\n- Upload a CSV file with beer data.\n- Choose between histograms or boxplots for IBU.\n- Filter data by state.\n- Visualize IBU vs. ABV with or without a regression line."
    ))
  })
  

  # social placeholders
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

# Run app
shinyApp(ui, server)
