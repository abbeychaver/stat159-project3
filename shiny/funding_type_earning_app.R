# advertising scatterplots

palette(c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3",
          "#FF7F00", "#FFFF33", "#A65628", "#F781BF", "#999999"))

library(shiny)
library(ggplot2)
income = read.csv("../data/Income.csv")

ui <- fluidPage(

  # Title for the page
  headerPanel('Earnings Gap VS School Funding'),
  sidebarPanel(

    # Dropdown for user to select either TV, Radio, or Newspaper as x variable
    selectInput('school_type', 'School Type', c("Public" = 1, "Private Nonprofit" = 2, "Private For-Profit" = 3))
  ),
  #sidebarPanel(

    # Dropdown for user to select either TV, Radio, or Newspaper as x variable
    #textInput(inputId = "school_name", label="School Name", value="John")
  #),

  # Adds panel that shows the plotted output
  mainPanel(
    plotOutput('plot')
  )
)

server <- function(input, output) {

  # Grabs only the selected x column from above and Sales from advertising
  selectedData <- reactive({
    income[income$CONTROL==input$school_type, c("INEXPFTE", "gap_earnings_high_low")]
  })

  # Plots Sales vs the selected x column and saves it as reactive output object
  output$plot <- renderPlot({
    par(mar = c(5.1, 4.1, 0, 1))
    plot(selectedData(),
         pch = 20, cex = 2)
  })
}

shinyApp(ui = ui, server = server)
