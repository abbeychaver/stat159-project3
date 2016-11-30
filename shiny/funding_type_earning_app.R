# earnings gap scatterplots

palette(c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3",
          "#FF7F00", "#FFFF33", "#A65628", "#F781BF", "#999999"))

library(shiny)
income = read.csv("../data/Income.csv")

ui <- fluidPage(

  # Title for the page
  headerPanel('Earnings Gap VS School Funding'),
  sidebarPanel(

    width = 5,
    # Dropdown for user to select which school type to plot
    selectInput('school_type', 'School Type', c("Public" = 1, "Private Nonprofit" = 2, "Private For-Profit" = 3))
  ),
  sidebarPanel(

    width = 5,
    # Dropdown for user to select which earnings gap between income levels to use as y variable
    selectInput('gap_earnings', 'Gap in Earnings Between', c("Highest and Lowest Income Terciles" = "gap_earnings_high_low", "Highest and Middle Income Terciles" = "gap_earnings_high_mid", "Middle and Lowest Income Terciles" = "gap_earnings_mid_low"))
  ),

  # Adds panel that shows the plotted output
  mainPanel(
    width = 10,
    plotOutput('plotAll')
  )
)

server <- function(input, output) {

  # Grabs only the rows corresponding to the school type selected above
  # Includes the expenditure per student column and the gap in earnings column selected above
  selectedData <- reactive({
    income[income$CONTROL==input$school_type, c("INEXPFTE", input$gap_earnings)]
  })

  # Plots selected earnings gap vs expenditure per student and saves it as reactive output object
  output$plotAll <- renderPlot({
    par(mar = c(5.1, 4.1, 0, 1))
    plot(selectedData(),
         pch = 20, cex = 0.8,
         ylim=c(-0.7, 0.7),
         xlim=c(0, 65000),
         ylab="Percent Gap in Earnings",
         xlab="Expenditure Per Student ($)"
        )
    abline(lm(selectedData()[,2] ~ selectedData()$INEXPFTE),  col = "coral2")
  })
}

shinyApp(ui = ui, server = server)
