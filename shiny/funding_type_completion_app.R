# completion gap scatterplots

palette(c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3",
          "#FF7F00", "#FFFF33", "#A65628", "#F781BF", "#999999"))

library(shiny)
completion_w_a = read.csv("../data/Completion_W_A.csv")
completion_w_b = read.csv("../data/Completion_W_B.csv")
completion_w_h = read.csv("../data/Completion_W_H.csv")

ui <- fluidPage(

  # Title for the page
  headerPanel('Completion Rate Gap VS School Funding'),
  sidebarPanel(

    width = 5,
    # Dropdown for user to select which school type to plot
    selectInput('school_type', 'School Type', c("Public" = 1, "Private Nonprofit" = 2, "Private For-Profit" = 3))
  ),
  sidebarPanel(

    width = 5,
    # Dropdown for user to select which completion gap between whites and other minorities to use as y variable
    selectInput('gap_completion', 'Gap in Completion Rate Between', c("Whites and Asians" = "gap_completion_white_asian", "Whites and Blacks" = "gap_completion_white_black", "Whites and Hispanics" = "gap_completion_white_hispanic"))
  ),

  # Adds panel that shows the plotted output
  mainPanel(
    width = 10,
    plotOutput('plotAll')
  )
)

server <- function(input, output) {

  # Grabs only the rows corresponding to the school type selected above
  # Includes the expenditure per student column and the gap in completion column selected above
  selectedData <- reactive({
    if (input$gap_completion == "gap_completion_white_asian") {
      completion_w_a[completion_w_a$CONTROL==input$school_type, c("INEXPFTE", input$gap_completion)]
    } else if (input$gap_completion == "gap_completion_white_black") {
      completion_w_b[completion_w_b$CONTROL==input$school_type, c("INEXPFTE", input$gap_completion)]
    } else {
      completion_w_h[completion_w_h$CONTROL==input$school_type, c("INEXPFTE", input$gap_completion)]
    }
  })

  # Plots selected completion gap vs expenditure per student and saves it as reactive output object
  output$plotAll <- renderPlot({
    par(mar = c(5.1, 4.1, 0, 1))
    plot(selectedData(),
         pch = 20, cex = 0.8,
         ylim=c(-7, 1),
         xlim=c(0, 65000),
         ylab="Percent Gap in Completion",
         xlab="Expenditure Per Student ($)"
    )
    abline(lm(selectedData()[,2] ~ selectedData()$INEXPFTE),  col = "coral2")
    dev.off()
  })
}

shinyApp(ui = ui, server = server)
