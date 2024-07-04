library(shiny)
library(ggplot2)
library(gridExtra)

# Define the UI
ui <- fluidPage(tags$head(tags$style(HTML("
    .title-panel {
      text-align: center;
    }
  "))),
                div(class = "title-panel",
                    titlePanel("Contribution of each polynomial term to the total function")
                ),
  sidebarLayout(
    sidebarPanel(
      h4("Coefficients"),
      sliderInput("a", withMathJax("Intercept"), min = -250, max = 250, value = 1.94),
      sliderInput("b", withMathJax("Linear (\\(x\\)):"), min = -250, max = 250, value = -71.99),
      sliderInput("c", withMathJax("Quadratic (\\(x^2\\)):"), min = -250, max = 250, value = -80.94),
      sliderInput("d", withMathJax("Cubic (\\(x^3\\)):"), min = -250, max = 250, value = 18.47	),
      sliderInput("e", withMathJax("Quartic (\\(x^4\\)):"), min = -250, max = 250, value = 0),
      width = 3
    ),
    mainPanel(
      plotOutput("polyPlots")
    )
  )
)

# Define the server logic
server <- function(input, output) {
  output$polyPlots <- renderPlot({
    x <- seq(250, 400, by = 1)
    t <- poly(x, 4)
    y_intercept <- rep(input$a, length(x))
    y_linear <- input$b*t[,1]
    y_quadratic <- input$c*t[,2]
    y_cubic <- input$d*t[,3]
    y_quartic <- input$e*t[,4]
    y_total <- rep(input$a, length(x)) + 
      input$b*t[,1] + 
      input$c*t[,2] + 
      input$d*t[,3] + 
      input$e*t[,4]
    
    # Create data frames for each plot
    df_intercept <- data.frame(x, y = y_intercept)
    df_linear <- data.frame(x, y = y_linear)
    df_quadratic <- data.frame(x, y = y_quadratic)
    df_cubic <- data.frame(x, y = y_cubic)
    df_quartic <- data.frame(x, y = y_quartic)
    df_total <- data.frame(x, y = y_total)
    
    # Create plots
    p_intercept <- ggplot(df_intercept, aes(x, y)) + geom_line(color = "blue") +
      ggtitle("Intercept") + theme_minimal(base_size = 14) + coord_cartesian(ylim = c(-250,250)) +
      xlab("timebin")+
      ylab("pupil size")
    
    p_linear <- ggplot(df_linear, aes(x, y)) + geom_line(color = "red") +
      ggtitle("Linear") + theme_minimal(base_size = 14) + coord_cartesian(ylim = c(-30,30)) +
        xlab("timebin") +
      ylab("pupil size")
    
    p_quadratic <- ggplot(df_quadratic, aes(x, y)) + geom_line(color = "green") +
      ggtitle("Quadratic") + theme_minimal(base_size = 14)+ coord_cartesian(ylim = c(-30,30)) +
      xlab("timebin") +
      ylab("pupil size")
    
    p_cubic <- ggplot(df_cubic, aes(x, y)) + geom_line(color = "purple") +
      ggtitle("Cubic") + theme_minimal(base_size = 14) + coord_cartesian(ylim = c(-30,30)) +
      xlab("timebin") +
      ylab("pupil size")
    
    p_quartic <- ggplot(df_quartic, aes(x, y)) + geom_line(color = "orange") +
      ggtitle("Quartic") + theme_minimal(base_size = 14)+ coord_cartesian(ylim = c(-30,30)) + 
      xlab("timebin") +
      ylab("pupil size")
    
    p_total <- ggplot(df_total, aes(x, y)) + geom_line(color = "black") +
      ggtitle("Total Function") + theme_minimal(base_size = 14) + xlab("timebin") +
      ylab("pupil size")
    
    # Arrange the plots in a grid
    grid.arrange(p_intercept, p_linear, p_quadratic, p_cubic, p_quartic, p_total, ncol = 3)
  }, height = 700)  
}

# Run the app
shinyApp(ui = ui, server = server)
