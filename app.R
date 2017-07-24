# Understanding the conversion between z-scores and proportions

# This app was written as an interactive exercise to convert test scores to z-scores and calculate proportions. This app displays the normal distribution of a provided mean and standard deviation with two axes, one for the original scores and one for the z-scores. The students can provide either a test score to find the z-score and a proportion (greater than, less than, mean to z), provide a proportion to find the equivalent z-score and test score.

# Code written by William X.Q. Ngiam for PSYC2012 at the University of Sydney

# WXQN started writing this on 18/7/17

# --- #

library(shiny)
library(shinythemes)

ui <- fluidPage(
  
  # Picking out a theme for the user interface
  theme=shinytheme("cerulean"),
  
  # Application title and include explanation text
  titlePanel(h1("Z-scores and Proportions"),
             windowTitle = "Z-scores"),
  
  # Sidebar with controls to input mean and standard deviation of the test distribution and buttons to select the desired proportion area.
  fluidRow(
    column(12,
           textOutput("appText"))
  ),
  br(),
  fluidRow(
    column(12,
      sidebarLayout(
        sidebarPanel(
          numericInput("Mean", "Mean of the test distribution:", 30),
          numericInput("SD", "Standard deviation of the test distribution:", 5),
          numericInput("X", "Test score", 30),
          br(),
          br(),
          numericInput("GuessZ", "Enter a guess for the z-score", value = NULL),
          numericInput("GuessP", "Enter a guess for the p-score", value = NULL,
                       min = 0, max = 1, step = 0.01),
          br(),
          br(),
          helpText(a(href="https://williamngiam.github.io", target ="_blank", "Written by William X. Q. Ngiam")),
          helpText(a(href="https://github.com/WilliamNgiam/Z-scores-ShinyApp", target="_blank", "Get the code for this app"))
        ),

      # Show a plot of the generated distribution in the main panel
        mainPanel(
          tabsetPanel(
            id = "tabs",
            tabPanel("Beyond Z", 
                     br(),
                     radioButtons("Tail", "Desired area:", 
                                  c("Less than" = "lessthan",
                                    "Greater than" = "greaterthan"),
                                  inline = TRUE),
                                  plotOutput("beyondZPlot"),
                     conditionalPanel(condition = "input.tabs == 'BeyondZTab' && input.GuessP !== null && input.GuessZ !== null",
                                      tableOutput("areaBeyondZ")),
                      value = "BeyondZTab"),
            tabPanel("Mean to Z", plotOutput("meanToZPlot"),
                     conditionalPanel(condition = "input.tabs == 'MeanToZTab' && input.GuessP !== null && input.GuessZ !== null",
                                      tableOutput("areaMeanToZ")),
                     value = "MeanToZTab"),
            tabPanel("Quiz")
            )
          )
        )
      )
    )
  )

server <- function(input, output) {
  
  output$appText <- renderText({
    "A Z-score is the number of standard deviations away from the mean a score is."
  })
  
  output$beyondZPlot <- renderPlot({
    
    # Define the inputs for the distribution as requested.
    mean <- input$Mean
    sd <- input$SD
    lower <- mean - 4*sd
    upper <- mean + 4*sd
    xscore <- input$X
    tail <- input$Tail
    
    # Plot the distribution
    par(mar = c(10, 5, 5, 5)) # Need to adjust margin for second axis
    xvalues = lower:upper
    yvalues = dnorm(lower:upper,mean,sd)
    plot(xvalues,
         yvalues,
         type = "l",
         xlab = "Test Scores",
         ylab = "Probability")
    
    # Label x-score on axis
    axis(1,
         at = xscore,
         labels = xscore,
         col = "navyblue",
         tick = "false",
         font = 2)
    
    # Draw line down to axis
    par(xpd=TRUE)
    lines(c(xscore,xscore),
          c(-0.04,dnorm(xscore,mean,sd)),
          lty = 2)
    
    # Add a second axis for the z-distrbution
    par(col.axis = "navyblue")
    axis(1,
         at = c(seq(from = lower,
                    to = upper,
                    by = sd)),
         labels = c(seq(from = (lower-mean)/sd,
                        to = (upper-mean)/sd,
                        by = 1)),
         line = 5,
         col = "navyblue",
         col.ticks = "navyblue")
    mtext("Z Scores",
          side = 1,
          line = 8,
          col = "navyblue")
    
    # Calculate z-score and label on axis
    zscore = (xscore-mean)/sd
    axis(1,
         at = xscore,
         labels = zscore,
         line = 5,
         tick = "false",
         col = "sienna4",
         font = 2)
    
    # Shade area underneath curve according to proportion
    if (tail == "lessthan")
      polygon(c(xvalues[xvalues<=xscore], xscore),  
              c(yvalues[xvalues<=xscore], 0), 
              col = "navyblue")
    else if (tail == "greaterthan")
      polygon(c(xvalues[xvalues>=xscore], xscore),
              c(yvalues[xvalues>=xscore], 0),
              col = "navyblue")
  })
  
  output$areaBeyondZ <- renderTable({
    # Calculate the Z-score
    mean <- input$Mean
    sd <- input$SD
    xscore <- input$X
    zscore = (xscore-mean)/sd
    
    # Calculate the proportion
    tail <- input$Tail
    if (tail == "lessthan")
      area = pnorm(xscore,mean,sd)
    else if (tail == "greaterthan")
      area = pnorm(xscore,mean,sd,lower.tail = FALSE)
    
    Values <- c(zscore,area)
    df <- data.frame(Values,
                     row.names = c("Z-score:","Area under curve:"),
                     check.names = FALSE)
    },
    rownames = TRUE,
    colnames = TRUE,
    striped = TRUE,
    bordered = TRUE)
    
  output$meanToZPlot <- renderPlot({
    
    # Define the inputs for the distribution as requested.
    mean <- input$Mean
    sd <- input$SD
    lower <- mean - 4*sd
    upper <- mean + 4*sd
    xscore <- input$X
    tail <- input$Tail
    
    # Plot the distribution
    par(mar = c(10, 5, 5, 5)) # Need to adjust margin for second axis
    xvalues = lower:upper
    yvalues = dnorm(lower:upper,mean,sd)
    plot(xvalues,
         yvalues,
         type = "l",
         xlab = "Test Scores",
         ylab = "Probability")
    
    # Label x-score on axis
    axis(1,
         at = xscore,
         labels = xscore,
         col = "navyblue",
         tick = "false",
         font = 2)
    
    # Draw line down to axis
    par(xpd=TRUE)
    lines(c(xscore,xscore),
          c(-0.04,dnorm(xscore,mean,sd)),
          lty = 2)
    
    # Add a second axis for the z-distrbution
    par(col.axis = "navyblue")
    axis(1,
         at = c(seq(from = lower,
                    to = upper,
                    by = sd)),
         labels = c(seq(from = (lower-mean)/sd,
                        to = (upper-mean)/sd,
                        by = 1)),
         line = 5,
         col = "navyblue",
         col.ticks = "navyblue")
    mtext("Z Scores",
          side = 1,
          line = 8,
          col = "navyblue")
    
    # Calculate z-score and label on axis
    zscore = (xscore-mean)/sd
    axis(1,
         at = xscore,
         labels = zscore,
         line = 5,
         tick = "false",
         col = "sienna4",
         font = 2)
    
    # Shade area underneath curve according to proportion
    polygon(c(xscore, xscore:mean, mean),  
            c( 0, dnorm(xscore:mean,mean,sd), 0), 
            col = "navyblue")
  })
  
  output$areaMeanToZ <- renderTable({
    # Calculate the Z-score
    mean <- input$Mean
    sd <- input$SD
    xscore <- input$X
    zscore = (xscore-mean)/sd
    
    # Calculate the proportion
    if (zscore<0) {area <- .5 - pnorm(xscore,mean,sd)}
    else  {area <- pnorm(xscore,mean,sd - .5)}
    
    Values <- c(zscore,area)
    df <- data.frame(Values,
                     row.names = c("Z-score:","Area under curve:"),
                     check.names = FALSE)
  },
  rownames = TRUE,
  colnames = TRUE,
  striped = TRUE,
  bordered = TRUE)
}

shinyApp(ui = ui, server = server)