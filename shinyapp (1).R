#--------------------------------------------------------------
#- Filename: shinyapp.R
#- Author: Evelyn Pan
#- Date: 11/5/20
#- Description: simple shiny app
#--------------------------------------------------------------

#- [0] dependencies --------------------------------------------------
library(shiny)
#- get EML data
load("dat/eml.rda")
eml <- eml[1:8400,]

#- [1] user interface: what the app looks like --------------------------------------------------
ui <- fluidPage(
  
  # App title ----
  titlePanel("Eagle Mountain Lake Plotter"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    sidebarPanel(
      
      # Input: Variables
      selectInput("var",
                  "What Variable are you interested in seeing?",
                  c("DO Sat" = "DOsat", 
                    "Temperature" = "Temp",
                    "Conductivity" = "Cond",
                    "pH" = "pH")),
      
      # Input: Slider
      sliderInput(inputId = "obs",
                  label = "How many observations would you like to see?",
                  min = 1,
                  max = 400,
                  value = 5),
      # Input: checkbox
      checkboxInput("mean_fun", 
                    "Show mean function", 
                    TRUE)
      
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: Plot ----
      plotOutput(outputId = "distPlot")
      
    )
  )
)


#- [2] server: defines inputs and outputs ---------------------------------------------------
server <- function(input, output) {
  
  output$distPlot <- renderPlot({
    
    data <- unlist(c(dplyr::select(eml, input$var)))
    Depth <- seq(0, 10, length.out = 21)
    xdata <- data[1:21]
    sd <- sd(xdata)
    minimum <- min(xdata) - (6*sd)
    maximum <- max(xdata) + (6*sd)
    plot(xdata, Depth, type = "l", ylim = rev(range(Depth)),
         col = rgb(0,0,0.5,alpha = .3), xlab = input$var,
         xlim = c(minimum, maximum))
    
    for (i in 2:input$obs) {
      end   <- i * 21
      start <- end - 20
      xdata <- data[start:end]
      lines(xdata, Depth, col = rgb(0,0,0.5,alpha = .2))
    }
    
    if (input$mean_fun == TRUE) {
    matt <- matrix(ncol = 21, nrow = input$obs)
    for (i in 1:input$obs) {
      end   <- i * 21
      start <- end - 20
      matt[i, 1:21] <- data[start:end]
    }
    mean_vals <- apply(matt, 2, mean)
    lines(mean_vals, Depth, lwd = 3)
    }
    
  })
  
}

#- [3] employ shiny app --------------------------------------------------
shinyApp(ui, server)

