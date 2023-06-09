library(shiny)
library(mowateR)
data(eml)

vars = setdiff(names(eml), c("Depth", "Date.Time"))
depths_list = unique(eml$Depth)

# Define UI ----
ui <- fluidPage(
  
  # App title ----
  titlePanel("Plotting EML Data"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      selectInput(inputId = "xAxis", label = "X Axis",
                  choices = vars,
                  selected = "Temp"),
      selectInput(inputId = "yAxis", label = "Y Axis",
                  choices = vars,
                  selected = "DO"),
      selectInput(inputId = "depth", label = "Depth",
                  choices = depths_list,
                  selected = depths_list[1])
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      plotOutput("plot1")
      
      
    )
  )
)

# Define server logic ----
server <- function(input, output) {
  
  output$plot1 = renderPlot({
    eml_depth = eml[which(eml$Depth==input$depth),]
    plot(eml_depth[,input$xAxis], eml_depth[,input$yAxis],
         xlab = input$xAxis,
         ylab = input$yAxis,
         main = paste(input$yAxis, "vs", input$xAxis))
  })
  
}

# Run the app ----
shinyApp(ui = ui, server = server)