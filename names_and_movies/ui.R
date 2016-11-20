library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title
  titlePanel("Movie Character Names and Baby Names"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
       textInput("name_input", "Enter a name:", "Elsa")
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("namePlot"),
      tableOutput("movieList")
    )
  )
))
