#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
source('names_and_movies.R')

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  my_name <- reactive({
    tolower(input$name_input)
  })
  
  output$noOutput <- renderText("Sorry, I don't have any data for that name.")
  output$movieList <- renderTable({
    movie_list <- movies_by_name(my_name(), movies)
    if(!is.null(movie_list)) {
      colnames(movie_list) <- c("Character", "Movie", "Year")
      movie_list
    } else {
      NULL
    }

  }, digits = 0)
  
  output$namePlot <- renderPlot({
      plot_name(my_name(), babynames, movies)
  })
  
})
