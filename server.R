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

# Connect to SQLite DB
my_db <- dbConnect(SQLite(), "names_and_movies.sqlite")

shinyServer(function(input, output) {
  
## Name search logic
  # NULL if there are no movies with a major character by the selected name
  movies_exist <- reactiveVal(NULL)
  
  # NULL if there is no SSA data for the selected name
  name_exists <- reactiveVal(NULL)
  
  # Get the variable for the user-inputted name and convert it to lower case.
  my_name <- reactive({
    tolower(input$name_input)
  })
  
  # Output a table of characters with the selected name, what film they are from, and the release
  # year for that film.
  output$movieList <- renderTable({
    movie_list <- movies_by_name(my_name(), my_db)
    if(!is.null(movie_list)) {
      movie_list <- select(movie_list, character, actor, title, year)
      colnames(movie_list) <- c("Character", "Actor", "Movie", "Year")
      movies_exist(TRUE)
      movie_list
    } else {
      movies_exist(NULL)
      NULL
    }
  }, digits = 0)
  
  # If the movie list is empty, output placeholder text.
  output$movieListIsEmpty <- renderText(
    if(is.null(movies_exist())) {
      paste("No movie characters found with the name ", capwords(my_name()), ".", sep = "")
    } else {
      NULL
    }
  )
  
  # Plot the name if it exists in the SSA data, with lines for movie years if they exist
  output$namePlot <- renderPlot({
    if(!is.null(get_baby_name(my_name(), my_db))) {
      name_exists(TRUE)
      plot_name(my_name(), my_db)
    } else {
      name_exists(NULL)
      NULL
    }
  })
  
  # If the name doesn't exist in the SSA data, output placeholder text.
  output$nameDoesNotExist <- renderText(
    if(is.null(name_exists())) {
      paste("Sorry, no babies named", capwords(my_name()), "were found.")
    } else {
      NULL
    }
  )
  
## Movies by year logic
  
  
})
