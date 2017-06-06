#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
shinyUI(navbarPage("My Application",
                   tabPanel("Plot Baby Names",
                            sidebarLayout(
                              sidebarPanel(
                                textInput("name_input", "Enter a name:", "Elsa"),
                                submitButton("Go")
                              ),
                              
                              # Show a plot of the generated distribution
                              mainPanel(
                                plotOutput("namePlot"),
                                textOutput("nameDoesNotExist"),
                                tableOutput("movieList"),
                                textOutput("movieListIsEmpty")
                              )
                            )
                   ),
                   tabPanel("Movies by Year"),
                   tabPanel("Movies by Title")
))