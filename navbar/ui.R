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
                                textInput("name_input", "Enter a name:", "Elsa")
                              ),
                              
                              # Show a plot of the generated distribution
                              mainPanel(
                                plotOutput("namePlot"),
                                tableOutput("movieList")
                              )
                            ),
                              mainPanel(
                                plotOutput("plot")
                              )
                            ),
                   tabPanel("Component 2"),
                   tabPanel("Component 3")
))