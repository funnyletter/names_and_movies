library(RSQLite)

db <- dbConnect(SQLite(), "names_and_movies.sqlite")
