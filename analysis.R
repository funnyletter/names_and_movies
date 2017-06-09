source('names_and_movies.R')

my_db <- dbConnect(SQLite(), "names_and_movies.sqlite")

movies <- dbGetQuery(my_db,
                     "SELECT * FROM cast
                     JOIN movies ON movie_id = id")

babynames <- dbGetQuery(my_db,
                        "SELECT * FROM babynames")

# Takes a multi-word character name and movie release year; searches for each word in the babynames list.
# Returns a dataframe containing matched movie/cast and babyname data
name_matcher <- function(name, year) {
  my_data <- data.frame()
  name_segments <- strsplit(name, " ")
  for(word in name_segments) {
    mydata <- dbGetQuery(my_db, "SELECT * FROM babynames
               WHERE name LIKE :x
               AND year = :y",
               params = list(x = word, y = year))
    mydata <- mydata %>% 
      group_by(name, year) %>% 
      summarize(count = sum(count))
    prev_2_years <- dbGetQuery(my_db, "SELECT year, SUM(count) FROM babynames
                              WHERE (year = :y OR year = :z)
                              AND name like :x
                              GROUP BY year",
                              params = list(x = word, 
                                            y = year - 1,
                                            z = year - 2))
    colnames(prev_2_years) <- c("year", "count")
    
    next_2_years <- dbGetQuery(my_db, "SELECT year, SUM(count) FROM babynames
                              WHERE (year = :y OR year = :z)
                               AND name like :x
                               GROUP BY year",
                               params = list(x = word, 
                                             y = year,
                                             z = year + 1))
    colnames(next_2_years) <- c("year", "count")
    
    diff_before <- prev_2_years$count[2] / prev_2_years$count[1] * 100
    diff_after <- next_2_years$count[2] / next_2_years$count[1] * 100
  }
}


# What % of movie characters have a name in the top 1000 names for that year?

# Is this name in a list of top names for the given year? 
is_top_name <- function(name, top_names_list) {
  is_top <- FALSE
  for(word in strsplit(name, " ", fixed = TRUE)[[1]]) {
    if(tolower(word) %in% top_names_list) {
      is_top <- TRUE
    }
  }
  return(is_top)
}


# How many names in movie cast lists are one of the top names for its year?
count_top_names <- function(rank_threshold) {
  my_df <- data.frame()
  for(year in 1966:2016){
    top_names <- dbGetQuery(my_db, "SELECT name FROM babynames
                            WHERE year = :x
                            AND rank <= :y",
                            params = list(x = year, y = rank_threshold))
    top_names <- tolower(top_names$name)
    my_movies <- dbGetQuery(my_db, "SELECT * FROM movies
                            JOIN cast ON id = movie_id
                            WHERE year = :x",
                            params = list(x = year))
    percent_top_names <- sum(apply(select(my_movies, character), 1, is_top_name, top_names)) /
      length(my_movies$character) * 100
    my_df <- rbind(my_df, data.frame(year = year, percent_top_names = percent_top_names))
  }
  return(my_df)
}

# How many names in movie cast lists have never been in the top 1000 names, ever?
count_unusual_names <- function() {
  my_df <- data.frame()
  unique_names <- dbGetQuery(my_db, "SELECT DISTINCT(name) FROM babynames")$name %>%
    tolower()
  for(year in 1966:2016){
    my_movies <- dbGetQuery(my_db, "SELECT * FROM movies
                            JOIN cast ON id = movie_id
                            WHERE year = :x",
                            params = list(x = year))
    percent_unique_names <- (1 - (sum(apply(select(my_movies, character), 1, is_top_name, unique_names)) /
      length(my_movies$character))) * 100
    my_df <- rbind(my_df, data.frame(year = year, percent_unique_names = percent_unique_names))
  }
  return(my_df)
}