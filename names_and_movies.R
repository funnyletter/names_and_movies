library("dplyr")
library("tidyr")
library("lubridate")
library("ggplot2")
library("DBI")
library("RSQLite")


# A wee function that converts the first letter of each word in a string to caps, stolen from
# the docs here: http://stat.ethz.ch/R-manual/R-devel/library/base/html/chartr.html
capwords <- function(s, strict = FALSE) {
  cap <- function(s) paste(toupper(substring(s, 1, 1)),
                           {s <- substring(s, 2); if(strict) tolower(s) else s},
                           sep = "", collapse = " " )
  sapply(strsplit(s, split = " "), cap, USE.NAMES = !is.null(names(s)))
}


# Find all characters with a selected name
movies_by_name <- function(name, db) {
  name <- trimws(tolower(name))
  mydata <- dbGetQuery(db, 
                       "SELECT * FROM cast 
                       JOIN movies ON movie_id = id 
                       WHERE character LIKE :a 
                       OR character LIKE :b 
                       OR character LIKE :c 
                       OR character LIKE :d",
                       params = list(a = paste('%', name, '%'),
                                     b = paste('%', name),
                                     c = paste(name, '%'),
                                     d = name))
  if(dim(mydata)[1] > 0) {
    return(mydata)
  } else {
    return(NULL)
  }
}


# Get all SSA data for the selected name
get_baby_name <- function(name, db) {
  name <- trimws(tolower(name))
  mydata <- dbGetQuery(db, 
                       "SELECT * FROM babynames 
                       WHERE name LIKE :a",
                       params = list(a = name))
  if(dim(mydata)[1] > 0) {
    return(mydata)
  } else {
    return(NULL)
  }
}


movies_by_year <- function(my_year, db) {
  if(my_year >= 1966 & my_year <= 2017) {
    mymovies <- dbGetQuery(db, "SELECT * FROM cast 
                           JOIN movies ON movie_id = id 
                           WHERE year = :a",
                           params = list(a = my_year))
    if(dim(mymovies)[1] > 0) {
      return(mymovies)
    } else {
      return(NULL)
    }
  } else {
    return(NULL)
  }
}

# Get movies + character names by movie title
movies_by_title <- function(title, db) {
  mymovies <- dbGetQuery(db, "SELECT * FROM cast 
                           JOIN movies ON movie_id = id 
                           WHERE title LIKE :a",
                         params = list(a = paste("%", title, "%", sep = "")))
  
  if(dim(mymovies)[1] > 0) {
    return(mymovies)
  } else {
    return(NULL)
  }
}


plot_name <- function(my_name, db) {
  my_movies <- movies_by_name(my_name, db)
  baby_name <- get_baby_name(my_name, db)
  if(is.null(baby_name)) {
    return(NULL)
  } else {
    title <- paste("Babies Named", capwords(my_name), sep = " ")
    y_label <- paste("# of Babies Named", capwords(my_name), sep = " ")
    my_plot <- ggplot(data = baby_name, aes(x = year, y = count, color = sex)) +
      geom_line(size = 1) +
      theme_classic() +
      theme(plot.title = element_text(hjust = 0.5)) +
      scale_x_continuous(breaks = seq(min(baby_name$year), max(baby_name$year), 15)) +
      ggtitle(title) +
      xlab("Year") +
      ylab(y_label)
    
    if(!is.null(my_movies)) {
      my_plot <- my_plot + geom_vline(xintercept = my_movies$year, alpha = 0.5)
    }
    return(my_plot)
  }
}
