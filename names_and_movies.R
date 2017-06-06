library("dplyr")
library("tidyr")
library("lubridate")
library("ggplot2")
library("DBI")
library("RSQLite")

# Connect to SQLite DB
db <- dbConnect(SQLite(), "names_and_movies.sqlite")

# Find all characters with a selected name
get_characters <- function(name) {
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
  return(mydata)
}

# Get all SSA data for the selected name
get_babyname <- function(name) {
  name <- trimws(tolower(name))
  mydata <- dbGetQuery(db, 
                       "SELECT * FROM babynames 
                       WHERE name LIKE :a",
                       params = list(a = name))
}


# A wee function that converts the first letter of each word in a string to caps, stolen from
# the docs here: http://stat.ethz.ch/R-manual/R-devel/library/base/html/chartr.html
capwords <- function(s, strict = FALSE) {
  cap <- function(s) paste(toupper(substring(s, 1, 1)),
                           {s <- substring(s, 2); if(strict) tolower(s) else s},
                           sep = "", collapse = " " )
  sapply(strsplit(s, split = " "), cap, USE.NAMES = !is.null(names(s)))
}

# Import blacklist for names column to remove titles, etc
blacklist <- read.table('blacklist.txt', stringsAsFactors = FALSE)$V1 %>%
  tolower()

# A function that takes the 'character' string, splits it on spaces, removes blacklisted words
# or anything containing a period (since those are always title abbreviations) and then returns
# the first remaining word (which is hopefully the character's given name).
# It is not a very clever function but it pretty much works most of the time.
name_cleaner <- function(name) {
  split_name <- strsplit(name, split = " ")[[1]]
  for(word in split_name) {
    word <- tolower(word)
    if(word %in% blacklist | grepl('\\.', word)) {
      split_name <- split_name[-which(split_name == word)]
    }
  }
  if(length(split_name) > 0){
    myname <- split_name[1] %>% trimws() %>% tolower()
    return(myname)
  } else {
    return(NA)
  }
}

## Import and clean movie data

movie_getter <- function(filename) {
  data <- read.csv(filename, stringsAsFactors = FALSE)
  data$release_date <- ymd(data$release_date)
  data$year <- year(data$release_date)
  data <- select(data, id, title, popularity, release_date, year, character0, character1, 
                 character2, character3, character4) %>%
    gather(temp, character, character0:character4) %>%
    select(-temp)
  
  # Get first names of characters
  data$first_name <- apply(select(data, character), 1, name_cleaner)
  
  return(data)
}


## Do stuff with data

# Get a list of movies and release years that have characters with a specific first name
movies_by_name <- function(name, dataset) {
  name <- tolower(name)
  my_movies <- filter(dataset, first_name == name) %>%
    select(character, title, year) %>%
    arrange(desc(year))
  if(length(my_movies$character > 0)) {
    return(my_movies)
  } else {
    return(NULL)
  }
}

movies_by_year <- function(my_year, dataset) {
  if(my_year >= 1966 & my_year <= 2014) {
    mymovies <- filter(dataset, year == my_year) %>%
      select(title, year, character, first_name)
    if(length(mymovies$character > 0)) {
      mymovies$character <- capwords(mymovies$character)
      return(mymovies)
    } else {
      return(NULL)
    }
  } else {
    return(NULL)
  }
}

movies_by_title <- function(my_title, dataset) {
  mymovies <- dataset[agrepl(my_title, dataset$title, ignore.case = TRUE),]
  if(length(mymovies$title) > 0) {
    return(mymovies)
  } else {
    return(NULL)
  }
}


get_baby_name <- function(my_name, dataset) {
  my_name <- tolower(my_name)
  my_babies <- filter(babynames, name == my_name)
  if(length(my_babies$name) > 0) {
    return(my_babies)
  } else {
    return(NULL)
  }
}


plot_name <- function(my_name, baby_data, movie_data) {
  my_movies <- movies_by_name(my_name, movie_data)
  baby_name <- get_baby_name(my_name, baby_data)
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

## Actual data imports

# Import baby names data
babynames <- concat_ssa_data('names')

#Import movie data
movies <- movie_getter('movies_characters.csv')
