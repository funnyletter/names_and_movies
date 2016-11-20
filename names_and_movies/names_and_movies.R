library(dplyr)
library(tidyr)
library(babynames)
library(lubridate)
library(ggplot2)


# A wee function that converts the first letter of each word in a string to caps, stolen from
# the docs here: http://stat.ethz.ch/R-manual/R-devel/library/base/html/chartr.html
capwords <- function(s, strict = FALSE) {
  cap <- function(s) paste(toupper(substring(s, 1, 1)),
                           {s <- substring(s, 2); if(strict) tolower(s) else s},
                           sep = "", collapse = " " )
  sapply(strsplit(s, split = " "), cap, USE.NAMES = !is.null(names(s)))
}


# Import movie data (includes title, release date, and names of 3 first-billed characters)
movies <- read.csv('movies_characters.csv', stringsAsFactors = FALSE, fileEncoding = "UTF-8")

# Convert date column from character to date type; add release year
movies$release_date <- ymd(movies$release_date)
movies$year <- year(movies$release_date)

# Get rid of irrelevant columns and reorder things so they look nice
movies <- select(movies, id, title, popularity, release_date, year, character0, character1, 
                 character2)

# Gather characters into a single column
movies <- gather(movies, temp, character, character0:character2) %>%
  select(-temp)

# Import blacklist for names column to remove titles, etc
blacklist <- read.table('blacklist.txt', stringsAsFactors = FALSE)$V1 %>%
  tolower()

# Import baby names data
data("babynames")
babynames$percent <- babynames$prop * 100
babynames$name <- tolower(babynames$name)

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

# Get first names of characters
movies$first_name <- apply(select(movies, character), 1, name_cleaner)

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

elsa_movies <- movies_by_name("elsa", movies)
elsa_name <- get_baby_name("elsa", babynames)

elsa_plot <- ggplot(data = elsa_name, aes(x = year, y = percent, color = sex)) +
  geom_line(size = 1) +
  theme_classic() +
  geom_vline(xintercept = elsa_movies$year, alpha = 0.5) +
  scale_x_continuous(breaks = seq(min(elsa_name$year), max(elsa_name$year), 15))


plot_name <- function(my_name, baby_data, movie_data) {
  my_movies <- movies_by_name(my_name, movie_data)
  baby_name <- get_baby_name(my_name, baby_data)
  if(is.null(my_movies) | is.null(baby_name)) {
    return(NULL)
  } else {
    title <- paste("Babies Named", capwords(my_name), sep = " ")
    y_label <- paste("% of Babies Named", capwords(my_name), sep = " ")
    my_plot <- ggplot(data = baby_name, aes(x = year, y = percent, color = sex)) +
      geom_line(size = 1) +
      theme_classic() +
      geom_vline(xintercept = my_movies$year, alpha = 0.5) +
      scale_x_continuous(breaks = seq(min(baby_name$year), max(baby_name$year), 15)) +
      ggtitle(title) +
      xlab("Year") +
      ylab(y_label)
    return(my_plot)
  }
}