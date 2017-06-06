library("RSQLite")
library("babynames")
library("dplyr")
library("lubridate")

### This script loads data from CSVs into SQLite. It only needs to be run when new data is added to the
### movie or baby names data sets.

### FUTURE PLANS: Hit tMDB API directly instead of pulling down data in Python and then pulling it into
### SQLite.

# Open SQLite DB connection
db <- dbConnect(SQLite(), db = "names_and_movies.sqlite")

## Import data from SSA text files into R
# Reads and cleans an individual SSA data file
read_ssa_data <- function(filename) {
  data <- read.csv(filename, header = FALSE, stringsAsFactors = FALSE)
  colnames(data) <- c("name", "sex", "count")
  data$year <- as.numeric(gsub("\\D", "", filename))
  return(data)
}

# Aggregate all SSA files in a directory into a single data frame
concat_ssa_data <- function(directory) {
  all_files <- dir(directory)
  agg <- data.frame()
  for(file in all_files) {
    agg <- rbind(agg, read_ssa_data(paste(directory, "/", file, sep = "")))
  }
  return(agg)
}


# Load data for baby names
babynames <- as.data.frame(concat_ssa_data('names'))

# Load and clean data for movies and cast lists
movies <- read.csv("movies.csv", stringsAsFactors = FALSE) %>%
  select(id, title, popularity, release_date) %>%
  mutate(release_date = mdy(release_date), year = year(release_date))

cast <- read.csv("cast.csv", stringsAsFactors = FALSE)

# Write data to SQLite DB tables. Note that this will overwrite existing tables, NOT append to them.
dbWriteTable(conn = db, name = "babynames", value = babynames, overwrite = TRUE)
dbWriteTable(conn = db, name = "movies", value = movies, overwrite = TRUE)
dbWriteTable(conn = db, name = "cast", value = cast, overwrite = TRUE)

dbDisconnect(db)
