## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

library(tidyverse)
library(dontpanic)


## ------------------------------------------------------------------------
con <- DBI::dbConnect(
  RSQLite::SQLite(), # DB backend .
  path = ":memory:" # Temporary in-memory db.
  )

## ------------------------------------------------------------------------
copy_to(
  con, # Specifies object to copy data to.
  nycflights13::flights, # df from an R package
  "flights",
  temporary = FALSE, 
  indexes = list( 
    # Is this akin to group in tidy and key in data.table?
    c("year", "month", "day"), 
    "carrier", 
    "tailnum",
    "dest"
  )
)

# Note that we're hacking an R package df into a RDBMS.
nycflights13::flights %>% 
  head() %>% 
  knitr::kable()

## ------------------------------------------------------------------------
# Hmm, I don't quite under why this is necessary.
flights_db <- tbl(con, "flights")

# Anyway, now we have a tibble-like object, but the source is different.
flights_db %>% head() %>% knitr::kable()

## ------------------------------------------------------------------------
flights_db %>%
  show_query() # Display equivalent SQL.

# Now, say we were interested in some kind of more complex manipulation.
delays <- flights_db %>% 
  group_by(carrier, origin) %>% 
  summarise(avg_dep_delay = mean(dep_delay),
            avg_arr_delay = mean(arr_delay))

# Take a look.
delays %>% 
  knitr::kable()

# In SQL.
delays %>% 
  show_query()


## ------------------------------------------------------------------------
# browseVignettes("etudes")

