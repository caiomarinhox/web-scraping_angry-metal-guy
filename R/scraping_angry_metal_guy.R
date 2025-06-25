library(here)
library(polite)
library(rvest)
library(tidyverse)

# Get custom scraping and cleaning functions
source(here("R", "functions.R"))

#' Scraping
# Ask for permission
url <- "https://www.angrymetalguy.com/category/reviews/"
session <- bow(url)

# Get every review link on the site
review_pages <- session %>% 
  get_review_pages(n_pages = 5) %>% 
  mutate(
    page = map(nod, scrape_page),
    links = map(page, get_review_links)
  ) %>% 
  unnest(links)

# Scrape each review page
review_links <- review_pages %>%
  mutate(review_raw = map(review_link, scrape_page))

#' It took me days to figure out that these scraping functions (`html_elements()` et al.) are NOT vectorized:
#' I kept getting errors while trying `mutate(review_links, review_title = html_elements(h1))` for instance during EDA.
#' Finally, I found [this R-bloggers post](https://www.r-bloggers.com/2018/09/using-purrrs-map-family-functions-in-dplyrmutate/).
#' Which led me to [this AMAZING teaching repo](https://github.com/jennybc/row-oriented-workflows) by Jenny Bryan.

#' Cleaning
reviews <- review_links %>%
  mutate(
    review_metadata = map(review_raw, get_review_metadata), # list-columns are fun!
    review_title = map(review_raw, get_review_title),
    review_author = map(review_raw, get_review_author),
    review_text = map(review_raw, get_review_text),
    review_tags = map(review_raw, get_review_tags) # list-columns are fun!
  ) %>%
  mutate(metadata_length = map_dbl(review_metadata, length)) %>% # identifies reviews with no metadata
  filter(metadata_length > 0) %>% # excludes reviews with no metadata
  unnest(cols = c(review_text, review_author)) %>%
  extract_review_metadata() %>%
  separate_wider_delim(
    cols = review_title,
    delim = " – ",
    names = c("band_name", "album_name")
  ) %>% 
  mutate(review_link = str_c("https://www.angrymetalguy.com/", review_link)) %>% 
  mutate(
    metadata_length = NULL,
    value = NULL,
    nod = NULL,
    page = NULL,
    review_raw = NULL,
    review_metadata = NULL
  ) %>% 
  relocate(review_link, .after = release_date)
  
#' Saving the data
reviews %>%
  write_rds(here("data", "reviews.rds"))