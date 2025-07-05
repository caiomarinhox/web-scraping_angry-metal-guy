library(here)
library(polite)
library(rvest)
library(tidyverse)

# Get custom scraping and cleaning functions
source(here("R", "scrape_review_links.R"))
source(here("R", "functions.R"))

# Scrape review links -----------------------------------------------------
review_links <- scrape_review_links()

# It takes a long time to scrape all these links
# So let's save them
review_links %>% 
  write_rds(here("data", "review_links.rds"))

# Scrape review pages -----------------------------------------------------
# To be used interactively
review_links <- here("data", "review_links.rds") %>%
  read_rds()

# Scrape each review page
# Too long to scrape all at once: let's divide in groups of 1000
review_pages <- review_links %>%
  filter(n_current > 7000) %>% 
  mutate(review_raw = map2(
    .x = review_link,
    .y = msg,
    .f = scrape_page
  ))

review_pages %>%
  write_rds(here("data", "review_pages_8000.rds"))

# Bind all review pages in a single data frame 
review_pages_dir <- here("data") %>% 
  dir(full.names = TRUE) %>% 
  as_tibble() %>% 
  filter(str_detect(value, "review_pages")) %>% 
  pull(value)

review_pages <- map(review_pages_dir, read_rds) %>% 
  bind_rows()

# Write to disk
review_pages %>% 
  write_rds(here("data", "review_pages"))
  
#' It took me days to figure out that these scraping functions (`html_elements()` et al.) are NOT vectorized:
#' I kept getting errors while trying `mutate(review_links, review_title = html_elements(h1))` for instance during EDA.
#' Finally, I found [this R-bloggers post](https://www.r-bloggers.com/2018/09/using-purrrs-map-family-functions-in-dplyrmutate/).
#' Which led me to [this AMAZING teaching repo](https://github.com/jennybc/row-oriented-workflows) by Jenny Bryan.

#' Cleaning
reviews <- review_pages %>%
  mutate(review_metadata = map(review_raw, get_review_metadata)) %>% # list-columns are fun!
  unnest(review_metadata) %>% 
  filter(str_detect(review_metadata, "Rating: ")) %>%                # gets only reviews containing a rating
  mutate(
    review_title = map(review_raw, get_review_title),
    review_author = map(review_raw, get_review_author),
    review_date = map(review_raw, get_review_date),
    review_text = map(review_raw, get_review_text),
    review_tags = map(review_raw, get_review_tags)                   # list-columns are fun!
  ) %>%
  unnest(cols = c(review_text, review_author)) %>%
  extract_review_metadata() %>%
  mutate(review_link = str_c("https://www.angrymetalguy.com/", review_link)) %>%
  mutate(
    metadata_length = NULL,
    value = NULL,
    nod = NULL,
    page = NULL,
    n_total = NULL,
    n_current = NULL,
    msg = NULL
  ) %>%
  relocate(review_link, .after = release_date)

# Sometimes, you just have to leave a dataset dirty and export it

#' Saving the data
reviews %>%
  write_rds(here("data", "reviews_4000.rds"))
