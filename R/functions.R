scrape_page <- function(.x) {
  # For interactive use: to show which page is being scraped
  message(.x)
  
  session %>%
    nod(path = .x) %>%
    scrape()
}

get_review_pages <- function(session, n_pages = NULL) {
  total_number_of_pages <- session %>%
    scrape() %>%
    html_elements("a") %>%
    html_attrs_dfr() %>% 
    filter(class == "page-numbers") %>% 
    mutate(.text = as.numeric(.text)) %>% 
    filter(.text == max(.text)) %>% 
    pull(.text)
  
  if (is.null(n_pages)) {
    n_pages = total_number_of_pages
  }
  
  # Returns a tibble of every review link on the site
  # or the number indicated by n_pages
  seq_along(1:total_number_of_pages) %>% 
    as_tibble() %>% 
    filter(value <= n_pages) %>% 
    mutate(nod = paste0("/page/", value))
}

get_review_links <- function(page) {
  # For interactive use: to show which page is being scraped
  message(page)
  
  page %>%
    html_elements("a") %>%
    html_attrs_dfr() %>%
    filter(class == "post-thumbnail") %>%
    select(href) %>%
    mutate(href = str_remove(href, "https://www.angrymetalguy.com/")) %>%
    rename(review_link = href)
}

get_review_title <- function(scraped_page) {
  scraped_page %>%
    html_elements("h1") %>%
    html_attrs_dfr() %>%
    pull(.text) %>%
    str_remove(" Review")
}

get_review_author <- function(scraped_page) {
  scraped_page %>%
    html_elements("a") %>%
    html_attrs_dfr() %>%
    filter(str_detect(href, "author")) %>%
    pull(.text)
}

get_review_text <- function(scraped_page) {
  scraped_page %>%
    html_elements("p") %>%
    html_attrs_dfr() %>%
    filter(str_detect(style, "justify")) %>%
    pull(.text) %>%
    str_c() %>%
    str_flatten()
}

get_review_tags <- function(scraped_page) {
  scraped_page %>%
    html_elements("a") %>%
    html_attrs_dfr() %>%
    filter(rel == "tag") %>%
    pull(.text)
}

get_review_metadata <- function(scraped_page) {
  # There are a number of data grouped in a single p html element (e.g. Rating, DR, Label).
  # I opted to get them all at once with this function, them extract each one via a mutate() call
  scraped_page %>%
    html_elements("p") %>%
    html_attrs_dfr() %>%
    filter(str_detect(style, "center")) %>%
    pull(.text)
}

#' This pipeline section could've been left in the cleaning section as-is.
#' But I bundled it in a single function because it's mostly the same pattern (extract metadata, then work with strings).
extract_review_metadata <- function(reviews) {
  reviews %>%
    mutate(
      review_rating = str_extract(review_metadata, "Rating:.*DR"),
      review_rating = str_remove(review_rating, "DR"),
      review_rating = str_remove(review_rating, "Rating: ")
    ) %>%
    mutate(
      review_dr = str_extract(review_metadata, "DR:.* \\| Format"),
      review_dr = str_remove(review_dr, " \\| Format"),
      review_dr = str_remove(review_dr, "DR: "),
    ) %>%
    mutate(
      review_format = str_extract(review_metadata, "Format.*Label"),
      review_format = str_remove(review_format, "Label"),
      review_format = str_remove(review_format, "Format Reviewed: "),
      review_format = str_remove(review_format, "Formats Reviewed: "),
      review_format = str_replace(review_format, " \\|", ",")
    ) %>%
    mutate(
      label = str_extract(review_metadata, "Label:.*Website"),
      label = str_remove(label, "Website"),
      label = str_remove(label, "Websites"), # I'm sure there's a way to extract both forms in one go, but I'm just starting on regex.
      label = str_remove(label, "Label: "),
      label = str_replace(label, " \\|", ",")
    ) %>%
    mutate(
      websites = str_extract(review_metadata, "Websites:.*\\|"),
      websites = str_remove(websites, " \\|$"),
      websites = str_remove(websites, " \\|"),
      websites = str_remove(websites, "Websites: "),
      websites = str_replace(websites, " ", ", ")
    ) %>%
    mutate(
      release_date = str_extract(review_metadata, "Releases Worldwide.*"),
      release_date = str_remove(release_date, "Releases Worldwide: ")
    )
}