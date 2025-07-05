scrape_review_links <- function() {
  # Ask for permission
  url <- "https://www.angrymetalguy.com/category/reviews/"
  session <- bow(url)
  
  # Get every review link on the site
  # It takes a while (~8530 links in total)
  review_links <- session %>%
    get_review_pages() %>%
    mutate(
      page = map(nod, scrape_page),
      links = map(page, get_review_links)
    ) %>%
    unnest(links)
  
  review_links %>%
    mutate(
      n_total = n(),
      n_current = row_number(),
      msg = paste0("(", n_current, "/", n_total, ") ", review_link) # to show where are we on this scraping adventure
    )
}