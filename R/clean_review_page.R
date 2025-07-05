clean_review_page <- function(review_page) {
  review_page %>% 
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
}
