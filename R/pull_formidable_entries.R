#' Pull Formidable Entries
#'
#' @param url website url
#' @param my_username Formidable Username
#' @param my_password Formidable Password
#'
#' @return Returns the Formidable entries as a data frame.
#' @export
pull_formidable_entries <- function(url, my_username, my_password, page_size = 500){

  page <- 1

  message("Retrieving page: ", page)

  all_entries <- httr::GET(url = paste0(url, "entries?page_size=", page_size, "&page=", page), config = httr::authenticate(my_username, my_password)) %>%
    httr::content("text") %>%
    jsonlite::fromJSON()

  next_page <- TRUE

  while(next_page){

    page <- page + 1

    message("Retrieving page: ", page)

    # pull data from website
    page_data <- httr::GET(url = paste0(url, "entries?page_size=", page_size, "&page=", page), config = httr::authenticate(my_username, my_password)) %>%
      httr::content("text") %>%
      jsonlite::fromJSON()

    # test if it's empty
    if(length(page_data) == 0){
      next_page <- FALSE
    } else {
      # make tbl_json
      all_entries <- dplyr::union(all_entries, page_data)
    }
  }

  purrr::map_df(all_entries, collapse_list_item_to_df)
}

# make tibble
collapse_list_item_to_df <- function(x){
  y <- purrr::flatten(x)
  y <- y[names(y)]
  tibble::as_tibble(y)
}
