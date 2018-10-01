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
    tidyjson::as.tbl_json() %>%
    tidyjson::gather_keys()

  next_page <- TRUE

  while(next_page){

    page <- page + 1

    message("Retrieving page: ", page)

    # pull data from website
    page_data <- httr::GET(url = paste0(url, "entries?page_size=", page_size, "/&page=", page), config = httr::authenticate(my_username, my_password)) %>%
      httr::content("text")

    # test if it's empty
    if(page_data == "[]"){
      next_page <- FALSE
    } else {
      # make tbl_json
      page_data <- page_data %>%
        tidyjson::as.tbl_json() %>%
        tidyjson::gather_keys()

      # bind to all_entries
      all_entries <- rbind_tbl_json(all_entries, page_data)
    }
  }

  all_entries
}


#' bind json pages together using tidyjson
rbind_tbl_json <- function(x, y) {

  tbl_json(
    bind_rows(x %>% unclass, y %>% unclass),
    c(attr(x, "JSON"), attr(y, "JSON"))
  )
}
