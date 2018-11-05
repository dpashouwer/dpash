#' Teacher Year-Over-Year Movement
#'
#' @param year1
#' @param year2
#'
#' @return
#' @export
#'
#' @examples
year_over_year_movement <- function(year1, year2){

  # sub function works on a single pair of strings
  movement_single <- function(year1, year2){
    if(is.na(year1)){
      "not present"
    } else if(is.na(year2)){
      "Not Retained"
    } else if (year1 == year2){
      "Retained in School"
    } else {
      "Retained in District"
    }
  }

  # call the subfunction on a pair of input vectors - this makes it vectorized
  out <- purrr::map2_chr(year1, year2, movement_single)
  out[length(out)] <- "unknown"
  out

}
