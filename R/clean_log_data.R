#' Clean raw log data frame
#'
#' @param data Raw log data frame
#' @param drop_na Drop rows containing missing values
#'
#' @return Clean log data frame
#' @export
#'
#' @importFrom rlang .data
clean_log_data <- function(data, drop_na = FALSE) {
  # Read search settings
  search_settings <- attr(data, "search_settings")

  # Drop NA columns
  if (drop_na) {
    data <- data %>%
      tidyr::drop_na()
  }

  # Use regex for getting the relevant content of each raw column
  data <- data %>%
    dplyr::mutate_at(
      .vars = dplyr::vars(.data$epoch, .data$step),
      .funs = function(x) stringr::str_extract(x, "[0-9]*/[0-9]*")
    ) %>%
    dplyr::mutate_at(
      .vars = dplyr::vars(.data$loss, .data$accuracy),
      .funs = function(x) stringr::str_extract(x, "[0-9]*\\.[0-9]*")
    )

  # Change data types and remove useless columns
  data <- data %>%
    dplyr::mutate_at(
      .vars = dplyr::vars(.data$run, .data$loss, .data$accuracy),
      .funs = as.numeric
    ) %>%
    dplyr::select(-.data$eta, -.data$epochs)

  # Set search settings attribute
  attr(data, "search_settings") <- search_settings

  return(data)
}
