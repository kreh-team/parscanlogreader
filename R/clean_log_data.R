#' Clean raw log data frame
#'
#' @param data Raw log data frame
#'
#' @return Clean log data frame
#' @export
#'
#' @importFrom rlang .data
clean_log_data <- function(data) {

  # Use regex for getting the relevant content of each raw column
  data <- data %>%
    dplyr::mutate(
      epoch = stringr::str_extract(.data$epoch, "[0-9]*/[0-9]*"),
      step = stringr::str_extract(.data$step, "[0-9]*/[0-9]*"),
      loss = stringr::str_extract(.data$loss, "[0-9]*\\.[0-9]*"),
      accuracy = stringr::str_extract(.data$accuracy, "[0-9]*\\.[0-9]*")
    )

  # Change data types and remove useless column
  data <- data %>%
    dplyr::mutate(
      run = as.numeric(.data$run),
      loss = as.numeric(.data$loss),
      accuracy = as.numeric(.data$accuracy)
    ) %>%
    dplyr::select(-.data$eta, -.data$epochs_raw)

  return(data)
}
