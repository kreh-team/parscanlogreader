#' Summarise clean log data frame
#'
#' @param data Clean log data frame
#' @param params_list Manual list of parameters
#'
#' @return Log summary data frame
#' @export
#'
#' @importFrom rlang .data
summarise_log_data <- function(data, params_list) {
  data <- data %>%
    # Get last step of each single run
    dplyr::group_by_at(
      dplyr::vars(c("run", "epoch", params_list))
    ) %>%
    dplyr::slice(dplyr::n()) %>%
    # Divide epoch into current and max epoch
    dplyr::mutate(
      curr_epoch = stringr::str_split(.data$epoch, "/") %>% unlist %>% .[1] %>% as.numeric(),
      max_epoch = stringr::str_split(.data$epoch, "/") %>% unlist %>% .[2] %>% as.numeric(),
    ) %>%
    dplyr::ungroup() %>%
    # Get final loss/accuracy of each epoch
    dplyr::filter(.data$curr_epoch == .data$max_epoch) %>%
    dplyr::select(-c(.data$step, .data$epoch, .data$run, .data$curr_epoch)) %>%
    dplyr::rename(epochs = .data$max_epoch) %>%
    dplyr::mutate(epochs = as.factor(.data$epochs)) %>%
    # Create model variable (5 runs)
    tibble::rowid_to_column(var = "run") %>%
    dplyr::mutate(
      model = cut(.data$run, breaks = seq(0, 1000, 5), label = 1:200)
    ) %>%
    dplyr::select(-.data$run) %>%
    # Summarise results
    dplyr::group_by_at(
      dplyr::vars(c("model", "epochs", params_list))
    ) %>%
    dplyr::summarise(
      loss_mean = mean(.data$loss),
      loss_sd = stats::sd(.data$loss),
      acc_mean = mean(.data$accuracy),
      acc_sd = stats::sd(.data$accuracy)
    )

  return(data)
}
