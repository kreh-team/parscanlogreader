#' Summarise clean log data frame
#'
#' @param data Clean log data frame
#' @param params_list Manual list of parameters (optional)
#' @param num_folds Number of folds per model/candidate (optional)
#' @param num_models Number of models/candidates (optional)
#'
#' @return Log summary data frame
#' @export
#'
#' @importFrom rlang .data
summarise_log_data <- function(data, params_list = NULL, num_folds = NULL, num_models = NULL) {

  # Read search settings
  search_settings <- attr(data, "search_settings")

  # Parse num_folds and models from
  if (any(is.null(c(num_folds, num_models)))) {
    num_folds <- search_settings[["num_folds"]]
    num_models <- search_settings[["num_models"]]
  }

  # Read params_list from data if not specified
  if (is.null(params_list)) {
    params_list <- dplyr::setdiff(
      colnames(data),
      c("run", "epoch", "step", "loss", "accuracy")
    )
  }

  data <- data %>%
    # Get last step of each single run
    dplyr::group_by_at(
      dplyr::vars(c("run", "epoch", params_list))
    ) %>%
    dplyr::slice(dplyr::n()) %>%
    # Divide epoch column into current and max epoch
    tidyr::separate(
      col = .data$epoch,
      into = c("curr_epoch", "max_epoch"),
      sep = "/",
      remove = FALSE
    ) %>%
    dplyr::mutate_at(
      .vars = dplyr::vars(.data$curr_epoch, .data$max_epoch),
      .funs = as.numeric
    ) %>%
    dplyr::ungroup() %>%
    # Get final loss/accuracy of each epoch
    dplyr::filter(.data$curr_epoch == .data$max_epoch) %>%
    dplyr::select(-c(.data$step, .data$epoch, .data$run, .data$curr_epoch)) %>%
    dplyr::mutate(epochs = as.factor(.data$max_epoch)) %>%
    # Split folds into separate models
    tibble::rowid_to_column(var = "fold") %>%
    dplyr::mutate(
      model = cut(
        x = .data$fold,
        breaks = seq(0, num_folds * num_models, num_folds),
        label = 1:as.numeric(num_models)
      )
    ) %>%
    dplyr::select(-.data$fold) %>%
    # Summarise results
    dplyr::group_by_at(
      .vars = dplyr::vars(c("model", "epochs", params_list))
    ) %>%
    dplyr::summarise(
      loss_mean = mean(.data$loss),
      loss_sd = stats::sd(.data$loss),
      acc_mean = mean(.data$accuracy),
      acc_sd = stats::sd(.data$accuracy)
    )

  # Set search settings attribute
  attr(data, "search_settings") <- search_settings

  return(data)
}
