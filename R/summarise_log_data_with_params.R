summarise_log_data_with_params_list <- function(data, params_list) {
  data <- data %>%
    # Get last step of each single run
    dplyr::group_by_at(
      dplyr::vars(c("run", "epoch", params_list))
    ) %>%
    dplyr::slice(dplyr::n()) %>%
    # Divide epoch into current and max epoch
    dplyr::mutate(
      curr_epoch = stringr::str_split(epoch, "/") %>% unlist %>% .[1] %>% as.numeric(),
      max_epoch = stringr::str_split(epoch, "/") %>% unlist %>% .[2] %>% as.numeric(),
    ) %>%
    dplyr::ungroup() %>%
    # Get final loss/accuracy of each epoch
    dplyr::filter(curr_epoch == max_epoch) %>%
    dplyr::select(-c(step, epoch, run, curr_epoch)) %>%
    dplyr::rename(epochs = max_epoch) %>%
    dplyr::mutate(epochs = as.factor(epochs)) %>%
    # Create model variable (5 runs)
    tibble::rowid_to_column(var = "run") %>%
    dplyr::mutate(
      model = cut(run, breaks = seq(0,1000,5), label = 1:200)
    ) %>%
    dplyr::select(-run) %>%
    # Summarise results
    dplyr::group_by_at(
      dplyr::vars(c("model", "epochs", params_list))
    ) %>%
    dplyr::summarise(
      loss_mean = mean(loss),
      loss_sd = stats::sd(loss),
      acc_mean = mean(accuracy),
      acc_sd = stats::sd(accuracy)
    )

  return(data)
}
