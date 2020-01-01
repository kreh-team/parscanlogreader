summarise_log_data_with_params_list <- function(data, params_list) {
  data <- data %>%
    # Get last step of each single run
    group_by_at(vars(c("run", "epoch", params_list))) %>%
    slice(n()) %>%
    # Divide epoch into current and max epoch
    mutate(
      curr_epoch = stringr::str_split(epoch, "/") %>% unlist %>% .[1] %>% as.numeric(),
      max_epoch = stringr::str_split(epoch, "/") %>% unlist %>% .[2] %>% as.numeric(),
    ) %>%
    ungroup() %>%
    # Get final loss/accuracy of each epoch
    dplyr::filter(curr_epoch == max_epoch) %>%
    select(-c(step, epoch, run, curr_epoch)) %>%
    dplyr::rename(epochs = max_epoch) %>%
    mutate(epochs = as.factor(epochs)) %>%
    # Create model variable (5 runs)
    tibble::rowid_to_column(var = "run") %>%
    mutate(
      model = cut(run, breaks = seq(0,1000,5), label = 1:200)
    ) %>%
    select(-run) %>%
    # Summarise results
    group_by_at(vars(c("model", "epochs", params_list))) %>%
    summarise(
      loss_mean = mean(loss),
      loss_sd = sd(loss),
      acc_mean = mean(accuracy),
      acc_sd = sd(accuracy)
    )

  return(data)
}
