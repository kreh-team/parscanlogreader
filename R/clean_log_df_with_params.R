clean_log_df_with_params <- function(data) {

  # Use regex for getting the relevant content of each raw column
  data <- data %>%
    dplyr::mutate(
      epoch = stringr::str_extract(epoch, "[0-9]*/[0-9]*"),
      step = stringr::str_extract(step, "[0-9]*/[0-9]*"),
      loss = stringr::str_extract(loss, "[0-9]*\\.[0-9]*"),
      accuracy = stringr::str_extract(accuracy, "[0-9]*\\.[0-9]*")
    )

  # Change data types and remove useless column
  data <- data %>%
    mutate(
      run = as.numeric(run),
      loss = as.numeric(loss),
      accuracy = as.numeric(accuracy)
    ) %>%
    select(-eta)

  return(data)
}
