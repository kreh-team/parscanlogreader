read_log_into_df_with_params_list <- function(file, params_list, numeric_params) {
  # Filter needed info from raw log, store in a vector of strings
  # lines <- system(paste("grep -E 'loss:.*acc:|Epoch'", file), intern = TRUE)
  # lines <- system(paste("grep -E 'loss:.*acc:|Epoch|\\[CV\\]'", file, "| grep -v 'total'"), intern = TRUE)
  lines <- system(paste("cat ", file, " | tr -d '\\000' | grep -E 'loss:.*acc:|Epoch|\\[CV\\]' | grep -v 'total'"), intern = TRUE)

  # Calculate size of vector
  num_lines <- length(lines)
  num_headers <- grep("Epoch", lines) %>% length()
  num_clean_lines <- num_lines - num_headers

  # We initialize the output vector of strings
  clean_lines <- rep(NA, num_clean_lines)
  count <- 1
  run_count <- 0

  # Loop for processing the lines
  params_str <- NULL
  for (i in 1:num_lines) {
    line <- lines[[i]]
    if (stringr::str_detect(line, "\\[CV\\]")) {
      params_str <- line
    } else if (stringr::str_detect(line, "Epoch")) {
      # Store epoch "header"
      epoch_str <- line
      run_count <- run_count + 1
    } else {
      # Store data/log line
      raw_str <- line

      # Paste and save processed lines
      clean_lines[[count]] <- paste(run_count, "-", params_str, "-", epoch_str, "-", raw_str)
      count <- count + 1
    }
  }

  # Transform vector of strings into data frame
  df <- data.frame(as.list(clean_lines)) %>%
    t() %>%
    as_tibble() %>%
    tibble::remove_rownames()

  # Separate single column into desired columns
  df <- df %>%
    tidyr::separate(V1, c("run", "params", "epoch", "step", "eta", "loss", "accuracy"), sep = "-") %>%
    tidyr::separate(params, params_list, sep = ", ") %>%
    dplyr::mutate_at(vars(params_list), function(x) stringr::str_split(x, "=", simplify = TRUE)[, 2]) %>%
    dplyr::mutate_at(
      numeric_params,
      as.numeric
    )

  return(df)
}
