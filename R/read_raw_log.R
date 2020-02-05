#' Read raw log file
#'
#' @param file Input log file
#' @param params_list Manual list of parameters (optional)
#' @param numeric_params List of numeric parameters (optional)
#'
#' @return Raw log data frame
#' @export
#'
#' @importFrom rlang .data
read_raw_log <- function(file, params_list = NULL, numeric_params = NULL) {

  # Read settings of the search on hyper parameters
  search_settings_raw <- system(paste("grep -E ^Fitting.*folds.*candidates.*fits$", file), intern = TRUE) %>%
    stringr::str_split(" ") %>%
    unlist() %>%
    stringr::str_remove_all("\\,")

  # Names of the settings parameters
  search_settings_names <- search_settings_raw %>%
    .[c(3, 8, 11)] %>%
    paste0("num_", .) %>%
    stringr::str_replace_all("candidate", "model")

  # Names vector of settings parameters values
  search_settings <- search_settings_raw %>%
    .[c(2, 7, 10)] %>%
    as.numeric() %>%
    `names<-`(search_settings_names)

  # Filter needed info from raw log, store in a vector of strings
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
  data <- data.frame(as.list(clean_lines)) %>%
    t() %>%
    dplyr::as_tibble() %>%
    tibble::remove_rownames()

  # Separate single column into desired columns
  data <- data %>%
    # Separate raw data into main columns
    tidyr::separate(
      .data$V1,
      c("run", "params", "epoch", "step", "eta", "loss", "accuracy"),
      sep = "-"
    ) %>%
    # Separate params column into individual parameters
    tidyr::separate(
      col = .data$params,
      into = params_list,
      sep = ", "
    ) %>%
    # Select values of parameters and make sure they are numeric
    dplyr::mutate_at(
      .vars = dplyr::vars(params_list),
      .funs = function(x) stringr::str_split(x, "=", simplify = TRUE)[, 2]
    ) %>%
    dplyr::mutate_at(
      .vars = dplyr::vars(numeric_params),
      .funs = as.numeric
    )

  # Set search settings attribute
  base::attr(data, "search_settings") <- search_settings

  return(data)
}
