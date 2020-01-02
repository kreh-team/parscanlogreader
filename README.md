
# parscanlogreader <!-- <img src="man/figures/logo.png" align="right" width="120" /> -->

<!-- badges: start -->

<!-- badges: end -->

## Overview

The goal of parscanlogreader is to read and process raw log files from
Scikit-learn’s RandomizedSearchCV.

## Example

This is a basic example which shows you how the data pipeline works:

``` r
library(parscanlogreader)

src_file <- "logs/cnn-gru-scan.log"
src_params_list <- c(
  "optimizers", "opt_recurrent_regs", "opt_kernel_regs", "opt_go_backwards",
  "opt_dropout_recurrent", "opt_dropout", "maxpool_size", "kernel_size",
  "gru_hidden_units", "filter_conv", "epochs_raw", "batch_size", "activation_conv"
)
src_numeric_params <- c(
  "opt_dropout_recurrent", "opt_dropout", "maxpool_size", "kernel_size",
  "gru_hidden_units", "filter_conv", "epochs_raw", "batch_size"
)
```

``` r
log_data_raw <- src_file %>%
  read_log_into_df_with_params_list(
    params_list = src_params_list, 
    numeric_params = src_numeric_params
  ) %>%
  clean_log_df_with_params()

log_data <- log_data_raw %>%
  tidyr::drop_na() %>%
  summarise_log_data_with_params_list(
    params_list = src_params_list
    runs_per_model = 5, 
    max_runs = 1000
  )
```

## Installation

<!-- You can install the released version of parscanlogreader from [CRAN](https://CRAN.R-project.org) with: -->

<!-- ``` r -->

<!-- install.packages("parscanlogreader") -->

<!-- ``` -->

<!-- And  -->

The development version from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("kreh-team/parscanlogreader")
```
