############################# Apply Lowpassfilter ##############################
apply_low_pass_filter <- function(data_list) {
  filtered_data <- lapply(data_list, function(df) {
    bf <- butter(3, 0.04, type = "low")
    df$vo2_abs_filt <- signal::filtfilt(bf, df$vo2_abs)
    df$vo2_rel_filt <- signal::filtfilt(bf, df$vo2_rel)

    df$vco2_filt <- signal::filtfilt(bf, df$vco2)

    df$ve_filt <- signal::filtfilt(bf, df$ve)

    df$vevo2_filt <- signal::filtfilt(bf, df$vevo2)

    df$vevco2_filt <- signal::filtfilt(bf, df$vevco2)
    df
  })

  return(filtered_data)
}

apply_moving_average_filter <- function(data_list, k) {
  filtered_data <- lapply(data_list, function(df, ...) {
    f <- rep(1 / k, k)
    df$vo2_abs_filt <- stats::filter(df$vo2_abs, f,
      method = "convolution",
      sides = 2, circular = TRUE
    )

    df$vo2_rel_filt <- stats::filter(df$vo2_rel, f,
      method = "convolution",
      sides = 2, circular = TRUE
    )

    df$vco2_filt <- stats::filter(df$vco2, f,
      method = "convolution",
      sides = 2, circular = TRUE
    )

    df$ve_filt <- stats::filter(df$ve, f,
      method = "convolution",
      sides = 2, circular = TRUE
    )

    df$vevo2_filt <- stats::filter(df$vevo2, f,
      method = "convolution",
      sides = 2, circular = TRUE
    )

    df$vevco2_filt <- stats::filter(df$vevco2, f,
      method = "convolution",
      sides = 2, circular = TRUE
    )
    df
  })
  return(filtered_data)
}

######################## Compute Ventilatory Variables #########################
compute_ventilatory_vars <- function(data_list) {
  data_list <- lapply(data_list, function(df) {
    df$exco2 <- (((df$vco2 * df$vco2) / df$vo2_abs) - df$vco2)
    df$exve <- (((df$ve * df$ve) / df$vco2) - df$ve)
    # no forgetti removi!!!
    df$heartrate <- 100
    ###################
    df$vo2max_percentage <- df$vo2_rel_filt / max(df$vo2_rel_filt)
    df$hrmax_percentage <- df$heartrate / max(df$heartrate)
    df
  })
  return(data_list)
}

################################ Truncate Data #################################
truncate_data <- function(data_list, metadata) {
  test_data_truncated <- mapply(
    df = data_list, meta = metadata, SIMPLIFY = FALSE,
    FUN = function(df, meta) {
      df <- df %>% slice(meta$start_exercise_index:meta$end_exercise_index)
      df
    }
  )
  return(test_data_truncated)
}

############################ Interpolate to Seconds ############################
interpolate_to_seconds <- function(data_list) {
  test_data_interpolated <- lapply(data_list, function(df) {
    interpolate <- function(df) {
      ## first make sure data only contains numeric columns
      data_num <- df %>%
        select(where(is.numeric))

      out <- lapply(data_num, \(i) {
        approx(
          x = data_num[[1]],
          y = i,
          xout = seq(min(data_num[[1]]), max(data_num[[1]], na.rm = TRUE), 1)
        )$y
      }) %>%
        as.data.frame()
      out
    }
    out <- interpolate(df)
    out
  })
  return(test_data_interpolated)
}

################################ Bin Averaging #################################
apply_bin_average <- function(data_list, bin) {
  test_data_binned <- lapply(data_list, function(df, ...) {
    data_num <- df %>%
      select(where(is.numeric))
    out <- data_num %>%
      mutate(across(1, \(x) round(x / bin) * bin)) %>%
      group_by(1) %>%
      summarise(across(everything(), mean, na.rm = TRUE))
    out
  })
  return(test_data_binned)
}
