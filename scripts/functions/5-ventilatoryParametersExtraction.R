##################### Extract ventilatory thresholds data ######################
find_vent_thresholds_data <- function(data_list) {
  thresholds_tables <- lapply(data_list, function(df) {
    ## truncate dataframe to ranges in which VT1/VT2 can occur (NASA Paper,2021)
    vt1_index_start <- which.min(abs(df$time - quantile(df$time, 0.3)))
    vt1_index_end <- which.min(abs(df$time - quantile(df$time, 0.8)))
    vt2_index_start <- which.min(abs(df$time - quantile(df$time, 0.5)))

    df_vt1 <- df %>% slice(vt1_index_start:vt1_index_end)
    df_vt2 <- df %>% slice_tail(n = vt2_index_start + 1)
    ## breakpointanalyses
    ## VT1##
    ## V-Slope##
    v_slope <- df_vt1 %>%
      select(vo2_abs, vco2) %>%
      as.matrix(.) %>%
      t(.)
    v_slope_index <- findchangepts_var(v_slope) + vt1_index_start - 1
    # EXCO2##
    excess_co2 <- df_vt1 %>%
      select(exco2) %>%
      as.matrix(.) %>%
      t(.)
    excess_co2_index <- findchangepts_var(excess_co2) + vt1_index_start - 1
    ## VT2##
    ## V-Slope##
    v_slope2 <- df_vt2 %>%
      select(vco2, ve) %>%
      as.matrix(.) %>%
      t(.)
    v_slope2_index <- findchangepts_var(v_slope2) + vt2_index_start - 1
    ## EXVE##
    excess_ventilation <- df_vt2 %>%
      select(exve) %>%
      as.matrix(.) %>%
      t(.)
    excess_vent_index <- findchangepts_var(excess_ventilation) +
      vt2_index_start - 1
    ## combine
    vt1_index_vslope <- v_slope_index
    vt1_index_exco <- excess_co2_index
    vt2_index_vslope <- v_slope2_index
    vt2_index_exvent <- excess_vent_index
    vt1_index <- round((v_slope_index + excess_co2_index) / 2)
    vt2_index <- round((v_slope2_index + excess_vent_index) / 2)
    # ´_´ seperator for pivot longer later
    vt1_time <- df$time[vt1_index]
    vt2_time <- df$time[vt2_index]
    vt1_vo2_abs <- df$vo2_abs_filt[vt1_index]
    vt2_vo2_abs <- df$vo2_abs_filt[vt2_index]
    vt1_vo2_rel <- df$vo2_rel_filt[vt1_index]
    vt2_vo2_rel <- df$vo2_rel_filt[vt2_index]

    vt1_work <- predict_work(df, vt1_vo2_abs)
    vt2_work <- predict_work(df, vt2_vo2_abs)

    vt1_heartrate <- df$heartrate[vt1_index]
    vt2_heartrate <- df$heartrate[vt2_index]

    vt1_vo2_percentage <- vt1_vo2_abs
    vt2_vo2_percentage <- vt2_vo2_abs
    vt1_work_percentage <- vt1_work
    vt2_work_percentage <- vt2_work
    vt1_hr_percentage <- vt1_heartrate
    vt2_hr_percentage <- vt2_heartrate

    out <- data.frame(
      vt1_index_vslope, vt1_index_exco, vt2_index_vslope,
      vt2_index_exvent, vt1_index, vt2_index, vt1_time,
      vt2_time, vt1_vo2_abs, vt2_vo2_abs, vt1_vo2_rel,
      vt2_vo2_rel, vt1_work, vt2_work, vt1_heartrate,
      vt2_heartrate, vt1_vo2_percentage, vt2_vo2_percentage,
      vt1_work_percentage, vt2_work_percentage,
      vt1_hr_percentage, vt2_hr_percentage
    )
    out
  })
  return(thresholds_tables)
}
############################# Extract VO2Max data ##############################
find_vo2max_data <- function(data_list) {
  vo2max_tables <- lapply(data_list, function(df) {
    max_index <- which.max(df$vo2_abs_filt)
    max_time <- df$time[max_index]
    max_vo2_abs <- max(df$vo2_abs_filt)
    max_vo2_rel <- max(df$vo2_rel_filt)
    max_work <- predict_work(df, max_vo2_abs)
    max_heartrate <- max(df$heartrate)

    max_vo2_percentage <- 1
    max_work_percentage <- 1
    max_hr_percentage <- 1

    out <- data.frame(
      max_index, max_time, max_vo2_abs, max_vo2_rel, max_work,
      max_heartrate, max_vo2_percentage, max_work_percentage, max_hr_percentage
    )
    out
  })
  return(vo2max_tables)
}

################ Create summary table of threshold and max data ################
create_summary_tables <- function(thresholds_tables, vo2max_tables) {
  summary_table <- mapply(
    vt_table = thresholds_tables, max_table = vo2max_tables,
    SIMPLIFY = FALSE, FUN = function(vt_table, max_table) {
      vt_table$vt1_vo2_percentage <- vt_table$vt1_vo2_percentage /
        max_table$max_vo2_abs
      vt_table$vt2_vo2_percentage <- vt_table$vt2_vo2_percentage /
        max_table$max_vo2_abs

      vt_table$vt1_work_percentage <- vt_table$vt1_work_percentage /
        max_table$max_work
      vt_table$vt2_work_percentage <- vt_table$vt2_work_percentage /
        max_table$max_work

      vt_table$vt1_hr_percentage <- vt_table$vt1_hr_percentage /
        max_table$max_heartrate
      vt_table$vt2_hr_percentage <- vt_table$vt2_hr_percentage /
        max_table$max_heartrate

      vt_table <- vt_table %>% select(-c(
        vt1_index_vslope, vt1_index_exco,
        vt2_index_vslope, vt2_index_exvent
      ))
      vt_table <- cbind(vt_table, max_table)
      vt_table <- vt_table %>%
        pivot_longer(everything(),
        names_pattern = "(vt[0-9]|max)_(.*)",
          names_to = c("variable", "measurement")
        ) %>%
        pivot_wider(
          id_cols = variable, names_from = measurement,
          values_from = value, names_repair = "check_unique"
        )

      out <- vt_table[order(vt_table$vo2_abs), ]
      out
    }
  )
  return(summary_table)
}
