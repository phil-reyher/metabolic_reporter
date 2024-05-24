############################# Format Test Details ##############################
format_test_details <- function(metadata_list) {
  test_details <- lapply(metadata_list, function(meta) {
    out <- meta %>%
      select(
        test_date, temperature, relative_humidity,
        barometric_pressure
      ) %>%
      mutate(across("test_date", ~ format(.x, format = "%d-%b-%Y"))) %>%
      mutate(across(-"test_date", ~ format(round(as.numeric(.), 1),
                                           nsmall = 1)))
    out
  })
  return(test_details)
}

############################ Format Summary Tables #############################
format_summary_table <- function(summary_tables) {
  formatted_summary_table <- lapply(summary_tables, function(df) {
    df <- df %>%
      mutate(across(
        ends_with("percentage", ignore.case = TRUE),
        ~ round(. * 100, digits = 0)
      )) %>%
      mutate(across(c(work, heartrate), ~ round(., digits = 0))) %>%
      mutate(across(c(vo2_abs, vo2_rel), ~ format(round(., digits = 1),
                                                  nsmall = 1)))
    out <- df %>% select(c(
      variable, work, work_percentage, vo2_abs, vo2_rel,
      vo2_percentage, heartrate, hr_percentage
    ))
    out
  })
  return(formatted_summary_table)
}

################################## GXT Table ###################################
create_gxt_table <- function(data_list, vo2max_data_list) {
  gxt_table <- mapply(
    df = data_list, max_table = vo2max_data_list, SIMPLIFY = FALSE,
    function(df, max_table) {
      # extract work, truncating to workmax for linear model
      df <- df %>% slice(1:which.max(df$work))
      work <- df$work
      # select columns to loop through
      df <- df %>% select(
        vo2_abs_filt, vo2_rel_filt, vo2max_percentage, heartrate,
        hrmax_percentage
      )
      # loop through columns, for linear models
      results_list <- lapply(df, function(vec) {
        model <- lm(vec ~ work)
        work_in <- seq(100, max(work), by = 25)
        new_observations <- data.frame(work = work_in)
        predicted_values <- predict(model, newdata = new_observations)
      })
      gxt_table <- bind_cols(results_list)
      work <- seq(100, max(work), by = 25)
      gxt_table <- cbind(work, gxt_table)
      # exchange last row with values of VO2MAX
      max_table <- max_table %>% select(
        "work" = max_work, "vo2_abs_filt" = max_vo2_abs,
        "vo2_rel_filt" = max_vo2_rel,
        "vo2max_percentage" = max_vo2_percentage,
        "heartrate" = max_heartrate,
        "hrmax_percentage" = max_hr_percentage
      )
      if (max(gxt_table$work) >= max_table$work) {
        gxt_table <- gxt_table[-nrow(gxt_table), ]
      }
      gxt_table <- rbind(gxt_table, max_table)
      gxt_table <- gxt_table %>%
        mutate(across(
          ends_with("percentage", ignore.case = TRUE),
          ~ round(. * 100, digits = 0)
        )) %>%
        mutate(across(c("work", "heartrate"), ~ round(., digits = 0))) %>%
        mutate(across(c("vo2_abs_filt", "vo2_rel_filt"),
                      ~ format(round(., digits = 1), nsmall = 1)))
      gxt_table
    }
  )
  return(gxt_table)
}

############################## Coggan Power Zones ##############################
create_coggans_zones_table <- function(vt2_data_list) {
  coggan_table <- lapply(vt2_data_list, function(df) {
    lvl1_work_low <- "-"
    lvl1_work_up <- round(df$vt2_work * 0.55)
    lvl1_heartrate_low <- "-"
    lvl1_heartrate_up <- round(df$vt2_heartrate * 0.68)
    lvl1_rpe_low <- "-"
    lvl1_rpe_up <- 9

    lvl2_work_low <- round(df$vt2_work * 0.56)
    lvl2_work_up <- round(df$vt2_work * 0.75)
    lvl2_heartrate_low <- round(df$vt2_heartrate * 0.69)
    lvl2_heartrate_up <- round(df$vt2_heartrate * 0.83)
    lvl2_rpe_low <- 9
    lvl2_rpe_up <- 11

    lvl3_work_low <- round(df$vt2_work * 0.76)
    lvl3_work_up <- round(df$vt2_work * 0.90)
    lvl3_heartrate_low <- round(df$vt2_heartrate * 0.84)
    lvl3_heartrate_up <- round(df$vt2_heartrate * 0.94)
    lvl3_rpe_low <- 11
    lvl3_rpe_up <- 13

    lvl4_work_low <- round(df$vt2_work * 0.91)
    lvl4_work_up <- round(df$vt2_work * 1.05)
    lvl4_heartrate_low <- round(df$vt2_heartrate * 0.95)
    lvl4_heartrate_up <- round(df$vt2_heartrate * 1.05)
    lvl4_rpe_low <- 13
    lvl4_rpe_up <- 15

    lvl5_work_low <- round(df$vt2_work * 1.06)
    lvl5_work_up <- round(df$vt2_work * 1.2)
    lvl5_heartrate_low <- round(df$vt2_heartrate * 1.06)
    lvl5_heartrate_up <- "-"
    lvl5_rpe_low <- 15
    lvl5_rpe_up <- 17

    lvl6_work_low <- round(df$vt2_work * 1.21)
    lvl6_work_up <- "-"
    lvl6_heartrate_low <- "-"
    lvl6_heartrate_up <- "-"
    lvl6_rpe_low <- 17
    lvl6_rpe_up <- 19

    lvl7_work_low <- "-"
    lvl7_work_up <- "-"
    lvl7_heartrate_low <- "-"
    lvl7_heartrate_up <- "-"
    lvl7_rpe_low <- 20
    lvl7_rpe_up <- "-"


    lvl1 <- c(
      1, "Active Recovery", lvl1_work_low, lvl1_work_up, lvl1_heartrate_low,
      lvl1_heartrate_up, lvl1_rpe_low, lvl1_rpe_up
    )
    lvl2 <- c(
      2, "Endurance", lvl2_work_low, lvl2_work_up, lvl2_heartrate_low,
      lvl2_heartrate_up, lvl2_rpe_low, lvl2_rpe_up
    )
    lvl3 <- c(
      3, "Tempo", lvl3_work_low, lvl3_work_up, lvl3_heartrate_low,
      lvl3_heartrate_up, lvl3_rpe_low, lvl3_rpe_up
    )
    lvl4 <- c(
      4, "Lactate Threshold", lvl4_work_low, lvl4_work_up, lvl4_heartrate_low,
      lvl4_heartrate_up, lvl4_rpe_low, lvl4_rpe_up
    )
    lvl5 <- c(
      5, "VO\\textsubscript{2max}", lvl5_work_low, lvl5_work_up,
      lvl5_heartrate_low, lvl5_heartrate_up, lvl5_rpe_low, lvl5_rpe_up
    )
    lvl6 <- c(
      6, "Anaerobic Capacity", lvl6_work_low, lvl6_work_up, lvl6_heartrate_low,
      lvl6_heartrate_up, lvl6_rpe_low, lvl6_rpe_up
    )
    lvl7 <- c(
      7, "Neuromuscular Power", lvl7_work_low, lvl7_work_up, lvl7_heartrate_low,
      lvl7_heartrate_up, lvl7_rpe_low, lvl7_rpe_up
    )
    training_zones <- as.data.frame(rbind(lvl1, lvl2, lvl3, lvl4,
                                          lvl5, lvl6, lvl7))
    colnames(training_zones) <- c(
      "Zone", "Intensity", "Lower \\par Range",
      "Upper \\par Range", "Lower \\par Range",
      "Upper \\par Range", "Lower \\par Range",
      "Upper \\par Range"
    )
    rownames(training_zones) <- NULL
    out <- training_zones
    out
  })
  return(coggan_table)
}
############################### AIS Power Zones ################################
create_ais_zones_table <- function(vo2max_data_list) {
  ais_table <- lapply(vo2max_data_list, function(df) {
    lvl0_work_low <- round(df$max_work * 0.4)
    lvl0_work_up <- round(df$max_work * 0.5)
    lvl0_heartrate_low <- "-"
    lvl0_heartrate_up <- round(df$max_heartrate * 0.65)
    lvl0_rpe_low <- "-"
    lvl0_rpe_up <- 11

    lvl1_work_low <- round(df$max_work * 0.5)
    lvl1_work_up <- round(df$max_work * 0.65)
    lvl1_heartrate_low <- round(df$max_heartrate * 0.65)
    lvl1_heartrate_up <- round(df$max_heartrate * 0.75)
    lvl1_rpe_low <- 12
    lvl1_rpe_up <- 13

    lvl2_work_low <- round(df$max_work * 0.65)
    lvl2_work_up <- round(df$max_work * 0.725)
    lvl2_heartrate_low <- round(df$max_heartrate * 0.75)
    lvl2_heartrate_up <- round(df$max_heartrate * 0.8)
    lvl2_rpe_low <- 13
    lvl2_rpe_up <- 15

    lvl3_work_low <- round(df$max_work * 0.725)
    lvl3_work_up <- round(df$max_work * 0.80)
    lvl3_heartrate_low <- round(df$max_heartrate * 0.8)
    lvl3_heartrate_up <- round(df$max_heartrate * 0.85)
    lvl3_rpe_low <- 15
    lvl3_rpe_up <- 16

    lvl4_work_low <- round(df$max_work * 0.8)
    lvl4_work_up <- round(df$max_work * 0.9)
    lvl4_heartrate_low <- round(df$max_heartrate * 0.85)
    lvl4_heartrate_up <- round(df$max_heartrate * 0.92)
    lvl4_rpe_low <- 16
    lvl4_rpe_up <- 17

    lvl5_work_low <- round(df$max_work * 0.9)
    lvl5_work_up <- round(df$max_work * 1)
    lvl5_heartrate_low <- round(df$max_heartrate * 0.92)
    lvl5_heartrate_up <- round(df$max_heartrate * 1)
    lvl5_rpe_low <- 17
    lvl5_rpe_up <- 19

    lvl0 <- c(
      0, "Recovery", lvl0_work_low, lvl0_work_up, lvl0_heartrate_low,
      lvl0_heartrate_up, lvl0_rpe_low, lvl0_rpe_up
    )
    lvl1 <- c(
      1, "Aerobic", lvl1_work_low, lvl1_work_up, lvl1_heartrate_low,
      lvl1_heartrate_up, lvl1_rpe_low, lvl1_rpe_up
    )
    lvl2 <- c(
      2, "Extensive Endurance", lvl2_work_low, lvl2_work_up, lvl2_heartrate_low,
      lvl2_heartrate_up, lvl2_rpe_low, lvl2_rpe_up
    )
    lvl3 <- c(
      3, "Intensive Endurance", lvl3_work_low, lvl3_work_up, lvl3_heartrate_low,
      lvl3_heartrate_up, lvl3_rpe_low, lvl3_rpe_up
    )
    lvl4 <- c(
      4, "Threshold", lvl4_work_low, lvl4_work_up, lvl4_heartrate_low,
      lvl4_heartrate_up, lvl4_rpe_low, lvl4_rpe_up
    )
    lvl5 <- c(
      5, "VO\\textsubscript{2max}", lvl5_work_low, lvl5_work_up,
      lvl5_heartrate_low, lvl5_heartrate_up, lvl5_rpe_low, lvl5_rpe_up
    )
    training_zones <- as.data.frame(rbind(lvl0, lvl1, lvl2, lvl3, lvl4, lvl5))
    colnames(training_zones) <- c(
      "Zone", "Intensity", "Lower \\par Range",
      "Upper \\par Range", "Lower \\par Range",
      "Upper \\par Range", "Lower \\par Range",
      "Upper \\par Range"
    )
    rownames(training_zones) <- NULL
    out <- training_zones
    out
  })
  return(ais_table)
}
