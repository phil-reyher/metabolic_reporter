################### Extract participant names out of dataList ##################
extract_participant_names <- function(data_list) {
  names <- sapply(data_list, function(df) {
    name <- regex_s(df, "\\bname\\b")
  })
  return(names)
}
################### Extract demographics and test parameters ###################
extract_metadata <- function(data_list) {
  metadata <- lapply(data_list, function(df) {
    name <- regex_s(df, "\\bname\\b")
    age <- regex_s(df, "\\bage\\b")
    sex <- regex_s(df, "\\bsex\\b")
    body_mass <- regex_s(df, "\\bweight\\b", unit = "kg")
    device <- regex_s(df, "^(?=.*(exercise))(?=.*(device)).*$")
    barometric_pressure <- regex_s(df, "^(?=.*(baro))(?=.*(press)).*$")
    temperature <- regex_s(df, "^(?=.*(insp))(?=.*(temp)).*$")
    relative_humidity <- regex_s(df, "^(?=.*(insp))(?=.*(humid)).*$")
    start_warmup <- regex_s(df, "^(?=.*(warm))(?=.*(up)).*$", 1)
    ifelse(length(start_warmup) == 0, start_warmup <- NA, start_warmup)
    start_exercise <- regex_s(df, "^(?=.*(start))(?=.*(exercise)).*$", 1)
    ifelse(length(start_exercise) == 0, start_exercise <- NA, start_exercise)
    end_exercise <- regex_s(df, "^(?=.*(cool))(?=.*(down)).*$", 1)
    ifelse(length(end_exercise) == 0, end_exercise <- NA, end_exercise)

    df <- data.frame(
      name, age, sex, body_mass, device, barometric_pressure,
      temperature, relative_humidity, start_warmup, start_exercise,
      end_exercise
    )
    df <- df %>% tidytable::select(tidytable::where(~ any(!is.na(.))))
  })
  return(metadata)
}

################ Extract testdate append to demographicsList ###################
extract_test_date <- function(extract_from, append_to) {
  test_dates <- mapply(
    df = append_to, vec = extract_from, SIMPLIFY = FALSE,
    FUN = function(df, vec) {
      test_date <- regmatches(vec, regexpr("\\d{8}", vec))
      df$test_date <- as.Date(test_date, format = "%Y%m%d")
      df
    }
  )
  return(test_dates)
}
