################################ Clean dataset #################################
# rename columns to appropriate names.
# remove first and last rows
tidy_up <- function(data_list) {
  data_list <- lapply(data_list, function(df) {
    ## find indices of string "time", this is where the spiro data starts
    tmp <- which(sapply(df, function(x) {
      grepl("TIME", fixed = TRUE, x)
    })) - 1
    ## finally remove the mess from the top
    df <- slice(df, -(1:tmp))
    ## Column names
    ## combine the two header rows
    column_names <- paste(df[1, ], df[2, ], sep = "")
    ## clean up NAs
    column_names <- gsub("NA", "", column_names)
    ## remove whitespace
    column_names <- gsub(" ", "", column_names)
    ## standardize colnames
    column_names <- sub("^(?=.*(VO2))(?=.*(kg)).*$", "vo2_rel", column_names,
      perl = TRUE, ignore.case = TRUE
    )
    column_names <- sub("VO2STPD", "vo2_abs", column_names,
      perl = TRUE,
      ignore.case = TRUE
    )

    column_names <- sub(".*work.*", "work", column_names,
      perl = TRUE,
      ignore.case = TRUE
    )

    column_names <- sub(".*hr.*", "heartrate", column_names,
      perl = TRUE,
      ignore.case = TRUE
    )
    column_names <- tolower(gsub("STPD", "", column_names))
    colnames(df) <- column_names
    ## remove anything but data
    df <- slice(df, -(1:4))
    ## find first and last instance of NA to remove mess at the bottom
    na_index <- which(is.na(df[, 1]))
    first_na <- min(na_index)
    l <- nrow(df)
    df <- slice(df, -(first_na:l))
    ## convert time from m:s format to s
    df$time <- mmss_to_ss(df$time)
    ## convert all to numeric, to character first to preserve factors
    df <- df %>% mutate(across(.cols = everything(), ~ as.character(.x) %>%
        as.numeric(.x)
    ))
    ## rename problematic column names
    df <- df %>% rename("vevo2" = `ve/vo2`, "vevco2" = `ve/vco2`)

    df
  })
  return(data_list)
}
############################# extend metadata ##################################
# add indices of exercise beginning and end do demographicList
extract_start_end_indices <- function(extract_from, append_to) {
  metadata <- mapply(
    df = extract_from, meta = append_to, SIMPLIFY = FALSE,
    FUN = function(df, meta) {
      start_exercise <- as.numeric(meta$start_exercise) * 60
      end_exercise <- as.numeric(meta$end_exercise) * 60
      beg <- which.min(abs(df$time - start_exercise))
      end <- which.min(abs(df$time - end_exercise))
      meta$start_exercise_index <- beg
      meta$end_exercise_index <- end
      meta
    }
  )
  return(metadata)
}
