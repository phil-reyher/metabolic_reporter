import_filelist <- function(file_list) {
  data_list <- lapply(file_list, function(file_list) {
    df <- tidytable::fread(file_list,
      header = FALSE, fill = TRUE, sep = ",",
      quote = "\"", dec = ".", na.strings = ""
    )
    df
  })
  return(data_list)
}
