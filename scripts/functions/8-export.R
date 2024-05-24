############################## Create Large List ###############################
create_combined_list <- function() {
  big_list <- mapply(
    x = metadata, y = test_details_formatted,
    z = summary_tables_formatted, a = gxt_tables_formatted,
    b = coggans_tables_formatted, c = ais_tables_formatted,
    d = participant_name_list, SIMPLIFY = FALSE,
    FUN = function(x, y, z, a, b, c, d) {
      list(
        metadata = x, test_details = y,
        summary_table = z, gxt_table = a, coggan_table = b, ais_table = c,
        participant_name = d
      )
    }
  )
  return(big_list)
}
################################### let's render################################
create_reports <- function(big_list) {
  purrr::walk(big_list, function(participant) {
    rmarkdown::render(
      input = "layout/report_layout.Rmd",
      intermediates_dir = "output/intermediates",
      output_file = paste0(participant$participant_name, ".pdf"),
      output_dir = "finished_reports",
      output_format = output_format,
      clean = TRUE
    )
  })
}
