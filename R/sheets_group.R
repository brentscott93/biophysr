#' Group sheets on Google Drive
#'
#' @param read_from
#' @param upload_to
#'
#' @return
#' @export
#'
#' @examples
sheets_group <- function(read_from, upload_to){

withProgress(message = 'Grouping Sheets', value = 0, max = 1, min = 0, {

  incProgress(amount = .3, detail = "Reading Files")

  read_from <- arrange(read_from, name)
  df_list <- vector("list")
  for(gsheet in 1:nrow(read_from)){

    df_list[[gsheet]] <- read_sheet(read_from[gsheet,], col_names = c("bead"))

  }

  grouped <- bind_rows(df_list)

  incProgress(amount = .6, detail = "Uploading to drive")

  grouped_dir <- paste0(tempdir(), "/grouped_", str_replace_all(Sys.time(), "[^[:alnum:]]", ""))
  dir.create(grouped_dir)

  grouped_local_filepath <- tempfile(pattern = "grouped", tmpdir = grouped_dir, fileext = ".csv")
  write_csv(grouped, grouped_local_filepath, col_names = FALSE)

  grouped_gsheet <- drive_upload(grouped_local_filepath, path = upload_to, name = paste0("grouped", upload_to$name), type = "spreadsheet")

  incProgress(1, detail = "Done")
})

showNotification("Success! Sheets grouped in drive", type = "message", duration = NULL)

}
