
#'Import processed trapping data files
#'
#'Imports all files in the current working directory into list
#'
#'
#' @param working_dir characters string of path to directory where files are located
#' @return One data frame. Recommended to use tidyr::separate() on 'filenames[i]' columns.
#' @export
#'
#' @examples import_txt()
#'
#'
#' trap_data <- import_txt("my_wd")
#'
#'
#'
#'
import_processed_trap <- function(working_dir){

  setwd(working_dir)

  filenames = list.files(pattern="processed.txt", recursive = TRUE) # makes a list of the names of all files w/processed4r.txt extension in the wd including its sub-directories

  datasheets = lapply(filenames, scan) # loads in all the .tsv files in the WD including subdirectories

  names(datasheets) <- filenames # for every datasheet in the dataset add a column identifying the filename it came from

  return(datasheets)


}
