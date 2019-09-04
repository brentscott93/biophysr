#' Import Files to R Shiny
#'
#' @param shiny_datapath
#' @param file_name
#'
#' @return
#' @export
#'
#' @examples
import_shiny <- function(shiny_datapath, file_name){

  filenames <- file_name # makes a list of the names of all files w/.tsv extension in the wd including its sub-directories

  data <- map(shiny_datapath, read_tsv) # loads in all the .tsv files in the WD including subdirectories

  for (i in seq_along(data)){
    data[[i]]<-cbind(data[[i]], filenames[i])} # for every datasheet in the dataset add a column identifying the filename it came from

  bind_data <- bind_rows(data) # combine all data into one data frame

}
