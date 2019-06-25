
#'Import .tsv files
#'
#'Imports and concactenates all .tsv files in the current working directory
#'
#'
#' @param working_dir characters string of path to directory where files are located
#' @return One data frame. Recommended to use tidyr::separate() on 'filenames[i]' columns.
#' @export
#'
#' @examples import_tsv()
#'
#'
#' motility_data <- import_tsv("my_wd") %>%
#' separate('filenames[i]', c("date, pH, mutation, pCa"), sep = "_")
#'
#'
#'
#'
import_tsv <- function(working_dir){

  setwd(working_dir)

  filenames = list.files(pattern="*.tsv", recursive = TRUE) # makes a list of the names of all files w/.tsv extension in the wd including its sub-directories

  datasheets = lapply(filenames, read_tsv) # loads in all the .tsv files in the WD including subdirectories

  for (i in seq_along(datasheets)){
    datasheets[[i]]<-cbind(datasheets[[i]], filenames[i])} # for every datasheet in the dataset add a column identifying the filename it came from

  bind_data <- bind_rows(datasheets) # combine all data into one data frame

}





