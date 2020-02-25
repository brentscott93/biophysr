#' Group files for windows
#'
#' @return
#' @export
#'
#' @examples
group_files <- function(){
  
  pathPrep <- function(path = "clipboard") {
    y <- if (path == "clipboard") {
      readClipboard()
    } else {
      cat("Please enter the path:\n\n")
      readline()
    }
    x <- chartr("\\", "/", y)
    writeClipboard(x)
    return(x)
  }
  
  folder <- pathPrep()
  
  files <- list.files(folder, full.names = TRUE)
  
  read <- bind_rows(map(files, read_tsv, col_names = FALSE))
  
  write_tsv(read, path = paste0(folder, "/g4r.txt"), col_names = FALSE)
  

}


