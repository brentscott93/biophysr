#' Make Trap Observations
#'
#' @param f
#' @param txts
#'
#' @return
#' @export
#'
#' @examples
make_trap_observations <- function(wd){

  writeLines("Reading Data")
  setwd(wd)
  dir.create(paste0(wd, "/", "observations"))


  f <- list.files(pattern = "*.txt")
  txts <- map(f, read.delim, header = FALSE)

  writeLines("Creating Observations")
  str_trap <- function(x){
    substring <- str_sub(x, c(6, 11, 14, 17, 20, 23), c(9, 12, 15, 18, 21, 24))

    substring[[6]] <- round(as.numeric(substring[[6]])/60, digits = 2)

    if(as.numeric(substring[[6]]) == "0"){
      substring[[6]] <- ".00"
    } else {
      find_decimal <- unname(str_locate(substring[[6]], "[.]")[,1])

      substring[[6]] <- str_sub(substring[[6]], start = find_decimal, end = str_length(substring[[6]]))
    }

    final_string <- as.numeric(str_c(substring, collapse = ""))

    return(final_string)

  }

  extract_numbers <- map(f, str_trap)

  diff_vector <- vector()
  for(i in seq_along(extract_numbers[-length(extract_numbers)])){
    dif <- extract_numbers[[i+1]] - extract_numbers[[i]]
    #dif2[[i]] <- extract_numbers[[i+1]] - extract_numbers[[i]]

    if(dif > 0.5){
      diff_vector[[i]] <- "end_observation"
    } else {
      diff_vector[[i]] <- "observing"
    }
  }

  diff_vector[[length(extract_numbers)]] <- "end_observation"


  diff_tibble2 <- tibble(index = 1:length(diff_vector),
                         observation = diff_vector)


  diff_tibble2$observation[[1]] <- "begin_observation"
  for(x in 2:(nrow(diff_tibble2)-1)){
    if(diff_tibble2$observation[[x-1]] == "end_observation"){
      diff_tibble2$observation[[x]] <- "begin_observation"
    }
  }

  diff_tibble2$observation[[length(diff_tibble2$observation)]] <- "end_observation"

  diff_tibble2 <- filter(diff_tibble2, observation != "observing") %>%
    group_split(observation) %>%
    bind_cols()

  writeLines("Arranging Folders")
  wd <- getwd()
  obs_file_names <- vector("list")
  #make new folders
  for(r in 1:nrow(diff_tibble2)){

   if(r < 10){
    dir.create(paste0(wd, "/observations/", "obs_0", r))
   } else {
     dir.create(paste0(wd, "/observations/","obs_", r))
   }
    obs_file_names[[r]] <- f[diff_tibble2$index[[r]]:diff_tibble2$index1[[r]]]
  }


  #move files

  for(o in seq_along(obs_file_names)){
    for(file in seq_along(obs_file_names[[o]])){
      if(o < 10){
      file.rename(from = paste0(wd, "/", obs_file_names[[o]][[file]]),
                  to = paste0(wd, "/observations/", "obs_0", o, "/", obs_file_names[[o]][[file]]))
    } else {
      file.rename(from = paste0(wd, "/", obs_file_names[[o]][[file]]),
                  to = paste0(wd, "/observations/", "obs_", o, "/", obs_file_names[[o]][[file]]))
    }
    }}


  #create obs
  create_obs <- vector("list")
  for(row in 1:nrow(diff_tibble2)){
    create_obs[[row]] <- dplyr::bind_rows(txts[diff_tibble2$index[[row]]:diff_tibble2$index1[[row]]])
  }

  writeLines("Saving Data")
  for(c in seq_along(create_obs)){
    if(c < 10){
      write.table(create_obs[[c]],
                  file = paste0(wd, "/observations/", "obs_0", c, "/", "grouped.txt"),
                  row.names = FALSE,
                  col.names = FALSE,
                   sep = "\t")
    } else {
      write.table(create_obs[[c]],
                  to = paste0(wd, "/observations/", "obs_", o, "/", "grouped.txt"),
                  row.names = FALSE,
                  col.names = FALSE,
                  sep = "\t")
    }
  }

writeLines("Done")
}


