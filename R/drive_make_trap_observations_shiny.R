#' Make Trap Observations
#'
#' @param f
#' @param txts
#'
#' @return
#' @export
#'
#' @examples
drive_make_trap_observations_shiny <- function(date_folder){

  withProgress(message = 'Making Observations', value = 0, max = 1, min = 0, {


    incProgress(amount = .25, detail = "Reading Data")


    f <- drive_ls(date_folder, orderBy = "name")





    incProgress(amount = .4, detail = "Determining Observations")
    # writeLines("Creating Observations")
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

    extract_numbers <- purrr::map(f$name, str_trap)
    dif2 <- vector("list") #for troubleshooting
    diff_vector <- vector()
    for(i in seq_along(extract_numbers[-length(extract_numbers)])){
      dif <- extract_numbers[[i+1]] - extract_numbers[[i]]
      dif2[[i]] <- extract_numbers[[i+1]] - extract_numbers[[i]] #for troubleshooting

      if(dif > 0.5){
        diff_vector[[i]] <- "end_observation"
      } else {
        diff_vector[[i]] <- "observing"
      }
    }

    diff_vector[[length(extract_numbers)]] <- "end_observation"



    diff_tibble2 <- tibble(index = 1:length(diff_vector),
                           observation = diff_vector)



    incomplete_obs <- filter(diff_tibble2, observation == "end_observation" & lag(observation) == "end_observation") %>%
      pull(index)

    if(identical(incomplete_obs, integer(0)) == TRUE){
      diff_tibble2 <- diff_tibble2

    } else {
      diff_tibble2 <- slice(diff_tibble2, -incomplete_obs)
    }

    if(diff_tibble2$observation[[1]] == "end_observation"){
      diff_tibble2 <- slice(diff_tibble2, -1)
    } else {
      diff_tibble2 <- diff_tibble2
    }


    #old method of fixing incomplete traces
    #if(diff_tibble2$observation[[(length(diff_tibble2$observation) - 1)]] == "end_observation"){
    #  diff_tibble2 <- slice(diff_tibble2, -(nrow(diff_tibble2)))
    #} else {
    #  diff_tibble2 <- diff_tibble2
    # }




    diff_tibble2$observation[[1]] <- "begin_observation"
    for(x in 2:(nrow(diff_tibble2)-1)){
      if(diff_tibble2$observation[[x-1]] == "end_observation"){
        diff_tibble2$observation[[x]] <- "begin_observation"
      }
    }



    diff_tibble2 <- filter(diff_tibble2, observation != "observing") %>%
      group_split(observation) %>%
      bind_cols()

    incProgress(.7, detail = "Arranging Folders")


    obs_file_names <- vector("list")
    obs_dribbles <- vector("list")
    #make new folders
    for(r in 1:nrow(diff_tibble2)){

      if(r < 10){
        obs_dribbles[[r]] <- drive_mkdir(paste0("obs_0", r), path = date_folder)

      } else {
        obs_dribbles[[r]] <- drive_mkdir(paste0("obs_", r), path = date_folder)
      }

      rows <- diff_tibble2$index[[r]]:diff_tibble2$index1[[r]]
      obs_file_names[[r]] <- f[rows,]
    }


    #move files

    for(o in seq_along(obs_file_names)){
      for(file in 1:nrow(obs_file_names[[o]])){

          drive_mv(obs_file_names[[o]][file,],
                      path = obs_dribbles[[o]])
      }}



    incProgress(.9, detail = "Saving Data")


   # directions <- tibble(obs = rep("", length(obs_file_names)),
                        # baseline_start_sec	=  rep("", length(obs_file_names)),
                        # baseline_stop_sec =  rep("", length(obs_file_names)),
                       #  detrend =  rep("", length(obs_file_names)),
                        # include =  rep("", length(obs_file_names)),
                        # condition = rep("", length(obs_file_names)))


  #  wd <- paste0(tempdir(),"/",  str_replace_all(Sys.time(), "[^[:alnum:]]", ""))
  #  dir.create(wd)

   # temp_file_path <- tempfile(pattern = "directions", tmpdir = wd, fileext = ".csv")
   # write_csv(directions, temp_file_path)

   # write_csv(directions,
       #       path = paste0(wd, "/directions.csv"))


   # drive_upload(temp_file_path, path = date_folder, name = "directions", type = "spreadsheet")

    incProgress(1, detail = "Done")
  })
}


