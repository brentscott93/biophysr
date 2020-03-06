#' Make Trap Observations
#'
#' @param f
#' @param txts
#'
#' @return
#' @export
#'
#' @examples
shiny_make_trap_observations <- function(trap_selected_date, threshold, cal_files = FALSE){

  withProgress(message = 'Making Observations', value = 0, max = 1, min = 0, {
    incProgress(amount = .25, detail = "Reading Data")

    all_files <- drop_dir(trap_selected_date) %>%
      arrange(name)

    if(cal_files == TRUE){
    cal_files <- all_files %>%
      dplyr::filter(str_detect(name, "Equi") | str_detect(name, "Step"))

    cal_folder_name <- paste0(trap_selected_date, "/cal")

    drop_create(cal_folder_name)

    cal_new_files_path <- paste0(cal_folder_name,"/", cal_files$name)

    map2(cal_files$path_display, cal_new_files_path, drop_move)
    }


    file_tibble <- all_files %>%
      dplyr::filter(str_detect(name, "Data"))

    txts <- purrr::map(file_tibble$path_lower, drop_read_csv)

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


    cutoff <- as.numeric(threshold)/60
    extract_numbers <- purrr::map(file_tibble$name, str_trap)
    dif2 <- vector("list") #for troubleshooting
    diff_vector <- vector()
    for(i in seq_along(extract_numbers[-length(extract_numbers)])){
      dif <- extract_numbers[[i+1]] - extract_numbers[[i]]
      dif2[[i]] <- extract_numbers[[i+1]] - extract_numbers[[i]] #for troubleshooting

      if(dif > cutoff){
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
    #make new folders
    for(r in 1:nrow(diff_tibble2)){

      if(r < 10){
        drop_create(paste0(trap_selected_date, "/obs_0", r))
      } else {
        drop_create(paste0(trap_selected_date,"/obs_", r))
      }
      obs_file_names[[r]] <- file_tibble$name[diff_tibble2$index[[r]]:diff_tibble2$index1[[r]]]
    }


    #move files

    for(o in seq_along(obs_file_names)){
      for(file in seq_along(obs_file_names[[o]])){
        if(o < 10){
          drop_move(from_path =  paste0(trap_selected_date, "/", obs_file_names[[o]][[file]]),
                      to_path = paste0(trap_selected_date, "/obs_0", o, "/", obs_file_names[[o]][[file]]))
        } else {
          drop_move(from_path = paste0(trap_selected_date, "/", obs_file_names[[o]][[file]]),
                      to_path = paste0(trap_selected_date, "/obs_", o, "/", obs_file_names[[o]][[file]]))
        }
      }}


    #create obs
    create_obs <- vector("list")
    for(row in 1:nrow(diff_tibble2)){
      create_obs[[row]] <- dplyr::bind_rows(txts[diff_tibble2$index[[row]]:diff_tibble2$index1[[row]]])
    }

    incProgress(.9, detail = "Saving Data")
    # writeLines("Saving Data")
    for(c in seq_along(create_obs)){
      if(c < 10){
       temp_grouped <-  write_temp_csv(create_obs[[c]], "grouped.csv",   col_names = TRUE)
       drop_upload(temp_grouped, path = paste0(trap_selected_date, "/obs_0", c))

      } else {
        temp_grouped <-  write_temp_csv(create_obs[[c]], "grouped.csv",   col_names = TRUE)
        drop_upload(temp_grouped, path = paste0(trap_selected_date, "/obs_", c))
      }
    }


    incProgress(1, detail = "Done")
  })

  showNotification("Obsevations created")
}


