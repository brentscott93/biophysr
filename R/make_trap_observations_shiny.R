#' Make Trap Observations
#'
#' @param f
#' @param txts
#'
#' @return
#' @export
#'
#' @examples
make_trap_observations_shiny <- function(f, txts){

  str_trap <- function(x){
    substring <- str_sub(x, c(6, 11, 14, 17, 20, 23), c(9, 12, 15, 18, 21, 24))

    substring[[6]] <- round(as.numeric(substring[[6]])/60, digits = 2)

    find_decimal <- unname(str_locate(substring[[6]], "[.]")[,1])

    substring[[6]] <- str_sub(substring[[6]], start = find_decimal, end = str_length(substring[[6]]))

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
  for(x in 2:nrow(diff_tibble2)){
    if(diff_tibble2$observation[[x-1]] == "end_observation"){
      diff_tibble2$observation[[x]] <- "begin_observation"
    }
  }

  diff_tibble2 <- filter(diff_tibble2, observation != "observing") %>%
    group_split(observation) %>%
    bind_cols()

  #create obs
  create_obs <- vector("list")
  for(row in 1:nrow(diff_tibble2)){
    create_obs[[row]] <- dplyr::bind_rows(txts[diff_tibble2$index[[row]]:diff_tibble2$index1[[row]]])
  }

  return(create_obs)
}
