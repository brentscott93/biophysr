
#' Calculate 'real-time' event frequency of laser trapping data
#'
#' @param processed_data a numeric vector of processed laser trap data
#' @param rle_object a df of run_length encoding from HM-Model
#' @param conversion a number that converts between running window time and 5kHz
#' @return a dataframe of event frequencies per each second
#' @export
#'

event_frequency <- function(processed_data, rle_object, conversion){

#get the number of seconds in the trace
seconds_in_trace <- length(processed_data)/5000

#get the start indices for each event
#events are id'd by HM-Model in running window length/time
#need to convert those indices back to 5kHz time

#remove the last row if trace ends in state 1
# because there is no event after the last state 1
start_event <- rle_object %>%
  dplyr::mutate(cumsum = cumsum(lengths),
         start_event = (cumsum + 1)*round(conversion)) %>%
  dplyr::filter(values == 1) %>%
  head(-1) %>%
  dplyr::pull(start_event)


#make a df where each row has 3 columns:
#1) event start index
#2) the index of the start of each second in datapoint
#3) the index of the end of each second in datapoint
freq_df <- purrr::map_dfr(start_event, ~tibble::tibble(start_event = .x,
                             begin = ((seq_len(seconds_in_trace)*5000)-4999),
                             end = seq_len(seconds_in_trace)*5000))

#test to see if the event is 'between' or in what second interval
find_it <- freq_df %>%
  dplyr::mutate(is_in = purrr::pmap_lgl(freq_df, ~dplyr::between(..1, ..2, ..3))) %>%
  dplyr::group_by(begin, end) %>%
  dplyr::summarize(freq = sum(is_in)) %>%
  tibble::rownames_to_column('second') %>%
  dplyr::mutate(second = as.numeric(second))

return(find_it)

#plots - histogram and point/line
# ggplot(find_it)+
#   geom_histogram(aes(x = freq, y = stat(density)), binwidth = 1, color = 'black')
#
#'real-time' event frequency plot
# ggplot(find_it, aes(x = second, y = freq))+
#   geom_line(aes(group = 1))+
#   geom_point()+
#   scale_x_continuous('Seconds', breaks = seq(0, nrow(find_it), by = 10))+
#   ylab('Events')+
#   ggtitle("'Real-time' Event Frequency")+
#   theme_classic(base_size = 18)

}
