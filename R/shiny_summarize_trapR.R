library(tidyverse)
trap_selected_project = "/Users/brentscott/Box Sync/Muscle Biophysics Lab/Data/biophysr/bscott/trap/project_myoV-phosphate"
shiny_summarize_trap <-  function(trap_selected_project, file_type){

  directions <- list_files(trap_selected_project,
                           pattern = "directions.csv",
                           recursive = TRUE)

  event_p <- list_files(trap_selected_project,
                        pattern = "hmm_events.csv",
                        recursive = TRUE)

  raw_data_paths <- list_files(trap_selected_project,
                               pattern = "grouped",
                               recursive = TRUE)


  read_exported_directions <- bind_rows(map(directions$path, read_csv)) %>%
    mutate(grouped_file = raw_data_paths$path) %>%
    filter(report == "success") %>%
    mutate(event_paths = event_p$path) %>%
    rename(quality_control = `Quality Control`) %>%
    filter(quality_control == TRUE) %>%
    mutate(split = event_paths) %>%
    separate(split, c('nada', 'u', 'b', 'box', 'mbl', 'b2', 'data', 'me', 'trap', 'project', 'conditions', 'date', 'res', 'e', 'filename'), sep = '/')



  all_hmm_events <- suppressMessages(map(read_exported_directions$event_paths, read_csv, col_names = TRUE))


  for(s in 1:nrow(read_exported_directions)){
    all_hmm_events[[s]] <- cbind(all_hmm_events[[s]], read_exported_directions$conditions[[s]])
  }




  event_files_filtered <- bind_rows(all_hmm_events) %>%
    rename(conditions = 'read_exported_directions$conditions[[s]]')# %>%
  # separate(conditions, c("myo", "ph", "phosphate"), sep = "_")



  #####summarise


  #####summarise


  #all_grouped <- suppressMessages(map(read_exported_directions$grouped_file, read_tsv, col_names = FALSE))

  all_grouped <- list()
  for(g in 1:nrow(read_exported_directions)){
    if(str_sub(read_exported_directions$grouped_file[[g]], -3) == 'txt'){
      all_grouped[[g]] <- read_tsv(read_exported_directions$grouped_file[[g]], col_names = c('bead', 'trap'))
    } else {
      all_grouped[[g]] <- read_csv(read_exported_directions$grouped_file[[g]], col_names = TRUE)
    }

    all_grouped[[g]] <- cbind(all_grouped[[g]], read_exported_directions$conditions[[g]])
  }


  all_grouped <- bind_rows(all_grouped) %>%
    rename(conditions = 'read_exported_directions$conditions[[g]]')

  get_time <- all_grouped %>%
    group_by(conditions) %>%
    summarize(minutes = round((length(bead)/5000)/60, 2))# %>%
  # separate(conditions, c("myo", "ph", "phosphate"), sep = "_")


  num_events <- event_files_filtered %>%
    group_by(conditions) %>%
    count()


  summarize_trap <- event_files_filtered %>%
    group_by(conditions) %>%
    summarize(time_on_avg = mean(time_on_ms),
              time_on_se = std.error(time_on_ms, na.rm = TRUE),
              time_off_avg = mean(time_off_ms, na.rm = TRUE),
              time_off_se = std.error(time_off_ms, na.rm = TRUE),
              displacement_avg = mean(displacement_nm, na.rm = TRUE),
              displacement_se = std.error(displacement_nm, na.rm = TRUE),
              force_avg = mean(force, na.rm = TRUE),
              force_se = std.error(force, na.rm = TRUE),
              num_events = n()) %>%
    right_join(get_time)


  event_files_filtered %<>% separate("conditions", c("myosin", "pH", "pi"), sep = "_")

  write_csv(event_files_filtered, paste0(trap_selected_project, '/all.csv'))

  event_files_filtered$condition <- factor(event_files_filtered$condition,
                                           levels = c("myoV-WT_pH7.0_2ndcontrol",
                                                      "myoV-WT_pH7.0_15mMPi",
                                                      "myoV-WT_pH7.0_30mM-Pi",
                                                      "myoV-S217A_pH7.0_2ndControl",
                                                      "myoV-S217A_pH7.0_15mM-Pi",
                                                      "myoV-S217A_pH7.0_30mM-Pi"))

  blu <- brewer.pal(n = 9, "Blues")[c(4, 6, 8)]
  red <- brewer.pal(n = 9, "Reds")[c(4, 6, 8)]
  colors <- c(blu, red)
  (step_histo <- ggplot(data = event_files_filtered,
                        aes(x = displacement_nm,
                            fill = condition))+
    geom_histogram(aes(y = stat(density)),
                     binwidth = 2,
                     color = "black")+
    facet_wrap(~condition)+
    xlab("Step Size (nm)")+
    scale_y_continuous(expand = c(0,0))+
    scale_x_continuous(breaks = seq(-40, 40, by = 5))+
    scale_fill_manual(values = colors)+
    #scale_fill_brewer(palette = "Dark2")+
      theme_linedraw()+
      theme(panel.grid = element_blank(),
            legend.position = "none"))

  ggplotly(step_histo)


  (time_on_histo <- ggplot(data = event_files_filtered,
                        aes(x = time_on_ms,
                            fill = condition))+
      geom_histogram(aes(y = stat(density)),
                     binwidth = 15,
                     color = "black")+
      facet_wrap(~condition)+
      xlab("Time on (ms)")+
      scale_y_continuous(expand = c(0,0))+
      scale_x_continuous(breaks = seq(0, 6000, by = 100))+
      scale_fill_manual(values = colors)+
      #scale_fill_brewer(palette = "Dark2")+
      theme_linedraw()+
      theme(panel.grid = element_blank(),
            legend.position = "none"))

  ggplotly(time_on_histo)


  (time_off_histo <- ggplot(data = event_files_filtered,
                           aes(x = time_off_ms,
                               fill = condition))+
      geom_histogram(aes(y = stat(density)),
                     binwidth = 100,
                     color = "black")+
      facet_wrap(~condition)+
      xlab("Time off (ms)")+
      scale_y_continuous(expand = c(0,0))+
      scale_x_continuous(breaks = seq(0, 20000, by = 500))+
      scale_fill_manual(values = colors)+
      #scale_fill_brewer(palette = "Dark2")+
      theme_linedraw()+
      theme(panel.grid = element_blank(),
            legend.position = "none"))

  ggplotly(time_off_histo)

  (force_histo <- ggplot(data = event_files_filtered,
                            aes(x = force,
                                fill = condition))+
      geom_histogram(aes(y = stat(density)),
                     binwidth = 0.1,
                     color = "black")+
      facet_wrap(~condition)+
      xlab("Time off (ms)")+
      scale_y_continuous(expand = c(0,0))+
      #scale_x_continuous(breaks = seq(0, 20000, by = 500))+
      scale_fill_manual(values = colors)+
      #scale_fill_brewer(palette = "Dark2")+
      theme_linedraw()+
      theme(panel.grid = element_blank(),
            legend.position = "none"))

  ggplotly(force_histo)



}
