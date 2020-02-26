# START SERVER

server = function(input, output, session) {
  
  result_auth <- secure_server(check_credentials = check_credentials(credentials))
  
  # output$res_auth <- renderPrint({
  # reactiveValuesToList(result_auth)
  # })
  
  rv <- reactiveValues()
  options(shiny.maxRequestSize=30*1024^2)
  
  current_user_comment <- reactive({ result_auth$comment })
  
  output$current_user_comment <- renderText({
    current_user_comment()
  })
  
  #find users google drive folder
  current_user <- reactive({ result_auth$user })
  
  users_biophysr_folder <- reactive({
    #get path to home, look into Box Sync folder for user
    home_path <- path.expand("~")
    box_sync <- "/Box Sync/Muscle Biophysics Lab/Data/biophysr/"
    
    user_path <- paste0(home_path, box_sync, current_user())
  
    return(user_path)
  })
  
  
  #------------------------------------------------------------------------------------------------------------
  #------------------------------------------------------------------------------------------------------------
  #------------------------------------------------------------------------------------------------------------
  ##MOTILITY SERVER START##
  
  
  
  
  
  
  
  
  
  ##MOTILITY SERVER END##
  #------------------------------------------------------------------------------------------------------------
  #------------------------------------------------------------------------------------------------------------
  #------------------------------------------------------------------------------------------------------------
  ##TRAP SERVER START##
  

  #------------------------------------------------------------------------------------------------------------
  # START CREATE NEW FOLDERS ON DRIVE 
  
  rv$new_trap_project <- 0
  observeEvent(input$trap_create_project_actionButton,{
    new_trap_project_name <- paste0("project_", input$trap_create_project_textInput)
    dir.create(path = paste0(users_biophysr_folder(), "/trap/", new_trap_project_name))
    showNotification("Project folder created", type = "message")
    rv$new_trap_project <- rv$new_trap_project + 1
    
  })
  
  rv$new_trap_condition <- 0
  observeEvent(input$trap_create_conditions_actionButton,{
    dir.create(path = paste0(trap_selected_project()$path,"/",input$trap_create_conditions_textInput))
    showNotification("Condition folder created", type = "message")
    rv$new_trap_condition <- rv$new_trap_condition + 1
    
  })
  
  
  rv$new_trap_date <- 0
  observeEvent(input$trap_create_date_actionButton,{
    dir.create(path = paste0(trap_selected_conditions()$path,"/",input$trap_create_date_textInput))
    showNotification("Date folder created", type = "message")
    rv$new_trap_date <- rv$new_trap_date + 1
    
  })
  
  
# END CREATE NEW FOLDERS ON DRIVE 
#--------------------------------------------------------------------------------------------------------
#Start obtain filenames/paths for trap file selectors  
  
  #list project folders
  project_names <- eventReactive(rv$new_trap_project,{
    projects <- dir(path = paste0(users_biophysr_folder(), "/trap"),
                          full.names = FALSE)
      return(projects)
    
  })
 
  #output for renderUI select input button with project names
  output$trap_project = renderUI({
    if(is_empty(project_names) == TRUE){
      selectInput('trap_project_selectInput', 'Select Project', c(Choose='', "Create New..." = "create", selectize = TRUE))
    } else {
      selectInput('trap_project_selectInput', 'Select Project', c(Choose='', "Create New..." = "create", project_names()), selectize = TRUE)
    }
  })
  
  #pick the user selected project folder
   trap_selected_project <-  reactive({
     full_paths <-dir(path = paste0(users_biophysr_folder(), "/trap"),
                                         full.names = TRUE)
     proj_tibble <- tibble(name = project_names(),
                           path = full_paths)
     user_selection <- proj_tibble %>% 
      dplyr::filter(name == input$trap_project_selectInput)

     return(user_selection)
  })
  
  #CONDITION
  #list of condition names in seleted project
 
  
  conditions_names <- reactive({ 
  conditions <- dir(trap_selected_project()$path, full.names = FALSE)
  return(conditions)
  })

  # make select input button with conditions names
   
  output$trap_conditions <-  renderUI({
    req(input$trap_project_selectInput)
    req(input$trap_project_selectInput != "create")
    
    # if(nrow(trap_selected_project() == 0)){
    # selectInput('trap_conditions_selectInput', 'Select Conditions', c(Choose='', "Create New..." = "create"), selectize = TRUE)
    #  }else{
    selectInput('trap_conditions_selectInput', 'Select Conditions', c(Choose='', "Create New..." = "create", conditions_names()), selectize = TRUE)
   #  }
  })

  #update choices when new folder is created
 
   observeEvent(rv$new_trap_condition, ignoreNULL = TRUE, ignoreInit = TRUE, {
    
     update_conditions <- dir(trap_selected_project()$path, full.names = FALSE)
  
     updateSelectizeInput(session, 'trap_conditions_selectInput',
                      'Select Conditions', choices = c(Choose='', "Create New..." = "create", update_conditions))
    
  })
   
  
  #4 selected condition
  trap_selected_conditions <-  reactive({
    
    full_paths_con <-dir(trap_selected_project()$path, 
                     full.names = TRUE)
    
    con_tibble <- tibble(name = conditions_names(),
                          path = full_paths_con)
    
    user_selection_con <- con_tibble %>% 
      dplyr::filter(name == input$trap_conditions_selectInput)
    
    return(user_selection_con)
    
  })
  
  #DATE
  
  #1 list of date names
  trap_date_names <- reactive({ 
    dir(trap_selected_conditions()$path, full.names = FALSE)
  })
  
  # make select input button with project names
  output$trap_date = renderUI({
    req(input$trap_conditions_selectInput)
    req(input$trap_conditions_selectInput != "create")
   # if(nrow(trap_selected_conditions() == 0)){
     # selectInput('trap_date_selectInput', 'Select Date', c(Choose='', "Create New..." = "create"), selectize = TRUE)
   # } else {
      selectInput('trap_date_selectInput', 'Select Date', c(Choose='', "Create New..." = "create", trap_date_names()), selectize = TRUE)
   # }
  })
  
  
  #update choices when new folder is created
  
  observeEvent(rv$new_trap_date, ignoreNULL = TRUE, ignoreInit = TRUE, {
    
    update_date <- dir(trap_selected_conditions()$path, full.names = FALSE)
    
    updateSelectizeInput(session, 'trap_date_selectInput',
                         'Select Date', choices = c(Choose='', "Create New..." = "create", update_date))
    
  })
  
  #4 selected date dribble
  trap_selected_date <-  reactive({
    
    full_paths_date <-dir(trap_selected_conditions()$path,
                         full.names = TRUE)
    
    date_tibble <- tibble(name = trap_date_names(),
                         path = full_paths_date)
    
    user_selection_date <- date_tibble %>% 
      dplyr::filter(name == input$trap_date_selectInput)
    
    return(user_selection_date)
  })
  
  #OBS
  #1 list of obs names
  trap_obs <- reactive({ 
    tibble(folder = dir(trap_selected_date()$path, full.names = FALSE)) %>% 
      filter(str_detect(folder, "obs_")) %>% 
      pull(folder)
  })
  
  # make select input button with project names
  output$trap_obs = renderUI({
    req(input$trap_date_selectInput)
    req(input$trap_date_selectInput != "create")
     if(is_empty(trap_obs()) == TRUE){
      selectInput('trap_obs_selectInput', label = NULL, c(Choose = '', "Make obs before continuing"), selectize = TRUE)
    } else {
      selectInput('trap_obs_selectInput', label = NULL, c(Choose='', trap_obs()), selectize = TRUE)
     }
  })
  
  # selected obs dribble
  trap_selected_obs <-  reactive({
    
    full_paths_obs <- tibble(all = dir(trap_selected_date()$path,
                          full.names = TRUE)) %>% 
      filter(str_detect(all, "obs_")) %>% 
      pull(all)
      
    
    obs_tibble <- tibble(name = trap_obs(),
                          path = full_paths_obs)
    
    user_selection_obs <- obs_tibble %>% 
      dplyr::filter(name == input$trap_obs_selectInput)
    
    return(user_selection_obs)
  })
  
  #files
  #1 tibble of file names
  trap_files <- reactive({ 
    tibble(name =  list.files(trap_selected_obs()$path, full.names = FALSE, pattern = "Data"),
           path = list.files(trap_selected_obs()$path, full.names = TRUE, pattern = "Data"))
  })

  #END obtain filenames/paths for trap file selectors  
#---------------------------------------------------------------------------------------------------------  
  # Start upload data / make trap observations
  #upload data and move to box sync
   observeEvent(input$trap_upload,{
    shiny_trap_upload(input_data = input$trap_txt_upload,
                      trap_selected_date = trap_selected_date())
  })
  
  
  #check if a date folder is properly selected
  observeEvent(input$make_observations_action_button, {
    if(is_empty(trap_selected_date()) == TRUE){
      showNotification("No 'Date' folder selected. Please select a folder with the folder chooser above.",
                       type = "error")
    } else {
      
      req(nchar(trap_selected_date()$path)>0)
      biophysr::shiny_make_trap_observations(trap_selected_date = trap_selected_date()$path,
                                             threshold = input$make_observations_numericInput)
    }
    
  })
  
  # End make trap observations
  #------------------------------------------------------------------------------------------------------------
  #Start prepare/clean data
  
  #print what folder user is working in
  output$trap_clean_data_save_location <- renderText({
    trap_selected_obs()$path
  })

  rv$clean_dygraph <- 0
  
  rv$update_graph <- 0
  #MOVE SHEETS to new obs
  observeEvent(input$trap_move_sheets_actionButton, {
    showModal(modalDialog(
      tagList(
        h4("Select an option to continue.")
      ), 
      title="Do you really want to move these file?",
      footer = tagList(actionButton("confirm_trap_move_sheets_actionButton", "Yes, move."),
                       modalButton("Cancel")
      )
    ))
  })
    
    
    observeEvent(input$confirm_trap_move_sheets_actionButton, {
    removeModal()
    shiny_move_trap(trap_selected_date = trap_selected_date(),
                    trap_obs = trap_obs(),
                    trap_files = trap_files(),
                    trap_selected_obs = trap_selected_obs(),
                    dygraph_clean_date_window_1 = input$dygraph_clean_date_window[[1]], 
                    dygraph_clean_date_window_2 = input$dygraph_clean_date_window[[2]])
    
    #rv$update_graph <- rv$update_graph + 1
  })

  
  
  trap_grouped_file <-  eventReactive(input$trap_clean_show_graph_actionButton, {
  current_obs <- trap_selected_obs()$path
  
  grouped_file <- list.files(current_obs, pattern = "grouped", full.names = TRUE)
  
  gf <- read_tsv(grouped_file, col_names = c("bead", "trap"))
  
  rv$clean_dygraph <- rv$clean_dygraph + 1
  
  return(gf)
  
  })
  
  
  output$trap_filter <- renderUI({
    
    sliderInput("trap_filter_sliderInput", 
                label = "Filter large dataset", 
                value = c(0, nrow(trap_grouped_file())),
                min = 0, 
                max = nrow(trap_grouped_file())/5000,
                width = "100%")
  })
 
  #dygraph clean and shave 
  dygraph_clean <- eventReactive(rv$clean_dygraph,{
   
    data <- tibble(seconds = 1:nrow(trap_grouped_file())/5000,
                   bead = trap_grouped_file()$bead)
    
    
    number_files <- nrow(data)/25000
    
    end_file <- seq(5, by = 5, length.out = number_files)
    
    add_labels <- function(x, events, ...){
      for(event in 1:length(events)){
        x <- dyEvent(x, events[[event]], paste0("F", event), ...)
      }
      x
    }
    
    if(input$hide_markers == "show"){
    
    dg <- dygraph(data,  ylab = "mV", xlab = "Seconds",  main = trap_selected_obs()$name) %>%
      dySeries("bead", color = "black") %>%
      dyRangeSelector(fillColor ="", strokeColor = "black") %>%
      add_labels(events = end_file, labelLoc = 'bottom', color = "black") %>%
      dyUnzoom() %>%
      dyOptions(axisLabelColor = "black",
                gridLineColor = "black",
                axisLineColor = "black",
                axisLineWidth = 3,
                axisLabelFontSize = 15,
                drawGrid = FALSE)
    
    return(dg)
    
    } else {
      
      dg <- dygraph(data,  ylab = "mV", xlab = "Seconds",  main = trap_selected_obs()$name) %>%
        dySeries("bead", color = "black") %>%
        dyRangeSelector(fillColor ="", strokeColor = "black") %>%
       # add_labels(events = end_file, labelLoc = 'bottom', color = "black") %>%
        dyUnzoom() %>%
        dyOptions(axisLabelColor = "black",
                  gridLineColor = "black",
                  axisLineColor = "black",
                  axisLineWidth = 3,
                  axisLabelFontSize = 15,
                  drawGrid = FALSE)
      
      
    }
    
  })
  
  output$dygraph_clean <- renderDygraph({
    dygraph_clean()
  })
  
  observeEvent(input$trap_clean_show_graph_actionButton, {
    addCssClass(id = "clean_col", class = "white")
  })
  
  #########
  
  move_from_index <- reactive({
  start_of_file_indices <- seq(0,
                               by = 5,
                               length.out = nrow(trap_files()))
  
  move_files_from <- round_any(input$dygraph_clean_date_window[[1]],
                               5,
                               f = floor)
  
  from_index <- which(start_of_file_indices == move_files_from)
  return(from_index)
  
  })
  
  move_to_index <- reactive({
  end_of_file_indices <- seq(5,
                             by = 5,
                             length.out = nrow(trap_files()))
  
  move_files_to <- round_any(input$dygraph_clean_date_window[[2]],
                             5,
                             f = ceiling)
  
  to_index <- which(end_of_file_indices == move_files_to)
  return(to_index)
  })
  
  #######
  output$move_files <- renderText({
    
   paste0("Create new observations with files ",
           move_from_index(),
           " to ",
           move_to_index()
           )
    
  })
  
  trim_from <- reactive({
    try(round_any(input$dygraph_clean_date_window[[1]], 0.0002, f = round))
  })
  
  trim_to <- reactive({
    
    try(round_any(input$dygraph_clean_date_window[[2]], 0.0002, f = round))
    
  })
  
  output$clipboard_copy_from <- renderUI({
    actionButton("copy_from_actionButton",
                 paste0("Copy Baseline Start (", as.character(trim_from()), ") to clipboard"),
                 icon = icon("clipboard"),
                 width = "100%")
    
  })
  
  observeEvent(input$copy_from_actionButton, {
    write_clip(as.character(trim_from()))
    
  })
  
  observeEvent(input$copy_to_actionButton, {
    write_clip(as.character(trim_to()))
    
  })
  
  output$clipboard_copy_to <- renderUI({
    actionButton("copy_to_actionButton",
                 paste0("Copy Baseline Stop (", as.character(trim_to()), ") to clipboard"),
                 icon = icon("clipboard"),
                 width = "100%")
    
  })
  
  
 
  
  output$trim_files <- renderText({
    
    paste0("Delete data from ",
           trim_from(),
           "s",
           " to ",
           trim_to(),
           "s"
           )
  })
  
  output$filter_text <- renderText({
   paste0("Push button to temporarily filter data")
    
  })
  
  output$dy_date_window_from <- renderText({
    paste0("From: ", input$dygraph_clean_date_window[[1]])
  })
  
  output$dy_date_window_to <- renderText({
    paste0("To: ", input$dygraph_clean_date_window[[2]])
  })
  
  #trim sheets data
  trim_dygraph <- observeEvent(input$trap_trim_dygraph_actionButton, {
    showModal(modalDialog(
      tagList(
        h4("This will delete the selected data.")
      ), 
      title="Do you really want ERASE the selection?",
      footer = tagList(actionButton("confirm_trap_trim_dygraph_actionButton", "Yes, cut."),
                       modalButton("Cancel")
      )
    ))
  })
  
  trim_dygraph <- observeEvent(input$confirm_trap_trim_dygraph_actionButton, {
    removeModal()
    shiny_trim_dygraph(trap_selected_obs = trap_selected_obs(),
                       trap_grouped_file = trap_grouped_file(), 
                       input_dygraph_clean_shave_date_window_1 = input$dygraph_clean_date_window[[1]], 
                       input_dygraph_clean_shave_date_window_2 =input$dygraph_clean_date_window[[2]])
        showNotification("Data trimmed. Please refresh graph.")              
  })
  
  #make new directions when button is pressed
  new_directions <-eventReactive(input$trap_new_directions_actionButton, {
    data <- tibble("Observation" = 1:length(trap_obs()),
                   "Baseline Start (seconds)" = rep("",length(trap_obs())),
                   "Baseline Stop (seconds)" = rep("", length(trap_obs())),
                   "Detrend" = rep("", length(trap_obs())),
                   "Include" = rep("", length(trap_obs())))
  })
  
  
  color_renderer <- "
  function(instance, td) {
    Handsontable.renderers.TextRenderer.apply(this, arguments);
    td.style.color = 'black';
  }
"
  
  #display directions table
  output$trap_directions <- renderRHandsontable({
    rhandsontable(new_directions(), stretchH = "all") %>% 
      hot_col(col = "Detrend", type = "dropdown", source = c("no", "yes"), render = color_renderer) %>% 
      hot_col(col = "Include", type = "dropdown", source = c("no", "yes"), render = color_renderer) %>% 
      hot_col(col = "Observation", type = "numeric", render = color_renderer) %>% 
      hot_col(col = "Baseline Start (seconds)", type = "numeric",  render = color_renderer) %>% 
      hot_col(col = "Baseline Stop (seconds)", type = "numeric",  render = color_renderer)
  })
  
 #save directions to 'obs' folder when save button pressed  
  observeEvent(input$trap_save_directions_actionButton, {
    write_csv(hot_to_r(input$trap_directions), paste0(trap_selected_date()$path, "/directions.csv"))
    showNotification("Directions saved.", type = "message")
  })
  
  
  #End prepare / clean data
  #------------------------------------------------------------------------------------------------------
  #Start laser trap analyzers
  
  #Check for valid folder
  observeEvent(input$mini_action_button, {
    if(is_empty(trap_selected_date()) == TRUE){
      showNotification("No Folder Selected. Please select a folder with the folder chooser above.",
                       type = "error")
    } else if(is_empty(trap_selected_date()) == FALSE){
      
      biophysr::shiny_mini_ensemble_analyzer(trap_selected_date = trap_selected_date(),
                                             mv2nm = as.numeric(input$mv2nm),
                                             nm2pn = as.numeric(input$nm2pn),
                                             run_mean_color = input$mini_col)
    }
  })
  
  observeEvent(input$hmm_action_button, {
    if(is_empty(laser_path()) == TRUE){
      showNotification("No Folder Selected. Please select a folder with the folder chooser above.",
                       type = "error")
    } else if(is_empty(laser_path()) == FALSE & str_sub(laser_path(), start = -12) != "observations"){
      showNotification("Not a valid folder. Please select an 'observations' folder to run analysis.",
                       type = "error")
    }
  })
  
  
  #HMM analysis
  hmm_analyzed <- eventReactive(input$hmm_action_button, {
    req(str_sub(laser_path(), start = -12) == "observations")
    biophysr::hidden_markov_analysis_shiny(parent_dir = laser_path(),
                                           mv2nm = as.numeric(input$mv2nm),
                                           nm2pn = as.numeric(input$nm2pn),
                                           overlay_color = input$mini_col)
    print("HMM Analysis Complete")
  })
  
  
  
  output$hmm_analysis <- renderText({
    hmm_analyzed()
  })
  
  
  # End laser analyzers
  #------------------------------------------------------------------------------------------------------------
  # Start Quality check
  
  #get dygraph names and path locations
  trap_dygraphs <- eventReactive(input$get_quality_check_data_actionButton, { 
    tibble(name = list.files(paste0(trap_selected_date()$path, "/results/plots"), pattern = ".html", full.names = FALSE, recursive = FALSE),
           path = list.files(paste0(trap_selected_date()$path, "/results/plots"), pattern = ".html", full.names = TRUE, recursive = FALSE))
  })
  
  # make select input button with dygraph names
  output$trap_quality_check_obs <- renderUI({
    req(input$trap_date_selectInput)
    req(input$trap_date_selectInput != "create")
   
      selectInput('trap_quality_check_obs_selectInput', label = 'Select obs to review', c(Choose = '', trap_dygraphs()$name), selectize = TRUE)
  })
  
  # selected dygraph for quality check
  trap_selected_quality_check <-  reactive({
    
   trap_dygraphs() %>% 
      filter(name == input$trap_quality_check_obs_selectInput)
  })
  
  observe({
    addResourcePath("user", users_biophysr_folder())
  })
    
  analysis_report_source <- eventReactive(input$show_quality_check_graph_actionButton, {
    
    paste0("user/trap/", 
           input$trap_project_selectInput,
           "/",
           input$trap_conditions_selectInput,
           "/",
           input$trap_date_selectInput,
           "/",
           "results/plots/",
          input$trap_quality_check_obs_selectInput)
    
  })
  output$analysis_report <- renderUI({
    tags$iframe(frameborder = "no", src = analysis_report_source(), width="100%", height = "600px", scrolling = "auto")
  })
  
  
  update_directions <- eventReactive(input$get_quality_check_data_actionButton, {
    dir_path <- list.files(trap_selected_date()$path, pattern = "directions", full.names = TRUE)
    
    dirs <- read_csv(dir_path, col_names = TRUE) %>% 
     dplyr::select(Folder, Report) %>% 
      mutate('Quality Control' = FALSE)
      
    
  })
  
  
  #display directions table
  output$update_directions <- renderRHandsontable({
    rhandsontable(update_directions(), stretchH = "all", height = 150) %>% 
      hot_col(col = "Folder", readOnly = TRUE, render = color_renderer) %>% 
      hot_col(col = "Report", readOnly = TRUE, render = color_renderer) 
     
  })
  
  
 
    output$manual_cal_input1 <- renderUI({
      req(input$trap_cal_choice == "manual")
      textInput("manual_step_cal", "Step Calibration (nm/mV)")
    })
    
    output$manual_cal_input2 <- renderUI({
      req(input$trap_cal_choice == "manual")
      textInput("manual_trap_stiffness", "Trap Stiffness (pN/nm)")
    })
    
  
 
  
  
  #------------------------------------------------------------------------------------------------------------
  #------------------------------------------------------------------------------------------------------------
  
  observeEvent(input$hmm_sim, {
    
    simulation_drive_hidden_markov_analysis_shiny(date_dribble = drive_trap4_selected_date_dribble(),
                                                  overlay_color = "red")
    
    
  })
 
 
 
} #server close







