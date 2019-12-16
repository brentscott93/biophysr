library(shiny)
library(shinyFiles)
library(shinythemes)
library(colourpicker)
library(gtools)
suppressPackageStartupMessages(library(gridExtra))
library(readr)
library(readxl)
suppressPackageStartupMessages(library(zoo))
suppressPackageStartupMessages(library(pracma))
suppressPackageStartupMessages(library(tidyverse))
library(knitr)
library(rmarkdown)
library(biophysr)

### ui end, to browse to desired folder
ui = fillPage(theme = shinytheme("slate"),

              navbarPage("biophysr",
                tabPanel("Home",
                         fluidRow(
                                  column(4),

                                  column(4, align = "center",
                                         br(),
                                         br(),
                                         br(),
                                         br(),
                                         br(),
                                         br(),
                                         br(),
                                         h1(strong("biophysr")),
                                         h5("analysis for muscle biophysics"),
                                         br(),
                                         #h3("Page is currentlty under construction"),
                                      ), # column close
                                  column(4)
                                  ) #  & fluidrow close

                         # fluidRow(

                         # column(12, align = "right",
                         # style="display: inline-block;",
                         # tags$video(id = "motility",
                         #     src = "motility.mp4",
                         #      type = "video/mp4",
                         # #         autoplay = NA, controls = NA,
                         #        width = "400px"
                         ###          #height = "250px"
                         #     )


                         #  )),#column close & row
                         #  fluidRow(


                         # column(12, align = "right",
                         #    style="display: inline-block;",
                         #    img(src = "Animated-Myosin-Ensemble-Laser-Trap-Unregulated-with-trace.gif",
                         #width = "500px",
                         #      height = "250px")
                         #    )),#col close & row
                         #  br()),
                         #tab panel close
                ), # home tab panel close

                         tabPanel("Motility"),

                         #Mini_ensmble trap Tab
                         tabPanel("Laser Trap",
                           fluidRow(column(12,
                                           h2("Laser Trap Analysis")
                                           )#column close
                                    ),  #row close



                           fluidRow(column(12,
                                           style = 'border-top: 2px solid grey',)),

                           fluidRow(column(2,
                                           # style='border-top: 2px solid grey',

                                           h3("Select folder"))),
# SELECT FOLDER ROW
                           fluidRow(column(2,
                                           style = 'border-right: 1px solid grey',
                                            shinyDirButton('laser_folder_select',
                                                           'Choose',
                                                           'Please select a folder',
                                                            icon = icon("folder"))

                                         ), #columns close

                                     column(10,
                                             verbatimTextOutput("laser_folder_select")

                                         ), # column close
                                    ), #fluid row close


                             br(),
# PART 1
                             fluidRow(column(12,
                                             style = 'border-bottom: 2px solid grey',
                                             h3("Part 1 - Data Preperation")
                                            ) # column close
                                      ), # row close


                             br(),

#MAKE OBSERVATIONS
                             fluidRow(column(3,
                                             h4("Make Trap Observations"),
                                             h5("1) Select folder that contains raw data (.txt files) using folder selector above"),
                                             br(),
                                             h5("2) Click button below to create observations"),
                                             br(),
                                             actionButton(inputId = "make_observations_action_button",
                                                          label = "Make Observations",
                                                          icon = icon("eye"))

                                              ),  #column close
#CLEAN DATA
                                    column(3,
                                           style = 'border-left: 1px solid grey',
                                           h4("Clean Data"),
                                           h5("1) Use pCLAMP to prepare data"),
                                           h5("2) Fill-out directions"),
                                           h6("For detailed instructions see:"),
                                           h6(em("link to protocol here")),
                                           h6("Too see example of complete 'directions.csv' click here:"),
                                           actionButton('directions', "Directions Demo")
                                         ),   #column close,



                                     column(3,
                                            style = 'border-left: 1px solid grey',
                                            h4("Trap Calibrations"),
                                            textInput(inputId = "mv2nm",
                                                      label = "nm/mV conversion",
                                                      value = "",
                                                      width = "75%",
                                                      placeholder = "31 nm/mV (Fall 2019)"),


                                             textInput(inputId = "nm2pn",
                                                       label = "pN/nm conversion",
                                                       value = "" ,
                                                       width = "75%",
                                                       placeholder = "0.04 pN/nm (Fall 2019)"   )


                                            ),   #column close

#SELECT COLOR
                                     column(3,
                                            style = 'border-left: 1px solid grey',
                                            h4("Select Plot Color"),
                                            colourInput("mini_col", "Color Selected:"),
                                            br(),
                                            br(),
                                            br(),
                                            br()
                                            ) #columns close

                                      ),       #fluid row close

                             br(),

                             fluidRow(column(12,
                                             style = 'border-bottom: 2px solid grey',
                                             h3("Part 2 - Analyze"))),
                              br(),

#Mini_ensmble trap Tab
                             fluidRow(column(4,
                                            style = 'border-right: 1px solid grey',
                                            tabsetPanel(
                                                tabPanel("Mini-Ensemble",
                                                      h4("Mini-Ensemble Analyzer"),
                                                      h5("1) Select the 'observations' folder that contains the 'obs_0#' folders and completed directions using the folder selector above"),
                                                      h5("2) Fill out trap calibrations and choose plot color"),
                                                      h5("3) Click button to analyze"),
                                                      actionButton(inputId = "mini_action_button",
                                                                   label = "Analyze Mini",
                                                                   icon = icon("microscope"))
                                                      ), #close tabPanel


                                                tabPanel("Hidden Markov",
                                                      h4("Hidden Markov Analyzer"),
                                                      h5("1) Select the 'observations' folder that contains the 'obs_0#' folders and completed directions using the folder selector above"),
                                                      h5("2) Fill out trap calibrations and choose plot color"),
                                                      h5("3) Click button to analyze"),
                                                      actionButton(inputId = "hmm_action_button",
                                                                   label = "Analyze HMM",
                                                                   icon = icon("microscope"))
                                                        ),# close hmm tab

                                                       br()
                                                     ) #tabsetPanel close
                                              ), #column close

                                      column(4,
                                             h4("Summarize Data"),
                                             h5("1) Select condition folder to summarize using the folder selector above"),
                                             h5("2) Choose analyzer that was used"),
                                             radioButtons(inputId = "summarize_radio_button",
                                                          label = NULL,
                                                          choices = c("Mini-Ensemble", "Hidden-Markov"),
                                                          inline = TRUE),
                                             h5("3) Click summarize to obtain means and std. errors"),
                                           actionButton(inputId = "summarize",
                                                        label = "Summarize",
                                                        icon = icon("calculator"))
                                               ) # columns close

                               ) #fluid row close
                             ) #trap tab close
                           ) #navbar page
                        ) #fill/fluid page close


# END UI
#############################################################################################################
#############################################################################################################
#############################################################################################################
#############################################################################################################
#############################################################################################################
#############################################################################################################
# START SERVER

server = function(input, output, session) {

  options(shiny.maxRequestSize=30*1024^2)
  #------------------------------------------------------------------------------------------------------------
  #------------------------------------------------------------------------------------------------------------
  #------------------------------------------------------------------------------------------------------------
##MOTILITY SERVER START##









  ##MOTILITY SERVER END##
  #------------------------------------------------------------------------------------------------------------
  #------------------------------------------------------------------------------------------------------------
  #------------------------------------------------------------------------------------------------------------
  ##TRAP SERVER START##

  # Start make trap observations
  # Get input folder and display selected folder

  laser_volumes <- getVolumes()
  shinyDirChoose(input, 'laser_folder_select', roots=laser_volumes, session=session)
  laser_path <- reactive({
    return(print(parseDirPath(laser_volumes, input$laser_folder_select)))
  })

  output$laser_folder_select <- renderText({
    laser_path()
  })

  #check if a folder is selected
  observeEvent(input$make_observations_action_button, {
    if(is_empty(laser_path()) == TRUE){
      showNotification("No Folder Selected. Please select a folder with the folder chooser above.",
                       type = "error")
    }
  })

  #Make the trap oservations fuction

  obs <- eventReactive(input$make_observations_action_button, {
    req(nchar(laser_path())>0)
    biophysr::make_trap_observations_shiny(laser_path())
  })


  # End make trap observations
  #------------------------------------------------------------------------------------------------------------
  #Start laser trap analyzers


 #Check for valid folder
  observeEvent(input$mini_action_button, {
    if(is_empty(laser_path()) == TRUE){
      showNotification("No Folder Selected. Please select a folder with the folder chooser above.",
                       type = "error")
    } else if(is_empty(laser_path()) == FALSE & str_sub(laser_path(), start = -12) != "observations"){
      showNotification("Not a valid folder. Please select an 'observations' folder to run analysis.",
                       type = "error")
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

  #analyze mini
  mini_analyzed <- eventReactive(input$mini_action_button, {

    req(str_sub(laser_path(), start = -12) == "observations")
    biophysr::mini_ensemble_analyzer_shiny(parent_dir = laser_path(),
                                           mv2nm = as.numeric(input$mv2nm),
                                           nm2pn = as.numeric(input$nm2pn),
                                           run_mean_color = input$mini_col)

  })

  #HMM analysis
  hmm_analyzed <- eventReactive(input$hmm_action_button, {
  req(str_sub(laser_path(), start = -12) == "observations")
  biophysr::hidden_markov_analysis(parent_dir = laser_path(),
                                         mv2nm = as.numeric(input$mv2nm),
                                         nm2pn = as.numeric(input$nm2pn),
                                         overlay_color = input$mini_col)
  })


  # End laser analyzers
  #------------------------------------------------------------------------------------------------------------
  #------------------------------------------------------------------------------------------------------------
  #------------------------------------------------------------------------------------------------------------



} #server close



shinyApp(ui = ui, server = server)
