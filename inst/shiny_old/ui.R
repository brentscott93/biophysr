suppressPackageStartupMessages({
library(shiny)
library(shinyFiles)
library(shinyWidgets)
library(shinyjs)
library(shinythemes)
library(colourpicker)
library(shinymanager)
library(shinycssloaders)
library(gtools)
library(gridExtra)
library(pracma)
library(knitr)
library(dygraphs)
library(rmarkdown)
library(plotrix)
library(rhandsontable)
library(RColorBrewer)
library(filesstrings)
library(clipr)
library(biophysr)
library(tidyverse)
library(depmixS4)
})

#start app
ui <- secure_app(head_auth = tags$script(inactivity),
                 fluidPage(theme = shinytheme("slate"),
                           useShinyjs(), # Set up shinyjs
                           # Add a CSS class for white text colour
                           inlineCSS(list(.white = "background: #FAFAFA")),
                           navbarPage("biophysr",
                                      tabPanel("Home",
                                               fluidRow(
                                                 column(4),

                                                 column(4, align = "center",
                                                        br(),
                                                        br(),
                                                        #h1(strong("biophysr")),
                                                        #h5("analysis for muscle biophysics"),
                                                        br(),
                                                        br(),
                                                        img(src = "bp3.png"),
                                                        br(),

                                                        br(),
                                                        textOutput("current_user_comment")
                                                        #h3("Page is currentlty under construction"),
                                                 ), # column close
                                                 column(4)
                                               ) #  & fluidrow close

                                      ), # home tab panel close

                                      tabPanel("Motility"),

                                      #Mini_ensmble trap Tab
                                      tabPanel("Laser Trap",
                                               fluidRow(column(12,
                                                               h2("Laser Trap Analysis")
                                               )#column close
                                               ),  #row close



                                               fluidRow(column(12,
                                                               style = 'border-top: 2px solid grey'
                                               )),
                                               br(),

                                               br(),

                                               fluidRow(column(3,
                                                               h4(strong("Tasks"))
                                               ),
                                               column(9,
                                                      h4(strong("Create and select folders to work in"))
                                               )),

                                               fluidRow(  column(3,
                                                                 radioButtons("trap_tasks_radioButtons", label = NULL,
                                                                              choices = c("None" = "none",
                                                                                          "Make Observations" = "obs",
                                                                                          "Clean Data" = "clean",
                                                                                          "Analyze" = "analyze",
                                                                                          "Summarize" = "summarize"))
                                               ), #columns close
                                               column(3,

                                                      style = 'border-left: 1px solid grey',
                                                      uiOutput("trap_project"),
                                                      conditionalPanel(
                                                        condition = "input.trap_project_selectInput == 'create'",
                                                        textInput("trap_create_project_textInput", "Name Project"),
                                                        actionButton("trap_create_project_actionButton", "Create Project")
                                                      )#conditional panel close

                                               ), #column close

                                               column(3,
                                                      uiOutput("trap_conditions"),
                                                      conditionalPanel(
                                                        condition = "input.trap_conditions_selectInput == 'create'",
                                                        textInput("trap_create_conditions_textInput", "Name Conditions"),
                                                        actionButton("trap_create_conditions_actionButton", "Create Conditions")
                                                      )#conditional panel close

                                               ), #column close

                                               column(3,
                                                      uiOutput("trap_date"),
                                                      conditionalPanel(
                                                        condition = "input.trap_date_selectInput == 'create'",
                                                        textInput("trap_create_date_textInput", "Enter Date", placeholder = "6 digits - MMDDYY"),
                                                        actionButton("trap_create_date_actionButton", "Create Date")
                                                      )#conditional panel close


                                               )#col close
                                               ), #fluid row close

                                               fluidRow(column(12,
                                                               style = 'border-top: 1px solid grey'
                                               )),

                                               fluidRow(column(12,
                                                               conditionalPanel(
                                                                 condition = "input.trap_tasks_radioButtons == 'obs'",
                                                                 h4(strong("Make Trap Observations")))
                                               )),

                                               fluidRow(column(3,

                                                               conditionalPanel(
                                                                 condition = "input.trap_tasks_radioButtons == 'obs'",
                                                                 wellPanel(
                                                                 #h4(strong("Make Trap Observations")),
                                                                 h5("1) Select Raw Data Files"),

                                                                 fileInput("trap_txt_upload",
                                                                           NULL,
                                                                           multiple = TRUE,
                                                                           accept = ".txt",
                                                                           buttonLabel = "Browse...",
                                                                           placeholder = "Select .txt"),



                                                                 h5("2) Move raw data to Dropbox"),

                                                                 actionButton(inputId = "trap_upload",
                                                                              label = "Upload to Dropbox",
                                                                              icon = icon("file-upload"),
                                                                              width = "100%"),
                                                                 br(),
                                                                 br(),
                                                                 h5("3) Choose number of seconds to divide obs by"),
                                                                 knobInput(inputId = "make_observations_numericInput",
                                                                              label = NULL,
                                                                              value = 20,
                                                                              min = 10,
                                                                              max = 30),


                                                                 h5("4) Toggle auto-cal"),
                                                                 switchInput(inputId = "trap_cal_files",
                                                                             label = NULL,
                                                                             value = FALSE,
                                                                             onStatus = "success",
                                                                             offStatus = "danger"),


                                                                 h5("5) Click button to make observations"),
                                                                 actionButton(inputId = "make_observations_action_button",
                                                                              label = "Make Observations",
                                                                              icon = icon("eye"),
                                                                              width = "100%")
                                                                 )
                                                               )
                                               ),

                                                        column(9,
                                                               plotOutput("trap_cal")
                                                        )#col close
                                               ),
                                               #row close


                                               fluidRow(
                                                 column(12,  conditionalPanel(

                                                 condition = "input.trap_tasks_radioButtons == 'clean'",
                                                 h4(strong("Clean Data")))
                                                 # h3("Toolbar"))#conditional close
                                               )#col close
                                               ),#row close



                                               fluidRow(
                                                 column(3,
                                                               conditionalPanel(
                                                                 condition = "input.trap_tasks_radioButtons == 'clean'",
                                                                 h5("Select Obs"),
                                                                 uiOutput("trap_obs"))

                                               ), #col close

                                               column(3,
                                                      conditionalPanel(
                                                        condition =  "input.trap_tasks_radioButtons == 'clean' &&
                                                                      input.trap_obs_selectInput != null &&
                                                                       input.trap_obs_selectInput.length > 0 ",
                                                        h5("Display/Refresh Graph"),
                                                           actionButton("trap_clean_show_graph_actionButton",
                                                                        label ="Graph",
                                                                   icon = icon("chart-line"),
                                                                    width = "100%")
                                                      )#conditional close
                                               ), #col close

                                               column(2,
                                                      conditionalPanel(
                                                        condition =  "input.trap_tasks_radioButtons == 'clean' &&
                                                                      input.trap_obs_selectInput != null &&
                                                                       input.trap_obs_selectInput.length > 0 ",
                                                        h5("File Markers"),
                                                        radioButtons("hide_markers",
                                                                     label = NULL,
                                                                     choices = c("Show" = "show",
                                                                                 "Hide" = "hide"),
                                                                     inline = TRUE,
                                                                     width = "100%")
                                                      )#conditional close
                                               ), #col close
                                               column(4,
                                                      conditionalPanel(
                                                        condition = "input.trap_tasks_radioButtons == 'clean' &&
                                                                      input.trap_obs_selectInput != null &&
                                                                       input.trap_obs_selectInput.length > 0 ",

                                                        uiOutput("trap_filter")

                                                      ) # conditionalclose

                                               ) #col close


                                               ), #row close

                                               fluidRow(
                                                 column(4,

                                                               conditionalPanel(
                                                                condition = "input.trap_tasks_radioButtons == 'clean' &&
                                                                      input.trap_obs_selectInput != null &&
                                                                       input.trap_obs_selectInput.length > 0 ",

                                                              textOutput("move_files"),
                                                              actionButton("trap_move_sheets_actionButton",
                                                                              "Move",
                                                                              icon=icon("suitcase"),
                                                                              width = "100%"),


                                                              ) #conditional close
                                              ), #col close

                                              column(4,
                                                 conditionalPanel(
                                                      condition = "input.trap_tasks_radioButtons == 'clean' &&
                                                                      input.trap_obs_selectInput != null &&
                                                                       input.trap_obs_selectInput.length > 0",

                                                      textOutput("trim_files"),
                                                      actionButton("trap_trim_dygraph_actionButton",
                                                               "Cut",
                                                               icon = icon("cut"),
                                                               width = "100%")

                                                    ) #conditional close

                                             ), #col close


                                             column(4,

                                                    conditionalPanel(
                                                      condition = "input.trap_tasks_radioButtons == 'clean' &&
                                                                      input.trap_obs_selectInput != null &&
                                                                       input.trap_obs_selectInput.length > 0 ",

                                                      textOutput("filter_text"),
                                                      actionButton("filter_trap_grouped",
                                                                   label = "Filter",
                                                                 icon=icon("filter"),
                                                                 width = "100%")
                                                    ) #con close
                                             ) #col close

                                               ), #row close
                                               br(),


                                               fluidRow(
                                                 column(12, id = "clean_col",

                                                        conditionalPanel(

                                                        condition = "input.trap_tasks_radioButtons == 'clean' &&
                                                                      input.trap_obs_selectInput != null &&
                                                                      input.trap_obs_selectInput.length > 0 &&
                                                                      input.trap_clean_show_graph_actionButton != '0'",
                                                          br(),
                                                        br(),
                                                        br(),
                                                          dygraphOutput("dygraph_clean") %>% withSpinner(type = 6, color = "#373B38"),
                                                       br(),
                                                       br(),
                                                       br(),




                                                        )
                                                 )),

                                             br(),

                                          fluidRow(
                                            column(3,
                                                   conditionalPanel(

                                                     condition = "input.trap_tasks_radioButtons == 'clean' &&
                                                      input.trap_date_selectInput.length > 0",

                                                     actionButton("trap_new_directions_actionButton",
                                                                  "New Directions",
                                                                  icon = icon('directions'),
                                                                  width = "100%")

                                                   )),


                                            column(3,
                                                   conditionalPanel(

                                                     condition = "input.trap_tasks_radioButtons == 'clean' &&
                                                      input.trap_date_selectInput.length > 0",

                                                     actionButton("trap_save_directions_actionButton",
                                                                  "Save Directions",
                                                                  icon = icon("save"),
                                                                  width = "100%")

                                                   )
                                            ),

                                            column(3,
                                                   conditionalPanel(

                                                     condition = "input.trap_tasks_radioButtons == 'clean' &&
                                                      input.trap_date_selectInput.length > 0",

                                                     uiOutput("clipboard_copy_from")

                                                   )
                                            ),

                                            column(3,
                                                   conditionalPanel(

                                                     condition = "input.trap_tasks_radioButtons == 'clean' &&
                                                      input.trap_date_selectInput.length > 0",

                                                     uiOutput("clipboard_copy_to")

                                                   )
                                            )

                                            ),#row close
                                          br(),

                                               fluidRow(
                                                 column(12,

                                                        conditionalPanel(
                                                          condition = "input.trap_tasks_radioButtons == 'clean' &&
                                                          input.trap_new_directions_actionButton != 0",
                                                          rHandsontableOutput("trap_directions")
                                                        )
                                                 ) #col close
                                               ), #row close

                                          br(),


                                               fluidRow(column(3,

                                                               conditionalPanel(condition = "input.trap_tasks_radioButtons == 'analyze'",
                                                                                h3("Analyze")
                                                               ))),
                                               fluidRow(column(4,
                                                               style = 'border-right: 1px solid grey',

                                                               conditionalPanel(condition = "input.trap_tasks_radioButtons == 'analyze'",
                                                                                h4("Run Analysis"),
                                                                                radioButtons("trap_analyzer",
                                                                                             "Select Analyzer",
                                                                                             choices = c("Mini-Ensemble" = "mini",
                                                                                                         "Hidden-Markov" = "hmm"),
                                                                                             inline = TRUE),
                                                                                radioButtons("trap_cal_choice",
                                                                                             "Select Calibration Method",
                                                                                             choices = c("Auto-cal" = "auto-cal",
                                                                                                         "Manual" = "manual"
                                                                                                         ),
                                                                                             inline = TRUE),
                                                                                uiOutput("manual_cal_input1"),
                                                                                uiOutput("manual_cal_input2"),
                                                                              colourpicker::colourInput("trap_color", label = "Select color", showColour = "both")



                                                               ) #condition
                                               ), #col,


                                               column(4,
                                                      # style = 'border-left: 1px solid grey',
                                                      conditionalPanel(condition = "input.trap_tasks_radioButtons == 'analyze'",
                                                                       h4("Quality Check Analysis"),
                                                                       h5("1) Click button to retrieve analysis results"),
                                                                      actionButton("get_quality_check_data_actionButton",
                                                                                   "Get Results",
                                                                                   icon = icon("sync-alt")),
                                                                      br(),
                                                                      uiOutput("trap_quality_check_obs"),


                                                      ) #con close
                                               ), # columns close


                                               column(4,
                                                      style = 'border-left: 1px solid grey',
                                                      conditionalPanel(condition = "input.trap_tasks_radioButtons == 'analyze'",
                                                                       h4("Update Directions"),
                                                                       h5("Check 'Quality Control' box to accept analysis"),

                                                                       rHandsontableOutput("update_directions"),
                                                                       br()


                                                      ) #con close
                                               ) # columns close
                                               ), #row close

                                          fluidRow(

                                            column(4,
                                                   conditionalPanel(condition = "input.trap_tasks_radioButtons == 'analyze'",
                                                   actionButton(inputId = "analyze",
                                                                label = "Analyze Trap",
                                                                icon = icon("microscope"),
                                                                width = "80%")
                                                   )
                                            ),
                                            column(4,
                                                   conditionalPanel(condition = "input.trap_tasks_radioButtons == 'analyze'",
                                                   actionButton(inputId = "show_quality_check_graph_actionButton",
                                                                label = "Display Results",
                                                                icon = icon("chart-line"),
                                                                width= "80%")
                                                   )
                                                   ),
                                            column(4,
                                                   conditionalPanel(condition = "input.trap_tasks_radioButtons == 'analyze'",
                                                   actionButton(inputId = "save_updated_directions",
                                                                label = " Save Updated Directions",
                                                                icon = icon("save"),
                                                                width = "80%")
                                                   )
                                            )
                                          ), #row close



                                          fluidRow(
                                            column(12,

                                                   conditionalPanel(
                                                     condition = "input.trap_tasks_radioButtons == 'analyze' &&
                                                                      input.trap_date_selectInput != null &&
                                                                      input.trap_date_selectInput.length > 0 &&
                                                                      input.show_quality_check_graph_actionButton != '0'",

                                                  br(),
                                                  br(),

                                                     htmlOutput("analysis_report"),
                                                  br(),
                                                  br()


                                                   )
                                            )),

                                       #   column(4,
                                         #        conditionalPanel(condition = "input.trap_tasks_radioButtons == 'analyze'",
                                          #                        h4("Summarize Data"),
                                          #                        h5("1) Select condition folder to summarize using the folder selector above"),
                                          #                        h5("2) Choose analyzer that was used"),
                                           #                       radioButtons(inputId = "summarize_radio_button",
                                            #                                   label = NULL,
                                             #                                  choices = c("Mini-Ensemble", "Hidden-Markov"),
                                              #                                 inline = TRUE),
                                               #                   h5("3) Click summarize to obtain means and std. errors"),
                                                #                  actionButton(inputId = "summarize",
                                                 #                              label = "Summarize",
                                                  #                             icon = icon("calculator"))
                                                # ) #con close
                                      #    ) # columns close
                                      #), #row close


                                      ) #trap tab close
                           ) #navbar page
                 ) #fill/fluid page close

) #secure app close
# END UI
