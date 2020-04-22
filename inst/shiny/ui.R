suppressPackageStartupMessages({
  library(shiny)
  library(shinyWidgets)
  library(shinythemes)
  library(colourpicker)
  library(shinymanager)
  library(shinyjqui)
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
  library(dashboardthemes)
  library(shinydashboard)
  library(shinyjs)
  #library(smoother)
  library(magrittr)
  library(changepoint)
  library(depmixS4)
})

header <- dashboardHeader(
    title = "biophysr"

)

sidebar <- dashboardSidebar(
  sidebarMenu(


        menuItem("Home", tabName = "home", icon = icon("home")),
        menuItem("Motility",  tabName = "motility", icon = icon("tachometer-alt")),

        menuItem("Laser Trap", tabName = "trap",  icon = icon("dna"),
                 menuSubItem(text = "Initialize Data", tabName = "trap_init_data"),
                 menuSubItem(text = "Data Preparation", tabName = "trap_data_prep"),
                 menuSubItem(text = "Analyze", tabName = "trap_analyze"),
                 menuSubItem(text = "Summarize", tabName = "trap_summarize")),
        menuItem("   Dropbox", icon = icon("dropbox"), href = "https://www.dropbox.com/sh/9voaw8mj4qvcg4z/AABAguMz9PUdkWBcAp_wilEva?dl=0",
                 newtab = TRUE)
  ),

  switchInput(
    inputId = "show",
    label = "Folders",
    labelWidth = "100%",
    value = FALSE,
    onLabel = "Show",
    offLabel = "Hide",
    onStatus = "success",
    offStatus = "danger"
  ),
      uiOutput('trap_project'),
      conditionalPanel(
        condition = "input.trap_project_selectInput == 'create'",
        textInput("trap_create_project_textInput", "Name Project"),
        actionButton("trap_create_project_actionButton", "Create Project")
      ),#conditional panel close
   uiOutput('trap_conditions'),
  conditionalPanel(
    condition = "input.trap_conditions_selectInput == 'create'",
    textInput("trap_create_conditions_textInput", "Name Conditions"),
    actionButton("trap_create_conditions_actionButton", "Create Conditions")
  ),#conditional panel close

  uiOutput("trap_date"),
  conditionalPanel(
    condition = "input.trap_date_selectInput == 'create'",
    textInput("trap_create_date_textInput", "Enter Date", placeholder = "6 digits - MMDDYY"),
    actionButton("trap_create_date_actionButton", "Create Date")
  )#conditional panel close


)






body <- dashboardBody(shinyDashboardThemes(
    theme = "grey_dark"),
    useShinyjs(),
    # Add a CSS class for white text colour
    inlineCSS(list(.white = "background: #FAFAFA")),
        tabItems(
            tabItem(tabName = "home",
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
                           #textOutput("txt")
                           #h3("Page is currentlty under construction"),
                    ) # column close

            ) #  & fluidrow close

        ), # home tab panel close







        tabItem(tabName = "trap_init_data",


                fluidRow(column(3, box(width = NULL, collapsible = TRUE, collapsed = TRUE,
                             title = "Split Observations",
                    #h4(strong("Make Trap Observations")),
                    h5("1) Select Raw Data Files"),

                    fileInput("trap_txt_upload",
                              NULL,
                              multiple = TRUE,
                              accept = ".txt",
                              buttonLabel = "Browse...",
                              placeholder = "Select .txt"),

                    br(),
                    br(),
                    h5("2) Choose number of seconds to divide obs by"),
                    knobInput(inputId = "make_observations_numericInput",
                              label = NULL,
                              value = 20,
                              min = 10,
                              max = 30),


                    h5("3) Trap Calibration Files?"),
                    switchInput(inputId = "trap_cal_files",
                                label = NULL,
                                value = FALSE,
                                onLabel = "Yes",
                                offLabel =  "No",
                                onStatus = "success",
                                offStatus = "danger"),


                    h5("4) Click button to make observations"),
                    actionButton(inputId = "make_observations_action_button",
                                 label = "Make Observations",
                                 icon = icon("eye"),
                                 width = "100%")



                ) #box close,



            ), #colclose
                column(6,
                       box(title = "Calibration Plots",
                           width = NULL, collapsible = TRUE, collapsed = TRUE,
                           dropdownButton(

                             tags$h3("Trap Calibration"),

                             sliderInput("step_cal_stepsize",
                                         label = "Step Cal Step Size",
                                         value = 50,
                                         min = 0,
                                         max = 100,
                                         step = 10),
                             actionButton(inputId = "trap_cal_actionButton",
                                          label = "Calibrate Trap",
                                          icon = icon("sliders")),

                             circle = TRUE, status = "danger",
                             icon = icon("gear"), width = "300px",

                             tooltip = tooltipOptions(title = "Click to calibrate trap")
                           ),








                           jqui_resizable(plotOutput("step_cal_plot")) %>% withSpinner(type = 8, color = "#35ED35")

                       ) #box close
                       ), #col close
            column(3,
                   valueBoxOutput("step_cal_valueBox", width = NULL),
                   valueBoxOutput("equipartition_valueBox", width = NULL)


                ) #col close
                ) #row close
                   ), #tabItmen close

       tabItem(tabName = "trap_data_prep",
             #  fluidRow(column(12,
             #                  box(title = "Current Directory", width = NULL,
              #                     verbatimTextOutput("current_trap_date")
              #                 )
             #  )),
             fluidRow(
             box(width = 8, title = "Graph Options",
               fluidRow(
                   column(4,

                              h5("Select Obs"),
                              uiOutput("trap_obs")

                   ), #col close
                   column(4,
                          conditionalPanel(
                            condition =  " input.trap_obs_selectInput != null &&
                                              input.trap_obs_selectInput.length > 0 ",
                            h5("Import grouped file"),
                            actionButton("trap_load_clean_graph_actionButton",
                                         label ="Load Data",
                                         icon = icon("truck-loading"),
                                         width = "100%"))),

                   column(4,
                          conditionalPanel(
                            condition =  "   input.trap_obs_selectInput != null &&
                                               input.trap_obs_selectInput.length > 0 ",
                            h5("File Markers"),
                            radioGroupButtons(
                              inputId = "hide_markers",
                              label = NULL,
                              choices = c("Show" = "show",
                                          "Hide" = "hide"),
                              direction = "horizontal",
                              width = "100%",
                              justified = TRUE,
                              checkIcon = list(
                                yes = tags$i(class = "fa fa-check-square",
                                             style = "color: black"),
                                no = tags$i(class = "fa fa-square-o",
                                            style = "color: black"))
                            )

                          ) #conditional close
                   )#col close
                   ),
               #row close
               fluidRow(
                 column(8,
                        conditionalPanel(
                          condition = "input.trap_obs_selectInput != null &&
                                         input.trap_obs_selectInput.length > 0 ",

                          uiOutput("trap_filter"),

                        ) # conditionalclose

                 ), #col close

                   column(4,
                          conditionalPanel(
                              condition =  " input.trap_obs_selectInput != null &&
                                              input.trap_obs_selectInput.length > 0 ",
                              h5("Display/Refresh Graph"),
                              actionBttn(
                                inputId = "trap_clean_show_graph_actionButton",
                                label = "Graph",
                                style = "unite",
                                block = TRUE,
                                icon = icon("chart-line"),
                                size = "lg",
                                color = "success"
                              )


                          )#conditional close
                   ), #col close

               )#fluid Row close
               ), #boxclose


          box(title = "Cleaning Tools", width = 4,
               fluidRow(
                   column(12,

                          conditionalPanel(
                              condition = " input.trap_obs_selectInput != null &&
                                             input.trap_obs_selectInput.length > 0 ",

                              textOutput("move_files"),
                              actionButton("trap_move_sheets_actionButton",
                                           "Move",
                                           icon=icon("suitcase"),
                                           width = "100%"),


                          ) #conditional close
                   ) #col close
               ) ,
              br(),
                fluidRow(
                   column(12,
                          conditionalPanel(
                              condition = " input.trap_obs_selectInput != null &&
                                             input.trap_obs_selectInput.length > 0",

                              textOutput("trim_files"),
                              actionButton("trap_trim_dygraph_actionButton",
                                           "Cut",
                                           icon = icon("cut"),
                                           width = "100%")

                          ) #conditional close

                   )#col close
                   )#row close



               ) #ox close
       ),#row close
               br(),


               fluidRow(
                   column(12, id = "clean_col",

                          conditionalPanel(

                              condition = " input.trap_obs_selectInput != null &&
                                            input.trap_obs_selectInput.length > 0 &&
                                            input.trap_clean_show_graph_actionButton != '0'",
                              br(),
                              br(),
                              br(),
                              dygraphOutput("dygraph_clean") %>% withSpinner(type = 8, color = "#373B38"),
                              br(),
                              br(),
                              br(),




                          )
                   )),

               br(),

              box(width = 12, title = "Directions Controls",
               fluidRow(
                   column(3,
                          conditionalPanel(

                              condition = "input.trap_date_selectInput.length > 0",

                              actionButton("trap_new_directions_actionButton",
                                           "New Directions",
                                           icon = icon('directions'),
                                           width = "100%")

                          )),


                   column(3,
                          conditionalPanel(

                              condition = " input.trap_date_selectInput.length > 0",

                              actionButton("trap_save_directions_actionButton",
                                           "Save Directions",
                                           icon = icon("save"),
                                           width = "100%")

                          )
                   ),

                   column(3,
                          conditionalPanel(

                              condition = " input.trap_date_selectInput.length > 0",

                              uiOutput("clipboard_copy_from")

                          )
                   ),

                   column(3,
                          conditionalPanel(

                              condition = " input.trap_date_selectInput.length > 0",

                              uiOutput("clipboard_copy_to")

                          )
                   )

               )),#row close
               br(),

               fluidRow(
                   column(12,

                          conditionalPanel(
                              condition = " input.trap_new_directions_actionButton != 0",
                              rHandsontableOutput("trap_directions")
                          )
                   ) #col close
               ),  #row close
                br(),
                br(),
                br(),
                br()

      # )#tab close

       ),

      tabItem(tabName = "trap_analyze",

            fluidRow(
              box(title = "Analyzers", width = 4, collapsible = TRUE,

                                       radioGroupButtons(
                                              inputId = "trap_analyzer",
                                              label = "Select Analyzer",
                                                choices = c("Mini-Ensemble" = "mini",
                                                            "Hidden-Markov" = "hmm"),
                                              justified = TRUE,
                                               checkIcon = list(
                                                 yes = tags$i(class = "fa fa-circle",
                                                    style = "color: green"),
                                                  no = tags$i(class = "fa fa-circle-o",
                                                     style = "color: black"))
                                                                           ),
                  radioGroupButtons(
                    inputId = "trap_cal_choice",
                    label = "Select Calibration Method",
                    choices = c("Auto-cal" = "auto-cal",
                                "Manual" = "manual" ),

                    justified = TRUE,
                    checkIcon = list(
                      yes = tags$i(class = "fa fa-circle",
                                   style = "color: steelblue"),
                      no = tags$i(class = "fa fa-circle-o",
                                  style = "color: black"))
                  ),




                   textInput("manual_step_cal", "Step Calibration (nm/mV)", value = NULL, placeholder = NULL),
                    textInput("manual_trap_stiffness", "Trap Stiffness (pN/nm)", value = NULL, placeholder = NULL),



                  prettyRadioButtons(
                    inputId = "trap_file_type",
                    label = "Choose File Type",
                    choices = c("txt", "csv"),
                    icon = icon("check"),
                    bigger = TRUE,
                    inline = TRUE,
                    status = "info",
                    animation = "jelly"
                  ),

                  switchInput(
                    inputId = "emcontrol",
                    label = "EM Random Start",
                    onLabel = "Yes",
                    offLabel = "No",
                    onStatus = "success",
                    offStatus = "danger",
                    width = "100%"
                  ),


                  colourpicker::colourInput("trap_color", label = "Select color", showColour = "both"),
                  actionButton(inputId = "analyze_trap",
                               label = "Analyze Trap",
                               icon = icon("microscope"),
                               width = "100%")




     ), #box close


      box(title = "Quality Check", width = 4, collapsible = TRUE, collapsed = TRUE,
             # style = 'border-left: 1px solid grey',
                              h5("Click button to retrieve analysis results"),
                              actionButton("get_quality_check_data_actionButton",
                                           "Get Results",
                                           icon = icon("sync-alt"),
                                           width = "100%"),
                              br(),
                              uiOutput("trap_quality_check_obs"),
          actionButton(inputId = "show_quality_check_graph_actionButton",
                       label = "Display Results",
                       icon = icon("chart-line"),
                       width= "100%")


             ), #box close



      box(title = "Update Directions", width = 4, collapsible = TRUE, collapsed = TRUE,
                              h5("Check 'Quality Control' box to accept analysis"),

                              rHandsontableOutput("update_directions"),
                              br(),
          actionButton(inputId = "save_updated_directions",
                       label = " Save Updated Directions",
                       icon = icon("save"),
                       width = "100%")


             ) #box close

), #fluidrow close



      fluidRow(
        column(12,

               conditionalPanel(
                 condition = " input.trap_date_selectInput != null &&
                                input.trap_date_selectInput.length > 0 &&
                                input.show_quality_check_graph_actionButton != '0'",

                 br(),
                 br(),
                 htmlOutput("analysis_report") %>% withSpinner(type = 8, color = "#373B38"),
                 br(),
                 br()


               )
        ))
      ),#tab close,
      tabItem(tabName = "trap_summarize",
              fluidRow(column(2,
                              box(width = NULL, title = "Summarize Options",
                                  radioGroupButtons(
                                    inputId = "summarize_analyzer",
                                    label = "Select Analyzer",
                                    choices = c("Mini" = "mini",
                                                "HMM" = "hmm"),
                                    justified = TRUE,
                                    checkIcon = list(
                                      yes = tags$i(class = "fa fa-circle",
                                                   style = "color: green"),
                                      no = tags$i(class = "fa fa-circle-o",
                                                  style = "color: black"))
                                  ),
                                  prettyRadioButtons(
                                    inputId = "summarize_file_type",
                                    label = "Choose File Type",
                                    choices = c("txt", "csv"),
                                    icon = icon("check"),
                                    bigger = TRUE,
                                    inline = TRUE,
                                    status = "info",
                                    animation = "jelly"
                                  ),

                              actionBttn(inputId = "summarize_trap",
                                         label = "Summarize",
                                         icon = icon("calculator"),
                                         style = "fill",
                                         color = "warning",
                                         block = TRUE)
                              )

                            ),#col
                       column(10,
                              box(width = NULL, title = "Summary Table",
                                  tableOutput("trap_summary")))
                       )#row
              )#tab

            ) #tabITEMS close



        ) #dashboard body close



secure_app(head_auth = tags$script(inactivity),
           dashboardPage(header,
                         sidebar,
                         body)
)
