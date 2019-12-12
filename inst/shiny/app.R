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
ui = fluidPage(theme = shinytheme("slate"),
          
          navbarPage("Laser Trap Analysis",
                     
                     #Mini_ensmble trap Tab
                     tabPanel("Make Observations",
                              
                              sidebarLayout(
                                sidebarPanel(
                                  h4("Make Trap Observations"),
                  
                                  h5("1) Select folder that contains raw data (.txt)"),
                                  shinyDirButton('obs_dir', 'Folder select', 'Please select a folder'),
                                 br(),
                                 
                                  h5("2) Click button below to create observations"),
                                  actionButton(inputId = "make_observations_action_button",
                                               label = "Make Observations")
                                ), #sidebarPanel close
                                
                                mainPanel(
                                h4("Selected Folder:"),
                                verbatimTextOutput("make_obs_folder_selected"),
                                textOutput("obs_done")
                                )#Main Panel Close
                              )#Sidebar layout Close
                     ), # Tab panel close - make observations 
                     
                     #Mini_ensmble trap Tab
                     tabPanel("Mini-Ensemble Trap",
                              
                              sidebarLayout(
                                sidebarPanel(
                                  shinyDirButton('mini_dir', "Select 'observations' folder", 'Please select a folder'),
                                
                                  textInput(inputId = "mv2nm", label = "nm/mV conversion", value = "", width = NULL,
                                            placeholder = "31 nm/mV (Fall 2019"),
                                  
                                  textInput(inputId = "nm2pn", label = "pN/nm conversion", value = "" , width = NULL,
                                            placeholder = "0.04 pN/nm (Fall 2019"),
                                  
                                  colourInput("mini_col", "Select colour"),
                                  
                          
                                  actionButton(inputId = "mini_action_button",
                                               label = "Analyze")
                                  
                                  
                                ), #mini sidebar layout close
                                
                                
                                mainPanel(
                                  h4("Selected Folder:"),
                                  verbatimTextOutput("mini_folder_selected"),
                                  textOutput("mini_analyzed")
                                 
                                ) #main panel close
                                
                              )
                     ), #mini-ensemble tab close
                     
                     tabPanel("Single-Molecule Trap",
                              print("This is for single molecule analysis")
                     ) #single molecule tab panel close
          )#navbar page close
)#fluidpage close


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
# START MAKE TRAP OBSERVATIONS
# Get input folder and display selected folder

  obs_volumes <- getVolumes()
  shinyDirChoose(input, 'obs_dir', roots=obs_volumes, session=session)
  obs_path <- reactive({
    return(print(parseDirPath(obs_volumes, input$obs_dir)))
  })
  
  output$make_obs_folder_selected <- renderText({
    obs_path()
  })  
  
#Make the trap oservations & display text when complete
  
  obs <- eventReactive(input$make_observations_action_button, {
        req(nchar(obs_path())>0)
        biophysr::make_trap_observations_shiny(obs_path())
        obs <- "Done! Observations Created in Selected Folder"
      })

    output$obs_done <-  renderText({
        obs()
    })
    
# END MAKE TRAP OBSERVATIONS
#------------------------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------------------------  
#------------------------------------------------------------------------------------------------------------  
#START MINI ENSEMBLE ANALYSIS
    #get folder
    mini_volumes <- getVolumes()
    shinyDirChoose(input, 'mini_dir', roots=mini_volumes, session=session)
    mini_path <- reactive({
      return(print(parseDirPath(mini_volumes, input$mini_dir)))
    })
    
    output$mini_folder_selected <- renderText({
      mini_path()
    })  
    
    #analyze mini
    mini_analyzed <- eventReactive(input$mini_action_button, {
      #print(as.numeric(input$mv2nm))
      #print(as.numeric(input$nm2pn))
      #print(input$mini_col)
      req(nchar(mini_path())>0)
      biophysr::mini_ensemble_analyzer_shiny(parent_dir = mini_path(),
                                             mv2nm = as.numeric(input$mv2nm),
                                             nm2pn = as.numeric(input$nm2pn), 
                                             run_mean_color = input$mini_col
                                             )
      mini_done <- "Done! Analysis Complete & Data Saved to 'Results' folder in selected directory"
    })
    
    output$mini_analyzed <-  renderText({
      mini_analyzed()
    })
    
    
      
    } #server close



shinyApp(ui = ui, server = server)