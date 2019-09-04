library(shiny)
library(tidyverse)
library(biophysr)
library(shinythemes)


# Define UI for app  ----
ui <- fluidPage(theme = shinytheme("spacelab"),

  navbarPage("Muscle Biophysics Lab"),

  # App title ----
  titlePanel("Automated Motility Analysis"),

  # Sidebar layout with input and output definitions ----
  sidebarLayout(

    # Sidebar panel for inputs ----
    sidebarPanel(

      # Input: File Input for uploading raw motility sheets ----
      fileInput(inputId = "file",
                  label = "Upload motility files (.xls)",
                  multiple = TRUE,
                  accept = ".xls",
                  buttonLabel = "Browse...",
                  placeholder = "No files selected"),

      radioButtons(inputId = "checkbox",
      label = "Myosin Type:",
      choiceNames = list("Skeletal", "Cardiac"),
      choiceValues = list("skeletal", "cardiac"),
      inline = TRUE),

      downloadButton("download_by_video",
                     "Download 'By Video'"),

      downloadButton("download_summary",
                     "Download Summary")
    ),


    # Main panel for displaying outputs ----
    mainPanel(

      tabsetPanel(type = "tabs",
                  tabPanel("By Video", tableOutput("video_table_output")),
                  tabPanel("Summary", tableOutput("summary_table_output"))

      )



    )
  ))



# Define server logic required to draw a histogram ----
server <- function(input, output) {


  data_by_video <- reactive({


    filenames <- input$file$name
    data_upload <- map(input$file$datapath, read.delim)
    for (i in seq_along(data_upload)){
      data_upload[[i]]<-cbind(data_upload[[i]], filenames[i])}

    combined <- bind_rows(data_upload) %>%
      separate('filenames[i]', c("Condition", "Rest"), sep = "_00") %>%
      separate("Rest", c("Video", "Extenstion"), sep = "_")

    data_analyzed <- combined %>%
      group_by(Condition, Video)%>%
      analyze_motility() %>%
      rename("Velocity" = average_velocity,
             "Percent_Moving" =  percent_moving,
             "Moving_Filaments" = moving_filaments,
             "Total_Filaments" = total_filaments)

  })

  output$video_table_output <- renderTable({
    if(is.null(input$file)) return(tibble("No data currently selected" =  "Please use 'Browse...' to upload"))
    else return(data_by_video())
  })



  data_summary <- reactive({

    filenames <- input$file$name
    data_upload <- map(input$file$datapath, read.delim)
    for (i in seq_along(data_upload)){
      data_upload[[i]]<-cbind(data_upload[[i]], filenames[i])}

    combined <- bind_rows(data_upload) %>%
      separate('filenames[i]', c("Condition", "Video"), sep = "_00")

    data_analyzed <- combined %>%
      group_by(Condition, Video)%>%
      analyze_motility() %>%
      rename("Velocity" = average_velocity,
             "Percent_Moving" =  percent_moving,
             "Moving_Filaments" = moving_filaments,
             "Total_Filaments" = total_filaments)

   summary_analysis <- data_analyzed %>%
     group_by(Condition) %>%
     summarize("Average Velocity" = mean(Velocity),
               "Average Percent Moving" = mean(Percent_Moving))


  })

  output$summary_table_output <- renderTable({
    if(is.null(input$file)) return(NULL)
          else return(data_summary())
  })


  output$download_by_video <- downloadHandler(
    filename = function() {
      paste("motility_analyzed_videos.csv", sep = "")
    },
    content = function(file1) {
      write.csv(data_by_video(), file1, row.names = FALSE)
    })

  output$download_summary <- downloadHandler(
    filename = function() {
      paste("motility_summary.csv", sep = "")
    },
    content = function(file1) {
      write.csv(data_summary(), file1, row.names = FALSE)
    })
}

shinyApp(ui = ui, server = server)


