
#' Create Plotly Histograms
#'
#' @param wd
#' @param trap_analysis
#' @param colors
#'
#' @return
#' @export
#'
#' @examples
#'
histo_plotly <- function(wd, trap_analysis, colors){

setwd(wd)

if(tolower(trap_analysis) == "mini"){
  #get mini ensemble event filenames
  files <- list.files("/Users/brentscott/trap_data/project_cardiacAzo",
                      pattern = "*_mini_ensemble_events.csv",
                      recursive = TRUE)

  plotly_data <- suppressMessages(bind_rows(map(files, read_csv)))

} else if(tolower(trap_analysis) == "hmm"){

  #get hmm events filenames
  files <- list.files("/Users/brentscott/trap_data/project_cardiacAzo",
                      pattern = "*_hmm_measured_events.csv",
                      recursive = TRUE)

  plotly_data <- bind_rows(map(files, read_csv))

}


save("plotly_data", file = paste0("plotly_data.RData"))


setwd(wd)

writeLines(c(

  "#+ echo=FALSE",
  paste0("wd <- ","'", wd, "'"),
  paste0("colors <- ", "'",colors, "'"),
  "
  #+ echo=FALSE, fig.width = 10, fig.height = 8, warnings = FALSE, message = FALSE

  suppressPackageStartupMessages(library(tidyverse))
  library(plotly)
  library(biophysr)

  setwd(wd)

  plotly_data <-  list.files(pattern = 'plotly_data.RData')
  load(plotly_data)


  time_on_plot <- ggplot(plotly_data)+
    geom_histogram(aes(x = time_on_ms, y = stat(count/sum(count)),  fill = conditions),
                   binwidth = 10, color = 'black')+
    facet_wrap(~conditions)+
    scale_fill_manual(values = colors, name = '')+
    xlab('10 ms bins')+
    ylab('Proportion')+
    ggtitle('Time On')+
    scale_y_continuous(expand = c(0, 0))+
    scale_x_continuous(breaks = seq(0, nrow(plotly_data), by = 10))+
    expand_limits(x = 0) +
    theme_ppt()+
    theme(axis.line = element_line(size = 1.5),
          strip.background = element_blank(),
          legend.position = 'none')

  ggplotly(time_on_plot)

  #+ echo=FALSE, fig.width = 10, fig.height = 8, warnings = FALSE, message = FALSE

   time_off_plot <- ggplot(plotly_data)+
    geom_histogram(aes(x = time_off_prior_ms, y = stat(count/sum(count)),  fill = conditions),
                   binwidth = 40, color = 'black')+
    facet_wrap(~conditions)+
    scale_fill_manual(values = colors, name ='')+
    xlab('10 ms bins')+
    ylab('Proportion')+
    ggtitle('Time Off')+
    scale_y_continuous(expand = c(0, 0))+
     scale_x_continuous(breaks = seq(0, nrow(plotly_data), by = 10))+
    expand_limits(x = 0) +
    theme_ppt()+
    theme(axis.line = element_line(size = 1.5),
          strip.background = element_blank(),
          legend.position = 'none')

   ggplotly(time_off_plot)

    #+ echo=FALSE, fig.width = 10, fig.height = 8, warnings = FALSE, message = FALSE

   displacement_plot <- ggplot(plotly_data)+
    geom_histogram(aes(x = displacement_nm, y = stat(count/sum(count)),  fill = conditions),
                   binwidth = 2, color = 'black')+
    facet_wrap(~conditions)+
    scale_fill_manual(values = colors, name = '')+
    xlab('4 nm bins')+
    ylab('Proportion')+
    ggtitle('Displacement')+
    scale_y_continuous(expand = c(0, 0))+
    expand_limits(x = 0) +
    theme_ppt()+
    theme(axis.line = element_line(size = 1.5),
          strip.background = element_blank(),
          legend.position = 'none')

   ggplotly(displacement_plot)

    #+ echo=FALSE, fig.width = 10, fig.height = 8, warnings = FALSE, message = FALSE

    force_plot <- ggplot(plotly_data)+
    geom_histogram(aes(x = force, y = stat(count/sum(count)),  fill = conditions),
                   binwidth = 0.25, color = 'black')+
    facet_wrap(~conditions)+
    scale_fill_manual(values = colors, name = '')+
    xlab('Force (pN)')+
    ylab('Proportion')+
    ggtitle('Forces')+
    scale_y_continuous(expand = c(0, 0))+
    expand_limits(x = 0) +
    theme_ppt()+
    theme(axis.line = element_line(size = 1.5),
          strip.background = element_blank(),
          legend.position = 'none')

   ggplotly(force_plot)
  "


),   paste0("histo_plotly.R"))


rmarkdown::render("histo_plotly.R")


}
