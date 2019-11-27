#' Runs GUI
#'
#' @return
#' @export
#'
#' @examples

gui <- function(){

  library(gWidgets2)
  options(guiToolkit = "tcltk")

  parent_dir <- NULL

  window <- gwindow("biophysr", visible=FALSE, width = 150)
  #group <- gnotebook(container=window, tab.pos = 3) ## a parent container
  group <- gvbox(cont = window)
 # glabel("", container=group)

  sb <- gstatusbar("Idle", container=window)




  b <- gbutton("Select Folder", cont=group,handler=function(...) {
    value <- gfile(type="selectdir")
    #gmessage(paste0("Input directory set to ",value))

    parent_dir <<- value
    svalue(sb) <<- "Folder Selected"


  })

  make_o <- gbutton("Make Observations", cont=group,handler=function(...) {

    make_trap_observations(wd = parent_dir)
    gmessage("Observations Made & Folders Oraganized")

    svalue(sb) <<- "Obs Made"


  })


  mv2nm_label <- glabel("nm/mV conversion", container=group)
  font(mv2nm_label) <- list(weight="bold")

  mv2nm_input <- gedit(container = group, handler=function(...){
    mv2nm <<- as.numeric(svalue(mv2nm_input))
    svalue(sb) <<- "nm/mV conversion set"

  })

  nm2pn_label <- glabel("pN/nm conversion", container=group)
  font(nm2pn_label) <- list(weight="bold")

  nm2pn_input <- gedit(container = group, handler=function(...){
    nm2pn <<- as.numeric(svalue(nm2pn_input))
    svalue(sb) <<- "pN/nm conversion set"

  })

  color_label <- glabel("Choose Color", container=group)
  font(color_label) <- list(weight="bold")
  color <- gedit(cont=group,handler=function(...) {
    run_mean_color <<- svalue(color)
    svalue(sb) <<- "color picked"

  })


  analysis<- gbutton("Run Analysis", cont=group,handler=function(...) {
    gmessage("Analysis Running. Statusbar will read 'Analysis complete' when finished")

    mini_ensemble_analyzer_gui(parent_dir = parent_dir,
                               mv2nm = mv2nm,
                               nm2pn = nm2pn,
                               run_mean_color = run_mean_color)

        svalue(sb) <<- "Analysis Complete"

  })





  visible(window) <- TRUE






}
