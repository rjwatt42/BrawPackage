
#' @export
emptyPlot<-function(mode) {

    switch(mode,
           "Basics"=tabs<-c("Plan","Sample","Data","Schematic","Jamovi"),
           "MetaScience"=tabs<-c("Plan","Data","Schematic"),
           "Simulation"=tabs<-c("Plan","Single","Multiple","Explore")
    )
    switch(mode,
           "Basics"=tabTitle<-"Basics:",
           "MetaScience"=tabTitle<-"MetaScience:",
           "Simulation"=tabTitle<-"Simulation:"
    )
    nullResults<-generate_tab(
      title=tabTitle,
      plainTabs=TRUE,
      titleWidth=100,
      tabs=tabs,
      tabContents=c(rep(nullPlot(),length(tabs))),
      height=450,
      outerHeight=450,
      open=1
    )
    return(nullResults)
    # self$results$simGraphHTML$setContent(nullResults)
  
}