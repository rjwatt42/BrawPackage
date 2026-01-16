
#' @export
emptyPlot<-function(mode,useHelp=FALSE) {
  
  if (mode=="Basics") return(doBasics(NULL,showOutput=FALSE))
  if (mode=="Theory") return(doTheory(NULL,showOutput=FALSE))
  if (mode=="MetaScience") return(doMetaScience(NULL,showOutput=FALSE))
  
  if (mode=="Simulation") {
    tabTitle<-"Simulation:"
    tabs<-c("Plan","Single","Multiple","Explore")
    tabContents<-c(showPlan(),rep(nullPlot(),length(tabs)-1))
    if (useHelp) {
      tabs<-c(tabs,"Help")
      tabContents<-c(tabContents,brawSimHelp(indent=100))
    }
    nullResults<-generate_tab(
      title=tabTitle,
      plainTabs=TRUE,
      titleWidth=100,
      tabs=tabs,
      tabContents=tabContents,
      height=450,
      outerHeight=450,
      open=0
    )
    return(nullResults)
  }
}