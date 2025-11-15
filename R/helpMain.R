
#' @export
brawMainHelp<-function(open=0,indent=0,titleWidth=100) {
  if (length(open)<3) open<-c(open,0,0,0)
  
    help<-generate_tab(
      title="Help Topics:",
      plainTabs=TRUE,
      titleWidth=titleWidth,
      indent=indent,
      tabs=c("Start","Basics","MetaScience","Simulation","Jamovi","Key"),
      tabContents=c(
        brawStartHelp(indent=50),
        brawBasicsHelp(open=open[2:3],indent=50,titleWidth=titleWidth),
        brawMetaSciHelp(open=open[2],indent=50),
        brawSimHelp(open=open[2],indent=50),
        brawJamoviHelp(open=open[2],indent=50)
      ),
      open=open[1]
    )
}

brawStartHelp<-function(indent=0) {
      paste0(
        '<div style="margin-left:',format(indent),'px;">',
        '<b>Help Tabs</b>',
        '<br>The help system contains useful information about BrawStats.',
        '<br>Use the grey tabs to open up the topics. ',
        "To close a tab, click the same grey tab. To close all tabs, click the white 'Help Topics:' label.",
        '<br>',
        '</div>'
      )
}

#' @export
brawSimHelp<-function(open=0,indent=0,plainTabs=TRUE) {
  return(
    generate_tab(
      title="Simulation",
      indent=indent,
      titleWidth=0,
      plainTabs=plainTabs,
      # titleTab="Click on the tabs for specific help.",
      tabs=c("Start","Plan","Single Sample","Multiple Samples","Explore"),
      tabContents = c(
        BrawInstructions("Overview"),
        BrawInstructions("Plan"),
        BrawInstructions("Single"),
        BrawInstructions("Multiple"),
        BrawInstructions("Explore")
      ),
      open=open
    )
  )
}

#' @export
brawMetaSciHelp<-function(open=0,indent=0,plainTabs=TRUE) {
  return(
    generate_tab(
      title="MetaScience",
      indent=indent,
      titleWidth=0,
      plainTabs=plainTabs,
      # titleTab="Click on the tabs for specific help.",
      tabs=c(
        # "Start",
        "Inferences","SamplingMethod","SampleSize","DoubleChecking","RealDifferences"),
      tabContents = c(
        # metaSciInstructions("Overview"),
        metaSciInstructions("1"),
        metaSciInstructions("2"),
        metaSciInstructions("3"),
        metaSciInstructions("4"),
        metaSciInstructions("5")
      ),
      open=open
    )
  )
}

#' @export
brawJamoviHelp<-function(open=0,indent=0,hypothesis=braw.def$hypothesis,design=braw.def$design,plainTabs=TRUE) {
  return(
  generate_tab(
    title="Jamovi",
    indent=indent,
    titleWidth=0,
    tabs=c("Analysis","Graph","EffectSize"),
    tabContents = c(
      JamoviInstructions(hypothesis,design,HelpType="Analysis"),
      JamoviInstructions(hypothesis,design,HelpType="Graph"),
      JamoviInstructions(hypothesis,design,HelpType="EffectSize")
    ),
    plain=1,
    open=open
  )
  )
}

#' @export
brawBasicsHelp<-function(open=c(0,0),indent=0,titleWidth=100,plainTabs=TRUE) {
  basicsTabNames<-c("Start","Data","Uncertainty","Design","Linear Models")
  basicsTabs<-c(
    basicsInstructions("start"),
    generate_tab(
      title="Data:",
      tabs=c("Overview","1a","1b","1c"),
      plainTabs=plainTabs,
      indent=indent,
      width=500,
      titleWidth=0,
      tabContents=c(
        basicsInstructions("1"),
        basicsInstructions("1a"),
        basicsInstructions("1b"),
        basicsInstructions("1c")
      ),
      plain=1,
      open=open[2]
    ),
    generate_tab(
      title="Uncertainty:",
      tabs=c("Overview","2a","2b","2c"),
      indent=0,
      width=500,
      titleWidth=titleWidth,
      tabContents=c(
        basicsInstructions("2"),
        basicsInstructions("2a"),
        basicsInstructions("2b"),
        basicsInstructions("2c")
      ),
      open=open[2]
    ),
    generate_tab(
      title="Design:",
      tabs=c("Overview","3a","3b","3c"),
      indent=0,
      width=500,
      titleWidth=titleWidth,
      tabContents=c(
        basicsInstructions("3"),
        basicsInstructions("3a"),
        basicsInstructions("3b"),
        basicsInstructions("3c")
      ),
      open=open[2]
    ),
    generate_tab(
      title="Linear Models:",
      tabs=c("Overview","4a","4b","4c"),
      indent=0,
      width=500,
      titleWidth=titleWidth,
      tabContents=c(
        basicsInstructions("4"),
        basicsInstructions("4a"),
        basicsInstructions("4b"),
        basicsInstructions("4c")
      ),
      open=open[2]
    )
  )

    return( generate_tab(
      title="Basics",
      indent=indent,
      titleWidth=0,
      plainTabs=plainTabs,
      plain=1,
      tabs=basicsTabNames,
      tabContents=basicsTabs,
      open=open[1]
    )
    )
}
