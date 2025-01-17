brawbasicHelp<-function(open=0,indent,titleWidth) {
  return(paste0(
    '<b>Help Tabs</b>',
    '<br>The help system contains useful information about BrawStats.',
    '<br>Use the grey tabs to open up the topics. ',
    "To close a tab, click the same grey tab. To close all tabs, click the white 'Help:' label.",
    '<br>',
    '<b>Plan Tabs</b>',
    '<br>The basic parameters of a research plan are shown graphically in the Plan Tabs.</b>',
    '<br>These show the current plan for simulations. The tabs operate in the same way as for the Help Tabs.',
    '<br>When the plan is changed, the relevant part is shown here.'
  )
  )
}

brawsimHelp<-function(open=0,indent,titleWidth) {
  return(
    generate_tab(
      title="BrawStats help:",
      indent=indent,
      titleWidth=titleWidth,
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

brawJamoviHelp<-function(open=0,indent,titleWidth,hypothesis,design) {
  return(
  generate_tab(
    title="Jamovi equivalent:",
    indent=indent,
    titleWidth=titleWidth,
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

brawDemosHelp<-function(open,indent,titleWidth,demos) {
  demoTabNames<-c("Start","Data","Uncertainty","Linear Models","Path Models")
  demoTabs<-c(
    demoInstructions("start"),
    generate_tab(
      title="Samples of Data:",
      tabs=c("Overview","1a","1b","1c"),
      indent=0,
      titleWidth=titleWidth,
      tabContents=c(
        demoInstructions("1"),
        demoInstructions("1a"),
        demoInstructions("1b"),
        demoInstructions("1c")
      ),
      plain=1,
      open=open[2]
    ),
    generate_tab(
      title="Uncertainty & Design:",
      tabs=c("Overview","2a","2b","2c"),
      indent=0,
      titleWidth=0,
      tabContents=c(
        demoInstructions("2"),
        demoInstructions("2a"),
        demoInstructions("2b"),
        demoInstructions("2c")
      ),
      open=open[2]
    ),
    generate_tab(
      title="Linear Models:",
      tabs=c("Overview","3a","3b","3c"),
      indent=0,
      titleWidth=0,
      tabContents=c(
        demoInstructions("3"),
        demoInstructions("3a"),
        demoInstructions("3b"),
        demoInstructions("3c")
      ),
      open=open[2]
    ),
    generate_tab(
      title="Path Models:",
      tabs=c("Overview","4a","4b","4c"),
      indent=0,
      titleWidth=0,
      tabContents=c(
        demoInstructions("4"),
        demoInstructions("4a"),
        demoInstructions("4b"),
        demoInstructions("4c")
      ),
      open=open[2]
    )
  )
    demoTabNames<-demoTabNames[1:demos]
    demoTabs<-demoTabs[1:demos]

    return( generate_tab(
      title="Demos:",
      indent=indent,
      titleWidth=titleWidth,
      plainTabs=TRUE,
      plain=1,
      tabs=demoTabNames,
      tabContents=demoTabs,
      open=open[1]
    )
    )
}