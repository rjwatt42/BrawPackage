

#' @export
showPlan<-function(helpOutput="") {
  oldHTML<-braw.env$graphicsType
  setBrawEnv("graphicsType","HTML")
  
  if (nchar(helpOutput)>0) {
    plain<-TRUE
    systemHTML<-paste0(helpOutput,'<div><br></div>')
  }
  else {
    systemHTML<-""
    plain<-FALSE
  }

  openSystem<-0

  svgBox(200*1)
  if (braw.def$hypothesis$effect$world$On) {
    h<-joinHTML(showSystem("world"),reportWorld())
  } else {
    h<-joinHTML(showSystem("hypothesis"),reportSystem(design=NULL))
  }
  svgBox(180*1)
  sd<-showSystem("design")
  svgBox(180)
  rd<-reportDesign()
  d<-joinHTML(sd,rd)

  # restore everything we might have changed
  setBrawEnv("graphicsType",oldHTML)
  svgBox(400)
  
  return(joinHTML(h,d))
}
