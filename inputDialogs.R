inputs <- function(){
  
  xvar <- tclVar("")
  yvar <- tclVar("")
  
  tt <- tktoplevel()
  tkwm.title(tt,"Input Numbers")
  x.entry <- tkentry(tt, textvariable=xvar)
  y.entry <- tkentry(tt, textvariable=yvar)
  
  reset <- function()
  {
    tclvalue(xvar)<-""
    tclvalue(yvar)<-""
  }
  
  reset.but <- tkbutton(tt, text="Reset", command=reset)
  
  submit <- function() {
    x <- as.numeric(tclvalue(xvar))
    y <- as.numeric(tclvalue(yvar))
    e <- parent.env(environment())
    e$x <- x
    e$y <- y
    tkdestroy(tt)
  }
  submit.but <- tkbutton(tt, text="submit", command=submit)
  
  tkgrid(tklabel(tt,text="Enter Two Inputs"),columnspan=2)
  tkgrid(tklabel(tt,text="Input1"), x.entry, pady = 10, padx =10)
  tkgrid(tklabel(tt,text="Input2"), y.entry, pady = 10, padx =10)
  tkgrid(submit.but, reset.but)
  
  tkwait.window(tt)
  return(c(x,y))
}
