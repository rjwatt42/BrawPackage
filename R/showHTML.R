#' @export
showHTML<-function(data,new=FALSE) {
  if (is.character(data)) {
    if (new) {
      for (i in 1:100) {
      htmlFile <- file.path(".", paste0("index",i,".html"))
      if (!file.exists(htmlFile)) break;
      }
    } else
      htmlFile <- file.path(".", "index.html")
    writeLines(data, con = htmlFile)
    rstudioapi::viewer(htmlFile)
  } else {
    assign("graphHTML",TRUE,braw.env)
    show<-data()
    
    htmlFile <- file.path(".", "index.html")
    writeLines(show, con = htmlFile)
    rstudioapi::viewer(htmlFile)
    
    assign("graphHTML",FALSE,braw.env)
  }
  return(invisible(NULL))
}
