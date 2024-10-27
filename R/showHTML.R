showHTML<-function(data) {
  if (is.character(data)) {
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
}
