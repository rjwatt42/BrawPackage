
makeCol<-function(col,alpha) {
  if (col=="black") col<-"#000000"
  if (col=="white") col<-"#FFFFFF"
  paste0(col,as.hexmode(floor(alpha*255)))
}

makeFont<-function(f) {
  font<-1
  switch (f,
          "bold"={font<-2},
          "italic"={font<-3},
          font<-1)
  
}

boxtext <- function(x, y, labels = NA, col = NULL, bg = NA, 
                    border = NA, adj = NULL, pos = NULL, offset = 0.5, 
                    padding = c(0.5, 0.5), cex = 1, font = graphics::par('font'),srt=0){
  
  ## The Character expansion factor to be used:
  theCex <- graphics::par('cex')*cex
  
  ## Width and height of text
  textHeight <- graphics::strheight(labels, cex = theCex, font = font)
  textWidth <- graphics::strwidth(labels, cex = theCex, font = font)
  
  ## Width of one character:
  charWidth <- graphics::strwidth("e", cex = theCex, font = font)
  
  ## Is 'adj' of length 1 or 2?
  if (!is.null(adj)){
    if (length(adj) == 1){
      adj <- c(adj[1], 0.5)            
    }        
  } else {
    adj <- c(0.5, 0.5)
  }
  
  ## Is 'pos' specified?
  if (!is.null(pos)){
    if (pos == 1){
      adj <- c(0.5, 1)
      offsetVec <- c(0, -offset*charWidth)
    } else if (pos == 2){
      adj <- c(1, 0.5)
      offsetVec <- c(-offset*charWidth, 0)
    } else if (pos == 3){
      adj <- c(0.5, 0)
      offsetVec <- c(0, offset*charWidth)
    } else if (pos == 4){
      adj <- c(0, 0.5)
      offsetVec <- c(offset*charWidth, 0)
    } else {
      stop('Invalid argument pos')
    }       
  } else {
    offsetVec <- c(0, 0)
  }
  
  ## Padding for boxes:
  if (length(padding) == 1){
    padding <- c(padding[1], padding[1])
  }
  
  ## Midpoints for text:
  xMid <- x + (-adj[1] + 1/2)*textWidth + offsetVec[1]
  yMid <- y + (-adj[2] + 1/2)*textHeight + offsetVec[2]
  
  ## Draw rectangles:
  rectWidth <- textWidth + 2*padding[1]*charWidth
  rectHeight <- textHeight + 2*padding[2]*charWidth    
  graphics::rect(xleft = xMid - rectWidth/2, 
                 ybottom = yMid - rectHeight/2, 
                 xright = xMid + rectWidth/2, 
                 ytop = yMid + rectHeight/2,
                 col = bg, border = border)
  
  ## Place the text:
  graphics::text(xMid, yMid, labels, col = col, cex = theCex, font = font, 
                 adj = c(0.5, 0.5))    
  
  ## Return value:
  if (length(xMid) == 1){
    invisible(c(xMid - rectWidth/2, xMid + rectWidth/2, yMid - rectHeight/2,
                yMid + rectHeight/2))
  } else {
    invisible(cbind(xMid - rectWidth/2, xMid + rectWidth/2, yMid - rectHeight/2,
                    yMid + rectHeight/2))
  }    
}

doGraphElementBase<-function(element) {
  args<-element$args
  switch(element$type,
         "Null"={
           par(mai=c(0,0,0,0))
           plot.new()
         },
         "Start"={
           braw.env$plotLimits<-args
         },
         "Text"={ 
           font<-makeFont(args[[11]])
           boxtext(x=args[[1]]$x,y=args[[1]]$y,labels=args[[2]],adj=c(args[[3]],args[[4]]),
                cex=args[[7]],col=args[[5]],font=font,srt=args[[8]])
         },
         "Label"={
           font<-makeFont(args[[8]])
           boxtext(x=args[[1]]$x,y=args[[1]]$y,labels=args[[2]],adj=c(args[[3]],args[[4]]),
                cex=args[[9]],col=args[[6]],bg=args[[5]],font=font,padding=args[[10]])
         },
         "Point"={
           col<-makeCol(args[[3]],args[[5]])
           points(x=args[[1]]$x,y=args[[1]]$y,pch=args[[2]],col=args[[3]],bg=args[[4]],cex=args[[6]])
         },
         "Path"={
           col<-makeCol(args[[3]],args[[6]])
           lines(x=args[[1]]$x,y=args[[1]]$y,col=col,lty=args[[4]],lwd=args[[5]])
         },
         "Polygon"={
           col<-makeCol(args[[3]],args[[4]])
           polygon(x=args[[1]]$x,y=args[[1]]$y,border=args[[2]],col=col,lwd=args[[5]])
         }
  )
  return(g)
}

buildGraphBase<-function(g1=NULL) {
  if (is.null(g1)) g1<-braw.env$history[[length(braw.env$history)]]
  for (i in 1:length(g1)) {
    doGraphElementBase(g1[[i]])
  }
}
