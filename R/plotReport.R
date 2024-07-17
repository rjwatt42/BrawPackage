
reportPlot<-function(outputText,nc,nr,fontSize=0.85,maxRows=14){

  bg<-braw.env$plotColours$graphC
  margin=0.5
  colSpace=1.5
  
  font_size=braw.env$labelSize*fontSize
  characterWidth=font_size/15
  
  top=max(nr,maxRows)
  edge=100*characterWidth

  oT<-matrix(outputText,ncol=nc,byrow=TRUE)
  nT<-nchar(oT) # no of characters per cell
  nrT<-rowSums(nT) # no characters per row
  # now break into blocks separated by empty rows
  blockEnds<-c(0,which(nrT==0),nrow(nT))
  colX<-c()
  cellSize<-c()
  if (length(blockEnds)>1) {
  for (i in 2:length(blockEnds)){
    block<-nT[(blockEnds[i-1]+1):blockEnds[i],]
    if (is.null(dim(block))){
      # empty block
      colSize<-block+colSpace
      colOffset<-cumsum(c(0,colSize))
      colX<-c(colX,rep(colOffset[1:length(block)],1))
      cellSize<-c(cellSize,rep(colSize,length(block)))
    }
    else     {
      colSize<-apply(block,2,max)+colSpace
      colOffset<-cumsum(c(0,colSize))
      colX<-c(colX,rep(colOffset[1:ncol(block)],nrow(block)))
      cellSize<-c(cellSize,rep(colSize,nrow(block)))
    }
  }
  }
  cellSize<-cellSize*characterWidth
  x_gap1<-colX*characterWidth
  
  d<-expand.grid(x=1:nc,y=1:nr)

  boldlabels<-grepl("\b",outputText)
  outputText<-sub("\b","",outputText)
  italiclabels<-grepl("!i",outputText)
  outputText<-sub("!i","",outputText)
  rightlabels<-grepl("!j",outputText)
  outputText<-sub("!j","",outputText)
  
  redlabels<-grepl("!r",outputText)
  outputText<-sub("!r","",outputText)
  greenlabels<-grepl("!g",outputText)
  outputText<-sub("!g","",outputText)
  bluelabels<-grepl("!b",outputText)
  outputText<-sub("!b","",outputText)
  
  xlargelabels<-grepl("!>>>",outputText)
  outputText<-sub("!>>>","",outputText)
  vlargelabels<-grepl("!>>",outputText)
  outputText<-sub("!>>","",outputText)
  largelabels<-grepl("!>",outputText)
  outputText<-sub("!>","",outputText)
  
  pts<-data.frame(x=x_gap1,y=d$y)
  g<-ggplot()

  for (i in 1:length(outputText)) {
    x<-x_gap1[i]+1
    y<-d$y[i]
    label<-outputText[i]
    parse<-FALSE
    switch(label,
           "Alpha"={label<-braw.env$alphaChar},
           "pNull"={label<-deparse(braw.env$Plabel)},
           "Lambda"={label<-deparse(braw.env$Llabel)},
           "p(sig)"={label<-deparse(braw.env$pSigLabel)}
           )
    fontface<-"plain"
    if (boldlabels[i]) fontface<-"bold" 
    if (italiclabels[i]) fontface<-"italic" 
    if (boldlabels[i] && italiclabels[i]) fontface<-"bold.italic" 
    if (rightlabels[i]) hjust<- 1 else hjust<- 0
    if (rightlabels[i]) {
      x<-x+cellSize[i]-characterWidth
    }
    fill<-bg
    col<-"black"
    if (bluelabels[i]) col="#0000DD"
    if (greenlabels[i]) col="#00DD00"
    if (redlabels[i]) col="#DD0000"
    sz<-1
    if (xlargelabels[i]) sz<-1.8 
    if (vlargelabels[i]) sz<-1.5 
    if (largelabels[i]) sz<-1.2 
    
    mathlabel<-grepl("['^']{1}",label) || grepl("['[']{1}",label)
    if (any(mathlabel)) parse<-TRUE
    pts<-data.frame(x=x,y=top+1-y)
    g<-g+geom_label(data=pts,aes(x=x, y=y), label=label,fontface=fontface, 
                                         hjust=hjust, vjust=0, 
                                         size=font_size*sz, 
                                         col=col,fill=fill,
                                         parse=parse,
                                         label.size=NA,label.padding=unit(0,"lines"))
  }
  
  g<-g+labs(x="  ",y="  ")+theme(legend.position = "none")
  g<-g+coord_cartesian(xlim = c(1-margin,edge+margin), ylim = c(1-margin,top+margin))
  g+braw.env$blankTheme()
}
