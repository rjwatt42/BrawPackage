
#' make SEM model object
#' @return sem object 
#' @examples
#' makeSEM<-function(filename)
#' @export
makeLM<-function(data,DV=NULL,IV=NULL) {
  if (is.character(data)) data<-readDataSEM(data)
  nvar<-ncol(data$data)
  if (is.null(DV)) {
    dataNames<-data$varnames
    DV<-dataNames[1]
    IV<-dataNames[2:nvar]
  }

  data<-data$data[c(DV,IV)]
  lm<-list(data=data,DV=DV,IV=IV)  
}

#' make path for a SEM model
#' @return path object 
#' @examples
#' makeSEMPath<-function(data,stages=NULL)
#' @export
makeSEMPath<-function(data=NULL,stages=NULL,
                  depth=1,
                  only_ivs=NULL,only_dvs=NULL,
                  within_stages=0,
                  add=NULL,remove=NULL) {
  
  if (is.null(stages) && !is.null(data)) {
    dataNames<-data$varnames
    nvar<-length(dataNames)
    stages<-c(list(dataNames[2:nvar]),
              list(dataNames[1])
    )
  }
  
  path<-list(stages=stages,
             depth=1,
             only_ivs=only_ivs,only_dvs=only_dvs,
             within_stages=within_stages,
             add=add,remove=remove
  )
  return(list(path=path))
  
}

#' read data for a SEM model
#' @return sample object 
#' @examples
#' readDataSEM<-function(filename)
#' @export
readDataSEM<-function(filename) {
  if (grepl(".csv",filename)) d<-read.csv(filename)
  if (grepl(".dat",filename)) d<-read.table(filename,header=TRUE)
  if (!is.character(filename)) d<-filename
  
  dataFull<-prepareSample(d)
  liveData<-dataFull$data[,2:ncol(dataFull$data)]
  if (ncol(dataFull$data)==2) liveData<-matrix(liveData,ncol=ncol(dataFull$data)-1)
  
  data<-list(data=liveData,
             varnames=dataFull$variables$name,
             varcat=(dataFull$variables$type=="Categorical")
  )
  return(data)
}

#' make SEM model object
#' @return sem object 
#' @examples
#' makeSEM<-function(filename)
#' @export
makeSEM<-function(data,path=NULL) {
  if (is.character(data)) data<-readDataSEM(data)
  if (is.null(path)) 
    path<-makeSEMPath(data)

  sem<-list(data=data,path=path)  
}

