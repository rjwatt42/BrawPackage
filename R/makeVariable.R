###########################
# variable utilities


CatProportions<-function(var) {
  if (is.numeric(var$proportions)) {
    pp<-var$proportions
  } else {
    pp<-as.numeric(unlist(strsplit(var$proportions,",")))
  }
  pp<-pp/max(pp)
}


OrdProportions<-function(var) {
  if (isempty(var$ordProportions) || is.na(var$ordProportions)) {
    ng<-var$nlevs
    centre<-((var$median-(ng+1)/2)/ng+0.5)
    concentration<-1/(var$iqr/2)*10
    alphaK<-1+centre*(concentration-2)
    betaK<-1+(1-centre)*(concentration-2)
    pp<-dbeta(seq(0,1,1/(ng+1)),alphaK,betaK)
    pp<-pp[2:(ng+1)]
    # pp<-exp(-0.5*(((1:ng)-(ng+1)/2)/(var$iqr/2)^2)
  } else {
    pp<-as.numeric(unlist(strsplit(var$ordProportions,",")))
  }
  pp<-pp/max(pp)
}

r2CatProportions<-function(rho,ncats1,ncats2) {
  
  # find proportions in each cell
  sigma<-matrix(c(1,rho,rho,1),nrow=2)
  mu<-c(0,0)
  
  xbreaks<-qnorm(seq(0,1,1/ncats1))
  ybreaks<-qnorm(seq(0,1,1/ncats2))
  division<-matrix(ncol=ncats1+1,nrow=ncats2)
  for (ix in 1:ncats1+1){
    whole<-pmnorm(c(xbreaks[ix],Inf),mu,sigma)
    divis<-pmnorm(matrix(c(rep(xbreaks[ix],ncats2+1),ybreaks),ncol=2,byrow=FALSE),mu,sigma)
    division[,ix]<-diff(divis)
  }
  division[,1]<-0
  division<-t(diff(t(division)))
  division
}


r2OrdProportions<-function(rho,ng) {
  
}
############################################
#
#' make a variable
#' 
#' @param type  "Interval","Ordinal","Categorical"
#' @returns a Variable object
#' @seealso [showVariable(variable=makeVariable("test"))]
#' @examples
#' makeVariable(name,type="Interval",
#'              mu=0,sd=1,skew=0,kurtosis=3,
#'              nlevs=7,iqr=3,median=NULL,discrete="discrete",ordProportions=NA,
#'              ncats=2,cases="C1,C2",proportions="1,1",source="discrete",
#'              deploy="Between",targetDeploys="",process="sim"
#' )
#' @export
makeVariable<-function(name,type="Interval",
                       mu=0,sd=1,skew=0,kurtosis=3,
                       nlevs=7,iqr=3,median=NULL,discrete="discrete",ordProportions=NA,
                       ncats=2,cases="C1,C2",proportions="1,1",source="discrete",
                       deploy="Between",targetDeploys="",process="sim"){
  
  if (is.null(median)) median<-((1+nlevs)/2)
  
  var<-list(name=name,type=type,
            mu=mu,sd=sd,skew=skew,kurtosis=kurtosis,
            nlevs=nlevs,iqr=iqr,median=median,discrete=discrete,ordProportions=ordProportions,
            ncats=ncats,cases=cases,proportions=proportions,source=source,
            deploy=deploy,targetDeploys=targetDeploys,
            process=process)
  
  # do ordinal mean and sd (for graphs)
  if (var$type=="Ordinal") {
    var$mu<-var$median
    var$sd<-var$iqr/2
    if (!is.na(ordProportions)) {
      pp<-strsplit(var$ordProportions,",")
      pp<-pp[[1]]
      if (length(pp)<var$nlevs) {
        pp<-c(pp,rep("1",var$nlevs-length(pp)))
      }
      if (length(pp)>var$nlevs){
        pp<-pp[1:var$nlevs]
      }
      var$ordProportions<-paste(pp,sep='',collapse=',')
    } else {
      var$ordProportions<-NA
    }
  }
  # check for cases
  if (var$type=="Categorical") {
    # sort the cases
    if (length(var$cases)==1) {
      cs<-strsplit(var$cases,",")
      cs<-cs[[1]]
      if (length(cs)<var$ncats){
        cs<-c(cs,paste("C",(length(cs)+1):var$ncats,sep=""))
      }
      if (length(cs)>var$ncats){
        cs<-cs[1:var$ncats]
      }
      var$cases<-cs
    }
    # check for proportions
    if (is.character(var$proportions)) {
      pp<-strsplit(var$proportions,",")
      pp<-as.numeric(pp[[1]])
      if (length(pp)<var$ncats) {
        pp<-c(pp,rep(1,var$ncats-length(pp)))
      }
      if (length(pp)>var$ncats){
        pp<-pp[1:var$ncats]
      }
      var$proportions<-pp
    }
  }
  # return var
  var
}

#' make a specific variable
#' 
#' @param name  "Psych","Treatment","Treatment?","IQ","Diligence","Perfectionism","Happiness","Grade","RiskTaking","Interesting","Coffee?","Smoker?","RiskTaker?","Musician?","StudySubject","BirthOrder"
#' @returns a Variable structure
#' @seealso [showVariable(variable=makeVariable("test"))]
#' @examples
#' variable<-getVariable(name)
#' @export
getVariable<-function(name) {
  
  defaultVars<-rbind(
    makeVariable(name="IV",type="Interval",mu=0,sd=1,ncats=2,cases="C1,C2"),
    makeVariable(name="IV2",type="Interval",mu=0,sd=1,ncats=2,cases="D1,D2"),
    makeVariable(name="DV",type="Interval",mu=0,sd=1,ncats=2,cases="E1,E2"),
    
    makeVariable(name="Treatment",type="Categorical",ncats=2,cases="before,after",proportions="1,1"),
    makeVariable(name="Treatment?",type="Categorical",ncats=2,cases="no,yes",proportions="1,1"),
    makeVariable(name="IQ",type="Interval",mu=100,sd=15),
    makeVariable(name="Diligence",type="Interval",mu=0,sd=2),
    makeVariable(name="Perfectionism",type="Interval",mu=0,sd=2),
    makeVariable(name="Happiness",type="Interval",mu=50,sd=12),
    makeVariable(name="Grade",type="Interval",mu=65,sd=10),
    makeVariable(name="RiskTaking",type="Interval",mu=30,sd=6),
    makeVariable(name="Interesting",type="Interval",mu=10,sd=2),
    
    makeVariable(name="Coffee?",type="Categorical",ncats=2,cases="no,yes",proportions="1,1"),
    makeVariable(name="Smoker?",type="Categorical",ncats=2,cases="no,yes",proportions="2,1"),
    makeVariable(name="RiskTaker?",type="Categorical",ncats=2,cases="no,yes"),
    makeVariable(name="Musician?",type="Categorical",ncats=2,cases="no,yes"),
    
    makeVariable(name="StudySubject",type="Categorical",ncats=3,cases="psych,phil,sports",proportions="1.5,1,2"),
    makeVariable(name="BirthOrder",type="Categorical",ncats=4,cases="first,middle,last,only",proportions="1,0.4,0.6,0.2")
  )
  
  variablesLocal<-matrix(defaultVars,nrow=18)
  
  use<-which(variablesLocal[,1]==name)
  if (isempty(use)) return(NULL)
  theVariable<-variablesLocal[use,]
  names(theVariable)<-names(makeVariable(" "))
  return(theVariable)
}
