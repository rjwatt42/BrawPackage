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

r2CatProportions<-function(rho,ncats1,ncats2,pp1=NULL,pp2=NULL) {
  
  if (is.null(pp1)) pp1<-rep(1,ncats1)
  pp1<-pp1/sum(pp1)
  if (is.null(pp2)) pp2<-rep(1,ncats2)
  pp2<-pp2/sum(pp2)
  # find proportions in each cell
  sigma<-matrix(c(1,rho,rho,1),nrow=2)
  mu<-c(0,0)
  
  xbreaks<-qnorm(cumsum(c(0,pp1)))
  ybreaks<-qnorm(cumsum(c(0,pp2)))
  division<-matrix(ncol=ncats1+1,nrow=ncats2)
  for (ix in 1:ncats1+1){
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
#' @param name  - a character string 
#' @param type  "Interval","Ordinal","Categorical"
#' @param ordSource "discrete", "continuous"
#' @param catSource "discrete", "continuous"
#' @returns a Variable object
#' @seealso showVariable(variable=makeVariable("test"))
#' @seealso getVariable(name)
#' @examples
#' makeVariable(name,type="Interval",
#'              mu=0,sd=1,skew=0,kurtosis=0,
#'              nlevs=7,iqr=3,median=NULL,ordProportions=NA,ordSource="discrete",
#'              ncats=2,cases=c("C1","C2"),proportions=c(1,1),catSource="discrete"
#'              )
#' @export
makeVariable<-function(name,type="Interval",
                       mu=0,sd=1,skew=0,kurtosis=0,
                       nlevs=7,iqr=3,median=NULL,ordSource="discrete",ordProportions=NA,
                       ncats=2,cases=c("C1","C2"),proportions=c(1,1),catSource="discrete",
                       deploy="Between",targetDeploys="",process="sim"){
  
  if (is.null(median)) median<-((1+nlevs)/2)
  
  var<-list(name=name,type=type,
            mu=mu,sd=sd,skew=skew,kurtosis=kurtosis,
            nlevs=nlevs,iqr=iqr,median=median,ordSource=ordSource,ordProportions=ordProportions,
            ncats=ncats,cases=cases,proportions=proportions,catSource=catSource,
            deploy=deploy,targetDeploys=targetDeploys,
            process=process)
  
  # do ordinal mean and sd (for graphs)
  if (var$type=="Ordinal") {
    var$mu<-var$median
    var$sd<-var$iqr/2
  }
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
  
  # check for cases
  # if (var$type=="Categorical") {
    # sort the cases
    if (length(var$cases)==1) {
      cs<-strsplit(var$cases,",")
      cs<-cs[[1]]
    } else cs<-var$cases
    if (length(cs)<var$ncats){
      cs<-c(cs,paste("C",(length(cs)+1):var$ncats,sep=""))
    }
    if (length(cs)>var$ncats){
      cs<-cs[1:var$ncats]
    }
    var$cases<-cs
  # check for proportions
    if (is.character(var$proportions)) {
      pp<-strsplit(var$proportions,",")
      pp<-as.numeric(pp[[1]])
    } else {
      pp<-var$proportions
    }
    if (length(pp)<var$ncats) {
      pp<-c(pp,rep(1,var$ncats-length(pp)))
    }
    if (length(pp)>var$ncats){
      pp<-pp[1:var$ncats]
    }
    var$proportions<-pp
    
  # }
  # return var
  var
}

#' make a specific variable
#' 
#' @param name  "Psych","Treatment","Treatment?","IQ","Diligence","Perfectionism", \cr
#'              "Happiness","ExamGrade","RiskTaking","Interesting","Coffee?","Smoker?", \cr
#'              "RiskTaker?","Musician?","StudySubject","BirthOrder"
#' @returns a Variable structure
#' @seealso showVariable(variable=makeVariable("test"))
#' @examples
#' variable<-getVariable(name)
#' @export
getVariable<-function(name=NULL) {
  
  if (is.null(name)) return(NULL)
  return(braw.env$variables[[name]])
  
}

addVariable<-function(var) {
  var<-list(var)
  names(var)<-var[[1]]$name
  vars<-c(braw.env$variables,var)
  assign("variables",vars,braw.env)
}

changeVariable<-function(var) {
  var<-list(var)
  vars<-braw.env$variables
  vars[var[[1]]$name]<-var
  assign("variables",vars,braw.env)
}

makeDefaultVariables<-function() {
  defaultVars<-list(
    IV=makeVariable(name="IV",type="Interval",mu=0,sd=1,ncats=2,cases="C1,C2"),
    IV2=makeVariable(name="IV2",type="Interval",mu=0,sd=1,ncats=2,cases="D1,D2"),
    DV=makeVariable(name="DV",type="Interval",mu=0,sd=1,ncats=2,cases="E1,E2"),
    
    IVOrd=makeVariable(name="IV",type="Ordinal"),
    IVCat=makeVariable(name="IV",type="Categorical",ncats=2,cases="C1,C2"),
    IV3Cat=makeVariable(name="IV",type="Categorical",ncats=3,cases="C1,C2,C3"),
    
    Treatment3=makeVariable(name="Treatment",type="Categorical",ncats=3,cases="active,passive,none",proportions="1,1,1"),
    Treatment=makeVariable(name="Treatment",type="Categorical",ncats=2,cases="active,control",proportions="1,1"),
    "Treatment?"=makeVariable(name="Treatment?",type="Categorical",ncats=2,cases="no,yes",proportions="1,1"),
    IQ=makeVariable(name="IQ",type="Interval",mu=100,sd=15),
    Diligence=makeVariable(name="Diligence",type="Interval",mu=0,sd=2),
    Perfectionism=makeVariable(name="Perfectionism",type="Interval",mu=0,sd=2),
    Anxiety=makeVariable(name="Anxiety",type="Interval",mu=5,sd=2),
    Happiness=makeVariable(name="Happiness",type="Interval",mu=50,sd=12),
    SelfConfidence=makeVariable(name="SelfConfidence",type="Interval",mu=50,sd=12),
    HoursSleep=makeVariable(name="HoursSleep",type="Interval",mu=7,sd=1,skew=-0.7),
    ExamGrade=makeVariable(name="ExamGrade",type="Interval",mu=65,sd=10,skew=-0.6),
    ExamPass=makeVariable(name="ExamPass?",type="Categorical",ncats=2,cases="no,yes",proportions="1,3"),
    "ExamPass?"=makeVariable(name="ExamPass?",type="Categorical",ncats=2,cases="no,yes",proportions="1,3"),
    RiskTaking=makeVariable(name="RiskTaking",type="Interval",mu=30,sd=6,skew=0.5),
    Interesting=makeVariable(name="Interesting",type="Interval",mu=10,sd=2),
    NeuroType=makeVariable(name="NeuroType",type="Categorical",ncats=2,cases="NT,ND",proportions="2,1"),
    
    SelfConfidenceOrd=makeVariable(name="SelfConfidenceRating",'Ordinal',nlevs=6),
    PerfectionismOrd=makeVariable(name="PerfectionismRating",'Ordinal',nlevs=6),
    "Coffee?"=makeVariable(name="Coffee?",type="Categorical",ncats=2,cases="no,yes",proportions="1,1"),
    "Smoker?"=makeVariable(name="Smoker?",type="Categorical",ncats=2,cases="no,yes",proportions="2,1"),
    "RiskTaker?"=makeVariable(name="RiskTaker?",type="Categorical",ncats=2,cases="no,yes"),
    "Musician?"=makeVariable(name="Musician?",type="Categorical",ncats=2,cases="no,yes"),
    TrialOutcome=makeVariable(name="TrialOutcome",type="Categorical",ncats=2,cases="-ve,+ve",proportions="1.2,1"),
    Sessions=makeVariable(name="Sessions",'Ordinal',nlevs=6,ordSource="discrete",ordProportions="0.1,0.2,0.4,0.4,0.6,0.8"),
    SessionsI=makeVariable(name="Sessions",'Interval',mu=4,sd=1,skew=-0.75),
    TrialPhase=makeVariable(name="TrialPhase",type="Categorical",ncats=2,cases="pre,post",proportions="1.1,1"),
    TrialPhase3=makeVariable(name="TrialPhase",type="Categorical",ncats=3,cases="before,during,after",proportions="1.2,1.1,1"),
    
    Condition=makeVariable(name="Condition",type="Categorical",ncats=2,cases="1,2",proportions="1,1"),
    MemoryCondition=makeVariable(name="Prompt",type="Categorical",ncats=3,cases="none,implicit,explicit",proportions="1,1,1"),
    Group=makeVariable(name="Group",type="Categorical",ncats=2,cases="group1,group2",proportions="1,1"),
    Group3=makeVariable(name="Group",type="Categorical",ncats=3,cases="group1,group2,grop3",proportions="1,1,1"),
    Response=makeVariable(name="Response",type="Interval",mu=50,sd=20),
    
    InformationLevel=makeVariable(name="InformationLevel",type="Interval",mu=10,sd=2,skew=-0.5),
    ReactionTime=makeVariable(name="ReactionTime",type="Interval",mu=500,sd=100,skew=0.5),
    PracticeTrials=makeVariable(name="PracticeTrials",'Ordinal',nlevs=6,ordSource="discrete"),
    
    Diagnosis=makeVariable(name="Diagnosis",type="Categorical",ncats=3,cases="BPD,ADHD,Autism",proportions="1,1.5,2"),
    StudySubject=makeVariable(name="StudySubject",type="Categorical",ncats=3,cases="psych,phil,sports",proportions="1.5,1,2"),
    BirthOrder=makeVariable(name="BirthOrder",type="Categorical",ncats=4,cases="first,middle,last,only",proportions="1,0.4,0.6,0.2")
  )
  
  return(defaultVars)
}