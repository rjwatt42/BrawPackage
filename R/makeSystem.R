################################################################        
# Convenience functions
#

#' set shortHand to true
#' @export
doShortHand<-function() {
  evidence<-braw.def$evidence
  evidence$shortHand<-TRUE
  setBrawDef("evidence",evidence)
}

#' set shortHand to false
#' @export
undoShortHand<-function() {
  evidence<-braw.def$evidence
  evidence$shortHand<-FALSE
  setBrawDef("evidence",evidence)
}


#' switch Replication on
#' @export
doReplication<-function() {
  e<-braw.def$design
  e$Replication$On<-TRUE
  setBrawDef("design",e)
}

#' set inference method to NHST
#' @export
doNHST<-function() {
  setBrawEnv("STMethod","NHST")
}

#' set inference method to dLLR
#' @export
doDLLR<-function() {
  evidence<-braw.def$evidence
  evidence$prior<-getWorld("Psych")
  setBrawDef("evidence",evidence)
  setBrawEnv("STMethod","dLLR")
}

################################################################        
# Hypothesis objects
#

#' make a world
#'  an object that specifies the distribution of effect sizes
#' 
#' @param PDF    "Single","Double","Uniform","Gauss","Exp","Gamma","GenExp"
#' @param RZ     "r","z"
#' @returns a world object
#' @seealso showWorld(world=makeWorld())
#' @examples
#' makeWorld<-function(On=FALSE,PDF="Single",RZ="r",
#'                     PDFk=0.2,pRPlus=1,worldAbs=FALSE
#' )
#' @export
makeWorld<-function(On=TRUE,PDF="Uniform",RZ="r",
                    PDFk=0.0,PDFs=2,PDFmu=0.0,pRPlus=1,
                    PDFsample=FALSE,PDFsamplemn=0.0,PDFsamplesd=0.0,PDFsamplebias=FALSE,
                    worldAbs=FALSE) {
 world<-list(On=On,
             PDF=PDF,RZ=RZ,
             PDFk=PDFk,PDFs=PDFs,PDFmu=PDFmu,pRPlus=pRPlus,
             PDFsample=PDFsample,PDFsamplemn=PDFsamplemn,PDFsamplesd=PDFsamplesd,PDFsamplebias=PDFsamplebias,
             worldAbs=worldAbs)
 world  
}

# PREDICTION & DESIGN & EVIDENCE
#' make an effect object
#' 
#' @param ResidDistr   "normal","skewed","uniform","cauchy","t(3)"
#' @returns an effect object
#' @examples
#' makeEffect(rIV=0.3,rIV2=0,rIVIV2=0,rIVIV2DV=0,rSD=0,Heteroscedasticity=0,
#'            ResidDistr="normal",world=makeWorld()
#' )
#' @export
makeEffect<-function(rIV=0,rIV2=0,rIVIV2=0,rIVIV2DV=0,rSD=0,
                     rM1=NULL,rM2=NULL,
                     Heteroscedasticity=0,ResidDistr="normal",
                     world=makeWorld(FALSE)){

  # check effect sizes before going any further
  fullES<-rIV^2+rIV2^2+2*rIV*rIV2*rIVIV2+rIVIV2DV^2
  while (fullES>=1) {
    rIV<-rIV*0.9
    rIV2<-rIV2*0.9
    rIVIV2<-rIVIV2*0.9
    rIVIV2DV<-rIVIV2DV*0.9
    fullES<-rIV^2+rIV2^2+2*rIV*rIV2*rIVIV2+rIVIV2DV^2
  }
  effect<-list(rIV=rIV,rIV2=rIV2,rIVIV2=rIVIV2,rIVIV2DV=rIVIV2DV,rSD=rSD,
               rM1=rM1,rM2=rM2,
               Heteroscedasticity=Heteroscedasticity,ResidDistr=ResidDistr,
               world=world
  )
  
  effect
}

#' make a hypothesis object
#' 
#' @returns a hypothesis object
#' @seealso showHypothesis()
#' @examples
#' makeHypothesis(IV=makeVariable("IV"),IV2=NULL,DV=makeVariable("DV"),effect=makeEffect())
#' @export
makeHypothesis<-function(IV=makeVariable("IV"),IV2=NULL,DV=makeVariable("DV"),
                         effect=makeEffect(),layout="normal") {
  if (is.character(IV)) IV<-getVariable(IV)
  if (is.character(IV2)) IV2<-getVariable(IV2)
  if (is.character(DV)) DV<-getVariable(DV)
  hypothesis<-list(IV=IV,IV2=IV2,DV=DV,effect=effect,layout=layout)
  # assign("hypothesis",hypothesis,braw.def)
  # braw.def$hypothesis<<-hypothesis
  return(hypothesis)
}

########################################
# Design objects

#' make a sampling method object
#' 
#' "Random"
#'    purely random sample from whole range
#' "Stratified"
#'   sampled at specific intervals
#'   
#' "Cluster"
#'   a number of clusters, 
#'   each cluster having a particular radius within the population
#'   within each cluster a number of members
#'   random sampling within each cluster
#'   
#' "Snowball"
#'   a number of clusters,
#'   each cluster having a particular radius within the population
#'   within each cluster a number of members
#'   from each cluster member a chain of contacts
#'   each contact having a particular radius from their predecessor
#'   
#' "Convenience"
#'   like Snowball but more clusters and shorter chains of contacts
#' 
#' @param type   "Random","Limited,"Stratified","Cluster","Snowball","Convenience"
#' @returns an effect object
#' @examples
#' makeSampling(type="Random")
#' @export
makeSampling<-function(type="Random") {
  switch (type,
          "Random"={method=list(type="Random")},
          "Limited"={method=list(type="Limited",sLimitedRange=4.5)},
          "Stratified"={
            method=list(type="Stratified",
                        sStrata_rRange=2,sStrata_n=5
                        )},
          "Cluster"={
            method=list(type="Cluster",
                          Main_rad=1,
                          Cluster_n=10,
                          Cluster_rad=0.3,
                          Contact_n=0,
                          Contact_rad=1
            )
          },
          "Snowball"={
            method=list(type="Snowball",
                           Main_rad=1,
                           Cluster_n=1,
                           Cluster_rad=1,
                           Contact_n=10,
                           Contact_rad=0.2
            )
          },
          "Convenience"={
            method=list(type="Convenience",
                              Main_rad=1,
                              Cluster_n=3,
                              Cluster_rad=0.3,
                              Contact_n=3,
                              Contact_rad=0.2
            )
          }
  )
}
#' make a replication object
#' 
#' @param Keep       "Cautious", "Last", "LargeN", "SmallP", "Median"
#' @param PowerPrior "None", "World", "Prior"
#' @param BudgetType "Fixed", "Unlimited"
#' @returns a replication object
#' @examples
#' makeReplication(On=TRUE,Repeats=1,Keep="Cautious",RepAlpha=0.05,
#'                 PowerOn=TRUE,Power=0.8,Tails=2,PowerPrior="None",
#'                 forceSigOriginal=FALSE,forceSign=TRUE,
#'                 BudgetType="Unlimited",Budget=1000
#'                 )
#' @export
makeReplication<-function(On=TRUE,Repeats=1,Keep="Cautious",RepAlpha=0.05,
                          PowerOn=TRUE,Power=0.8,Tails=2,PowerPrior="None",
                          UseLikelihood=FALSE,
                          replicateAll=FALSE,
                          forceSigOriginal=FALSE,forceSign=TRUE,
                          maxN=2000,
                          BudgetType="Unlimited",Budget=1000,
                          RepNoStudies=1
                          ) {
  
  replication<-list(On=On,Repeats=Repeats,Keep=Keep,RepAlpha=RepAlpha,
                    PowerOn=PowerOn,Power=Power,Tails=Tails,PowerPrior=PowerPrior,
                    UseLikelihood=UseLikelihood,
                    replicateAll=replicateAll,
                    forceSigOriginal=forceSigOriginal,forceSign=forceSign,
                    maxN=maxN,
                    BudgetType=BudgetType,Budget=Budget
                    )
}

#' make a design
#' 
#' @param sMethod         sampling method object
#' @param sIV1Use         "Between","Within"
#' @param sCheating       "None","Grow","Prune","Replace","Retry"
#' @param sCheatingLimit  "Fixed","Budget"
#' @returns a design object
#' @seealso [showDesign()]
#' @examples
#' makeDesign(sN=42, sMethod=makeSampling("Random"),sMethodSeverity=0.1,
#'            sNRand=FALSE,sNRandK=2,sNRandDist="Gamma",
#'            sBudgetOn=FALSE,sNBudget=1000,
#'            sIV1Use="Between",sIV2Use="Between",  sWithinCor=0.5,
#'            
#'            sRangeOn=FALSE, sIVRange=c(-3,3), sDVRange=c(-3,3), sRangeProb=1,
#'            sDependence=0, sOutliers=0, sNonResponse=0,
#'            
#'            sCheating="None", sCheatingAttempts=5,
#'            sCheatingLimit="Fixed", sCheatingBudget=1000,
#'            
#'            Replication=makeReplication()
#' )
#' @export
makeDesign<-function(sN=42, sMethod=makeSampling("Random"),sMethodSeverity=0.1,
                     sNRand=FALSE,sNRandSD=33.3, sNRandDist="Gamma",
                     sIV1Use="Between",sIV2Use="Between", 
                     sWithinCor=0.5,
                     sBudgetOn=FALSE,sNBudget=1000,
                     sIVRangeOn=FALSE, sIVRange=c(-1,1)*4, 
                     sIV2RangeOn=FALSE, sIV2Range=c(-1,1)*4, sDVRange=c(-1,1)*4, 
                     sRangeProb=1, sRangeVary=0,
                     sDependence=0, sOutliers=0, sNonResponse=0,
                     sCheating="None",sCheatingAttempts=10,sCheatingLimit="Fixed",sCheatingBudget=1000,sCheatingFixedPop=TRUE,
                     Replication=makeReplication(FALSE)
) {
  
  if (is.character(sN)) 
    design<-getDesign(sN)
  else
  design<-list(sN=sN, sMethod=sMethod, sMethodSeverity=sMethodSeverity,
               sNRand=sNRand,sNRandSD=sNRandSD,sNRandDist=sNRandDist,
               sIV1Use=sIV1Use,sIV2Use=sIV2Use, 
               sWithinCor=sWithinCor,
               sBudgetOn=sBudgetOn,sNBudget=sNBudget,
               sIVRangeOn=sIVRangeOn, sIVRange=sIVRange, 
               sIV2RangeOn=sIV2RangeOn, sIV2Range=sIV2Range, sDVRange=sDVRange, 
               sRangeProb=sRangeProb, sRangeVary=sRangeVary,
               sDependence=sDependence, sOutliers=sOutliers,sNonResponse=sNonResponse,
               sCheating=sCheating,sCheatingAttempts=sCheatingAttempts,sCheatingLimit=sCheatingLimit,sCheatingBudget=sCheatingBudget,sCheatingFixedPop=sCheatingFixedPop,
               Replication=Replication
               )
  # assign("design",design,braw.def)
  # braw.def$design<<-design
  
    design
}
####################################
# evidence objects

#' make an evidence definition
#' 
#' @param ssqType     "Type1","Type2","Type3"
#' @param caseOrder   "Alphabetic","AsFound","Frequency"
#' @param Transform   "None","Log","Exp"
#' @examples
#' makeEvidence(shortHand=FALSE,sigOnly=0,
#'              AnalysisTerms=TRUE,rInteractionOnly=TRUE,ssqType="Type3",
#'              caseOrder="Alphabetic",
#'              llr=list(e1=c(),e2=0),
#'              useAIC="AIC",
#'              doSEM=FALSE,
#'              Welch=FALSE,Transform="None",
#'              prior=makeWorld(TRUE,"Uniform","r")
#'              metaAnalysis=makeMetaAnalysis()
#'              )
#' @export
makeEvidence<-function(shortHand=FALSE,sigOnly=0,
                       AnalysisTerms=c(TRUE,TRUE,FALSE),rInteractionOnly=TRUE,ssqType="Type3",
                       caseOrder="AsStated",
                       llr=list(e1=c(),e2=0),
                       useAIC="AIC",
                       doSEM=FALSE,
                       Welch=FALSE,Transform="None",
                       McFaddens=TRUE,
                       minRp=0,
                       prior=makeWorld(TRUE,"Uniform","r"),
                       metaAnalysis=makeMetaAnalysis()
                       ){
  
  evidence<-list(AnalysisTerms=AnalysisTerms,rInteractionOnly=rInteractionOnly,ssqType=ssqType,
                 caseOrder=caseOrder,shortHand=shortHand,sigOnly=sigOnly,
                 llr=llr,useAIC=useAIC,doSEM=doSEM,
                 Welch=Welch,Transform=Transform,McFaddens=McFaddens,
                 minRp=minRp,
                 prior=prior,
                 metaAnalysis=metaAnalysis
  )

  # braw.def$evidence<<-evidence
  evidence
}

##############################################

#' set default hypothesis
#' @export
setDefaults<-function() {
  setBrawDef("hypothesis",braw.def$defaultHypothesis)
  setBrawDef("design",braw.def$defaultDesign)
  setBrawDef("evidence",braw.def$defaultEvidence)
  setBrawDef("metaAnalysis",braw.def$defaultMetaAnalysis)
  setBrawDef("explore",braw.def$defaultExplore)
}

#' set default hypothesis
#' @export
setHypothesis<-function(IV=braw.def$hypothesis$IV,IV2=braw.def$hypothesis$IV2,DV=braw.def$hypothesis$DV,
                        effect=braw.def$hypothesis$effect,layout=braw.def$hypothesis$layout) {
  if (is.list(IV) && !is.null(IV$IV)) e<-IV
  else
    e<-makeHypothesis(IV=IV,IV2=IV2,DV=DV,effect=effect,layout=layout)
  setBrawDef("hypothesis",e)
}

#' set default effect
#' @export
setEffect<-function(rIV=braw.def$hypothesis$effect$rIV,rIV2=braw.def$hypothesis$effect$rIV2,rIVIV2=braw.def$hypothesis$effect$rIVIV2,rIVIV2DV=braw.def$hypothesis$effect$rIVIV2DV,rSD=braw.def$hypothesis$effect$rSD,
                    rM1=braw.def$hypothesis$effect$rM1,rM2=braw.def$hypothesis$effect$rM2,
                    Heteroscedasticity=braw.def$hypothesis$effect$Heteroscedasticity,ResidDistr=braw.def$hypothesis$effect$ResidDistr,
                    world=braw.def$hypothesis$effect$world) {
  e<-makeEffect(rIV=rIV,rIV2=rIV2,rIVIV2=rIVIV2,rIVIV2DV=rIVIV2DV,rSD=rSD,
                rM1=rM1,rM2=rM2,
                Heteroscedasticity=Heteroscedasticity,ResidDistr=ResidDistr,
                world=world)
  h<-braw.def$hypothesis
  h$effect<-e
  setBrawDef("hypothesis",h)
}

#' set default world
#' @export
setWorld<-function(On=braw.def$hypothesis$effect$world$On,
                   PDF=braw.def$hypothesis$effect$world$PDF,RZ=braw.def$hypothesis$effect$world$RZ,
                   PDFk=braw.def$hypothesis$effect$world$PDFk,PDFs=braw.def$hypothesis$effect$world$PDFs,
                   PDFmu=braw.def$hypothesis$effect$world$PDFmu,pRPlus=braw.def$hypothesis$effect$world$pRPlus,
                   PDFsample=braw.def$hypothesis$effect$world$PDFsample,PDFsamplemn=braw.def$hypothesis$effect$world$PDFsamplemn,PDFsamplesd=braw.def$hypothesis$effect$world$PDFsamplesd,PDFsamplebias=braw.def$hypothesis$effect$world$PDFsamplebias,
                   worldAbs=braw.def$hypothesis$effect$world$worldAbs) {
  if (is.character(On)) e<-getWorld(On)
  else {
    if (is.list(On)) e<-On
    else
      e<-makeWorld(On=On,
                   PDF=PDF,RZ=RZ,
                   PDFk=PDFk,PDFs=PDFs,PDFmu=PDFmu,pRPlus=pRPlus,
                   PDFsample=PDFsample,PDFsamplemn=PDFsamplemn,PDFsamplesd=PDFsamplesd,PDFsamplebias=PDFsamplebias,
                   worldAbs=worldAbs)
  }
  h<-braw.def$hypothesis
  h$effect$world<-e
  setBrawDef("hypothesis",h)
}

#' set default design
#' @export
setDesign<-function(sN=braw.def$design$sN, sMethod=braw.def$design$sMethod, sMethodSeverity=braw.def$design$sMethodSeverity,
                    sNRand=braw.def$design$sNRand,sNRandSD=braw.def$design$sNRandSD,sNRandDist=braw.def$design$sNRandDist,
                    sIV1Use=braw.def$design$sIV1Use,sIV2Use=braw.def$design$sIV2Use, 
                    sWithinCor=braw.def$design$sWithinCor,
                    sBudgetOn=braw.def$design$sBudgetOn,sNBudget=braw.def$design$sNBudget,
                    sIVRangeOn=braw.def$design$sIVRangeOn, sIVRange=braw.def$design$sIVRange, 
                    sIV2RangeOn=braw.def$design$sIV2RangeOn, sIV2Range=braw.def$design$sIV2Range, sDVRange=braw.def$design$sDVRange, 
                    sRangeProb=braw.def$design$sRangeProb, sRangeVary=braw.def$design$sRangeVary,
                    sDependence=braw.def$design$sDependence, sOutliers=braw.def$design$sOutliers,sNonResponse=braw.def$design$sNonResponse,
                    sCheating=braw.def$design$sCheating,sCheatingAttempts=braw.def$design$sCheatingAttempts,sCheatingLimit=braw.def$design$sCheatingLimit,sCheatingBudget=braw.def$design$sCheatingBudget,
                    Replication=braw.def$design$Replication) {
  if (is.list(sN)) e<-sN
  else
    e<-makeDesign(sN=sN, sMethod=sMethod, sMethodSeverity=sMethodSeverity,
                  sNRand=sNRand,sNRandSD=sNRandSD,sNRandDist=sNRandDist,
                  sIV1Use=sIV1Use,sIV2Use=sIV2Use, 
                  sWithinCor=sWithinCor,
                  sBudgetOn=sBudgetOn,sNBudget=sNBudget,
                  sIVRangeOn=sIVRangeOn, sIVRange=sIVRange, sIV2RangeOn=sIV2RangeOn, sIV2Range=sIV2Range, sDVRange=sDVRange, 
                  sRangeProb=sRangeProb,sRangeVary=sRangeVary,
                  sDependence=sDependence, sOutliers=sOutliers,sNonResponse=sNonResponse,
                  sCheating=sCheating,sCheatingAttempts=sCheatingAttempts,sCheatingLimit=sCheatingLimit,sCheatingBudget=sCheatingBudget,
                  Replication=Replication)
  setBrawDef("design",e)
}

#' set default evidence
#' @export
setEvidence<-function(shortHand=braw.def$evidence$shortHand,sigOnly=braw.def$evidence$sigOnly,
                      AnalysisTerms=braw.def$evidence$AnalysisTerms,rInteractionOnly=braw.def$evidence$rInteractionOnly,ssqType=braw.def$evidence$ssqType,
                      caseOrder=braw.def$evidence$caseOrder,
                      llr=braw.def$evidence$llr,useAIC=braw.def$evidence$useAIC,doSEM=braw.def$evidence$doSEM,
                      Welch=braw.def$evidence$Welch,Transform=braw.def$evidence$Transform,McFaddens=braw.def$evidence$McFaddens,
                      minRp=braw.def$evidence$minRp,
                      prior=braw.def$evidence$prior,
                      metaAnalysis=braw.def$evidence$metaAnalysis) {
  e<-makeEvidence(shortHand=shortHand,sigOnly=sigOnly,
                  AnalysisTerms=AnalysisTerms,rInteractionOnly=rInteractionOnly,ssqType=ssqType,
                  caseOrder=caseOrder,
                  llr=llr,useAIC=useAIC,doSEM=doSEM,
                  Welch=Welch,Transform=Transform,McFaddens=McFaddens,
                  minRp=minRp,
                  prior=prior,
                  metaAnalysis=metaAnalysis)
  setBrawDef("evidence",e)
}

##################################################################################  
