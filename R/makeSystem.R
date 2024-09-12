################################################################        
# Hypothesis objects
#

#' make a world
#'  an object that specifies the distribution of effect sizes
#' 
#' @param populationPDF    "Single","Double","Uniform","Gauss","Exp"
#' @param populationRZ     "r","z"
#' @returns a world object
#' @seealso showWorld(world=makeWorld())
#' @examples
#' makeWorld<-function(worldOn=FALSE,populationPDF="Single",populationRZ="r",
#'                     populationPDFk=0.2,populationNullp=0,worldAbs=FALSE
#' )
#' @export
makeWorld<-function(worldOn=FALSE,populationPDF="Single",populationRZ="r",
                    populationPDFk=0.0,populationNullp=0,worldAbs=FALSE) {
 world<-list(worldOn=worldOn,populationPDF=populationPDF,populationPDFk=populationPDFk,populationRZ=populationRZ,populationNullp=populationNullp,worldAbs=worldAbs)
 world  
}

# PREDICTION & DESIGN & EVIDENCE
#' make an effect object
#' 
#' @param ResidDistr   "normal","skewed","uniform","cauchy","t(3)"
#' @returns an effect object
#' @examples
#' makeEffect(rIV=0.3,rIV2=0,rIVIV2=0,rIVIV2DV=0,Heteroscedasticity=0,
#'            ResidDistr="normal",world=makeWorld()
#' )
#' @export
makeEffect<-function(rIV=0,rIV2=0,rIVIV2=0,rIVIV2DV=0,Heteroscedasticity=0,
                     ResidDistr="normal",world=braw.def$world){

  effect<-list(rIV=rIV,rIV2=rIV2,rIVIV2=rIVIV2,rIVIV2DV=rIVIV2DV,
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
makeHypothesis<-function(IV=braw.def$IV,IV2=braw.def$IV2,DV=braw.def$DV,effect=braw.def$effect) {
  hypothesis<-list(IV=IV,IV2=IV2,DV=DV,effect=effect)
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
#' @param type   "Random","Stratified","Cluster","Snowball","Convenience"
#' @returns an effect object
#' @examples
#' makeSampling(type="Random")
#' @export
makeSampling<-function(type="Random") {
  switch (type,
          "Random"={method=list(type="Random")},
          "Stratified"={
            method=list(type="Stratified",
                        sStrata_rRange=2,sStrata_n=5
                        )},
          "Cluster"={
            method=list(type="Cluster",
                          Main_rad=1,
                          Cluster_n=6,
                          Cluster_rad=0.3,
                          Contact_n=1,
                          Contact_rad=0
            )
          },
          "Snowball"={
            method=list(type="Snowball",
                           Main_rad=1,
                           Cluster_n=2,
                           Cluster_rad=0.3,
                           Contact_n=8,
                           Contact_rad=0.3
            )
          },
          "Convenience"={
            method=list(type="Convenience",
                              Main_rad=1,
                              Cluster_n=4,
                              Cluster_rad=0.2,
                              Contact_n=2,
                              Contact_rad=0.3
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
#' makeReplication(On=FALSE,Repeats=1,Keep="Cautious",RepAlpha=0.05,
#'                 PowerOn=TRUE,Power=0.8,Tails=2,PowerPrior="None",
#'                 forceSigOriginal="No",
#'                 BudgetType="Unlimited",Budget=1000
#'                 )
#' @export
makeReplication<-function(On=FALSE,Repeats=1,Keep="Cautious",RepAlpha=0.05,
                          PowerOn=TRUE,Power=0.8,Tails=2,PowerPrior="None",
                          forceSigOriginal=FALSE,forceSign=TRUE,
                          BudgetType="Unlimited",Budget=1000
                          ) {
  
  replication<-list(On=On,Repeats=Repeats,Keep=Keep,RepAlpha=RepAlpha,
                    PowerOn=PowerOn,Power=Power,Tails=Tails,PowerPrior=PowerPrior,
                    forceSigOriginal=forceSigOriginal,forceSign=forceSign,
                    BudgetType=BudgetType,Budget=Budget
                    )
}

#' make a design
#' 
#' @param sMethod         sampling method object
#' @param sIV1Use         "Between","Within"
#' @param sCheating       "None","Grow","Prune","Replace","Retry","Add"
#' @param sCheatingLimit  "Fixed","Budget"
#' @returns a design object
#' @seealso [showDesign()]
#' @examples
#' makeDesign(sN=42, sMethod=makeSampling("Random") ,sNRand=FALSE,sNRandK=2, 
#'            sBudgetOn=FALSE,sNBudget=1000,
#'            sIV1Use="Between",sIV2Use="Between",  sWithinCor=0.5,
#'            
#'            sRangeOn=FALSE, sIVRange=c(-3,3), sDVRange=c(-3,3), 
#'            sDependence=0, sOutliers=0,
#'            
#'            sCheating="None", sCheatingAttempts=5,
#'            sCheatingLimit="Fixed", sCheatingBudget=1000,
#'            
#'            Replication=makeReplication()
#' )
#' @export
makeDesign<-function(sN=42, sMethod=makeSampling("Random") ,sNRand=FALSE,sNRandK=2, 
                     sIV1Use="Between",sIV2Use="Between", 
                     sWithinCor=0.5,
                     sBudgetOn=FALSE,sNBudget=1000,
                     sRangeOn=FALSE, sIVRange=c(-3,3), sDVRange=c(-3,3), 
                     sDependence=0, sOutliers=0, 
                     sCheating="None",sCheatingAttempts=5,sCheatingLimit="Fixed",sCheatingBudget=1000,
                     Replication=makeReplication()
) {
  
  design<-list(sN=sN, sMethod=sMethod, sNRand=sNRand,sNRandK=sNRandK,
               sIV1Use=sIV1Use,sIV2Use=sIV2Use, 
               sWithinCor=sWithinCor,
               sBudgetOn=sBudgetOn,sNBudget=sNBudget,
               sRangeOn=sRangeOn, sIVRange=sIVRange, sDVRange=sDVRange, 
               sDependence=sDependence, sOutliers=sOutliers,
               sCheating=sCheating,sCheatingLimit=sCheatingLimit,sCheatingAttempts=sCheatingAttempts,sCheatingBudget=sCheatingBudget,
               Replication=Replication
               )

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
#' makeEvidence(shortHand=FALSE,sigOnly=FALSE,
#'              rInteractionOn=TRUE,rInteractionOnly=TRUE,ssqType="Type3",
#'              caseOrder="Alphabetic",
#'              llr=list(e1=c(),e2=0),
#'              Welch=FALSE,Transform="None",
#'              prior=makeWorld(TRUE,"Uniform","r"))
#' @export
makeEvidence<-function(shortHand=FALSE,sigOnly=FALSE,
                       rInteractionOn=TRUE,rInteractionOnly=TRUE,ssqType="Type3",
                       caseOrder="Alphabetic",
                       llr=list(e1=c(),e2=0),
                       Welch=FALSE,Transform="None",
                       McFaddens=TRUE,
                       prior=makeWorld(TRUE,"Uniform","r")
                       ){
  
  evidence<-list(rInteractionOn=rInteractionOn,rInteractionOnly=rInteractionOnly,ssqType=ssqType,
                 caseOrder=caseOrder,shortHand=shortHand,sigOnly=sigOnly,
                 llr=llr,
                 Welch=Welch,Transform=Transform,McFaddens=McFaddens,
                 prior=prior
  )


  evidence
}

##################################################################################  
