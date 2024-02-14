################################################################        
# update basic functions
#

makeWorld<-function(worldOn=FALSE,populationPDF="Single",populationPDFk=0.2,populationRZ="r",populationNullp=0,worldAbs=FALSE) {
 world<-list(worldOn=worldOn,populationPDF=populationPDF,populationPDFk=populationPDFk,populationRZ=populationRZ,populationNullp=populationNullp,worldAbs=worldAbs)
 world  
}

# PREDICTION & DESIGN & EVIDENCE
makeEffect<-function(rIV=0,rIV2=0,rIVIV2=0,rIVIV2DV=0,Heteroscedasticity=0,
                     ResidDistr="normal",world=NA){

  if (is.na(world))
    world<-list(worldOn=FALSE,populationPDF="Single",populationPDFk=rIV,populationRZ="r",populationNullp=0,worldAbs=FALSE)

  effect<-list(rIV=0,rIV2=0,rIVIV2=0,rIVIV2DV=0,
               Heteroscedasticity=Heteroscedasticity,ResidDistr=ResidDistr,
               world=world
  )
  
  effect
}

makeHypothesis<-function(IV=makeVariable("IV"),IV2=NULL,DV=makeVariable("DV"),effect=makeEffect()) {
  hypothesis<-list(IV=IV,IV2=IV2,DV=DV,effect=effect)
}

makeReplication<-function(sReplicationOn=FALSE,sReplPowerOn=TRUE,sReplPower=0.8,sReplTails=2,sReplType="Fixed",
                          sReplSigOnly="No",sReplRepeats=1,sReplKeep="last",sReplBudget=1000,
                          sReplCorrection="None",
                          sReplVarAlpha=FALSE,sReplAlpha=2) {
  
  replication<-list(sReplicationOn=sReplicationOn,
                    sReplPowerOn=sReplPowerOn,sReplPower=sReplPower,
                    sReplSigOnly=sReplSigOnly,
                    sReplType=sReplType,sReplRepeats=sReplRepeats,sReplBudget=sReplBudget,
                    sReplCorrection=sReplCorrection,sReplTails=sReplTails,
                    sReplKeep=sReplKeep,
                    sReplVarAlpha=sReplVarAlpha,sReplAlpha=sReplAlpha)
}

makeDesign<-function(sN=42, sMethod="Random" ,sNRand=FALSE,sNRandK=2, 
                     sIV1Use="Between",sIV2Use="Between", 
                     sWithinCor=0.5,
                     sRangeOn=FALSE, sIVRange=c(-3,3), sDVRange=c(-3,3), 
                     sDependence=0, sOutliers=0, sClustering=0,
                     sCheating="None",sCheatingLimit="Budget",sCheatingAmount=5,sCheatingBudget=1000,
                     sBudgetOn=FALSE,sNBudget=1000,
                     sReplication=makeReplication(),
                     sN_Strata=5, sR_Strata=2,
                     sNClu_Cluster=5,     sRClu_Cluster=0.7,
                     sNClu_Convenience=1, sRClu_Convenience=0.7, sNCont_Convenience=5, sRCont_Convenience=0.7, sRSpread_Convenience=0.5,
                     sNClu_Snowball=2,   sRClu_Snowball=0.7,   sNCont_Snowball=2,    sRCont_Snowball=0.7,    sRSpread_Snowball=0.1
) {
  
  design<-list(sN=sN, sNRand=sNRand,sNRandK=sNRandK,
               sBudgetOn=sBudgetOn,sNBudget=sNBudget,
               sMethod=sMethod ,sIV1Use=sIV1Use,sIV2Use=sIV2Use, 
               sWithinCor=sWithinCor,
               sRangeOn=sRangeOn, sIVRange=sIVRange, sDVRange=sDVRange, 
               sDependence=sDependence, sOutliers=sOutliers, sClustering=sClustering,
               sCheating=sCheating,sCheatingLimit=sCheatingLimit,sCheatingAmount=sCheatingAmount,sCheatingBudget=sCheatingBudget,
               sReplication=sReplication,
               sN_Strata=sN_Strata, sR_Strata=sR_Strata,
               sNClu_Cluster=sNClu_Cluster, sRClu_Cluster=sRClu_Cluster,
               sNClu_Convenience=sNClu_Convenience, sRClu_Convenience=sRClu_Convenience, sNCont_Convenience=sNCont_Convenience, sRCont_Convenience=sRCont_Convenience, sRSpread_Convenience=sRSpread_Convenience,
               sNClu_Snowball=sNClu_Snowball, sRClu_Snowball=sRClu_Snowball, sNCont_Snowball=sNCont_Snowball, sRCont_Snowball=sRCont_Snowball, sRSpread_Snowball=sRSpread_Snowball
               )

    design
}

makeEvidence<-function(rInteractionOn=TRUE,rInteractionOnly=TRUE,ssqType="Type3",dataType="Raw",analysisType="Anova",
                       caseOrder="Alphabetic",shortHand=FALSE,sigOnly=FALSE,
                       llr=list(e1=c(),e2=0),
                       Welch=FALSE,Transform="None",
                       usePrior="world",
                       prior=list(worldOn=FALSE,populationPDF="Uniform",
                                  populationPDFk=0,populationRZ="r",
                                  populationNullp=0)){
  
  evidence<-list(rInteractionOn=rInteractionOn,rInteractionOnly=rInteractionOnly,ssqType=ssqType,
                 caseOrder=caseOrder,shortHand=shortHand,sigOnly=sigOnly,
                 llr=llr,
                 Welch=Welch,Transform=Transform,
                 dataType=dataType,analysisType=analysisType,
                 usePrior=usePrior,
                 prior=prior
  )


  evidence
}

##################################################################################  
