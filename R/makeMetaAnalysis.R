
makeMetaAnalysis<-function(nstudies=100,
                           meta_fixedAnal="random",
                           meta_pdf="All",
                           sig_only=FALSE,
                           meta_psigAnal=FALSE,
                           meta_nullAnal=TRUE) {
  
  metaAnalysis<-list(
    nstudies=nstudies,
    meta_fixedAnal=meta_fixedAnal,
    meta_pdf=meta_pdf,
    sig_only=sig_only,
    meta_psigAnal=meta_psigAnal,
    meta_nullAnal=meta_nullAnal
  )
  
}

