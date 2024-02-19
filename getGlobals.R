getGlobals<-function(BW=FALSE,fontScale=1) {

################################
# graph design

char3D<-1.3
labelSize<-4*fontScale

if (BW) graphcolours<-list(graphC="#FFFFFF",graphBack="#FFFFFF")
else    graphcolours<-list(graphC="#BFECFF",graphBack="#999999")

# graphcoloursBL<<-list(graphC="#BFECFF",graphBack="#999999")
# graphcoloursBW<<-list(graphC="#FFFFFF",graphBack="#FFFFFF")

plotColours<-list(graphC=graphcolours$graphC,graphBack=graphcolours$graphBack,
                  maineffectES="#FFCC00",covariationES="#FF1100",interactionES="#0011FF",
                  sampleC="#FFCC00",descriptionC="#FF9955",
                  descriptionC1="#FF5533",descriptionC2="#CCBB33",
                  infer_sigC="#11CC00",infer_nsigC="#FF4400",infer_none="#AAAAAA",
                  infer_sigNonNull="#11CC00",infer_isigNonNull="#881100",infer_nsNonNull="#881100",infer_nsdNonNull="#DDCCCC",
                  infer_sigNull="#118800",infer_isigNull="#FF4400",infer_nsNull="#FF4400",infer_nsdNull="#CCDDCC",
                  psig="#FFAA00",alpha="#44FF22",one="#FF4422",
                  fdr="#227700",fmr="#BB5555")

plotShapes<-list(data=21,study=22,parameter=21,meta=24)

# graph themes

  mainTheme<-theme(panel.background = element_rect(fill=graphcolours$graphBack, colour="black"),
                    panel.grid.major = element_line(linetype="blank"),panel.grid.minor = element_line(linetype="blank"),
                    plot.background = element_rect(fill=graphcolours$graphC, colour=graphcolours$graphC))
  SMplotTheme<-theme(plot.title=element_text(size=14,face="bold"),axis.title=element_text(size=16,face="bold"),
                      axis.text.x=element_text(size=12),axis.text.y=element_text(size=12))
  LGplotTheme<-theme(plot.title=element_text(size=21,face="bold"),axis.title=element_text(size=24,face="bold"),
                      axis.text.x=element_text(size=18),axis.text.y=element_text(size=18))
  
  plotTheme<-mainTheme+SMplotTheme+theme(plot.margin=margin(1.0,1.5,0.5,0.5,"cm"))
  diagramTheme<-mainTheme+SMplotTheme+theme(plot.margin=margin(0.15,0.8,0,0.25,"cm"))
  blankTheme<-mainTheme+theme(panel.background = element_rect(fill=graphcolours$graphC, colour=graphcolours$graphC))
  reportTheme<-blankTheme+theme(plot.margin=margin(0.15,0.8,0,0.25,"cm"),
                                axis.title.x=element_blank(),
                                axis.text.x=element_blank(),
                                axis.ticks.x=element_blank(),
                                axis.title.y=element_blank(),
                                axis.text.y=element_blank(),
                                axis.ticks.y=element_blank()
  )
  

BrawOpts<<-list(
  plotColours=plotColours,
  plotShapes=plotShapes,
  plotTheme=plotTheme,
  reportTheme=reportTheme,
  diagramTheme=diagramTheme,
  blankTheme=blankTheme,
  labelSize=4,
  char3D=1.3
)


##########################
# NHST constants

alphaSig<-0.05
alphaLLR<-0.5*qnorm(1-alphaSig/2)^2
STMethod<-"NHST"
lrRange<-10

BrawOpts<<-c(BrawOpts,
              list(alphaSig=alphaSig,
                   alphaLLR=alphaLLR,
                   STMethod=STMethod,
                   lrRange=lrRange)
)
  


#########################
# display choices

report_precision<<-3
graph_precision<<-2

RZ<<-"r"

z_range<<-1.5
r_range<<-0.99
w_range<<-c(0.05,1)
fullRange<<-3
nNpoints<<-101
worldNPoints<<-101
varNPoints<<-101

allScatter<<-"all"
showMedians<<-FALSE
minN<<-10
maxRandN<<-5 # times mean sample size
reportGroupMeans<<-TRUE
plotDescriptionCols<<-c()
CatCatcols<<-c()
doLegendBars<<-TRUE
doLegendPoints<<-FALSE
simData<<-TRUE

wPlotScale<<-"log10"
pPlotScale<<-"log10"
nPlotScale<<-"linear"

useSignificanceCols<<-TRUE
showInteractionOnly<<-TRUE

includeSingle<-FALSE  # in "All" meta-analysis

alphaChar<<-'\u03B1'

brawFormat<<-function(numbers,digits=3) {
  pad<-function(x) if(x>=0) paste0(" ",x) else x
  if (all(numbers==round(numbers))) {
    r<-sprintf(numbers,fmt="%d")
  } else {
    r<-sprintf(numbers,fmt=paste0("%0.",digits,"f"))
  }
  r<-unname(sapply(r,pad))
  r
}

##################################
# notation for worlds

rpLabel<<-bquote(bold(r[p]))
rsLabel<<-bquote(bold(r[s]))
zpLabel<<-bquote(bold(z[p]))
zsLabel<<-bquote(bold(z[s]))

# source("Notation.R")

# Pchar<-'\u03A9'
Pchar<<-"P" 
Zchar<<-"Z"
Lchar<<-'\u03BB'

useLabels<-list(psig="psig",UD="D",P="0")

switch(useLabels$psig,
       "psig"={pSigLabel<<-bquote(bold(p[.('sig')]))},
       "w"={pSigLabel<<-bquote(bold(w))}
)

LabelUD<<-useLabels$UD

posChar<<-"+"
nullChar<<-"0"
switch(useLabels$P,
       "+"={
         pPlus<<-TRUE
       },
       "0"={
         pPlus<<-FALSE
       },
       "-"={
         pPlus<<-FALSE
         nullChar<<-'\u2013'
       }
)


if (pPlus) {
  Ptypechar<-posChar 
} else {
  Ptypechar<-nullChar
}
Ltypechar<-posChar

pPlusLabel<<-paste0("P(",Ptypechar,")")

switch (LabelUD, 
        "U"={
          Plabel<<-bquote(bold(.(Pchar)^.(Ptypechar)))
          Llabel<<-bquote(bold(.(Lchar)^.(Ltypechar)))
          
          nonNullPositive<<-bquote(.(Zchar)^.(posChar)~'+sig')  # "Z+ +ve"
          nonNullNS<<-bquote(.(Zchar)^.(posChar) ~"ns")  # "Z+ -ve"
          nonNullNegative<<-bquote(.(Zchar)^.(posChar) ~"-sig")  # "Z+ -ve"
          nullPositive<<-bquote(.(Zchar)^.(nullChar) ~"+sig")   # "Z0 +ve"
          nullNS<<-bquote(.(Zchar)^.(nullChar) ~"ns")  # "Z0 -ve"
          nullNegative<<-bquote(.(Zchar)^.(nullChar) ~"-sig")  # "Z0 -ve"
        },
        "D"={
          Plabel<<-bquote(bold(.(Pchar)[.(Ptypechar)]))
          Llabel<<-bquote(bold(.(Lchar)[.(Ltypechar)]))
          
          nonNullPositive<<-bquote(.(Zchar)[.(posChar)] ~"+sig")  # "Z+ +ve"
          nonNullNS<<-bquote(.(Zchar)[.(posChar)] ~"ns")  # "Z+ -ve"
          nonNullNegative<<-bquote(.(Zchar)[.(posChar)] ~"-sig")  # "Z+ -ve"
          nullPositive<<-bquote(.(Zchar)[.(nullChar)] ~"+sig")   # "Z0 +ve"
          nullNS<<-bquote(.(Zchar)[.(nullChar)] ~"ns")  # "Z0 -ve"
          nullNegative<<-bquote(.(Zchar)[.(nullChar)] ~"-sig")  # "Z0 -ve"
        }
)
allPositive<<-bquote(.(Zchar) ~"+ve")
allNegative<<-bquote(.(Zchar) ~"ns")

if (pPlus) effect$world$populationNullp<<-1-effect$world$populationNullp

}
