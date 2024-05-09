prepareSample<-function(raw_data, doOrdinals=FALSE, maxOrdinal=9, header=c()){
  if (is.null(raw_data)) {return(NULL)}
  if (nrow(raw_data)==0) {return(NULL)}

  waste<-raw_data=="#N/A"
  raw_data[waste]<-NA
  
  if (is.null(header)) {
  vars<-colnames(raw_data)
  } else {
    vars<-header
  }

  # check for data type row
  if (all(is.element(raw_data[1,],c("Type","type","Interval","interval","Categorical","categorical")))){
    dataTypes<-raw_data[1,]
    raw_data<-raw_data[2:nrow(raw_data),]
  } else {
    dataTypes<-rep("Unknown",ncol(raw_data))
  }
  
  # check for a participant column
  if (is.element(vars[1],c("participant","Participant","participants","Participants"))) {
    vars[1] <- "participant"
  } else{
    participants<-1:nrow(raw_data)
    raw_data<-cbind(data.frame(participant=participants),raw_data)
    vars<-c("participant",vars)
    dataTypes<-c("Type",dataTypes)
  }
  
  withins<-grepl("[^[:space:]]{1,}[|]{1}[^[:space:]]{1,}[=]{1}[^[:space:]]{1,}",vars)
  added<-0
  if (any(withins)){
    DVs<-sub("[|]{1}[^[:space:]]{1,}","",vars[withins])
    IVs<-sub("[=]{1}[^[:space:]]{1,}","",sub("[^[:space:]]{1,}[|]{1}","",vars[withins]))
    DVwiths<-unique(sub("[|]{1}[^[:space:]]{1,}","",vars[withins]))
    IVwiths<-c()
    IVwithsadded<-c()
    IVuseDV<-c()
    # IVwiths<-unique(sub("[=]{1}[^[:space:]]{1,}","",sub("[^[:space:]]{1,}[|]{1}","",vars[withins])))
    # IVwithsadded<-zeros(length(IVwiths),1)
    z<-raw_data
    for (iDV in 1:length(DVwiths)){
      tempName<-paste("Minions",iDV,sep="")
      
      z<-gather_(raw_data, tempName, DVwiths[iDV], vars[startsWith(vars,paste(DVwiths[iDV],"|",sep=""))])
      # pull out the new column "DVname|IVname=Cname|IV2name=C2name"
      v<-z[tempName]
      # remove the "DVname"
      v1<-sapply(v,function(x) sub(paste(DVwiths[iDV],"|",sep=""),"",x))
      while (any(str_length(v1)>0)){
        # "|IVname=Cname"
        # find the "|IVname" bit
        IVlocal<-str_extract(v1,"[^=|]+|[^=|]+")
        # now the "=Cname" bits
        IVlocalcases<-str_match(v1,"=([^=|]+)")[,2]
        # add this variable to z
        # but only if it isn't already there
        use<-which(IVwiths==IVlocal[1])
        if (isempty(use) || isempty(IVwiths)) {
          IVwiths<-c(IVwiths,IVlocal[1])
          use<-which(IVwiths==IVlocal[1])
          IVwithsadded<-c(IVwithsadded,0)
          IVuseDV<-c(IVuseDV,"")
        } 
        if (IVwithsadded[use]==0) {
          # column 1 is participants the last column is the DV
          z<-cbind(z[1],IVlocalcases,z[2:ncol(z)])
          # and change its name
          colnames(z)[2]<-IVlocal[1]
          added<-added+1
          IVwithsadded[use]<-1
          IVuseDV[use]<-paste0(IVuseDV[use],",",DVwiths[iDV])
        }
        # remove the part we have just dealt with
        v1<-sub("[|].[^|]+","",v1)
      }
      # now remove the temporary column
      z[tempName]<-NULL
      raw_data<-z
    }
    vars<-colnames(raw_data)
  }
  deploys<-rep("Between",ncol(raw_data))
  targets<-rep("",ncol(raw_data))
  if (added>0) {
    for (i in 1:length(IVwiths)) {
      change<-which(vars == IVwiths[i])
      deploys[change]<-"Within"
      targets[change]<-paste0(IVuseDV[i],",")
    }
  }
  keep<-c()
  for (i in 1:length(vars)){
    data<-raw_data[,i]
    if (length(unique(data[!is.na(data)]))>1) {
      keep=c(keep,i)
    }
  }
  vars<-vars[keep]
  raw_data<-raw_data[,keep]
  dataTypes<-dataTypes[keep]

  importedData<-raw_data
  importedData[[1]]<-factor(importedData[[1]])
  
  # return(importedData)
  
  
  # now prepare the variables
  newVariables<-data.frame()
  ivar<-1
  for (i in 2:length(vars)){
    varname<-vars[i]
    data<-raw_data[,i]
    # set up default values first
    varmu<-0
    varsd<-1
    varskew<-0
    varkurt<-3
    varMedian<-3
    varIQR<-1
    varnlevs<-5
    varcases<-"C1,C2"
    varncats<-2
    varnprop<-paste(c(1,1),collapse=",")
    ordProportions<-NA
    
    if ((is.character(data[[1]]) || 
         is.element(dataTypes[i],c("Categorical","categorical"))) &&
        !is.element(dataTypes[i],c("Interval","interval"))
    ) {
      if (!is.character(data)){data<-data[[1]]}
      importedData[[ivar+1]]<-factor(importedData[[i]])
      vartype<-"Categorical"
      varcases<-sort(unique(data))
      varncats<-length(varcases)
      proportions<-hist(as.numeric(factor(data)),(0:varncats)+0.5,plot=FALSE)$density
      # proportions<-proportions/max(proportions)
      varnprop<-paste(format(proportions,digits=2),collapse=",")
    } else {
      data<-sapply(data,as.numeric)
      data<-data[!is.na(data)]
      importedData[[ivar+1]]<-importedData[[i]]
      if (doOrdinals && all(data==round(data),na.rm=TRUE) && all(data>=0,na.rm=TRUE) && max(data,na.rm=TRUE)<maxOrdinal) {
        vartype<-"Ordinal"
        varMedian<-median(data,na.rm=TRUE)
        varIQR<-IQR(data,na.rm=TRUE)/2
        varnlevs<-max(data)
        ordProportions<-hist(data,(0:varnlevs)+0.5,plot=FALSE)$density
        ordProportions<-paste(format(ordProportions,digits=2),collapse=",")
        varmu<-mean(data,na.rm=TRUE)
        varskew<-skewness(data,na.rm=TRUE)
        varkurt<-kurtosis(data,na.rm=TRUE)+3
        varsd<-sd(data,na.rm=TRUE)
      } else {
        vartype<-"Interval"
        varmu<-mean(data,na.rm=TRUE)
        varskew<-skewness(data,na.rm=TRUE)
        varkurt<-kurtosis(data,na.rm=TRUE)+3
        varsd<-sd(data,na.rm=TRUE)
      }
    }
    var<-makeVariable(name=varname,type=vartype,
                 mu=varmu,sd=varsd,skew=varskew,kurtosis=varkurt,
                 median=varMedian,iqr=varIQR,nlevs=varnlevs,ordProportions=ordProportions,
                 ncats=varncats,cases=paste(varcases,collapse=","),proportions=varnprop,
                 deploy=deploys[i],targetDeploys=targets[i],
                 process="data")
    if (ivar==1) newVariables<-data.frame(var)
    else newVariables<-rbind(newVariables,var)
    ivar<-ivar+1
  }
  allData<-list(data=importedData,variables=newVariables)
  setBrawRes("importedData",allData)
  return(allData)
}

readSample<-function(data,DV,IV,IV2=NULL) {

  if (is.character(data)) data<-read.csv(data)
  if (length(IV)>1) IV2<-IV[2]
  
  data1<-prepareSample(data)
  
  dvUse<-as.list(data1$variables[which(names(data)==DV),])
  ivUse<-as.list(data1$variables[which(names(data)==IV[1]),])
  if (!is.null(IV2)) as.list(iv2Use<-data1$variables[which(names(data)==IV2),])
  else iv2Use<-NULL
  setBrawDef("hypothesis",makeHypothesis(DV=dvUse,IV=ivUse,IV2=iv2Use))
  setBrawDef("design",makeDesign(sN=length(data1$data$participant)))
  
  sample<-doSample()
  return(sample)
}