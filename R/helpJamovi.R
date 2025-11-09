#' @export
JamoviInstructions <- function(hypothesis=braw.def$hypothesis,
                               design=braw.def$design,
                               HelpType="All") {
  
  to_char="to"
  in_char="in"
  # to_char="\u21D2"
  
  optionsGroup=NULL
  options=c()
  if (is.null(hypothesis$IV2)) {
    switch(hypothesis$DV$type,
           "Interval"={
             switch(hypothesis$IV$type,
                    "Interval"=test<-"correlationPearson",
                    "Ordinal"=test<-"correlationSpearman",
                    "Categorical"={
                      if (hypothesis$IV$ncats==2) test<-"t-test"
                      else                        test<-"anova"
                      repeated<-(design$sIV1Use=="Within")
                    }
             )
           },
           "Ordinal"={
             switch(hypothesis$IV$type,
                    "Interval"=test<-"correlationSpearman",
                    "Ordinal"=test<-"correlationSpearman",
                    "Categorical"={
                      if (hypothesis$IV$ncats==2) test<-"u-test"
                      else                        test<-"kw-test"
                      repeated<-(design$sIV1Use=="Within")
                    }
             )
           },
           "Categorical"={
             switch(hypothesis$IV$type,
                    "Interval"=test<-"logisticRegression",
                    "Ordinal"=test<-"logisticRegression",
                    "Categorical"={
                      test<-"chisqr"
                      repeated<-(design$sIV1Use=="Within")
                    }
             )
           }
    )
    switch(test,
           "correlationPearson"={
             ribbon="Regression"
             menu="Correlation Matrix"
             IVgoes="Variables"
             DVgoes="Variables"
             options=paste("<b>Pearson</b>",in_char,"<b>Correlation Coefficients</b>")
             graphMenu="Scatter Plot"
             IVGraph="X-Axis"
             DVGraph="Y-Axis"
             graphOptions=paste("<b>Linear</b>",in_char,"<b>Regression Line</b>")
           },
           "correlationSpearman"={
             ribbon="Regression"
             menu="Correlation Matrix"
             IVgoes="Variables"
             DVgoes="Variables"
             options=paste("<b>Spearman</b>",in_char,"<b>Correlation Coefficients</b>")
             graphMenu="Scatter Plot"
             IVGraph="X-Axis"
             DVGraph="Y-Axis"
             graphOptions=paste("<b>Linear</b>",in_char,"<b>Regression Line</b>")
           },
           "t-test"={
             ribbon="T-Tests"
             if (repeated) menu="Paired Samples T-Test"
             else menu="Independent Samples T-Test"
             IVgoes="Grouping Variable"
             DVgoes="Dependent Variables"
             options=paste("<b>Student's</b>",in_char,"<b>Tests</b>")
             graphMenu="Box Plot"
             IVGraph="Split by"
             DVGraph="Variables"
             graphOptions=paste("<b>Data</b>",in_char,"<b>Box Plots</b>")
           },
           "anova"={
             ribbon="ANOVA"
             if (repeated) menu="Repeated Measures ANOVA"
             else menu="One-way ANOVA"
             IVgoes="Grouping Variable"
             DVgoes="Dependent Variables"
             if (!repeated) 
                options=paste("<b>Assume equal (Fisher's)</b>",in_char,"<b>Variances</b>")
             graphMenu="Box Plot"
             IVGraph="Split by"
             DVGraph="Variables"
             graphOptions=paste("<b>Data</b>",in_char,"<b>Box Plots</b>")
           },
           "u-test"={
             ribbon="T-Tests"
             if (repeated) menu="Paired Samples T-Test"
             else menu="Independent Samples T-Test"
             IVgoes="Grouping Variable"
             DVgoes="Dependent Variables"
             if (repeated)
               options=paste("<b>Wilcoxon rank</b>",in_char,"<b>Tests</b>")
             else
               options=paste("<b>Mann-Whitney U</b>",in_char,"<b>Tests</b>")
             graphMenu="Box Plot"
             IVGraph="Split by"
             DVGraph="Variables"
             graphOptions=paste("<b>Data</b>",in_char,"<b>Box Plots</b>")
           },
           "kw-test"={
             ribbon="ANOVA"
             if (repeated) menu="Repeated Measures ANOVA (Friedman)"
             else menu="One-way ANOVA (Kruskal Wallace)"
             IVgoes="Grouping Variable"
             DVgoes="Dependent Variables"
             graphMenu="Box Plot"
             IVGraph="Split by"
             DVGraph="Variables"
             graphOptions=paste("<b>Data</b>",in_char,"<b>Box Plots</b>")
           },
           "logisticRegression"={
             ribbon="Regression"
             menu="2 Outcomes"
             IVgoes="Covariates"
             DVgoes="Dependent Variable"
             optionsGroup="<b>Model Fit</b>"
             options=c(
               paste("<b>Overall model test</b>",in_char,"<b>Fit Measures</b>"),
               paste("<b>McFadden's R<sup>2</sup></b>",in_char,"<b>Psuedo R<sup>2</sup></b>")
             )
             graphMenu="Scatter Plot"
             IVGraph="X-Axis"
             DVGraph="Y-Axis"
             graphOptions=paste("<b>Smooth</b>",in_char,"<b>Regression Line</b>")
           },
           "chisqr"={
             ribbon="Frequencies"
             menu="Independent Samples"
             IVgoes="Rows"
             DVgoes="Columns"
             graphMenu="Bar Plot"
             IVGraph="X-Axis"
             DVGraph="Counts"
             graphOptions=c()
           },
    )
  } else {
    switch(hypothesis$DV$type,
           "Interval"=test<-"generalLinear",
           "Ordinal"=test<-"generalLinear",
           "Categorical"=test<-"generalizedLinear"
    )
    if(hypothesis$IV$type=="Categorical") repeated1<-(design$sIV1Use=="Within")
    if(hypothesis$IV2$type=="Categorical") repeated2<-(design$sIV2Use=="Within")
    switch(test,
           "generalLinear"={
             ribbon="Regression"
             menu="Linear Regression"
             DVgoes="Dependent Variable"
             if (hypothesis$IV$type=="Categorical") IVgoes="Factors"
             else                                   IVgoes="Covariates"
             if (hypothesis$IV2$type=="Categorical") IV2goes="Factors"
             else                                    IV2goes="Covariates"
             graphMenu=NULL
           },
           "generalizedLinear"={
             ribbon="Regression"
             menu="2 Outcomes"
             DVgoes="Dependent Variable"
             if (hypothesis$IV$type=="Categorical") IVgoes="Factors"
             else                                   IVgoes="Covariates"
             if (hypothesis$IV2$type=="Categorical") IV2goes="Factors"
             else                                    IV2goes="Covariates"
             graphMenu=NULL
           }
    )
    DVgoes="Dependent Variable"
    if (hypothesis$IV$type=="Categorical") IVgoes="Factors"
    else IVgoes="Covariates"
    if (hypothesis$IV2$type=="Categorical") IV2goes="Factors"
    else IV2goes="Covariates"
  }
  
  EffectSizeribbon<-"Regression"
  if (hypothesis$DV$type=="Categorical") {
    EffectSizemenu="2 Outcomes"
    EffectSizeoptionsGroup="Model Fit"
    EffectSizeoptions=c(paste("<b>McFadden's R<sup>2</sup></b>",in_char,"<b>Pseudo R<sup>2</sup></b>"))
    EffectSizewarningLogistic="Take the square root of the R<sup>2</sup> effect size."
  } else {
    EffectSizemenu="Linear Regression"
    EffectSizeoptionsGroup="Model Fit"
    EffectSizeoptions=c(paste("<b>R</b>",in_char,"<b>Fit Measures</b>"))
    EffectSizewarningLogistic=""
  }
  
  output<-c("<div style='border: none; padding: 4px;'>")
  output<-c(output,paste0("Go to the top of the Jamovi window.<br>There is a row of <b>menu</b> options:",
                          '<span style="color:hsl(205, 100%, 41%)"><b> Variables</b>, <b>Data</b>, <b>Analyses</b>, <b>Plots</b>, <b>Edit</b></span>',
                          "<br>"),
                   paste0("Beneath each is a set of <b>icons</b>:",
                          '<span style="color:hsl(205, 100%, 41%)"> Exploration, T-Tests, ANOVA etc</span>',
                          "<br><br>")
  )
    
  # Analysis
  ribbon<-paste0('<span style="color:hsl(205, 100%, 41%)">',ribbon,'</span>')
  menu<-paste0('<span style="color:hsl(205, 100%, 41%)">',menu,'</span>')
  if (HelpType=="All" || HelpType=="Analysis") {
    output<-c(output,'<b>Analysis</b>: choose the menu <span style="color:hsl(205, 100%, 41%);"><b>Analyses</b></span>')
      output<-c(output,paste0('<ol style="margin-top:0px;"><li>Press the ',ribbon," icon",
                              "<br> & choose ",menu," from the drop down menu</li>"))
    list1<-paste0("<ul><li><b style=color:red>",hypothesis$DV$name,"</b> to <b>",DVgoes,"</b></li>")
    list1<-paste0(list1,"<li><b style=color:red>",hypothesis$IV$name,"</b> to <b>",IVgoes,"</b></li>")
    if (!is.null(hypothesis$IV2))
      list1<-paste0(list1,"<li><b style=color:red>",hypothesis$IV2$name,"</b> to <b>",IV2goes,"</b></li>")
    list1<-paste0(list1,"</ul>")
    output<-c(output,paste0("<li>Now move",list1,"</li>"))
    
    if (!is.null(options)) {
      if (!is.null(optionsGroup)) 
        output<-c(output,paste0("<li>Select <b>",optionsGroup,"</b> options group and check"))
      else 
        output<-c(output,paste0("<li>Set the option "))
      list2<-paste0("<ul>")
      for (option in options)
        list2<-paste0(list2,"<li>",option,"</li>")
      list2<-paste0(list2,"</ul>")
      output<-c(output,list2,paste0("</li>"))
    }
    
    output<-c(output,paste0("</ol>"))
  }
  
  # Graph
  if (HelpType=="All" || HelpType=="Graph") {
    if (is.null(graphMenu)) {
      output<-c(output,"<b>Graph</b>: Equivalent graphs not available<br><br>")
    } else {
      output<-c(output,'<b>Graph</b>: choose the menu <span style="color:hsl(205, 100%, 41%);"><b>Plots</b></span>')
      graphMenu<-paste0('<span style="color:hsl(205, 100%, 41%)">',graphMenu,'</span>')
      output<-c(output,paste0('<ol style="margin-top:0px;"><li>Press the ',graphMenu," icon"))
      list1<-paste0("<ul><li><b style=color:red>",hypothesis$DV$name,"</b> to <b>",DVGraph,"</b></li>")
      list1<-paste0(list1,"<li><b style=color:red>",hypothesis$IV$name,"</b> to <b>",IVGraph,"</b></li>")
      if (!is.null(hypothesis$IV2))
        list1<-paste0(list1,"<li><b style=color:red>",hypothesis$IV2$name,"</b> to <b>",IV2Graph,"</b></li>")
      list1<-paste0(list1,"</ul>")
      output<-c(output,paste0("<li>Now move",list1,"</li>"))
      
      if (!is.null(graphOptions)) {
        output<-c(output,paste0("<li>Set the option "))
        list2<-paste0("<ul>")
        for (option in graphOptions)
          list2<-paste0(list2,"<li>",option,"</li>")
        list2<-paste0(list2,"</ul>")
        output<-c(output,list2,paste0("</li>"))
      }
      
      output<-c(output,paste0("</ol>"))
    }
  }
  
  # EffectSize
  EffectSizeribbon<-paste0('<span style="color:hsl(205, 100%, 41%)">',EffectSizeribbon,'</span>')
  EffectSizemenu<-paste0('<span style="color:hsl(205, 100%, 41%)">',EffectSizemenu,'</span>')
  if (HelpType=="All" || HelpType=="EffectSize") {
    output<-c(output,'<b>Effect Size</b>: choose the menu <span style="color:hsl(205, 100%, 41%);"><b>Analyses</b></span>')
    output<-c(output,paste0('<ol style="margin-top:0px;"><li>Press the ',EffectSizeribbon," icon",
                            "<br> & choose ",EffectSizemenu," from the drop down menu</li>"))
    DVgoes="Dependent Variable"
    if (hypothesis$IV$type=="Categorical") IVgoes="Factors"
    else                                   IVgoes="Covariates"
    
    list1<-paste0("<ul><li><b style=color:red>",hypothesis$DV$name,"</b> to <b>",DVgoes,"</b></li>")
    list1<-paste0(list1,"<li><b style=color:red>",hypothesis$IV$name,"</b> to <b>",IVgoes,"</b></li>")
    if (!is.null(hypothesis$IV2)) {
      if (hypothesis$IV2$type=="Categorical") IV2goes="Factors"
      else                                   IV2goes="Covariates"
      list1<-paste0(list1,"<li><b style=color:red>",hypothesis$IV2$name,"</b> to <b>",IV2goes,"</b></li>")
    }
    list1<-paste0(list1,"</ul>")
    output<-c(output,paste0("<li>Now move",list1,"</li>"))
    
    if (!is.null(EffectSizeoptions)) {
      if (!is.null(EffectSizeoptionsGroup)) 
        output<-c(output,paste0("<li>Select <b>",EffectSizeoptionsGroup,"</b> options group and set"))
      else 
        output<-c(output,paste0("<li>Set the option "))
      list2<-paste0("<ul>")
      for (option in EffectSizeoptions)
        list2<-paste0(list2,"<li>",option,"</li>")
      list2<-paste0(list2,"</ul>")
      output<-c(output,list2,paste0("</li>"))
    }
    output<-c(output,paste0("</ol>"))
    output<-c(output,paste0(EffectSizewarningLogistic))
  }
  output<-c(output,paste0("</div>"))
    
  wholePanel<-paste0(output,collapse="")
  return(wholePanel)

}