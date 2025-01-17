BrawInstructions <- function(HelpType="Plan") {
  

  switch(HelpType,
         "Overview"={
           output<-c(
             '<b>Plan</b> some research - make a hypothesis, a design and prepare an analysis.',
             '<ul style=margin:0px;>',
             '<li> What type of variables are you going to use - Interval, Ordinal or Categorical?',
             '<li> How big is the effect expected to be?',
             '<li> What size of sample will you use?',
             '<li> How will you obtain the sample?',
             '</ul>',
             '<br>',
             'Make a <b>Single Sample</b> of data, inspect it and analyse it.',
             '<ul style=margin:0px;>',
             '<li> See how the graphs depend on the type of variables you used.',
             '<li> Compare the sample effect size with the population effect size.',
             '<li> Did you get a statistically significant result?',
             '</ul>',
             '<br>',
             'Use <b>Multiple Samples</b> to see how variable the possible results of your plan are.',
             '<ul style=margin:0px;>',
             '<li> How often does your plan produce a significant result?',
             '<li> What parts of the plan have the biggest effect on that outcome?',
             '</ul>',
             '<br>',
             'Then <b>Explore</b> what happens when you cary parts of your plan.',
             '<ul style=margin:0px;>',
             '<li> How often does the probability of a significant result vary with sample size?',
             '<li> What happens when the hypothesis is for an effect size of zero?',
             '</ul>',
             '<br>'
           )
           extras<-c()
         },
         "Plan"={
           output<-c(
           'A <b>Plan</b> proposes 3 parts - hypothesis, design and analysis.',
           'The hypothesis is a question; the design is a method of obtaining evidence on that question; an analysis is the process of answering the question.',
           'In BrawStats, each of these is specified and then carried out.',
           '<ol>',
           ' <li>',
           '<b>Hypothesis:</b>',
           '<br> A hypothesis is a prediction about the relationship between variables. It requires 2 or more variables and a predicted effect size between each pair of variables.',
           'The variables are defined as a dependent variable, whose variations are of interest and independent variables whose variations might explain the variation in the dependent variable.',
           ' </li>',
           ' <li>',
           '<b>Design:</b>',
           '<br> A design is a specification of how a sample from the population will be obtained.',
           'The main components are the size of the sample and the method by which it will be recruited.',
           ' </li>',
           ' <li>',
           '<b>Analysis:</b>',
           '<br> An analysis is the procedure by which the sample will be used to reach an inference about the population.',
           'If null hypothesis testing is used, then the most important part of the analysis is the choice of &alpha; (normally 0.05).',
           ' </li>',
           '</ol>')
           
           extras<-c("Effect size types","Worlds")
           extrasContent<-c(
             paste('Effects are transmitted from a source variable to a destination.',
                   '<ul style=margin:0px;>',
                   '<li> <b>Direct</b>: the amount of variance transmitted through one direct link.',
                   '<li> <b>Unique</b>: the amount of variance transmitted only by that source variable.',
                   'The effect of one IV when all other IVs are controlled for.',
                   '<li> <b>Total</b>: the amount of variance transmitted summed over all possible routes.',
                   'The effect of one IV in the absence of any others.',
                   '</ul>'
             ),
             paste(
               'In simple mode, BrawStats models a single hypothesis with a fixed effect.',
               'The hypothesis is always correct. This is, of course, unrealistic.',
               '<br>',
               'A <b>world</b> is a system that models an entire discipline.',
               'The population effect sizes for each sample is drawn at random from a distribution of possible values.',
               'The population effect size has a probability of being set to zero. ie. the hypothesis is wrong.',
               'Sample sizes are also drawn at random for each sample from a distribution of possible values.',
               ''
             )
           )
         },
         
         "Single"={output<-c(
           'When the <b>Single</b> button is pressed, BrawStats generates a simulated sample of data using the hypothesis and design.',
           '<br><br>',
           'The effect sizes proposed in the hypothesis are treated as the population effect sizes.',
           'This means that the hypothesis is always correct - although the specific sample generated may not indicate this.',
           '<br><br>',
           '<b>Outputs:</b>',
           '<ul style=margin:0px;>',
           ' <li>',
           '<b>Compact</b> - shows the most important information.',
           ' </li>',
           ' <li>',
           '<b>Sample</b> - shows the individual variables. Use this to look for any simple issues with the data such as outliers.',
           ' </li>',
           ' <li>',
           '<b>Description</b> - shows the sample effect. Use this to look at the relationshiop between the variables.',
           ' </li>',
           ' <li>',
           '<b>Infer</b> - shows the inference about the hypothesis. Use this to see what the sample says about the hypothesis.',
           ' </li>',
           '</ul>',
           '<br>')
          extras<-c("Suggestions")
          extrasContent<-c(
            paste(
           '<ol style=margin:0px;>',
           ' <li>',
           'Make a few samples from just one hypothesis and design.',
           '<br> Look and see how much the samples vary from each other. No two samples are ever the same. No two samples lead to the same inference.',
           '<br>This is one of the fundamental problems that statistics has to deal with: the evidence from a sample is to a degree uncertain.',
           ' </li>',
           ' <li>',
           'Change the variable types.',
           '<br>What happens to the graphs? With the Infer display, you can see what statistical test is used for each combination of variable types.',
           ' </li>',
           '</ol>'
            )
           )
         },
         
         "Multiple"={output<-c(
           'Press the <b>Multiple</b> button and BrawStats will make a lot of samples, showing you the set of inferential outcomes.',
           'If you press the button again, more samples are added to the set unless you have changed something in the Plan.',
           '<br><br>',
           'This is a convenient way of seeing how the outcomes vary - and how much they vary.',
           'You will see that the outcomes are limited to a range of possible values.',
           'If we were to take a single sample, the outcome would be drawn from this limited range.',
           'In that way, the multiple samples show us what to expect from our Plan.',
           '<br><br>',
           '<b>Outputs:</b>',
           # '<br>',
           # 'The sample can be viewed in several different ways, each showing something different about the sample.',
           '<ul style=margin:0px;>',
           ' <li>',
           '<b>Basic</b> - shows the most important information.',
           ' </li>',
           ' <li>',
           '<b>p(sig)</b> - shows the probability of a significant result.',
           ' </li>',
           ' <li>',
           '<b>NHST</b> - shows the probability of the different possible outcomes: significant (true or false) and non-significant (true or false).',
           ' </li>',
           ' <li>',
           '<b>Hits</b> & <b>Misses</b> - as NHST but only using either the significant or the non-significant results.',
           ' </li>',
           '</ul>',
           'If you use NHST, Hits or Misses, then BrawStats will run equal number of the hypothesis you have planned and a null hypothesis. This is what would happen if your hypothesis has a 50:50 chance of being correct.',
           '<br><br>')
         extras<-c("Suggestions")
         extrasContent<-c(
           paste(
           '<ol style=margin:0px;>',
           ' <li>',
           'Change the sample size and look at 100 samples.',
           '<br>Do this with an effect size of r=0.3 to see how sample size affects how often you get a significant result (when you should).',
           '<br>Repeat this with an effect size of r=0.0 to significant results when you shouldn\'t.',
           ' </li>',
           ' <li>',
           'Using r=0.3, try to find a sample size that gives you 80% significant results.',
           ' </li>',
           ' <li>',
           'Change the sampling method and look at 100 samples.',
           '<br>Do this with an effect size of r=0.3 to see whether sampling method affects how often you get a significant result (when you should).',
           '<br>Repeat this with an effect size of r=0.0 to significant results when you shouldn\'t.',
           '<br>Can you see how sampling method matters?',
           ' </li>',
           ' <li>',
           'Using r=0.3, change the variable types.',
           '<br>What happens to how often you see a significant effect?. What variable type is best?',
           ' </li>',
           '</ol>'
           )
           )
         },
         
         "Explore"={output<-c(
           'Explore allows you to automatically vary any of the decisions in the Plan. This is much more convenient that changing the Plan by hand.',
           '<br><br>',
           'Choose which part of the plan to explore (hypothesis, design, analysis). Choose which decision to explore. Then press the <b>Explore</b> button. You may have a short wait (eg 10secs) for the result.',
           '<br><br>',
           '<b>Outputs:</b>',
           # '<br>',
           # 'The sample can be viewed in several different ways, each showing something different about the sample.',
           '<ul style=margin:0px;>',
           ' <li>',
           '<b>Basic</b> - shows the most important information.',
           ' </li>',
           ' <li>',
           '<b>p(sig)</b> - shows how the probability of a significant result changes with the decision being explored.',
           ' </li>',
           ' <li>',
           '<b>NHST</b> - shows the probability of the different possible outcomes: significant (true or false) and non-significant (true or false).',
           ' </li>',
           ' <li>',
           '<b>Hits</b> & <b>Misses</b> - as NHST but only using either the significant or the non-significant results.',
           ' </li>',
           '</ul>',
           'If you use NHST, Hits or Misses, then BrawStats will run equal number of the hypothesis you have planned and a null hypothesis. This is what would happen if your hypothesis has a 50:50 chance of being correct.',
           '<br><br>')
         
         extras<-c("Suggestions")
         extrasContent<-c(
           paste(
           '<ol style=margin:0px;>',
           ' <li>',
           'Explore the effects of hypothesis effect size.',
           '<br>First display the results as Basic.',
           'You can see that the sample effect sizes all centre around the respective population effect size.',
           '<br>The display the results as p(sig).',
           ' </li>',
           ' <li>',
           'Explore the effects of sample size.',
           '<br>Display the results as p(sig). Do this with an effect size of r=0.3 and then with r=0.0.',
           '<br>Repeat with an IV that is Categorical with 3 cases. Does this change the result?',
           '<br>This is a power analysis.',
           ' </li>',
           ' <li>',
           'Compare directly the effects of sampling method.',
           '<br>Do this with an effect size of r=0.3 and then with r=0.0.',
           '<br>First display the results as Basic.',
           'Then display the results as p(sig).',
           ' </li>',
           ' <li>',
           'Using two IVs.',
           '<br>Display the results as Basic.',
           ' </li>',
           '</ol>')
           )
         },
         "Key"={output<-c(
           '<style>',
           'td.here {vertical-align:top;padding-left:10px;padding-right:10px;}',
           '</style>',
           '<table>',
           ' <tr>',
           '   <td class="here">',
           '<b>Standard</b>',
           '<br><b>r<sub>s</sub></b> sample effectsize',
           '<br><b>r<sub>p</sub></b> population effectsize',
           '<br><b>r<sub>e</sub></b> sample error',
           '<br><b>n</b> sample size',
           '<br><b>p</b> p-value',
           '   </td>',
           '   <td class="here">',
           '<b>Power</b>',
           '<br><b>w<sub>s</sub></b> sample power (r<sub>s</sub> & n)',
           '<br><b>w<sub>p</sub></b> population power (r<sub>p</sub> & n)',
           '<br><b>n<sub>w</sub></b> required sample size from r<sub>s</sub>',
           '   </td>',
           '   <td class="here">',
           '<b>Replication</b>',
           '<br><b>r<sub>o</sub></b> original r<sub>s</sub>',
           '<br><b>n<sub>o</sub></b> original n',
           '<br><b>p<sub>o</sub></b> original p',
           '   </td>',
           ' </tr>',
           '</table>',
           '')
         extras<-c()
         extrasContent<-c()
         }
         )
  if (length(extras)>0) {
    output<-c(output,
              '<b>More...</b><br>')
    for (i in 1:length(extras)) {
      id<-paste0(HelpType,extras[i])
      output<-c(output,
                '<style> button.here {font-size:12px;margin:0px;border:none;cursor:pointer;background-color:#3498db;color:white;} </style>',
                '<button class="here" onclick="showExtra(event,\'',id,'\')">',extras[i],':','</button>',
                '<div ID="',id,'" style=display:none>', extrasContent[i],'</div><br>'
      )
    }
  }
  control<-paste0(
    '<script>',
    'function showExtra(evt, extraID) {',
    '  var tabState;',
    '    tabState = document.getElementById(extraID).style.display;',
    '    if (tabState!="block") {',
    '      document.getElementById(extraID).style.display = "block";',
    '    }',
    '    else {',
    '      document.getElementById(extraID).style.display = "none";',
    '    }',
    '}',
    '</script>'
  )
output<-c(control,"<div style='border: none; padding: 4px;'>",output,"</div>")

  wholePanel<-paste0(output,collapse=" ")
  return(wholePanel)

}