metaSciInstructions <- function(HelpType="Plan") {

  oldEvidence<-braw.def$evidence
  
  switch(HelpType,
         "Overview"={
           output<-c(
             '<b>Inferences?</b> inferences using statistical testing are, of course, subject to errors.',
             'How much do these matter?',
             '<ul style=margin:0px;>',
             '<li> False hits',
             '<li> False misses',
             '</ul>',
             '<br>',
             '<b>Sample Size?</b> a researcher has to make an important choice about sample size because larger sample sizes are more costly. ',
             '<ul style=margin:0px;>',
             '<li> Should I use the biggest I can afford?',
             '<li> Or should I use the smallest I can get away with?',
             '</ul>',
             '<br>',
             '<b>Sampling Method?</b> a researcher cannot make a random sample. So we compromise. How much does this matter? ',
             '<ul style=margin:0px;>',
             '<li> Should I use the biggest I can afford?',
             '<li> Or should I use the smallest I can get away with?',
             '</ul>',
             '<br>',
             '<b>Double Checking?</b> what do we do when there are 2 (or more) different results to compare or contrast?',
             '<ul style=margin:0px;>',
             '<li> Replication asks whether the result can be found again and is therefore competitive.',
             '<li> Meta-analysis asks what inference might cover both results and is therefore collaborative.',
             '</ul>',
             '<br>',
             '<b>Real Differences</b> sometimes 2 different results can happen quite properly. Why?',
             'In these cases, replication is highly problematical.',
             '<ul style=margin:0px;>',
             '<li> Interactions (moderations) can do this.',
             '<li> Covariation (mediations) can do this.',
             '</ul>',
             '<br>'
           )
         },
         "Inferences"={
           output<-c(
             '<b>Inferences?</b> inferences using statistical testing are, of course, subject to errors.',
             'How much do these matter?',
             '<ul style=margin:0px;>',
             '<li> False hits',
             '<li> False misses',
             '</ul>',
             '<br>',
             '<b>Just as an introduction</b> we use samples from this:',
             '<ul style=margin:0px;>',
             '<li> The population effect size is r<sub>p</sub>=0.3 with equal probability.',
             '<li> The sample size is 60',
             '</ul>',
             'Did you get a statistically significant result?',
             'Try this a few times and see (i) how safe the p-value is as a guide to the population.',
             '<br>',
             '<br>',
             '<b>In the first part of this step</b> we use a more realistic situation.',
             '<ul style=margin:0px;>',
             '<li> The population effect size is either r<sub>p</sub>=0.0 or r<sub>p</sub>=0.3 with equal probability.',
             '<li> The sample size is 60',
             '</ul>',
             'Can you guess which population your result came from?',
             'Did you get a statistically significant result?',
             '<br>',
             'Try this a few times and see (i) how easy it is to guess the population and ',
             '(ii) how safe the p-value is as a guide to which population.',
             '<br>',
             '<br>',
             '<b>In the second part of this step</b> we use a world that models Psychology.',
             '<ul style=margin:0px;>',
             '<li> The population effect size is either zero (50%) or drawn from an exponential distribution (50%).',
             '<li> The sample size is 60',
             '</ul>',
             'What happens now? How likely is a significant result? And if you get a significant result, how likely is it false discovery?',
             '<br>'
           )
         },
         
         "Sample Size"={output<-c(
           '<b>Sample Size?</b> a researcher has to make an important choice about sample size because larger sample sizes are more costly. ',
           '<ul style=margin:0px;>',
           '<li> Should I use the biggest I can afford?',
           '<li> Or should I use the smallest I can get away with?',
           '</ul>',
           '<br>',
           'Larger sample sizes cost more time, effort and often money. It is therefore worth asking whether two smaller studies are more productive than one large one.',
           'The question is important not least because increasing sample size offers diminishing returns.',
           'The benefit of going from 20 to 40 data points is much, much greater than the benefit of going from 120 to 140.',
           'In this step, we look at this.',
           '<br>',
           '<br>',
           '<b>In the first part of this step</b> you simply choose how many participants to use. ',
           'Whilst a larger sample always produces a more precise result, the question is how to justify the costs (not least to participants themselves).',
           '<br>',
           '<br>',
           '<b>In the second part of this step</b> you have a budget of 320 participants and you are operating in the model Psychology world.',
           '<ul style=margin:0px;>',
           '<li> Use all 320 participants in a single study.',
           '<li> Now split them into 4 studies.',
           '<li> Then 16 studies.',
           '<li> And then 64 studies.',
           '</ul>',
           'In each case, note how many significant result and how many false discoveries you make.',
           'Which combination gave you the most significant results? This is what will probably matter to you as a researcher.',
           'Which combination gave you the most false discoveries? This is what will matter most to the discipline.',
           '<br>')
         },
         
         "Sampling Method"={output<-c(
           '<b>Sampling Method?</b> a researcher cannot make a random sample. So we compromise. How much does this matter? ',
           '<ul style=margin:0px;>',
           '<li> Should I use the biggest I can afford?',
           '<li> Or should I use the smallest I can get away with?',
           '</ul>',
           '<br>',
           'Larger sample sizes cost more time, effort and often money. It is therefore worth asking whether two smaller studies are more productive than one large one.',
           'The question is important not least because increasing sample size offers diminishing returns.',
           'The benefit of going from 20 to 40 data points is much, much greater than the benefit of going from 120 to 140.',
           'In this step, we look at this.',
           '<br>',
           '<br>',
           '<b>In the first part of this step</b> you have a budget of 320 participants and you are operating in the model Psychology world.',
           '<ul style=margin:0px;>',
           '<li> Use all 320 participants in a single study.',
           '<li> Now split them into 4 studies.',
           '<li> Then 16 studies.',
           '<li> And then 64 studies.',
           '</ul>',
           'In each case, note how many significant result and how many false discoveries you make.',
           'Which combination gave you the most significant results? This is what will probably matter to you as a researcher.',
           'Which combination gave you the most false discoveries? This is what will matter most to the discipline.',
           '<br>',
           '<br>',
           '<b>For the second part, we are going to do a bit of cheating.</b> Each time we run a study, we start with a pre-planned sample size. If the result is not significant, then we add a few participants.',
           'We continue until either our budget is used up, or we have a significant result.',
           '<br>',
           'How does this affect the outcomes? Can you see any temptation here?',
           '<br>')
         },
         
         "Double Checking"={output<-c(
           '<b>Double Checking?</b> what do we do when there are 2 (or more) different results to compare or contrast?',
           '<ul style=margin:0px;>',
           '<li> Replication asks whether the result can be found again and is therefore competitive.',
           '<li> Meta-analysis asks what inference might cover both results and is therefore collaborative.',
           '</ul>',
           '<br>',
           'False discoveries are inevitable. For the most part their frequency is driven by the number of false hypotheses.',
           'It is hard to know whether a hypothesis is true or false and so there are no really good ways of preventing false discoveries.',
           'Instead, we must check up on a promising looking result.',
           '<br><br>',
           'There are two ways to do this:',           
           '<ul style=margin:0px;>',
           '<li> Replication: repeat the result and prefer the new result',
           '<li> Meta-analysis: repeat the result and combine the new result with the old one',
           '</ul>',
           'How do the two compare?',
           '<br><br>',
           '<b>In the first part of this step</b>, we look at replication.',
           'The process only looks at results that were significant in the first place. ',
           'There are 3 types of outcome: ns no follow-up; sig, then ns; sig then sig.',
           'If the population effect size is zero, then the first two outcomes are correct.',
           'If the population effect size is not zero, then the third outcome is correct.',
           'How many correct results does replication produce?',
           '<br>',
           'Replication is widely used and accepted, but is that entirely safe?',
           '<br><br>',
           '<b>In the second part of this step</b>, we look at meta-analysis.',
           'Although normally used with many studies, the process can be applied to just two studies',
           'and we can think of it as a way of combining an original study and the replication.',
           'As before, there are three possible outcomes and as before, we ask how many correct results meta-analysis produces.',
           '<br><br>',
           'What we see is the familiar issue of a trade-off between false discoveries (very few for replication)',
           'and false misses (very many for replication).',
           'Actually, the same trade-off exists for just one study when one changes alpha (normally 0.05).',
           '<br>')
         },
         
         "Real Differences"={output<-c(
           '<b>Real Differences</b> sometimes 2 different results can happen quite properly. Why?',
           'In these cases, replication is highly problematical.',
           '<ul style=margin:0px;>',
           '<li> Interactions (moderations) can do this.',
           '<li> Covariation (mediations) can do this.',
           '</ul>',
           '<br>',
           'When a study fails to replicate, this is usually understood to be an indication that the original finding was a false discovery.',
           'And, <it>sometimes</it> it is thought that the original study was in some way faulty.',
           '<br><br>',
           'In this step, we set up a situation where two different groups of researchers get completely conflicting results.',
           'However, there is a good reason for this. ',
           '<br><br>',
           'In short, the two researchers are, without noticing it, studying different populations as might happen if they worked in different countries.',
           'This can be shown by including a third variable in the hypothesis, where the different countries favour different values for that variable.',
           '<br>'
           )
         }
         )
  
  if (HelpType!="Overview") {
    switch(HelpType,
           "Inferences"=HelpNo<-1,
           "Sample Size"=HelpNo<-2,
           "Sampling Method"=HelpNo<-3,
           "Double Checking"=HelpNo<-4,
           "Real Differences"=HelpNo<-5
    )
    extras<-paste0('<br>',
                   'More information ',
                   '<a href=',
                   '"https://doingpsychstats.wordpress.com/metasci-',HelpNo,'/"',
                   ' target="_blank">',
                   'here',
                   '</a>',
                   ' and leave any comments ',
                   '<a href=',
                   '"https://doingpsychstats.wordpress.com/metasci-',HelpNo,'/#respond"',
                   ' target="_blank">',
                   'here',
                   '</a>'
    )
  }
  else extras<-c()
  
  output<-c(output,extras)
  output<-c("<div style='border: none; padding: 4px;'>",output,"</div>")
  
  wholePanel<-paste0(output,collapse=" ")
  return(wholePanel)
}

metaSciComment<-function(whichComment) {
  
  svgBox(height=120,aspect=1.5)
  setBrawEnv("graphicsType","HTML")
  
  p<-'Under construction: a few comments to help understand this.'
  if (is.element(whichComment,c(1:2))) 
    p<-paste0(
      '<b>Detect the null hypotheses</b><br> ',
      'Some questions to ask yourself as you decide whether the sample comes from r<sub>p</sub>=0 or r<sub>p</sub>=0.3:',
      '<ul style=margin:0px;>',
      '<li> Is the p-value a good guide?',
      '<li> Is the sample effect-size a good guide?',
      '</ul>',
      'Neither is very good because the sampling distributions for r<sub>p</sub>=0 and r<sub>p</sub>=0.3 overlap considerably. ',
      'A much larger sample size can help.',
      '<br><br>',
      'Note that NHST only allows us to reject r<sub>p</sub>=0 or not. ',
      'It does not allow us to reach a conclusion about r<sub>p</sub>=0.3. ',
      "Although in this toy world we've made it appears to.",
      '<br><br>',
      'Now try an experiment. Set the proportion of r<sub>p</sub>=0 to a much higher number (like p(H<sub>0</sub>)=80%). ',
      'Repeat a few samples and see whether knowing that there is high a prior likelihood that the r<sub>p</sub>=0 changes how you decide. ',
      'The p-values do not change with this change in the a-priori likelihood of r<sub>p</sub>=0. ',
      'This is definitely an issue in the widespread use of NHST across a discipline.'
    )
  if (is.element(whichComment,c(3:4))) 
    p<-paste0(
      '<b>Detect the null hypotheses</b><br> ',
      "Psychology has lots of very small effects. And researchers also test a lot of hypotheses that are wrong. ",
      "These combine to make it much harder to determine whether an effect is real or not. ",
      "Even a much larger sample size doesn't help much. ",
      'In terms of false discoveries, these small effects are a puzzle.',
      '<br><br>',
      'NHST works best (which may not be very good) when there is a lot of difference between r<sub>p</sub>=0 and r<sub>p</sub>!=0. ',
      'It also works best when the a priori likelihood of r<sub>p</sub>=0 is low. ',
      '<br>',
      'Neither holds in most real worlds.'
      
    )
  if (is.element(whichComment,c(5:6))) 
    p<-paste0(
      '<b>Choosing sample sizes with a limited budget</b><br> ',
      'The choice of how to use a limited budget is a reality for most researchers. ',
      'The simple issue here is that using more small samples will produce:',
      '<ul style=margin:0px;>',
      '<li> more significant results, which suits the researcher better,',
      '<li> but more of these are false discoveries, to the detriment of the disipline.',
      '</ul>',
      'This is the basic conflict of interest in the use of NHST: researcher vs their discipline?'
    )
  if (is.element(whichComment,c(7:8))) 
    p<-paste0(
      '<b>Cheating - does it matter how we get our data?</b><br> ',
      'These examples are blatant cheating. They all involve manipulating the sample to nudge the result to where the researcher wants. ',
      'It is important to understand how much cheating can affect a result.',
      '<ul style=margin:0px;>',
      '<li> grow: keep adding data until the result suits',
      '<li> prune: remove data until the results suit',
      '<li> replace: replace data until the results suit',
      '</ul>',
      'Note that they all produce samples that are real data. In theory, the data that these produce, <i>could</i> have been obtained fairly. ',
      '<br>',
      'Strictly speaking, unless the sample is obtained by absolutely <i>random</i> sampling, ',
      "NHST shouldn't be used. Any way in which the researcher is involved in data collection - advertising, paying, etc - ",
      'undermines the central assumption of NHST and is arguably cheating. ',
      '<br>',
      'In other words, <i>we are always cheating</i>.'
    )
  if (is.element(whichComment,c(9:10))) 
    p<-paste0(
      '<b>Replication - does it really help? Sort of.</b><br> ',
      'Pretty much the whole debate around statistical issues concerns false discoveries. But they are inescapable. ',
      'A standard, much used and wholly relied on approach, is replication: repeat the research and see whether the same result holds. ',
      '<ul style=margin:0px;>',
      "<li> If is does, then all is well",
      "<li> If is doesn't, then we have to ask which is more likely",
      '<ul style=margin:0px;margin-left:0px;>',
      '<li> original sample led to a Type I error',
      '<li> replication sample led to a Type II error',
      '</ul>',
      '</ul>',
      'It is always assumed that result from the replication sample should be preferred. ',
      'In this example, the replication sample size is calculated to give an intended power of 90%. ',
      "Even so, often it doesn't - check w<sub>p</sub> in the results of your simulation. ",
      '<br><br>',
      'How well does this work? ',
      '<br>',
      'You will see that replication removes nearly all false discoveries. But it also <i>always</i> removes further true discoveries. '
    )
  if (is.element(whichComment,c(11:12))) 
    p<-paste0(
      '<b>Meta-analysis - any better?</b><br> ',
      'Meta-analysis is also a standard, much used and wholly relied on procedure. ',
      'And there is no reason why an original sample and a repeat sample cannot be combined by meta-analysis to give an alternative to the replication "winner-takes-all". ',
      'The outcome can still have a sample effect size and a p-value: these will reflect both samples.',
      '<br><br>',
      'How well does this work? ',
      '<br>',
      'You will see that meta-analysis removes many but not all false discoveries. But it also preserves most true discoveries. ',
      '<br><br>',
      'This is meant as food for thought: replication is confrontational. Is that important or is it a hindrance? '
    )
  if (is.element(whichComment,c(13:14))){
    # setEvidence(AnalysisTerms=c(TRUE,TRUE,TRUE))
    s1<-showHypothesis()
    p<-paste0(
      '<b>Actually, a failure to replicate can be meaningful.</b><br> ',
      'In this, we will see some situations where replication should fail. Properly fail. ',
      '<br>',
      'They always involve an extra influence on the DV, a variable that is probably not even measured. ',
      'When that extra variable interacts or covaries with the IV we did measure, interesting things can happen.',
      'They also always involve the different groups having different ranges of values for that extra variable.',
      '<br>',
      '<div style="height:120px;padding:0;">',
      '<div style="height:120px;margin:0;padding:0;float:left;width:50%;">',
      '<br>',
      '<b>Interactions (moderations)</b> are where the strength of an effect of an IV on a DV is affected by a second IV. ',
      'This means that the measured main effect of IV1 will depend on the range of values of IV2 in the sample. ',
      '<br>',
      'In this example, group A have only the higher values for IV2 whereas group B have the lower values.',
      '<br>',
      '</div>',
      s1,'</svg>',
      '</div>'
    )
  }
    if (is.element(whichComment,c(15:16))){
      # setEvidence(AnalysisTerms=c(TRUE,TRUE,TRUE))
      s2<-showHypothesis()
      p<-paste0(
        '<b>Actually, a failure to replicate can be meaningful.</b><br> ',
        '<div style="height:120px;padding:0;">',
        '<div style="height:120px;margin:0;padding:0;float:left;width:50%;">',
        '<br>',
        '<b>Covariation</b> between two IVs. In this situation there are two separate effects of the IV on the DV: a direct one and an indirect one via the other IV.',
        'So the measured effect of IV on DV will depend on the range of values of IV2 in the sample. ',
        '<br>',
        'In this example, group A have a very restricted range of values for IV2 whereas group B have the full range.',
        '<br>',
        '</div>',
        s2,'</svg>',
        '</div>'
      )
    }

  setBrawDef("evidence",oldEvidence)
  
  return(paste0('<div style="height:500px;">',p,'<br>','</div>'))
    
}
