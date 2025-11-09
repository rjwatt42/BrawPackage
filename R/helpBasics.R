basicsInstructions <- function(HelpType="1") {
  

  switch(HelpType,
         "start"={
           output<-c(
             '<b>Getting started</b>',
             '<br>',
             'These demos show how basic statistics works.',
             'If you work through them in sequence, you will be exploring the range of statistical principles that lie behind most analysis.',
             '<ul style=margin:0px;>',
             '<li> Data: some basic characteristics of data',
             '<li> Uncertainty: why samples are never the same as the population',
             '<li> Design: choose a plan that gives satisfactory rate of success',
             '<li> Linear models: putting several variables to work simultaneously',
             '<li> Path models: doing more complex, imaginative analysis <i>(to follow)</i>',
             '</ul>'
           )
         },
         "1"={output<-c(
           '<b>Data</b>',
           '<br>',
           'We begin with just dealing with data. Data is the link between the reality and the inference. ',
           'Data usually comes as a sample taken a population. That sample is usually a very small proportion of the whole population.',
           '<ul style=margin:0px;>',
           '<li> Data analysis is usually not difficult.',
           '<li> A sample of data can be good or bad.',
           '<li> There is always sampling error.',
           '</ul>'
         )},
         "1a"={output<-c(
           '<b>Demo 1a</b>',
           '<br>',
           'A simple demo to show how basic statistics is easy.',
           '<ul style=margin:0px;>',
           '<li> Make a sample of data.',
           '<li> Let BrawStats automatically analyse it.',
           '<li> Follow instructions to repeat the analysis in Jamovi.',
           '<li> And get the same result.',
           '</ul>'
         )
         },
         
         "1b"={output<-c(
           '<b>Demo 1b</b>',
           '<br>',
           "A demo about samples - what they should and shouldn't look like.",
           '<ul style=margin:0px;>',
           '<li> Make samples of data.',
           '<li> Look at the distributions of values that are produced by random sampling.',
           '<li> Repeat this with "convenience" sampling.',
           '<li> And see how this differs.',
           '</ul>'
         )
         },
         
         "1c"={output<-c(
           '<b>Demo 1c</b>',
           '<br>',
           'A demo about variability and uncertainty - how much samples can randomly differ from the population.',
           'This is the fundamental issue that statistics has to deal with.',
           '<ul style=margin:0px;>',
           '<li> Make a population with known <i>r<sub>p</sub></i>.',
           '<li> Draw samples from it and see how much <i>r<sub>s</sub></i> differs from <i>r<sub>p</sub></i>.',
           '<li> Repeat this with <i>r<sub>p</sub></i> set to zero.',
           '<li> Notice that we can get quite large values for <i>r<sub>s</sub></i> .',
           '</ul>'
         )
         },
         
         "2"={output<-c(
           '<b>Uncertainty</b>',
           '<br>',
           'A sample is always different from the population. ',
           'But we can never know how it is different. That means that the sample is an uncertain guide to the population.',
           'We can, however, set a limit on how much the uncertainty is.',
           '<ul style=margin:0px;>',
           '<li> We start by just looking at the range of sample effect sizes.',
           '<li> Then we see how the sample variability makes inference difficult.',
           '<li> Finally, we see how these convert into inferential errors.',
           # '<li> Finally we consider how careful design can minimize these errors.',
           '</ul>'
         )},
         "2a"={output<-c(
           '<b>Demo 2a</b>',
           '<br>',
           'A demo to see how far getting a significant result depends on chance.',
           '<ul style=margin:0px;>',
           '<li> Make individual samples and see whether they are significant or not.',
           '<li> Use the "Multiple" function to make a large number of samples.',
           '<li> For any <i>n</i>, there is a fixed relationship between <i>r<sub>s</sub></i> and <i>p</i>.',
           '</ul>'
         )
         },
         
         "2b"={output<-c(
           '<b>Demo 2b</b>',
           '<br>',
           'A demo about how uncertainty makes inference difficult',
           '<ul style=margin:0px;>',
           '<li> Make a world where 50% of samples come from null effects.',
           '<li> Try and guess whether any sample has a real effect or not.',
           '<li> See how sample size changes this.',
           '</ul>'
         )
         },
         
         "2c"={output<-c(
           '<b>Demo 2c</b>',
           '<br>',
           'A demo about uncertainty and inferential errors',
           '<ul style=margin:0px;>',
           '<li> Use the "Multiple" function to make a large number of samples.',
           '<li> View this as the 4 possible NHST outcomes.',
           '<li> See how the proportion of inferential errors changes with population effect size.',
           '<li> Sampling type.',
           '</ul>'
         )
         },
         
         "3"={output<-c(
           '<b>Design to reduce errors</b>',
           '<br>',
           ' ',
           ' ',
           ' ',
           '<ul style=margin:0px;>',
           '<li> The effects of sample size.',
           '<li> Taking into account population effect size.',
           '<li> Using within/between designs.',
           '</ul>'
         )},
         "3a"={output<-c(
           '<b>Demo 3a</b>',
           '<br>',
           'Choosing sample size.',
           '<ul style=margin:0px;>',
           '<li> Use the "Multiple" function to make a large number of samples.',
           '<li> Repeat this with different sample sizes. Larger samples give more chance of a significant result.',
           '<li> Use the "Explore" function to see how p(sig) varies with sample size.',
           '</ul>'
         )
         },
         "3b"={output<-c(
           '<b>Demo 3b</b>',
           '<br>',
           'Population effect size matters.',
           '<ul style=margin:0px;>',
           '<li> Use the "Explore" function to see how p(sig) varies with effect size.',
           '<li> Repeat this with different sample sizes.',
           '<li> What are the consequences of poor sampling?',
           '</ul>'
         )
         },
         "3c"={output<-c(
           '<b>Demo 3c</b>',
           '<br>',
           'A demo about the sample usage - within samples have lower uncertainty.',
           '<ul style=margin:0px;>',
           '<li> Use the "Multiple" function to make a large number of samples.',
           '<li> Compare this with the result of "Within" sampling.',
           '<li> Use the "Explore" function to see how p(sig) varies with effect size for the two usages.',
           '</ul>'
         )
         },
         
         "4"={output<-c(
           '<b>Linear models with more than 1 IV</b>',
           '<br>',
           'Statistical analysis is simple provided there is just one effect (one IV and 1 DV). ',
           'But often it is much more interesting and useful to consider more than one IV.',
           'This introduces some extra considerations.',
           '<ul style=margin:0px;>',
           '<li> Linear models are those where the different effects simply add together.',
           '<li> Linear models can have interactions (aka moderations).',
           '<li> Covariation occurs when the IVs are themselves inter-related.',
           '</ul>'
         )},
         "4a"={output<-c(
           '<b>Demo 4a</b>',
           '<br>',
           'A demo about using multiple IVs. ',
           'In linear models, these are treated as separate, additive effects.',
           'We can see that these effects behave quite independently.',
           '<ul style=margin:0px;>',
           '<li> Change <i>r<sub>p</sub></i> for the main effect of IV on DV.',
           '<li> Change <i>r<sub>p</sub></i> for the main effect of IV2 on DV.',
           '<li> Change <i>r<sub>p</sub></i> for the interaction effect of IVxIV2 on DV.',
           '</ul>'
         )
         },
         
         "4b"={output<-c(
           '<b>Demo 4b</b>',
           '<br>',
           'A demo about interactions as moderators. ',
           'Technically an interaction is the product (multiplication) or two variables. ',
           'It is easier and better to think of it as a situation where one variable affects the strength of an effect between two other variables. ',
           'An interaction or moderation is a switch that can switch effects on or off. ',
           '<ul style=margin:0px;>',
           '<li> See how interactions can be seen as an on/off switch.',
           '<li> See how interactions can be seen as an +/- switch.',
           '</ul>'
         )
         },
         
         "4c"={output<-c(
           '<b>Demo 4c</b>',
           '<br>',
           'A demo about covariation between IV and IV2. ',
           'Covariation is when the two IVs are themselves related/correlated. ',
           'As we will see, this has consequences for the total effect of each and the unique effect of each. ',
           'The website has more examples. ',
           '<ul style=margin:0px;>',
           '<li> Make samples from a population with covariation.',
           '<li> See what the covariation looks like in in the scatter plots.',
           '<li> See what the covariation does to the effect sizes of the main effects.',
           '</ul>'
         )
         },
         "5"={output<-c(
           '<b>Path models</b>',
           '<br>',
           'Path models can have a sequence of effects linked into a chain - or path. ',
           ''
         )
         },
         "5a"={output<-c(
           '<b>Demo 5a</b>',
           '<br>',
           'A demo about mediation effects. ',
           'We say that the effect of IV on DV is fully mediated by IV2, when there is no direct link from IV to DV, but instead links from IV to IV2 and IV2 to DV.',
           '<ul style=margin:0px;>',
           '<li> Make a basic mediation model - with quite strong effect sizes for clarity.',
           '<li> We can see the pattern of total effects in the data.',
           '<li> We might also notice that the covariation and its meaning are rather hidden.',
           '<li> Now link IV to DV directly as well and see what happens.',
           '</ul>'
         )
         },
         
         "5b"={output<-c(
           '<b>Demo 5b</b>',
           '<br>',
           'A demo comparing models. ',
           'We say that the effect of IV on DV is fully mediated by IV2, when there is no direct link from IV to DV, but instead links from IV to IV2 and IV2 to DV.',
           '<ul style=margin:0px;>',
           '<li> Make a basic mediation model - with quite strong effect sizes for clarity.',
           '<li> We can see the pattern of total effects in the data.',
           '<li> We might also notice that the covariation and its meaning are rather hidden.',
           '<li> Now link IV to DV directly as well and see what happens.',
           '</ul>'
         )
         },
         
         "5c"={output<-c(
           '<b>Demo 5c</b>',
           '<br>',
           'A demo about causation. ',
           'We say that the effect of IV on DV is fully mediated by IV2, when there is no direct link from IV to DV, but instead links from IV to IV2 and IV2 to DV.',
           '<ul style=margin:0px;>',
           '<li> Make a basic mediation model - with quite strong effect sizes for clarity.',
           '<li> We can see the pattern of total effects in the data.',
           '<li> We might also notice that the covariation and its meaning are rather hidden.',
           '<li> Now link IV to DV directly as well and see what happens.',
           '</ul>'
         )
         }
         
  )
    extras<-paste0('<br>',
                   'More information ',
                    '<a href=',
                   '"https://doingpsychstats.wordpress.com/demonstration-',HelpType,'/"',
                   ' target="_blank">',
                   'here',
                   '</a>',
                   ' and leave any comments ',
                   '<a href=',
                   '"https://doingpsychstats.wordpress.com/demonstration-',HelpType,'/#respond"',
                   ' target="_blank">',
                   'here',
                   '</a>'
                   )
#    if (!is.element(HelpType,c("1","2","3","4")))
    output<-c(output,extras)
    output<-c("<div style='border: none; padding: 4px;'>",output,"</div>")
    
    wholePanel<-paste0(output,collapse=" ")
    return(wholePanel)
    
}