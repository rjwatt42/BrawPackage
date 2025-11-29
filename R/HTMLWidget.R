# HTMLWidget <- R6::R6Class("HTMLWidget",
#                              public = list(
#                                  initialize = function() {
#                                  },
# 

#' @export
generate_tab = function(title="Tab",tabs=c("1","2","3"),tabContents=c("a","b","c"),
                        titleTab="",titleWidth=135,
                        history=NULL,
                        tabLink=NULL,tabLinkLabel='here',tabLinkStyle='button',
                        plainTabs=FALSE,indent=0,topMargin=5,
                        colours=c("#3498db","#888","#888"),fontSize="12px",
                        plain=FALSE,width=600,height=NULL,outerHeight=NULL,open=0) {
  if (is.null(height)) ht<-'' else ht<-paste0('height: ',height,'px;')
  if (is.null(open)) open<-0
  if (open==0) openCode<-''
  else openCode<-paste0(
    'document.getElementById("',title,'||',tabs[open],'").style.display = "block";'
  )
  
  if (!plainTabs) 
    tabBorders<-''
  else tabBorders<-paste0('border: none;')
  
  openTab<-function(tabName) {
    paste0(
    '  var tabName;',
    '  tabName = \'',tabName,'\';',
    '  var tabState;',
    '  var i, tabID, panelID, buttonID, tabcontent, tablinks;',
    '  if (tabName!=\'tabtitle\') {',
    '    tabState = document.getElementById(tabName).style.display;',
    # '    closeTabs(evt, tabName);',
    '    tabID = tabName.split(\'||\',1);',
    '    tablinks = document.getElementsByClassName(\'tablinks\');',
    '    tabcontent = document.getElementsByClassName(\'tabcontent\');',
    '    for (i = 0; i < tabcontent.length; i++) {',
    '      panelID=tabcontent[i].id.split(\'||\',1);',
    '      if (panelID[0]==tabID[0]) {',
    '        tabcontent[i].style.display = \'none\';',
    '      }',
    '    }',
    '    for (i = 0; i < tabcontent.length; i++) {',
    '      buttonID=tablinks[i].id.split(\'||\',1);',
    '      if (buttonID[0]==tabID[0]) {',
    '        tablinks[i].className = tablinks[i].className.replace(\' active\', \'\');',
    '      }',
    '    }',
    '    if (tabState!=\'block\') {',
    '      document.getElementById(tabName).style.display = \'block\';',
    '      event.currentTarget.className += \' active\';',
    '    }',
    '  }'
  )
  }
  closeTabs<-function(tabName) {
    paste0(
    '  var tabName;',
    '  tabName = \'',tabName,'\';',
    '  var i, tabID, panelID, buttonID, tabcontent, tablinks;',
    '    tabID = tabName.split(\'||\',1);',
    '    tablinks = document.getElementsByClassName(\'tablinks\');',
    '    tabcontent = document.getElementsByClassName(\'tabcontent\');',
    '    for (i = 0; i < tabcontent.length; i++) {',
    '      panelID=tabcontent[i].id.split(\'||\',1);',
    '      if (panelID[0]==tabID[0]) {',
    '        tabcontent[i].style.display = \'none\';',
    '      }',
    '    }',
    '    for (i = 0; i < tabcontent.length; i++) {',
    '      buttonID=tablinks[i].id.split(\'||\',1);',
    '      if (buttonID[0]==tabID[0]) {',
    '        tablinks[i].className = tablinks[i].className.replace(\' active\', \'\');',
    '      }',
    '    }'
    )
  }
  historyGoTo<-function(blockName) {
    paste0(
  '  var blockName;',
  '  blockName = \'',blockName,'\';', 
  '  var found, historyBlocks;',
  '    historyBlocks = document.getElementsByClassName(\'history\');',
  '    found = false;',
  '    for (i = 0; i < historyBlocks.length; i++) {',
  '        if (historyBlocks[i].id==blockName) { found = true;}',
  '    }',
  '    if (found) {',
  '    for (i = 0; i < historyBlocks.length; i++) {',
  '        if (historyBlocks[i].id==blockName) {',
  '        historyBlocks[i].style.display = \'block\';',
  '       } else {',
  '        historyBlocks[i].style.display = \'none\';',
  '       }',
  '    }',
  '  }'
    )
  }
  
  script<-paste0(
  # '<script>',
    # 'function openTab(evt, tabName) {',
    # '  var tabState;',
    # '  if (tabName!="tabtitle") {',
    # '    tabState = document.getElementById(tabName).style.display;',
    # '    closeTabs(evt, tabName);',
    # '    if (tabState!="block") {',
    # '      document.getElementById(tabName).style.display = "block";',
    # '      evt.currentTarget.className += " active";',
    # '    }',
    # '  }',
    # '}',
    # 'function closeTabs(evt, tabName) {',
    # '  var i, tabID, panelID, buttonID, tabcontent, tablinks;',
    # '    tabID = tabName.split("||",1);',
    # '    tablinks = document.getElementsByClassName("tablinks");',
    # '    tabcontent = document.getElementsByClassName("tabcontent");',
    # '    for (i = 0; i < tabcontent.length; i++) {',
    # '      panelID=tabcontent[i].id.split("||",1);',
    # '      if (panelID[0]==tabID[0]) {',
    # '        tabcontent[i].style.display = "none";',
    # '      }',
    # '    }',
    # '    for (i = 0; i < tabcontent.length; i++) {',
    # '      buttonID=tablinks[i].id.split("||",1);',
    # '      if (buttonID[0]==tabID[0]) {',
    # '        tablinks[i].className = tablinks[i].className.replace(" active", "");',
    # '      }',
    # '    }',
    # '}',
    # 'function historyGoTo(evt, blockName) {',
    # '  var found, historyBlocks;',
    # '    historyBlocks = document.getElementsByClassName("history");',
    # '    found = false;',
    # '    for (i = 0; i < historyBlocks.length; i++) {',
    # '        if (historyBlocks[i].id==blockName) { found = true;}',
    # '    }',
    # '    if (found) {',
    # '    for (i = 0; i < historyBlocks.length; i++) {',
    # '        if (historyBlocks[i].id==blockName) {',
    # '        historyBlocks[i].style.display = "block";',
    # '       } else {',
    # '        historyBlocks[i].style.display = "none";',
    # '       }',
    # '    }',
    # '  }',
    # '}',
    # 'function linkGoTo(evt, linkName) {',
    # '    open(linkName);',
    # '}',
    # openCode,
  # '</script>'
  )
  
  style<-paste0(
    '<style>',
    # Style the tab 
    '.tab {overflow: hidden;',
    'margin: 0px 0px 0px 0px;',
    'padding: 5px 0px 0px 0px;',
    'border: none;',
    'background-color: inherit;',
    'border-bottom: solid ',colours[3],' 1px;',
    'width: ',width+2,'px;',
    'margin: none;',
    '}',
    
    # Style the buttons inside the tab 
    '.tab button {',
    'background-color: #ccc;',
    'float: left;',
    'border: none;',
    'outline: none;',
    'cursor: pointer;',
    'padding: 2px 4px;',
    'margin: 0px 1px;',
    'transition: 0.3s;',
    'font-size: ',fontSize,';',
    'font-weight: normal;',
    'border-top-left-radius: 4px;',
    'border-top-right-radius: 4px;',
    '}',
    
    # Change background color of buttons on hover 
    '.tab button:hover {',
    'background-color: #ddd;',
    '}',
    
    # Create an active/current tablink class 
    '.tab button.active {',
    'background-color: ',colours[2],';',
    'color: white;',
    '}',
    
    # Style the null tab content 
    '.tabnullcontent {',
    'display: none;',
    'padding: 0px 0px;',
    'margin: 0px;',
    'border: 1px solid ',colours[3],';','border-top: none;',
    'width: ',width,'px;',
    'height: 0px;',
    '}',
    
    # Style the tab content 
    '.tabcontent {',
    'display: none;',
    'padding: 0px 0px;',
    'margin: 0px;',
    'background-color: ','#ffffff',';',
    'border: 1px solid ',colours[3],';','border-top: none;',
    'width: ',width,'px;',
    ht,
    '}',
    '</style>'
  )
  
  buttons<-''
  panels<-''
  block<-0
  if (!is.null(history)) 
    block<-sum(gregexpr('ID="Block[0-9]{1,2}"',history)[[1]]>0)
    
  if (titleWidth==0) titleShow<-''
  else titleShow<-paste0('<b>',title,'</b>')
  
  titleID<-paste0(title,block)
  buttons <- paste0(buttons,
                    '  <button id="tabtitle',titleID,'" class="tablinks" ',
                    '  onclick="',closeTabs(titleID),'"',
                    ' style="background-color:rgba(0,0,0,0);color:black;cursor:default;',
                    'font-weight: 500;font-size:13px;width:',titleWidth,'px;text-align: right;',
                    'margin:0px;padding: 0px;">',
                    titleShow,
                    '</button>')
  if (nchar(titleTab)>0)
    panels <- paste0(panels,
                     '  <div id="',titleID,'" class="tabcontent" style="display:block">',
                     titleTab,
                     '</div>')
  else
    panels <- paste0(panels,
                     '  <div id="',titleID,'" class="tabcontent" style="display:none;"','width:',width-indent,'px;','>',
                     '</div>')
  for (itab in 1:length(tabs)) {
    panelID<-paste0(titleID,'||',tabs[itab])
    if (itab==open) {
      buttons <- paste0(buttons,
                        # '  <button class="tablinks active" onclick="openTab(event,\'',panelID,'\')"',
                        '  <button class="tablinks active" onclick="',openTab(panelID),'"',
                        ' id="',paste0(panelID,"button"),
                        'margin:0px;padding:0px;">',
                        tabs[itab],
                        '</button>')
      panels <- paste0(panels,
                       '  <div id="',panelID,'" class="tabcontent" style="',tabBorders,';',
                       'width:',width-indent,'px;',';height:100%;','margin-left:',indent,'px;','display:block;">',
                       tabContents[itab],
                       '</div>')
    } else {
      buttons <- paste0(buttons,
                        # '  <button class="tablinks" onclick="openTab(event,\'',panelID,'\')" id="',paste0(panelID,"button"),'">',
                        '  <button class="tablinks" onclick="',openTab(panelID),'"',
                        ' id="',paste0(panelID,"button"),'">',
                        tabs[itab],
                        '</button>')
      panels <- paste0(panels,
                       '  <div id="',panelID,'" class="tabcontent" style="',tabBorders,';',
                       'width:',width-indent,'px;',';margin-left:',indent,'px;','">',
                       tabContents[itab],
                       '</div>')
    }
  }
  
  buttonFormat<-' style="float:right;border-radius:4px;padding:0px;padding-left:4px;padding-right:4px;margin:0px;margin-right:2px;margin-left:2px;"'
  if (!is.null(tabLink)) {
    link<-paste0(
      '<div style="float:right;padding-left:4px;padding-right:4px;font-size:12px;">',
      '<a href=','"',tabLink,'" target="_blank">',
      tabLinkLabel,
      '</a>',
      '</div>'
    )
  # link<-paste0(
  #   '<button class="linkButton"',
  #   sub(';"',';color:#0000FF;"',buttonFormat),
  #   '" onclick="linkGoTo(event,\'',tabLink,'\')">',
  #   tabLinkLabel,'</button>'
  # )
  } else link<-''
  
  if (!is.null(history)) {
    historyButtons<-''
    nextButton<-paste0(
      '<button class="historyButton"',
      buttonFormat,
      '" onclick="',historyGoTo(paste0('Block',format(block))),'">',
      # '" onclick="historyGoTo(event,\'','Block',format(block),'\')">',
      '>>','</button>'
    )
    nextButtonDisabled<-paste0(
      '<button class="historyButton" disabled',
      buttonFormat,
      '" onclick="',historyGoTo(paste0('Block',format(block))),'">',
      # '" onclick="historyGoTo(event,\'','Block',format(block),'\')">',
      '>>','</button>'
    )
    previousButton<-paste0(
      '<button class="historyButton" disabled'
    )
    if (block>0) {
      # back
      if (nchar(history)>0) plain<-TRUE
      historyButtons<-paste0(historyButtons,
                             '<button class="historyButton" disabled',
                             buttonFormat,
                             '" onclick="',historyGoTo(paste0('Block',format(block+1))),'">',
                             # '" onclick="historyGoTo(event,\'','Block',format(block+1),'\')">',
                             '>>','</button>'
      )
      historyButtons<-paste0(historyButtons,link)
      historyButtons<-paste0(historyButtons,
                             '<button class="historyButton"',
                             buttonFormat,
                             '" onclick="',historyGoTo(paste0('Block',format(block-1))),'">',
                             # '" onclick="historyGoTo(event,\'','Block',format(block-1),'\')">',
                             '<<','</button>'
      )
      history<-sub('<button class="historyButton" disabled',
                   '<button class="historyButton" ',
                   history)
    } else {
      historyButtons<-paste0(historyButtons,
                             '<button class="historyButton" disabled',
                             buttonFormat,
                             '" onclick="',historyGoTo(paste0('Block',format(1))),'">',
                             # '" onclick="historyGoTo(event,\'','Block',format(1),'\')">',
                             '>>','</button>'
      )
      historyButtons<-paste0(historyButtons,link)
      historyButtons<-paste0(historyButtons,
                             '<button class="historyButton" disabled',
                             buttonFormat,
                             '" onclick="',historyGoTo(paste0('Block',format(0))),'">',
                             # '" onclick="historyGoTo(event,\'','Block',format(0),'\')">',
                             '<<','</button>'
      )
    }
  } else historyButtons<-''
  buttons<-paste0('<div class="tab" style="','width:',width-indent+2,'px;',
                  'padding:0px;margin:0px;','margin-left:',indent,'px;','padding-top:',topMargin,'px;',
                  # 'border:solid;',
                  '">',buttons,
                  historyButtons,
                  '</div>')
  if (plain)
    html_content <- paste0(
      buttons,
      panels
    )
  else
    html_content <- paste0(
      style,
      buttons,
      panels,
      script
    )
  if (!is.null(outerHeight)) {
    html_content<-paste0(
      '<div style="height:',outerHeight,'px;">',
      html_content,
      '</div>'
    )
  }
  if (!is.null(history)) {
    history<-gsub('class="history"','class="history" style="display:none"',history)
    html_content<-paste0(
      '<div class="history" ID="Block',format(block),'" >',
      html_content,
      '</div>',
      history
    )
  }
  return(html_content)
}

showMore<-function(extraID, buttonID) {
  paste0(
    '  var extraID;',
    '  extraID = \'',extraID,'\';',
    '  var buttonID;',
    '  buttonID = \'',buttonID,'\';',
    '  var tabState;',
    '    tabState = document.getElementById(extraID).style.display;',
    '    if (tabState!=\'block\') {',
    '      document.getElementById(extraID).style.display = \'block\';',
    '      document.getElementById(buttonID).textContent = \'-\';',
    '    }',
    '    else {',
    '      document.getElementById(extraID).style.display = \'none\';',
    '      document.getElementById(buttonID).textContent = \'+\';',
    '    }'
  )
}

#' @export
moreHTML<-function(content,title="more",ID="p") {
  control<-paste0(
    # '<script>',
    # 'function showMore(evt, extraID, buttonID) {',
    # '  var tabState;',
    # '    tabState = document.getElementById(extraID).style.display;',
    # '    if (tabState!="block") {',
    # '      document.getElementById(extraID).style.display = "block";',
    # '      document.getElementById(buttonID).textContent = "-";',
    # '    }',
    # '    else {',
    # '      document.getElementById(extraID).style.display = "none";',
    # '      document.getElementById(buttonID).textContent = "+";',
    # '    }',
    # '}',
    # '</script>',
    '<style> button.more {font-size:12px;margin:0px;border:none;cursor:pointer;',
    'border-top-left-radius: 4px;',
    'border-top-right-radius: 4px;',
    'border-bottom-left-radius: 4px;',
    'border-bottom-right-radius: 4px;',
    'background-color:#3498db;color:white;} </style>'
  )
  
  IDContent<-paste0(ID,'content')
  extra<-paste0(
    control,
    '<button class="more" ID="',ID,'" onclick="',showMore(IDContent,ID),'">',
    '+','</button>',
    ' >',title,
    '<div ID="',IDContent,'" style="display:none;">', 
    content,
    '</div>'
  )
}

