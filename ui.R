fluidPage(  #h1(img(src='logo2.jpg',width = 350, height = 180),align='center'),
  navbarPage(title='CLAW - Clustering of Associated Words', #theme='http://bootswatch.com/cosmo/bootstrap.css',
  
  tabPanel("Home",         
    sidebarLayout(
      sidebarPanel( 
        fileInput('file1','Upload Sprout Saved File',multiple=FALSE,accept='.spt2')
      ), 
      mainPanel(
        wellPanel(
          fluidRow(
            column(6,
               
               numericInput('eps', 'Select an eps value',value=1.2,,min=0,max=10,step=.1),
               actionLink('help1',label='What is an eps value?'),
               bsModal('ex','What is an eps value?','help1','Eps (epsilon) is a distance measure that specifies how close points should be to each other to be considered a cluster. That is, if eps is set to 5, points with a distance of 5 or less will belong to that particular cluster. In general, lower eps values will result in a higher number of smaller clusters, whereas higher eps values will result in a lower number of large clusters. Smaller eps values are typically better, as only a small fraction of points should be within a certain distance of each other. NOTE: High eps values tend to make words segregate into one massive, meaningless cluster (this massive cluster is not returned in CLAW). If CLAW returns very few clusters, try lowering the eps value.'),
               hr(),
               numericInput('minpts','Select the number of minimum points',value=3,,min=0,max=10,step=1),
               actionLink('help2',label='How many points should I select?'),
               bsModal('ex2','How many points should I select?','help2','This value determines the minimum number of points (words) that must be located within the eps distance to be considered a cluster. There is not necessarily an optimal minium number of points - this value depends on what the user is looking for. Lower numbers will result in more clusters that are smaller, and a higher number will yield fewer, larger clusters.'),
               hr(),
               numericInput('skipgram','Select the window size',value=5,,min=0,max=20,step=1),
               actionLink('help5',label='What is the window size?'),
               bsModal('ex5','What is the window size?','help5','For each term, we look for other context terms within a certain number of words before and after each term. This value determines what the window size is before and after any given term (i.e. a window size of 5 will compare each term with the five previous terms and the 5 following terms).')
               
        ),
        column(6, 
               
               numericInput('perplexity','Select a perplexity value',value=30,,min=0,max=100,step=1),
               actionLink('help3',label='What is perplexity?'),
               bsModal('ex3','What is perplexity?','help3','The perplexity value helps to determine the number of effective nearest neighbors in a cluster. Perplexity is generally robust under different settings, so in most cases the default value of 30 will work well. Smaller datasets will sometimes require smaller perplexity values, and larger datasets can require larger perplexity values.'),
               hr(),
               numericInput('min_term_input','Select the minimum word count',value=10,min=0,max=100,step=1),
               actionLink('help4',label='What is the minimum word count?'),
               bsModal('ex4','What is the minimum word count?','help4','The minimum word count is the minimum number of words that must be present in the uploaded sprout file for them to be considered in the analysis. This is just a further step of text pre-processing, as it helps filter out common words.'),
               hr()
        )
      ),
      
      br(),
      
      actionButton(inputId = 'go',label='Update')),
      
      DT::dataTableOutput('concepts'),
      br()
      ,DT::dataTableOutput('search')
      
    
      )
    )
  ),
    tabPanel("t-SNE Plot",  
           imageOutput('image')
    ),
  
  ##################
  # Loading spinner for whenever app is busy
  ##################
  tags$head(tags$script(src="spin.min.js"),
            tags$style(type="text/css", "
                       #customloading {
                       position: fixed;
                       top: 0;
                       left: 0;
                       width: 100%;
                       height: 100%;
                       vertical-align: middle;
                       text-align: center;
                       font-weight: bold;
                       font-size: 100%;
                       color: #000000;
                       background-color: rgba(0, 0, 0, 0.1);
                       z-index: 105;
                       }
                       
                       #customloadingtext {
                       position: fixed;
                       width: 100%;
                       height: 100%;
                       top: 50%;
                       z-index:104;
                       }")
                            ),
  conditionalPanel(condition="$('html').hasClass('shiny-busy')",
                   fluidRow(
                     tags$div(tags$div("",id="customloadingtext"),id="customloading"),
                     HTML("
                                           <script type='text/javascript'>
                                           var opts = {
                                           lines: 14 // The number of lines to draw
                                           , length: 28 // The length of each line
                                           , width: 14 // The line thickness
                                           , radius: 42 // The radius of the inner circle
                                           , scale: 1 // Scales overall size of the spinner
                                           , corners: 1 // Corner roundness (0..1)
                                           , color: '#000' // #rgb or #rrggbb or array of colors
                                           , opacity: 0.25 // Opacity of the lines
                                           , rotate: 0 // The rotation offset
                                           , direction: 1 // 1: clockwise, -1: counterclockwise
                                           , speed: 1 // Rounds per second
                                           , trail: 60 // Afterglow percentage
                                           , fps: 20 // Frames per second when using setTimeout() as a fallback for CSS
                                           , zIndex: 2e9 // The z-index (defaults to 2000000000)
                                           , className: 'spinner' // The CSS class to assign to the spinner
                                           , top: '50%' // Top position relative to parent
                                           , left: '50%' // Left position relative to parent
                                           , shadow: true // Whether to render a shadow
                                           , hwaccel: false // Whether to use hardware acceleration
                                           , position: 'absolute' // Element positioning
                                           }
                                           var target = document.getElementById('customloading')
                                           var spinner = new Spinner(opts).spin(target);
                                           </script>
                        ")
                   )
  )
  )
)#, h1(img(src='logo2.jpg',width = 340, height = 200),align='left'))