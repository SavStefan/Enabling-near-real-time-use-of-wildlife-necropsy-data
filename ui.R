#ui.R

navbarPage("D.E.E", #maybe incorporate some Te reo??
           
           navbarMenu('Describe', 
                      tabPanel('Signalment Description',
                               titlePanel(strong('Signalment Description')),
                               sidebarLayout(
                                 sidebarPanel(fileInput("file","Upload a .csv file from the Massey Pathology Register"),
                                             # checkboxInput("header","header",TRUE),
                                              uiOutput('ageselect'),
                                              uiOutput('genderselect'),
                                              uiOutput('speciesselect'),
                                              uiOutput('daterange'),
                                              p('Drop down menu options are created from all levels found in the respective variable within The Register'),
                                              p('Filters selected here apply across all tabs in the app')
                                 ),         
                                 mainPanel(
                                   fluidRow(
                                     column(6,
                                            plotOutput('speciesplot'),
                                            tableOutput('speciestable')),
                                     column(6,
                                            plotOutput('ageplot'),
                                            tableOutput('agetable')),
                                     column(6,
                                            plotOutput('sexplot'),
                                            tableOutput('sextable')),
                                     
                                     column(6,
                                            plotOutput('timeplot'),
                                            tableOutput('timetable'),
                                            radioButtons('time','Pick Time',
                                                         choices = c('Month Submitted' = 'Month_Submitted',
                                                                     'Year Submitted' = 'Year_Submitted')
                                                         
                                            ))  
                                   )    
                                   
                                 ) #Make the time plot, number of sumbissions per month for the last 12 months?
                                 #Otherwise just make the date filter apply to all the others?
                               )
                      ),
                      tabPanel('Proportion by Frequency Comparison',
                               titlePanel(strong('Proportion by Frequency comparison')),
                               sidebarLayout(
                                 sidebarPanel( uiOutput('frequency'),
                                               uiOutput('proportional'),
                                               downloadButton("customdata","Download Table"),
                                               br(),
                                               br(),
                                               p('Change the proportion and the frequency variables by that available in the drop down lists to explore variable pair relationships'),
                                               p('This works best with discrete categorical variables such as those pertaining to signalment'),
                                               p('The resultant table can be downloaded as a .csv for further analysis')
                                 ),
                                 mainPanel(    plotOutput('customplot'),
                                               tableOutput('customtable')
                                               
                                 )
                               )
                      )
           ),
           navbarMenu('Explore',
                      tabPanel('Word Correlations',
                               titlePanel(strong('Explore word correlations')),
                               sidebarLayout(
                                 sidebarPanel(sliderInput(
                                   "corr", label = "Set minimum word correlation value",
                                   min =0.5, value = 0.7, max = 1),
                                   br(),
                                   p('Connected words are likely to occur within the same report, but not necessarily next to each other'),
                                   p('Bear in mind that the visualisation does not represent the frequency of that correlation'),
                                   p('This process is a bit more computationally intensive so the visualisation may take a few moments to render or,
                                     may crash the application in large datasets particularly with low correlation cut offs'),
                                   p('If the visualisation appears strange, try increasing or decreasing the minimum correlation value')
                                 ),
                                 mainPanel(   
                                   plotOutput('ggraph', height = '800px')
                                 )
                               )
                      ),
                      #need to make the wordcloud scale dynamically but otherwise this is good! Will likely require CSS 
                      tabPanel ('Topic Modelling',
                                titlePanel(strong('Topic Modelling')),
                                sidebarLayout(
                                  sidebarPanel(
                                    sliderInput(
                                      "k", label = "Number of topics",
                                      min =2, value = 2, max = 8),
                                    sliderInput(
                                      'top_n', label = 'Words per topic',
                                      min = 10, value = 30, max= 100),
                                    br(),
                                    p('All uploaded documents are clustered into groups with common topics based on word use via Latent Dirichlet allocation (LDA)'),
                                    p('Larger words in the cloud are more representative of the allocated topic'),
                                    p('Too few topics leads to a loss of separation, too many leads to more arbitrary clustering'),
                                    p('Explore concepts important in the uploaded records by seeing what happens when the number of topics is changed'),
                                    p('Using large numbers of words and topics requires more computational power so may take a few moments to render')
                                  ),
                                  mainPanel(plotOutput('wordcloud', width = '900px', height = '900px')       
                                  )
                                )  
                      )
           ),
           
           navbarMenu('Examine',
                      tabPanel('Text Mining',
                               titlePanel(strong('Text Mining')),       
                               sidebarLayout(
                                 sidebarPanel( 
                                   sliderInput(
                                     "slice_max", label = "Set limit to number of important words extracted per record",
                                     min = 1, value = 40, max = 40),
                                   p("Use lower numbers for larger datasets, and higher numbers for smaller datasets or when greater sensitivity is required"),
                                   downloadButton("dldata","Download selected records"), actionButton('resetSelection',label = "Click to reset row selection"),
                                   br(),
                                   br(),
                                   p('This page uses a statistical model to predict which words in each record are most important for 
                                     imparting unique meaning to that record, i.e. differentiating it from other records'),
                                   p('The result of this model is presented in the first table seen to the right of screen'),
                                   p('The number on the right hand column displays the number of records for which that word is considered important'), 
                                   p('As you may see, in the case of necrpsy records many of these important words are clinicopathologic descriptors or diagnoses'),
                                   p('Specific words can be searched for, if a topic or word seems interesting try searching for synonyms of that word. Set the slider higher to increase sensitivity'),
                                   p('Records identified in this process can then be downloaded as a group in .csv format for closer analysis'),
                                      ),
                                 mainPanel(   
                                   fluidRow(
                                     column(6,
                                            h3('Table of Important Words '),
                                            dataTableOutput("table")),
                                     column(6,    
                                            h3('Cases of Selected Clinicopathologic Descriptors'),
                                            dataTableOutput("seltable"))
                                   ),
                                  fluidRow(   
                                     h4("Total number of cases"),
                                     verbatimTextOutput('num')
                                   ),
                                   fluidRow(column(6, 
                                                   plotOutput('plot')))
                                 )
                               )
                      )
           )
)

