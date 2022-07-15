ui <- dashboardPage( title="TonFIST Dashboard", 
  dashboardHeader(
    title = "TonFIST Dashboard",
    titleWidth = 230
  ), #end of dashboard header
  
  dashboardSidebar(
    sidebarMenu(
      id = "sidebar",

      style = "position: fixed;overflow: visible; color: #FFF; width: 220px; white-space: nowrap;",

      menuItem(
        "About TonFIST",
        tabName = "about",
        icon = icon("info")
      ),

      menuItem(
        "The Dashboard",
        tabName = "dash",
        icon = icon("dashboard")
      ),
      
      conditionalPanel("input.sidebar == 'dash' && input.countries == 'uganda'",
        h4("Scenarios"),
        selectInput("ug_scenarios",
                    "Select a scenario",
                    unique(Uganda$Scenarios),
                    selected = "Scenario 2")
      ), # end of conditional panel
      
      
      conditionalPanel("input.sidebar == 'dash' && input.countries == 'uganda'",
                       h4("Targets"),
                       selectInput("ug_targets",
                                   "Select a target",
                                   unique(Uganda$Targets),
                                   selected = "Target 1")
      ), # end of conditional panel
      
      conditionalPanel("input.sidebar == 'dash' && input.countries == 'rwanda'",
                       h4("Scenarios"),
                       selectInput("rw_scenarios",
                                   "Select a scenario",
                                   unique(Rwanda$Scenarios),
                                   selected = "Scenario 2")
      ), # end of conditional panel
      
      
      conditionalPanel("input.sidebar == 'dash' && input.countries == 'rwanda'",
                       h4("Targets"),
                       selectInput("rw_targets",
                                   "Select a target",
                                   unique(Rwanda$Targets),
                                   selected = "Target B1")
      ), # end of conditional panel
      
      div(class = "sticky_footer", 
          sidebarUserPanel("Developers:",
          subtitle = "Victor Mutugi, Dr. Brian Chiputwa"
      ))

    ) #end of sidebar menu
    
  ), #end of dashboard sidebar
  
  dashboardBody(
    tags$script(
      HTML("$('body').addClass('fixed');")
    ), #end of tags script
    
    
    
    fluidRow(
      tags$style(".nav-tabs {
                background-color: #17202A;
              }
              
              .nav-tabs-custom 
              .nav-tabs li.active:hover a, 
              .nav-tabs-custom 
              .nav-tabs li.active a {
              background-color: #327A00;
              border-top-color: #17202A;
              border: 5px solid;    
              border-right-color: #FF3E00;
              color: white;
              }
              
              .nav-tabs-custom .nav-tabs li.active {
                  border-top-color: #FF3E00;
              }
              
              .nav-tabs-custom .nav-tabs li a{
                  color: #FFFFFF;
              }")
    ), #end of fluidrow
    
    
    
    tags$head(
      # tags$style(
      #   HTML('
      #     .main-header.logo {
      #      font-family: "Georgia", Times, "Times New Roman", serif;
      #      font-weight: bold;
      #     font-size: 20px;
      #     }
      #     /* logo */
      #   .skin-blue .main-header .logo {
      #                         background-color: #2D2E30;
      #                         color:#FFFFFF;
      #                         }
      # 
      #   /* logo when hovered */
      #   .skin-blue .main-header .logo:hover {
      #                         background-color: #151313;
      #                         color:#FFFFFF;
      #                         }
      # 
      #   /* navbar (rest of the header) */
      #   .skin-blue .main-header .navbar {
      #                         background-color: #14CA02;
      #                         color:#FFFFFF;
      #                         }        
      # 
      #   /* main sidebar */
      #   .skin-blue .main-sidebar {
      #                         background-color: #151313;
      #                         color:#FFFFFF;
      #   }
      #   
      #   .skin-blue .main-header .logo:hover {
      #     background-color: #14CA02;
      #   }
      #   
      #   * { font-family: "Arial"; }
      #   
      #   ')
      # ),
      tags$link(
        rel = "stylesheet", 
        type = "text/css",
        href = "style.css"
      )
    ), # end of tags head
    
    
    fluidRow(
      tabItems(
        #The About page tabs
        tabItem(tabName = "about",
                tabBox(id = "about", width = 12,
                       fluidRow(style = "background-image: url(forest.jpg); background-size: cover;",
                         column(width = 3, style = "background-image: url(forest.jpg); background-size: cover;", align = "left", 
                                div(
                                  img(
                                    src = "tonf-logo.png",
                                    height = 100,
                                    width = 350,
                                    style = "margin:10px 10px"
                                  )
                                  # h1(tags$p("Trees on Farms Investment Scenario Tool"))
                                )
                         ),
                         
                         tags$br(),
                         tags$br(),
                         
                         column(width = 9, align = "right", style = 'border-bottom: 5px solid; color: white;',
                                h1(HTML("<strong>Trees on Farms Investment Scenarios Tool (TonFIST)</strong>"))
                         )
                       ),
                       
                  tabPanel("About TonFIST",
                           icon=icon("address-card"),
                           
                           
                           fluidRow(
                             box(width = 12,
                                 column(width = 12, align = "center",  
                                        h4("The major objective of the digital Trees on Farm (TonF) 
                                           Investment Scenarios Tool is to be able to account for the investment 
                                           expenses related to adopting TonF choices at different levels. 
                                           The tool takes into account two different types of investment requirements: 
                                           (1.) costs borne by the farmer associated with integrating TonF options, 
                                           such as the purchase of inputs, including tree seedlings, manure, 
                                           and fertilizers; and 
                                           (2.) institutional costs required for setting up an enabling environment, 
                                           such as the provision of infrastructure, including tree nurseries and 
                                           processing factories for tree commodities."
                                           )
                                        )
                             )
                           ),
                           
                           fluidRow(
                             box(width = 12,
                               column(width = 6, style = "height:400px; background-image: url(land.jpg); background-size: cover;",
                               
                             ), 
                             
                             column(width = 6,
                                    align = "center", style = "height:400px;background-color: #193B02; color: white;",
                                    
                                    tags$br(),
                                    
                                    tags$br(),
                                    
                                    h4("Biodiversity exists naturally on Earth. Life as we know it is supported by this biodiversity, 
                                        which includes the trees, birds, bees, mammals, snakes, and more. However, biodiverse landscapes 
                                        are disappearing at an alarming rate, frequently due to the spread of agriculture. The pressing 
                                        need to feed the world's expanding population and the imperative to preserve biodiversity seem at 
                                        odds with one another. We will continue to lose biodiversity and the essential benefits that healthy 
                                        ecosystems offer if we do not improve our resource management. Growing food will become increasingly 
                                        challenging for us."
                                       ),
                                       
                                       tags$br(),
                                       
                                       h3("Thankfully, farms may be run to maximize the importance of biodiversity protection. Our solution is 
                                       Trees on Farms for Biodiversity (TonF)."
                                          )
                             )
                             )
                             
                           ),
                           
                           
                           fluidRow(
                             box(width = 12,
                               column(width = 6,
                                    align = "center", style = "height:250px;background-color: #FDFCFC; color: black;",
                                    
                                    tags$br(),
                                    
                                    tags$br(),
                                    
                                    h4("With activity in five nations, the World Agroforestry (ICRAF) carries out the project's global 
                                       implementation; Uganda, Rwanda, and Peru also have direct management responsibilities. Tanjungpura 
                                       University, Pontianak, and the Centre for International Forestry Research (CIFOR) are the leaders in 
                                       Indonesia. Leading in Honduras is the Centro Agronómico Tropical de Investigación y Enseza (CATIE). 
                                       Knowledge management and outreach expertise are provided by the International Union for Conservation of Nature."
                                    )
                             ),
                             
                             column(width = 6, style = "height:250px;background-color: #FDFCFC;",
                                    tags$img(src="partners.png", width = "100%")
                             )
                             )
                           ),
                           
                           
                           tags$br(),
                           
                           tags$br(),
                  )
                )
          
        ),
        
        # The Dashboard page tabs
        tabItem(tabName = "dash",
                tabBox(id = "countries", width = 12,
                  tabPanel("Uganda Dashboard",
                           value = "uganda",
                     tags$p("This is the Uganda dashboard"),      
                    tabsetPanel(
                      id = "uganda.tabs",
                      
                      tabPanel("Institutional level",
                         withSpinner(plotlyOutput("ug_inst", height = "650px")),
                         
                         tags$br(),
                         tags$br()
                      ),
                      
                      tabPanel("Individual farmer level",
                               
                        fluidRow(
                          column(width = 6,
                            withSpinner(plotlyOutput("ug_indv", height = "650px"))
                          ),
                          
                          column(width = 6,
                            withSpinner(plotlyOutput("ug_incremntal", height = "650px"))
                          )
                          
                        ), #end of fluidrow
                        
                        tags$br(),
                        tags$br(),
                        
                        fluidRow(
                          column(width = 6,
                                 withSpinner(plotlyOutput("ug_percent_increase", height = "650px"))
                          ),
                          
                          column(width = 6,
                                 withSpinner(plotlyOutput("ug_gross", height = "650px"))
                          )
                          
                        ), #end of fluidrow
                        
                        tags$br(),
                        tags$br(),
                        
                        fluidRow(
                          column(width = 6,
                                 withSpinner(plotlyOutput("ug_net", height = "650px"))
                          ),
                          
                          column(width = 6,
                                 withSpinner(plotlyOutput("ug_bcr", height = "650px"))
                          )
                          
                        ),#end of fluidrow
                        
                        tags$br(),
                        tags$br()
                        
                      ),
                      
                    )
                    
                  ),
                  
                  tabPanel("Rwanda Dashboard",
                           value = "rwanda",
                           tags$p("This is the Rwanda dashboard"),      
                           tabsetPanel(
                             id = "rwanda.tabs",
                             
                             tabPanel("Institutional level",
                                      withSpinner(plotlyOutput("rw_inst", height = "650px")),
                                      
                                      tags$br(),
                                      tags$br()      
                             ),
                             
                             tabPanel("Individual farmer level",
                                      fluidRow(
                                        column(width = 6,
                                               withSpinner(plotlyOutput("rw_indv", height = "650px"))
                                        ),
                                        
                                        column(width = 6,
                                               withSpinner(plotlyOutput("rw_incremntal", height = "650px"))
                                        )
                                        
                                      ), #end of fluidrow
                                      
                                      
                                      tags$br(),
                                      tags$br(),
                                      
                                      fluidRow(
                                        column(width = 6,
                                               withSpinner(plotlyOutput("rw_percent_increase", height = "650px"))
                                        ),
                                        
                                        column(width = 6,
                                               withSpinner(plotlyOutput("rw_gross", height = "650px"))
                                        )
                                        
                                      ), #end of fluidrow
                                      
                                      tags$br(),
                                      tags$br(),
                                      
                                      fluidRow(
                                        column(width = 6,
                                               withSpinner(plotlyOutput("rw_net", height = "650px"))
                                        ),
                                        
                                        column(width = 6,
                                               withSpinner(plotlyOutput("rw_bcr", height = "650px"))
                                        )
                                        
                                      ),#end of fluidrow     \
                                      
                                      
                                      tags$br(),
                                      tags$br()
                                      
                             ),
                             
                           )
                           
                  ),
                  
                  tabPanel("Peru Dashboard",
                           value = "peru",
                           tags$p("The Peru dashboard is under construction and will be made available when data is loaded"),
                           
                           fluidRow(
                             column(width = 12,
                                    tags$img(src="coming-soon.png", width = "100%" , 
                                             height = 750)
                             )
                           )
                           # tabsetPanel(
                           #   id = "peru.tabs",
                           #   
                           #   tabPanel("Institutional level"
                           #            
                           #   ),
                           #   
                           #   tabPanel("Individual farmer level"
                           #            
                           #   ),
                           #   
                           # )
                           
                  ),
                  
                  tabPanel("Indonesia Dashboard",
                           value = "indonesia",
                           tags$p("The Indonesia dashboard is under construction and will be made available when data is loaded"),
                           
                           fluidRow(
                             column(width = 12,
                                    tags$img(src="coming-soon.png", width = "100%" , 
                                             height = 750)
                             )
                           )
                  #          tabsetPanel(
                  #            id = "peru.tabs",
                  #            
                  #            tabPanel("Institutional level"
                  #                     
                  #            ),
                  #            
                  #            tabPanel("Individual farmer level"
                  #                     
                  #            ),
                  #            
                  #          )
                  #          
                  ),

                  tabPanel("Honduras  Dashboard",
                           value = "honduras ",
                           tags$p("The Honduras  dashboard is under construction and will be made available when data is loaded"),
                           
                           fluidRow(
                             column(width = 12,
                                    tags$img(src="coming-soon.png", width = "100%" , 
                                             height = 750)
                             )
                           )
                  #          tabsetPanel(
                  #            id = "peru.tabs",
                  #            
                  #            tabPanel("Institutional level"
                  #                     
                  #            ),
                  #            
                  #            tabPanel("Individual farmer level"
                  #                     
                  #            ),
                  #            
                  #          )
                  #          
                  ),
                  
                )
                
        )
      )
    )
    
    
  ), #end of dashboard body
  
  footer = dashboardFooter(
    left = tags$a(href="https://www.worldagroforestry.org/", icon("fas fa-caret-up"), "World Agroforestry (ICRAF)", target="_blank"),
    right = tags$a(href="https://treesonfarmsforbiodiversity.com/about-trees-on-farms/", icon("fas fa-caret-up"), "IKI Trees on Farms", target="_blank")
  )
  
)